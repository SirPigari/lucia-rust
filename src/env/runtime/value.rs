use crate::env::runtime::types::{Float, Int, Type};
use crate::env::runtime::functions::Function;
use crate::env::runtime::generators::Generator;
use crate::env::runtime::statements::Statement;
use crate::env::runtime::modules::Module;
use crate::env::runtime::errors::Error;
use crate::env::runtime::utils::{format_float, format_int, fix_path};
use crate::env::runtime::tokens::Location;
use crate::env::runtime::structs_and_enums::{Enum, Struct};
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::fmt;
use serde::ser::{Serialize, Serializer, SerializeStruct};
use serde::de::{Deserialize, Deserializer};
use bincode::{
    enc::{Encode, Encoder},
    de::{BorrowDecode, Decode, Decoder},
    error::{EncodeError, DecodeError},
};
use std::sync::Arc;

#[derive(Clone, PartialEq, PartialOrd)]
pub enum Value {
    Float(Float),
    Int(Int),
    String(String),
    Boolean(bool),
    Null,
    Map {
        keys: Vec<Value>,
        values: Vec<Value>,
    },
    Tuple(Vec<Value>),
    List(Vec<Value>),
    Bytes(Vec<u8>),
    Type(Type),
    Function(Function),
    Generator(Generator),
    Module(Module),
    Enum(Enum),
    Struct(Struct),
    Pointer(Arc<Value>),
    Error(&'static str, &'static str, Option<Error>),
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Generator(g) => write!(f, "{:#?}", g),
            Value::Function(func) => write!(f, "{:#?}", func),
            Value::Module(obj) => write!(f, "Module('{}')", obj.name()),
            Value::Pointer(arc) => {
                let raw_ptr = Arc::as_ptr(arc);
                let addr = raw_ptr as usize;
                write!(f, "Pointer(0x{:X})", addr)
            },
            Value::Error(kind, msg, _) => write!(f, "Error({}, {})", kind, msg),
            Value::Struct(v) => write!(f, "{}", v.display()),
            Value::Enum(v) => write!(f, "{}", v.display()),
            _ => write!(f, "{:?}", self.to_string()),
        }
    }
}

impl Serialize for Value {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Value::Float(f) => {
                format_float(f).serialize(serializer)
            }
            Value::Int(i) => {
                format_int(i).serialize(serializer)
            }
            Value::String(s) => serializer.serialize_str(s),
            Value::Boolean(b) => serializer.serialize_bool(*b),
            Value::Null => serializer.serialize_unit(),
            Value::Map { keys, values } => {
                use serde::ser::SerializeMap;

                let mut map = serializer.serialize_map(Some(keys.len()))?;
                for (k, v) in keys.iter().zip(values.iter()) {
                    map.serialize_entry(k, v)?;
                }
                map.end()
            }
            Value::Tuple(vec) | Value::List(vec) => {
                serializer.collect_seq(vec)
            }
            Value::Bytes(b) => serializer.serialize_bytes(b),
            Value::Type(t) => t.display().serialize(serializer),
            Value::Function(_) => {
                serializer.serialize_str("Function(opaque)")
            }
            Value::Generator(_) => {
                serializer.serialize_str("Generator(opaque)")
            }
            Value::Module(..) => {
                serializer.serialize_str("Module(opaque)")
            }
            Value::Enum(_) => {
                serializer.serialize_str("Enum(opaque)")
            }
            Value::Struct(_) => {
                serializer.serialize_str("Struct(opaque)")
            }
            Value::Error(kind, msg, _) => {
                let mut s = serializer.serialize_struct("Error", 2)?;
                s.serialize_field("kind", kind)?;
                s.serialize_field("message", msg)?;
                s.end()
            }
            Value::Pointer(arc) => {
                let mut s = serializer.serialize_struct("Pointer", 1)?;
                let raw_ptr = Arc::as_ptr(arc);
                let addr = raw_ptr as usize;
                s.serialize_field("address", &format!("0x{:X}", addr))?;
                s.end()
            }
        }
    }
}

impl<'de> Deserialize<'de> for Value {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        use serde::de::{Visitor, MapAccess, SeqAccess};
        use std::fmt;

        struct ValueVisitor;

        impl<'de> Visitor<'de> for ValueVisitor {
            type Value = Value;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("a valid Lucia runtime Value")
            }

            fn visit_bool<E>(self, v: bool) -> Result<Self::Value, E> {
                Ok(Value::Boolean(v))
            }

            fn visit_i64<E>(self, v: i64) -> Result<Self::Value, E> {
                Ok(Value::Int(Int::from(v)))
            }

            fn visit_u64<E>(self, v: u64) -> Result<Self::Value, E> {
                Ok(Value::Int(Int::from(v as i64)))
            }

            fn visit_f64<E>(self, v: f64) -> Result<Self::Value, E> {
                Ok(Value::Float(Float::from(v)))
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E> {
                Ok(Value::String(v.to_string()))
            }

            fn visit_string<E>(self, v: String) -> Result<Self::Value, E> {
                Ok(Value::String(v))
            }

            fn visit_none<E>(self) -> Result<Self::Value, E> {
                Ok(Value::Null)
            }

            fn visit_unit<E>(self) -> Result<Self::Value, E> {
                Ok(Value::Null)
            }

            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: SeqAccess<'de>,
            {
                let mut values = Vec::new();
                while let Some(elem) = seq.next_element()? {
                    values.push(elem);
                }
                Ok(Value::List(values))
            }

            fn visit_map<M>(self, mut access: M) -> Result<Self::Value, M::Error>
            where
                M: MapAccess<'de>,
            {
                let mut keys = Vec::new();
                let mut values = Vec::new();

                while let Some((k, v)) = access.next_entry()? {
                    keys.push(k);
                    values.push(v);
                }

                Ok(Value::Map { keys, values })
            }
        }

        deserializer.deserialize_any(ValueVisitor)
    }
}

impl Encode for Value {
    fn encode<E: Encoder>(&self, encoder: &mut E) -> Result<(), EncodeError> {
        use Value::*;

        match self {
            Float(f) => {
                0u8.encode(encoder)?;
                f.to_string().encode(encoder)
            }
            Int(i) => {
                1u8.encode(encoder)?;
                i.to_string().encode(encoder)
            }
            String(s) => {
                2u8.encode(encoder)?;
                s.encode(encoder)
            }
            Boolean(b) => {
                3u8.encode(encoder)?;
                b.encode(encoder)
            }
            Null => {
                4u8.encode(encoder)
            }
            Map { keys, values } => {
                5u8.encode(encoder)?;
                keys.encode(encoder)?;
                values.encode(encoder)
            }
            Tuple(v) => {
                6u8.encode(encoder)?;
                v.encode(encoder)
            }
            List(v) => {
                7u8.encode(encoder)?;
                v.encode(encoder)
            }
            Bytes(b) => {
                8u8.encode(encoder)?;
                b.encode(encoder)
            }
            Type(t) => {
                9u8.encode(encoder)?;
                t.encode(encoder)
            }
            Module(_) | Error(_, _, _) | Generator(_) => {
                4u8.encode(encoder) // fallback to Null
            }
            Function(func) => {
                13u8.encode(encoder)?;
                func.encode(encoder)
            }
            Enum(enm) => {
                10u8.encode(encoder)?;
                enm.encode(encoder)
            }
            Struct(strct) => {
                11u8.encode(encoder)?;
                strct.encode(encoder)
            }
            Pointer(ptr) => {
                12u8.encode(encoder)?;
                ptr.encode(encoder)
            }
        }
    }
}

impl<C> Decode<C> for Value {
    fn decode<D: Decoder>(decoder: &mut D) -> Result<Self, DecodeError> {
        let tag = u8::decode(decoder)?;

        match tag {
            0 => {
                let s = String::decode(decoder)?;
                let f = Float::from_str(&s).map_err(|_| DecodeError::Other("parse Float failed".into()))?;
                Ok(Value::Float(f))
            }
            1 => {
                let s = String::decode(decoder)?;
                let i = Int::from_str(&s).map_err(|_| DecodeError::Other("parse Int failed".into()))?;
                Ok(Value::Int(i))
            }
            2 => Ok(Value::String(String::decode(decoder)?)),
            3 => Ok(Value::Boolean(bool::decode(decoder)?)),
            4 => Ok(Value::Null),
            5 => {
                let keys = Vec::<Value>::decode(decoder)?;
                let values = Vec::<Value>::decode(decoder)?;
                Ok(Value::Map { keys, values })
            }
            6 => Ok(Value::Tuple(Vec::<Value>::decode(decoder)?)),
            7 => Ok(Value::List(Vec::<Value>::decode(decoder)?)),
            8 => Ok(Value::Bytes(Vec::<u8>::decode(decoder)?)),
            9 => Ok(Value::Type(Type::decode(decoder)?)),
            10 => Ok(Value::Enum(Enum::decode(decoder)?)),
            11 => Ok(Value::Struct(Struct::decode(decoder)?)),
            12 => Ok(Value::Pointer(unsafe { Arc::from_raw(usize::decode(decoder)? as *const Value) })),
            13 => Ok(Value::Function(Function::decode(decoder)?)),
            _ => Err(DecodeError::Other("invalid tag".into())),
        }
    }
}

impl<'de, C> BorrowDecode<'de, C> for Value {
    fn borrow_decode<D: Decoder>(decoder: &mut D) -> Result<Self, DecodeError> {
        Decode::decode(decoder)
    }
}

impl Eq for Value {}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Value::Float(n) => {
                0u8.hash(state);
                let canonical = n.to_string();
                canonical.hash(state);
            }

            Value::Int(n) => {
                1u8.hash(state);
                let canonical = n.to_string();
                canonical.hash(state);
            }

            Value::String(s) => s.hash(state),

            Value::Boolean(b) => b.hash(state),

            Value::Null => 0.hash(state),

            Value::Map { keys, values, .. } => {
                keys.hash(state);
                values.hash(state);
            }

            Value::List(v) => v.hash(state),
            
            Value::Bytes(bytes) => {
                2u8.hash(state);
                bytes.hash(state);
            }

            Value::Type(t) => {
                3u8.hash(state);
                t.display().hash(state);
            }

            Value::Function(func) => {
                func.get_name().hash(state);
                func.get_parameters().hash(state);
                func.get_return_type().hash(state);
            }

            Value::Generator(generator) => {
                generator.name().hash(state);
                generator.ptr().hash(state);
            }

            Value::Module(obj) => {
                obj.name().hash(state);
                let props = obj.get_properties();
                for var in props.values() {
                    var.value.hash(state);
                }
                obj.get_parameters().hash(state);
            }

            Value::Enum(enm) => {
                enm.get_type().hash(state);
                enm.variant.hash(state);
            }

            Value::Struct(strct) => {
                strct.get_type().hash(state);

                let mut entries: Vec<_> = strct.fields().iter().collect();
                entries.sort_by(|a, b| a.0.cmp(b.0));

                for (k, v) in entries {
                    k.hash(state);
                    v.hash(state);
                }
            }

            Value::Error(err_type, err_msg, referr) => {
                err_type.hash(state);
                err_msg.hash(state);
                if let Some(err) = referr {
                    err.error_type().hash(state);
                    err.msg().hash(state);
                }
            }
            Value::Tuple(tuple) => {
                3u8.hash(state);
                tuple.hash(state);
            }
            Value::Pointer(ptr) => {
                4u8.hash(state);
                ptr.hash(state);
            }
        }
    }
}

impl Value {
    pub fn convert_to_statement(&self) -> Statement {
        match self {
            Value::Map { keys, values } => {
                let mut loc: Option<Location> = None;
    
                let mut new_keys = Vec::new();
                let mut new_values = Vec::new();
    
                for (i, key) in keys.iter().enumerate() {
                    if let Value::String(s) = key {
                        if s == "_loc" {
                            if let Some(Value::Map { keys: loc_keys, values: loc_values }) = values.get(i) {
                                let mut file = String::new();
                                let mut line_string = String::new();
                                let mut line_number = 0;
                                let mut range = (0, 0);
                                let mut lucia_source_loc = String::new();
    
                                for (k, v) in loc_keys.iter().zip(loc_values.iter()) {
                                    match (k, v) {
                                        (Value::String(s), Value::String(val)) if s == "_file" => {
                                            file = val.clone();
                                        }
                                        (Value::String(s), Value::String(val)) if s == "_line_string" => {
                                            line_string = val.clone();
                                        }
                                        (Value::String(s), Value::Int(n)) if s == "_line_number" => {
                                            line_number = n.to_i64().unwrap_or(0) as usize;
                                        }
                                        (Value::String(s), Value::Tuple(vs)) if s == "_range" && vs.len() == 2 => {
                                            if let (Value::Int(start), Value::Int(end)) = (&vs[0], &vs[1]) {
                                                range = (
                                                    start.to_i64().unwrap_or(0) as usize,
                                                    end.to_i64().unwrap_or(0) as usize,
                                                );
                                            }
                                        }
                                        (Value::String(s), Value::String(val)) if s == "_lucia_source_loc" => {
                                            lucia_source_loc = val.clone();
                                        }
                                        _ => {}
                                    }
                                }
    
                                loc = Some(Location {
                                    file,
                                    line_string,
                                    line_number,
                                    range,
                                    lucia_source_loc,
                                });
                            }
                            continue;
                        }
                    }
    
                    new_keys.push(key.clone());
                    if let Some(value) = values.get(i) {
                        new_values.push(value.clone());
                    }
                }
    
                Statement::Statement {
                    keys: new_keys,
                    values: new_values,
                    loc,
                }
            }
    
            Value::Null => Statement::Statement {
                keys: vec![
                    Value::String("type".to_string()),
                    Value::String("value".to_string()),
                ],
                values: vec![
                    Value::String("BOOLEAN".to_string()),
                    Value::String("null".to_string()),
                ],
                loc: None,
            },
    
            _ => Statement::Null,
        }
    }
    pub fn from_json(val: &serde_json::Value) -> Self {
        match val {
            serde_json::Value::Null => Value::Null,
            serde_json::Value::Bool(b) => Value::Boolean(*b),
            serde_json::Value::Number(num) => {
                if let Some(i) = num.as_i64() {
                    Value::Int(i.into())
                } else if let Some(f) = num.as_f64() {
                    Value::Float(f.into())
                } else {
                    Value::Null
                }
            }
            serde_json::Value::String(s) => Value::String(s.clone()),
            serde_json::Value::Array(arr) => {
                let vals = arr.iter().map(Value::from_json).collect();
                Value::List(vals)
            }
            serde_json::Value::Object(obj) => {
                let keys = obj.keys()
                    .map(|k| Value::String(k.clone()))
                    .collect::<Vec<_>>();
                let values = obj.values()
                    .map(Value::from_json)
                    .collect::<Vec<_>>();
                Value::Map { keys, values }
            }
        }
    }
    pub fn is_iterable(&self) -> bool {
        match self {
            Value::List(_) => true,
            Value::String(s) if !s.is_empty() => true,
            Value::Bytes(b) if !b.is_empty() => true,
            Value::Tuple(items) if !items.is_empty() => true,
            Value::Map { keys, values } if !keys.is_empty() && !values.is_empty() => true,
            Value::Generator(_) => true,
            _ => false,
        }
    }
    pub fn iter(&self) -> Box<dyn Iterator<Item = Value> + '_> {
        match self {
            Value::List(items) => Box::new(items.clone().into_iter()),

            Value::Map { keys, values } => Box::new(
                keys.clone()
                    .into_iter()
                    .zip(values.clone().into_iter())
                    .map(|(k, v)| Value::Tuple(vec![k, v])),
            ),

            Value::String(s) => Box::new(s.chars().map(|c| Value::String(c.to_string()))),

            Value::Bytes(b) => Box::new(b.clone().into_iter().map(|byte| Value::Int(Int::from(byte as i32)))),

            Value::Tuple(items) => Box::new(items.clone().into_iter()),

            Value::Generator(generator) => Box::new(generator.make_iter()),

            _ => Box::new(std::iter::empty()),
        }
    }
    pub fn is_zero(&self) -> bool {
        match self {
            Value::Int(val) if val == &Int::from_i64(0) => true,
            Value::Float(f) if *f == Float::from(0.0) => true,
            _ => false,
        }
    }
    pub fn type_name(&self) -> String {
        self.get_type().display()
    }
    pub fn get_type(&self) -> Type {
        match self {
            Value::Float(_) => Type::new_simple("float"),
            Value::Int(_) => Type::new_simple("int"),
            Value::String(_) => Type::new_simple("str"),
            Value::Boolean(_) => Type::new_simple("bool"),
            Value::Null => Type::new_simple("void"),
            Value::Map { .. } => Type::new_simple("map"),
            Value::List(_) => Type::new_simple("list"),
            Value::Tuple(_) => Type::new_simple("tuple"),
            Value::Bytes(_) => Type::new_simple("bytes"),
            Value::Type(_) => Type::new_simple("type"),
            Value::Function(f) => f.get_type(),
            Value::Generator(_) => Type::new_simple("generator"),
            Value::Module(..) => Type::new_simple("module"),
            Value::Enum(e) => e.get_type(),
            Value::Struct(s) => s.get_type(),
            Value::Pointer(arc) => {
                let mut t = arc.get_type();
                t.set_reference(true);
                t
            }
            Value::Error(..) => Type::new_simple("error"),
        }
    }
    pub fn get_size(&self) -> usize {
        match self {
            Value::Float(_) => std::mem::size_of::<Float>(),
            Value::Int(_) => std::mem::size_of::<Int>(),
            Value::String(s) => s.len(),
            Value::Boolean(_) => std::mem::size_of::<bool>(),
            Value::Null => 0,
            Value::Map { keys, values } => {
                let mut final_size: usize = 0;
                for (k, v) in keys.iter().zip(values.iter()) {
                    final_size += k.get_size() + v.get_size();
                }
                final_size
            }
            Value::List(v) => v.iter().map(|item| item.get_size()).sum(),
            Value::Tuple(items) => items.iter().map(|item| item.get_size()).sum(),
            Value::Bytes(b) => b.len(),
            Value::Type(_) => std::mem::size_of::<Type>(),
            Value::Function(func) => func.get_size(),
            Value::Generator(generator) => generator.get_size(),
            Value::Module(obj) => obj.get_size(),
            Value::Enum(enm) => enm.get_size(),
            Value::Struct(strct) => strct.get_size(),
            Value::Pointer(p) => Arc::strong_count(p),
            Value::Error(_, _, _) => std::mem::size_of::<Error>(),
        }
    }
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Boolean(b) => *b,
            Value::Int(n) => *n != Int::from_i64(0),
            Value::Float(f) => *f != Float::from(0.0),
            Value::String(s) => !s.is_empty(),
            Value::List(l) => !l.is_empty(),
            Value::Map { keys, .. } => !keys.is_empty(),
            Value::Tuple(items) => !items.is_empty(),
            Value::Bytes(b) => !b.is_empty(),
            Value::Type(_) => true,
            Value::Function(_) => true,
            Value::Generator(_) => true,
            Value::Module(..) => true,
            Value::Enum(e) => e.is_truthy(),
            Value::Struct(s) => s.is_truthy(),
            Value::Error(_, _, _) => true,
            Value::Pointer(p) => Arc::strong_count(p) > 0 && !p.is_null(),
            Value::Null => false,
        }
    }
    pub fn to_string(&self) -> String {
        match self {
            Value::Float(f) => {
                f.to_str()
            }
            Value::Int(i) => {
                format_int(i)
            }
            Value::String(s) => s.clone(),
            Value::Boolean(b) => b.to_string(),
            Value::Null => "null".to_string(),
            Value::Map { keys, values } => {
                let pairs: Vec<String> = keys.iter().zip(values.iter())
                    .map(|(k, v)| format!("{}: {}", k.to_string(), v.to_string()))
                    .collect();
                format!("{{{}}}", pairs.join(", "))
            }
            Value::Tuple(items) => {
                let items: Vec<String> = items.iter().map(|item| item.to_string()).collect();
                format!("({})", items.join(", "))
            }
            Value::List(v) => {
                let items: Vec<String> = v.iter().map(|item| item.to_string()).collect();
                format!("[{}]", items.join(", "))
            }
            Value::Bytes(bytes) => {
                match String::from_utf8(bytes.clone()) {
                    Ok(s) => s,
                    Err(_) => "<invalid utf-8>".to_string(),
                }
            }
            Value::Pointer(ptr) => {
                let raw_ptr = Arc::as_ptr(ptr);
                let addr = raw_ptr as usize;
                format!("<pointer to 0x{:X}>", addr)
            }
            Value::Type(t) => (*t).display_simple(),
            Value::Function(func) => {
                let addr = func.ptr() as *const () as usize;
                format!("<function '{}' at 0x{:X}>", func.get_name(), addr)
            }
            Value::Generator(generator) => {
                let addr = generator.ptr() as *const () as usize;
                match generator.name() {
                    Some(name) => format!("<generator '{}' at 0x{:X}>", name, addr),
                    None => format!("<generator at 0x{:X}>", addr),
                }
            }
            Value::Module(obj) => {
                let addr = obj.ptr() as *const () as usize;
                format!("<module '{}' from '{}' at 0x{:X}>", obj.name(), fix_path(obj.path().display().to_string()), addr)
            }
            Value::Enum(enm) => {
                let addr = enm.ptr() as *const () as usize;
                format!("<enum '{}' at 0x{:X}>", enm.get_type().display_simple(), addr)
            }
            Value::Struct(strct) => {
                let addr = strct.ptr() as *const () as usize;
                format!("<struct '{}' at 0x{:X}>", strct.get_type().display_simple(), addr)
            }
            Value::Error(err_type, err_msg, _) => format!("<{}: {}>", err_type, err_msg),
        }
    }    
    pub fn to_bytes(&self) -> Option<Vec<u8>> {
        match self {
            Value::Bytes(bytes) => Some(bytes.clone()),
    
            Value::String(s) => Some(s.as_bytes().to_vec()),
    
            Value::Int(i) => Some(i.to_string().into_bytes()),
    
            Value::Float(f) => Some(f.to_string().into_bytes()),

            Value::Boolean(true) => Some(vec![0x01]),
            Value::Boolean(false) => Some(vec![0x00]),
    
            Value::Null => Some(vec![0x00]),
    
            // opaque types
            Value::Map { keys: _, values: _ } | Value::Type(_) | Value::Function(_) | Value::Generator(_) | Value::Module(_) | Value::Pointer(_) | Value::Error(_, _, _) | Value::Enum(_) | Value::Struct(_) | Value::List(_) | Value::Tuple(_) => {
                let cfg = bincode::config::standard();
                match bincode::encode_to_vec(&self, cfg) {
                    Ok(vec) => Some(vec),
                    Err(_) => None,
                }
            }
        }
    }
    pub fn is_infinity(&self) -> bool {
        match self {
            Value::Float(f) => f.is_infinity(),
            Value::Int(i) => match Float::from_int(&i.clone()) {
                Ok(f) => f.is_infinity(),
                Err(_) => false,
            },
            _ => false,
        }
    }
    pub fn is_nan(&self) -> bool {
        match self {
            Value::Float(f) => f.is_nan(),
            Value::Int(i) => match Float::from_int(&i.clone()) {
                Ok(f) => f.is_nan(),
                Err(_) => false,
            },
            _ => false,
        }
    }
    pub fn is_statement(&self) -> bool {
        match self {
            Value::Map { keys, .. } => { keys.iter().any(|k| matches!(k, Value::String(s) if s == "type"))},
            Value::Null => true,
            _ => false,
        }
    }
    pub fn convert_to_hashmap(&self) -> Option<HashMap<String, Value>> {
        match self {
            Value::Map { keys, values } => {
                if keys.len() != values.len() {
                    return None;
                }
                let mut map = HashMap::default();
                for (key, value) in keys.iter().zip(values.iter()) {
                    if let Value::String(s) = key {
                        map.insert(s.clone(), value.clone());
                    } else {
                        return None;
                    }
                }
                Some(map)
            }
            _ => None,
        }
    }
    pub fn convert_to_hashmap_value(&self) -> HashMap<Value, Value> {
        match self {
            Value::Map { keys, values } => {
                if keys.len() != values.len() {
                    return HashMap::default();
                }
                let mut map = HashMap::default();
                for (key, value) in keys.iter().zip(values.iter()) {
                    map.insert(key.clone(), value.clone());
                }
                map
            }
            _ => HashMap::default(),
        }
    }
    pub fn is_null(&self) -> bool {
        matches!(self, Value::Null)
    }
    pub fn iterable_to_vec(&self) -> Vec<Value> {
        match self {
            Value::List(items) => items.clone(),
            Value::String(s) => s.chars().map(|c| Value::String(c.to_string())).collect(),
            Value::Bytes(b) => b.iter().map(|&byte| Value::Int(Int::from(byte as i32))).collect(),
            Value::Tuple(items) => items.clone(),
            Value::Map { keys, values } => keys.iter().zip(values.iter()).map(|(k, v)| Value::Map { keys: vec![k.clone()], values: vec![v.clone()] }).collect(),
            Value::Generator(generator) => generator.make_iter().collect(),
            _ => vec![],
        }
    }
}

impl From<&str> for Value {
    fn from(s: &str) -> Self {
        Value::String(s.to_string())
    }
}

impl From<String> for Value {
    fn from(s: String) -> Self {
        Value::String(s)
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}
