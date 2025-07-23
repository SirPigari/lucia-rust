use crate::env::runtime::types::{Float, Int};
use crate::env::runtime::functions::Function;
use crate::env::runtime::statements::Statement;
use crate::env::runtime::objects::Object;
use crate::env::runtime::errors::Error;
use crate::env::runtime::utils::{format_float, format_int};
use crate::env::runtime::tokens::Location;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::fmt;
use serde::ser::{Serialize, Serializer, SerializeStruct};
use serde::de::{Deserialize, Deserializer};
use std::path::PathBuf;
use bincode::{
    enc::{Encode, Encoder},
    de::{BorrowDecode, Decode, Decoder},
    error::{EncodeError, DecodeError},
};

#[derive(Debug, Clone, PartialEq, PartialOrd)]
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
    Function(Function),
    Module(Object, PathBuf),
    Pointer(usize),
    Error(&'static str, &'static str, Option<Error>),
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
            Value::Function(_) => {
                serializer.serialize_str("Function(opaque)")
            }
            Value::Module(..) => {
                serializer.serialize_str("Object(opaque)")
            }
            Value::Error(kind, msg, _) => {
                let mut s = serializer.serialize_struct("Error", 2)?;
                s.serialize_field("kind", kind)?;
                s.serialize_field("message", msg)?;
                s.end()
            }
            Value::Pointer(ptr) => {
                let mut s = serializer.serialize_struct("Pointer", 1)?;
                let raw_ptr = *ptr as *const ();  
                s.serialize_field("address", &format!("{:p}", raw_ptr))?;
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
            Function(_) | Module(_, _) | Error(_, _, _) => {
                4u8.encode(encoder) // fallback to Null
            }
            Pointer(ptr) => {
                11u8.encode(encoder)?;
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
            9 | 10 | 12 => Ok(Value::Null),
            11 => Ok(Value::Pointer(usize::decode(decoder)?)),
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
            Value::Function(func) => {
                func.get_name().hash(state);
                func.get_parameters().hash(state);
                func.get_return_type().hash(state);
            }

            Value::Module(obj, path) => {
                obj.name().hash(state);
                if let Some(props) = obj.get_properties() {
                    for var in props.values() {
                        var.value.hash(state);
                    }
                }                
                obj.get_parameters().hash(state);
                path.to_str().unwrap_or("").hash(state);
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
                                        _ => {}
                                    }
                                }
    
                                loc = Some(Location {
                                    file,
                                    line_string,
                                    line_number,
                                    range,
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
            _ => false,
        }
    }
    pub fn iter(&self) -> Box<dyn Iterator<Item = Value> + '_> {
        match self {
            Value::List(items) => Box::new(items.clone().into_iter()),

            Value::Map { keys, .. } => Box::new(keys.clone().into_iter()),

            Value::String(s) => Box::new(s.chars().map(|c| Value::String(c.to_string()))),

            Value::Bytes(b) => Box::new(b.clone().into_iter().map(|byte| Value::Int(Int::from(byte as i32)))),

            Value::Tuple(items) => Box::new(items.clone().into_iter()),

            _ => Box::new(std::iter::empty()),
        }
    }
    pub fn is_zero(&self) -> bool {
        match self {
            Value::Int(val) if val == &Int::from_i64(0) => true,
            Value::Float(f) if *f == 0.0.into() => true,
            _ => false,
        }
    }
    pub fn type_name(&self) -> String {
        match self {
            Value::Float(_) => "float".to_string(),
            Value::Int(_) => "int".to_string(),
            Value::String(_) => "str".to_string(),
            Value::Boolean(_) => "bool".to_string(),
            Value::Null => "void".to_string(),
            Value::Map { .. } => "map".to_string(),
            Value::List(_) => "list".to_string(),
            Value::Tuple(_) => "tuple".to_string(),
            Value::Bytes(_) => "bytes".to_string(),
            Value::Function(_) => "function".to_string(),
            Value::Module(obj, _) => obj.name().to_string(),
            Value::Pointer(ptr) => {
                let raw = *ptr as *const Value;
                let recovered = unsafe { std::rc::Rc::from_raw(raw) };
                let name = recovered.type_name();
                std::mem::forget(recovered);
                format!("&{}", name)
            }            
            Value::Error(..) => "error".to_string(),
        }
    }
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Boolean(b) => *b,
            Value::Int(n) => *n != 0.into(),
            Value::Float(f) => *f != 0.0.into(),
            Value::String(s) => !s.is_empty(),
            Value::List(l) => !l.is_empty(),
            Value::Map { keys, .. } => !keys.is_empty(),
            Value::Tuple(items) => !items.is_empty(),
            Value::Bytes(b) => !b.is_empty(),
            Value::Function(_) => true,
            Value::Module(..) => true,
            Value::Error(_, _, _) => true,
            Value::Pointer(_) => true,
            Value::Null => false,
        }
    }
    pub fn to_string(&self) -> String {
        match self {
            Value::Float(f) => {
                let s = format!("{:.15}", f);
                if s.contains('.') {
                    s.trim_end_matches('0').trim_end_matches('.').to_string()
                } else {
                    s
                }
            }
            Value::Int(i) => {
                i.to_string()
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
                let raw_ptr = *ptr as *const ();
                format!("<pointer to {:p}>", raw_ptr)
            }            
            Value::Function(func) => format!("<function '{}' at {:p}>", func.get_name(), func.ptr()),
            Value::Module(obj, _) => format!("<module '{}' at {:p}>", obj.name(), obj.ptr()),
            Value::Error(err_type, err_msg, _) => format!("<{}: {}>", err_type, err_msg),
        }
    }    
    pub fn to_bytes(&self) -> Option<Vec<u8>> {
        match self {
            Value::Bytes(bytes) => Some(bytes.clone()),
    
            Value::String(s) => Some(s.as_bytes().to_vec()),
    
            Value::Int(i) => Some(i.to_string().into_bytes()),
    
            Value::Float(f) => Some(f.to_string().into_bytes()),

            Value::Boolean(true) => Some(vec![1]),
            Value::Boolean(false) => Some(vec![0]),
    
            Value::Null => Some(vec![]),
    
            Value::Map { keys: _, values: _ } | Value::List(_) | Value::Tuple(_) => {
                let description = format!("<{}>", self.type_name());
                Some(description.into_bytes())
            }
    
            Value::Function(func) => {
                let description = format!("<function '{}' at {:p}>", func.get_name(), func.ptr());
                Some(description.into_bytes())
            }

            Value::Module(obj, _) => {
                let description = format!("<module '{}' at {:p}>", obj.name(), obj.ptr());
                Some(description.into_bytes())
            }

            Value::Pointer(ptr) => {
                let raw_ptr = *ptr as *const ();
                let description = format!("<pointer to {:p}>", raw_ptr);
                Some(description.into_bytes())
            }
    
            Value::Error(kind, msg, _) => {
                let error = format!("<{}: {}>", kind, msg);
                Some(error.into_bytes())
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
                let mut map = HashMap::new();
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
    pub fn is_null(&self) -> bool {
        matches!(self, Value::Null)
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