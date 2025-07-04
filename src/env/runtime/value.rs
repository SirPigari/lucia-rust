use crate::env::runtime::types::{Float, Int};
use crate::env::runtime::functions::Function;
use crate::env::runtime::statements::Statement;
use crate::env::runtime::objects::Object;
use crate::env::runtime::errors::Error;
use crate::env::runtime::utils::{to_static, format_float, format_int};
use crate::env::runtime::tokens::Location;
use std::hash::{Hash, Hasher};
use std::ops::{Add, Sub, Mul, Div, Rem, Neg};
use std::fmt;
use std::collections::HashMap;
use serde::ser::{Serialize, Serializer, SerializeStruct};
use std::path::PathBuf;

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
            Value::List(_) | Value::Map { .. } => true,
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
    pub fn type_name_as_str(&self) -> &'static str {
        match self {
            Value::Float(_) => "float",
            Value::Int(_) => "int",
            Value::String(_) => "str",
            Value::Boolean(_) => "bool",
            Value::Null => "void",
            Value::Map { .. } => "map",
            Value::List(_) => "list",
            Value::Tuple(_) => "tuple",
            Value::Bytes(_) => "bytes",
            Value::Function(_) => "function",
            Value::Module(obj, _) => to_static(obj.name().to_string()),
            Value::Pointer(ptr) => {
                let raw = *ptr as *const Value;
                let recovered = unsafe { std::rc::Rc::from_raw(raw) };
                let name = recovered.type_name();
                std::mem::forget(recovered);
                to_static(format!("&{}", name))
            }            
            Value::Error(..) => "error",
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
            Value::Function(func) => "function".to_string(),
            Value::Module(obj, _) => obj.name().to_string(),
            Value::Pointer(ptr) => {
                let raw = *ptr as *const Value;
                let recovered = unsafe { std::rc::Rc::from_raw(raw) };
                let name = recovered.type_name();
                std::mem::forget(recovered);
                format!("&{}", name)
            }            
            Value::Error(err_type, _, _) => "error".to_string(),
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
    pub fn convert_to_hashmap(&self) -> Option<std::collections::HashMap<String, Value>> {
        match self {
            Value::Map { keys, values } => {
                if keys.len() != values.len() {
                    return None;
                }
                let mut map = std::collections::HashMap::new();
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