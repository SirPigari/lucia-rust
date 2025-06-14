use crate::env::core::types::{Float, Int};
use crate::env::core::functions::Function;
use crate::env::core::statements::Statement;
use std::hash::{Hash, Hasher};
use std::ops::{Add, Sub, Mul, Div, Rem, Neg};
use std::fmt;
use num_traits::{Float as NumFloat, Zero, ToPrimitive, FromPrimitive};

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
    Error(&'static str, &'static str),
}

impl Eq for Value {}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Value::Float(n) => {
                0u8.hash(state);
                let canonical = n.value.to_string();
                canonical.hash(state);
            }

            Value::Int(n) => {
                1u8.hash(state);
                let canonical = n.value.to_str_radix(10);
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
            Value::Error(err_type, err_msg) => {
                err_type.hash(state);
                err_msg.hash(state);
            }
            Value::Tuple(tuple) => {
                3u8.hash(state);
                tuple.hash(state);
            }
        }
    }
}

impl Add for Value {
    type Output = Value;

    fn add(self, other: Value) -> Value {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a + b),
            (Value::Float(a), Value::Float(b)) => Value::Float(a + b),
            (Value::Int(a), Value::Float(b)) => Value::Float((a + b.into()).into()),
            (Value::Float(a), Value::Int(b)) => Value::Float(a + b.into()),
            (Value::String(a), Value::String(b)) => Value::String(a + &b),
            (Value::Tuple(mut a), Value::Tuple(b)) => {
                a.extend(b);
                Value::Tuple(a)
            }
            (Value::Bytes(mut a), Value::Bytes(b)) => {
                a.extend(b);
                Value::Bytes(a)
            }
            _ => Value::Null,
        }
    }
}

impl Sub for Value {
    type Output = Value;

    fn sub(self, other: Value) -> Value {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a - b),
            (Value::Float(a), Value::Float(b)) => Value::Float(a - b),
            (Value::Int(a), Value::Float(b)) => Value::Float((a - b.into()).into()),
            (Value::Float(a), Value::Int(b)) => Value::Float(a - b.into()),
            _ => Value::Null,
        }
    }
}

impl Mul for Value {
    type Output = Value;

    fn mul(self, other: Value) -> Value {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a * b),
            (Value::Float(a), Value::Float(b)) => Value::Float(a * b),
            (Value::Int(a), Value::Float(b)) => Value::Float((a * b.into()).into()),
            (Value::Float(a), Value::Int(b)) => Value::Float(a * b.into()),
            (Value::String(a), Value::Int(b)) if b >= 0.into() => {
                let mut result = String::new();
                for _ in 0..b.to_usize().unwrap() {
                    result.push_str(&a);
                }
                Value::String(result)
            }
            _ => Value::Null,
        }
    }
}

impl Div for Value {
    type Output = Value;

    fn div(self, other: Value) -> Value {
        match (self, other) {
            (Value::Int(_), Value::Int(b)) if b.is_zero() => Value::Null,
            (Value::Float(_), Value::Float(b)) if b == 0.0.into() => Value::Null,
            (Value::Int(a), Value::Int(b)) => Value::Int(a / b),
            (Value::Float(a), Value::Float(b)) => Value::Float(a / b),
            (Value::Int(a), Value::Float(b)) => {
                if b == 0.0.into() {
                    Value::Null
                } else {
                    Value::Float((a / b.into()).into())
                }
            }
            (Value::Float(a), Value::Int(b)) => {
                if b == 0.into() {
                    Value::Null
                } else {
                    Value::Float(a / b.into())
                }
            }
            _ => Value::Null,
        }
    }
}

impl Rem for Value {
    type Output = Value;

    fn rem(self, other: Value) -> Value {
        match (self, other) {
            (_, Value::Int(b)) if b == 0.into() => Value::Null,
            (_, Value::Float(b)) if b == 0.0.into() => Value::Null,

            (Value::Int(a), Value::Int(b)) => Value::Int(a % b),
            (Value::Float(a), Value::Float(b)) => Value::Float(a % b),
            (Value::Int(a), Value::Float(b)) => Value::Float((a % b.into()).into()),
            (Value::Float(a), Value::Int(b)) => Value::Float(a % b.into()),
            _ => Value::Null,
        }
    }
}

impl Neg for Value {
    type Output = Value;

    fn neg(self) -> Value {
        match self {
            Value::Int(a) => Value::Int(-a),
            Value::Float(a) => Value::Float(-a),
            _ => Value::Null,
        }
    }
}

impl Value {
    pub fn convert_to_statement(&self) -> Statement {
        match self {
            Value::Map { keys, values } => {
                let mut line = 0;
                let mut column = 0;

                let mut new_keys = Vec::new();
                let mut new_values = Vec::new();

                for (i, key) in keys.iter().enumerate() {
                    if let Value::String(s) = key {
                        if s == "_line" {
                            if let Some(Value::Int(val)) = values.get(i) {
                                line = val.to_u32().unwrap() as usize;
                                continue;
                            }
                        }
                        if s == "_column" {
                            if let Some(Value::Int(val)) = values.get(i) {
                                column = val.to_u32().unwrap() as usize;
                                continue;
                            }
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
                    line,
                    column,
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
                line: 0,
                column: 0,
            },
            _ => Statement::Null,
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
            Value::Int(val) if val == &Int::new(0) => true,
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
            Value::Error(_, _) => "error",
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
            Value::Function(func) => func.get_return_type().to_string(),
            Value::Error(err_type, _) => err_type.to_string(),
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
            Value::Error(_, _) => true,
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
                let s = i.value.to_str_radix(10);
                if s.starts_with("-0") {
                    "0".to_string()
                } else if s.trim_start_matches('0').is_empty() {
                    "0".to_string()
                } else {
                    s
                }
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
            Value::Function(func) => format!("<function '{}' at {:p}>", func.get_name(), func),
            Value::Error(err_type, err_msg) => format!("<{}: {}>", err_type, err_msg),
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
                let description = format!("<function '{}' at {:p}>", func.get_name(), func);
                Some(description.into_bytes())
            }
    
            Value::Error(kind, msg) => {
                let error = format!("<{}: {}>", kind, msg);
                Some(error.into_bytes())
            }
        }
    }
    pub fn is_infinity(&self) -> bool {
        match self {
            Value::Float(f) => !f.value.is_finite(),
            Value::Int(i) => Value::Float(Float::from(i.value.clone())).is_infinity(),
            _ => false,
        }
    }
    pub fn is_nan(&self) -> bool {
        match self {
            Value::Float(f) => f.value.is_nan(),
            Value::Int(i) => Value::Float(Float::from(i.value.clone())).is_nan(),
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
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}