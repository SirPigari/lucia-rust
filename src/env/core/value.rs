use crate::env::core::types::{Float, Int};
use crate::env::core::functions::Function;
use crate::env::core::statements::Statement;
use std::hash::{Hash, Hasher};
use std::ops::{Add, Sub, Mul, Div, Rem, Neg};
use std::collections::HashMap;
use std::sync::Mutex;
use std::fmt;
use once_cell::sync::Lazy;


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
    List(Vec<Value>),
    ListCompletion {
        pattern: Vec<Value>,
        end: Option<Box<Value>>,
    },
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

            Value::ListCompletion { pattern, end } => {
                pattern.hash(state);
                end.hash(state);
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
            _ => Statement::Null,
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
            Value::String(_) => "string",
            Value::Boolean(_) => "bool",
            Value::Null => "null",
            Value::Map { .. } => "map",
            Value::List(_) => "list",
            Value::ListCompletion { .. } => "list_completion",
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
            Value::Null => "null".to_string(),
            Value::Map { .. } => "map".to_string(),
            Value::List(_) => "list".to_string(),
            Value::ListCompletion { .. } => "list_completion".to_string(),
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
            Value::Map { keys, .. } => !keys.is_empty(), // If no keys, assume empty
            Value::ListCompletion { pattern, .. } => !pattern.is_empty(),
            Value::Function(_) => true,
            Value::Error(_, _) => true,
            Value::Null => false,
        }
    }
    pub fn to_string(&self) -> String {
        match self {
            Value::Float(f) => f.to_string(),
            Value::Int(i) => i.to_string(),
            Value::String(s) => s.clone(),
            Value::Boolean(b) => b.to_string(),
            Value::Null => "null".to_string(),
            Value::Map { keys, values } => {
                let pairs: Vec<String> = keys.iter().zip(values.iter())
                    .map(|(k, v)| format!("{}: {}", k.to_string(), v.to_string()))
                    .collect();
                format!("{{{}}}", pairs.join(", "))
            }
            Value::List(v) => {
                let items: Vec<String> = v.iter().map(|item| item.to_string()).collect();
                format!("[{}]", items.join(", "))
            }
            Value::ListCompletion { pattern, end } => {
                let pattern_str: Vec<String> = pattern.iter().map(|v| v.to_string()).collect();
                let end_str = match end {
                    Some(e) => e.to_string(),
                    None => "None".to_string(),
                };
                format!("ListCompletion {{ pattern: [{}], end: {} }}", pattern_str.join(", "), end_str)
            }
            Value::Function(func) => format!("<function '{}' at {:p}>", func.get_name(), func),
            Value::Error(err_type, err_msg) => format!("<{}: {}>", err_type, err_msg),
        }
    }
}