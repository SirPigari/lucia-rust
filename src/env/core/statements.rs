use crate::env::core::value::Value;
use crate::env::core::types::{Float, Int};

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub enum Statement {
    Statement {
        keys: Vec<Value>,
        values: Vec<Value>,
        line: usize,
        column: usize,
    },
    Null,
}

impl Statement {
    pub fn convert_to_map(&self) -> Value {
        match self {
            Statement::Statement { keys, values, line, column } => {
                Value::Map {
                    keys: {
                        let mut new_keys = keys.clone();
                        new_keys.push(Value::String("_line".to_string()));
                        new_keys.push(Value::String("_column".to_string()));
                        new_keys
                    },
                    values: {
                        let mut new_values = values.clone();
                        new_values.push(Value::Int(Int::from(*line as i64)));
                        new_values.push(Value::Int(Int::from(*column as i64)));
                        new_values
                    },
                }
            },
            Statement::Null => {
                Value::Map {
                    keys: vec![
                        Value::String("_line".to_string()),
                        Value::String("_column".to_string())
                    ],
                    values: vec![
                        Value::Int(0.into()),
                        Value::Int(0.into()),
                    ],
                }
            },
        }
    }
}