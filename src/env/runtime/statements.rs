use crate::env::runtime::value::Value;
use crate::env::runtime::types::{Float, Int};
use std::collections::HashMap;

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
    pub fn convert_to_hashmap(&self) -> HashMap<Value, Value> {
        match self {
            Statement::Statement { keys, values, line, column } => {
                let mut map = HashMap::new();
                for (key, value) in keys.iter().zip(values.iter()) {
                    map.insert(key.clone(), value.clone());
                }
                map.insert(Value::String("_line".to_string()), Value::Int(Int::from(*line as i64)));
                map.insert(Value::String("_column".to_string()), Value::Int(Int::from(*column as i64)));
                map
            },
            Statement::Null => {
                let mut map = HashMap::new();
                map.insert(Value::String("_line".to_string()), Value::Int(0.into()));
                map.insert(Value::String("_column".to_string()), Value::Int(0.into()));
                map
            },
        }
    }
    pub fn get_value(&self, key: &str) -> Option<Value> {
        let map = self.convert_to_hashmap();
        map.get(&Value::String(key.to_string())).cloned()
    }    
    pub fn get_type(&self) -> String {
        let map = self.convert_to_hashmap();
        match map.get(&Value::String("type".to_string())) {
            Some(Value::String(s)) => s.clone(),
            _ => "any".to_string(),
        }
    }    
}