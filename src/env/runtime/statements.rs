use crate::env::runtime::value::Value;
use crate::env::runtime::types::{Float, Int};
use std::collections::HashMap;
use crate::env::runtime::tokens::Location;

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub enum Statement {
    Statement {
        keys: Vec<Value>,
        values: Vec<Value>,
        loc: Option<Location>,
    },
    Null,
}

impl Statement {
    pub fn convert_to_map(&self) -> Value {
        match self {
            Statement::Statement { keys, values, loc } => {
                Value::Map {
                    keys: {
                        let mut new_keys = keys.clone();
                        new_keys.push(Value::String("_loc".to_string()));
                        new_keys
                    },
                    values: {
                        let mut new_values = values.clone();
                        new_values.push(
                            Value::Map {
                                keys: vec![
                                    Value::String("_file".to_string()),
                                    Value::String("_line_string".to_string()),
                                    Value::String("_line_number".to_string()),
                                    Value::String("_range".to_string())
                                ],
                                values: vec![
                                    Value::String(loc.as_ref().map_or("".to_string(), |l| l.file.clone())),
                                    Value::String(loc.as_ref().map_or("".to_string(), |l| l.line_string.clone())),
                                    Value::Int(Int::from(loc.as_ref().map_or(0, |l| l.line_number as i64))),
                                    Value::Tuple (
                                        vec![
                                            Value::Int(Int::from(loc.as_ref().map_or(0, |l| l.range.0 as i64))),
                                            Value::Int(Int::from(loc.as_ref().map_or(0, |l| l.range.1 as i64))),
                                        ]
                                    )
                                ],
                            }
                        );
                        new_values
                    },
                }
            },
            Statement::Null => {
                Value::Map {
                    keys: vec![
                        Value::String("_loc".to_string()),
                    ],
                    values: vec![
                        Value::Map {
                            keys: vec![
                                Value::String("_file".to_string()),
                                Value::String("_line_string".to_string()),
                                Value::String("_line_number".to_string()),
                                Value::String("_range".to_string())
                            ],
                            values: vec![
                                Value::String("".to_string()),
                                Value::String("".to_string()),
                                Value::Int(0.into()),
                                Value::Tuple(
                                    vec![Value::Int(0.into()), Value::Int(0.into())],
                                )
                            ],
                        }
                    ],
                }
            },
        }
    }
    pub fn convert_to_hashmap(&self) -> HashMap<Value, Value> {
        match self {
            Statement::Statement { keys, values, loc } => {
                let mut map = HashMap::new();
                for (key, value) in keys.iter().zip(values.iter()) {
                    map.insert(key.clone(), value.clone());
                }
                map.insert(Value::String("_loc".to_string()), Value::Map {
                    keys: vec![
                        Value::String("_file".to_string()),
                        Value::String("_line_string".to_string()),
                        Value::String("_line_number".to_string()),
                        Value::String("_range".to_string())
                    ],
                    values: vec![
                        Value::String(loc.as_ref().map_or("".to_string(), |l| l.file.clone())),
                        Value::String(loc.as_ref().map_or("".to_string(), |l| l.line_string.clone())),
                        Value::Int(Int::from(loc.as_ref().map_or(0, |l| l.line_number as i64))),
                        Value::Tuple(
                            vec![
                                Value::Int(Int::from(loc.as_ref().map_or(0, |l| l.range.0 as i64))),
                                Value::Int(Int::from(loc.as_ref().map_or(0, |l| l.range.1 as i64))),
                            ]
                        )
                    ],
                });
                map
            },
            Statement::Null => {
                let mut map = HashMap::new();
                map.insert(Value::String("_loc".to_string()), Value::Map {
                    keys: vec![
                        Value::String("_file".to_string()),
                        Value::String("_line_string".to_string()),
                        Value::String("_line_number".to_string()),
                        Value::String("_range".to_string())
                    ],
                    values: vec![
                        Value::String("".to_string()),
                        Value::String("".to_string()),
                        Value::Int(0.into()),
                        Value::Tuple(vec![Value::Int(0.into()), Value::Int(0.into())]),
                    ],
                });
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