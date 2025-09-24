use crate::env::runtime::value::Value;
use crate::env::runtime::types::{Int};
use std::collections::HashMap;
use crate::env::runtime::tokens::Location;
use serde::{Serialize, Deserialize};
use bincode::{Encode, Decode};

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash, Serialize, Deserialize, Encode, Decode)]
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
    pub fn make_value(val: Value) -> Statement {
        Statement::Statement {
            keys: vec!["TYPE".into(), "value".into()],
            values: vec!["VALUE".into(), val],
            loc: None,
        }
    }
    pub fn convert_to_hashmap(&self) -> HashMap<Value, Value> {
        match self {
            Statement::Statement { keys, values, loc } => {
                let mut map = HashMap::default();
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
                let mut map = HashMap::default();
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
    pub fn convert_from_hashmap(map: &HashMap<Value, Value>) -> Statement {
        if map.is_empty() {
            return Statement::Null;
        }

        let mut keys = Vec::new();
        let mut values = Vec::new();
        let mut loc: Option<Location> = None;

        for (k, v) in map {
            if let Value::String(s) = k {
                if s == "_loc" {
                    if let Value::Map { keys: loc_keys, values: loc_values } = v {
                        let mut file = "".to_string();
                        let mut line_string = "".to_string();
                        let mut line_number = 0;
                        let mut range = (0, 0);
                        let mut lucia_source_loc = "".to_string();

                        for (lk, lv) in loc_keys.iter().zip(loc_values.iter()) {
                            match (lk, lv) {
                                (Value::String(s), Value::String(val)) if s == "_file" => {
                                    file = val.clone();
                                }
                                (Value::String(s), Value::String(val)) if s == "_line_string" => {
                                    line_string = val.clone();
                                }
                                (Value::String(s), Value::Int(n)) if s == "_line_number" => {
                                    line_number = n.to_string().parse().unwrap_or(0);
                                }
                                (Value::String(s), Value::Tuple(vals)) if s == "_range" && vals.len() == 2 => {
                                    if let (Value::Int(start), Value::Int(end)) = (&vals[0], &vals[1]) {
                                        range = (
                                            start.to_string().parse().unwrap_or(0),
                                            end.to_string().parse().unwrap_or(0),
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
                            lucia_source_loc
                        });
                    }
                } else {
                    keys.push(k.clone());
                    values.push(v.clone());
                }
            } else {
                keys.push(k.clone());
                values.push(v.clone());
            }
        }

        if keys.is_empty() && values.is_empty() {
            Statement::Null
        } else {
            Statement::Statement { keys, values, loc }
        }
    }
    pub fn get_value(&self, key: &str) -> Option<Value> {
        match self {
            Statement::Statement { keys, values, loc } => {
                for (k, v) in keys.iter().zip(values.iter()) {
                    if let Value::String(s) = k {
                        if s == key {
                            return Some(v.clone());
                        }
                    }
                }
                match key {
                    "_loc" => Some(Value::Map {
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
                            Value::Tuple(vec![
                                Value::Int(Int::from(loc.as_ref().map_or(0, |l| l.range.0 as i64))),
                                Value::Int(Int::from(loc.as_ref().map_or(0, |l| l.range.1 as i64))),
                            ])
                        ]
                    }),
                    _ => None,
                }
            }
            Statement::Null => None,
        }
    }       
    pub fn get_type(&self) -> String {
        let map = self.convert_to_hashmap();
        match map.get(&Value::String("type".to_string())) {
            Some(Value::String(s)) => s.clone(),
            _ => "any".to_string(),
        }
    }
    pub fn get_name(&self) -> String {
        let map = self.convert_to_hashmap();
        match map.get(&Value::String("name".to_string())) {
            Some(Value::String(s)) => s.clone(),
            _ => "".to_string(),
        }
    }
    pub fn is_empty(&self) -> bool {
        match self {
            Statement::Statement { keys, values, .. } => keys.is_empty() && values.is_empty(),
            Statement::Null => true,
        }
    }
    pub fn from_hashmap_values(values: &HashMap<Value, Value>) -> Statement {
        Statement::convert_from_hashmap(values)
    }
}
