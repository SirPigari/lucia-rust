use crate::env::core::value::Value;
use crate::env::core::utils::make_native_method;
use crate::env::core::functions::Parameter;
use std::collections::HashMap;
use std::fmt::{self, Display};
use std::str::FromStr;
use num_bigint::BigInt;
use num_bigfloat::BigFloat;
use crate::env::core::types::{Float, Int};

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    name: String,
    value: Value,
    type_: String,
    is_static: bool,
    is_public: bool,
    is_final: bool,
    pub properties: HashMap<String, Variable>,
}

impl Variable {
    pub fn new(name: String, value: Value, type_: String, is_static: bool, is_public: bool, is_final: bool) -> Self {
        Self {
            name,
            value,
            type_,
            is_static,
            is_public,
            is_final,
            properties: HashMap::new(),
        }
    }

    pub fn init_properties(&mut self) {
        let val_clone = self.value.clone();
        let to_string = {
            let val_clone = self.value.clone();
            make_native_method(
                "toString",
                move |_args| {
                    match val_clone.to_string() {
                        s if !s.is_empty() => Value::String(s),
                        _ => Value::Null,
                    }
                },
                vec![],
                "str",
                true, true, true,
                None,
            )
        };
    
        self.properties.insert(
            "toString".to_string(),
            Variable::new(
                "toString".to_string(),
                to_string,
                "function".to_string(),
                false,
                true,
                true,
            ),
        );
    
        match &self.value {
            Value::String(_) => {
                let to_bytes = {
                    let val_clone = self.value.clone();
                    make_native_method(
                        "toBytes",
                        move |_args| {
                            match val_clone.to_bytes() {
                                Some(bytes) => Value::Bytes(bytes),
                                None => Value::Null,
                            }
                        },
                        vec![],
                        "bytes",
                        true, true, true,
                        None,
                    )
                };
                
                let endswith = {
                    let val_clone = self.value.clone();
                    make_native_method(
                        "endswith",
                        move |args| {
                            if let Some(s) = args.get("suffix").and_then(|v| match v {
                                Value::String(s) => Some(s),
                                _ => None,
                            }) {
                                if let Value::String(val) = &val_clone {
                                    return Value::Boolean(val.ends_with(s));
                                }
                            }
                            Value::Null
                        },
                        vec![Parameter::positional("suffix", "str")],
                        "bool",
                        true, true, true,
                        None,
                    )
                };
                
                let startswith = {
                    let val_clone = self.value.clone();
                    make_native_method(
                        "startswith",
                        move |args| {
                            if let Some(s) = args.get("prefix").and_then(|v| match v {
                                Value::String(s) => Some(s),
                                _ => None,
                            }) {
                                if let Value::String(val) = &val_clone {
                                    return Value::Boolean(val.starts_with(s));
                                }
                            }
                            Value::Null
                        },
                        vec![Parameter::positional("prefix", "str")],
                        "bool",
                        true, true, true,
                        None,
                    )
                };
                let to_int = {
                    let val_clone = self.value.clone();
                    make_native_method(
                        "toInt",
                        move |_args| {
                            match &val_clone {
                                Value::String(s) => Value::Int( Int {
                                    value: BigInt::parse_bytes(&s.clone().into_bytes().as_slice(), 10).unwrap_or_else(|| BigInt::from(0))
                                }),
                                _ => Value::Null,
                            }
                        },
                        vec![],
                        "int",
                        true, true, true,
                        None,
                    )
                };
                let to_float = {
                    let val_clone = self.value.clone();
                    make_native_method(
                        "toFloat",
                        move |_args| {
                            match &val_clone {
                                Value::String(s) => {
                                    if let Ok(f) = BigFloat::from_str(s) {
                                        Value::Float(
                                            Float {
                                                value: f
                                            }
                                        )
                                    } else {
                                        Value::Null
                                    }
                                },
                                _ => Value::Null,
                            }
                        },
                        vec![],
                        "float",
                        true, true, true,
                        None,
                    )
                };
                

                self.properties.insert(
                    "toBytes".to_string(),
                    Variable::new(
                        "toBytes".to_string(),
                        to_bytes,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "endsWith".to_string(),
                    Variable::new(
                        "endsWith".to_string(),
                        endswith,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "startsWith".to_string(),
                    Variable::new(
                        "startsWith".to_string(),
                        startswith,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "toInt".to_string(),
                    Variable::new(
                        "toInt".to_string(),
                        to_int,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "toFloat".to_string(),
                    Variable::new(
                        "toFloat".to_string(),
                        to_float,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
            }
            Value::Int(_) => {
                let to_float = {
                    let val_clone = self.value.clone();
                    make_native_method(
                        "toFloat",
                        move |_args| {
                            match &val_clone {
                                Value::Int(i) => Value::Float(i.to_float()),
                                Value::Float(f) => Value::Float(f.clone()),
                                _ => Value::Float(0.0.into()),
                            }
                        },
                        vec![],
                        "float",
                        true, true, true,
                        None,
                    )
                };                

                self.properties.insert(
                    "toFloat".to_string(),
                    Variable::new(
                        "toFloat".to_string(),
                        to_float,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
            }
            Value::Float(_) => {
                let to_int = {
                    let val_clone = self.value.clone();
                    make_native_method(
                        "toInt",
                        move |_args| {
                            match &val_clone {
                                Value::Float(f) => Value::Int(f.to_int()),
                                Value::Int(i) => Value::Int(i.clone()),
                                _ => Value::Int(0.into()),
                            }
                        },
                        vec![],
                        "int",
                        true, true, true,
                        None,
                    )
                };

                self.properties.insert(
                    "toInt".to_string(),
                    Variable::new(
                        "toInt".to_string(),
                        to_int,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
            }
            _ => {}
        };
    }

    pub fn is_init(&self) -> bool {
        !self.properties.is_empty()
    }

    pub fn get_value(&self) -> &Value {
        &self.value
    }

    pub fn set_value(&mut self, value: Value) {
        self.value = value;
    }

    pub fn type_name(&self) -> &str {
        &self.type_
    }

    pub fn get_name(&self) -> &str {
        &self.name
    }
    
    pub fn is_static(&self) -> bool {
        self.is_static
    }

    pub fn is_public(&self) -> bool {
        self.is_public
    }

    pub fn is_final(&self) -> bool {
        self.is_final
    }

    pub fn set_final(&mut self, is_final: bool) {
        self.is_final = is_final;
    }

    pub fn set_static(&mut self, is_static: bool) {
        self.is_static = is_static;
    }

    pub fn set_public(&mut self, is_public: bool) {
        self.is_public = is_public;
    }

    pub fn set_type(&mut self, type_: String) {
        self.type_ = type_;
    }

    pub fn set_name(&mut self, name: String) {
        self.name = name;
    }
}