use crate::env::core::value::Value;
use crate::env::core::utils::make_native_method;
use crate::env::core::functions::Parameter;
use std::collections::HashMap;
use std::fmt::{self, Display};

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
                "to_string",
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
            "to_string".to_string(),
            Variable::new(
                "to_string".to_string(),
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
                        "to_bytes",
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
                

                self.properties.insert(
                    "to_bytes".to_string(),
                    Variable::new(
                        "to_bytes".to_string(),
                        to_bytes,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "ends_with".to_string(),
                    Variable::new(
                        "ends_with".to_string(),
                        endswith,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "starts_with".to_string(),
                    Variable::new(
                        "starts_with".to_string(),
                        startswith,
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
                        "to_float",
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
                    "to_float".to_string(),
                    Variable::new(
                        "to_float".to_string(),
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
                        "to_int",
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
                    "to_int".to_string(),
                    Variable::new(
                        "to_int".to_string(),
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