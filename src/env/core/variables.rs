use crate::env::core::value::Value;
use std::fmt::{self, Display};

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Variable {
    name: String,
    value: Value,
    type_: String,
    is_static: bool,
    is_public: bool,
    is_final: bool,
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
        }
    }

    pub fn get_value(&self) -> &Value {
        &self.value
    }

    pub fn set_value(&mut self, value: Value) {
        self.value = value;
    }

    pub fn get_type(&self) -> &str {
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