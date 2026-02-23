use crate::env::runtime::value::Value;
use crate::env::runtime::types::Type;
use crate::env::runtime::utils::to_static;
use serde::{Serialize, Deserialize};
use bincode::{Encode, Decode};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Encode, Decode)]
pub struct Variable {
    name: String,
    pub value: Value,
    pub type_: Type,
    is_static: bool,
    is_public: bool,
    is_final: bool,
}

#[allow(dead_code)]
impl Variable {
    pub fn new(name: String, value: Value, type_: String, is_static: bool, is_public: bool, is_final: bool) -> Self {
        Self {
            name,
            value,
            type_: Type::new_simple(&type_),
            is_static,
            is_public,
            is_final,
        }
    }

    pub fn new_pt(name: String, value: Value, type_: Type, is_static: bool, is_public: bool, is_final: bool) -> Self {
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

    pub fn get_value_mut(&mut self) -> &mut Value {
        &mut self.value
    }

    pub fn set_value(&mut self, value: Value) {
        self.value = value;
    }

    pub fn get_type(&self) -> Type {
        self.type_.clone()
    }

    pub fn type_name(&self) -> &str {
        to_static(self.type_.display())
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

    pub fn set_type(&mut self, type_: Type) {
        self.type_ = type_;
    }

    pub fn set_name(&mut self, name: String) {
        self.name = name;
    }

    pub fn is_native(&self) -> bool {
        match &self.value {
            Value::Function(f) => f.is_native(),
            _ => false,
        }
    }
}
