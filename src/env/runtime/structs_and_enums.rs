#![allow(dead_code)]
use crate::env::runtime::types::Type;
use crate::env::runtime::value::Value;
use std::collections::HashMap;

pub struct Enum {
    pub ty: Type,
    pub variant: (String, Value),
}

pub struct Struct {
    pub ty: Type,
    pub fields: HashMap<String, Value>,
}

impl Enum {
    pub fn new(ty: Type, variant: (String, Value)) -> Self {
        Self { ty, variant }
    }
}

impl Struct {
    pub fn new(ty: Type) -> Self {
        Self {
            ty,
            fields: HashMap::new(),
        }
    }

    pub fn get(&self, field: &str) -> Option<&Value> {
        self.fields.get(field)
    }
    pub fn set(&mut self, field: String, value: Value) -> Option<Value> {
        self.fields.insert(field, value)
    }
    pub fn remove(&mut self, field: &str) -> Option<Value> {
        self.fields.remove(field)
    }
    pub fn fields(&self) -> &HashMap<String, Value> {
        &self.fields
    }
}