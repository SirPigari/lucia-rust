use crate::env::runtime::types::Type;
use crate::env::runtime::value::Value;
use crate::env::runtime::utils::format_value;
use serde::{Serialize, Deserialize};
use bincode::{Encode, Decode};
use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize, Encode, Decode)]
pub struct Enum {
    pub ty: Type,
    pub variant: (String, Box<Value>),
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize, Encode, Decode)]
pub struct Struct {
    pub ty: Type,
    pub fields: HashMap<String, Box<Value>>,
}

impl Enum {
    pub fn new(ty: Type, variant: (String, Value)) -> Self {
        Self { ty, variant: (variant.0, Box::new(variant.1)) }
    }

    pub fn display(&self) -> String {
        format!("{}.{}{}", self.ty.display_simple(), self.variant.0, if matches!(*self.variant.1, Value::Null) { "".to_string() } else { format!("({})", format_value(&self.variant.1)) })
    }

    pub fn get_type(&self) -> Type {
        self.ty.clone()
    }

    pub fn get_size(&self) -> usize {
        std::mem::size_of::<Self>() + self.variant.1.get_size()
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Self { variant: (_, v), ty: _ } => !matches!(**v, Value::Null),
        }
    }

    pub fn ptr(&self) -> *const Self {
        self as *const Self
    }
}

impl PartialOrd for Enum {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.variant.partial_cmp(&other.variant)
    }
}

impl PartialOrd for Struct {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        let mut self_vec: Vec<_> = self.fields.iter().collect();
        let mut other_vec: Vec<_> = other.fields.iter().collect();
        self_vec.sort_by(|a, b| a.0.cmp(b.0));
        other_vec.sort_by(|a, b| a.0.cmp(b.0));
        self_vec.partial_cmp(&other_vec)
    }
}

impl Struct {
    pub fn new(ty: Type) -> Self {
        Self {
            ty,
            fields: HashMap::new(),
        }
    }

    pub fn display(&self) -> String {
        format!("{} {{ {} }}", self.ty.display_simple(), self.fields.iter()
            .map(|(k, v)| format!("{} = {}", k, format_value(&v)))
            .collect::<Vec<_>>().join(", "))
    }

    pub fn get_type(&self) -> Type {
        self.ty.clone()
    }

    pub fn get_size(&self) -> usize {
        std::mem::size_of::<Self>() + self.fields.iter().map(|(k, v)| k.len() + v.get_size()).sum::<usize>()
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Self { fields, .. } if fields.is_empty() => false,
            _ => true,
        }
    }

    pub fn ptr(&self) -> *const Self {
        self as *const Self
    }

    pub fn get(&self, field: &str) -> Option<&Value> {
        self.fields.get(field).map(|v| v.as_ref())
    }
    pub fn set(&mut self, field: String, value: Value) -> Option<Box<Value>> {
        self.fields.insert(field, Box::new(value))
    }
    pub fn remove(&mut self, field: &str) -> Option<Box<Value>> {
        self.fields.remove(field)
    }
    pub fn fields(&self) -> &HashMap<String, Box<Value>> {
        &self.fields
    }
}