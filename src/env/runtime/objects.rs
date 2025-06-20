use crate::env::runtime::variables::Variable;
use crate::env::runtime::functions::Parameter;
use crate::env::runtime::value::Value;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct ObjectMetadata {
    pub name: String,
    pub properties: HashMap<String, Variable>,
    pub parameters: Vec<Parameter>,
    pub is_public: bool,
    pub is_static: bool,
    pub is_final: bool,
    pub state: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Class {
    pub name: String,
    pub meta: ObjectMetadata,
}

impl Class {
    pub fn new(name: String, meta: ObjectMetadata) -> Self {
        Class { name, meta }
    }

    pub fn metadata(&self) -> &ObjectMetadata {
        &self.meta
    }

    pub fn is_final(&self) -> bool {
        self.meta.is_final
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Class(Class),
    Instance(Class, HashMap<String, Variable>),
}

impl PartialOrd for Object {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.name().cmp(other.name()))
    }
}

impl Object {
    pub fn name(&self) -> &str {
        match self {
            Object::Class(c) => &c.name,
            Object::Instance(c, _) => &c.name,
        }
    }

    pub fn ptr(&self) -> *const () {
        match self {
            Object::Class(c) => c as *const _ as *const (),
            Object::Instance(c, _) => c as *const _ as *const (),
        }
    }

    pub fn metadata(&self) -> &ObjectMetadata {
        match self {
            Object::Class(c) => &c.meta,
            Object::Instance(c, _) => &c.meta,
        }
    }

    pub fn is_final(&self) -> bool {
        self.metadata().is_final
    }

    pub fn is_instance(&self) -> bool {
        matches!(self, Object::Instance(..))
    }

    pub fn is_class(&self) -> bool {
        matches!(self, Object::Class(..))
    }

    pub fn get_parent_name(&self) -> Option<&str> {
        match self {
            Object::Class(c) => Some(&c.meta.name),
            Object::Instance(c, _) => Some(&c.meta.name),
        }
    }

    pub fn get_properties(&self) -> Option<&HashMap<String, Variable>> {
        match self {
            Object::Instance(_, props) => Some(props),
            Object::Class(c) => Some(&c.meta.properties),
            _ => None,
        }
    }

    pub fn get_parameters(&self) -> Option<&[Parameter]> {
        match self {
            Object::Class(c) => Some(&c.meta.parameters),
            Object::Instance(c, _) => Some(&c.meta.parameters),
        }
    }

    pub fn get_property(&self, key: &str) -> Option<&Variable> {
        self.get_properties()?.get(key)
    }

    pub fn set_property(&mut self, key: String, value: Variable) -> Option<()> {
        match self {
            Object::Instance(_, props) => {
                props.insert(key, value);
                Some(())
            }
            _ => None,
        }
    }
}
