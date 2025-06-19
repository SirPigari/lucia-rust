use crate::env::runtime::variables::Variable;
use crate::env::runtime::functions::Parameter;
use crate::env::runtime::value::Value;
use std::collections::HashMap;

pub struct ObjectMetadata {
    pub name: String,
    pub properties: HashMap<String, Variable>,
    pub parameters: Vec<Parameter>,
    pub is_public: bool,
    pub is_static: bool,
    pub is_final: bool,
    pub state: Option<String>,
}

struct Class {
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

pub enum Object {
    Class(Class),
    Instance(Class, HashMap<String, Variable>),
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

    pub fn get_properties(&self) -> Option<&HashMap<String, Variable>> {
        match self {
            Object::Instance(_, props) => Some(props),
            _ => None,
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
