use crate::env::runtime::variables::Variable;
use crate::env::runtime::functions::Parameter;
use std::collections::HashMap;
use std::path::PathBuf;

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub name: String,
    pub properties: HashMap<String, Variable>,
    pub parameters: Vec<Parameter>,
    pub is_public: bool,
    pub is_static: bool,
    pub is_final: bool,
    pub state: Option<String>,
    pub path: PathBuf,
}

impl PartialOrd for Module {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.name().cmp(other.name()))
    }
}

#[allow(dead_code)]
impl Module {
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn ptr(&self) -> *const () {
        self as *const _ as *const ()
    }

    pub fn is_final(&self) -> bool {
        self.is_final
    }

    pub fn get_properties(&self) -> &HashMap<String, Variable> {
        &self.properties
    }

    pub fn get_parameters(&self) -> &[Parameter] {
        &self.parameters
    }

    pub fn get_property(&self, key: &str) -> Option<&Variable> {
        self.get_properties().get(key)
    }

    pub fn get_properties_mut(&mut self) -> &mut HashMap<String, Variable> {
        &mut self.properties
    }

    pub fn set_property(&mut self, key: String, value: Variable) -> Option<()> {
        self.get_properties_mut().insert(key, value);
        Some(())
    }

    pub fn get_size(&self) -> usize {
        std::mem::size_of::<Module>() + self.get_properties().len() * std::mem::size_of::<Variable>()
    }

    pub fn path(&self) -> &PathBuf {
        &self.path
    }
}
