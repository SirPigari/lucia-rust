use crate::env::runtime::value::Value;
use std::collections::HashMap;
use serde::{Serialize, Deserialize};
use bincode::{Encode, Decode};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum State {
    Normal,
    Exit,
    Defer,
    Break,
    Continue,
}

#[derive(Debug, Clone, Serialize, Deserialize, Encode, Decode)]
pub struct Cache {
    pub operations: HashMap<String, Value>,
    pub constants: HashMap<String, Value>,
    pub iterables: HashMap<Value, Value>,
}

#[derive(Debug, Clone)]
pub struct InternalStorage {
    pub types: HashMap<String, Value>,
    pub lambda_counter: usize,
}

#[derive(Serialize)]
pub struct BuildInfo {
    pub name: &'static str,
    pub version: &'static str,
    pub uuid: &'static str,
    pub rustc_version: &'static str,
    pub rustc_channel: &'static str,
    pub target: &'static str,
    pub repository: &'static str,
    pub git_hash: &'static str,
    pub file_hash: &'static str,
    pub profile: &'static str,
    pub ci: &'static str,
    pub build_date: &'static str,
    pub dependencies: &'static str,
}