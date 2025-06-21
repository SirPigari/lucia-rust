use std::collections::HashMap;
use std::sync::Arc;

use crate::env::runtime::functions::{Function, NativeFunction, Parameter};
use crate::env::runtime::types::{Float, Int};
use crate::env::runtime::utils::to_static;
use crate::env::runtime::value::Value;
use crate::env::runtime::variables::Variable;
use crate::env::runtime::config::{Config, get_from_config};
use crate::insert_native_fn;

// This module provides configuration management functionality for the Lucia environment.
// It includes functions to register and initialize configuration variables.
// Lucia version 2.0.0, module: config@0.2.6

fn set(args: &HashMap<String, Value>) -> Value {
    Value::Error(
        "NotImplementedError",
        "set function is not implemented",
    )
}

pub fn register(config: Arc<Config>) -> HashMap<String, Variable> {
    let mut map = HashMap::new();

    insert_native_fn!(
        map,
        "set",
        set,
        vec![Parameter::positional("key", "str"), Parameter::positional("value", "any")],
        "void"
    );
    insert_native_fn!(
        map,
        "get",
        move |args: &HashMap<String, Value>| -> Value {
            let key = match args.get("key") {
                Some(Value::String(s)) => s.as_str(),
                _ => return Value::Error("TypeError", "Expected a string for 'key'"),
            };
            get_from_config(&config, key)
        },
        vec![Parameter::positional("key", "str")],
        "any"
    );

    map
}

pub fn init() -> Value {
    Value::Null
}