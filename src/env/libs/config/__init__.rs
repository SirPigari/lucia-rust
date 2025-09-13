use std::collections::HashMap;
use std::sync::Arc;

use crate::env::runtime::functions::{Function, NativeFunction, Parameter};
use crate::env::runtime::types::{Int};
use crate::env::runtime::value::Value;
use crate::env::runtime::variables::Variable;
use crate::env::runtime::config::{Config, get_from_config};
use crate::{insert_native_fn, insert_native_var};

// This module provides configuration management functionality for the Lucia environment.
// It includes functions to register and initialize configuration variables.
// Lucia version 2.0.0, module: config@0.2.6

fn set(_args: &HashMap<String, Value>) -> Value {
    Value::Error(
        "NotImplementedError",
        "set function is not implemented",
        None,
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

    let config_for_get = config.clone(); // clone Arc for closure use

    insert_native_fn!(
        map,
        "get",
        move |args: &HashMap<String, Value>| -> Value {
            let key = match args.get("key") {
                Some(Value::String(s)) => s.as_str(),
                _ => return Value::Error("TypeError", "Expected a string for 'key'", None),
            };
            get_from_config(&config_for_get, key)
        },
        vec![Parameter::positional("key", "str")],
        "any"
    );

    // Now use original config for vars below safely
    insert_native_var!(
        map,
        "moded",
        Value::Boolean(config.moded),
        "bool"
    );
    insert_native_var!(
        map,
        "debug",
        Value::Boolean(config.debug),
        "bool"
    );
    insert_native_var!(
        map,
        "debug_mode",
        Value::String(config.debug_mode.clone()),
        "str"
    );
    insert_native_var!(
        map,
        "supports_color",
        Value::Boolean(config.supports_color),
        "bool"
    );
    insert_native_var!(
        map,
        "use_lucia_traceback",
        Value::Boolean(config.use_lucia_traceback),
        "bool"
    );
    insert_native_var!(
        map,
        "warnings",
        Value::Boolean(config.warnings),
        "bool"
    );
    insert_native_var!(
        map,
        "allow_fetch",
        Value::Boolean(config.allow_fetch),
        "bool"
    );
    insert_native_var!(
        map,
        "allow_unsafe",
        Value::Boolean(config.allow_unsafe),
        "bool"
    );
    insert_native_var!(
        map,
        "home_dir",
        Value::String(config.home_dir.clone()),
        "str"
    );
    insert_native_var!(
        map,
        "stack_size",
        Value::Int(Int::from_i64(config.stack_size as i64)),
        "int"
    );
    insert_native_var!(
        map,
        "version",
        Value::String(config.version.clone()),
        "str"
    );
    insert_native_var!(
        map,
        "color_scheme",
        Value::Map {
            keys: vec![
                Value::String("exception".to_string()),
                Value::String("warning".to_string()),
                Value::String("help".to_string()),
                Value::String("debug".to_string()),
                Value::String("input_arrows".to_string()),
                Value::String("note".to_string()),
                Value::String("output_text".to_string()),
                Value::String("info".to_string()),
            ],
            values: vec![
                Value::String(config.color_scheme.exception.clone()),
                Value::String(config.color_scheme.warning.clone()),
                Value::String(config.color_scheme.help.clone()),
                Value::String(config.color_scheme.debug.clone()),
                Value::String(config.color_scheme.input_arrows.clone()),
                Value::String(config.color_scheme.note.clone()),
                Value::String(config.color_scheme.output_text.clone()),
                Value::String(config.color_scheme.info.clone()),
            ],
        },
        "map"
    );

    map
}
