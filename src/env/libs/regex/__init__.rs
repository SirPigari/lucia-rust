use std::collections::HashMap;
use std::sync::Arc;
use crate::env::runtime::functions::{Function, NativeFunction, Parameter};
use crate::env::runtime::types::{Float, Int};
use crate::env::runtime::value::Value;
use crate::env::runtime::utils::{to_static};
use crate::env::runtime::variables::Variable;
use regex::Regex;

use crate::{insert_native_fn, insert_native_var};

// This module provides regular expression matching capabilities.
// It includes functions for compiling, matching, and replacing patterns in strings.
// Lucia version 2.0.0, module: regex@0.9.0

fn regex_match(args: &HashMap<String, Value>) -> Value {
    let pattern = match args.get("pattern") {
        Some(Value::String(s)) => s,
        _ => return Value::Error("TypeError".into(), "expected a string for 'pattern'".into(), None),
    };

    let value = match args.get("value") {
        Some(Value::String(s)) => s,
        _ => return Value::Error("TypeError".into(), "expected a string for 'value'".into(), None),
    };

    match Regex::new(pattern) {
        Ok(re) => {
            match re.find(value) {
                Some(mat) => Value::String(mat.as_str().to_string()),
                None => Value::Null,
            }
        }
        Err(e) => Value::Error("RegexError".into(), to_static(format!("invalid regex pattern: {}", e)), None),
    }
}

fn regex_replace(args: &HashMap<String, Value>) -> Value {
    let pattern = match args.get("pattern") {
        Some(Value::String(s)) => s,
        _ => return Value::Error("TypeError".into(), "expected a string for 'pattern'".into(), None),
    };

    let value = match args.get("value") {
        Some(Value::String(s)) => s,
        _ => return Value::Error("TypeError".into(), "expected a string for 'value'".into(), None),
    };

    let replacement = match args.get("replacement") {
        Some(Value::String(s)) => s.as_str(),
        _ => return Value::Error("TypeError".into(), "expected a string for 'replacement'".into(), None),
    };

    match Regex::new(pattern) {
        Ok(re) => {
            let replaced = re.replace_all(value, replacement);
            Value::String(replaced.into_owned())
        }
        Err(e) => Value::Error("RegexError".into(), to_static(format!("invalid regex pattern: {}", e)), None),
    }
}


pub fn register() -> HashMap<String, Variable> {
    let mut map = HashMap::new();

    insert_native_fn!(
        map,
        "match",
        regex_match,
        vec![Parameter::positional("pattern", "str"), Parameter::positional("value", "str")],
        "any"
    );
    insert_native_fn!(
        map,
        "replace",
        regex_replace,
        vec![
            Parameter::positional("pattern", "str"),
            Parameter::positional("value", "str"),
            Parameter::positional_optional("replacement", "str", Value::String("".into()))
        ],
        "str"
    );

    map
}
