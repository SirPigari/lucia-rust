use std::collections::HashMap;
use std::sync::Arc;
use crate::env::runtime::functions::{Function, NativeFunction, Parameter};
use crate::env::runtime::types::{Float, Int};
use crate::env::runtime::value::Value;
use crate::env::runtime::utils::{
    to_static,
    clear_terminal,
    levenshtein_distance,
    hex_to_ansi,
    format_value,
    unescape_string,
    capitalize,
    get_type_default,
    unescape_string_literal,
    replace_accented,
    sanitize_alias,
    TRUE, FALSE, NULL,
};
use crate::env::runtime::variables::Variable;

use crate::{insert_native_fn, insert_native_var};

// This module provides various collection utilities.
// It includes functions for working with lists, sets, and maps.
// Lucia version 2.0.0, module: collections@1.0.0

fn clear_terminal_handler(_args: &HashMap<String, Value>) -> Value {
    clear_terminal();
    Value::Null
}

fn levenshtein_distance_handler(args: &HashMap<String, Value>) -> Value {
    if let (Some(Value::String(s1)), Some(Value::String(s2))) = (args.get("s1"), args.get("s2")) {
        Value::Int(Int::from_i64(levenshtein_distance(s1, s2) as i64))
    } else {
        Value::Error("TypeError", "expected two strings", None)
    }
}

fn panic_handler(args: &HashMap<String, Value>) -> Value {
    if let Some(Value::String(message)) = args.get("message") {
        panic!("{}", message);
    }
    Value::Error(
        "PanicError",
        "Panic called".into(),
        None,
    )
}

fn hex_to_ansi_handler(args: &HashMap<String, Value>) -> Value {
    if let Some(Value::String(hex)) = args.get("hex") {
        Value::String(hex_to_ansi(hex, Some(true)))
    } else {
        Value::Error("TypeError", "expected a string", None)
    }
}

fn format_value_handler(args: &HashMap<String, Value>) -> Value {
    if let Some(value) = args.get("value") {
        Value::String(format_value(value))
    } else {
        Value::Error("TypeError", "expected a value", None)
    }
}

fn unescape_string_handler(args: &HashMap<String, Value>) -> Value {
    if let Some(Value::String(s)) = args.get("s") {
        match unescape_string(s) {
            Ok(unescaped) => Value::String(unescaped),
            Err(e) => Value::Error("UnescapeError".into(), to_static(e), None),
        }
    } else {
        Value::Error("TypeError", "expected a string", None)
    }
}

fn capitalize_handler(args: &HashMap<String, Value>) -> Value {
    if let Some(Value::String(s)) = args.get("s") {
        Value::String(capitalize(s))
    } else {
        Value::Error("TypeError", "expected a string", None)
    }
}

fn get_type_default_handler(args: &HashMap<String, Value>) -> Value {
    if let Some(Value::String(typ)) = args.get("type") {
        get_type_default(typ)
    } else {
        Value::Error("TypeError", "expected a string", None)
    }
}

fn replace_accented_handler(args: &HashMap<String, Value>) -> Value {
    if let Some(Value::String(s)) = args.get("s") {
        let replaced: String = s.chars()
            .map(|c| replace_accented(c))
            .collect();
        Value::String(replaced)
    } else {
        Value::Error("TypeError".into(), "expected a string".into(), None)
    }
}

fn sanitize_alias_handler(args: &HashMap<String, Value>) -> Value {
    if let Some(Value::String(s)) = args.get("s") {
        Value::String(sanitize_alias(s))
    } else {
        Value::Error("TypeError", "expected a string", None)
    }
}

pub fn register() -> HashMap<String, Variable> {
    let mut map = HashMap::new();

    insert_native_fn!(
        map,
        "clear_terminal",
        clear_terminal_handler,
        vec![],
        "void"
    );
    insert_native_fn!(
        map,
        "panic",
        panic_handler,
        vec![Parameter::positional_optional("message", "str", "Panic called without a message".into())],
        "void"
    );
    insert_native_fn!(
        map,
        "levenshtein_distance",
        levenshtein_distance_handler,
        vec![
            Parameter::positional("s1", "str"),
            Parameter::positional("s2", "str")
        ],
        "int"
    );
    insert_native_fn!(
        map,
        "hex_to_ansi",
        hex_to_ansi_handler,
        vec![Parameter::positional("hex", "str")],
        "str"
    );
    insert_native_fn!(
        map,
        "format_value",
        format_value_handler,
        vec![Parameter::positional("value", "any")],
        "str"
    );
    insert_native_fn!(
        map,
        "unescape_string",
        unescape_string_handler,
        vec![Parameter::positional("s", "str")],
        "str"
    );
    insert_native_fn!(
        map,
        "capitalize",
        capitalize_handler,
        vec![Parameter::positional("s", "str")],
        "str"
    );
    insert_native_fn!(
        map,
        "get_type_default",
        get_type_default_handler,
        vec![Parameter::positional("type", "str")],
        "any"
    );
    insert_native_fn!(
        map,
        "replace_accented",
        replace_accented_handler,
        vec![Parameter::positional("s", "str")],
        "str"
    );
    insert_native_fn!(
        map,
        "valid_alias",
        sanitize_alias_handler,
        vec![Parameter::positional("s", "str")],
        "str"
    );

    insert_native_var!(
        map,
        "TRUE",
        TRUE,
        "bool"
    );
    insert_native_var!(
        map,
        "FALSE",
        FALSE,
        "bool"
    );
    insert_native_var!(
        map,
        "NULL",
        NULL,
        "null"
    );
    map
}
