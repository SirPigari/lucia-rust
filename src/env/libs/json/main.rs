use std::collections::HashMap;
use std::fs::File;
use std::sync::Arc;

use crate::env::runtime::functions::{Function, Parameter};
use crate::env::runtime::types::{Float, Int};
use crate::env::runtime::utils::to_static;
use crate::env::runtime::value::Value;
use crate::env::runtime::variables::Variable;
use crate::env::runtime::internal_structs::EffectFlags;
use crate::insert_native_fn;

use serde_json::{self, Serializer, Value as JsonValue};
use serde::Serialize;

// This module provides JSON parsing and serialization functionality.
// It includes functions to parse JSON strings into structured data and serialize structured data into JSON strings.
// Lucia version 2.0.0, module: json@1.0.82

fn to_json_value(val: &Value) -> Result<JsonValue, String> {
    serde_json::to_value(val).map_err(|e| format!("{}", e))
}

fn from_json_value(jv: &JsonValue) -> Value {
    match jv {
        JsonValue::Null => Value::Null,
        JsonValue::Bool(b) => Value::Boolean(b.clone()),
        JsonValue::Number(n) => {
            if n.is_i64() || n.is_u64() {
                Value::Int(Int::from(n.as_i64().unwrap()))
            } else if let Ok(f) = Float::from_str(&n.to_string()) {
                Value::Float(f)
            } else {
                Value::Error("JSONError", to_static("invalid number".to_string()), None)
            }
        }
        JsonValue::String(s) => Value::String(s.clone()),
        JsonValue::Array(arr) => Value::List(arr.into_iter().map(from_json_value).collect()),
        JsonValue::Object(map) => {
            let keys = map.keys().map(|k| Value::String(k.clone())).collect();
            let values = map.values().map(from_json_value).collect();
            Value::Map { keys, values }
        }
    }
}

fn escape_handler(args: &HashMap<String, Value>) -> Value {
    let value = match args.get("value") {
        Some(v) => v,
        None => return Value::Error("ValueError", "missing 'value'", None),
    };

    match to_json_value(value) {
        Ok(json_val) => {
            let mut buf = Vec::new();
            serde_json::to_writer(&mut buf, &json_val).unwrap();
            Value::String(String::from_utf8(buf).unwrap())
        }
        Err(e) => Value::Error("JSONError", to_static(e), None),
    }
}

fn unescape_handler(args: &HashMap<String, Value>) -> Value {
    let value = match args.get("value") {
        Some(Value::String(s)) => s,
        _ => return Value::Error("TypeError", "expected string for 'value'", None),
    };

    match serde_json::from_str::<JsonValue>(value) {
        Ok(json_val) => from_json_value(&json_val),
        Err(e) => Value::Error("JSONError", to_static(e.to_string()), None),
    }
}

pub fn register() -> HashMap<String, Variable> {
    let mut map = HashMap::new();

    insert_native_fn!(
        map,
        "stringify",
        |args: &HashMap<String, Value>| -> Value {
            let value = match args.get("value") {
                Some(v) => v,
                None => return Value::Error("ValueError", "missing 'value'", None),
            };

            let indent = match args.get("indent") {
                Some(Value::Int(i)) => match i.to_i64() {
                    Ok(val) => Some(val as usize),
                    Err(_) => return Value::Error("TypeError", "expected int for 'indent'", None),
                },
                
                Some(_) => return Value::Error("TypeError", "expected int for 'indent'", None),
                None => None,
            };

            match to_json_value(value) {
                Ok(json_val) => {
                    let mut buf = Vec::new();
                    if let Some(indent) = indent {
                        let indent_vec = vec![b' '; indent];
                        let formatter = serde_json::ser::PrettyFormatter::with_indent(indent_vec.as_slice());
                        let mut ser = Serializer::with_formatter(&mut buf, formatter);
                        json_val.serialize(&mut ser).unwrap();
                    } else {
                        serde_json::to_writer(&mut buf, &json_val).unwrap();
                    }
                    Value::String(String::from_utf8(buf).unwrap())
                }
                Err(e) => Value::Error("JSONError", to_static(e), None),
            }
        },
        vec![
            Parameter::positional("value", "any"),
            Parameter::positional_optional("indent", "int", Value::Int(Int::from_i64(0))),
        ],
        "str",
        EffectFlags::PURE
    );

    insert_native_fn!(
        map,
        "parse",
        |args: &HashMap<String, Value>| -> Value {
            let text = match args.get("text") {
                Some(Value::String(s)) => s,
                _ => return Value::Error("TypeError", "expected string for 'text'", None),
            };

            match serde_json::from_str::<JsonValue>(text) {
                Ok(json_val) => from_json_value(&json_val),
                Err(e) => Value::Error("JSONError", to_static(e.to_string()), None),
            }
        },
        vec![Parameter::positional("text", "str")],
        "any",
        EffectFlags::PURE
    );

    insert_native_fn!(
        map,
        "dump",
        |args: &HashMap<String, Value>| -> Value {
            let path = match args.get("path") {
                Some(Value::String(s)) => s,
                _ => return Value::Error("TypeError", "expected string for 'path'", None),
            };

            let value = match args.get("value") {
                Some(v) => v,
                None => return Value::Error("ValueError", "missing 'value'", None),
            };

            let indent = match args.get("indent") {
                Some(Value::Int(i)) => match i.to_i64() {
                    Ok(val) => Some(val as usize),
                    Err(_) => return Value::Error("TypeError", "expected int for 'indent'", None),
                },

                Some(_) => return Value::Error("TypeError", "expected int for 'indent'", None),
                None => None,
            };

            let json_val = match to_json_value(value) {
                Ok(v) => v,
                Err(e) => return Value::Error("JSONError", to_static(e), None),
            };

            let file = File::create(path);
            if file.is_err() {
                return Value::Error("IOError", "failed to open file", None);
            }
            let mut file = file.unwrap();

            let result = if let Some(indent) = indent {
                let indent_vec = vec![b' '; indent];
                let formatter = serde_json::ser::PrettyFormatter::with_indent(indent_vec.as_slice());
                let mut ser = Serializer::with_formatter(&mut file, formatter);                
                json_val.serialize(&mut ser)
            } else {
                serde_json::to_writer(&mut file, &json_val)
            };

            match result {
                Ok(_) => Value::Null,
                Err(e) => Value::Error("JSONError", to_static(e.to_string()), None),
            }
        },
        vec![
            Parameter::positional("path", "str"),
            Parameter::positional("value", "any"),
            Parameter::positional_optional("indent", "int", Value::Int(Int::from_i64(0))),
        ],
        "void",
        EffectFlags::IO
    );
    insert_native_fn!(
        map,
        "escape",
        escape_handler,
        vec![Parameter::positional("value", "any")],
        "str",
        EffectFlags::PURE
    );
    insert_native_fn!(
        map,
        "unescape",
        unescape_handler,
        vec![Parameter::positional("value", "str")],
        "any",
        EffectFlags::PURE
    );

    map
}
