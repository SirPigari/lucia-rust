use std::collections::HashMap;
use std::fs::{self, File, OpenOptions};
use std::io::{Read, Write};
use std::path::Path;
use std::sync::Arc;
use std::env as std_env;

use crate::env::runtime::functions::{Function, NativeFunction, Parameter};
use crate::env::runtime::types::{Float, Int};
use crate::env::runtime::utils::to_static;
use crate::env::runtime::value::Value;
use crate::env::runtime::variables::Variable;
use crate::insert_native_fn;

// This module provides file system operations and utilities.
// It includes functions for reading and writing files, checking file existence, and manipulating file paths.
// Lucia version 2.0.0, module: fs@0.4.0

fn write_file_handler(args: &HashMap<String, Value>) -> Value {
    if let (Some(Value::String(path)), Some(Value::String(content))) =
        (args.get("path"), args.get("content"))
    {
        match fs::write(path, content) {
            Ok(_) => Value::Null,
            Err(e) => Value::Error("IOError", to_static(e.to_string()), None),
        }
    } else {
        Value::Error("TypeError", "expected 'path' and 'content' as strings", None)
    }
}

fn read_file_handler(args: &HashMap<String, Value>) -> Value {
    if let Some(Value::String(path)) = args.get("path") {
        match fs::read_to_string(path) {
            Ok(data) => Value::String(data),
            Err(e) => Value::Error("IOError", to_static(e.to_string()), None),
        }
    } else {
        Value::Error("TypeError", "expected 'path' as string", None)
    }
}

fn append_file_handler(args: &HashMap<String, Value>) -> Value {
    if let (Some(Value::String(path)), Some(Value::String(content))) =
        (args.get("path"), args.get("content"))
    {
        match OpenOptions::new().append(true).create(true).open(path) {
            Ok(mut file) => {
                if let Err(e) = file.write_all(content.as_bytes()) {
                    Value::Error("IOError", to_static(e.to_string()), None)
                } else {
                    Value::Null
                }
            }
            Err(e) => Value::Error("IOError", to_static(e.to_string()), None),
        }
    } else {
        Value::Error("TypeError", "expected 'path' and 'content' as strings", None)
    }
}

fn file_exists_handler(args: &HashMap<String, Value>) -> Value {
    if let Some(Value::String(path)) = args.get("path") {
        let exists = Path::new(path).exists();
        Value::Boolean(exists)
    } else {
        Value::Error("TypeError", "expected 'path' as string", None)
    }
}

fn delete_file_handler(args: &HashMap<String, Value>) -> Value {
    if let Some(Value::String(path)) = args.get("path") {
        match fs::remove_file(path) {
            Ok(_) => Value::Null,
            Err(e) => Value::Error("IOError", to_static(e.to_string()), None),
        }
    } else {
        Value::Error("TypeError", "expected 'path' as string", None)
    }
}

fn make_dir_handler(args: &HashMap<String, Value>) -> Value {
    if let Some(Value::String(path)) = args.get("path") {
        match fs::create_dir_all(path) {
            Ok(_) => Value::Null,
            Err(e) => Value::Error("IOError", to_static(e.to_string()), None),
        }
    } else {
        Value::Error("TypeError", "expected 'path' as string", None)
    }
}

fn remove_dir_handler(args: &HashMap<String, Value>) -> Value {
    if let Some(Value::String(path)) = args.get("path") {
        match fs::remove_dir_all(path) {
            Ok(_) => Value::Null,
            Err(e) => Value::Error("IOError", to_static(e.to_string()), None),
        }
    } else {
        Value::Error("TypeError", "expected 'path' as string", None)
    }
}

fn read_dir_handler(args: &HashMap<String, Value>) -> Value {
    if let Some(Value::String(path)) = args.get("path") {
        match fs::read_dir(path) {
            Ok(read_dir) => {
                let mut entries = vec![];
                for entry in read_dir {
                    if let Ok(entry) = entry {
                        if let Some(name) = entry.file_name().to_str() {
                            entries.push(Value::String(name.to_string()));
                        }
                    }
                }
                Value::List(entries)
            }
            Err(e) => Value::Error("IOError", to_static(e.to_string()), None),
        }
    } else {
        Value::Error("TypeError", "expected 'path' as string", None)
    }
}

fn change_dir_handler(args: &HashMap<String, Value>) -> Value {
    if let Some(Value::String(path)) = args.get("path") {
        match std_env::set_current_dir(path) {
            Ok(_) => Value::Null,
            Err(e) => Value::Error("IOError", to_static(e.to_string()), None),
        }
    } else {
        Value::Error("TypeError", "expected 'path' as string", None)
    }
}

fn current_dir_handler(_: &HashMap<String, Value>) -> Value {
    match std_env::current_dir() {
        Ok(path) => Value::String(path.to_string_lossy().to_string()),
        Err(e) => Value::Error("IOError", to_static(e.to_string()), None),
    }
}

pub fn register() -> HashMap<String, Variable> {
    let mut map = HashMap::new();

    insert_native_fn!(
        map,
        "write_file",
        write_file_handler,
        vec![
            Parameter::positional("path", "str"),
            Parameter::positional("content", "str")
        ],
        "void"
    );
    insert_native_fn!(
        map,
        "read_file",
        read_file_handler,
        vec![Parameter::positional("path", "str")],
        "str"
    );
    insert_native_fn!(
        map,
        "append_file",
        append_file_handler,
        vec![
            Parameter::positional("path", "str"),
            Parameter::positional("content", "str")
        ],
        "void"
    );
    insert_native_fn!(
        map,
        "file_exists",
        file_exists_handler,
        vec![Parameter::positional("path", "str")],
        "bool"
    );
    insert_native_fn!(
        map,
        "delete_file",
        delete_file_handler,
        vec![Parameter::positional("path", "str")],
        "void"
    );
    insert_native_fn!(
        map,
        "make_dir",
        make_dir_handler,
        vec![Parameter::positional("path", "str")],
        "void"
    );
    insert_native_fn!(
        map,
        "remove_dir",
        remove_dir_handler,
        vec![Parameter::positional("path", "str")],
        "void"
    );
    insert_native_fn!(
        map,
        "read_dir",
        read_dir_handler,
        vec![Parameter::positional("path", "str")],
        "list"
    );
    insert_native_fn!(
        map,
        "change_dir",
        change_dir_handler,
        vec![Parameter::positional("path", "str")],
        "void"
    );
    insert_native_fn!(
        map,
        "current_dir",
        current_dir_handler,
        vec![],
        "str"
    );

    map
}
