use std::collections::HashMap;
use std::sync::Arc;
use std::io::{self, Write};
use crate::env::runtime::functions::{Function, NativeFunction, Parameter};
use crate::env::runtime::types::{Int, Type};
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
    replace_accented,
    sanitize_alias,
    TRUE, FALSE, NULL,
};
use crate::env::runtime::variables::Variable;
use sha2::{Sha256, Digest};

use crate::{insert_native_fn, insert_native_var};

// This module provides various collection utilities.
// It includes functions for working with lists, sets, and maps.
// Lucia version 2.0.0, module: collections@1.0.0

fn clear_terminal_handler(_args: &HashMap<String, Value>) -> Value {
    let _ = clear_terminal();
    Value::Null
}

fn sha256_hash(input: &str) -> String {
    let mut hasher = Sha256::new();
    hasher.update(input);
    let result = hasher.finalize();
    format!("{:x}", result)
}

fn levenshtein_distance_handler(args: &HashMap<String, Value>) -> Value {
    if let (Some(Value::String(s1)), Some(Value::String(s2))) = (args.get("s1"), args.get("s2")) {
        Value::Int(Int::from_i64(levenshtein_distance(s1, s2) as i64))
    } else {
        Value::Error("TypeError", "expected two strings", None)
    }
}

fn hex_to_ansi_handler(args: &HashMap<String, Value>) -> Value {
    if let Some(Value::String(hex)) = args.get("hex") {
        Value::String(hex_to_ansi(hex, true))
    } else {
        Value::Error("TypeError", "expected a string", None)
    }
}

fn format_string_handler(args: &HashMap<String, Value>) -> Value {
    if let Some(Value::String(format_str)) = args.get("format") {
        if let Some(Value::List(args_list)) = args.get("args") {
            let mut result = String::new();
            let mut chars = format_str.chars().peekable();
            let mut arg_index = 0;

            while let Some(c) = chars.next() {
                if c == '{' {
                    if let Some(&next) = chars.peek() {
                        if next == '{' {
                            chars.next();
                            result.push('{');
                        } else if next == '}' {
                            chars.next();
                            if let Some(arg) = args_list.get(arg_index) {
                                result.push_str(&format_value(arg));
                                arg_index += 1;
                            } else {
                                result.push_str("{}");
                            }
                        } else if next == ':' {
                            chars.next();
                            if let Some(&next2) = chars.peek() {
                                if next2 == '?' {
                                    chars.next();
                                    if let Some(&end) = chars.peek() {
                                        if end == '}' {
                                            chars.next();
                                            if let Some(arg) = args_list.get(arg_index) {
                                                result.push_str(&format!("{:?}", arg));
                                                arg_index += 1;
                                            } else {
                                                result.push_str("{:?}");
                                            }
                                        } else {
                                            result.push_str("{:?");
                                        }
                                    }
                                } else {
                                    result.push_str("{:");
                                    result.push(next2);
                                    chars.next();
                                }
                            } else {
                                result.push_str("{:");
                            }
                        } else {
                            result.push('{');
                        }
                    } else {
                        result.push('{');
                    }
                } else if c == '}' {
                    if let Some(&next) = chars.peek() {
                        if next == '}' {
                            chars.next();
                            result.push('}');
                        } else {
                            result.push('}');
                        }
                    } else {
                        result.push('}');
                    }
                } else {
                    result.push(c);
                }
            }

            Value::String(result)
        } else {
            Value::Error("TypeError", "expected a list of arguments", None)
        }
    } else {
        Value::Error("TypeError", "expected a format string", None)
    }
}

fn print_to_handler(args: &HashMap<String, Value>) -> Value {
    if let Some(Value::String(s)) = args.get("s") {
        if let Some(Value::Int(stream)) = args.get("stream") {
            match stream.to_i64() {
                Ok(1) => {
                    let _ = io::stdout().write_all(s.as_bytes());
                    let _ = io::stdout().flush();
                    Value::Null
                }
                Ok(2) => {
                    let _ = io::stderr().write_all(s.as_bytes());
                    let _ = io::stderr().flush();
                    Value::Null
                }
                Ok(3) => {
                    Value::Error("ValueError", "cannot write to stdin", None)
                }
                _ => Value::Error("ValueError", "invalid stream", None),
            }
        } else {
            Value::Error("TypeError", "expected an integer stream", None)
        }
    } else {
        Value::Error("TypeError", "expected a string", None)
    }
}

fn read_line_handler(args: &HashMap<String, Value>) -> Value {
    if let Some(Value::Int(input_stream)) = args.get("input_stream") {
        match input_stream.to_i64() {
            Ok(3) => {
                let mut input = String::new();
                if io::stdin().read_line(&mut input).is_ok() {
                    Value::String(input.trim_end().to_string())
                } else {
                    Value::Error("IOError", "failed to read from stdin", None)
                }
            }
            _ => Value::Error("ValueError", "invalid input stream", None),
        }
    } else {
        Value::Error("TypeError", "expected an integer input stream", None)
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

fn range_handler(args: &HashMap<String, Value>) -> Value {
    let (a, b) = if let Some(Value::Null) = args.get("b") {
        // `a` is actually the end, default start to 0
        let end = if let Some(Value::Int(i)) = args.get("a") {
            i.to_i64().unwrap_or(0)
        } else {
            return Value::Error("TypeError", "expected an integer for 'a'", None);
        };
        (0, end)
    } else {
        let start = if let Some(Value::Int(i)) = args.get("a") {
            i.to_i64().unwrap_or(0)
        } else {
            return Value::Error("TypeError", "expected an integer for 'a'", None);
        };
        let end = if let Some(Value::Int(i)) = args.get("b") {
            i.to_i64().unwrap_or(0)
        } else {
            return Value::Error("TypeError", "expected an integer for 'b'", None);
        };
        (start, end)
    };

    let step = if let Some(Value::Int(i)) = args.get("step") {
        i.to_i64().unwrap_or(1)
    } else {
        1
    };

    if step == 0 {
        return Value::Error("ValueError", "step cannot be zero", None);
    }

    let range: Vec<Value> = (a..b)
        .step_by(step as usize)
        .map(|x| Value::Int(imagnum::Int::from(x)))
        .collect();
    Value::List(range)
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
        "format_string",
        format_string_handler,
        vec![
            Parameter::positional("format", "str"),
            Parameter::positional("args", "list")
        ],
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
        "printto",
        print_to_handler,
        vec![
            Parameter::positional("s", "str"),
            Parameter::positional("stream", "int")
        ],
        "void"
    );
    insert_native_fn!(
        map,
        "read_line",
        read_line_handler,
        vec![Parameter::positional("input_stream", "int")],
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
    insert_native_fn!(
        map,
        "sha256",
        |args: &HashMap<String, Value>| {
            if let Some(Value::String(input)) = args.get("input") {
                Value::String(sha256_hash(input))
            } else {
                Value::Error("TypeError", "expected a string", None)
            }
        },
        vec![Parameter::positional("input", "str")],
        "str"
    );
    insert_native_fn!(
        map,
        "range",
        range_handler,
        vec![
            Parameter::positional("a", "int"),
            Parameter::positional_optional_pt("b", Type::new_simple("int").set_maybe_type(true), Value::Null),
            Parameter::positional_optional("step", "int", Value::Int(Int::from_i64(1)))
        ],
        "list"
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
