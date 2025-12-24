use std::collections::HashMap;
use std::io::{self, Write};
use crate::env::runtime::functions::{Function, Parameter};
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
    replace_accented,
    sanitize_alias,
    get_type_default,
    convert_json_value_to_lucia_value,
};
use crate::env::runtime::variables::Variable;
use crate::env::runtime::internal_structs::EffectFlags;
use sha2::{Sha256, Digest};

use crate::insert_native_fn_state;

// This module provides various collection utilities that are deprecated and queued for removal in future versions.
// Lucia version 2.0.0, module: collections@2.0.0

pub fn add_deprecated_functions(map: &mut HashMap<String, Variable>) {
    insert_native_fn_state!(
        map,
        "clear_terminal",
        clear_terminal_handler,
        vec![],
        "void",
        "Deprecated: 'clear_terminal' was moved to 'os.terminal' as 'clear_screen' in v2.0.0, use it from there instead.",
        EffectFlags::IO
    );
    insert_native_fn_state!(
        map,
        "levenshtein_distance",
        levenshtein_distance_handler,
        vec![
            Parameter::positional("s1", "str"),
            Parameter::positional("s2", "str")
        ],
        "int",
        "Deprecated: 'levenshtein_distance' was moved to 'elevator.utils' in v2.0.0, use 'levenshtein_distance' from there instead.",
        EffectFlags::PURE
    );
    insert_native_fn_state!(
        map,
        "hex_to_ansi",
        hex_to_ansi_handler,
        vec![Parameter::positional("hex", "str")],
        "str",
        "Deprecated: 'hex_to_ansi' was moved to 'elevator.utils' in v2.0.0, use 'hex_to_ansi' from there instead.",
        EffectFlags::PURE
    );
    insert_native_fn_state!(
        map,
        "format_value",
        format_value_handler,
        vec![Parameter::positional("value", "any")],
        "str",
        "Deprecated: 'format_value' was moved to 'elevator.utils' in v2.0.0, use 'format_value' from there instead.",
        EffectFlags::PURE
    );
    insert_native_fn_state!(
        map,
        "format_string",
        format_string_handler,
        vec![
            Parameter::positional("format", "str"),
            Parameter::positional("args", "list")
        ],
        "str",
        "Deprecated: 'format_string' was removed in v2.0.0, use the built-in formatting instead (f-strings).",
        EffectFlags::PURE
    );
    insert_native_fn_state!(
        map,
        "unescape_string",
        unescape_string_handler,
        vec![Parameter::positional("s", "str")],
        "str",
        "Deprecated: 'unescape_string' was moved to 'elevator.utils' in v2.0.0, use 'unescape_string' from there instead.",
        EffectFlags::PURE
    );
    insert_native_fn_state!(
        map,
        "capitalize",
        capitalize_handler,
        vec![Parameter::positional("s", "str")],
        "str",
        "Deprecated: 'capitalize' was moved to 'str' as 'to_upper' in v2.0.0, use it from there instead.",
        EffectFlags::PURE
    );
    insert_native_fn_state!(
        map,
        "printto",
        print_to_handler,
        vec![
            Parameter::positional("s", "str"),
            Parameter::positional("stream", "int")
        ],
        "void",
        "Deprecated: 'printto' was moved to 'os.terminal' in v2.0.0, use 'printto' from there instead.",
        EffectFlags::IO
    );
    insert_native_fn_state!(
        map,
        "read_line",
        read_line_handler,
        vec![Parameter::positional("input_stream", "int")],
        "str",
        "Deprecated: 'read_line' was moved to 'fs' as 'read_from_fd' in v2.0.0, use it from there instead.",
        EffectFlags::IO
    );
    insert_native_fn_state!(
        map,
        "get_type_default",
        get_type_default_handler,
        vec![Parameter::positional("type", "str")],
        "any",
        "Deprecated: 'get_type_default' was moved removed in v2.0.0, use default constructors instead.",
        EffectFlags::PURE
    );
    insert_native_fn_state!(
        map,
        "replace_accented",
        replace_accented_handler,
        vec![Parameter::positional("s", "str")],
        "str",
        "Deprecated: 'replace_accented' was moved to 'elevator.utils' in v2.0.0, use 'replace_accented' from there instead.",
        EffectFlags::PURE
    );
    insert_native_fn_state!(
        map,
        "valid_alias",
        sanitize_alias_handler,
        vec![Parameter::positional("s", "str")],
        "str",
        "Deprecated: 'valid_alias' was moved to 'elevator.utils' in v2.0.0, use 'valid_alias' from there instead.",
        EffectFlags::PURE
    );
    insert_native_fn_state!(
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
        "str",
        "Deprecated: 'sha256' was moved to 'hash' in v2.0.0, use 'sha256' from there instead.",
        EffectFlags::PURE
    );
    insert_native_fn_state!(
        map,
        "range",
        range_handler,
        vec![
            Parameter::positional("a", "int"),
            Parameter::positional_optional_pt("b", &Type::new_simple("?int"), Value::Null),
            Parameter::positional_optional("step", "int", Value::Int(Int::from_i64(1)))
        ],
        "list",
        "deprecated: 'range' was added to builtins in v2.0.0, use 'range' from there instead.",
        EffectFlags::PURE
    );
    insert_native_fn_state!(
        map,
        "str_to_json",
        str_to_json,
        vec![Parameter::positional("s", "str")],
        "any",
        "Deprecated: 'str_to_json' was moved to 'json' as 'parse' in v2.0.0, use it from there instead.",
        EffectFlags::PURE
    );
}

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

fn str_to_json(args: &HashMap<String, Value>) -> Value {
    if let Some(Value::String(s)) = args.get("s") {
        match serde_json::from_str::<serde_json::Value>(s) {
            Ok(json_value) => convert_json_value_to_lucia_value(&json_value),
            Err(e) => Value::Error("JSONError", to_static(format!("failed to parse JSON: {}", e)), None),
        }
    } else {
        Value::Error("TypeError", "expected a string", None)
    }
}