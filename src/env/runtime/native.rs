use crate::env::runtime::utils::{to_static, capitalize, format_float, format_int, self};
use crate::env::runtime::value::Value;
use crate::env::runtime::types::{Int, Float};
use crate::env::runtime::functions::{Function, FunctionMetadata, NativeFunction, Parameter};
use serde_json::json;
use crate::env::runtime::config::{get_config, get_version};
use std::collections::HashMap;
use std::sync::Arc;
use std::io::{self, Write};
use std::str::FromStr;



// -------------------------------
// Function Implementations
fn print(args: &HashMap<String, Value>) -> Value {
    let sep = match args.get("sep") {
        Some(Value::String(s)) => s.clone(),
        Some(other) => format_value(other),
        None => " ".to_string(),
    };

    let end = match args.get("end") {
        Some(Value::String(s)) => s.clone(),
        Some(other) => format_value(other),
        None => "\n".to_string(),
    };

    let values = match args.get("args") {
        Some(Value::List(list)) => list,
        _ => &vec![],
    };

    let mut out = String::new();
    for (i, val) in values.iter().enumerate() {
        if i > 0 {
            out.push_str(&sep);
        }
        out.push_str(&format_value(val));
    }

    out.push_str(&end);
    print!("{}", out);

    Value::Null
}

fn styled_print(args: &HashMap<String, Value>) -> Value {
    let values = match args.get("args") {
        Some(Value::List(list)) => list,
        _ => &vec![],
    };

    let sep = match args.get("sep") {
        Some(Value::String(s)) => s.clone(),
        Some(other) => format_value(other),
        None => " ".to_string(),
    };

    let mut text = values
        .iter()
        .map(|v| format_value(v))
        .collect::<Vec<_>>()
        .join(sep.as_str());

    let mut style = String::new();

    if let Some(Value::String(fg)) = args.get("fg_color") {
        style += &utils::hex_to_ansi(fg, Some(true));
    }

    if let Some(Value::String(bg)) = args.get("bg_color") {
        let hex = if bg.starts_with('#') { &bg[1..] } else { bg };
        if hex.len() == 6 {
            if let (Ok(r), Ok(g), Ok(b)) = (
                u8::from_str_radix(&hex[0..2], 16),
                u8::from_str_radix(&hex[2..4], 16),
                u8::from_str_radix(&hex[4..6], 16),
            ) {
                style.push_str(&format!("\x1b[48;2;{};{};{}m", r, g, b));
            }
        }
    }

    if matches!(args.get("bold"), Some(Value::Boolean(true))) {
        style.push_str("\x1b[1m");
    }
    if matches!(args.get("italic"), Some(Value::Boolean(true))) {
        style.push_str("\x1b[3m");
    }
    if matches!(args.get("underline"), Some(Value::Boolean(true))) {
        style.push_str("\x1b[4m");
    }
    if matches!(args.get("blink"), Some(Value::Boolean(true))) {
        style.push_str("\x1b[5m");
    }
    if matches!(args.get("reverse"), Some(Value::Boolean(true))) {
        style.push_str("\x1b[7m");
    }
    if matches!(args.get("strikethrough"), Some(Value::Boolean(true))) {
        style.push_str("\x1b[9m");
    }

    if let Some(Value::String(link)) = args.get("link") {
        text = format!("[{}]({})", text, link);
    }

    let reset = "\x1b[0m";
    let styled_text = format!("{}{}{}", style, text, reset);

    let mut print_args = HashMap::new();
    print_args.insert("args".to_string(), Value::List(vec![Value::String(styled_text)]));
    if let Some(end_val) = args.get("end") {
        print_args.insert("end".to_string(), end_val.clone());
    }

    print(&print_args);
    return Value::Null;
}

fn input(args: &HashMap<String, Value>) -> Value {
    let prompt = match args.get("prompt") {
        Some(Value::String(s)) => s,
        Some(v) => &format_value(v),
        None => "",
    };

    print!("{}", prompt);
    io::stdout().flush().unwrap();

    let mut buffer = String::new();
    match io::stdin().read_line(&mut buffer) {
        Ok(_) => Value::String(buffer.trim_end().to_string()),
        Err(_) => Value::Error("IOError", "Failed to read input"),
    }
}

fn len(args: &HashMap<String, Value>) -> Value {
    match args.get("v") {
        Some(Value::List(list)) => Value::Int(list.len().into()),
        Some(Value::String(s)) => Value::Int(s.chars().count().into()),
        Some(Value::Map { keys, .. }) => Value::Int(keys.len().into()),
        Some(v @ Value::Int(_) | v @ Value::Float(_)) => {
            Value::Int(format_value(v).len().into())
        }
        Some(v) => {
            let msg = format!(
                "Function 'len()' doesn't support type '{}'",
                v.type_name()
            );
            Value::Error("TypeError", to_static(msg))
        }
        None => Value::Null,
    }
}

// i need that rn 
fn help(args: &HashMap<String, Value>) -> Value {
    let info = json!({
        "print": {
            "type": "function",
            "description": "Prints the given values to the standard output.",
            "args": {
                "args": {
                    "type": "*any",
                    "description": "Values to print."
                },
                "end": {
                    "type": "str",
                    "description": "String to append after printing (default: '\\n')."
                },
                "sep": {
                    "type": "str",
                    "description": "String to separate values (default: ' ')."
                }
            }
        },
        "exit": {
            "type": "function",
            "description": "Exits the program.",
            "example": "exit()"
        }
    });

    if let Some(Value::String(obj)) = args.get("object") {
        let obj_name = obj.trim_end_matches("()");
        if let Some(obj_info) = info.get(obj_name) {
            if let Some(obj_type) = obj_info.get("type").and_then(|v| v.as_str()) {
                println!("Help for '{}':", obj_name);
                println!("  Type: {}", obj_type);
            }

            if let Some(desc) = obj_info.get("description").and_then(|v| v.as_str()) {
                println!("  Description: {}", desc);
            }

            if let Some(example) = obj_info.get("example").and_then(|v| v.as_str()) {
                println!("  Example: {}", example);
            }

            if let Some(args) = obj_info.get("args").and_then(|v| v.as_object()) {
                println!("  Arguments:");
                for (arg_name, arg_info) in args {
                    if let Some(arg_map) = arg_info.as_object() {
                        let ty = arg_map.get("type").and_then(|v| v.as_str()).unwrap_or("unknown");
                        let desc = arg_map.get("description").and_then(|v| v.as_str()).unwrap_or("");
                        println!("    - {}: {} ({})", arg_name, ty, desc);
                    }
                }
            }
        } else {
            println!("No help available for '{}'", obj_name);
            println!("This object may not exist or is not documented.");
            return Value::Null;
        }

        return Value::Null;
    }

    let version = get_version();

    let content = format!(r#"Welcome to Lucia-{}!

If you're new to Lucia, start with the tutorial:
https://github.com/SirPigari/lucia/tree/main/env/Docs/introduction.md

- Need help? Enter help(object="object name") to get information about a function or object.
- Want to see available modules or keywords? Use modules() or keywords().
- Ready to exit? Type exit() to leave the Lucia REPL.

Happy coding!"#, version);

    println!("{}", content);
    Value::Null
}

fn type_name(args: &HashMap<String, Value>) -> Value {
    if let Some(value) = args.get("obj") {
        return Value::String(value.type_name().to_string());
    }
    Value::Error("TypeError", "No value provided for type_name()")
}

fn sum(args: &HashMap<String, Value>) -> Value {
    if let Some(Value::List(values)) = args.get("args") {
        let mut total = Float::from_f64(0.0);
        let mut stack = values.iter().collect::<Vec<_>>();

        while let Some(value) = stack.pop() {
            match value {
                Value::Int(i) => {
                    match i.to_i64() {
                        Ok(n) => {
                            total += Float::from_f64(n as f64);
                        }
                        Err(_) => {
                            return Value::Error("TypeError", "Failed to convert BigInt to f64");
                        }
                    }
                }
                Value::Float(f) => {
                    total += f.clone();
                }
                Value::List(ls) | Value::Tuple(ls) => {
                    for v in ls {
                        stack.push(v);
                    }
                }
                _ => return Value::Error("TypeError", "Value is not summable"),
            }
        }

        Value::Float(total)
    } else {
        Value::Error("TypeError", "Expected a list of numeric values")
    }
}

fn __placeholder__(args: &HashMap<String, Value>) -> Value {
    Value::Error("PlaceholderError", "This is a placeholder function and should not be called.")
}

// -------------------------------
// Utility Functions
fn format_value(value: &Value) -> String {
    match value {
        Value::Float(n) => format_float(&n.clone()),
        Value::Int(n) => format_int(&n.clone()),
        Value::String(s) => s.clone(),
        Value::Boolean(b) => b.to_string(),
        Value::Null => "null".to_string(),
        Value::Map { keys, values, .. } => {
            let formatted_pairs: Vec<String> = keys
                .iter()
                .zip(values.iter())
                .filter(|(key, _)| {
                    if let Value::String(s) = key {
                        s != "_line" && s != "_column"
                    } else {
                        true
                    }
                })
                .map(|(key, value)| format!("{}: {}", utils::format_value(key), utils::format_value(value)))
                .collect();
            format!("{{{}}}", formatted_pairs.join(", "))
        }
        Value::List(values) => {
            if values.is_empty() {
                "[]".to_string()
            } else {
                let formatted_values: Vec<String> = values.iter().map(utils::format_value).collect();
                format!("[{}]", formatted_values.join(", "))
            }
        }
        Value::Tuple(values) => {
            if values.is_empty() {
                "()".to_string()
            } else {
                let formatted_values: Vec<String> = values.iter().map(utils::format_value).collect();
                format!("({})", formatted_values.join(", "))
            }
        }
        Value::Bytes(bytes) => {
            match String::from_utf8(bytes.clone()) {
                Ok(decoded) => decoded,
                Err(_) => format!("<invalid utf-8: {:?}>", bytes),
            }
        }        
        Value::Function(func) => {
            format!("<function '{}' at {:p}>", func.get_name(), func)
        }
        Value::Error(err_type, err_msg) => {
            format!("<{}: {}>", err_type, err_msg)
        }
        _ => "<unsupported value type>".to_string(),
    }
}

// -------------------------------
// Function Definitions
pub fn print_fn() -> Function {
    Function::Native(Arc::new(NativeFunction::new(
        "print",
        print,
        vec![  
            Parameter::variadic_optional("args", "any", Value::String("".to_string())), 
            Parameter::positional_optional("end", "str", Value::String("\n".to_string())),
            Parameter::positional_optional("sep", "str", Value::String(" ".to_string())),
        ],
        "void",
        true, true, true,
        None,
    )))
}

pub fn styled_print_fn() -> Function {
    Function::Native(Arc::new(NativeFunction::new(
        "styled_print",
        styled_print,
        vec![
            Parameter::variadic_optional("args", "any", Value::String("".to_string())),
            Parameter::keyword_optional("sep", "str", Value::String(" ".to_string())),
            Parameter::keyword_optional("end", "str", Value::String("\n".to_string())),
            Parameter::keyword_optional("fg_color", "str", Value::String("reset".to_string())),
            Parameter::keyword_optional("bg_color", "str", Value::String("reset".to_string())),
            Parameter::keyword_optional("bold", "bool", Value::Boolean(false)),
            Parameter::keyword_optional("italic", "bool", Value::Boolean(false)),
            Parameter::keyword_optional("underline", "bool", Value::Boolean(false)),
            Parameter::keyword_optional("blink", "bool", Value::Boolean(false)),
            Parameter::keyword_optional("reverse", "bool", Value::Boolean(false)),
            Parameter::keyword_optional("strikethrough", "bool", Value::Boolean(false)),
            Parameter::keyword_optional("link", "any", Value::Null),
        ],
        "void",
        true, true, true,
        None,
    )))
}

pub fn input_fn() -> Function {
    Function::Native(Arc::new(NativeFunction::new(
        "input",
        input,
        vec![
            Parameter::positional_optional("prompt", "str", Value::String("".to_string())),
        ],
        "str",
        true, true, true,
        None,
    )))
}

pub fn len_fn() -> Function {
    Function::Native(Arc::new(NativeFunction::new(
        "len",
        len,
        vec![
            Parameter::positional("v", "any"),
        ],
        "int",
        true, true, true,
        None,
    )))
}

pub fn help_fn() -> Function {
    Function::Native(Arc::new(NativeFunction::new(
        "help",
        help,
        vec![
            Parameter::positional_optional("object", "any", Value::Null),
        ],
        "void",
        true, true, true,
        None,
    )))
}

pub fn type_fn() -> Function {
    Function::Native(Arc::new(NativeFunction::new(
        "type",
        type_name,
        vec![
            Parameter::positional("obj", "any"),
        ],
        "str",
        true, true, true,
        None,
    )))
}

pub fn sum_fn() -> Function {
    Function::Native(Arc::new(NativeFunction::new(
        "sum",
        sum,
        vec![
            Parameter::variadic_optional("args", "any", Value::Int(0.into())), 
            Parameter::keyword_optional("start", "any", Value::Int(0.into())),
        ],
        "float",
        true, true, true,
        None,
    )))
}

pub fn placeholder_fn() -> Function {
    Function::Native(Arc::new(NativeFunction::new(
        "__placeholder__",
        __placeholder__,
        vec![
            Parameter::variadic_optional("args", "any", Value::Null),
            Parameter::keyword_optional("kwargs", "any", Value::Null),
        ],
        "void",
        true, true, true,
        None,
    )))
}