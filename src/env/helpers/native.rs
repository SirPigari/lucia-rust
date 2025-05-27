use crate::env::helpers::utils::{Value, to_static, self};
use crate::env::helpers::functions::{Function, FunctionMetadata, NativeFunction, Parameter};
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

    let mut text = if let Some(Value::String(s)) = values.get(0) {
        s.clone()
    } else {
        values.get(0).map_or("".to_string(), |v| format_value(v))
    };

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
                style += &format!("\x1b[48;2;{};{};{}m", r, g, b);
            }
        }
    }

    if matches!(args.get("bold"), Some(Value::Boolean(true))) {
        style += "\x1b[1m";
    }
    if matches!(args.get("italic"), Some(Value::Boolean(true))) {
        style += "\x1b[3m";
    }
    if matches!(args.get("underline"), Some(Value::Boolean(true))) {
        style += "\x1b[4m";
    }
    if matches!(args.get("blink"), Some(Value::Boolean(true))) {
        style += "\x1b[5m";
    }
    if matches!(args.get("reverse"), Some(Value::Boolean(true))) {
        style += "\x1b[7m";
    }
    if matches!(args.get("strikethrough"), Some(Value::Boolean(true))) {
        style += "\x1b[9m";
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

    print(&print_args)
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

fn exit(args: &HashMap<String, Value>) -> Value {
    let code = args.get("code").map_or(0, |val| match val {
        Value::Int(n) => n.to_i64().unwrap_or(0),
        Value::Float(f) => f.round().to_i64().unwrap_or(0),
        Value::String(s) => s.parse::<i64>().unwrap_or(0),
        _ => 0,
    });

    std::process::exit(code as i32);
    Value::Int(code.into())
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


// -------------------------------
// Utility Functions
fn format_value(value: &Value) -> String {
    match value {
        Value::Float(n) => {
            let n_str = n.to_string();
            if let Some(dot_pos) = n_str.find('.') {
                let mut trimmed = n_str.trim_start_matches('0');
                if trimmed.starts_with('.') {
                    trimmed = &trimmed[1..];
                }
                let trimmed = trimmed.trim_end_matches('0');
                if trimmed == "" {
                    "0".to_string()
                } else {
                    trimmed.to_string()
                }
            } else {
                n_str.trim_start_matches('0').to_string()
            }
        }
        Value::Int(n) => {
            let trimmed = n.to_string().trim_start_matches('0').to_string();
            if trimmed == "" {
                "0".to_string()
            } else {
                trimmed
            }
        }
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
            Parameter::keyword_variadic_optional("end", "str", Value::String("\n".to_string())),
            Parameter::keyword_variadic_optional("fg_color", "str", Value::String("#000000".to_string())),
            Parameter::keyword_variadic_optional("bg_color", "str", Value::String("#FFFFFF".to_string())),
            Parameter::keyword_variadic_optional("bold", "bool", Value::Boolean(false)),
            Parameter::keyword_variadic_optional("italic", "bool", Value::Boolean(false)),
            Parameter::keyword_variadic_optional("underline", "bool", Value::Boolean(false)),
            Parameter::keyword_variadic_optional("blink", "bool", Value::Boolean(false)),
            Parameter::keyword_variadic_optional("reverse", "bool", Value::Boolean(false)),
            Parameter::keyword_variadic_optional("strikethrough", "bool", Value::Boolean(false)),
            Parameter::keyword_variadic_optional("link", "str", Value::String("".to_string())),
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

pub fn exit_fn() -> Function {
    Function::Native(Arc::new(NativeFunction::new(
        "exit",
        exit,
        vec![
            Parameter::keyword_variadic_optional("code", "int", Value::Int(0.into())),
        ],
        "void",
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
