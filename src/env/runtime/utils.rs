use std::io::{self, Write, stdout};
use std::fmt::{self, Write as FmtWrite};
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::ops::{Add, Sub, Mul, Div, Rem, Neg};
use crate::env::runtime::config::{Config};
use std::any::Any;
use std::str::FromStr;
use crate::env::runtime::functions::Function;
use once_cell::sync::Lazy;
use std::sync::{Mutex, Arc};
use std::cmp::Ordering;
use crate::env::runtime::functions::{Parameter, NativeMethod, NativeFunction, FunctionMetadata, UserFunction};
use crate::env::runtime::statements::Statement;
use crate::env::runtime::types::{Int, Float, Boolean};
use crate::env::runtime::value::{Value};
use crate::env::runtime::errors::Error;
use crate::env::runtime::variables::Variable;
use crossterm::{
    execute,
    terminal::{Clear, ClearType},
    cursor::MoveTo,
};
use imagnum::math::{
    ERR_UNIMPLEMENTED,
    ERR_INVALID_FORMAT,
    ERR_DIV_BY_ZERO,
    ERR_NEGATIVE_RESULT,
    ERR_NEGATIVE_SQRT,
    ERR_NUMBER_TOO_LARGE,
    ERR_INFINITE_RESULT,
};


static ERROR_CACHE: Lazy<Mutex<HashMap<String, &'static str>>> = Lazy::new(|| Mutex::new(HashMap::new()));

pub fn to_static(s: String) -> &'static str {
    let mut cache = ERROR_CACHE.lock().unwrap();
    if let Some(&static_ref) = cache.get(&s) {
        return static_ref;
    }
    let static_ref: &'static str = Box::leak(s.clone().into_boxed_str());
    cache.insert(s, static_ref);
    static_ref
}

pub fn levenshtein_distance(s1: &str, s2: &str) -> usize {
    let (s1, s2) = if s1.len() > s2.len() { (s2, s1) } else { (s1, s2) };
    let mut costs = (0..=s1.len()).collect::<Vec<_>>();
    for (i, c2) in s2.char_indices() {
        let mut last_value = i;
        for (j, c1) in s1.char_indices() {
            let new_value = if c1 == c2 {
                costs[j]
            } else {
                std::cmp::min(
                    std::cmp::min(costs[j + 1], last_value),
                    costs[j] + 1,
                )
            };
            costs[j] = last_value;
            last_value = new_value;
        }
    }
    costs[s1.len()]
}

pub fn get_line_info(source: &str, line_number: usize) -> Option<String> {
    source.lines().nth(line_number.saturating_sub(1)).map(|s| s.to_string())
}

pub fn clear_terminal() -> Result<(), io::Error> {
    let mut stdout = stdout();
    execute!(
        stdout,
        Clear(ClearType::All),
        MoveTo(0, 0)
    )?;
    stdout.flush()?;
    Ok(())
}

pub fn hex_to_ansi(hex_color: &str, use_colors: Option<bool>) -> String {
    let use_colors = use_colors.unwrap_or(true);

    if !use_colors {
        return "".to_string();
    }

    if hex_color == "reset" {
        return "\x1b[0m".to_string();
    }

    let hex = if hex_color.starts_with('#') { &hex_color[1..] } else { hex_color };

    if hex.len() == 6 {
        let r = u8::from_str_radix(&hex[0..2], 16).unwrap();
        let g = u8::from_str_radix(&hex[2..4], 16).unwrap();
        let b = u8::from_str_radix(&hex[4..6], 16).unwrap();
        return format!("\x1b[38;2;{};{};{}m", r, g, b);
    }

    "\x1b[0m".to_string()
}

pub fn print_colored(message: &str, color: &str, use_colors: Option<bool>) {
    let use_colors = use_colors.unwrap_or(true);
    let colored_message = format!("{}{}{}", hex_to_ansi(color, Some(use_colors)), message, hex_to_ansi("reset", Some(use_colors)));
    println!("{}", colored_message);
}

pub fn read_input(prompt: &str) -> String {
    print!("{}", prompt);
    io::stdout().flush().unwrap();
    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap();
    input.trim().to_string()
}

pub fn format_value(value: &Value) -> String {
    match value {
        Value::Float(n) => format_float(&n),
        Value::Int(n) => format_int(&n),
        Value::String(s) => format!("\"{}\"", s),
        Value::Boolean(b) => b.to_string(),
        Value::Null => "null".to_string(),

        Value::Map { keys, values, .. } => {
            let formatted_pairs: Vec<String> = keys
                .iter()
                .zip(values.iter())
                .filter(|(key, _)| match key {
                    Value::String(s) => s != "_line" && s != "_column",
                    _ => true,
                })
                .map(|(key, value)| format!("{}: {}", format_value(key), format_value(value)))
                .collect();
            format!("{{{}}}", formatted_pairs.join(", "))
        }

        Value::List(values) => {
            let formatted_values: Vec<String> = values.iter().map(format_value).collect();
            format!("[{}]", formatted_values.join(", "))
        }

        Value::Tuple (values) => {
            let formatted_values: Vec<String> = values.iter().map(format_value).collect();
            format!("({})", formatted_values.join(", "))
        }

        Value::Bytes(bytes) => {
            let formatted_bytes: Vec<String> = bytes.iter().map(|b| format!("{:02x}", b)).collect();
            format!("b\"{}\"", formatted_bytes.join(" "))
        }

        Value::Function(func) => {
            format!("<function '{}' at {:p}>", func.get_name(), func.ptr())
        }
        

        Value::Error(err_type, err_msg) => format!("<{}: {}>", err_type, err_msg),
    }
}

pub fn format_float(f: &Float) -> String {
    f.to_string()
}

pub fn format_int(n: &Int) -> String {
    n.to_string()
}

pub fn find_closest_match<'a>(target: &str, options: &'a [String]) -> Option<&'a str> {
    fn levenshtein(a: &str, b: &str) -> usize {
        let mut costs = vec![0; b.len() + 1];
        for j in 0..=b.len() {
            costs[j] = j;
        }

        for (i, ca) in a.chars().enumerate() {
            let mut last = i;
            costs[0] = i + 1;
            for (j, cb) in b.chars().enumerate() {
                let old = costs[j + 1];
                costs[j + 1] = std::cmp::min(
                    std::cmp::min(costs[j] + 1, costs[j + 1] + 1),
                    last + if ca == cb { 0 } else { 1 },
                );
                last = old;
            }
        }
        costs[b.len()]
    }

    let mut closest: Option<(&str, usize)> = None;

    for opt in options {
        let dist = levenshtein(target, opt);
        if closest.is_none() || dist < closest.unwrap().1 {
            closest = Some((opt.as_str(), dist));
        }
    }

    match closest {
        Some((s, dist)) if dist <= 2 && dist < target.len() => Some(s),
        _ => None,
    }
}

pub fn check_ansi<'a>(ansi: &'a str, use_colors: &bool) -> &'a str {
    if !*use_colors {
        &ansi[0..0]
    } else {
        ansi
    }
}

pub fn debug_log(message: &str, config: &Config, use_colors: Option<bool>) {
    let use_colors = use_colors.unwrap_or(true);
    if config.debug {
        let single_line_message = message.replace('\n', "\\n").replace('\r', "\\r").replace('\t', "\\t").replace('\0', "\\0").replace('\x1b', "\\e");
        print_colored(&single_line_message, &config.color_scheme.debug, Some(use_colors));
    }
}


pub fn capitalize(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
        None => String::new(),
    }
}

pub fn unescape_string(s: &str) -> Result<String, String> {
    if s.len() < 2 {
        return Err("String too short to unescape".into());
    }

    let first = s.chars().next().unwrap();
    let last = s.chars().last().unwrap();

    if (first == '"' && last == '"') || (first == '\'' && last == '\'') {
        let inner = &s[1..s.len() - 1];

        let json_quoted = format!("\"{}\"", inner);

        match serde_json::from_str::<String>(&json_quoted) {
            Ok(unescaped) => Ok(unescaped),
            Err(e) => Err(format!("Failed to unescape string: {}", e)),
        }
    } else {
        Err("String not properly quoted".into())
    }
}

pub fn make_native_method<F>(
    name: &str,
    func: F,
    parameters: Vec<Parameter>,
    return_type: &str,
    is_public: bool,
    is_static: bool,
    is_final: bool,
    state: Option<String>,
) -> Value
where
    F: Fn(&HashMap<String, Value>) -> Value + Send + Sync + 'static,
{
    let method = NativeMethod {
        func: Arc::new(func),
        meta: FunctionMetadata {
            name: name.to_string(),
            parameters,
            return_type: Value::String(return_type.to_string()),
            is_public,
            is_static,
            is_final,
            is_native: true,
            state,
        },
    };

    Value::Function(Function::NativeMethod(Arc::new(method)))
}

pub fn make_native_function<F>(
    name: &str,
    func: F,
    parameters: Vec<Parameter>,
    return_type: &str,
    is_public: bool,
    is_static: bool,
    is_final: bool,
    state: Option<String>,
) -> Value
where
    F: Fn(&HashMap<String, Value>) -> Value + Send + Sync + 'static,
{
    let method = NativeFunction {
        func: Arc::new(func),
        meta: FunctionMetadata {
            name: name.to_string(),
            parameters,
            return_type: Value::String(return_type.to_string()),
            is_public,
            is_static,
            is_final,
            is_native: true,
            state,
        },
    };

    Value::Function(Function::Native(Arc::new(method)))
}

pub fn get_operator_precedence(op: &str) -> u8 {
    match op {
        "||" => 1,
        "&&" => 2,
        "==" | "!=" => 3,
        "<" | ">" | "<=" | ">=" => 4,
        "+" | "-" => 5,
        "*" | "/" => 6,
        _ => 0,
    }
}

pub fn get_type_default(type_: &str) -> Value {
    match type_ {
        "float" => Value::Float(0.0.into()),
        "int" => Value::Int(0.into()),
        "string" => Value::String(String::new()),
        "bool" => Value::Boolean(false),
        "any" => Value::Null,
        "map" => Value::Map { keys: vec![], values: vec![] },
        "list" => Value::List(vec![]),
        "bytes" => Value::Bytes(vec![]),
        "tuple" => Value::Tuple(vec![]),
        "void" => Value::Null,
        _ => Value::Null,
    }
}

pub fn get_type_default_as_statement(type_: &str) -> Statement {
    match type_ {
        "int" => Statement::Statement {
            keys: vec![
                Value::String("type".to_string()),
                Value::String("value".to_string())
            ],
            values: vec![
                Value::String("NUMBER".to_string()),
                Value::String("0".to_string())
            ],
            line: 0,
            column: 0,
        },
        "float" => Statement::Statement {
            keys: vec![
                Value::String("type".to_string()),
                Value::String("value".to_string())
            ],
            values: vec![
                Value::String("NUMBER".to_string()),
                Value::String("0.0".to_string())
            ],
            line: 0,
            column: 0,
        },
        "str" => Statement::Statement {
            keys: vec![
                Value::String("type".to_string()),
                Value::String("value".to_string()),
                Value::String("mods".to_string()),
            ],
            values: vec![
                Value::String("STRING".to_string()),
                Value::String("\"\"".to_string()),
                Value::List(vec![]),
            ],
            line: 0,
            column: 0,
        },
        "bool" => Statement::Statement {
            keys: vec![
                Value::String("type".to_string()),
                Value::String("value".to_string())
            ],
            values: vec![
                Value::String("BOOLEAN".to_string()),
                Value::String("false".to_string()),
            ],
            line: 0,
            column: 0,
        },
        "any" => Statement::Statement {
            keys: vec![
                Value::String("type".to_string()),
                Value::String("value".to_string())
            ],
            values: vec![
                Value::String("BOOLEAN".to_string()),
                Value::String("null".to_string()),
            ],
            line: 0,
            column: 0,
        },
        "map" => Statement::Statement {
            keys: vec![
                Value::String("type".to_string()),
                Value::String("keys".to_string()),
                Value::String("values".to_string())
            ],
            values: vec![
                Value::String("MAP".to_string()),
                Value::List(vec![]),
                Value::List(vec![])
            ],
            line: 0,
            column: 0,
        },
        "list" => Statement::Statement {
            keys: vec![
                Value::String("type".to_string()),
                Value::String("iterable_type".to_string()),
                Value::String("elements".to_string())
            ],
            values: vec![
                Value::String("ITERABLE".to_string()),
                Value::String("LIST".to_string()),
                Value::List(vec![])
            ],
            line: 0,
            column: 0,
        },
        "bytes" => Statement::Statement {
            keys: vec![
                Value::String("type".to_string()),
                Value::String("value".to_string()),
                Value::String("mods".to_string()),
            ],
            values: vec![
                Value::String("STRING".to_string()),
                Value::String("\"\"".to_string()),
                Value::List(vec![Value::String("b".to_string())])
            ],
            line: 0,
            column: 0,
        },
        "tuple" => Statement::Statement {
            keys: vec![
                Value::String("type".to_string()),
                Value::String("elements".to_string())
            ],
            values: vec![
                Value::String("TUPLE".to_string()),
                Value::List(vec![])
            ],
            line: 0,
            column: 0,
        },
        "void" => Statement::Statement {
            keys: vec![
                Value::String("type".to_string()),
                Value::String("value".to_string())
            ],
            values: vec![
                Value::String("BOOLEAN".to_string()),
                Value::String("null".to_string()),
            ],
            line: 0,
            column: 0,
        },
        _ => Statement::Statement {
            keys: vec![
                Value::String("type".to_string()),
                Value::String("value".to_string())
            ],
            values: vec![
                Value::String("BOOLEAN".to_string()),
                Value::String("null".to_string()),
            ],
            line: 0,
            column: 0,
        },
    }
}

pub fn get_type_default_as_statement_from_statement(type_: &Statement) -> Statement {
    let binding = type_.convert_to_hashmap();
    let default = Value::String("any".to_string());

    let type_name = binding.get(&Value::String("value".to_string())).unwrap_or(&default);

    match type_name {
        Value::String(type_str) => get_type_default_as_statement(&type_str),
        _ => get_type_default_as_statement("any"),
    }
}


pub fn get_imagnum_error_message(err: i16) -> String {
    match err {
        ERR_DIV_BY_ZERO => "Division by zero.".to_string(),
        ERR_NEGATIVE_RESULT => "Negative result.".to_string(),
        ERR_NEGATIVE_SQRT => "Square root of a negative number.".to_string(),
        ERR_NUMBER_TOO_LARGE => "Number too large to convert.".to_string(),
        ERR_INFINITE_RESULT => "Result is infinite.".to_string(),
        ERR_UNIMPLEMENTED => "This operation is not implemented.".to_string(),
        ERR_INVALID_FORMAT => "Invalid format.".to_string(),
        _ => "Unknown error.".to_string(),
    }
}

pub fn create_function(metadata: FunctionMetadata, body: Vec<Statement>) -> Value {
    Value::Function(Function::Custom(UserFunction {
        meta: metadata,
        body,
    }.into()))
}

pub fn create_note(text: &str, use_colors: Option<bool>, note_color: &str) -> String {
    let use_colors = use_colors.unwrap_or(false);
    format!(
        "{}{}Note:{} {}{}",
        hex_to_ansi(note_color, Some(use_colors)),
        check_ansi("\x1b[1m", &use_colors),
        check_ansi("\x1b[22m", &use_colors),
        text,
        hex_to_ansi("reset", Some(use_colors))
    )
}

pub fn format_type(value: &Value) -> String {
    fn as_str(v: &Value) -> Option<&str> {
        if let Value::String(s) = v {
            Some(s)
        } else {
            None
        }
    }

    match value {
        Value::String(s) => s.clone(),
        Value::Map { keys, values } => {
            let mut map = HashMap::new();

            for (k, v) in keys.iter().zip(values.iter()) {
                if let Some(key_str) = as_str(k) {
                    map.insert(key_str, v);
                }
            }

            let type_kind = map.get("type_kind").and_then(|v| as_str(*v));

            let elements = match map.get("elements") {
                Some(Value::Map { .. }) => vec![],
                Some(Value::String(_)) => vec![map.get("elements").and_then(|v| as_str(*v)).unwrap().to_string()],
                Some(_) => vec![],
                None => vec![],
            };

            let base = map.get("base").and_then(|v| as_str(*v));
            let variadic = map.get("variadic").and_then(|v| {
                if let Value::String(s) = v {
                    Some(s == "true")
                } else {
                    None
                }
            }).unwrap_or(false);

            let return_type = map.get("return_type");

            match type_kind {
                Some("indexed") => {
                    if let Some(base) = base {
                        if !elements.is_empty() {
                            format!("{}[{}]", base, elements.join(","))
                        } else {
                            base.to_string()
                        }
                    } else {
                        "unknown".to_string()
                    }
                }

                Some("function") => {
                    let elems = if !elements.is_empty() {
                        format!("[{}]", elements.join(","))
                    } else {
                        "".to_string()
                    };

                    let ret_str = if let Some(Value::Map { keys: rt_keys, values: rt_values }) = return_type {
                        let mut rt_map = std::collections::HashMap::new();
                        for (k, v) in rt_keys.iter().zip(rt_values.iter()) {
                            if let Some(key_str) = as_str(k) {
                                rt_map.insert(key_str, v);
                            }
                        }

                        if rt_map.get("type_kind").and_then(|v| as_str(*v)) == Some("simple") {
                            if let Some(Value::String(val)) = rt_map.get("value") {
                                let mut o = "".to_string();
                                if !(val == "any" || val == "void") {
                                    o = format!(" -> {}", val);
                                };
                                o
                            } else {
                                "".to_string()
                            }
                        } else {
                            "".to_string()
                        }
                    } else {
                        "".to_string()
                    };

                    format!("function{}{}", elems, ret_str)
                }

                Some("simple") => {
                    if let Some(Value::String(val)) = map.get("value") {
                        val.to_string()
                    } else {
                        "unknown".to_string()
                    }
                }

                _ => {
                    map.get("type").and_then(|v| as_str(*v)).unwrap_or("unknown").to_string()
                }
            }
        }
        Value::Null => "void".to_string(),
        _ => {
            "unknown".to_string()
        }
    }
}


pub const NULL: Value = Value::Null;
pub const TRUE: Value = Value::Boolean(true);
pub const FALSE: Value = Value::Boolean(false);