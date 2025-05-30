use std::io::{self, Write, stdout};
use std::fmt;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::ops::{Add, Sub, Mul, Div, Rem, Neg};
use crate::env::core::config::{Config};
use std::any::Any;
use num_bigint::BigInt;
use num_bigfloat::BigFloat;
use num_traits::{ToPrimitive, FromPrimitive, Zero, One, Signed};
use std::str::FromStr;
use crate::env::core::functions::Function;
use once_cell::sync::Lazy;
use std::sync::{Mutex, Arc};
use crate::env::core::functions::{Parameter, NativeMethod, FunctionMetadata};
use crate::env::core::statements::Statement;
use crate::env::core::types::{Int, Float, Boolean};
use crate::env::core::value::{Value};
use crate::env::core::errors::Error;
use crate::env::core::variables::Variable;
use crossterm::{
    execute,
    terminal::{Clear, ClearType},
    cursor::MoveTo,
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
        Value::Float(n) => format_bigfloat(&n.value),
        Value::Int(n) => format_bigint(&n.value),
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

        Value::Bytes(bytes) => {
            let formatted_bytes: Vec<String> = bytes.iter().map(|b| format!("{:02x}", b)).collect();
            format!("b\"{}\"", formatted_bytes.join(" "))
        }

        Value::Function(func) => format!("<function '{}' at {:p}>", func.get_name(), func),

        Value::Error(err_type, err_msg) => format!("<{}: {}>", err_type, err_msg),
    }
}

fn format_bigfloat(n: &BigFloat) -> String {
    let s = n.to_string();

    if let Some(dot_pos) = s.find('.') {
        let (int_part, frac_part) = s.split_at(dot_pos);
        let trimmed_frac = frac_part.trim_end_matches('0');

        if trimmed_frac == "." {
            int_part.to_string()
        } else {
            format!("{}{}", int_part, trimmed_frac)
        }
    } else {
        s
    }
}

fn format_bigint(n: &BigInt) -> String {
    let s = n.to_string();
    if s.starts_with("-0") {
        "0".to_string()
    } else if s.trim_start_matches('0').is_empty() {
        "0".to_string()
    } else {
        s
    }
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
        print_colored(message, &config.color_scheme.debug, Some(use_colors));
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
            return_type: return_type.to_string(),
            is_public,
            is_static,
            is_final,
            is_native: true,
            state,
        },
    };

    Value::Function(Function::NativeMethod(Arc::new(method)))
}

pub const NULL: Value = Value::Null;
pub const TRUE: Value = Value::Boolean(true);
pub const FALSE: Value = Value::Boolean(false);