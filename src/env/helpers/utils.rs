use std::io::{self, Write};
use crate::env::helpers::structs::Boolean;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};

#[derive(Debug, Clone)]
pub struct Error {
    pub error_type: String,
    pub msg: String,
    pub line: (usize, String),
    pub column: usize,
}

impl Error {
    pub fn new(error_type: &str, msg: &str) -> Self {
        Self {
            error_type: error_type.to_string(),
            msg: msg.to_string(),
            line: (0, "<unknown>".to_string()),
            column: 0,
        }
    }

    pub fn with_position(error_type: &str, msg: &str, line: usize, line_text: &str, column: usize) -> Self {
        Self {
            error_type: error_type.to_string(),
            msg: msg.to_string(),
            line: (line, line_text.to_string()),
            column,
        }
    }

    pub fn error_type(&self) -> &str {
        &self.error_type
    }

    pub fn msg(&self) -> &str {
        &self.msg
    }
}

impl From<&str> for Error {
    fn from(msg: &str) -> Self {
        Error::new("UnknownError", msg)
    }
}


#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Float(f64),
    Int(i64),
    String(String),
    Boolean(bool),
    Null,
    Map {
        keys: Vec<Value>,
        values: Vec<Value>,
        line: usize,
        column: usize,
    },
    List(Vec<Value>),
    ListCompletion { pattern: Vec<Value>, end: Option<Box<Value>> },
}

impl Eq for Value {}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match *self {
            Value::Float(n) => {
                0u8.hash(state);
                n.to_bits().hash(state);
            }
            Value::Int(n) => {
                1u8.hash(state);
                n.hash(state);
            }
            Value::String(ref s) => s.hash(state),
            Value::Boolean(b) => b.hash(state),
            Value::Null => 0.hash(state),
            Value::Map { ref keys, ref values, .. } => {
                keys.hash(state);
                values.hash(state);
            }
            Value::List(ref v) => v.hash(state),
            Value::ListCompletion { ref pattern, ref end } => {
                pattern.hash(state);
                end.hash(state);
            }
        }
    }
}

pub fn get_line_info(source: &str, line_number: usize) -> Option<String> {
    source.lines().nth(line_number.saturating_sub(1)).map(|s| s.to_string())
}


pub fn clear_terminal() {
    print!("{}[2J", 27 as char);
    io::stdout().flush().unwrap();
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
