use std::io::{self, Write};
use crate::env::helpers::structs::Boolean;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::ops::{Add, Sub, Mul, Div, Rem, Neg};
use crate::env::helpers::config::{Config};
use std::any::Any;

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

pub struct Variable {
    name: String,
    value: Value,
    type_: String,
    is_static: bool,
    is_public: bool,
    is_final: bool,
}

impl Variable {
    pub fn new(name: String, value: Value, type_: String, is_static: bool, is_public: bool, is_final: bool) -> Self {
        Self {
            name,
            value,
            type_,
            is_static,
            is_public,
            is_final,
        }
    }

    pub fn get_value(&self) -> &Value {
        &self.value
    }

    pub fn set_value(&mut self, value: Value) {
        self.value = value;
    }

    pub fn get_type(&self) -> &str {
        &self.type_
    }

    pub fn get_name(&self) -> &str {
        &self.name
    }
    
    pub fn is_static(&self) -> bool {
        self.is_static
    }

    pub fn is_public(&self) -> bool {
        self.is_public
    }

    pub fn is_final(&self) -> bool {
        self.is_final
    }

    pub fn set_final(&mut self, is_final: bool) {
        self.is_final = is_final;
    }

    pub fn set_static(&mut self, is_static: bool) {
        self.is_static = is_static;
    }

    pub fn set_public(&mut self, is_public: bool) {
        self.is_public = is_public;
    }

    pub fn set_type(&mut self, type_: String) {
        self.type_ = type_;
    }

    pub fn set_name(&mut self, name: String) {
        self.name = name;
    }
}

pub struct Function {
    name: String,
    parameters: Vec<String>,
    return_type: String,    
    is_static: bool,
    is_public: bool,
    is_final: bool,
    is_builtin: bool,
    function: Box<dyn Fn(&[Box<dyn Any>]) -> Box<dyn Any>>,
}

impl Function {
    pub fn new(
        name: String,
        parameters: Vec<String>,
        return_type: String,
        is_static: bool,
        is_public: bool,
        is_final: bool,
        is_builtin: bool,
        function: Box<dyn Fn(&[Box<dyn Any>]) -> Box<dyn Any>>,
    ) -> Self {
        Self {
            name,
            parameters,
            return_type,
            is_static,
            is_public,
            is_final,
            is_builtin,
            function,
        }
    }

    pub fn execute(&self, args: Vec<Box<dyn Any>>) -> Box<dyn Any> {
        (self.function)(&args)
    }

    pub fn get_name(&self) -> &str {
        &self.name
    }
    pub fn get_parameters(&self) -> &Vec<String> {
        &self.parameters
    }

    pub fn get_return_type(&self) -> &str {
        &self.return_type
    }
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


#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Value {
    Float(f64),
    Int(i64),
    String(String),
    Boolean(bool),
    Null,
    Map {
        keys: Vec<Value>,
        values: Vec<Value>
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

impl Add for Value {
    type Output = Value;

    fn add(self, other: Value) -> Value {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a + b),
            (Value::Float(a), Value::Float(b)) => Value::Float(a + b),
            (Value::Int(a), Value::Float(b)) => Value::Float(a as f64 + b),
            (Value::Float(a), Value::Int(b)) => Value::Float(a + b as f64),
            (Value::String(a), Value::String(b)) => Value::String(a + &b),
            _ => Value::Null,
        }
    }
}

impl Sub for Value {
    type Output = Value;

    fn sub(self, other: Value) -> Value {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a - b),
            (Value::Float(a), Value::Float(b)) => Value::Float(a - b),
            (Value::Int(a), Value::Float(b)) => Value::Float(a as f64 - b),
            (Value::Float(a), Value::Int(b)) => Value::Float(a - b as f64),
            _ => Value::Null,
        }
    }
}

impl Mul for Value {
    type Output = Value;

    fn mul(self, other: Value) -> Value {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a * b),
            (Value::Float(a), Value::Float(b)) => Value::Float(a * b),
            (Value::Int(a), Value::Float(b)) => Value::Float(a as f64 * b),
            (Value::Float(a), Value::Int(b)) => Value::Float(a * b as f64),
            _ => Value::Null,
        }
    }
}

impl Div for Value {
    type Output = Value;

    fn div(self, other: Value) -> Value {
        match (self, other) {
            (Value::Int(_), Value::Int(0)) => Value::Null,
            (Value::Float(_), Value::Float(b)) if b == 0.0 => Value::Null,
            (Value::Int(a), Value::Int(b)) => Value::Int(a / b),
            (Value::Float(a), Value::Float(b)) => Value::Float(a / b),
            (Value::Int(a), Value::Float(b)) => {
                if b == 0.0 {
                    Value::Null
                } else {
                    Value::Float(a as f64 / b)
                }
            }
            (Value::Float(a), Value::Int(b)) => {
                if b == 0 {
                    Value::Null
                } else {
                    Value::Float(a / b as f64)
                }
            }
            _ => Value::Null,
        }
    }
}


impl Rem for Value {
    type Output = Value;

    fn rem(self, other: Value) -> Value {
        match (self, other) {
            (_, Value::Int(0)) => Value::Null,

            (_, Value::Float(b)) if b == 0.0 => Value::Null,

            (Value::Int(a), Value::Int(b)) => Value::Int(a % b),
            (Value::Float(a), Value::Float(b)) => Value::Float(a % b),
            (Value::Int(a), Value::Float(b)) => Value::Float(a as f64 % b),
            (Value::Float(a), Value::Int(b)) => Value::Float(a % b as f64),

            _ => Value::Null,
        }
    }
}

impl Neg for Value {
    type Output = Value;

    fn neg(self) -> Value {
        match self {
            Value::Int(a) => Value::Int(-a),
            Value::Float(a) => Value::Float(-a),
            _ => Value::Null,
        }
    }
}

impl Value {
    pub fn convert_to_statement(&self) -> Statement {
        match self {
            Value::Map { keys, values } => {
                let mut line = 0;
                let mut column = 0;

                let mut new_keys = Vec::new();
                let mut new_values = Vec::new();

                for (i, key) in keys.iter().enumerate() {
                    if let Value::String(s) = key {
                        if s == "_line" {
                            if let Some(Value::Int(val)) = values.get(i) {
                                line = *val as usize;
                                continue;
                            }
                        }
                        if s == "_column" {
                            if let Some(Value::Int(val)) = values.get(i) {
                                column = *val as usize;
                                continue;
                            }
                        }
                    }
                    new_keys.push(key.clone());
                    if let Some(value) = values.get(i) {
                        new_values.push(value.clone());
                    }
                }

                Statement::Statement {
                    keys: new_keys,
                    values: new_values,
                    line,
                    column,
                }
            }
            _ => Statement::Null,
        }
    }
    pub fn is_zero(&self) -> bool {
        match self {
            Value::Int(0) => true,
            Value::Float(f) if *f == 0.0 => true,
            _ => false,
        }
    }
    pub fn type_name_as_str(&self) -> &'static str {
        match self {
            Value::Float(_) => "float",
            Value::Int(_) => "int",
            Value::String(_) => "string",
            Value::Boolean(_) => "bool",
            Value::Null => "null",
            Value::Map { .. } => "map",
            Value::List(_) => "list",
            Value::ListCompletion { .. } => "list_completion",
        }
    }
    pub fn type_name(&self) -> String {
        match self {
            Value::Float(_) => "float".to_string(),
            Value::Int(_) => "int".to_string(),
            Value::String(_) => "string".to_string(),
            Value::Boolean(_) => "bool".to_string(),
            Value::Null => "null".to_string(),
            Value::Map { .. } => "map".to_string(),
            Value::List(_) => "list".to_string(),
            Value::ListCompletion { .. } => "list_completion".to_string(),
        }
    }
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Boolean(b) => *b,
            Value::Int(n) => *n != 0,
            Value::Float(f) => *f != 0.0,
            Value::String(s) => !s.is_empty(),
            Value::List(l) => !l.is_empty(),
            Value::Map { keys, .. } => !keys.is_empty(), // If no keys, assume empty
            Value::ListCompletion { pattern, .. } => !pattern.is_empty(),
            Value::Null => false,
        }
    }
}


#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Statement {
        keys: Vec<Value>,
        values: Vec<Value>,
        line: usize,
        column: usize,
    },
    Null,
}

impl Statement {
    pub fn convert_to_map(&self) -> Value {
        match self {
            Statement::Statement { keys, values, line, column } => {
                Value::Map {
                    keys: {
                        let mut new_keys = keys.clone();
                        new_keys.push(Value::String("_line".to_string()));
                        new_keys.push(Value::String("_column".to_string()));
                        new_keys
                    },
                    values: {
                        let mut new_values = values.clone();
                        new_values.push(Value::Int(*line as i64));
                        new_values.push(Value::Int(*column as i64));
                        new_values
                    },
                }
            },
            Statement::Null => {
                Value::Map {
                    keys: vec![
                        Value::String("_line".to_string()),
                        Value::String("_column".to_string())
                    ],
                    values: vec![
                        Value::Int(0),
                        Value::Int(0),
                    ],
                }
            },
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


pub fn format_value(value: &Value) -> String {
    match value {
        Value::Float(n) => format!("{}", *n as f64),
        Value::Int(n) => format!("{}", *n as i64),
        Value::String(s) => format!("\"{}\"", s),
        Value::Boolean(b) => format!("{}", b),
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
                .map(|(key, value)| format!("{}: {}", format_value(key), format_value(value)))
                .collect();
            format!("{{{}}}", formatted_pairs.join(", "))
        }
        Value::List(values) => {
            if values.is_empty() {
                "[]".to_string()
            } else {
                let formatted_values: Vec<String> = values.iter().map(|v| format_value(v)).collect();
                format!("[{}]", formatted_values.join(", "))
            }
        },
        Value::ListCompletion { pattern, end } => {
            let formatted_pattern: Vec<String> = pattern.iter().map(|v| format_value(v)).collect();
            let formatted_end = match end {
                Some(e) => format_value(e),
                None => "None".to_string(),
            };
            format!(
                "ListCompletion {{ pattern: [{}], end: {} }}",
                formatted_pattern.join(", "),
                formatted_end
            )
        }
    }
}

pub fn debug_log(message: &str, config: &Config, use_colors: Option<bool>) {
    let use_colors = use_colors.unwrap_or(true);
    if config.debug {
        print_colored(message, &config.color_scheme.debug, Some(use_colors));
    }
}


pub const NULL: Value = Value::Null;
pub const TRUE: Value = Value::Boolean(true);
pub const FALSE: Value = Value::Boolean(false);