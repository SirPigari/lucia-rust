use std::io::{self, Write};
use std::fmt;
use crate::env::helpers::structs::Boolean;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::ops::{Add, Sub, Mul, Div, Rem, Neg};
use crate::env::helpers::config::{Config};
use std::any::Any;
use num_bigint::BigInt;
use num_bigfloat::BigFloat;
use num_traits::{ToPrimitive, FromPrimitive, Zero, One, Signed};
use std::str::FromStr;
use crate::env::helpers::functions::Function;
use once_cell::sync::Lazy;
use std::sync::Mutex;

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


#[derive(Clone, PartialEq, PartialOrd, Eq, Debug)]
pub struct Int {
    pub value: BigInt,
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub struct Float {
    pub value: BigFloat,
}

impl From<usize> for Value {
    fn from(i: usize) -> Self {
        Value::Int(Int { value: BigInt::from(i) })
    }
}

impl From<usize> for Int {
    fn from(i: usize) -> Self {
        Self { value: BigInt::from(i) }
    }
}

impl From<i32> for Int {
    fn from(i: i32) -> Self {
        Self { value: BigInt::from(i) }
    }
}

impl From<i64> for Int {
    fn from(i: i64) -> Self {
        Self { value: BigInt::from(i) }
    }
}

impl From<f64> for Float {
    fn from(f: f64) -> Self {
        Self { value: BigFloat::from_f64(f) }
    }
}

impl From<Float> for Int {
    fn from(f: Float) -> Self {
        Self { value: BigInt::from_f64(f.value.to_f64()).unwrap() }
    }
}

impl From<Int> for Float {
    fn from(i: Int) -> Self {
        Self { value: BigFloat::from_i64(i.value.to_i64().unwrap()) }
    }
}

impl From<BigFloat> for Int {
    fn from(f: BigFloat) -> Self {
        Self { value: BigInt::from_f64(f.to_f64()).unwrap() }
    }
}

impl From<BigInt> for Float {
    fn from(i: BigInt) -> Self {
        Self { value: BigFloat::from_i64(i.to_i64().unwrap()) }
    }
}

impl Hash for Int {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.value.to_str_radix(10).hash(state);
    }
}

impl Hash for Float {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.value.to_string().hash(state);
    }
}

// === Arithmetic Ops for Int ===
impl Add for Int {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        Self { value: self.value + rhs.value }
    }
}

impl Sub for Int {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        Self { value: self.value - rhs.value }
    }
}

impl Mul for Int {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self::Output {
        Self { value: self.value * rhs.value }
    }
}

impl Div for Int {
    type Output = Self;
    fn div(self, rhs: Self) -> Self::Output {
        Self { value: self.value / rhs.value }
    }
}

impl Rem for Int {
    type Output = Self;
    fn rem(self, rhs: Self) -> Self::Output {
        Self { value: self.value % rhs.value }
    }
}

impl Neg for Int {
    type Output = Self;
    fn neg(self) -> Self::Output {
        Self { value: -self.value }
    }
}

// === Arithmetic Ops for Float ===
impl Add for Float {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        Self { value: self.value + rhs.value }
    }
}

impl Sub for Float {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        Self { value: self.value - rhs.value }
    }
}

impl Mul for Float {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self::Output {
        Self { value: self.value * rhs.value }
    }
}

impl Div for Float {
    type Output = Self;
    fn div(self, rhs: Self) -> Self::Output {
        Self { value: self.value / rhs.value }
    }
}

impl Rem for Float {
    type Output = Self;
    fn rem(self, rhs: Self) -> Self::Output {
        Self { value: self.value % rhs.value }
    }
}

impl Neg for Float {
    type Output = Self;
    fn neg(self) -> Self::Output {
        Self { value: -self.value }
    }
}

impl Float {
    pub fn new(f: f64) -> Self {
        Float {
            value: BigFloat::from_f64(f),
        }
    }
    pub fn is_zero(&self) -> bool {
        self.value.is_zero()
    }
    pub fn to_i64(&self) -> Option<i64> {
        self.value.to_i64()
    }
    pub fn to_f64(&self) -> Option<f64> {
        Some(self.value.to_f64())
    }
    pub fn to_u32(&self) -> Option<u32> {
        self.value.to_u32()
    }
    pub fn abs(&self) -> Self {
        Float {
            value: self.value.abs(),
        }
    }
    pub fn powf(&self, exp: Float) -> Self {
        Float {
            value: self.value.pow(&exp.value),
        }
    }
    pub fn powi(&self, exp: Int) -> Self {
        Float {
            value: self.value.pow(&BigFloat::from_i64(exp.to_i64().unwrap())),
        }
    }
    pub fn from_int(i: Int) -> Self {
        Float {
            value: BigFloat::from(i.to_i64().unwrap()),
        }
    }
    pub fn to_int(&self) -> Int {
        Int {
            value: BigInt::from_f64(self.value.to_f64()).unwrap(),
        }
    }
    pub fn checked_pow(&self, exp: &Float) -> Option<Float> {
        if exp.is_zero() {
            return Some(Float::new(1.0));
        }

        let result = self.value.pow(&exp.value);
        if result.is_zero() {
            return None;
        }
        Some(Float { value: result })
    }
    pub fn checked_mul(&self, exp: u32) -> Option<Float> {
        let result = self.value.mul(BigFloat::from(exp));
        Some(Float { value: result })
    }
    pub fn checked_powf(&self, exp: &Float) -> Option<Float> {
        let result = self.value.pow(&exp.value);
        Some(Float { value: result })
    }

    pub fn checked_mulf(&self, exp: &Float) -> Option<Float> {
        let result = self.value.mul(&exp.value);
        Some(Float { value: result })
    }
    pub fn round(&self) -> Self {
        Float {
            value: self.clone().value,
        }
    }
}

impl Int {
    pub fn new(i: i64) -> Self {
        Int {
            value: BigInt::from(i),
        }
    }
    pub fn is_zero(&self) -> bool {
        self.value.is_zero()
    }
    pub fn to_i64(&self) -> Option<i64> {
        self.value.to_i64()
    }
    pub fn to_f64(&self) -> Option<f64> {
        self.value.to_f64()
    }
    pub fn to_u32(&self) -> Option<u32> {
        self.value.to_u32()
    }
    pub fn abs(&self) -> Self {
        Int {
            value: self.value.abs(),
        }
    }
    pub fn pow(&self, exp: Int) -> Self {
        Int {
            value: self.value.pow(exp.to_u32().unwrap()),
        }
    }
    pub fn checked_pow(&self, exp: u32) -> Option<Int> {
        if exp == 0 {
            return Some(Int::new(1));
        }

        let result = self.value.pow(exp);
        if result.is_zero() {
            return None;
        }
        Some(Int { value: result })
    }
    pub fn checked_mul(&self, exp: &Int) -> Option<Int> {
        let bigint_exp = &exp.value;
        self.value.checked_mul(bigint_exp).map(|v| Int { value: v })
    }
    pub fn from_float(f: Float) -> Self {
        let s = f.value.to_string();
        let parts: Vec<&str> = s.split('.').collect();
        let int_str = parts[0];

        let value = BigInt::parse_bytes(int_str.as_bytes(), 10)
            .expect("Failed to parse BigFloat string into BigInt");

        Int { value }
    }

    pub fn checked_powf(&self, exp: &Float) -> Option<Float> {
        if let Some(exp_u32) = exp.to_u32() {
            let result = self.value.pow(exp_u32);
            let result_str = result.to_str_radix(10);
            let result_bigfloat = BigFloat::from_str(&result_str).ok()?;
            return Some(Float { value: result_bigfloat });
        }

        if let Some(exp_f64) = exp.to_f64() {
            let result = self.value.to_f64().unwrap_or(0.0).powf(exp_f64);
            let result_bigfloat = BigFloat::from_f64(result);
            return Some(Float { value: result_bigfloat });
        }

        None
    }
    pub fn checked_mulf(&self, exp: &Float) -> Option<Float> {
        if let Some(f64_value) = self.value.to_f64() {
            let big_float = BigFloat::from_f64(f64_value);
            let result = big_float.mul(&exp.value);
            Some(Float { value: result })
        } else {
            None
        }
    }
    pub fn round(&self) -> Self {
        Int {
            value: self.clone().value,
        }
    }
}

impl fmt::Display for Float {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl fmt::Display for Int {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}


#[derive(Debug, Clone, PartialOrd, PartialEq)]
pub struct Error {
    pub error_type: String,
    pub msg: String,
    pub help: Option<String>,
    pub line: (usize, String),
    pub column: usize,
}

impl Error {
    pub fn new(error_type: &str, msg: &str) -> Self {
        Self {
            error_type: error_type.to_string(),
            msg: msg.to_string(),
            help: None,
            line: (0, "<unknown>".to_string()),
            column: 0,
        }
    }

    pub fn error_with_help(error_type: &'static str, msg: &'static str, help: String) -> Self {
        Self {
            error_type: to_static(error_type.to_string()).to_string(),
            msg: to_static(msg.to_string()).to_string(),
            help: Some(help),
            line: (0, "<unknown>".to_string()),
            column: 0,
        }
    }

    pub fn with_position(error_type: &str, msg: &str, line: usize, line_text: &str, column: usize) -> Self {
        Self {
            error_type: error_type.to_string(),
            msg: msg.to_string(),
            help: None,
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

    pub fn help(&self) -> Option<&str> {
        self.help.as_deref()
    }
}

impl From<&str> for Error {
    fn from(msg: &str) -> Self {
        Error::new("UnknownError", msg)
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
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
    Float(Float),
    Int(Int),
    String(String),
    Boolean(bool),
    Null,
    Map {
        keys: Vec<Value>,
        values: Vec<Value>,
    },
    List(Vec<Value>),
    ListCompletion {
        pattern: Vec<Value>,
        end: Option<Box<Value>>,
    },
    Function(Function),
    Error(&'static str, &'static str),
}

impl Eq for Value {}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Value::Float(n) => {
                0u8.hash(state);
                let canonical = n.value.to_string();
                canonical.hash(state);
            }

            Value::Int(n) => {
                1u8.hash(state);
                let canonical = n.value.to_str_radix(10);
                canonical.hash(state);
            }

            Value::String(s) => s.hash(state),

            Value::Boolean(b) => b.hash(state),

            Value::Null => 0.hash(state),

            Value::Map { keys, values, .. } => {
                keys.hash(state);
                values.hash(state);
            }

            Value::List(v) => v.hash(state),

            Value::ListCompletion { pattern, end } => {
                pattern.hash(state);
                end.hash(state);
            }
            Value::Function(func) => {
                func.get_name().hash(state);
                func.get_parameters().hash(state);
                func.get_return_type().hash(state);
            }
            Value::Error(err_type, err_msg) => {
                err_type.hash(state);
                err_msg.hash(state);
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
            (Value::Int(a), Value::Float(b)) => Value::Float((a + b.into()).into()),
            (Value::Float(a), Value::Int(b)) => Value::Float(a + b.into()),
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
            (Value::Int(a), Value::Float(b)) => Value::Float((a - b.into()).into()),
            (Value::Float(a), Value::Int(b)) => Value::Float(a - b.into()),
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
            (Value::Int(a), Value::Float(b)) => Value::Float((a * b.into()).into()),
            (Value::Float(a), Value::Int(b)) => Value::Float(a * b.into()),
            _ => Value::Null,
        }
    }
}

impl Div for Value {
    type Output = Value;

    fn div(self, other: Value) -> Value {
        match (self, other) {
            (Value::Int(_), Value::Int(b)) if b.is_zero() => Value::Null,
            (Value::Float(_), Value::Float(b)) if b == 0.0.into() => Value::Null,
            (Value::Int(a), Value::Int(b)) => Value::Int(a / b),
            (Value::Float(a), Value::Float(b)) => Value::Float(a / b),
            (Value::Int(a), Value::Float(b)) => {
                if b == 0.0.into() {
                    Value::Null
                } else {
                    Value::Float((a / b.into()).into())
                }
            }
            (Value::Float(a), Value::Int(b)) => {
                if b == 0.into() {
                    Value::Null
                } else {
                    Value::Float(a / b.into())
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
            (_, Value::Int(b)) if b == 0.into() => Value::Null,
            (_, Value::Float(b)) if b == 0.0.into() => Value::Null,

            (Value::Int(a), Value::Int(b)) => Value::Int(a % b),
            (Value::Float(a), Value::Float(b)) => Value::Float(a % b),
            (Value::Int(a), Value::Float(b)) => Value::Float((a % b.into()).into()),
            (Value::Float(a), Value::Int(b)) => Value::Float(a % b.into()),
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
                                line = val.to_u32().unwrap() as usize;
                                continue;
                            }
                        }
                        if s == "_column" {
                            if let Some(Value::Int(val)) = values.get(i) {
                                column = val.to_u32().unwrap() as usize;
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
            Value::Int(val) if val == &Int::new(0) => true,
            Value::Float(f) if *f == 0.0.into() => true,
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
            Value::Function(_) => "function",
            Value::Error(_, _) => "error",
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
            Value::Function(func) => func.get_return_type().to_string(),
            Value::Error(err_type, _) => err_type.to_string(),
        }
    }
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Boolean(b) => *b,
            Value::Int(n) => *n != 0.into(),
            Value::Float(f) => *f != 0.0.into(),
            Value::String(s) => !s.is_empty(),
            Value::List(l) => !l.is_empty(),
            Value::Map { keys, .. } => !keys.is_empty(), // If no keys, assume empty
            Value::ListCompletion { pattern, .. } => !pattern.is_empty(),
            Value::Function(_) => true,
            Value::Error(_, _) => true,
            Value::Null => false,
        }
    }
    pub fn to_string(&self) -> String {
        match self {
            Value::Float(f) => f.to_string(),
            Value::Int(i) => i.to_string(),
            Value::String(s) => s.clone(),
            Value::Boolean(b) => b.to_string(),
            Value::Null => "null".to_string(),
            Value::Map { keys, values } => {
                let pairs: Vec<String> = keys.iter().zip(values.iter())
                    .map(|(k, v)| format!("{}: {}", k.to_string(), v.to_string()))
                    .collect();
                format!("{{{}}}", pairs.join(", "))
            }
            Value::List(v) => {
                let items: Vec<String> = v.iter().map(|item| item.to_string()).collect();
                format!("[{}]", items.join(", "))
            }
            Value::ListCompletion { pattern, end } => {
                let pattern_str: Vec<String> = pattern.iter().map(|v| v.to_string()).collect();
                let end_str = match end {
                    Some(e) => e.to_string(),
                    None => "None".to_string(),
                };
                format!("ListCompletion {{ pattern: [{}], end: {} }}", pattern_str.join(", "), end_str)
            }
            Value::Function(func) => format!("<function '{}' at {:p}>", func.get_name(), func),
            Value::Error(err_type, err_msg) => format!("<{}: {}>", err_type, err_msg),
        }
    }
}


#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
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
                        new_values.push(Value::Int(Int::from(*line as i64)));
                        new_values.push(Value::Int(Int::from(*column as i64)));
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
                        Value::Int(0.into()),
                        Value::Int(0.into()),
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
        Value::Float(n) => {
            let n_str = n.to_string();
            if let Some(dot_pos) = n_str.find('.') {
                let mut trimmed = n_str.trim_start_matches('0');
                if trimmed.starts_with('.') {
                    trimmed = &trimmed[1..];
                }
                let trimmed = trimmed.trim_end_matches('0');
                if trimmed == "" {
                    return "0".to_string();
                }
                trimmed.to_string()
            } else {
                n_str.trim_start_matches('0').to_string()
            }
        }
        Value::Int(n) => {
            let trimmed = n.to_string().trim_start_matches('0').to_string();
            if trimmed == "" {
                return "0".to_string();
            }
            trimmed
        }
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
        Value::Function(func) => {
            format!("<function '{}' at {:p}>", func.get_name(), func)
        }
        Value::Error(err_type, err_msg) => {
            format!(
                "<{}: {}>",
                err_type,
                err_msg
            )
        }
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

pub const NULL: Value = Value::Null;
pub const TRUE: Value = Value::Boolean(true);
pub const FALSE: Value = Value::Boolean(false);