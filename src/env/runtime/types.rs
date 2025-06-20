use std::fmt;
use std::hash::{Hash, Hasher};
use std::ops::{Add, Sub, Mul, Div, Rem, Neg, Not, BitAnd, BitOr};
use std::str::FromStr;
use crate::env::runtime::value::Value;
use crate::env::runtime::variables::Variable;
use once_cell::sync::Lazy;

pub use imagnum::{
    Int, Float
};

pub const VALID_TYPES: &[&str] = &[
    "void", "any", "int", "float", "bool", "str", "map", "list", "function", "error", "bytes", "tuple", "object"
];

#[derive(Debug, Clone)]
pub struct Boolean {
    pub value: String,
    pub literal: bool,
}

impl fmt::Display for Boolean {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Not for Boolean {
    type Output = Self;
    fn not(self) -> Self::Output {
        Boolean {
            value: format!("!{}", self.value),
            literal: !self.literal,
        }
    }
}

impl BitAnd for Boolean {
    type Output = Self;
    fn bitand(self, rhs: Self) -> Self::Output {
        Boolean {
            value: format!("({} && {})", self.value, rhs.value),
            literal: self.literal && rhs.literal,
        }
    }
}

impl BitOr for Boolean {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self::Output {
        Boolean {
            value: format!("({} || {})", self.value, rhs.value),
            literal: self.literal || rhs.literal,
        }
    }
}

impl PartialEq for Boolean {
    fn eq(&self, other: &Self) -> bool {
        self.literal == other.literal
    }
}

impl Eq for Boolean {}