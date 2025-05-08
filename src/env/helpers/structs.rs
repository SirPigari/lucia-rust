use std::fmt;
use std::ops::{Not, BitAnd, BitOr};
use once_cell::sync::Lazy;

#[derive(Debug, Clone)]  // Derive Clone so that we can clone the Boolean
pub struct Boolean {
    pub value: String,  // For display, using String type
    pub literal: bool,  // For computation
}

impl fmt::Display for Boolean {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

// Logical NOT
impl Not for Boolean {
    type Output = Self;
    fn not(self) -> Self::Output {
        Boolean {
            value: format!("!{}", self.value),  // Format to String
            literal: !self.literal,
        }
    }
}

// Logical AND
impl BitAnd for Boolean {
    type Output = Self;
    fn bitand(self, rhs: Self) -> Self::Output {
        Boolean {
            value: format!("({} && {})", self.value, rhs.value),  // Format to String
            literal: self.literal && rhs.literal,
        }
    }
}

// Logical OR
impl BitOr for Boolean {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self::Output {
        Boolean {
            value: format!("({} || {})", self.value, rhs.value),  // Format to String
            literal: self.literal || rhs.literal,
        }
    }
}

// Equality
impl PartialEq for Boolean {
    fn eq(&self, other: &Self) -> bool {
        self.literal == other.literal
    }
}

impl Eq for Boolean {}