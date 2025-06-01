use num_bigint::BigInt;
use num_bigfloat::BigFloat;
use num_traits::{ToPrimitive, FromPrimitive, Zero, One, Signed, };
use std::fmt;
use std::hash::{Hash, Hasher};
use std::ops::{Add, Sub, Mul, Div, Rem, Neg, Not, BitAnd, BitOr};
use std::str::FromStr;
use crate::env::core::value::Value;
use once_cell::sync::Lazy;

pub const VALID_TYPES: &[&str] = &[
    "void", "any", "null", "int", "float", "bool", "str", "map", "list", "function", "error", "bytes"
];

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
    pub fn to_usize(&self) -> Option<usize> {
        self.value.to_usize()
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
            value: BigFloat::from_str(&i.value.to_string()).unwrap(),
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
    pub fn to_usize(&self) -> Option<usize> {
        self.value.to_usize()
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

    pub fn to_float(&self) -> Float {
        let int_str = self.value.to_string();
        let value = BigFloat::from_str(&int_str).unwrap_or_else(|_| BigFloat::from_u32(0));
        Float { value }
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