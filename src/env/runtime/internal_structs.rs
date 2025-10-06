#![allow(dead_code)]

use crate::env::runtime::value::Value;
use std::collections::HashMap;
use serde::{Serialize, Deserialize, Serializer, Deserializer};
use core::ops::{BitOr, BitOrAssign, BitAnd, BitAndAssign, Not};
use bincode::{Encode, Decode};
use std::sync::RwLock;
use crate::env::runtime::tokens::Location;
use crate::env::runtime::statements::Statement;
use rustc_hash::FxHashMap;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Serialize, Deserialize, Encode, Decode, Hash)]
pub struct EffectFlags(u32);

impl EffectFlags {
    /// PURE — no side effects at all.
    /// Functions with this flag do not perform IO, mutate state,
    /// fail, or use unsafe features.
    pub const PURE: Self                = Self(1 << 1);

    /// IO — the function performs input/output,
    /// such as 'print', 'input', or interacting with ports.
    pub const IO: Self                  = Self(1 << 2);

    /// STATE — the function modifies or reads global state.
    /// (local variables don't count)
    pub const STATE: Self               = Self(1 << 3);

    /// UNSAFE — the function uses unsafe constructs that require
    /// '#config allow_unsafe = true', such as raw pointers or C interop.
    pub const UNSAFE: Self              = Self(1 << 4);

    /// MAY_FAIL — the function may throw an error,
    /// or is marked with '?' (which implicitly adds this flag).
    pub const MAY_FAIL: Self            = Self(1 << 5);

    /// UNKNOWN — the function's effects are not known,
    /// such as when calling an external library function.
    pub const UNKNOWN: Self             = Self(1 << 6);

    // ALL — all possible effects.
    pub const ALL: Self                 = Self(Self::PURE.0 | Self::IO.0 | Self::STATE.0 | Self::UNSAFE.0 | Self::MAY_FAIL.0 | Self::UNKNOWN.0);

    /// Returns an empty flag set (equivalent to PURE).
    #[inline]
    pub const fn empty() -> Self {
        Self(0)
    }

    #[inline]
    pub const fn is_some(self) -> bool {
        self.0 != 0
    }

    /// Combines two flags into one (bitwise OR).
    #[inline]
    pub const fn union(self, other: Self) -> Self {
        Self(self.0 | other.0)
    }

    /// Adds another flag to this set (bitwise OR assignment).
    #[inline]
    pub fn insert(&mut self, other: Self) {
        self.0 |= other.0;
    }

    /// Removes a flag from this set (bitwise AND with inverse).
    #[inline]
    pub fn remove(&mut self, other: Self) {
        self.0 &= !other.0;
    }

    /// Checks if a specific flag is set.
    #[inline]
    pub const fn contains(self, other: Self) -> bool {
        (self.0 & other.0) != 0
    }

    /// Returns true if there are no side effects (PURE).
    #[inline]
    pub const fn is_pure(self) -> bool {
        self.0 == Self::PURE.0
    }

    #[inline]
    pub const fn is_empty(self) -> bool {
        self.0 == 0
    }

    /// Returns the raw bit value (for debugging or serialization).
    #[inline]
    pub const fn bits(self) -> u32 {
        self.0
    }

    /// Creates a flag set from a raw u32 bitmask.
    #[inline]
    pub const fn from_u32(bits: u32) -> Self {
        Self(bits)
    }

    pub fn check_branches(self, a: EffectFlags, mask: EffectFlags) -> Option<(bool, EffectFlags)> {
        let mask = !mask.0;
        let self_masked = self.0 & mask;
        let a_masked = a.0 & mask;

        let missing_bits = a_masked & !self_masked;
        if missing_bits != 0 {
            return Some((false, EffectFlags(missing_bits)));
        }

        let extra_bits = self_masked & !a_masked;
        if extra_bits != 0 {
            return Some((true, EffectFlags(extra_bits)));
        }

        None
    }

    pub fn get_names(&self) -> Vec<&'static str> {
        let mut names = Vec::new();
        if self.contains(Self::PURE) {
            names.push("PURE");
        }
        if self.contains(Self::IO) {
            names.push("IO");
        }
        if self.contains(Self::STATE) {
            names.push("STATE");
        }
        if self.contains(Self::UNSAFE) {
            names.push("UNSAFE");
        }
        if self.contains(Self::MAY_FAIL) {
            names.push("MAY_FAIL");
        }
        if self.contains(Self::UNKNOWN) {
            names.push("UNKNOWN");
        }
        names
    }
}

impl BitOr for EffectFlags {
    type Output = Self;
    #[inline]
    fn bitor(self, rhs: Self) -> Self::Output {
        Self(self.0 | rhs.0)
    }
}

impl BitOrAssign for EffectFlags {
    #[inline]
    fn bitor_assign(&mut self, rhs: Self) {
        self.0 |= rhs.0;
    }
}

impl BitAnd for EffectFlags {
    type Output = Self;
    #[inline]
    fn bitand(self, rhs: Self) -> Self::Output {
        Self(self.0 & rhs.0)
    }
}

impl BitAndAssign for EffectFlags {
    #[inline]
    fn bitand_assign(&mut self, rhs: Self) {
        self.0 &= rhs.0;
    }
}

impl Not for EffectFlags {
    type Output = Self;
    #[inline]
    fn not(self) -> Self::Output {
        Self(!self.0)
    }
}


#[derive(Debug, Clone, PartialEq)]
pub enum PathElement {
    Path {
        segments: Vec<String>,       // a::b::c
        args: Vec<PathElement>,      // for variants like a::b(x, y)
    },
    Tuple(Vec<PathElement>),         // for (a, b, c)
    Literal(Value),                  // for literal values like 42, "hello"
    Union(Vec<PathElement>),         // for union types like a | b | c
}

impl PathElement {
    pub fn to_value(&self) -> Value {
        match self {
            PathElement::Path { segments, args } => {
                let seg_values = segments.iter().map(|s| Value::String(s.clone())).collect();
                let arg_values = args.iter().map(|a| a.to_value()).collect();
                Value::Map {
                    keys: vec![Value::String("segments".into()), Value::String("args".into())],
                    values: vec![Value::List(seg_values), Value::List(arg_values)],
                }
            }
            PathElement::Tuple(elems) => Value::Tuple(elems.iter().map(|e| e.to_value()).collect()),
            PathElement::Literal(value) => value.clone(),
            PathElement::Union(elems) => Value::List(elems.iter().map(|e| e.to_value()).collect()),
        }
    }
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Encode, Decode)]
pub struct Stack {
    pub frames: Vec<StackFrame>,
}

impl Stack {
    pub fn new() -> Self {
        Stack {
            frames: Vec::new(),
        }
    }

    pub fn push(&mut self, frame: (String, Option<Location>, StackType)) {
        self.frames.push(StackFrame::new(frame.0, frame.1, frame.2));
    }

    pub fn pop(&mut self) -> Option<(String, Option<Location>, StackType)> {
        while self.frames.len() > 2 {
            if let Some(frame) = self.frames.last() {
                if frame.stack_type != StackType::File {
                    self.frames.pop();
                    continue;
                }
            }
            break;
        }
        self.frames.pop().map(|frame| (frame.file_path, frame.location, frame.stack_type))
    }

    pub fn clear(&mut self) {
        self.frames.clear();
    }

    pub fn iter(&self) -> impl Iterator<Item = (String, Option<Location>, StackType)> {
        self.frames.iter().map(|frame| (frame.file_path.clone(), frame.location.clone(), frame.stack_type.clone()))
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Encode, Decode)]
pub struct StackFrame {
    pub file_path: String,
    pub location: Option<Location>,
    pub stack_type: StackType,
}

impl StackFrame {
    pub fn new(file_path: String, location: Option<Location>, stack_type: StackType) -> Self {
        StackFrame {
            file_path,
            location,
            stack_type,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Encode, Decode)]
pub enum StackType {
    FunctionCall,
    MethodCall,
    Import,
    Scope,
    File,
}

pub struct LibRegistry {
    inner: RwLock<HashMap<String, LibInfo>>,
}

impl LibRegistry {
    pub fn new() -> Self {
        LibRegistry {
            inner: RwLock::new(HashMap::new()),
        }
    }

    pub fn get(&self, name: &str) -> Option<LibInfo> {
        self.inner.read().ok()?.get(name).cloned()
    }

    pub fn set_all(&self, new_libs: HashMap<String, LibInfo>) {
        if let Ok(mut inner) = self.inner.write() {
            *inner = new_libs;
        }
    }
}

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq)]
pub struct LibInfo {
    pub description: String,
    pub version: String,
    pub expected_lucia_version: String,
}

impl LibInfo {
    pub fn new(description: &str, version: &str, expected_lucia_version: &str) -> Self {
        LibInfo {
            description: description.to_string(),
            version: version.to_string(),
            expected_lucia_version: expected_lucia_version.to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum PatternMethod {
    Arithmetic,
    Fibonacci,
    Geometric,
    Linear,
    Factorial,
    Quadratic { a: f64, b: f64, c: f64 },
    Exponential,
    Polynomial(Vec<f64>),
    Alternating,
    Trigonometric,
}

impl PatternMethod {
    pub fn display(&self) -> &'static str {
        match self {
            PatternMethod::Arithmetic => "arithmetic",
            PatternMethod::Fibonacci => "fibonacci",
            PatternMethod::Geometric => "geometric",
            PatternMethod::Linear => "linear",
            PatternMethod::Factorial => "factorial",
            PatternMethod::Quadratic { .. } => "quadratic",
            PatternMethod::Exponential => "exponential",
            PatternMethod::Polynomial (_) => "polynomial",
            PatternMethod::Alternating => "alternating",
            PatternMethod::Trigonometric => "trigonometric",
        }
    }

    pub fn full(&self) -> String {
        let math_str = match self {
            PatternMethod::Quadratic { a, b, c } => {
                let mut parts = Vec::new();
                if *a != 0.0 {
                    parts.push(format!("{}x²", a));
                }
                if *b != 0.0 {
                    parts.push(format!("{}x", if *b > 0.0 { format!("+ {}", b) } else { b.to_string() }));
                }
                if *c != 0.0 {
                    parts.push(format!("{}", if *c > 0.0 { format!("+ {}", c) } else { c.to_string() }));
                }
                if parts.is_empty() {
                    parts.push("0".to_string());
                }
                parts.join(" ")
            }
            PatternMethod::Polynomial(coeffs) => {
                let degree = coeffs.len() - 1;
                let mut parts = Vec::new();
                for (i, coeff) in coeffs.iter().enumerate() {
                    if *coeff == 0.0 {
                        continue;
                    }
                    let power = degree - i;
                    let sign = if parts.is_empty() {
                        ""
                    } else if *coeff > 0.0 {
                        "+ "
                    } else {
                        "- "
                    };
                    let abs_coeff = coeff.abs();
                    let part = match power {
                        0 => format!("{}{}", sign, abs_coeff),
                        1 => format!("{}{}x", sign, abs_coeff),
                        _ => format!("{}{}x^{}", sign, abs_coeff, power),
                    };
                    parts.push(part);
                }
                if parts.is_empty() {
                    parts.push("0".to_string());
                }
                parts.join(" ")
            }
            _ => String::new(),
        };

        if math_str.is_empty() {
            self.display().to_string()
        } else {
            format!("{} ({})", self.display(), math_str)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Encode, Decode)]
pub enum State {
    Normal,
    Exit,
    Defer,
    Break,
    Continue,
    TCO(Statement),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Encode, Decode)]
pub enum CacheFormat {
    NoCache,
    Json,
    BinLe,
    BinBe,
    ZstdLeFast,
    ZstdLeBest,
    ZstdBeFast,
    ZstdBeBest,
}

impl CacheFormat {
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "no_cache" => Some(Self::NoCache),
            "json" => Some(Self::Json),
            "bin_le" => Some(Self::BinLe),
            "bin_be" => Some(Self::BinBe),
            "zstd_le_fast" => Some(Self::ZstdLeFast),
            "zstd_le_best" => Some(Self::ZstdLeBest),
            "zstd_be_fast" => Some(Self::ZstdBeFast),
            "zstd_be_best" => Some(Self::ZstdBeBest),
            _ => None,
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            Self::NoCache => "no_cache".to_string(),
            Self::Json => "json".to_string(),
            Self::BinLe => "bin_le".to_string(),
            Self::BinBe => "bin_be".to_string(),
            Self::ZstdLeFast => "zstd_le_fast".to_string(),
            Self::ZstdLeBest => "zstd_le_best".to_string(),
            Self::ZstdBeFast => "zstd_be_fast".to_string(),
            Self::ZstdBeBest => "zstd_be_best".to_string(),
        }
    }

    pub fn compression_level(self) -> Option<i32> {
        match self {
            Self::ZstdLeFast | Self::ZstdBeFast => Some(1),
            Self::ZstdLeBest | Self::ZstdBeBest => Some(10),
            _ => None,
        }
    }

    pub fn is_enabled(self) -> bool {
        self != Self::NoCache
    }

    pub fn is_zstd(self) -> bool {
        self.compression_level().is_some()
    }
}

impl Serialize for CacheFormat {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

impl<'de> Deserialize<'de> for CacheFormat {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        CacheFormat::from_str(&s).ok_or_else(|| {
            serde::de::Error::custom(format!("invalid cache format: {}", s))
        })
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Encode, Decode)]
pub struct Cache {
    pub operations: FxHashMap<String, Value>,
    pub constants: FxHashMap<String, Value>,
    pub iterables: FxHashMap<Value, Value>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Encode, Decode)]
pub struct InternalStorage {
    pub lambda_counter: usize,
    pub use_42: (bool, bool),
    pub in_try_block: bool,
    pub in_function: bool,
}

#[derive(Serialize)]
pub struct BuildInfo {
    pub name: &'static str,
    pub version: &'static str,
    pub uuid: &'static str,
    pub rustc_version: &'static str,
    pub rustc_channel: &'static str,
    pub target: &'static str,
    pub repository: &'static str,
    pub git_hash: &'static str,
    pub file_hash: &'static str,
    pub profile: &'static str,
    pub ci: &'static str,
    pub build_date: &'static str,
    pub dependencies: &'static str,
}