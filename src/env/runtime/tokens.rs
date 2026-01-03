use once_cell::sync::Lazy;
use serde::{Serialize, Deserialize};
use bincode::{Encode, Decode};
use std::borrow::Cow;
use std::panic::Location as PanicLocation;

pub static DEFAULT_TOKEN: Lazy<Token> = Lazy::new(Token::default);

// Static token type constants
pub const TK_EOF: &str = "EOF";
pub const TK_IDENTIFIER: &str = "IDENTIFIER";
pub const TK_STRING: &str = "STRING";
pub const TK_RAW_STRING: &str = "RAW_STRING";
pub const TK_NUMBER: &str = "NUMBER";
pub const TK_BOOLEAN: &str = "BOOLEAN";
pub const TK_OPERATOR: &str = "OPERATOR";
pub const TK_SEPARATOR: &str = "SEPARATOR";
pub const TK_INVALID: &str = "INVALID";

#[derive(Debug, Clone, Eq, Hash, PartialEq, PartialOrd, Ord, Serialize, Deserialize, Encode, Decode)]
pub struct Location {
    pub file: String,
    pub lucia_source_loc: String,
    pub line_string: String,
    pub line_number: usize,
    pub range: (usize, usize),
}

impl Location {
    pub fn new(file: impl Into<String>, line_string: impl Into<String>, line_number: usize, range: (usize, usize), lucia_source_loc: impl Into<String>) -> Self {
        Self {
            file: file.into(),
            lucia_source_loc: lucia_source_loc.into(),
            line_string: line_string.into(),
            line_number,
            range,
        }
    }

    #[track_caller]
    pub fn from_lasm_loc(lasm_loc: &lasm::Location, file: impl Into<String>) -> Self {
        let lucia_source_loc = format!("{}:{}:{}", PanicLocation::caller().file(), PanicLocation::caller().line(), PanicLocation::caller().column());
        Self {
            file: file.into(),
            lucia_source_loc,
            line_string: lasm_loc.line_text.clone(),
            line_number: lasm_loc.line,
            range: (lasm_loc.column, lasm_loc.column + 1),
        }
    }

    pub fn set_lucia_source_loc(mut self, lucia_source_loc: String) -> Self {
        self.lucia_source_loc = lucia_source_loc;
        self
    }
}

impl Default for Location {
    fn default() -> Self {
        Location {
            file: String::new(),
            lucia_source_loc: String::new(),
            line_string: String::new(),
            line_number: 0,
            range: (0, 0),
        }
    }
}

#[derive(Debug, Clone, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub struct Token(pub Cow<'static, str>, pub Cow<'static, str>, pub Option<Location>);

impl Token {
    // Helper to create tokens with static kinds
    #[inline]
    pub fn new_static(kind: &'static str, value: impl Into<Cow<'static, str>>, loc: Option<Location>) -> Self {
        Token(Cow::Borrowed(kind), value.into(), loc)
    }

    #[inline]
    pub fn new_owned(kind: String, value: String, loc: Option<Location>) -> Self {
        Token(Cow::Owned(kind), Cow::Owned(value), loc)
    }
}

impl Default for Token {
    fn default() -> Self {
        Token(Cow::Borrowed(""), Cow::Borrowed(""), None)
    }
}

// Custom Serialize/Deserialize for Cow
impl Serialize for Token {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::SerializeTuple;
        let mut tup = serializer.serialize_tuple(3)?;
        tup.serialize_element(self.0.as_ref())?;
        tup.serialize_element(self.1.as_ref())?;
        tup.serialize_element(&self.2)?;
        tup.end()
    }
}

impl<'de> Deserialize<'de> for Token {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::{Visitor, SeqAccess};
        
        struct TokenVisitor;
        
        impl<'de> Visitor<'de> for TokenVisitor {
            type Value = Token;
            
            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("a Token tuple")
            }
            
            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: SeqAccess<'de>,
            {
                let kind: String = seq.next_element()?.ok_or_else(|| serde::de::Error::invalid_length(0, &self))?;
                let value: String = seq.next_element()?.ok_or_else(|| serde::de::Error::invalid_length(1, &self))?;
                let loc: Option<Location> = seq.next_element()?.ok_or_else(|| serde::de::Error::invalid_length(2, &self))?;
                Ok(Token(Cow::Owned(kind), Cow::Owned(value), loc))
            }
        }
        
        deserializer.deserialize_tuple(3, TokenVisitor)
    }
}

// Custom Encode/Decode for Cow
impl Encode for Token {
    fn encode<E: bincode::enc::Encoder>(&self, encoder: &mut E) -> Result<(), bincode::error::EncodeError> {
        self.0.as_ref().encode(encoder)?;
        self.1.as_ref().encode(encoder)?;
        self.2.encode(encoder)?;
        Ok(())
    }
}

impl<Context> bincode::Decode<Context> for Token {
    fn decode<D: bincode::de::Decoder>(decoder: &mut D) -> Result<Self, bincode::error::DecodeError> {
        let kind = String::decode(decoder)?;
        let value = String::decode(decoder)?;
        let loc = Option::<Location>::decode(decoder)?;
        Ok(Token(Cow::Owned(kind), Cow::Owned(value), loc))
    }
}

impl<'de, Context> bincode::BorrowDecode<'de, Context> for Token {
    fn borrow_decode<D: bincode::de::Decoder>(decoder: &mut D) -> Result<Self, bincode::error::DecodeError> {
        Self::decode(decoder)
    }
}
