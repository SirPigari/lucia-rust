use once_cell::sync::Lazy;
use serde::{Serialize, Deserialize};
use bincode::{Encode, Decode};

pub static DEFAULT_TOKEN: Lazy<Token> = Lazy::new(Token::default);

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

    pub fn set_lucia_source_loc(mut self, lucia_source_loc: String) -> Self {
        self.lucia_source_loc = lucia_source_loc;
        self
    }
}

#[derive(Debug, Clone, Eq, Hash, PartialEq, PartialOrd, Ord, Serialize, Deserialize, Encode, Decode)]
pub struct Token(pub String, pub String, pub Option<Location>);

impl Default for Token {
    fn default() -> Self {
        Token("".to_string(), "".to_string(), None)
    }
}
