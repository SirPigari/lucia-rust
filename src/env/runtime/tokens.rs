use once_cell::sync::Lazy;

pub static DEFAULT_TOKEN: Lazy<Token> = Lazy::new(Token::default);

#[derive(Debug, Clone, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub struct Location {
    pub file: String,
    pub line_string: String,
    pub line_number: usize,
    pub range: (usize, usize),
}

impl Location {
    pub fn new(file: impl Into<String>, line_string: impl Into<String>, line_number: usize, range: (usize, usize)) -> Self {
        Self {
            file: file.into(),
            line_string: line_string.into(),
            line_number,
            range,
        }
    }
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct Token(pub String, pub String, pub Option<Location>);

impl Default for Token {
    fn default() -> Self {
        Token("".to_string(), "".to_string(), None)
    }
}
