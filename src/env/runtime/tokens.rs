use once_cell::sync::Lazy;

pub static DEFAULT_TOKEN: Lazy<Token> = Lazy::new(|| Token("".to_string(), "".to_string(), None));

#[derive(Debug, Clone, Eq, Hash, PartialEq, PartialOrd)]
pub struct Location {
    pub file: String,
    pub line_string: String,
    pub line_number: usize,
    pub range: (usize, usize),
}

impl Location {
    pub fn new(file: String, line_string: String, line_number: usize, range: (usize, usize)) -> Self {
        Self {
            file,
            line_string,
            line_number,
            range,
        }
    }
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct Token(pub String, pub String, pub Option<Location>);
