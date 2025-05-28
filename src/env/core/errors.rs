use crate::env::core::value::Value;
use crate::env::core::utils::{to_static, format_value, self};
use std::fmt::{self, Display};

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