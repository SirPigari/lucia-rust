use crate::env::runtime::value::Value;
use crate::env::runtime::utils::{to_static, format_value, self};
use std::fmt::{self, Display};

#[derive(Debug, Clone, PartialOrd, PartialEq)]
pub struct Error {
    pub error_type: String,
    pub msg: String,
    pub help: Option<String>,
    pub line: (usize, String),
    pub column: usize,
    pub file: String,
    pub ref_err: Option<Box<Error>>,
}

impl Error {
    pub fn new(error_type: &str, msg: &str, file_path: &str) -> Self {
        Self {
            error_type: error_type.to_string(),
            msg: msg.to_string(),
            help: None,
            line: (0, "<unknown>".to_string()),
            column: 0,
            file: file_path.to_string(),
            ref_err: None,
        }
    }

    pub fn error_with_help(error_type: &'static str, msg: &'static str, help: String, file_path: &str) -> Self {
        Self {
            error_type: to_static(error_type.to_string()).to_string(),
            msg: to_static(msg.to_string()).to_string(),
            help: Some(help),
            line: (0, "<unknown>".to_string()),
            column: 0,
            file: file_path.to_string(),
            ref_err: None,
        }
    }

    pub fn with_position(error_type: &str, msg: &str, line: usize, line_text: &str, column: usize, file_path: &str) -> Self {
        Self {
            error_type: error_type.to_string(),
            msg: msg.to_string(),
            help: None,
            line: (line, line_text.to_string()),
            column,
            file: file_path.to_string(),
            ref_err: None,
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
        Error::new("UnknownError", msg, "<unknown>")
    }
}