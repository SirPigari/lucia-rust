use crate::env::runtime::utils::{to_static};
use crate::env::runtime::tokens::Location;
use crate::env::runtime::value::Value;

use serde::{Serialize, Deserialize};
use bincode::{Encode, Decode};

#[derive(Debug, Clone, PartialOrd, PartialEq, Serialize, Deserialize, Encode, Decode)]
pub struct Error {
    pub error_type: String,
    pub msg: String,
    pub help: Option<String>,
    pub loc: Option<Location>,
    pub ref_err: Option<Box<Error>>,
}

impl Error {
    pub fn new(error_type: &str, msg: &str, file_path: &str) -> Self {
        Self {
            error_type: error_type.to_string(),
            msg: msg.to_string(),
            help: None,
            loc: Some(Location {
                file: file_path.to_string(),
                line_string: "".to_string(),
                line_number: 0,
                range: (0, 0),
                lucia_source_loc: "<unknown>".to_string(),
            }),
            ref_err: None,
        }
    }

    pub fn new_anonymous(error_type: &str, msg: &str) -> Self {
        Self {
            error_type: to_static(error_type.to_string()).to_string(),
            msg: to_static(msg.to_string()).to_string(),
            help: None,
            loc: None,
            ref_err: None,
        }
    }

    pub fn error_with_help(error_type: &'static str, msg: &'static str, help: String, loc: Location) -> Self {
        Self {
            error_type: to_static(error_type.to_string()).to_string(),
            msg: to_static(msg.to_string()).to_string(),
            help: Some(help),
            loc: Some(loc),
            ref_err: None,
        }
    }

    pub fn with_help(error_type: &str, msg: &str, help: &str, file_path: &str) -> Self {
        Self {
            error_type: error_type.to_string(),
            msg: msg.to_string(),
            help: Some(help.to_string()),
            loc: Some(Location {
                file: file_path.to_string(),
                line_string: "".to_string(),
                line_number: 0,
                range: (0, 0),
                lucia_source_loc: "<unknown>".to_string(),
            }),
            ref_err: None,
        }
    }

    pub fn with_ref(error_type: &str, msg: &str, ref_err: Error, file_path: &str) -> Self {
        Self {
            error_type: error_type.to_string(),
            msg: msg.to_string(),
            help: None,
            loc: Some(Location {
                file: file_path.to_string(),
                line_string: "".to_string(),
                line_number: 0,
                range: (0, 0),
                lucia_source_loc: "<unknown>".to_string(),
            }),
            ref_err: Some(Box::new(ref_err)),
        }
    }

    pub fn error_with_ref(error_type: &'static str, msg: &'static str, ref_err: Error, loc: Location) -> Self {
        Self {
            error_type: to_static(error_type.to_string()).to_string(),
            msg: to_static(msg.to_string()).to_string(),
            help: None,
            loc: Some(loc),
            ref_err: Some(Box::new(ref_err)),
        }
    }

    pub fn with_location(error_type: &str, msg: &str, loc: Location) -> Self {
        Self {
            error_type: error_type.to_string(),
            msg: msg.to_string(),
            help: None,
            loc: Some(loc),
            ref_err: None,
        }
    }

    pub fn with_some_location(error_type: &str, msg: &str, loc: Option<Location>) -> Self {
        Self {
            error_type: error_type.to_string(),
            msg: msg.to_string(),
            help: None,
            loc,
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

    pub fn location(&self) -> Option<&Location> {
        self.loc.as_ref()
    }

    pub fn location_string(&self) -> String {
        if let Some(loc) = &self.loc {
            format!("{}:{}:{}", loc.file, loc.line_number, loc.range.0)
        } else {
            "<unknown location>".to_string()
        }
    }

    pub fn to_value(&self) -> Value {
        Value::Error(to_static(self.error_type.clone()), to_static(self.msg.clone()), None)
    }
}
