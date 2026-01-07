use crate::env::runtime::tokens::Location;
use crate::env::runtime::value::Value;
use std::sync::Arc;

use serde::{Serialize, Deserialize};
use bincode::{Encode, Decode};

#[derive(Debug, Clone, PartialOrd, PartialEq, Serialize, Deserialize, Encode, Decode, Hash)]
pub struct Error {
    pub err_type: String,
    pub err_msg: String,
    pub help: Option<String>,
    pub loc: Option<Location>,
    pub ref_err: Option<Box<Error>>,
}

impl Error {
    pub fn new(err_type: &str, err_msg: &str, file_path: &str) -> Self {
        Self {
            err_type: err_type.to_owned(),
            err_msg: err_msg.to_owned(),
            help: None,
            loc: Some(Location {
                file: file_path.to_owned(),
                line_string: "".to_owned(),
                line_number: 0,
                range: (0, 0),
                lucia_source_loc: "<unknown>".to_owned(),
            }),
            ref_err: None,
        }
    }

    pub fn new_anonymous(err_type: &str, err_msg: &str) -> Self {
        Self {
            err_type: err_type.to_owned(),
            err_msg: err_msg.to_owned(),
            help: None,
            loc: None,
            ref_err: None,
        }
    }

    pub fn new_anonymous_ref(err_type: String, err_msg: String, ref_err: Option<Error>) -> Self {
        Self {
            err_type,
            err_msg,
            help: None,
            loc: None,
            ref_err: ref_err.map(|e| Box::new(e)),
        }
    }

    pub fn error_with_help(err_type: &str, err_msg: &str, help: String, loc: Location) -> Self {
        Self {
            err_type: err_type.to_owned(),
            err_msg: err_msg.to_owned(),
            help: Some(help),
            loc: Some(loc),
            ref_err: None,
        }
    }

    pub fn with_help(err_type: &str, err_msg: &str, help: &str, file_path: &str) -> Self {
        Self {
            err_type: err_type.to_owned(),
            err_msg: err_msg.to_owned(),
            help: Some(help.to_owned()),
            loc: Some(Location {
                file: file_path.to_owned(),
                line_string: "".to_owned(),
                line_number: 0,
                range: (0, 0),
                lucia_source_loc: "<unknown>".to_owned(),
            }),
            ref_err: None,
        }
    }

    pub fn with_ref(err_type: &str, err_msg: &str, ref_err: Error, file_path: &str) -> Self {
        Self {
            err_type: err_type.to_owned(),
            err_msg: err_msg.to_owned(),
            help: None,
            loc: Some(Location {
                file: file_path.to_owned(),
                line_string: "".to_owned(),
                line_number: 0,
                range: (0, 0),
                lucia_source_loc: "<unknown>".to_owned(),
            }),
            ref_err: Some(Box::new(ref_err)),
        }
    }

    pub fn error_with_ref(err_type: &str, err_msg: &str, ref_err: Error, loc: Location) -> Self {
        Self {
            err_type: err_type.to_owned(),
            err_msg: err_msg.to_owned(),
            help: None,
            loc: Some(loc),
            ref_err: Some(Box::new(ref_err)),
        }
    }

    pub fn with_location(err_type: &str, err_msg: &str, loc: Location) -> Self {
        Self {
            err_type: err_type.to_owned(),
            err_msg: err_msg.to_owned(),
            help: None,
            loc: Some(loc),
            ref_err: None,
        }
    }

    pub fn with_some_location(err_type: &str, err_msg: &str, loc: Option<Location>) -> Self {
        Self {
            err_type: err_type.to_owned(),
            err_msg: err_msg.to_owned(),
            help: None,
            loc,
            ref_err: None,
        }
    }

    pub fn err_type(&self) -> &str {
        &self.err_type
    }

    pub fn err_msg(&self) -> &str {
        &self.err_msg
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
            "<unknown location>".to_owned()
        }
    }

    pub fn to_value(&self) -> Value {
        Value::Error(Arc::new(self.clone()))
    }
}
