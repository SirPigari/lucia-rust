#![allow(unused_imports)]
#![allow(dead_code)]
#![allow(unused_variables)]

use std::collections::{HashMap, BTreeMap};
use crate::env::runtime::config::{Config, get_from_config, set_in_config};
use crate::env::runtime::utils::{
    hex_to_ansi,
    format_value,
    find_closest_match,
    TRUE, FALSE, NULL,
    debug_log,
    check_ansi,
    unescape_string,
    to_static,
    create_function,
    create_note,
    get_type_from_token_name,
    sanitize_alias,
    special_function_meta,
    deep_insert,
    deep_get,
    check_version,
    fix_path,
    fix_and_parse_json,
    json_to_value,
    char_to_digit
};
use crate::env::runtime::pattern_reg::{predict_sequence, predict_sequence_until_length};
use crate::env::runtime::types::{Int, Float, VALID_TYPES};
use crate::env::runtime::value::Value;
use crate::env::runtime::errors::Error;
use crate::env::runtime::variables::Variable;
use crate::env::runtime::statements::Statement;
use crate::env::runtime::objects::{Object, ObjectMetadata, Class};
use crate::env::runtime::native;
use crate::env::runtime::functions::{FunctionMetadata, Parameter, ParameterKind};
use crate::env::runtime::libs::STD_LIBS;
use std::sync::Arc;
use std::path::{PathBuf, Path};
use std::fs;
use regex::Regex;
use tokio::runtime::Runtime;
use reqwest;
use serde_urlencoded;

use crate::lexer::Lexer;
use crate::env::runtime::preprocessor::Preprocessor;
use crate::parser::Parser;
use crate::env::runtime::tokens::{Location};

const VERSION: &str = env!("VERSION");

pub struct Transpiler {
    ast: Vec<Statement>,
    config: Config,
    cwd: PathBuf,
    home_dir_path: PathBuf,
    config_path: PathBuf,
    errors: Vec<Error>,
}


impl Transpiler {
    pub fn new(ast: Vec<Statement>, config: Config, cwd: PathBuf, home_dir_path: PathBuf, config_path: PathBuf) -> Self {        
        Transpiler {
            ast,
            config,
            cwd,
            home_dir_path,
            config_path,
            errors: Vec::new(),
        }
    }

    pub fn transpile(&mut self) -> Result<String, Vec<Error>> {
        // TODO: Implement the transpiler
        // NOTE: Transpile to C language
        // i want to also implement transpiling to javascript just for fun because everyone hates js
        // stick to C for now
        panic!("\x1BTranspiler is not implemented yet");
    }
}