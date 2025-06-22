use std::collections::{HashMap, BTreeMap, HashSet};
use crate::env::runtime::config::{Config, CodeBlocks, ColorScheme};
use crate::env::runtime::utils::{
    print_colored,
    hex_to_ansi,
    format_value,
    find_closest_match,
    TRUE, FALSE, NULL,
    debug_log,
    check_ansi,
    unescape_string,
    to_static,
    make_native_method,
    make_native_function,
    get_imagnum_error_message,
    create_function,
    create_note,
    format_type,
    get_type_default,
    get_type_from_token_name,
    sanitize_alias,
    special_function_meta,
};
use crate::env::runtime::pattern_reg::{extract_seed_end, predict_sequence};
use crate::env::runtime::types::{Int, Float, VALID_TYPES};
use crate::env::runtime::value::Value;
use crate::env::runtime::errors::Error;
use crate::env::runtime::variables::Variable;
use crate::env::runtime::statements::Statement;
use crate::env::runtime::objects::{Object, ObjectMetadata, Class};
use std::ops::{Add, Sub, Mul, Div, Rem, Neg};
use crate::env::runtime::native;
use crate::env::runtime::functions::{Function, FunctionMetadata, NativeFunction, Parameter, ParameterKind, Callable, NativeCallable};
use crate::env::runtime::libs::STD_LIBS;
use std::sync::Arc;
use std::cmp::Ordering;
use std::sync::Mutex;
use std::path::{PathBuf, Path};
use once_cell::sync::Lazy;
use std::fs;
use regex::Regex;
use tokio::runtime::Runtime;
use reqwest;
use serde_urlencoded;

use crate::lexer::Lexer;
use crate::env::runtime::preprocessor::Preprocessor;
use crate::parser::{Parser, Token};

#[derive(Debug, Clone)]
pub struct Interpreter {
    config: Config,
    err: Option<Error>,
    is_returning: bool,
    is_stopped: bool,
    return_value: Value,
    source: String,
    stack: Vec<(String, HashMap<String, Value>, HashMap<String, Variable>)>,
    use_colors: bool,
    current_statement: Option<Statement>,
    variables: HashMap<String, Variable>,
    file_path: String,
    cwd: PathBuf,
    preprocessor_info: (PathBuf, PathBuf, bool),
}

impl Interpreter {
    pub fn new(config: Config, use_colors: bool, file_path: &str, cwd: &PathBuf, preprocessor_info: (PathBuf, PathBuf, bool)) -> Self {
        let mut this = Self {
            config,
            err: None,
            return_value: NULL,
            is_returning: false,
            is_stopped: false,
            source: String::new(),
            stack: vec![],
            use_colors,
            current_statement: None,
            variables: HashMap::new(),
            file_path: file_path.to_string(),
            cwd: cwd.clone(),
            preprocessor_info,
        };

        this.variables.insert(
            "print".to_string(),
            Variable::new("print".to_string(), Value::Function(native::print_fn()), "function".to_string(), false, true, true),
        );
        this.variables.insert(
            "styledprint".to_string(),
            Variable::new("styledprint".to_string(), Value::Function(native::styled_print_fn()), "function".to_string(), false, true, true),
        );
        this.variables.insert(
            "input".to_string(),
            Variable::new("input".to_string(), Value::Function(native::input_fn()), "function".to_string(), false, true, true),
        );
        this.variables.insert(
            "len".to_string(),
            Variable::new("len".to_string(), Value::Function(native::len_fn()), "function".to_string(), false, true, true),
        );
        this.variables.insert(
            "help".to_string(),
            Variable::new("help".to_string(), Value::Function(native::help_fn()), "help".to_string(), false, true, true),
        );
        this.variables.insert(
            "type".to_string(),
            Variable::new("type".to_string(), Value::Function(native::type_fn()), "function".to_string(), false, true, true),
        );
        this.variables.insert(
            "sum".to_string(),
            Variable::new("sum".to_string(), Value::Function(native::sum_fn()), "function".to_string(), false, true, true),
        );
        // 00 so user cant call it
        this.variables.insert(
            "00__placeholder__".to_string(),
            Variable::new("__placeholder__".to_string(), Value::Function(native::placeholder_fn()), "function".to_string(), false, true, true),
        );

        this
    }

    pub fn is_stopped(&self) -> bool {
        self.is_stopped
    }

    fn to_index(&mut self, val: &Value, len: usize) -> Result<usize, Value> {
        match val {
            Value::Int(i) => {
                let idx = i.to_i64().map_err(|_| { self.raise("ConversionError", "Failed to convert Int to isize") })? as isize;
                let adjusted = if idx < 0 { len as isize + idx } else { idx };
                if adjusted >= 0 && (adjusted as usize) < len {
                    Ok(adjusted as usize)
                } else {
                    Err(Value::Error("IndexError", "Index out of range"))
                }
            }
            Value::String(s) => {
                let idx: isize = s.parse()
                    .map_err(|_| self.raise("ConversionError", "Failed to parse string to int"))?;
                let adjusted = if idx < 0 { len as isize + idx } else { idx };
                if adjusted >= 0 && (adjusted as usize) < len {
                    Ok(adjusted as usize)
                } else {
                    Err(self.raise("IndexError", "Index out of range"))
                }
            }            
            Value::Float(f) => {
                if !f.is_integer_like() {
                    return Err(Value::Error("IndexError", "Float index must have zero fractional part"));
                }
                let idx = f.to_f64().map_err(|_| { self.raise("ConversionError", "Failed to convert Float to isize") })? as isize;
                let adjusted = if idx < 0 { len as isize + idx } else { idx };
                if adjusted >= 0 && (adjusted as usize) < len {
                    Ok(adjusted as usize)
                } else {
                    Err(Value::Error("IndexError", "Index out of range"))
                }
            }
            _ => Err(Value::Error("IndexError", "Index must be Int, String or Float with no fraction")),
        }
    }

    fn check_type(&mut self, value: &Value, expected: &Value, error: bool) -> bool {
        let valid_types = VALID_TYPES.to_vec();
        let mut types_mapping = HashMap::new();
        types_mapping.insert("void", "void");
        types_mapping.insert("any", "any");
        types_mapping.insert("float", "float");
        types_mapping.insert("method", "function");
        types_mapping.insert("int", "int");
        types_mapping.insert("float", "float");
        types_mapping.insert("bool", "bool");
        types_mapping.insert("str", "str");
        types_mapping.insert("list", "list");
        types_mapping.insert("map", "map");
        types_mapping.insert("function", "function");
        types_mapping.insert("bytes", "bytes");
        for obj in self.variables.values() {
            if let Value::Object(obj) = &obj.value {
                let name = obj.name();
                types_mapping.insert(&name, "object");
            }
        }

        fn is_type_equal(type_: Value, expected: Value) -> bool {
            match (type_, expected) {
                (Value::String(t), Value::String(e)) => {
                    if e == "any" || t == "any" {
                        return true;
                    }
                    if t == "null" && e == "bool" {
                        return true;
                    }
                    e == t
                },
                _ => false,
            }
        }

        if let Value::String(type_str) = expected {
            let type_type = value.type_name();
            let type_type_str = type_type.as_str();
            let type_str_str = type_str.as_str();

            let normalized_type = types_mapping.get(type_type_str).unwrap_or(&type_type_str);
            let expected_type = type_str.as_str();
        
            let normalized_type = *normalized_type;
            let expected_type = expected_type;
        
            if normalized_type == "bool" && expected_type == "null" {
                return true;
            }
        
            if expected_type == "any" || normalized_type == "any" {
                return true;
            }
        
            if !valid_types.contains(&normalized_type) || !valid_types.contains(&expected_type) {
                return self._handle_invalid_type(expected_type, valid_types);
            }
        
            if normalized_type == "int" && expected_type == "float" {
                return true;
            }
        
            if normalized_type != expected_type {
                if error {
                    self.raise(
                        "TypeError",
                        &format!("Expected type '{}', but got '{}'", expected_type, normalized_type),
                    );
                    return false;
                } else {
                    return false;
                }
            }
        
            return true;
        } else if let Value::Map { keys, values } = expected {
            let expected_hashmap: HashMap<_, _> = keys.iter().cloned().zip(values.iter().cloned()).collect();
            
            let type_kind = expected_hashmap.get(&Value::String("type_kind".to_string()));
            match type_kind {
                Some(Value::String(kind)) if kind == "function" => {
                    if let Value::Function(func) = value {
                        let metadata = func.metadata();
                        if metadata.is_native {
                            return true;
                        }
                        let elements = expected_hashmap.get(&Value::String("elements".to_string()));
                        let return_type = expected_hashmap.get(&Value::String("return_type".to_string()));
                        let variadic = expected_hashmap.get(&Value::String("variadic".to_string()));
                        let variadic_type = expected_hashmap.get(&Value::String("variadic_type".to_string()));

                        let mut status = true;

                        let elements_vec = match elements {
                            Some(Value::List(e)) => e,
                            _ => {self.raise("TypeError", "Expected a list for 'elements' in function type"); return false; },
                        };

                        if elements_vec.len() == 1 {
                            let element_type = elements_vec.get(0).unwrap();
                            for param in metadata.parameters.iter() {
                                if !is_type_equal(param.ty.clone(), element_type.clone()) {
                                    if error {
                                        self.raise("TypeError", &format!("Parameter '{}' expected type '{}', got '{}'", param.name, element_type.to_string(), param.ty.to_string()));
                                    }
                                    status = false;
                                }
                            }
                            status = true;
                        }

                        if let Some(expected_ret_type) = return_type {
                            let matches = match metadata.return_type.clone() {
                                Value::String(s) => {
                                    let expected = get_type_default(&s);
                                    self.check_type(&expected, &Value::String(s.clone()), false)
                                }
                                _ => self.check_type(expected_ret_type, &metadata.return_type, false),
                            };
                        
                            if !matches {
                                if error {
                                    self.raise(
                                        "TypeError",
                                        &format!(
                                            "Function '{}' expected return type '{}', got '{}'",
                                            metadata.name,
                                            expected_ret_type.to_string(),
                                            metadata.return_type.to_string()
                                        ),
                                    );
                                }
                                status = false;
                            }
                        } else {
                            if error {
                                self.raise("TypeError", "Missing 'return_type' in expected function type");
                            }
                            status = false;
                        }                     
                        return status;
                    } else {
                        if error {
                            self.raise("TypeError", &format!("Expected type 'function', got '{}'", value.to_string()));
                        }
                        return false;
                    }
                }
                Some(Value::String(kind)) if kind == "indexed" => {
                    if let Value::List(_) | Value::String(_) | Value::Bytes(_) = value {
                        return true;
                    }
                }
                _ => {}
            }
        }
        if error {
            self.raise("TypeError", &format!("Expected type '{}', got '{}'", expected.to_string(), value.to_string()));
        }
        false
    }

    pub fn exit_with_code(&mut self, code: Value) -> Value {
        self.is_returning = true;
        self.return_value = match code {
            Value::Int(i) => Value::Int(i),
            Value::Float(f) => Value::Float(f),
            Value::String(s) => Value::String(s),
            _ => {
                self.raise("TypeError", "Exit code must be an int, float or string");
                NULL
            }
        };
        return self.return_value.clone();
    }

    pub async fn fetch(
        &self,
        url: &str,
        method: Option<&str>,
        headers: Option<HashMap<String, String>>,
        params: Option<HashMap<String, String>>,
        data: Option<HashMap<String, String>>,
        json_: Option<Value>,
    ) -> Result<Value, Box<dyn std::error::Error>> {
        if !self.config.allow_fetch {
            return Err(Box::new(std::io::Error::new(
                std::io::ErrorKind::PermissionDenied,
                "Fetching is not allowed in this context.",
            )));
        }
    
        let mut parsed_url = reqwest::Url::parse(url).map_err(|_| {
            Box::new(std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                "Invalid URL",
            )) as Box<dyn std::error::Error>
        })?;
    
        if let Some(params) = &params {
            if !params.is_empty() {
                let mut pairs = parsed_url.query_pairs_mut();
                for (k, v) in params {
                    pairs.append_pair(k, v);
                }
                drop(pairs);
            }
        }
    
        let method = method.unwrap_or("GET");
        let method = reqwest::Method::from_bytes(method.as_bytes())?;
    
        let client = reqwest::Client::new();
    
        let mut req_headers = reqwest::header::HeaderMap::new();
        if let Some(hdrs) = headers {
            for (k, v) in hdrs.into_iter() {
                req_headers.insert(
                    reqwest::header::HeaderName::from_bytes(k.as_bytes())?,
                    reqwest::header::HeaderValue::from_str(&v)?,
                );
            }
        }
    
        let (body, content_type) = if let Some(json) = json_ {
            (Some(json.to_string()), Some("application/json"))
        } else if let Some(data) = data {
            let encoded = serde_urlencoded::to_string(data)?;
            (Some(encoded), Some("application/x-www-form-urlencoded"))
        } else {
            (None, None)
        };
    
        if let Some(ct) = content_type {
            req_headers.insert(
                reqwest::header::CONTENT_TYPE,
                reqwest::header::HeaderValue::from_static(ct),
            );
        }
    
        debug_log(&format!(
            "<Fetching {} {} with data: {}>",
            method,
            parsed_url.as_str(),
            body.as_deref().unwrap_or("null")
        ), &self.config.clone(), Some(self.use_colors));
    
        let req = client
            .request(method, parsed_url)
            .headers(req_headers);
    
        let req = if let Some(body_str) = body {
            req.body(body_str)
        } else {
            req
        };
    
        let resp = req.send().await?;
    
        debug_log(&format!("<Response status: {}>", resp.status()), &self.config.clone(), Some(self.use_colors));
    
        let status = resp.status().as_u16();
        let headers = resp.headers().clone();
        let body = resp.text().await?;
    
        let headers_map = headers.iter()
            .map(|(k, v)| (k.to_string(), v.to_str().unwrap_or("").to_string()))
            .collect::<HashMap<_, _>>();
    
        Ok(Value::Map {
            keys: vec![
                Value::String("status".to_string()),
                Value::String("headers".to_string()),
                Value::String("body".to_string()),
            ],
            values: vec![
                Value::Int(Int::from_i64(status as i64)),
                Value::Map {
                    keys: headers_map.keys().cloned().map(Value::String).collect(),
                    values: headers_map.values().cloned().map(Value::String).collect(),
                },
                Value::String(body),
            ],
        })
    }
    
    fn fetch_fn(
        &mut self,
        args: &HashMap<String, Value>,
    ) -> Value {
        if !self.config.allow_fetch {
            return self.raise("PermissionError", "Fetching is not allowed in this context");
        }

        let url = match args.get("url") {
            Some(Value::String(s)) => s,
            _ => return self.raise("TypeError", "Expected 'url' to be a string"),
        };

        let method = match args.get("method") {
            Some(Value::String(s)) => Some(s.as_str()),
            _ => None,
        };

        let headers = match args.get("headers") {
            Some(Value::Map { keys, values }) => {
                let mut map = HashMap::new();
                for (k, v) in keys.iter().zip(values.iter()) {
                    if let Value::String(key) = k {
                        if let Value::String(value) = v {
                            map.insert(key.clone(), value.clone());
                        }
                    }
                }
                Some(map)
            }
            _ => None,
        };

        let params = match args.get("params") {
            Some(Value::Map { keys, values }) => {
                let mut map = HashMap::new();
                for (k, v) in keys.iter().zip(values.iter()) {
                    if let Value::String(key) = k {
                        if let Value::String(value) = v {
                            map.insert(key.clone(), value.clone());
                        }
                    }
                }
                Some(map)
            }
            _ => None,
        };

        let data = match args.get("data") {
            Some(Value::Map { keys, values }) => {
                let mut map = HashMap::new();
                for (k, v) in keys.iter().zip(values.iter()) {
                    if let Value::String(key) = k {
                        if let Value::String(value) = v {
                            map.insert(key.clone(), value.clone());
                        }
                    }
                }
                Some(map)
            }
            _ => None,
        };

        let json_ = match args.get("json") {
            Some(v) => Some(v.clone()),
            _ => None,
        };

        let rt = Runtime::new().unwrap();
        let result = rt.block_on(self.fetch(url, method, headers, params, data, json_));
    
        match result {
            Ok(response) => response,
            Err(e) => self.raise("FetchError", &format!("Failed to fetch: {}", e)),
        }
    }

    fn _handle_invalid_type(&mut self, type_: &str, valid_types: Vec<&str>) -> bool {
        let valid_types_owned: Vec<String> = valid_types.into_iter().map(|s| s.to_string()).collect();
        let valid_types_slice: &[String] = &valid_types_owned;
    
        if let Some(closest_match) = find_closest_match(type_, valid_types_slice) {
            self.raise(
                "TypeError",
                &format!("Type '{}' is not supported. Did you mean: '{}'?", type_, closest_match),
            );
        } else {
            self.raise("TypeError", &format!("Type '{}' is not supported.", type_));
        }
    
        false
    }

    fn get_properties_from_file(&mut self, path: &PathBuf) -> HashMap<String, Variable> {
        if !path.exists() || !path.is_file() {
            self.raise("ImportError", to_static(format!("File '{}' does not exist or is not a file", path.display())));
            return HashMap::new();
        }
        let content = match fs::read_to_string(path) {
            Ok(c) => c,
            Err(e) => {
                self.raise("ImportError", &format!("Failed to read file '{}': {}", path.display(), e));
                return HashMap::new();
            }
        };
        
        let lexer = Lexer::new(&content);
        let raw_tokens = lexer.tokenize(false);

        let (pr1, pr2, ep) = self.preprocessor_info.clone();
        let processed_tokens = if ep {
            let mut preprocessor = Preprocessor::new(
                pr1,
                pr2,
                path.clone().display().to_string().as_str(),
            );
            match preprocessor.process(raw_tokens, path.parent().unwrap_or(Path::new(""))) {
                Ok(tokens) => tokens,
                Err(e) => {
                    self.raise_with_ref(
                        "ImportError",
                        &format!("Error while preprocessing '{}'", path.display()),
                        e,
                    );
                    return HashMap::new();
                }
            }
        } else {
            raw_tokens
        };
        let tokens: Vec<Token> = processed_tokens
            .into_iter()
            .map(|(t, v)| Token(t, v))
            .collect();
        let mut parser = Parser::new(tokens, self.config.clone(), content.clone(), self.use_colors, path.display().to_string().as_str());
        let statements = match parser.parse_safe() {
            Ok(stmts) => stmts,
            Err(error) => {
                self.raise_with_ref(
                    "ImportError",
                    &format!("Error while parsing '{}'", path.display()),
                    error,
                );
                return HashMap::new();
            }
        };
    
        let parent_dir = path
            .parent()
            .unwrap_or(Path::new("."))
            .canonicalize()
            .unwrap_or_else(|_| PathBuf::from("."));
        let mut interpreter = Interpreter::new(self.config.clone(), self.use_colors, path.display().to_string().as_str(), &parent_dir, self.preprocessor_info.clone());
        interpreter.stack = self.stack.clone();
        let result = interpreter.interpret(statements, content);
        self.stack = interpreter.stack.clone();
    
        let properties = interpreter.variables.clone();

        if let Some(err) = interpreter.err {
            self.raise_with_ref(
                "ImportError",
                &format!("Error while importing '{}'", path.display()),
                err.clone(),
            );
            return HashMap::new();
        }

        let mut final_properties = HashMap::new();
        for (var_name, var) in properties {
            if var.is_public() && !var.is_native() {
                final_properties.insert(var_name, var);
            }
        }
    
        match result {
            Ok(_) => final_properties,
            Err(e) => {
                let mut pos = String::new();
                let line = e.line.0;
                if line != 0 {
                    let column = e.column;
                    if column > 0 {
                        pos = format!(":{}:{}", line, column);
                    } else {
                        pos = line.to_string();
                    }
                }
                self.raise_with_ref(
                    "ImportError",
                    &format!("Error while importing '{}'", path.display()),
                    e.clone(),
                );
                return HashMap::new();
            },
        }
    }

    pub fn interpret(&mut self, statements: Vec<Statement>, source: String) -> Result<Value, Error> {
        self.source = String::new();
        self.is_returning = false;
        self.return_value = NULL;
        self.err = None;
        self.current_statement = None;
        for statement in statements {
            if let Some(err) = &self.err {
                println!("Error: {}", err.msg);
                return Err(err.clone());
            }

            if let Statement::Statement { keys, values, line, column } = statement.clone() {
                self.current_statement = Some(statement.clone());

                let value = self.evaluate(statement.clone());
                if let Value::Error(err_type, err_msg) = value {
                    self.raise(err_type, &err_msg);
                }
                if self.is_returning {
                    return Ok(value.clone());
                }

                if let Some(err) = &self.err {
                    return Err(err.clone());
                }

                self.return_value = value.clone();
            } else {
                return Err(Error::new(
                    "InvalidStatement",
                    "Expected a map. This is probably an issue with your installation. Try installing the latest stable version.",
                    &self.file_path.to_string()
                ));
            }
        }

        Ok(self.return_value.clone())
    }

    pub fn raise(&mut self, error_type: &str, msg: &str) -> Value {
        if let Some(current_statement) = &self.current_statement {
            if let Statement::Statement { line, column, .. } = current_statement {
                self.err = Some(Error {
                    error_type: error_type.to_string(),
                    msg: msg.to_string(),
                    help: None,
                    line: (*line, "".to_string()),
                    column: *column,
                    file: self.file_path.clone(),
                    ref_err: None,
                });
            } else {
                self.err = Some(Error {
                    error_type: error_type.to_string(),
                    msg: msg.to_string(),
                    help: None,
                    line: (0, "".to_string()),
                    column: 0,
                    file: self.file_path.clone(),
                    ref_err: None,
                });
            }
        } else {
            self.err = Some(Error {
                error_type: error_type.to_string(),
                msg: msg.to_string(),
                help: None,
                line: (0, "".to_string()),
                column: 0,
                file: self.file_path.clone(),
                ref_err: None,
            });
        }
    
        NULL
    }

    pub fn raise_with_help(&mut self, error_type: &str, msg: &str, help: &str) -> Value {
        if let Some(current_statement) = &self.current_statement {
            if let Statement::Statement { line, column, .. } = current_statement {
                self.err = Some(Error {
                    error_type: error_type.to_string(),
                    msg: msg.to_string(),
                    help: Some(help.to_string()),
                    line: (*line, "".to_string()),
                    column: *column,
                    file: self.file_path.clone(),
                    ref_err: None,
                });
            } else {
                self.err = Some(Error {
                    error_type: error_type.to_string(),
                    msg: msg.to_string(),
                    help: Some(help.to_string()),
                    line: (0, "".to_string()),
                    column: 0,
                    file: self.file_path.clone(),
                    ref_err: None,
                });
            }
        } else {
            self.err = Some(Error {
                error_type: error_type.to_string(),
                msg: msg.to_string(),
                help: Some(help.to_string()),
                line: (0, "".to_string()),
                column: 0,
                file: self.file_path.clone(),
                ref_err: None,
            });
        }
        NULL
    }

    pub fn raise_with_ref(&mut self, error_type: &str, msg: &str, ref_err: Error) -> Value {
        if let Some(current_statement) = &self.current_statement {
            if let Statement::Statement { line, column, .. } = current_statement {
                self.err = Some(Error {
                    error_type: error_type.to_string(),
                    msg: msg.to_string(),
                    help: None,
                    line: (*line, "".to_string()),
                    column: *column,
                    file: self.file_path.clone(),
                    ref_err: Some(Box::new(ref_err)),
                });
            } else {
                self.err = Some(Error {
                    error_type: error_type.to_string(),
                    msg: msg.to_string(),
                    help: None,
                    line: (0, "".to_string()),
                    column: 0,
                    file: self.file_path.clone(),
                    ref_err: Some(Box::new(ref_err)),
                });
            }
        } else {
            self.err = Some(Error {
                error_type: error_type.to_string(),
                msg: msg.to_string(),
                help: None,
                line: (0, "".to_string()),
                column: 0,
                file: self.file_path.clone(),
                ref_err: Some(Box::new(ref_err)),
            });
        }
        NULL
    }
    
    pub fn evaluate(&mut self, statement: Statement) -> Value {
        self.current_statement = Some(statement.clone());

        if self.stack.len() > self.config.recursion_limit {
            return self.raise("RecursionError", "Maximum recursion depth exceeded");
        }
    
        if !self.variables.contains_key("_") {
            self.variables.insert(
                "_".to_string(),
                Variable::new("_".to_string(), NULL.clone(), "any".to_string(), false, true, true),
            );
        }
        
        if !self.variables.contains_key("_err") {
            self.variables.insert(
                "_err".to_string(),
                Variable::new(
                    "_err".to_string(),
                    Value::Tuple(vec![]),
                    "tuple".to_string(),
                    false,
                    true,
                    true,
                ),
            );
        }
        
        if self.err.is_some() {
            return NULL;
        }
    
        let Statement::Statement { keys, values, .. } = statement else {
            return self.raise("SyntaxError", to_static(format!("Expected a statement map, got {:?}", statement)));
        };
    
        let statement: HashMap<_, _> = keys.iter().cloned().zip(values.iter().cloned()).collect();
    
        let result = match statement.get(&Value::String("type".to_string())) {
            Some(Value::String(t)) => match t.as_str() {
                // Function declaration & return
                "FUNCTION_DECLARATION" => self.handle_function_declaration(statement.clone()),
                "RETURN" => self.handle_return(statement.clone()),

                // Imports
                "IMPORT" => self.handle_import(statement.clone()),

                // Primitive types
                "NUMBER" => self.handle_number(statement.clone()),
                "STRING" => self.handle_string(statement.clone()),
                "BOOLEAN" => self.handle_boolean(statement.clone()),
        
                // Collections and data structures
                "ITERABLE" => self.handle_iterable(statement.clone()),
                "TUPLE" => self.handle_tuple(statement.clone()),
                "MAP" => self.handle_map(statement.clone()),
        
                // Operations
                "OPERATION" => self.handle_operation(statement.clone()),
                "UNARY_OPERATION" => self.handle_unary_op(statement.clone()),
        
                // Calls and access
                "CALL" => self.handle_call(statement.clone()),
                "METHOD_CALL" => self.handle_method_call(statement.clone()),
                "PROPERTY_ACCESS" => self.handle_property_access(statement.clone()),
                "INDEX_ACCESS" => self.handle_index_access(statement.clone()),
        
                // Control flow
                "FOR" => self.handle_for_loop(statement.clone()),
                "IF" => self.handle_if(statement.clone()),
                "TRY_CATCH" | "TRY" => self.handle_try(statement.clone()),
                "THROW" => self.handle_throw(statement.clone()),

                // Variables and assignment
                "VARIABLE" => self.handle_variable(statement.clone()),
                "VARIABLE_DECLARATION" => self.handle_variable_declaration(statement.clone()),
                "ASSIGNMENT" => self.handle_assignment(statement.clone()),
                "FORGET" => self.handle_forget(statement.clone()),
        
                // Types
                "TYPE" => self.handle_type(statement.clone()),
                "TYPE_CONVERT" => self.handle_type_conversion(statement.clone()),
        
                _ => self.raise("NotImplemented", &format!("Unsupported statement type: {}", t)),
            },
            _ => self.raise("SyntaxError", "Missing or invalid 'type' in statement map"),
        };
        
        if let Some(err) = self.err.clone() {
            let tuple = Value::Tuple(vec![
                Value::String(err.error_type),
                Value::String(err.msg),
                Value::String(err.help.unwrap_or_default()),
            ]);
    
            if let Some(var) = self.variables.get_mut("_") {
                var.set_value(tuple.clone());
            }
    
            if let Some(var) = self.variables.get_mut("_err") {
                var.set_value(tuple);
            }
    
            return NULL;
        }
        
        if result != NULL {
            if let Some(var) = self.variables.get_mut("_") {
                var.set_value(result.clone());
            }
        }
        
        result
    }

    fn handle_map(&mut self, statement: HashMap<Value, Value>) -> Value {
        let keys_opt = statement.get(&Value::String("keys".to_string())).unwrap_or_else(|| {
            self.raise("RuntimeError", "Missing 'keys' in map statement");
            &Value::Null
        });
        let values_opt = statement.get(&Value::String("values".to_string())).unwrap_or_else(|| {
            self.raise("RuntimeError", "Missing 'values' in map statement");
            &Value::Null
        });
        
        let raw_keys = match keys_opt {
            Value::List(v) => v,
            _ => {
                self.raise("TypeError", "Expected list for map keys");
                return Value::Null;
            }
        };
        
        let raw_values = match values_opt {
            Value::List(v) => v,
            _ => {
                self.raise("TypeError", "Expected list for map values");
                return Value::Null;
            }
        };
    
        if raw_keys.len() != raw_values.len() {
            self.raise("RuntimeError", "Keys and values lists must have the same length");
            return Value::Null;
        }
    
        let mut keys = Vec::with_capacity(raw_keys.len());
        let mut values = Vec::with_capacity(raw_values.len());
    
        for key in raw_keys {
            let evaluated_key = self.evaluate(key.convert_to_statement());
            keys.push(evaluated_key);
        }
        for value in raw_values {
            let evaluated_value = self.evaluate(value.convert_to_statement());
            values.push(evaluated_value);
        }
    
        let mut seen = std::collections::HashSet::new();
        for key in &keys {
            if !seen.insert(key) {
                self.raise("SyntaxError", &format!("Duplicate key in map: {}", key));
                return Value::Null;
            }
        }
    
        Value::Map { keys, values }
    }

    fn handle_type_conversion(&mut self, statement: HashMap<Value, Value>) -> Value {
        let value_opt = match statement.get(&Value::String("value".to_string())) {
            Some(v) => v,
            None => return self.raise("RuntimeError", "Missing 'value' in type conversion statement"),
        };

        let value = self.evaluate(value_opt.convert_to_statement());

        let target_type_opt = match statement.get(&Value::String("to".to_string())) {
            Some(Value::Map { keys, values } ) => {
                Value::Map {
                    keys: keys.clone(),
                    values: values.clone(),
                }
            },
            _ => return self.raise("RuntimeError", "Missing or invalid 'to' in type conversion statement"),
        };

        let target_type = match self.evaluate(target_type_opt.convert_to_statement()) {
            Value::String(s) => s,
            _ => return self.raise("RuntimeError", "Invalid 'to' in type conversion statement"),
        };

        if !VALID_TYPES.contains(&target_type.as_str()) {
            return self.raise("TypeError", &format!("Invalid target type '{}'", target_type));
        }

        match target_type.as_str() {
            "str" => {
                if let Value::String(s) = value {
                    Value::String(s.clone())
                } else {
                    Value::String(value.to_string())
                }
            },
            "int" => {
                if let Value::Int(i) = value {
                    Value::Int(i.clone())
                } else if let Value::Float(f) = value {
                    if f.is_integer_like() {
                        f.to_int().map_or_else(
                            |_| self.raise("ConversionError", "Failed to convert float to int"),
                            |i| Value::Int(i),
                        )
                    } else {
                        self.raise("ConversionError", "Float value has non-zero fractional part, cannot convert to int");
                        NULL
                    }
                } else if let Value::String(s) = value {
                    Value::Int(Int::from_str(&s).unwrap_or_else(|_| {
                        self.raise("ConversionError", &format!("Failed to convert string '{}' to int", s));
                        Int::from_i64(0)
                    }))
                } else {
                    self.raise("TypeError", &format!("Cannot convert '{}' to int", value.type_name()))
                }
            },
            "float" => {
                if let Value::Float(f) = value {
                    Value::Float(f.clone())
                } else if let Value::Int(i) = value {
                    i.to_float().map_or_else(
                        |_| self.raise("ConversionError", "Failed to convert int to float"),
                        |f| Value::Float(f),
                    )
                } else if let Value::String(s) = value {
                    Float::from_str(&s).map_or_else(
                        |_| self.raise("ConversionError", &format!("Failed to convert string '{}' to float", s)),
                        |f| Value::Float(f),
                    )
                } else {
                    self.raise("TypeError", &format!("Cannot convert '{}' to float", value.type_name()))
                }
            },
            "bool" => {
                if value.is_truthy() {
                    Value::Boolean(true)
                } else {
                    Value::Boolean(false)
                }
            },
            "void" => {
                if value.is_null() {
                    NULL
                } else {
                    self.raise("TypeError", "Cannot convert non-null value to 'void'");
                    NULL
                }
            },
            "any" => value.clone(),
            "list" => {
                if let Value::List(l) = value {
                    Value::List(l.clone())
                } else if let Value::Tuple(t) = value {
                    Value::List(t.into_iter().map(|v| v.clone()).collect())
                } else if let Value::String(s) = value {
                    Value::List(s.chars().map(|c| Value::String(c.to_string())).collect())
                } else {
                    self.raise("TypeError", &format!("Cannot convert '{}' to list", value.type_name()))
                }
            },
            "map" => {
                if let Value::Map { keys, values } = value {
                    Value::Map { keys: keys.clone(), values: values.clone() }
                } else if let Value::Object(obj) = value {
                    let mut keys = Vec::new();
                    let mut values = Vec::new();
                    for (name, var) in obj.get_properties().iter().flat_map(|map| map.iter()) {
                        keys.push(Value::String(name.clone()));
                        values.push(var.value.clone());
                    }                    
                    Value::Map { keys, values }                
                } else {
                    self.raise("TypeError", &format!("Cannot convert '{}' to map", value.type_name()))
                }
            },
            "bytes" => {
                if let Value::Bytes(b) = value {
                    Value::Bytes(b.clone())
                } else if let Value::String(s) = value {
                    Value::Bytes(s.into_bytes())
                } else {
                    self.raise("TypeError", &format!("Cannot convert '{}' to bytes", value.type_name()))
                }
            },
            "object" => {
                if let Value::Object(obj) = value {
                    Value::Object(obj.clone())
                } else {
                    self.raise("TypeError", &format!("Cannot convert '{}' to object", value.type_name()))
                }
            },
            "function" => {
                if let Value::Function(func) = value {
                    Value::Function(func.clone())
                } else {
                    self.raise("TypeError", &format!("Cannot convert '{}' to function", value.type_name()))
                }
            },
            "tuple" => {
                if let Value::Tuple(t) = value {
                    Value::Tuple(t.clone())
                } else if let Value::List(l) = value {
                    Value::Tuple(l.into_iter().map(|v| v.clone()).collect())
                } else if let Value::String(s) = value {
                    Value::Tuple(s.chars().map(|c| Value::String(c.to_string())).collect())
                } else {
                    self.raise("TypeError", &format!("Cannot convert '{}' to tuple", value.type_name()))
                }
            },
            _ => {
                self.raise("NotImplemented", &format!("Type conversion to '{}' is not implemented", target_type));
                NULL
            }
        }
    }

    fn handle_import(&mut self, statement: HashMap<Value, Value>) -> Value {
        let module_name = match statement.get(&Value::String("module_name".to_string())) {
            Some(Value::String(name)) => {
                let parts: Vec<&str> = name.split('.').collect();
                if parts.len() == 1 {
                    name.clone()
                } else {
                    let mut new_name = parts[0].to_string();
                    for part in &parts[1..] {
                        if *part == "lc" || *part == "lucia" {
                            new_name.push('.');
                        } else {
                            new_name.push('/');
                        }
                        new_name.push_str(part);
                    }
                    new_name
                }
            }
            _ => return self.raise("RuntimeError", "Missing or invalid 'module_name' in import statement"),
        };

        let alias = match statement.get(&Value::String("alias".to_string())) {
            Some(Value::String(a)) => a,
            Some(Value::Null) => &module_name,
            _ => {
                self.raise("RuntimeError", "Missing or invalid 'alias' in import statement");
                return NULL;
            },
        };
    
        let valid_alias_re = Regex::new(r"^[a-zA-Z_]\w*$").unwrap();
        if !valid_alias_re.is_match(alias) {
            return self.raise_with_help(
                "ImportError",
                &format!("'{}' is an invalid name. Please use a different alias.", alias),
                &format!("Use 'import ... as <valid_alias>'. Suggested alias: '{}'", sanitize_alias(alias)),
            );            
        }
    
        if self.stack.iter().any(|(name, _, _)| name == &module_name) {
            return self.raise(
                "RecursionError",
                &format!("Recursive import detected for module '{}'", module_name),
            );
        }
    
        self.stack.push((
            module_name.clone(),
            statement.iter().filter_map(|(k, v)| {
                if let Value::String(s) = k {
                    Some((s.clone(), v.clone()))
                } else {
                    None
                }
            }).collect::<HashMap<String, Value>>(),
            self.variables.clone(),
        ));
    
        if self.variables.contains_key(alias) {
            self.stack.pop();
            if module_name == *alias {
                return self.raise("ImportError", &format!("Module '{}' already imported", module_name));
            }
            return self.raise("ImportError", &format!("Module '{}' already imported as '{}'", module_name, alias));
        }
    
        let mut properties = HashMap::new();
    
        if let Some(lib_info) = STD_LIBS.get(module_name.as_str()) {
            debug_log(&format!("<Loading standard library module '{}', version {}, description: {}>", module_name, lib_info.version, lib_info.description), &self.config, Some(self.use_colors));
        
            match module_name.as_str() {
                "math" => {
                    use crate::env::libs::math::__init__ as math;
                    let math_module_props = math::register();
                    let result = math::init();
                    for (name, var) in math_module_props {
                        properties.insert(name, var);
                    }
                }
                "os" => {
                    use crate::env::libs::os::__init__ as os;
                    let os_module_props = os::register();
                    let result = os::init();
                    for (name, var) in os_module_props {
                        properties.insert(name, var);
                    }
                }
                "time" => {
                    use crate::env::libs::time::__init__ as time;
                    let time_module_props = time::register();
                    let result = time::init();
                    for (name, var) in time_module_props {
                        properties.insert(name, var);
                    }
                }
                "json" => {
                    use crate::env::libs::json::__init__ as json;
                    let json_module_props = json::register();
                    let result = json::init();
                    for (name, var) in json_module_props {
                        properties.insert(name, var);
                    }
                }
                "config" => {
                    use crate::env::libs::config::__init__ as config;
                    let arc_config = Arc::new(self.config.clone());
                    let config_module_props = config::register(arc_config);
                    let result = config::init();
                    for (name, var) in config_module_props {
                        properties.insert(name, var);
                    }
                }
                "clib" => {
                    use crate::env::libs::clib::__init__ as clib;
                    let arc_config = Arc::new(self.config.clone());
                    let module_path = PathBuf::from(self.config.home_dir.clone()).join("libs").join("clib").join("__init__.rs").display().to_string();
                    let result = clib::init_clib(arc_config, module_path);
                    if let Err(e) = result {
                        self.stack.pop();
                        return self.raise_with_ref(
                            "ImportError",
                            "Failed to initialize clib module",
                            e,
                        );
                    }
                    let clib_module_props = clib::register();
                    clib::init();
                    for (name, var) in clib_module_props {
                        properties.insert(name, var);
                    }
                }
                _ => {
                    self.stack.pop();
                    return self.raise(
                        "ImportError",
                        &format!("Standard library module '{}' is not currently supported", module_name),
                    );
                }
            }
        } else {
            let module_path_opt = statement.get(&Value::String("path".to_string()));
    
            let libs_dir = PathBuf::from(self.config.home_dir.clone()).join("libs");
    
            let base_module_path = match module_path_opt {
                Some(Value::Map { keys, values }) => {
                    let map_statement = Value::Map {
                        keys: keys.clone(),
                        values: values.clone(),
                    }.convert_to_statement();
            
                    let path_eval = self.evaluate(map_statement);
                    let path_str = path_eval.to_string();
            
                    if path_str.is_empty() {
                        self.stack.pop();
                        self.raise("RuntimeError", "Empty 'path' in import statement");
                        return NULL;
                    }
            
                    let mut path = PathBuf::from(path_str);
                    if path.is_relative() {
                        path = self.cwd.join(path);
                    }
                    
                    let path = match path.canonicalize() {
                        Ok(p) => p,
                        Err(_) => path.clone(),
                    };
                    path
                }
                Some(Value::Null) | None => {
                    match libs_dir.canonicalize() {
                        Ok(p) => p,
                        Err(_) => libs_dir.clone(),
                    }
                }
                _ => {
                    self.stack.pop();
                    return self.raise("RuntimeError", "Invalid 'path' in import statement");
                },
            };
    
            let candidate_dir = base_module_path.join(&module_name);
            let mut resolved_module_path: Option<PathBuf> = None;
    
            if candidate_dir.exists() && candidate_dir.is_dir() {
                resolved_module_path = Some(candidate_dir);
            } else {
                let extensions = [".lc", ".lucia", ".rs", ""];
                for ext in extensions.iter() {
                    let candidate_file = base_module_path.join(format!("{}{}", module_name, ext));
                    if candidate_file.exists() && candidate_file.is_file() {
                        resolved_module_path = Some(candidate_file);
                        break;
                    }
                }
            }
    
            if resolved_module_path.is_none() {
                let mut candidates = vec![];
                if let Ok(entries) = fs::read_dir(&base_module_path) {
                    for entry in entries.flatten() {
                        let path = entry.path();
                        let file_stem_opt = path.file_stem().and_then(|s| s.to_str());
                        let is_valid_ext = path.is_file() && path.extension()
                            .and_then(|e| e.to_str())
                            .map_or(false, |ext| ext == "lc" || ext == "lucia" || ext == "rs");
    
                        let is_dir = path.is_dir();
    
                        if let Some(file_stem) = file_stem_opt {
                            if is_valid_ext || is_dir {
                                candidates.push(file_stem.to_string());
                            }
                        }
                    }
                }
    
                if let Some(closest) = find_closest_match(&module_name, &candidates) {
                    self.stack.pop();
                    return self.raise_with_help(
                        "ImportError",
                        &format!("Module '{}' not found at path '{}'", &module_name, base_module_path.display()),
                        &format!("Did you mean '{}{}{}'?",
                            check_ansi("\x1b[4m", &self.use_colors),
                            closest,
                            check_ansi("\x1b[24m", &self.use_colors),
                        ),
                    );
                }
    
                let display_path = base_module_path
                .display()
                .to_string()
                .trim_start_matches(r"\\?\")
                .to_string();
            
                self.stack.pop();
                return self.raise("ImportError", &format!(
                    "Module '{}' not found at path '{}'",
                    &module_name,
                    display_path
                ));
            }
    
            let module_path = resolved_module_path.unwrap();
    
            if module_path.is_file() {
                if let Some(ext) = module_path.extension().and_then(|e| e.to_str()) {
                    if ext == "lc" || ext == "lucia" || ext == "rs" {
                        let properties_result = self.get_properties_from_file(&module_path);
                        if self.err.is_some() {
                            self.stack.pop();
                            return NULL;
                        }
                        properties.extend(properties_result);
                    } else {
                        self.stack.pop();
                        return self.raise("ImportError", &format!("Unsupported file extension '{}'", ext));
                    }
                } else {
                    self.stack.pop();
                    return self.raise("ImportError", "Module file has no extension");
                }
            } else if module_path.is_dir() {
                let init_path_lc = module_path.join("__init__.lc");
                let init_path_lucia = module_path.join("__init__.lucia");
                if init_path_lc.exists() && init_path_lc.is_file() {
                    let props = self.get_properties_from_file(&init_path_lc);
                    if self.err.is_some() {
                        self.stack.pop();
                        return NULL;
                    }
                    properties.extend(props);
                } else if init_path_lucia.exists() && init_path_lucia.is_file() {
                    let props = self.get_properties_from_file(&init_path_lucia);
                    if self.err.is_some() {
                        self.stack.pop();
                        return NULL;
                    }
                    properties.extend(props);
                }
    
                if let Ok(entries) = fs::read_dir(&module_path) {
                    for entry in entries.flatten() {
                        let path = entry.path();
                        if path.is_file() {
                            if let Some(ext) = path.extension().and_then(|e| e.to_str()) {
                                if (ext == "lc" || ext == "lucia") && path.file_name().and_then(|n| n.to_str()) != Some("__init__.lc") && path.file_name().and_then(|n| n.to_str()) != Some("__init__.lucia") {
                                    let properties_result = self.get_properties_from_file(&path);
                                    if self.err.is_some() {
                                        self.stack.pop();
                                        return NULL;
                                    }
                                    properties.extend(properties_result);
                                } else if ext != "lc" && ext != "lucia" {
                                    self.stack.pop();
                                    return self.raise("ImportError", &format!("Unsupported file extension '{}'", ext));
                                }
                            }
                        }
                    }
                }
            } else {
                self.stack.pop();
                return self.raise("ImportError", &format!("Module path '{}' is neither file nor directory", module_path.display()));
            }
        }
    
        let mut categorized: BTreeMap<&str, Vec<&String>> = BTreeMap::new();
    
        for (name, var) in properties.iter() {
            let category = match &var.value {
                Value::Object(_) => "object",
                Value::Function(_) => "function",
                _ if var.is_final() => "constant",
                _ => "variable",
            };
            categorized.entry(category).or_default().push(name);
        }
    
        let order = ["object", "function", "constant", "variable"];
    
        for &category in &order {
            if let Some(names) = categorized.get(category) {
                for name in names {
                    debug_log(&format!("<Importing {} '{}' from module '{}'>", category, name, module_name), &self.config, Some(self.use_colors.clone()));
                }
            }
        }
        if self.err.is_some() {
            self.stack.pop();
            return NULL;
        }
    
        debug_log(&format!("<Module '{}' imported successfully>", module_name), &self.config, Some(self.use_colors.clone()));
    
        let module_meta = ObjectMetadata {
            name: module_name.clone(),
            properties,
            parameters: Vec::new(),
            is_public: true,
            is_static: true,
            is_final: true,
            state: None,
        };
    
        let class = Class::new(module_name.clone(), module_meta.clone());
        let object = Object::Class(class);
    
        let module = Value::Object(object);
        self.variables.insert(
            alias.to_string(),
            Variable::new(alias.to_string(), module.clone(), "module".to_string(), false, true, true),
        );
    
        self.stack.pop();
    
        module
    }

    fn handle_return(&mut self, statement: HashMap<Value, Value>) -> Value {
        let value = match statement.get(&Value::String("value".to_string())) {
            Some(v) => v,
            None => return self.raise("RuntimeError", "Missing 'value' in return statement"),
        };
    
        let evaluated_value = self.evaluate(value.convert_to_statement());
        if self.err.is_some() {
            return NULL;
        }
    
        self.is_returning = true;
        self.return_value = evaluated_value.clone();
        evaluated_value
    }

    fn handle_function_declaration(&mut self, statement: HashMap<Value, Value>) -> Value {
        let name = match statement.get(&Value::String("name".to_string())) {
            Some(Value::String(n)) => n,
            _ => return self.raise("RuntimeError", "Missing or invalid 'name' in function declaration"),
        };
    
        let pos_args = match statement.get(&Value::String("pos_args".to_string())) {
            Some(Value::List(p)) => p,
            _ => return self.raise("RuntimeError", "Expected a list for 'pos_args' in function declaration"),
        };

        let named_args = match statement.get(&Value::String("named_args".to_string())) {
            Some(Value::Map { keys, values }) => {
                Value::Map {
                    keys: keys.clone(),
                    values: values.clone(),
                }
            }
            _ => return self.raise("RuntimeError", "Expected a list for 'named_args' in function declaration"),
        };
    
        let body = match statement.get(&Value::String("body".to_string())) {
            Some(Value::List(b)) => b,
            _ => return self.raise("RuntimeError", "Expected a list for 'body' in function declaration"),
        };

        let modifiers = match statement.get(&Value::String("modifiers".to_string())) {
            Some(Value::List(m)) => m,
            _ => return self.raise("RuntimeError", "Expected a list for 'modifiers' in function declaration"),
        };
    
        let return_type = match statement.get(&Value::String("return_type".to_string())) {
            Some(Value::Map { keys, values } ) => Value::Map {
                keys: keys.clone(),
                values: values.clone(),
            },
            _ => return self.raise("RuntimeError", "Missing or invalid 'return_type' in function declaration"),
        };

        let return_type_str = self.evaluate(return_type.convert_to_statement());

        if self.err.is_some() {
            return NULL;
        }

        let mut is_public = false;
        let mut is_static = false;
        let mut is_final = false;

        for modifier in modifiers {
            if let Value::String(modifier_str) = modifier {
                match modifier_str.as_str() {
                    "public" => is_public = true,
                    "static" => is_static = true,
                    "final" => is_final = true,
                    "private" => is_public = false,
                    "non-static" => is_static = false,
                    "mutable" => is_final = false,
                    _ => return self.raise("SyntaxError", &format!("Unknown function modifier: {}", modifier_str)),
                }
            } else {
                return self.raise("TypeError", "Function modifiers must be strings");
            }
        }

        let mut parameters = Vec::new();

        for arg in pos_args {
            if let Value::Map { keys, values } = arg {
                let name = match keys.iter().position(|k| k == &Value::String("name".to_string())) {
                    Some(pos) => match &values[pos] {
                        Value::String(n) => n,
                        _ => return self.raise("RuntimeError", "'name' must be a string in function parameter"),
                    },
                    None => return self.raise("RuntimeError", "Missing 'name' in function parameter"),
                };
        
                let type_str_value = match keys.iter().position(|k| k == &Value::String("type".to_string())) {
                    Some(pos) => {
                        let type_val = &values[pos];
                        self.evaluate(type_val.convert_to_statement())
                    }
                    None => return self.raise("RuntimeError", "Missing 'type' in function parameter"),
                };
        
                match &type_str_value {
                    Value::Map { .. } => {
                        parameters.push(Parameter::positional_pt(name.as_str(), &type_str_value));
                    }
                    Value::String(s) => {
                        parameters.push(Parameter::positional(name.as_str(), s.as_str()));
                    }
                    _ => return self.raise("RuntimeError", "Invalid type for function parameter 'type'"),
                }
            } else {
                return self.raise("TypeError", "Expected a map for function parameter");
            }
        }
        
        let named_args_hashmap = named_args.convert_to_hashmap().unwrap_or_else(|| {
            self.raise("TypeError", "Expected a map for named arguments");
            HashMap::new()
        });
        
        for (name_str, info) in named_args_hashmap {
            if let Value::Map { keys, values } = info {
                let type_val = match keys.iter().position(|k| k == &Value::String("type".to_string())) {
                    Some(pos) => &values[pos],
                    None => return self.raise("RuntimeError", "Missing 'type' in named argument"),
                };
        
                let type_eval = self.evaluate(type_val.convert_to_statement());
        
                let value = match keys.iter().position(|k| k == &Value::String("value".to_string())) {
                    Some(pos) => {
                        let val = &values[pos];
                        self.evaluate(val.convert_to_statement())
                    }
                    None => Value::Null,
                };
        
                match &type_eval {
                    Value::Map { .. } => {
                        parameters.push(Parameter::positional_optional_pt(
                            name_str.as_str(),
                            &type_eval,
                            value,
                        ));
                    }
                    Value::String(s) => {
                        parameters.push(Parameter::positional_optional(
                            name_str.as_str(),
                            s.as_str(),
                            value,
                        ));
                    }
                    _ => return self.raise("RuntimeError", "Invalid type for named argument 'type'"),
                }
            } else {
                return self.raise("TypeError", "Expected a map for named argument");
            }
        }        

        let body_formatted: Vec<Statement> = body.iter().map(|v| v.convert_to_statement()).collect();

        let metadata = FunctionMetadata {
            name: name.to_string(),
            parameters,
            return_type: return_type_str,
            is_public,
            is_static,
            is_final,
            is_native: false,
            state: None,
        };

        if self.variables.contains_key(name) {
            if let Some(var) = self.variables.get(name) {
                if var.is_final() {
                    return self.raise("AssigmentError", &format!("Cannot redefine final function '{}'", name));
                }
            }
        }

        self.variables.insert(
            name.to_string(),
            Variable::new(
                name.to_string(),
                create_function(
                    metadata.clone(),
                    body_formatted,
                ),
                "function".to_string(),
                is_public,
                is_static,
                is_final,
            ),
        );
        self.variables.get(name)
            .map_or(NULL, |var| var.value.clone())
    }

    fn handle_throw(&mut self, statement: HashMap<Value, Value>) -> Value {
        let error_type_val = match statement.get(&Value::String("from".to_string())) {
            Some(v) => self.evaluate(Value::convert_to_statement(v)),
            None => return self.raise("RuntimeError", "Missing 'from' in throw statement"),
        };
        
        let error_msg_val = match statement.get(&Value::String("message".to_string())) {
            Some(v) => self.evaluate(Value::convert_to_statement(v)),
            None => return self.raise("RuntimeError", "Missing 'message' in throw statement"),
        };
        
        self.raise(
            to_static(error_type_val.to_string()),
            to_static(error_msg_val.to_string()),
        )
    }

    fn handle_forget(&mut self, statement: HashMap<Value, Value>) -> Value {
        let value = match statement.get(&Value::String("value".to_string())) {
            Some(v) => v,
            None => return self.raise("RuntimeError", "Missing 'value' in forget statement"),
        };
    
        let value_map = match value {
            Value::Map { keys, values } => keys.iter().cloned().zip(values.iter().cloned()).collect::<HashMap<_, _>>(),
            _ => return self.raise("RuntimeError", "Expected a map for forget value"),
        };
    
        let value_type = match value_map.get(&Value::String("type".to_string())) {
            Some(Value::String(t)) => t.as_str(),
            _ => return self.raise("RuntimeError", "Missing or invalid 'type' in forget value"),
        };
    
        match value_type {
            "VARIABLE" => {
                let name = match value_map.get(&Value::String("name".to_string())) {
                    Some(Value::String(n)) => n,
                    _ => return self.raise("RuntimeError", "Missing or invalid 'name' in variable forget"),
                };
    
                if self.variables.remove(name).is_some() {
                    NULL
                } else {
                    self.raise("NameError", &format!("Variable '{}' not found for forget", name))
                }
            }

            "INDEX_ACCESS" => {
                let object_val = match value_map.get(&Value::String("object".to_string())) {
                    Some(v) => self.evaluate(Value::convert_to_statement(v)),
                    None => return self.raise("RuntimeError", "Missing 'object' in index access forget"),
                };
                let variable_name = match value_map.get(&Value::String("object".to_string())) {
                    Some(Value::Map { keys, values }) => {
                        let obj_map: HashMap<_, _> = keys.iter().cloned().zip(values.iter().cloned()).collect();
                        match obj_map.get(&Value::String("type".to_string())) {
                            Some(Value::String(t)) if t == "VARIABLE" => {
                                match obj_map.get(&Value::String("name".to_string())) {
                                    Some(Value::String(name)) => Some(name.clone()),
                                    _ => None,
                                }
                            }
                            _ => None,
                        }
                    }
                    _ => None,
                };

                let access_val = match value_map.get(&Value::String("access".to_string())) {
                    Some(v) => v,
                    None => return self.raise("RuntimeError", "Missing 'access' in index access forget"),
                };
                let access_hashmap = match access_val {
                    Value::Map { keys, values } => keys.iter().cloned().zip(values.iter().cloned()).collect::<HashMap<_, _>>(),
                    _ => return self.raise("RuntimeError", "Expected a map for index access forget"),
                };
                let start_val_opt = access_hashmap.get(&Value::String("start".to_string()));
                let end_val_opt = access_hashmap.get(&Value::String("end".to_string()));

                let len = match &object_val {
                    Value::String(s) => s.chars().count(),
                    Value::List(l) => l.len(),
                    Value::Bytes(b) => b.len(),
                    Value::Tuple(_) => return self.raise("TypeError", "Tuples are immutable, their indexes cannot be forgotten."),
                    Value::Map { keys, values } => {
                        if keys.len() != values.len() {
                            return self.raise("TypeError", "Map keys and values must have the same length");
                        }
                        keys.len()
                    }
                    _ => return self.raise("TypeError", "Object not indexable"),
                };

                let start_val = match start_val_opt {
                    Some(v) => self.evaluate(v.convert_to_statement()),
                    None => NULL,
                };
                let end_val = match end_val_opt {
                    Some(v) => self.evaluate(v.convert_to_statement()),
                    None => NULL,
                };

                let start_idx = if start_val == NULL {
                    0
                } else {
                    match self.to_index(&start_val, len) {
                        Ok(i) => i,
                        Err(e) => return e,
                    }
                };

                let end_idx = if end_val == NULL {
                    start_idx + 1
                } else {
                    match self.to_index(&end_val, len) {
                        Ok(i) => i,
                        Err(e) => return e,
                    }
                };                

                if start_idx > end_idx || end_idx > len {
                    return self.raise("IndexError", "Invalid slice indices");
                }

                let changed_value = match &object_val {
                    Value::List(l) => {
                        let mut new_list = l.clone();
                        new_list.drain(start_idx..end_idx);
                        Value::List(new_list)
                    }
                    Value::Bytes(b) => {
                        let mut new_bytes = b.clone();
                        new_bytes.drain(start_idx..end_idx);
                        Value::Bytes(new_bytes)
                    }
                    Value::String(_) => {
                        return self.raise("TypeError", "Cannot forget slice on immutable string");
                    }
                    Value::Tuple(_) => {
                        return self.raise("TypeError", "Tuples are immutable, their indexes cannot be forgotten.");
                    }
                    Value::Map { keys, values } => {
                        let mut new_keys = keys.clone();
                        let mut new_values = values.clone();
                        if start_idx < new_keys.len() && end_idx <= new_keys.len() {
                            new_keys.drain(start_idx..end_idx);
                            new_values.drain(start_idx..end_idx);
                        }
                        Value::Map { keys: new_keys, values: new_values }
                    }
                    _ => return self.raise("TypeError", "Object not indexable for forget"),
                };

                if let Some(var_name) = variable_name {
                    if let Some(var) = self.variables.get_mut(&var_name) {
                        var.set_value(changed_value.clone());
                    }
                }

                changed_value
            }

            _ => self.raise("RuntimeError", &format!("Unsupported forget value type: {}", value_type)),
        }
    }    

    fn handle_type(&mut self, statement: HashMap<Value, Value>) -> Value {
        let default_type = Value::String("any".to_string());
    
        let type_kind = match statement.get(&Value::String("type_kind".to_string())) {
            Some(kind) => kind,
            None => {
                self.raise("RuntimeError", "missing 'type_kind' in statement");
                return NULL;
            }
        };
        
        let kind_str = match type_kind {
            Value::String(s) => s.as_str(),
            _ => {
                self.raise("RuntimeError", "Invalid 'kind' for type statement");
                return NULL;
            }
        };
        
        match kind_str {
            "simple" => {
                let type_name = statement.get(&Value::String("value".to_string())).unwrap_or(&default_type);
        
                if let Value::String(s) = type_name {
                    if VALID_TYPES.contains(&s.as_str()) {
                        return Value::String(s.clone());
                    }
        
                    self.raise(
                        "TypeError",
                        &format!(
                            "Invalid type '{}'. Valid types are: {}",
                            s,
                            VALID_TYPES.join(", ")
                        ),
                    );
                    return NULL;
                } else {
                    self.raise("TypeError", "Type value is not a string");
                    return NULL;
                }
            }
            "function" | "indexed" => {
                let mut keys = vec![Value::String("_note".to_string())];
                let mut values = vec![Value::String(create_note(
                    "This map is for the interpreter's internal use to track types. You don't need to worry about it.",
                    Some(self.use_colors),
                    &self.config.color_scheme.note,
                ))];                

                keys.extend(statement.keys().cloned());
                values.extend(statement.values().cloned());

                return Value::Map {
                    keys,
                    values,
                }
            }
            _ => {
                self.raise("RuntimeError", "Invalid 'kind' for type statement");
                return NULL;
            }
        }
        
        NULL
    }    

    fn handle_if(&mut self, statement: HashMap<Value, Value>) -> Value {
        let condition = match statement.get(&Value::String("condition".to_string())) {
            Some(v) => v,
            None => return self.raise("RuntimeError", "Missing 'condition' in if statement"),
        };
    
        let condition_value = self.evaluate(condition.convert_to_statement());
        if self.err.is_some() {
            return NULL;
        }
    
        let body = match statement.get(&Value::String("body".to_string())) {
            Some(Value::List(body)) => body,
            _ => return self.raise("RuntimeError", "Expected a list for 'body' in if statement"),
        };
    
        let else_body = match statement.get(&Value::String("else_body".to_string())) {
            Some(Value::List(body)) => Some(body),
            _ => None,
        };
    
        let stmts_to_run = if condition_value.is_truthy() { Some(body) } else { else_body };
    
        if let Some(stmts) = stmts_to_run {
            for stmt in stmts {
                if !stmt.is_statement() {
                    continue;
                }
                let result = self.evaluate(stmt.convert_to_statement());
                if self.err.is_some() {
                    return NULL;
                }
                if result != NULL {
                    return result;
                }
            }
        }
    
        NULL
    }

    fn handle_tuple(&mut self, statement: HashMap<Value, Value>) -> Value {
        let items = match statement.get(&Value::String("items".to_string())) {
            Some(Value::List(items)) => items,
            _ => return Value::Tuple(vec![]),
        };
    
        let mut values = Vec::new();
        for item in items {
            let value = self.evaluate(item.convert_to_statement());
            if self.err.is_some() {
                return NULL;
            }
            values.push(value);
        }
    
        Value::Tuple(values)
    }

    fn handle_try(&mut self, statement: HashMap<Value, Value>) -> Value {
        let stmt_type = match statement.get(&Value::String("type".to_string())) {
            Some(Value::String(s)) => s.as_str(),
            _ => return self.raise("RuntimeError", "Missing or invalid 'type' in try statement"),
        };
    
        let body = match statement.get(&Value::String("body".to_string())) {
            Some(Value::List(body)) => body,
            _ => return self.raise("RuntimeError", "Expected a list for 'body' in try statement"),
        };
    
        let mut catch_body = &vec![];
        let mut exception_vars = &vec![];
    
        if stmt_type == "TRY_CATCH" {
            catch_body = match statement.get(&Value::String("catch_body".to_string())) {
                Some(Value::List(catch_body)) => catch_body,
                _ => return self.raise("RuntimeError", "Expected a list for 'catch' in try-catch statement"),
            };
    
            exception_vars = match statement.get(&Value::String("exception_vars".to_string())) {
                Some(Value::List(vars)) => vars,
                _ => return self.raise("RuntimeError", "Expected a list for 'exception_vars' in try-catch statement"),
            };
    
            if exception_vars.len() > 3 {
                return self.raise(
                    "SyntaxError",
                    "Too many exception variables (max is 3, err_type, err_msg, err_help)",
                );
            }
    
            if exception_vars.is_empty() {
                return self.raise_with_help(
                    "SyntaxError",
                    "No exception variables provided",
                    &format!(
                        "Use '_' if you want to ignore the caught exception(s): 'try: ... end catch ( {}_{} ): ... end'",
                        hex_to_ansi("#1CC58B", Some(self.use_colors)),
                        hex_to_ansi(&self.config.color_scheme.help, Some(self.use_colors)),
                    ),
                );
            }
    
            let mut seen = std::collections::HashSet::new();
            for var in exception_vars {
                if let Value::String(name) = var {
                    if !seen.insert(name) {
                        return self.raise("NameError", &format!("Duplicate exception variable name '{}'", name));
                    }
                } else {
                    return self.raise("RuntimeError", "Invalid exception variable type");
                }
            }
        }
    
        for stmt in body {
            if !stmt.is_statement() {
                continue;
            }
    
            let result = self.evaluate(stmt.convert_to_statement());
            if self.err.is_some() {
                if stmt_type != "TRY_CATCH" {
                    return self.err.take().map_or(NULL, |_| NULL);
                }
    
                let err = self.err.take().unwrap();
                self.err = None;
    
                match exception_vars.len() {
                    1 => {
                        if let Value::String(name) = &exception_vars[0] {
                            let tuple = Value::Tuple(vec![
                                Value::String(err.error_type.clone()),
                                Value::String(err.msg.clone()),
                            ]);
                            self.variables.insert(
                                name.clone(),
                                Variable::new(name.clone(), tuple, "tuple".to_string(), false, true, true),
                            );
                        }
                    }
                    2 => {
                        if let Value::String(name) = &exception_vars[0] {
                            self.variables.insert(
                                name.clone(),
                                Variable::new(
                                    name.clone(),
                                    Value::String(err.error_type.clone()),
                                    "string".to_string(),
                                    false,
                                    true,
                                    true,
                                ),
                            );
                        }
                        if let Value::String(name) = &exception_vars[1] {
                            self.variables.insert(
                                name.clone(),
                                Variable::new(
                                    name.clone(),
                                    Value::String(err.msg.clone()),
                                    "string".to_string(),
                                    false,
                                    true,
                                    true,
                                ),
                            );
                        }
                    }
                    3 => {
                        if let Value::String(name) = &exception_vars[0] {
                            self.variables.insert(
                                name.clone(),
                                Variable::new(
                                    name.clone(),
                                    Value::String(err.error_type.clone()),
                                    "string".to_string(),
                                    false,
                                    true,
                                    true,
                                ),
                            );
                        }
                        if let Value::String(name) = &exception_vars[1] {
                            self.variables.insert(
                                name.clone(),
                                Variable::new(
                                    name.clone(),
                                    Value::String(err.msg.clone()),
                                    "string".to_string(),
                                    false,
                                    true,
                                    true,
                                ),
                            );
                        }
                        if let Value::String(name) = &exception_vars[2] {
                            self.variables.insert(
                                name.clone(),
                                Variable::new(
                                    name.clone(),
                                    Value::String(err.help.unwrap_or_default()),
                                    "string".to_string(),
                                    false,
                                    true,
                                    true,
                                ),
                            );
                        }
                    }
                    _ => {}
                }
    
                for catch_stmt in catch_body {
                    if !catch_stmt.is_statement() {
                        continue;
                    }
                    let _ = self.evaluate(catch_stmt.convert_to_statement());
                }
    
                return NULL;
            }
        }
    
        NULL
    }

    fn handle_assignment(&mut self, statement: HashMap<Value, Value>) -> Value {
        let left = match statement.get(&Value::String("left".to_string())) {
            Some(v) => v,
            None => return self.raise("RuntimeError", "Missing 'left' in assignment statement"),
        };
        let right = match statement.get(&Value::String("right".to_string())) {
            Some(v) => v,
            None => return self.raise("RuntimeError", "Missing 'right' in assignment statement"),
        };
    
        let right_value = self.evaluate(right.convert_to_statement());
    
        if self.err.is_some() {
            return NULL;
        }
    
        let left_hashmap = match left {
            Value::Map { keys, values } => keys.iter().cloned().zip(values.iter().cloned()).collect::<HashMap<_, _>>(),
            Value::Null => Statement::Statement {
                keys: vec![
                    Value::String("type".to_string()),
                    Value::String("value".to_string())
                ],
                values: vec![
                    Value::String("BOOLEAN".to_string()),
                    Value::String("null".to_string())
                ],
                column: 0,
                line: 0,
            }.convert_to_hashmap(),
            _ => return self.raise("RuntimeError", "Expected a map for assignment"),
        };
    
        let left_type = match left_hashmap.get(&Value::String("type".to_string())) {
            Some(Value::String(t)) => t,
            _ => return self.raise("RuntimeError", "Missing or invalid 'type' in assignment left"),
        };
    
        match left_type.as_str() {
            "VARIABLE" => {
                let name = match left_hashmap.get(&Value::String("name".to_string())) {
                    Some(Value::String(n)) => n,
                    _ => return self.raise("RuntimeError", "Missing or invalid 'name' in variable assignment"),
                };
    
                let expected_type = {
                    let var = match self.variables.get(name) {
                        Some(v) => v,
                        None => return self.raise_with_help(
                            "NameError",
                            &format!("Variable '{}' is not defined", name),
                            &format!(
                                "Use this instead: '{}{}: {} = {}{}'",
                                check_ansi("\x1b[4m", &self.use_colors),
                                name,
                                right_value.type_name(),
                                format_value(&right_value),
                                check_ansi("\x1b[24m", &self.use_colors),
                            ),
                        ),
                    };
                    var.type_name().to_owned()
                };
    
                if !self.check_type(&right_value, &Value::String(expected_type.clone()), true) {
                    return self.raise(
                        "TypeError",
                        &format!(
                            "Invalid type for variable '{}': expected '{}', got '{}'",
                            name, expected_type, right_value.type_name()
                        ),
                    );
                }
    
                let var = self.variables.get_mut(name).unwrap();
    
                if var.is_final() {
                    return self.raise(
                        "AssignmentError",
                        &format!("Cannot assign to final variable '{}'", name),
                    );
                }
    
                var.set_value(right_value);
                var.value.clone()
            }
            "INDEX_ACCESS" => {
                let variable_name: Option<String> = match left_hashmap.get(&Value::String("object".to_string())) {
                    Some(Value::Map { keys, values }) => {
                        keys.iter()
                            .zip(values.iter())
                            .find_map(|(k, v)| if k == &Value::String("name".to_string()) {
                                if let Value::String(name) = v {
                                    Some(name.clone())
                                } else {
                                    None
                                }
                            } else {
                                None
                            })
                    }
                    _ => None,
                };

                let is_map = if let Some(name) = &variable_name {
                    match self.variables.get(name) {
                        Some(var) => matches!(var.value, Value::Map { .. }),
                        None => return self.raise("NameError", &format!("Variable '{}' not found for index assignment", name)),
                    }
                } else {
                    return self.raise("RuntimeError", "Expected variable name for index assignment object");
                };                

                let access_val = match left_hashmap.get(&Value::String("access".to_string())) {
                    Some(v) => v,
                    None => return self.raise("RuntimeError", "Missing 'access' in index access"),
                };

                let access_hashmap = match access_val {
                    Value::Map { keys, values } => keys.iter().cloned().zip(values.iter().cloned()).collect::<HashMap<_, _>>(),
                    _ => return self.raise("RuntimeError", "Expected a map for index access"),
                };

                if let Some(name) = &variable_name {
                    let len = match self.variables.get(name) {
                        Some(var) => match &var.value {
                            Value::String(s) => s.chars().count(),
                            Value::List(l) => l.len(),
                            Value::Bytes(b) => b.len(),
                            Value::Tuple(t) => t.len(),
                            Value::Map { keys, values } => {
                                if keys.len() != values.len() {
                                    return self.raise("TypeError", "Map keys and values must have the same length");
                                }
                                keys.len()
                            }
                            _ => return self.raise("TypeError", "Object not indexable"),
                        },
                        None => return self.raise("NameError", &format!("Variable '{}' not found for index assignment", name)),
                    };
                
                    let start_val_opt = access_hashmap.get(&Value::String("start".to_string()));
                    let end_val_opt = access_hashmap.get(&Value::String("end".to_string()));
                
                    let evaluated_key = if let Some(start_val) = start_val_opt {
                        Some(self.evaluate(Value::convert_to_statement(start_val)))
                    } else {
                        None
                    };

                    if is_map {
                        let evaluated_key = if let Some(start_val) = start_val_opt {
                            self.evaluate(Value::convert_to_statement(start_val))
                        } else {
                            return self.raise("RuntimeError", "Missing index for map assignment");
                        };
                    
                        if let Some(var) = self.variables.get_mut(name) {
                            if var.is_final() {
                                return self.raise("AssignmentError", &format!("Cannot assign to final variable '{}'", name));
                            }
                            if let Value::Map { keys, values } = &mut var.value {
                                if let Some(pos) = keys.iter().position(|k| k == &evaluated_key) {
                                    values[pos] = right_value;
                                } else {
                                    keys.push(evaluated_key);
                                    values.push(right_value);
                                }
                                return NULL;
                            }                            
                        } else {
                            return self.raise("NameError", &format!("Variable '{}' not found for index assignment", name));
                        }
                    }                    
                
                    let start_idx = match start_val_opt {
                        Some(v) => match self.to_index(v, len) {
                            Ok(i) => i,
                            Err(e) => return e,
                        },
                        None => 0,
                    };
                
                    let end_idx = match end_val_opt {
                        Some(v) => match self.to_index(v, len) {
                            Ok(i) => i,
                            Err(e) => return e,
                        },
                        None => len,
                    };
                    
                    if let Some(var) = self.variables.get_mut(name) {
                        if var.is_final() {
                            return self.raise("AssignmentError", &format!("Cannot assign to final variable '{}'", name));
                        }
                
                        if let Value::Map { keys, values } = &mut var.value {
                            if let Some(key) = evaluated_key {
                                if let Some(pos) = keys.iter().position(|k| k == &key) {
                                    values[pos] = right_value;
                                    return NULL;
                                } else {
                                    return self.raise("KeyError", &format!("Key '{}' not found in map", key));
                                }
                            } else {
                                return self.raise("RuntimeError", "Missing index for map assignment");
                            }
                        }
                
                        match &mut var.value {
                            Value::String(_) => {
                                return self.raise("TypeError", "Cannot assign to string index (immutable)");
                            }
                            Value::List(l) => {
                                if start_idx >= l.len() || end_idx > l.len() || start_idx > end_idx {
                                    return self.raise("IndexError", "Invalid slice indices");
                                }
                                if start_idx + 1 == end_idx {
                                    l[start_idx] = right_value;
                                } else {
                                    return self.raise("NotImplementedError", "Slice assignment not supported");
                                }
                            }
                            Value::Bytes(b) => {
                                if let Value::Int(i) = right_value {
                                    if start_idx >= b.len() || end_idx > b.len() || start_idx > end_idx {
                                        return self.raise("IndexError", "Invalid slice indices");
                                    }
                                    if start_idx + 1 == end_idx {
                                        let int_val = i.to_i64().unwrap_or(-1);
                                        if int_val < 0 || int_val > 255 {
                                            return self.raise("ValueError", "Byte value out of range");
                                        }
                                        b[start_idx] = int_val as u8;
                                    } else {
                                        return self.raise("NotImplementedError", "Slice assignment not supported");
                                    }
                                } else {
                                    return self.raise("TypeError", "Expected integer for byte assignment");
                                }
                            }
                            Value::Tuple(_) => {
                                return self.raise("TypeError", "Tuples are immutable, their indexes cannot be assigned to.");
                            }
                            Value::Map { keys: _, values: _ } => {
                                return self.raise("RuntimeError", "Unexpected state: map handled earlier");
                            }
                            _ => return self.raise("TypeError", "Object not indexable"),
                        }
                        NULL
                    } else {
                        self.raise("NameError", &format!("Variable '{}' not found for index assignment", name))
                    }
                } else {
                    self.raise("RuntimeError", "Expected variable name for index assignment object")
                }
            }
            _ => self.raise("TypeError", &format!("Cannot modify type '{}'", get_type_from_token_name(left_type))),
        }
    }
    
    fn handle_index_access(&mut self, statement: HashMap<Value, Value>) -> Value {
        let object_val = match statement.get(&Value::String("object".to_string())) {
            Some(v) => self.evaluate(Value::convert_to_statement(v)),
            None => return self.raise("RuntimeError", "Missing object in index access"),
        };
    
        let access_val = match statement.get(&Value::String("access".to_string())) {
            Some(v) => v,
            None => return self.raise("RuntimeError", "Missing access in index access"),
        };
    
        let access_hashmap = match access_val {
            Value::Map { keys, values } => keys.iter().cloned().zip(values.iter().cloned()).collect::<HashMap<_, _>>(),
            _ => return self.raise("RuntimeError", "Expected a map for index access"),
        };
    
        let start_val_opt = access_hashmap.get(&Value::String("start".to_string()));
        let end_val_opt = access_hashmap.get(&Value::String("end".to_string()));
    
        let len = match &object_val {
            Value::String(s) => s.chars().count(),
            Value::List(l) => l.len(),
            Value::Bytes(b) => b.len(),
            Value::Tuple(t) => t.len(),
            Value::Map { keys, values } => {
                if keys.len() != values.len() {
                    return self.raise("TypeError", "Map keys and values must have the same length");
                }
                keys.len()
            }
            _ => return self.raise("TypeError", "Object not indexable"),
        };
    
        let start_eval_opt = start_val_opt
            .map(|v| self.evaluate(v.convert_to_statement()))
            .and_then(|val| if val == Value::Null { None } else { Some(val) });
        let end_eval_opt = end_val_opt
            .map(|v| self.evaluate(v.convert_to_statement()))
            .and_then(|val| if val == Value::Null { None } else { Some(val) });
    
        if self.err.is_some() {
            return Value::Null;
        }
    
        let mut to_index = |val: &Value| -> Result<usize, Value> {
            match val {
                Value::Int(i) => {
                    let idx = i.to_i64().map_err(|_| self.raise("ConversionError", "Failed to convert Int to isize"))? as isize;
                    let adjusted = if idx < 0 { len as isize + idx } else { idx };
                    if adjusted >= 0 && (adjusted as usize) <= len {
                        Ok(adjusted as usize)
                    } else {
                        Err(self.raise("IndexError", "Index out of range"))
                    }
                }
                Value::String(s) => {
                    let idx: isize = s.parse().map_err(|_| self.raise("ConversionError", "Failed to parse string to int"))?;
                    let adjusted = if idx < 0 { len as isize + idx } else { idx };
                    if adjusted >= 0 && (adjusted as usize) <= len {
                        Ok(adjusted as usize)
                    } else {
                        Err(self.raise("IndexError", "Index out of range"))
                    }
                }
                Value::Float(f) => {
                    if !f.is_integer_like() {
                        return Err(self.raise("ConversionError", "Float index must have zero fractional part"));
                    }
                    let idx = f.to_f64().map_err(|_| self.raise("ConversionError", "Failed to convert Float to isize"))? as isize;
                    let adjusted = if idx < 0 { len as isize + idx } else { idx };
                    if adjusted >= 0 && (adjusted as usize) <= len {
                        Ok(adjusted as usize)
                    } else {
                        Err(self.raise("IndexError", "Index out of range"))
                    }
                }
                _ => Err(self.raise("TypeError", "Index must be Int, String or Float with no fraction")),
            }
        };

        match (start_eval_opt, end_eval_opt) {
            (Some(start_val), None) => match &object_val {
                Value::String(_) | Value::List(_) | Value::Bytes(_) | Value::Tuple(_) => {
                    let start_idx = match self.to_index(&start_val, len) {
                        Ok(i) => i,
                        Err(e) => return e,
                    };
    
                    match &object_val {
                        Value::String(s) => s.chars().nth(start_idx).map(|c| Value::String(c.to_string())).unwrap_or_else(|| self.raise("IndexError", "Index out of range")),
                        Value::List(l) => l.get(start_idx).cloned().unwrap_or_else(|| self.raise("IndexError", "Index out of range")),
                        Value::Bytes(b) => b.get(start_idx).map(|&b| Value::Int((b as i64).into())).unwrap_or_else(|| self.raise("IndexError", "Index out of range")),
                        Value::Tuple(t) => t.get(start_idx).cloned().unwrap_or_else(|| self.raise("IndexError", "Index out of range")),
                        _ => unreachable!(),
                    }
                }
                Value::Map { keys, values } => {
                    let key_val = start_val;
                    for (k, v) in keys.iter().zip(values.iter()) {
                        if k == &key_val {
                            return v.clone();
                        }
                    }
                    return self.raise("KeyError", "Key not found in map");
                }
                _ => return self.raise("TypeError", "Start must be int or string"),
            },
            (start_opt, end_opt) => {
                let start_idx = match start_opt {
                    Some(ref v) => match self.to_index(v, len) {
                        Ok(i) => i,
                        Err(e) => return e,
                    },
                    None => 0,
                };
    
                let end_idx = match end_opt {
                    Some(ref v) => match self.to_index(v, len) {
                        Ok(i) => i,
                        Err(e) => return e,
                    },
                    None => len,
                };
    
                if start_idx > end_idx {
                    return self.raise("IndexError", "Start index greater than end index");
                }
    
                match &object_val {
                    Value::String(s) => {
                        let slice = s.chars().skip(start_idx).take(end_idx - start_idx).collect::<String>();
                        Value::String(slice)
                    }
                    Value::List(l) => Value::List(l.get(start_idx..end_idx).unwrap_or(&[]).to_vec()),
                    Value::Bytes(b) => Value::Bytes(b.get(start_idx..end_idx).unwrap_or(&[]).to_vec()),
                    Value::Tuple(t) => Value::Tuple(t.get(start_idx..end_idx).unwrap_or(&[]).to_vec()),
                    _ => return self.raise("TypeError", "Object not sliceable"),
                }
            }
        }
    }
    
    fn handle_variable_declaration(&mut self, statement: HashMap<Value, Value>) -> Value {
        let name = match statement.get(&Value::String("name".to_string())) {
            Some(Value::String(s)) => s,
            _ => return self.raise("SyntaxError", "Expected a string for variable name"),
        };
    
        let value = match statement.get(&Value::String("value".to_string())) {
            Some(v) => v,
            None => return self.raise("RuntimeError", "Missing 'value' in variable declaration"),
        };
    
        let value = self.evaluate(value.convert_to_statement());
    
        let declared_type = match statement.get(&Value::String("var_type".to_string())) {
            Some(t) => self.evaluate(t.convert_to_statement()),
            _ => Value::String("any".to_string()),
        };

        let modifiers = match statement.get(&Value::String("modifiers".to_string())) {
            Some(Value::List(mods)) => mods,
            _ => &vec![],
        };

        let is_public = modifiers.iter().any(|m| m == &Value::String("public".to_string()));
        let is_final = modifiers.iter().any(|m| m == &Value::String("final".to_string()));
        let is_static = modifiers.iter().any(|m| m == &Value::String("static".to_string()));

        if self.err.is_some() {
            return NULL;
        }
    
        if !self.check_type(&value.clone(), &declared_type, false) {
            return self.raise("TypeError", &format!("Variable '{}' declared with type '{}', but value is of type '{}'", name, format_type(&declared_type), value.type_name()));
        }
    
        let variable = Variable::new(name.to_string(), value, declared_type.to_string(), is_static, is_public, is_final);
        self.variables.insert(name.to_string(), variable);
    
        NULL
    }

    fn handle_variable(&mut self, statement: HashMap<Value, Value>) -> Value {
        let name = match statement.get(&Value::String("name".to_string())) {
            Some(Value::String(s)) => s,
            _ => return self.raise("SyntaxError", "Expected a string for variable name"),
        };
    
        if let Some(var) = self.variables.get(name) {
            return var.get_value().clone();
        } else {
            let lib_dir = PathBuf::from(self.config.home_dir.clone()).join("libs").join(name);
            let extensions = ["lc", "lucia", "rs", ""];
            for ext in extensions.iter() {
                let candidate = lib_dir.with_extension(ext);
                if candidate.exists() {
                    return self.raise_with_help(
                        "ImportError",
                        &format!("Variable '{}' is not defined.", name),
                        &format!("Maybe you forgot to import '{}'? Use '{}import {} from \"{}\"{}'.", name, 
                            check_ansi("\x1b[4m", &self.use_colors),
                            name,
                            candidate.display(),
                            check_ansi("\x1b[24m", &self.use_colors),
                        ),
                    );
                }
            }
    
            let available_names: Vec<String> = self.variables.keys().cloned().collect();
            if let Some(closest) = find_closest_match(name, &available_names) {
                return self.raise_with_help(
                    "NameError",
                    &format!("Variable '{}' is not defined.", name),
                    &format!("Did you mean '{}{}{}'?",
                        check_ansi("\x1b[4m", &self.use_colors),
                        closest,
                        check_ansi("\x1b[24m", &self.use_colors),
                    ),
                );
            } else {
                return self.raise("NameError", &format!("Variable '{}' is not defined.", name));
            }
        }
    }

    fn handle_for_loop(&mut self, statement: HashMap<Value, Value>) -> Value {
        let iterable = match statement.get(&Value::String("iterable".to_string())) {
            Some(v) => v,
            None => return self.raise("RuntimeError", "Missing 'iterable' in for loop statement"),
        };

        let iterable_value = self.evaluate(iterable.convert_to_statement());
        if !iterable_value.is_iterable() {
            return self.raise("TypeError", "Expected an iterable for 'for' loop");
        }

        let body = match statement.get(&Value::String("body".to_string())) {
            Some(Value::List(body)) => body,
            _ => return self.raise("RuntimeError", "Expected a list for 'body' in for loop statement"),
        };

        let variable_name = match statement.get(&Value::String("variable".to_string())) {
            Some(Value::String(name)) => name,
            _ => return self.raise("RuntimeError", "Expected a string for 'variable' in for loop statement"),
        };

        let mut result = NULL;
        for item in iterable_value.iter() {
            let mut local_vars = self.variables.clone();
            local_vars.insert(
                variable_name.to_string(),
                Variable::new(variable_name.to_string(), item.clone(), item.type_name(), false, true, true),
            );
            self.variables = local_vars;

            for stmt in body {
                result = self.evaluate(stmt.convert_to_statement());
                if self.is_returning {
                    return result;
                }
            }
        }

        result
    }

    fn handle_iterable(&mut self, statement: HashMap<Value, Value>) -> Value {
        let Some(iterable) = statement.get(&Value::String("iterable_type".to_string())).cloned() else {
            return self.raise("IterableError", "Missing 'iterable' in iterable statement");
        };
        
        let iterable_type = statement.get(&Value::String("iterable_type".to_string()))
            .and_then(|v| Some(v.to_string()))
            .unwrap_or_else(|| "LIST".to_string());
        
        match iterable_type.as_str() {
            "LIST" => {
                let elements = statement.get(&Value::String("elements".to_string())).unwrap_or_else(|| {
                    self.raise("IterableError", "Missing 'elements' in iterable statement");
                    &NULL
                });

                if let Value::List(elements_list) = elements {
                    let mut evaluated_elements = Vec::new();
                    for element in elements_list {
                        evaluated_elements.push(self.evaluate(element.convert_to_statement()));
                    }
                    Value::List(evaluated_elements)
                } else {
                    self.raise("TypeError", "Expected 'elements' to be a list")
                }
            }
            "LIST_COMPLETION" => {
                let seed_raw = statement.get(&Value::String("seed".to_string()))
                    .cloned()
                    .unwrap_or(Value::List(vec![]));

                let end_raw = statement.get(&Value::String("end".to_string()))
                    .cloned()
                    .unwrap_or(Value::Null);

                let pattern_flag = statement.get(&Value::String("pattern_reg".to_string()))
                    .cloned()
                    .unwrap_or(Value::Boolean(false));

                let seed: Vec<Value> = match &seed_raw {
                    Value::List(elements) => elements.clone(),
                    _ => {
                        self.raise("TypeError", "Expected 'seed' to be a list");
                        return Value::Null;
                    }
                };

                let evaluated_seed: Vec<Value> = seed.into_iter().map(|map_val| {
                    if let Value::Map { .. } = map_val {
                        self.evaluate(map_val.convert_to_statement())
                    } else {
                        self.raise("RuntimeError", "Expected all elements in seed to be Map");
                        Value::Null
                    }
                }).collect();

                if self.err.is_some() {
                    return Value::Null;
                }

                let end = self.evaluate(end_raw.convert_to_statement());

                let pattern_flag_bool: bool = match pattern_flag {
                    Value::Boolean(b) => b,
                    _ => {
                        self.raise("RuntimeError", "Expected 'pattern_reg' to be a boolean");
                        return Value::Null;
                    }
                };

                if !pattern_flag_bool {
                    let len = evaluated_seed.len();
                    if len == 0 {
                        self.raise("ValueError", "Seed list cannot be empty");
                        return Value::Null;
                    }
                
                    for v in &evaluated_seed {
                        if !matches!(v, Value::Int(_)) {
                            self.raise("TypeError", "Seed elements must be Int");
                            return Value::Null;
                        }
                    }
                
                    // Convert BigInt seed elements to i64 or raise error and return early
                    let nums_i64: Vec<i64> = {
                        let mut temp = Vec::with_capacity(evaluated_seed.len());
                        for v in &evaluated_seed {
                            if let Value::Int(i) = v {
                                match i.to_i64() {
                                    Ok(n) => temp.push(n),
                                    Err(_) => {
                                        self.raise("OverflowError", "Seed element out of i64 range");
                                        return Value::Null;
                                    }
                                }                                
                            } else {
                                unreachable!();
                            }
                        }
                        temp
                    };
                
                    if len >= 2 {
                        let initial_step = nums_i64[1] - nums_i64[0];
                        for i in 1..(len - 1) {
                            if nums_i64[i + 1] - nums_i64[i] != initial_step {
                                self.raise("PatternCompletionError", "Seed values do not have consistent step");
                                return Value::Null;
                            }
                        }
                    }
                
                    let end_i64 = match &end {
                        Value::Int(i) => i.to_i64().unwrap_or_else(|_| {
                            self.raise("OverflowError", "End value out of i64 range");
                            0
                        }),
                        _ => {
                            self.raise("TypeError", "End value must be Int");
                            return Value::Null;
                        }
                    };
                
                    let step = if len == 1 {
                        if nums_i64[0] <= end_i64 { 1 } else { -1 }
                    } else {
                        nums_i64[1] - nums_i64[0]
                    };
                
                    if step == 0 {
                        self.raise("ValueError", "Step cannot be zero");
                        return Value::Null;
                    }
                
                    let last_val = nums_i64[len - 1];
                    let diff = end_i64 - last_val;
                
                    if (step > 0 && diff < 0) || (step < 0 && diff > 0) {
                        self.raise("PatternCompletionError", "End value is unreachable with given seed and step");
                        return Value::Null;
                    }
                
                    if diff % step != 0 {
                        self.raise("PatternCompletionError", "Pattern does not fit into range defined by end");
                        return Value::Null;
                    }
                
                    let total_steps = (diff / step).abs() as usize;
                
                    let mut result_list = Vec::with_capacity(len + total_steps);
                
                    result_list.extend_from_slice(&evaluated_seed);
                
                    let mut current = last_val;
                    for _ in 0..total_steps {
                        current += step;
                        result_list.push(Value::Int(Int::from_i64(current)));
                    }
                
                    return Value::List(result_list);
                }

                // omg im a god it works yay
                let vec_f64 = match predict_sequence(evaluated_seed.clone(), end) {
                    Ok(v) => v,
                    Err((err_type, err_msg, err_help)) => {
                        if err_help.is_empty() {
                            return self.raise(err_type, &err_msg);
                        } else {
                            return self.raise_with_help(err_type, &err_msg, &err_help);
                        }
                    }
                };
                let contains_float = evaluated_seed.iter().any(|v| matches!(v, Value::Float(_)));

                let result_list: Vec<Value> = if contains_float {
                    vec_f64.into_iter()
                        .map(|v| Value::Float(Float::from_f64(v)))
                        .collect()
                } else {
                    vec_f64.into_iter()
                        .map(|v| Value::Int(Int::from_i64(v as i64)))
                        .collect()
                };

                return Value::List(result_list);
            }
            _ => self.raise("TypeError", &format!("Unsupported iterable type: {}", iterable_type)),
        }
    }

    fn handle_method_call(&mut self, statement: HashMap<Value, Value>) -> Value {
        let Some(object) = statement.get(&Value::String("object".to_string())).cloned() else {
            return Value::Error("RuntimeError", "Missing 'object' in method call");
        };
    
        let Some(Value::String(method)) = statement.get(&Value::String("method".to_string())).cloned() else {
            return Value::Error("RuntimeError", "Missing or invalid 'method' in method call");
        };
    
        let (pos_args, named_args) = match self.translate_args(
            statement.get(&Value::String("pos_args".to_string())).cloned().unwrap_or(Value::List(vec![])),
            statement.get(&Value::String("named_args".to_string())).cloned().unwrap_or(Value::Map { keys: vec![], values: vec![] }),
        ) {
            Ok(args) => args,
            Err(err) => {
                return self.raise("RuntimeError", to_static(err.to_string()));
            }
        };
        
    
        let method_name = method.as_str();
        let object_value = self.evaluate(object.convert_to_statement());
        let object_type = object_value.type_name();

        let mut object_variable = Variable::new(
            "_".to_string(),
            object_value.clone(),
            object_type.clone(),
            false,
            true,
            true,
        );
        
        if let Value::Object(ref o) = object_value {
            if let Some(props) = o.get_properties() {
                object_variable.properties = props.clone();
            }
        } else if !object_variable.is_init() {
            object_variable.init_properties();
        }

        if self.err.is_some() {
            return Value::Null;
        }

        return self.call_method(
            &object_variable,
            method_name,
            pos_args,
            named_args,
        );
    }

    fn call_method(
        &mut self,
        object_variable: &Variable,
        method_name: &str,
        pos_args: Vec<Value>,
        named_args: HashMap<String, Value>,
    ) -> Value {
        let var = match object_variable.properties.get(method_name) {
            Some(v) => v.clone(),
            None => {
                let available_names: Vec<String> = object_variable.properties.keys().cloned().collect();
                if let Some(closest) = find_closest_match(method_name, &available_names) {
                    return self.raise_with_help(
                        "NameError",
                        &format!("No method '{}' in '{}'", method_name, object_variable.type_name()),
                        &format!("Did you mean '{}{}{}'?",
                            check_ansi("\x1b[4m", &self.use_colors),
                            closest,
                            check_ansi("\x1b[24m", &self.use_colors),
                        ),
                    );
                } else {
                    return self.raise("NameError", &format!("No method '{}' in '{}'", method_name, object_variable.type_name()));
                }
            }
        };
    
        let value = var.get_value();
    
        match value {
            Value::Function(func) => {
                if let Some(state) = func.metadata().state.as_ref() {
                    if state == "deprecated" {
                        println!("Warning: Method '{}' is deprecated", method_name);
                    } else if let Some(alt_name) = state.strip_prefix("renamed_to: ") {
                        return self.raise_with_help(
                            "NameError",
                            &format!(
                                "Method '{}' has been renamed to '{}'.",
                                method_name, alt_name
                            ),
                            &format!(
                                "Try using: '{}{}{}'",
                                check_ansi("\x1b[4m", &self.use_colors),
                                alt_name,
                                check_ansi("\x1b[24m", &self.use_colors)
                            ),
                        );
                    } else if let Some(alt_info) = state.strip_prefix("removed_in_with_alt: ") {
                        let mut parts = alt_info.splitn(2, ',').map(str::trim);
                        let version = parts.next().unwrap_or("unknown");
                        let alt_name = parts.next().unwrap_or("an alternative");
                    
                        return self.raise_with_help(
                            "NameError",
                            &format!(
                                "Method '{}' has been removed in version {}.",
                                method_name, version
                            ),
                            &format!(
                                "Use '{}{}{}' instead.",
                                check_ansi("\x1b[4m", &self.use_colors),
                                alt_name,
                                check_ansi("\x1b[24m", &self.use_colors)
                            ),
                        );                
                    } else if let Some(alt_name) = state.strip_prefix("removed_in: ") {
                        return self.raise(
                            "NameError",
                            &format!(
                                "Method '{}' has been removed in version {}.",
                                method_name, alt_name
                            ),
                        );
                    } else if let Some(alt_name) = state.strip_prefix("removed: ") {
                        return self.raise_with_help(
                            "NameError",
                            &format!(
                                "Method '{}' has been removed.",
                                method_name
                            ),
                            &format!(
                                "Use '{}{}{}' instead.",
                                check_ansi("\x1b[4m", &self.use_colors), alt_name, check_ansi("\x1b[24m", &self.use_colors)
                            ),
                        );
                    }
                }

                let metadata = func.metadata();
    
                let positional: Vec<Value> = pos_args.clone();
                let mut named_map: HashMap<String, Value> = named_args.clone();
                let mut final_args: HashMap<String, Value> = HashMap::new();
    
                let passed_args_count = positional.len() + named_map.len();
                let expected_args_count = metadata.parameters.len();
                
                if expected_args_count == 0 && passed_args_count > 0 {
                    return self.raise(
                        "TypeError",
                        &format!(
                            "Function '{}' expects no arguments, but got {}.",
                            method_name,
                            passed_args_count
                        ),
                    );
                }

                let has_variadic = metadata.parameters.iter().any(|p| p.kind == ParameterKind::Variadic);
                
                if !has_variadic && passed_args_count > expected_args_count {
                    return self.raise(
                        "TypeError",
                        &format!(
                            "Too many arguments for function '{}'. Expected at most {}, but got {}.",
                            method_name,
                            expected_args_count,
                            passed_args_count
                        ),
                    );
                }

                let mut pos_index = 0;
                let required_positional_count = metadata.parameters
                .iter()
                .filter(|p| p.kind == ParameterKind::Positional && p.default.is_none())
                .count();
            
                let provided_positional_count = positional.len();
                
                let mut matched_positional_count = 0;
                for param in &metadata.parameters {
                    if param.kind == ParameterKind::Positional && param.default.is_none() {
                        if pos_index < positional.len() || named_map.contains_key(&param.name) {
                            matched_positional_count += 1;
                        }
                    }
                }
                
                if matched_positional_count < required_positional_count {
                    return self.raise(
                        "TypeError",
                        &format!(
                            "Missing required positional argument{} for function '{}'. Expected at least {}, but got {}.",
                            if required_positional_count == 1 { "" } else { "s" },
                            method_name,
                            required_positional_count,
                            matched_positional_count
                        ),
                    );
                }                     
    
                for param in &metadata.parameters {
                    let param_name = &param.name;
                    let param_type = &param.ty;
                    let param_default = &param.default;

                    match param.kind {
                        ParameterKind::Positional => {
                            if pos_index < positional.len() {
                                let arg_value = positional[pos_index].clone();
                                if self.check_type(&arg_value, param_type, false) {
                                    final_args.insert(param_name.clone(), arg_value);
                                } else {
                                    return self.raise_with_help(
                                        "TypeError",
                                        &format!("Argument '{}' does not match expected type '{}', got '{}'", param_name, format_type(param_type), arg_value.type_name()),
                                        &format!(
                                            "Try using: '{}{}={}({}){}'",
                                            check_ansi("\x1b[4m", &self.use_colors),
                                            param_name,
                                            format_type(param_type),
                                            format_value(&positional[pos_index]).to_string(),
                                            check_ansi("\x1b[24m", &self.use_colors)
                                        ),
                                    );
                                }
                                pos_index += 1;
                            } else if let Some(named_value) = named_map.remove(param_name) {
                                if self.check_type(&named_value, param_type, false) {
                                    final_args.insert(param_name.clone(), named_value);
                                } else {
                                    return self.raise_with_help(
                                        "TypeError",
                                        &format!("Argument '{}' does not match expected type '{}', got '{}'", param_name, format_type(param_type), named_value.type_name()),
                                        &format!(
                                            "Try using: '{}{}={}({}){}'",
                                            check_ansi("\x1b[4m", &self.use_colors),
                                            param_name,
                                            format_type(param_type),
                                            check_ansi("\x1b[24m", &self.use_colors),
                                            named_value.to_string()
                                        ),
                                    );
                                }
                            } else if let Some(default) = param_default {
                                final_args.insert(param_name.clone(), default.clone());
                            } else {
                                return self.raise("TypeError", &format!("Missing required positional argument: '{}'", param_name));
                            }
                        }                        
                        ParameterKind::Variadic => {
                            let mut variadic_args = positional[pos_index..].to_vec();

                            for (i, arg) in variadic_args.iter().enumerate() {
                                if !self.check_type(&arg, param_type, false) {
                                    return self.raise(
                                        "TypeError",
                                        &format!("Variadic argument #{} does not match expected type '{}'", i, format_type(param_type)),
                                    );
                                }
                            }
                    
                            final_args.insert(param_name.clone(), Value::List(variadic_args));
                    
                            pos_index = positional.len();
                            named_map.remove(param_name);
                        }
                        ParameterKind::KeywordVariadic => {
                            if let Some(named_value) = named_map.remove(param_name) {
                                if self.check_type(&named_value, &param_type, false) {
                                    final_args.insert(param_name.clone(), named_value);
                                } else {
                                    return self.raise(
                                        "TypeError",
                                        &format!(
                                            "Keyword argument '{}' does not match expected type '{}', got '{}'",
                                            param_name,
                                            format_type(param_type),
                                            named_value.type_name()
                                        ),
                                    );
                                }
                            } else if let Some(default) = param_default {
                                final_args.insert(param_name.clone(), default.clone());
                            }
                        }
                    }
                }

                if !named_map.is_empty() {
                    let mut expect_one_of = String::new();
                    let params_count = metadata.parameters.len();
                    
                    if params_count > 4 {
                        expect_one_of.clear();
                    } else {
                        expect_one_of.push_str(" Expected one of: ");
                        
                        for (i, param) in metadata.parameters.iter().enumerate() {
                            expect_one_of.push_str(&format!("{} ({})", param.name, param.ty));
                            if i != params_count - 1 {
                                expect_one_of.push_str(", ");
                            }
                        }
                    }
                    
                    if named_map.len() == 1 {
                        let (key, value) = named_map.into_iter().next().unwrap();
                        return self.raise(
                            "TypeError",
                            &format!("Unexpected keyword argument '{}'.{}", key, expect_one_of),
                        );
                    } else {
                        return self.raise(
                            "TypeError",
                            &format!("Unexpected keyword arguments: {}.{}", named_map.keys().cloned().collect::<Vec<String>>().join(", "), expect_one_of),
                        );
                    }
                }

                debug_log(
                    &format!(
                        "<Call: {}({})>",
                        method_name,
                        final_args
                            .iter()
                            .map(|(k, v)| format!("{}: {}", k, format_value(v)))
                            .collect::<Vec<String>>()
                            .join(", ")
                    ),
                    &self.config,
                    Some(self.use_colors.clone()),
                );
    
                self.stack.push((method_name.to_string(), final_args.clone(), self.variables.clone()));

                let mut result = NULL;

                let final_args_variables = final_args
                    .iter()
                    .map(|(k, v)| {
                        let name = k.clone();
                        let variable = Variable::new(name.clone(), v.clone(), "any".to_string(), false, true, true);
                        (name, variable)
                    })
                    .collect::<HashMap<String, Variable<>>>();            
            
                if !metadata.is_native {
                    let mut new_interpreter = Interpreter::new(
                        self.config.clone(),
                        self.use_colors.clone(),
                        &self.file_path.clone(),
                        &self.cwd.clone(),
                        self.preprocessor_info.clone(),
                    );
                    let mut merged_variables = self.variables.clone();
                    merged_variables.extend(final_args_variables);
                    new_interpreter.variables = merged_variables;
                    new_interpreter.stack = self.stack.clone();
                    let body = func.get_body();
                    new_interpreter.interpret(body, self.source.clone());
                    if new_interpreter.err.is_some() {
                        self.err = new_interpreter.err.clone();
                        return NULL;
                    }
                    result = new_interpreter.return_value.clone();
                    self.stack = new_interpreter.stack;
                } else {
                    result = func.call(&final_args);
                }
                self.stack.pop();
                debug_log(
                    &format!("<Function '{}' returned: {}>", method_name, format_value(&result)),
                    &self.config,
                    Some(self.use_colors.clone()),
                );
                if let Value::Error(err_type, err_msg) = &result {
                    return self.raise(err_type, err_msg);
                }
                if !self.check_type(&result, &metadata.return_type, false) {
                    return self.raise(
                        "TypeError",
                        &format!("Return value does not match expected type '{}', got '{}'", format_type(&metadata.return_type), result.type_name())
                    );
                }
                return result;
            }
            other => {
                self.raise_with_help(
                    "TypeError",
                    &format!("'{}' is not callable", method_name),
                    &format!("Expected a method, but got: {}", other.to_string()),
                )
            }
        }
    }

    fn handle_property_access(&mut self, statement: HashMap<Value, Value>) -> Value {
        let Some(object) = statement.get(&Value::String("object".to_string())).cloned() else {
            return Value::Error("RuntimeError", "Missing 'object' in property access");
        };
    
        let Some(Value::String(property)) = statement.get(&Value::String("property".to_string())).cloned() else {
            return Value::Error("RuntimeError", "Missing or invalid 'property' in property access");
        };
    
        let property_name = property.as_str();
        let object_value = self.evaluate(object.convert_to_statement());
        let object_type = object_value.type_name();

        let mut object_variable = Variable::new(
            "_".to_string(),
            object_value.clone(),
            object_type.clone(),
            false,
            true,
            true,
        );
        
        if let Value::Object(ref o) = object_value {
            if let Some(props) = o.get_properties() {
                object_variable.properties = props.clone();
            }
        } else if !object_variable.is_init() {
            object_variable.init_properties();
        }

        if self.err.is_some() {
            return Value::Null;
        }
        
        let var = match object_variable.properties.get(property_name) {
            Some(v) => v.clone(),
            None => {
                let available_names: Vec<String> = object_variable.properties.keys().cloned().collect();
                if let Some(closest) = find_closest_match(property_name, &available_names) {
                    return self.raise_with_help(
                        "NameError",
                        &format!("No property '{}' in '{}'", property_name, object_variable.type_name()),
                        &format!("Did you mean '{}{}{}'?",
                            check_ansi("\x1b[4m", &self.use_colors),
                            closest,
                            check_ansi("\x1b[24m", &self.use_colors),
                        ),
                    );
                } else {
                    return self.raise("NameError", &format!("No property '{}' in '{}'", property_name, object_variable.type_name()));
                }
            }
        };
        var.get_value().clone()
    }

    fn translate_args(
        &mut self,
        pos_args: Value,
        named_args: Value,
    ) -> Result<(Vec<Value>, HashMap<String, Value>), Value> {
        let pos_args = match pos_args {
            Value::List(args) => {
                args.iter()
                    .map(|stmt| self.evaluate(stmt.clone().convert_to_statement()))
                    .collect::<Vec<Value>>()
            }
            _ => return Err(self.raise("TypeError", "Expected a list for function arguments")),
        };
    
        let named_args = match named_args {
            Value::Map { keys, values } => {
                let mut map = HashMap::new();
                for (key, val_stmt) in keys.iter().zip(values.iter()) {
                    if let Value::String(key_str) = key {
                        let val = self.evaluate(val_stmt.clone().convert_to_statement());
                        map.insert(key_str.clone(), val);
                    } else {
                        return Err(self.raise("TypeError", "Expected string keys for named arguments"));
                    }
                }
                map
            }
            _ => return Err(self.raise("TypeError", "Expected a map for named arguments")),
        };
    
        Ok((pos_args, named_args))
    }
    
    fn handle_call(&mut self, statement: HashMap<Value, Value>) -> Value {
        let function_name = match statement.get(&Value::String("name".to_string())) {
            Some(Value::String(s)) => s.as_str(),
            _ => return self.raise("TypeError", "Expected a string for function name"),
        };

        let pos_args = match statement.get(&Value::String("pos_arguments".to_string())) {
            Some(Value::List(args)) => {
                args.iter()
                    .map(|stmt| self.evaluate(stmt.clone().convert_to_statement()))
                    .collect::<Vec<Value>>()
            }
            _ => return self.raise("TypeError", "Expected a list for function arguments"),
        };

        let named_args = match statement.get(&Value::String("named_arguments".to_string())) {
            Some(Value::Map { keys, values }) => {
                let mut map = HashMap::new();
                for (key, val_stmt) in keys.iter().zip(values.iter()) {
                    if let Value::String(key_str) = key {
                        let val = self.evaluate(val_stmt.clone().convert_to_statement());
                        map.insert(key_str.clone(), val);
                    } else {
                        return self.raise("TypeError", "Expected string keys for named arguments");
                    }
                }
                map
            }
            _ => return self.raise("TypeError", "Expected a map for named arguments"),
        };
        self.call_function(function_name, pos_args, named_args)
    }

    fn call_function(
        &mut self,
        function_name: &str,
        pos_args: Vec<Value>,
        named_args: HashMap<String, Value>,
    ) -> Value {
        let special_functions = [
            "exit", "fetch"
        ];
        let special_functions_meta = special_function_meta();
        let is_special_function = special_functions.contains(&function_name);

        let mut value = Value::Null;

        if !is_special_function {
            value = match self.variables.get(function_name) {
                Some(v) => v.get_value().clone(),
                None => {
                    let available_names: Vec<String> = self.variables.keys().cloned().collect();
                    if let Some(closest) = find_closest_match(function_name, &available_names) {
                        return self.raise_with_help(
                            "NameError",
                            &format!("Function '{}' is not defined", function_name),
                            &format!("Did you mean '{}{}{}'?",
                                check_ansi("\x1b[4m", &self.use_colors),
                                closest,
                                check_ansi("\x1b[24m", &self.use_colors),
                            ),
                        );
                    } else {
                        return self.raise("NameError", &format!("Function '{}' is not defined", function_name));
                    }
                }
            };
        } else {
            value = match self.variables.get("00__placeholder__") {
                Some(v) => v.get_value().clone(),
                None => {
                    return self.raise(
                        "RuntimeError",
                        &format!("'00__placeholder__' is not defined."),
                    );
                }
            };
        }
    
        match value {
            Value::Function(func) => {
                let mut metadata = &special_functions_meta
                    .get(function_name)
                    .cloned()
                    .unwrap_or(func.metadata().clone());

                if let Some(state) = metadata.state.as_ref() {
                        if state == "deprecated" {
                            println!("Warning: Function '{}' is deprecated", function_name);
                        } else if let Some(alt_name) = state.strip_prefix("renamed_to: ") {
                            return self.raise_with_help(
                                "NameError",
                                &format!(
                                    "Function '{}' has been renamed to '{}'.",
                                    function_name, alt_name
                                ),
                                &format!(
                                    "Try using: '{}{}{}'",
                                    check_ansi("\x1b[4m", &self.use_colors),
                                    alt_name,
                                    check_ansi("\x1b[24m", &self.use_colors)
                                ),
                            );
                        } else if let Some(alt_info) = state.strip_prefix("removed_in_with_alt: ") {
                            let mut parts = alt_info.splitn(2, ',').map(str::trim);
                            let version = parts.next().unwrap_or("unknown");
                            let alt_name = parts.next().unwrap_or("an alternative");
                        
                            return self.raise_with_help(
                                "NameError",
                                &format!(
                                    "Function '{}' has been removed in version {}.",
                                    function_name, version
                                ),
                                &format!(
                                    "Use '{}{}{}' instead.",
                                    check_ansi("\x1b[4m", &self.use_colors),
                                    alt_name,
                                    check_ansi("\x1b[24m", &self.use_colors)
                                ),
                            );                
                        } else if let Some(alt_name) = state.strip_prefix("removed_in: ") {
                            return self.raise(
                                "NameError",
                                &format!(
                                    "Function '{}' has been removed in version {}.",
                                    function_name, alt_name
                                ),
                            );
                        } else if let Some(alt_name) = state.strip_prefix("removed: ") {
                            return self.raise_with_help(
                                "NameError",
                                &format!(
                                    "Function '{}' has been removed.",
                                    function_name
                                ),
                                &format!(
                                    "Use '{}{}{}' instead.",
                                    check_ansi("\x1b[4m", &self.use_colors), alt_name, check_ansi("\x1b[24m", &self.use_colors)
                                ),
                            );
                        }
                }
                
                let positional: Vec<Value> = pos_args.clone();
                let mut named_map: HashMap<String, Value> = named_args.clone();
                let mut final_args: HashMap<String, Value> = HashMap::new();
    
                let passed_args_count = positional.len() + named_map.len();
                let expected_args_count = metadata.parameters.len();
                
                if expected_args_count == 0 && passed_args_count > 0 {
                    return self.raise(
                        "TypeError",
                        &format!(
                            "Function '{}' expects no arguments, but got {}.",
                            function_name,
                            passed_args_count
                        ),
                    );
                }

                let has_variadic = metadata.parameters.iter().any(|p| p.kind == ParameterKind::Variadic);
                
                if !has_variadic && passed_args_count > expected_args_count {
                    return self.raise(
                        "TypeError",
                        &format!(
                            "Too many arguments for function '{}'. Expected at most {}, but got {}.",
                            function_name,
                            expected_args_count,
                            passed_args_count
                        ),
                    );
                }

                let mut pos_index = 0;
                let required_positional_count = metadata.parameters
                .iter()
                .filter(|p| p.kind == ParameterKind::Positional && p.default.is_none())
                .count();
            
                let provided_positional_count = positional.len();
                
                let mut matched_positional_count = 0;
                for param in &metadata.parameters {
                    if param.kind == ParameterKind::Positional && param.default.is_none() {
                        if pos_index < positional.len() || named_map.contains_key(&param.name) {
                            matched_positional_count += 1;
                        }
                    }
                }
                
                if matched_positional_count < required_positional_count {
                    return self.raise(
                        "TypeError",
                        &format!(
                            "Missing required positional argument{} for function '{}'. Expected at least {}, but got {}.",
                            if required_positional_count == 1 { "" } else { "s" },
                            function_name,
                            required_positional_count,
                            matched_positional_count
                        ),
                    );
                }                     
    
                for param in &metadata.parameters {
                    let param_name = &param.name;
                    let param_type = &param.ty;
                    let param_default = &param.default;

                    match param.kind {
                        ParameterKind::Positional => {
                            if pos_index < positional.len() {
                                let arg_value = positional[pos_index].clone();
                                if self.check_type(&arg_value, param_type, false) {
                                    final_args.insert(param_name.clone(), arg_value);
                                } else {
                                    return self.raise_with_help(
                                        "TypeError",
                                        &format!("Argument '{}' does not match expected type '{}', got '{}'", param_name, format_type(param_type), arg_value.type_name()),
                                        &format!(
                                            "Try using: '{}{}={}({}){}'",
                                            check_ansi("\x1b[4m", &self.use_colors),
                                            param_name,
                                            format_type(param_type),
                                            format_value(&positional[pos_index]).to_string(),
                                            check_ansi("\x1b[24m", &self.use_colors)
                                        ),
                                    );
                                }
                                pos_index += 1;
                            } else if let Some(named_value) = named_map.remove(param_name) {
                                if self.check_type(&named_value, param_type, false) {
                                    final_args.insert(param_name.clone(), named_value);
                                } else {
                                    return self.raise_with_help(
                                        "TypeError",
                                        &format!("Argument '{}' does not match expected type '{}', got '{}'", param_name, format_type(param_type), named_value.type_name()),
                                        &format!(
                                            "Try using: '{}{}={}({}){}'",
                                            check_ansi("\x1b[4m", &self.use_colors),
                                            param_name,
                                            format_type(param_type),
                                            check_ansi("\x1b[24m", &self.use_colors),
                                            named_value.to_string()
                                        ),
                                    );
                                }
                            } else if let Some(default) = param_default {
                                final_args.insert(param_name.clone(), default.clone());
                            } else {
                                return self.raise("TypeError", &format!("Missing required positional argument: '{}'", param_name));
                            }
                        }                        
                        ParameterKind::Variadic => {
                            let mut variadic_args = positional[pos_index..].to_vec();

                            for (i, arg) in variadic_args.iter().enumerate() {
                                if !self.check_type(&arg, param_type, false) {
                                    return self.raise(
                                        "TypeError",
                                        &format!("Variadic argument #{} does not match expected type '{}'", i, format_type(param_type)),
                                    );
                                }
                            }
                    
                            final_args.insert(param_name.clone(), Value::List(variadic_args));
                    
                            pos_index = positional.len();
                            named_map.remove(param_name);
                        }
                        ParameterKind::KeywordVariadic => {
                            if let Some(named_value) = named_map.remove(param_name) {
                                if self.check_type(&named_value, &param_type, false) {
                                    final_args.insert(param_name.clone(), named_value);
                                } else {
                                    return self.raise(
                                        "TypeError",
                                        &format!(
                                            "Keyword argument '{}' does not match expected type '{}', got '{}'",
                                            param_name,
                                            format_type(param_type),
                                            named_value.type_name()
                                        ),
                                    );
                                }
                            } else if let Some(default) = param_default {
                                final_args.insert(param_name.clone(), default.clone());
                            }
                        }
                    }
                }

                if !named_map.is_empty() {
                    let mut expect_one_of = String::new();
                    let params_count = metadata.parameters.len();
                    
                    if params_count > 4 {
                        expect_one_of.clear();
                    } else {
                        expect_one_of.push_str(" Expected one of: ");
                        
                        for (i, param) in metadata.parameters.iter().enumerate() {
                            expect_one_of.push_str(&format!("{} ({})", param.name, param.ty));
                            if i != params_count - 1 {
                                expect_one_of.push_str(", ");
                            }
                        }
                    }
                    
                    if named_map.len() == 1 {
                        let (key, value) = named_map.into_iter().next().unwrap();
                        return self.raise(
                            "TypeError",
                            &format!("Unexpected keyword argument '{}'.{}", key, expect_one_of),
                        );
                    } else {
                        return self.raise(
                            "TypeError",
                            &format!("Unexpected keyword arguments: {}.{}", named_map.keys().cloned().collect::<Vec<String>>().join(", "), expect_one_of),
                        );
                    }
                }

                debug_log(
                    &format!(
                        "<Call: {}({})>",
                        function_name,
                        final_args
                            .iter()
                            .map(|(k, v)| format!("{}: {}", k, format_value(v)))
                            .collect::<Vec<String>>()
                            .join(", ")
                    ),
                    &self.config,
                    Some(self.use_colors.clone()),
                );
    
                self.stack.push((function_name.to_string(), final_args.clone(), self.variables.clone()));

                let mut result = NULL;

                let final_args_variables = final_args
                    .iter()
                    .map(|(k, v)| {
                        let name = k.clone();
                        let variable = Variable::new(name.clone(), v.clone(), "any".to_string(), false, true, true);
                        (name, variable)
                    })
                    .collect::<HashMap<String, Variable<>>>();
                    
                if is_special_function {
                    match function_name {
                        "exit" => {
                            let code = if let Some(code) = named_args.get("code") {
                                code.clone()
                            } else if !pos_args.is_empty() {
                                pos_args[0].clone()
                            } else {
                                Value::Int(Int::from_i64(0))
                            };
                            self.is_returning = true;
                            self.is_stopped = true;
                            self.return_value = code.clone();
                            return code;
                        }
                        "fetch" => {
                            return self.fetch_fn(&final_args)
                        }
                        _ => {}
                    }
                }
            
                if !metadata.is_native {
                    let mut new_interpreter = Interpreter::new(
                        self.config.clone(),
                        self.use_colors.clone(),
                        &self.file_path.clone(),
                        &self.cwd.clone(),
                        self.preprocessor_info.clone(),
                    );
                    let mut merged_variables = self.variables.clone();
                    merged_variables.extend(final_args_variables);
                    new_interpreter.variables = merged_variables;
                    new_interpreter.stack = self.stack.clone();
                    let body = func.get_body();
                    new_interpreter.interpret(body, self.source.clone());
                    if new_interpreter.err.is_some() {
                        self.err = new_interpreter.err.clone();
                        return NULL;
                    }
                    result = new_interpreter.return_value.clone();
                    self.stack = new_interpreter.stack;
                } else {
                    result = func.call(&final_args);
                }
                self.stack.pop();
                debug_log(
                    &format!("<Function '{}' returned: {}>", function_name, format_value(&result)),
                    &self.config,
                    Some(self.use_colors.clone()),
                );
                if let Value::Error(err_type, err_msg) = &result {
                    return self.raise(err_type, err_msg);
                }
                if !self.check_type(&result, &metadata.return_type, false) {
                    return self.raise(
                        "TypeError",
                        &format!("Return value does not match expected type '{}', got '{}'", format_type(&metadata.return_type), result.type_name())
                    );
                }
                return result
            }
            other => {
                self.raise_with_help(
                    "TypeError",
                    &format!("'{}' is not callable", function_name),
                    &format!("Expected a function, but got: {}", other.to_string()),
                )
            }
        }
    }
    
    fn handle_operation(&mut self, statement: HashMap<Value, Value>) -> Value {
        let left = match statement.get(&Value::String("left".to_string())) {
            Some(val_left) => self.evaluate(val_left.convert_to_statement()),
            None => {
                self.raise("KeyError", "Missing 'left' key in the statement.");
                return NULL;
            }
        };
    
        if self.err.is_some() {
            return NULL;
        }
    
        let right = match statement.get(&Value::String("right".to_string())) {
            Some(val_right) => self.evaluate(val_right.convert_to_statement()),
            None => {
                self.raise("KeyError", "Missing 'right' key in the statement.");
                return NULL;
            }
        };
    
        let operator = match statement.get(&Value::String("operator".to_string())) {
            Some(Value::String(s)) => s,
            _ => return self.raise("TypeError", "Expected a string for operator"),
        };
    
        // Convert Boolean to Float
        let left = if let Value::Boolean(b) = left {
            Value::Float(if b { 1.0.into() } else { 0.0.into() })
        } else {
            left
        };
        let right = if let Value::Boolean(b) = right {
            Value::Float(if b { 1.0.into() } else { 0.0.into() })
        } else {
            right
        };
    
        self.make_operation(left, right, operator)
    }
    
    fn handle_unary_op(&mut self, statement: HashMap<Value, Value>) -> Value {
        let operand = match statement.get(&Value::String("operand".to_string())) {
            Some(val) => self.evaluate(val.convert_to_statement()),
            None => {
                self.raise("KeyError", "Missing 'operand' key in the statement.");
                return NULL;
            }
        };
    
        let operator = match statement.get(&Value::String("operator".to_string())) {
            Some(Value::String(s)) => s,
            _ => return self.raise("TypeError", "Expected a string for operator"),
        };
    
        debug_log(
            &format!("<UnaryOperation: {}{}>", operator, format_value(&operand)),
            &self.config,
            Some(self.use_colors.clone()),
        );
    
        match operator.as_str() {
            "-" => match operand {
                Value::Int(n) => Value::Int(-n),
                Value::Float(f) => Value::Float(-f),
                _ => self.raise("TypeError", &format!("Cannot negate {}", operand.type_name())),
            },
            "+" => match operand {
                Value::Int(n) => Value::Int(n.abs()),
                Value::Float(f) => Value::Float(f.abs()),
                _ => self.raise("TypeError", &format!("Cannot apply unary plus to {}", operand.type_name())),
            },
            "!" => Value::Boolean(!operand.is_truthy()),
            _ => self.raise("SyntaxError", &format!("Unexpected unary operator: '{}'", operator)),
        }
    }
    
    fn make_operation(&mut self, left: Value, right: Value, mut operator: &str) -> Value {
        operator = match operator {
            "isnt" | "isn't" | "nein" => "!=",
            "or" => "||",
            "and" => "&&",
            "is" => "==",
            "not" => "!",
            other => other,
        };
    
        // Division/modulo zero check
        if (operator == "/" || operator == "%") && right.is_zero() {
            return self.raise("ZeroDivisionError", format!("{} by zero.", if operator == "/" { "Division" } else { "Modulo" }).as_str());
        }
    
        if left.is_nan() || right.is_nan() {
            return self.raise("MathError", "Operation with NaN is not allowed");
        }
    
        if left.is_infinity() || right.is_infinity() {
            return self.raise("MathError", "Operation with Infinity is not allowed");
        }
    
        if operator == "abs" {
            debug_log(
                &format!("<Operation: |{}|>", format_value(&right)),
                &self.config,
                Some(self.use_colors.clone()),
            );
        } else {
            debug_log(
                &format!("<Operation: {} {} {}>", format_value(&left), operator, format_value(&right)),
                &self.config,
                Some(self.use_colors.clone()),
            );
        }
    
        match operator {
            "+" => match (left, right) {
                (Value::Int(a), Value::Int(b)) => match a + b {
                    Ok(res) => Value::Int(res),
                    Err(_) => return self.raise("TypeError", "Int addition overflow"),
                },
                (Value::Float(a), Value::Float(b)) => match a + b {
                    Ok(res) => Value::Float(res),
                    Err(_) => return self.raise("TypeError", "Float addition failed"),
                },
                (Value::Int(a), Value::Float(b)) => {
                    let fa = match Float::from_int(&a) {
                        Ok(f) => f,
                        Err(_) => return self.raise("TypeError", "Failed to convert Int to Float"),
                    };
                    match fa + b {
                        Ok(res) => Value::Float(res),
                        Err(_) => return self.raise("TypeError", "Float addition failed"),
                    }
                }
                (Value::Float(a), Value::Int(b)) => {
                    let fb = match Float::from_int(&b) {
                        Ok(f) => f,
                        Err(_) => return self.raise("TypeError", "Failed to convert Int to Float"),
                    };
                    match a + fb {
                        Ok(res) => Value::Float(res),
                        Err(_) => return self.raise("TypeError", "Float addition failed"),
                    }
                }
                (Value::String(a), Value::String(b)) => Value::String(a + &b),
                (Value::String(a), b) => Value::String(a + &b.to_string()),
                (a, Value::String(b)) => Value::String(a.to_string() + &b),
                (a, b) => {
                    return self.raise("TypeError", &format!("Cannot add {} and {}", a.type_name(), b.type_name()));
                }
                
            },
            "-" => match (left, right) {
                (Value::Int(a), Value::Int(b)) => match a - b {
                    Ok(res) => Value::Int(res),
                    Err(_) => return self.raise("TypeError", "Int subtraction overflow"),
                },
                (Value::Float(a), Value::Float(b)) => match a - b {
                    Ok(res) => Value::Float(res),
                    Err(_) => return self.raise("TypeError", "Float subtraction failed"),
                },
                (Value::Int(a), Value::Float(b)) => {
                    let fa = match Float::from_int(&a) {
                        Ok(f) => f,
                        Err(_) => return self.raise("TypeError", "Failed to convert Int to Float"),
                    };
                    match fa - b {
                        Ok(res) => Value::Float(res),
                        Err(_) => return self.raise("TypeError", "Float subtraction failed"),
                    }
                }
                (Value::Float(a), Value::Int(b)) => {
                    let fb = match Float::from_int(&b) {
                        Ok(f) => f,
                        Err(_) => return self.raise("TypeError", "Failed to convert Int to Float"),
                    };
                    match a - fb {
                        Ok(res) => Value::Float(res),
                        Err(_) => return self.raise("TypeError", "Float subtraction failed"),
                    }
                }
                (a, b) => return self.raise("TypeError", &format!("Cannot subtract {} and {}", a.type_name(), b.type_name())),
            },
            "*" => match (left, right) {
                (Value::Int(a), Value::Int(b)) => {
                    match a * b {
                        Ok(res) => Value::Int(res),
                        Err(_) => return self.raise("TypeError", "Int multiplication failed"),
                    }
                }
                (Value::Float(a), Value::Float(b)) => match a * b {
                    Ok(res) => Value::Float(res),
                    Err(_) => return self.raise("TypeError", "Float multiplication failed"),
                },
                (Value::Int(a), Value::Float(b)) => {
                    let fa = match Float::from_int(&a) {
                        Ok(f) => f,
                        Err(_) => return self.raise("TypeError", "Failed to convert Int to Float"),
                    };
                    match fa * b {
                        Ok(res) => Value::Float(res),
                        Err(_) => return self.raise("TypeError", "Float multiplication failed"),
                    }
                }
                (Value::Float(a), Value::Int(b)) => {
                    let fb = match Float::from_int(&b) {
                        Ok(f) => f,
                        Err(_) => return self.raise("TypeError", "Failed to convert Int to Float"),
                    };
                    match a * fb {
                        Ok(res) => Value::Float(res),
                        Err(_) => return self.raise("TypeError", "Float multiplication failed"),
                    }
                }
                (Value::String(s), Value::Int(i)) => match i.to_usize() {
                    Ok(times) => Value::String(s.repeat(times)),
                    Err(_) => return self.raise("TypeError", "Invalid repeat count in string multiplication"),
                },
                (a, b) => return self.raise("TypeError", &format!("Cannot multiply {} and {}", a.type_name(), b.type_name())),
            },
            "/" => match right {
                Value::Int(ref val) if val.is_zero() => self.raise("ZeroDivisionError", "Division by zero."),
                Value::Float(ref f) if f.is_zero() => self.raise("ZeroDivisionError", "Division by zero."),
                _ => {
                    let base = match left {
                        Value::Int(ref i) => match Float::from_int(i) {
                            Ok(f) => f,
                            Err(_) => return self.raise("TypeError", "Failed to convert Int to Float"),
                        },
                        Value::Float(ref f) => f.clone(),
                        _ => return self.raise("TypeError", &format!("Cannot divide {} by {}", left.type_name(), right.type_name())),
                    };
                    let divisor = match right {
                        Value::Int(ref i) => match Float::from_int(i) {
                            Ok(f) => f,
                            Err(_) => return self.raise("TypeError", "Failed to convert Int to Float"),
                        },
                        Value::Float(ref f) => f.clone(),
                        _ => return self.raise("TypeError", &format!("Cannot divide {} by {}", left.type_name(), right.type_name())),
                    };
                    match base / divisor {
                        Ok(result) => Value::Float(result),
                        Err(_) => self.raise("TypeError", "Failed to perform division"),
                    }
                }
            },
            "%" => match (left, right) {
                (_, Value::Int(ref val)) if val.is_zero() => self.raise("ZeroDivisionError", "Modulo by zero."),
                (_, Value::Float(ref f)) if f.is_zero() => self.raise("ZeroDivisionError", "Modulo by zero."),
                (a, b) => {
                    let base = match a {
                        Value::Int(ref i) => match Float::from_int(&i) {
                            Ok(f) => f,
                            Err(_) => return self.raise("TypeError", "Failed to convert Int to Float"),
                        },
                        Value::Float(ref f) => f.clone(),
                        _ => return self.raise("TypeError", &format!("Cannot modulo {} and {}", a.type_name(), b.type_name())),
                    };
                    let divisor = match b {
                        Value::Int(i) => match Float::from_int(&i) {
                            Ok(f) => f,
                            Err(_) => return self.raise("TypeError", "Failed to convert Int to Float"),
                        },
                        Value::Float(f) => f.clone(),
                        _ => return self.raise("TypeError", &format!("Cannot modulo {} and {}", a.type_name(), b.type_name())),
                    };
                    match base % divisor {
                        Ok(res) => Value::Float(res),
                        Err(_) => self.raise("TypeError", "Float modulo failed"),
                    }
                }
            },
            "^" => match (&left, &right) {
                (_, Value::Int(b)) if b.is_zero() => Value::Float(1.0.into()),
                (_, Value::Float(b)) if b.is_zero() => Value::Float(1.0.into()),
                (Value::Int(a), Value::Int(b)) => {
                    match a.pow(b) {  // directly pass &Int here
                        Ok(res) => Value::Int(res),
                        Err(_) => return self.raise("TypeError", "Int pow failed"),
                    }
                }
                (Value::Float(a), Value::Float(b)) => match a.pow(b) {
                    Ok(res) => Value::Float(res),
                    Err(_) => return self.raise("TypeError", "Float pow failed"),
                },
                (Value::Int(a), Value::Float(b)) => {
                    let base = match Float::from_int(a) {
                        Ok(f) => f,
                        Err(_) => return self.raise("TypeError", "Failed to convert Int to Float"),
                    };
                    match base.pow(b) {
                        Ok(res) => Value::Float(res),
                        Err(_) => return self.raise("TypeError", "Float pow failed"),
                    }
                }
                (Value::Float(a), Value::Int(b)) => {
                    let exp = match Float::from_int(b) {
                        Ok(f) => f,
                        Err(_) => return self.raise("TypeError", "Failed to convert Int to Float"),
                    };
                    match a.pow(&exp) {
                        Ok(res) => Value::Float(res),
                        Err(_) => return self.raise("TypeError", "Float pow failed"),
                    }
                }
                (a, b) => self.raise("TypeError", &format!(
                    "Operator '^' requires numeric operands, got '{}' and '{}'",
                    a.type_name(),
                    b.type_name()
                )),
            },
            "==" => Value::Boolean(left == right),
            "!=" => Value::Boolean(left != right),
            ">" => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Value::Boolean(a > b),
                (Value::Float(a), Value::Float(b)) => Value::Boolean(a > b),
                (Value::Int(a), Value::Float(b)) => match Float::from_int(&a) {
                    Ok(a_float) => Value::Boolean(a_float > b),
                    Err(_) => self.raise("TypeError", "Failed to convert Int to Float"),
                },
                (Value::Float(a), Value::Int(b)) => match Float::from_int(&b) {
                    Ok(b_float) => Value::Boolean(a > b_float),
                    Err(_) => self.raise("TypeError", "Failed to convert Int to Float"),
                },
                (Value::String(a), Value::String(b)) => Value::Boolean(a > b),
                (a, b) => self.raise("TypeError", &format!("Cannot compare {} > {}", a.type_name(), b.type_name())),
            },
            "<" => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Value::Boolean(a < b),
                (Value::Float(a), Value::Float(b)) => Value::Boolean(a < b),
                (Value::Int(a), Value::Float(b)) => match Float::from_int(&a) {
                    Ok(a_float) => Value::Boolean(a_float < b),
                    Err(_) => self.raise("TypeError", "Failed to convert Int to Float"),
                },
                (Value::Float(a), Value::Int(b)) => match Float::from_int(&b) {
                    Ok(b_float) => Value::Boolean(a < b_float),
                    Err(_) => self.raise("TypeError", "Failed to convert Int to Float"),
                },
                (Value::String(a), Value::String(b)) => Value::Boolean(a < b),
                (a, b) => self.raise("TypeError", &format!("Cannot compare {} < {}", a.type_name(), b.type_name())),
            },
            ">=" => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Value::Boolean(a >= b),
                (Value::Float(a), Value::Float(b)) => Value::Boolean(a >= b),
                (Value::Int(a), Value::Float(b)) => match Float::from_int(&a) {
                    Ok(a_float) => Value::Boolean(a_float >= b),
                    Err(_) => self.raise("TypeError", "Failed to convert Int to Float"),
                },
                (Value::Float(a), Value::Int(b)) => match Float::from_int(&b) {
                    Ok(b_float) => Value::Boolean(a >= b_float),
                    Err(_) => self.raise("TypeError", "Failed to convert Int to Float"),
                },
                (Value::String(a), Value::String(b)) => Value::Boolean(a >= b),
                (a, b) => self.raise("TypeError", &format!("Cannot compare {} >= {}", a.type_name(), b.type_name())),
            },
            "<=" => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Value::Boolean(a <= b),
                (Value::Float(a), Value::Float(b)) => Value::Boolean(a <= b),
                (Value::Int(a), Value::Float(b)) => Value::Boolean(Float::from_int(&a) <= Ok(b)),
                (Value::Float(a), Value::Int(b)) => {
                    match Float::from_int(&b) {
                        Ok(b_float) => Value::Boolean(a <= b_float),
                        Err(_) => self.raise("TypeError", "Failed to convert Int to Float"),
                    }
                }                
                (Value::Int(a), Value::Float(b)) => {
                    match Float::from_int(&a) {
                        Ok(a_float) => Value::Boolean(a_float <= b),
                        Err(_) => self.raise("TypeError", "Failed to convert Int to Float"),
                    }
                }                
                (a, b) => self.raise("TypeError", &format!("Cannot compare {} <= {}", a.type_name(), b.type_name())),
            },
            "&&" => Value::Boolean(left.is_truthy() && right.is_truthy()),
            "||" => Value::Boolean(left.is_truthy() || right.is_truthy()),
            "in" => {
                if let Value::String(s) = &right {
                    if let Value::String(sub) = &left {
                        Value::Boolean(s.contains(sub))
                    } else {
                        self.raise("TypeError", "Left operand must be a string for 'in' operation");
                        NULL
                    }
                } else if let Value::List(list) = &right {
                    Value::Boolean(list.contains(&left))
                } else {
                    self.raise("TypeError", "Right operand must be a string or list for 'in' operation");
                    NULL
                }
            },
            "xor" => {
                match (left, right) {
                    (Value::Boolean(a), Value::Boolean(b)) => Value::Boolean(a ^ b),
                    (Value::Int(a), Value::Int(b)) => Value::Boolean(a != b),
                    (Value::Float(a), Value::Float(b)) => Value::Boolean(a != b),
                    (Value::String(a), Value::String(b)) => Value::Boolean(a != b),
                    (a, b) => self.raise("TypeError", &format!("Cannot apply 'xor' to {} and {}", a.type_name(), b.type_name())),
                }
            },
            "xnor" => {
                match (left, right) {
                    (Value::Boolean(a), Value::Boolean(b)) => Value::Boolean(!(a ^ b)),
                    (Value::Int(a), Value::Int(b)) => Value::Boolean(a == b),
                    (Value::Float(a), Value::Float(b)) => Value::Boolean(a == b),
                    (Value::String(a), Value::String(b)) => Value::Boolean(a == b),
                    (a, b) => self.raise("TypeError", &format!("Cannot apply 'xnor' to {} and {}", a.type_name(), b.type_name())),
                }
            },
            "abs" => {
                match right {
                    Value::Int(n) => Value::Int(n.abs()),
                    Value::Float(f) => Value::Float(f.abs()),
                    _ => self.raise("TypeError", &format!("Cannot apply 'abs' to {}", right.type_name())),
                }
            },
            _ => self.raise("SyntaxError", &format!("Unknown operator '{}'", operator)),
        }
    }

    fn handle_number(&mut self, map: HashMap<Value, Value>) -> Value {
        if let Some(Value::String(s)) = map.get(&Value::String("value".to_string())) {
            if s.contains('.') {
                match Float::from_str(s) {
                    Ok(num) => Value::Float(num),
                    Err(_) => self.raise("RuntimeError", "Invalid float format"),
                }
            } else {
                match Int::from_str(s) {
                    Ok(num) => Value::Int(num),
                    Err(_) => self.raise("RuntimeError", "Invalid integer format"),
                }
            }
        } else {
            self.raise("RuntimeError", "Missing 'value' in number statement")
        }
    }    

    fn handle_string(&mut self, map: HashMap<Value, Value>) -> Value {
        if let Some(Value::String(s)) = map.get(&Value::String("value".to_string())) {
            let mut modified_string = s.clone();
            let mut is_raw = false;
            let mut is_bytes = false;
    
            if let Some(Value::List(mods)) = map.get(&Value::String("mods".to_string())) {
                for mod_value in mods {
                    if let Value::String(modifier) = mod_value {
                        match modifier.as_str() {
                            "f" => {
                                let mut output = String::new();
                                let mut chars = modified_string.chars().peekable();
    
                                while let Some(c) = chars.next() {
                                    if c == '{' {
                                        if let Some(&'{') = chars.peek() {
                                            chars.next();
                                            output.push('{');
                                            continue;
                                        }
    
                                        let mut expr = String::new();
                                        let mut brace_level = 1;
    
                                        while let Some(&next_c) = chars.peek() {
                                            chars.next();
                                            if next_c == '{' {
                                                brace_level += 1;
                                            } else if next_c == '}' {
                                                brace_level -= 1;
                                                if brace_level == 0 {
                                                    break;
                                                }
                                            }
                                            expr.push(next_c);
                                        }
    
                                        if brace_level != 0 {
                                            return self.raise("SyntaxError", "Unmatched '{' in f-string");
                                        }
    
                                        let raw_tokens = Lexer::new(&expr).tokenize(self.config.print_comments);
                                        if raw_tokens.is_empty() {
                                            return self.raise("SyntaxError", "Empty expression inside {}");
                                        }
                                        debug_log(
                                            &format!(
                                                "Generated f-string tokens: {:?}",
                                                raw_tokens
                                                    .iter()
                                                    .filter(|token| token.0 != "WHITESPACE")
                                                    .collect::<Vec<_>>()
                                            ),
                                            &self.config,
                                            Some(self.use_colors),
                                        );
                                        
                                        let tokens: Vec<Token> = raw_tokens.into_iter()
                                            .map(|(t, v)| Token(t, v))
                                            .collect();
                                        let parsed = match Parser::new(tokens, self.config.clone(), expr.clone(), self.use_colors, &self.file_path).parse_safe() {
                                            Ok(parsed) => parsed,
                                            Err(error) => {
                                                return self.raise("SyntaxError", &format!("Error parsing f-string expression: {}", error.msg));
                                            }
                                        };
                                        if parsed.is_empty() {
                                            return self.raise("SyntaxError", "Empty expression inside {}");
                                        }
                                        if parsed.len() > 1 {
                                            return self.raise("SyntaxError", "Expected a single expression inside {} in f-string");
                                        }
                                        debug_log(
                                            &format!(
                                                "Generated f-string statements: [{}]",
                                                parsed
                                                    .iter()
                                                    .map(|stmt| format_value(&stmt.convert_to_map()))
                                                    .collect::<Vec<String>>()
                                                    .join(", ")
                                            ),
                                            &self.config,
                                            Some(self.use_colors),
                                        );

                                        debug_log(
                                            &format!("<FString: {}>", expr.clone()),
                                            &self.config,
                                            Some(self.use_colors.clone()),
                                        );
    
                                        let result = self.evaluate(parsed[0].clone());
                                        output.push_str(&result.to_string());
                                    } else if c == '}' {
                                        if let Some(&'}') = chars.peek() {
                                            chars.next();
                                            output.push('}');
                                            continue;
                                        } else {
                                            return self.raise("SyntaxError", "Single '}' encountered in format string");
                                        }
                                    } else {
                                        output.push(c);
                                    }
                                }
    
                                modified_string = output;
                            }
                            "r" => {
                                is_raw = true;
                            }
                            "b" => {
                                is_bytes = true;
                            }
                            _ => return self.raise("RuntimeError", &format!("Unknown string modifier: {}", modifier)),
                        }
                    } else {
                        self.raise("TypeError", "Expected a string for string modifier");
                        return NULL;
                    }
                }
            }

            if !is_raw {
                modified_string = match unescape_string(&modified_string) {
                    Ok(unescaped) => unescaped,
                    Err(e) => return self.raise("UnicodeError", &e),
                };
            }
            
            let unquoted = if modified_string.len() >= 2 {
                let first = modified_string.chars().next().unwrap();
                let last = modified_string.chars().last().unwrap();
                if (first == '"' && last == '"') || (first == '\'' && last == '\'') {
                    modified_string[1..modified_string.len() - 1].to_string()
                } else {
                    modified_string
                }
            } else {
                modified_string
            };

            if is_bytes {
                return Value::Bytes(unquoted.clone().into_bytes());
            }
    
            Value::String(unquoted)
        } else {
            self.raise("RuntimeError", "Missing 'value' in string statement")
        }
    }

    fn handle_boolean(&mut self, map: HashMap<Value, Value>) -> Value {
        if let Some(Value::String(s)) = map.get(&Value::String("value".to_string())) {
            match s.as_str() {
                "true" => TRUE,
                "false" => FALSE,
                "null" => NULL,
                _ => self.raise("RuntimeError", &format!("Invalid boolean format: {}, expected: true, false or null.", s).as_str()),
            }
        } else {
            self.raise("RuntimeError", "Missing 'value' in boolean statement")
        }
    }
}
