use std::collections::{HashMap, BTreeMap};
use crate::env::runtime::config::{Config, get_from_config, set_in_config};
use crate::env::runtime::utils::{
    hex_to_ansi,
    format_value,
    find_closest_match,
    TRUE, FALSE, NULL,
    debug_log,
    debug_log_no_newline,
    check_ansi,
    unescape_string,
    to_static,
    create_function,
    create_note,
    format_type,
    get_type_from_token_name,
    sanitize_alias,
    special_function_meta,
    check_version,
    fix_path,
    fix_and_parse_json,
    json_to_value,
    char_to_digit,
    get_remaining_stack_size,
    wrap_in_help
};
use crossterm::{
    cursor::{MoveUp, MoveToColumn},
    terminal::{Clear, ClearType},
    ExecutableCommand,
};
use crate::env::runtime::pattern_reg::{predict_sequence, predict_sequence_until_length};
use crate::env::runtime::types::{Int, Float, VALID_TYPES};
use crate::env::runtime::value::Value;
use crate::env::runtime::errors::Error;
use crate::env::runtime::variables::Variable;
use crate::env::runtime::statements::Statement;
use crate::env::runtime::objects::{Object, ObjectMetadata, Class};
use crate::env::runtime::native;
use crate::env::runtime::functions::{FunctionMetadata, Parameter, ParameterKind, Function, NativeFunction, UserFunctionMethod};
use crate::env::runtime::generators::{Generator, GeneratorType, NativeGenerator, CustomGenerator, RangeValueIter, InfRangeIter, RangeLengthIter};
use crate::env::runtime::libs::STD_LIBS;
use crate::env::runtime::internal_structs::{Cache, InternalStorage, State, PatternMethod, Stack, StackType};
use std::sync::{Arc, atomic::{AtomicBool, Ordering}};
use std::path::{PathBuf, Path};
use std::fs;
use regex::Regex;
use tokio::runtime::Runtime;
use reqwest;
use serde_urlencoded;
use std::io::{stdout, Write};
use std::sync::Mutex;

use crate::lexer::Lexer;
use crate::env::runtime::preprocessor::Preprocessor;
use crate::parser::Parser;
use crate::env::runtime::tokens::{Location};

const VERSION: &str = env!("VERSION");

#[derive(Debug, Clone)]
pub struct Interpreter {
    config: Config,
    og_cfg: Config,
    pub err: Option<Error>,
    pub is_returning: bool,
    pub return_value: Value,
    pub state: State,
    stack: Stack,
    use_colors: bool,
    pub current_statement: Option<Statement>,
    pub variables: HashMap<String, Variable>,
    file_path: String,
    cwd: PathBuf,
    preprocessor_info: (PathBuf, PathBuf, bool),
    cache: Cache,
    internal_storage: InternalStorage,
    defer_stack: Vec<Vec<Statement>>,
    scope: String,
    pub stop_flag: Option<Arc<AtomicBool>>,
}

impl Interpreter {
    pub fn new(config: Config, use_colors: bool, file_path: &str, cwd: &PathBuf, preprocessor_info: (PathBuf, PathBuf, bool), argv: &[String]) -> Self {
        let mut this = Self {
            config: config.clone(),
            og_cfg: config.clone(),
            err: None,
            return_value: NULL,
            is_returning: false,
            state: State::Normal,
            stack: Stack::new(),
            use_colors,
            current_statement: None,
            variables: HashMap::new(),
            file_path: file_path.to_owned(),
            cwd: cwd.clone(),
            preprocessor_info,
            cache: Cache {
                operations: HashMap::new(),
                constants: HashMap::new(),
                iterables: HashMap::new(),
            },
            internal_storage: InternalStorage {
                types: HashMap::new(),
                lambda_counter: 0,
                use_42: (false, false),
            },
            defer_stack: vec![],
            scope: "main".to_owned(),
            stop_flag: None,
        };


        for type_ in VALID_TYPES {
            this.internal_storage.types.insert(
                type_.to_string(),
                Value::String(type_.to_string()),
            );
        }
        
        this.variables.insert(
            "argv".to_owned(),
            Variable::new(
                "argv".to_owned(),
                Value::List(argv.iter().cloned().map(Value::String).collect()),
                "list".to_owned(),
                false,
                true,
                true,
            ),
        );

        let dir = Path::new(file_path).parent().unwrap_or_else(|| Path::new(".")).to_str().unwrap_or(".").to_owned();
        std::env::set_current_dir(&dir).ok();

        this.variables.insert(
            "__dir__".to_owned(),
            Variable::new(
                "__dir__".to_owned(),
                Value::String(dir),
                "str".to_owned(),
                false,
                true,
                true,
            ),
        );
        this.variables.insert(
            "__file__".to_owned(),
            Variable::new(
                "__file__".to_owned(),
                Value::String(file_path.to_owned()),
                "str".to_owned(),
                false,
                true,
                true,
            ),
        );
    
        let mut insert_builtin = |name: &str, val: Value| {
            this.variables.insert(
                name.to_owned(),
                Variable::new(name.to_owned(), val, "function".to_owned(), false, true, true),
            );
        };
    
        insert_builtin("print", Value::Function(native::print_fn()));
        insert_builtin("styledprint", Value::Function(native::styled_print_fn()));
        insert_builtin("input", Value::Function(native::input_fn()));
        insert_builtin("len", Value::Function(native::len_fn()));
        insert_builtin("help", Value::Function(native::help_fn()));
        insert_builtin("type_of", Value::Function(native::type_fn()));
        insert_builtin("sum", Value::Function(native::sum_fn()));
        insert_builtin("ord", Value::Function(native::ord_fn()));
        insert_builtin("char", Value::Function(native::char_fn()));
        insert_builtin("styledstr", Value::Function(native::styledstr_fn()));
        insert_builtin("array", Value::Function(native::array_fn()));
        this.variables.insert(
            "00__placeholder__".to_owned(),
            Variable::new("__placeholder__".to_owned(), Value::Function(native::placeholder_fn()), "function".to_owned(), false, true, true),
        );
    
        this
    }

    pub fn set_scope(&mut self, scope: &str) {
        self.scope = scope.to_owned();
    }

    pub fn is_stopped(&self) -> bool {
        self.state == State::Exit
    }

    pub fn set_cache(&mut self, cache: Cache) {
        self.cache = cache;
    }

    pub fn get_cache(&self) -> &Cache {
        &self.cache
    }

    fn to_index(&mut self, val: &Value, len: usize) -> Result<usize, Value> {
        let mut val = val.clone();
        if val.is_statement() {
            val = self.evaluate(val.convert_to_statement());
        }
        match val {
            Value::Int(i) => {
                let idx = i.to_i64().map_err(|_| { self.raise("ConversionError", "Failed to convert Int to isize") })? as isize;
                let adjusted = if idx < 0 { len as isize + idx } else { idx };
                if adjusted >= 0 && (adjusted as usize) < len {
                    Ok(adjusted as usize)
                } else {
                    Err(Value::Error("IndexError", "Index out of range", None))
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
                    return Err(Value::Error("IndexError", "Float index must have zero fractional part", None));
                }
                let idx = f.to_f64().map_err(|_| { self.raise("ConversionError", "Failed to convert Float to isize") })? as isize;
                let adjusted = if idx < 0 { len as isize + idx } else { idx };
                if adjusted >= 0 && (adjusted as usize) < len {
                    Ok(adjusted as usize)
                } else {
                    Err(Value::Error("IndexError", "Index out of range", None))
                }
            }
            _ => Err(Value::Error("IndexError", "Index must be Int, String or Float with no fraction", None)),
        }
    }

    pub fn exit(&mut self, code: Value) {
        self.state = State::Defer;
        if !self.defer_stack.is_empty() {
            let defer_statements = self.defer_stack.concat();
            for stmt in defer_statements {
                self.current_statement = Some(stmt);
                self.evaluate(self.current_statement.clone().unwrap());
            }
        }
        self.state = State::Exit;
        self.is_returning = true;
        self.stack.clear();
        self.return_value = match code {
            Value::Int(i) => Value::Int(i),
            Value::Float(f) => Value::Float(f),
            Value::String(s) => Value::String(s),
            _ => {
                self.raise("TypeError", "Exit code must be an int, float or string");
                NULL
            }
        };
        debug_log(
            &format!("<Exit with code: {}>", format_value(&self.return_value)),
            &self.config,
            Some(self.use_colors),
        );
    }

    pub fn get_traceback(&self) -> BTreeMap<String, String> {
        let mut traceback = BTreeMap::new();
        for (file, loc, _) in self.stack.iter() {
            let location = loc.as_ref().map_or("unknown location".to_string(), |l| format!("{}:{}:{}", l.file, l.line_number, l.range.0));
            traceback.insert(file.clone(), location);
        }
        traceback
    }

    // i personally hate this function
    fn check_type(&mut self, value: &Value, expected: &Value, error: bool) -> bool {
        static TYPES_MAPPING: once_cell::sync::Lazy<std::collections::HashMap<&'static str, &'static str>> = once_cell::sync::Lazy::new(|| {
            let mut m = std::collections::HashMap::new();
            m.insert("void", "void");
            m.insert("any", "any");
            m.insert("float", "float");
            m.insert("method", "function");
            m.insert("int", "int");
            m.insert("bool", "bool");
            m.insert("str", "str");
            m.insert("list", "list");
            m.insert("map", "map");
            m.insert("function", "function");
            m.insert("bytes", "bytes");
            m.insert("auto", "any");
            m
        });
    
        let mut types_mapping = TYPES_MAPPING.clone();
    
        for obj in self.variables.values() {
            if let Value::Module(obj, _) = &obj.value {
                types_mapping.insert(&obj.name(), "object");
            }
        }
    
        let valid_types = &VALID_TYPES;
    
        if let Value::String(s) = expected {
            if s.trim_start().starts_with('{') {
                let fixed = match fix_and_parse_json(s) {
                    Some(f) => f,
                    None => {
                        self.raise("TypeError", &format!("Invalid JSON string: '{}'", s));
                        return false;
                    }
                };
                if let Value::Map { keys, values } = json_to_value(&fixed) {
                    return self.check_type(value, &Value::Map { keys, values }, error);
                }
            }
        }
    
        fn split_type_prefix(t: &str) -> (bool, bool, String) {
            let mut chars = t.chars().peekable();
            let mut is_ref = false;
            let mut is_maybe = false;
    
            while let Some(&c) = chars.peek() {
                match c {
                    '&' => {
                        is_ref = true;
                        chars.next();
                    }
                    '?' => {
                        is_maybe = true;
                        chars.next();
                    }
                    _ => break,
                }
            }
            (is_ref, is_maybe, chars.collect())
        }
    
        if let Value::String(type_str) = expected {
            if value.type_name() == *type_str {
                return true;
            }
    
            let (is_ref_expected, is_maybe, expected_base) = split_type_prefix(type_str);
    
            if is_maybe && value.type_name() == "void" {
                return true;
            }
    
            if expected_base.contains('|') {
                for ty in expected_base.split('|') {
                    let ty_str = if is_ref_expected { format!("&{}", ty) } else { ty.to_string() };
                    let ty_str = if is_maybe { format!("?{}", ty_str) } else { ty_str };
                    if self.check_type(value, &Value::String(ty_str), false) {
                        return true;
                    }
                }
                if error {
                    self.raise(
                        "TypeError",
                        &format!("Expected type '{}', got '{}'", type_str, value.type_name()),
                    );
                }
                return false;
            }
    
            let actual_type_str = value.type_name();
            let normalized_actual = types_mapping
                .get(actual_type_str.as_str())
                .copied()
                .unwrap_or(actual_type_str.as_str());
            let normalized_expected = types_mapping
                .get(expected_base.as_str())
                .copied()
                .unwrap_or_else(|| to_static(expected_base));
    
            let is_ref_actual = normalized_actual.starts_with('&');
    
            if normalized_expected == "any" || normalized_actual == "any" {
                return true;
            }
    
            if is_ref_expected != is_ref_actual {
                if error {
                    self.raise(
                        "TypeError",
                        &format!("Expected type '{}', but got '{}'", type_str, normalized_actual),
                    );
                }
                return false;
            }
    
            if !valid_types.contains(&normalized_expected) {
                return self._handle_invalid_type(normalized_expected, valid_types.to_vec());
            }
            if !valid_types.contains(&normalized_actual) {
                return self._handle_invalid_type(normalized_actual, valid_types.to_vec());
            }
    
            if normalized_actual != normalized_expected {
                if error {
                    self.raise(
                        "TypeError",
                        &format!("Expected type '{}', but got '{}'", type_str, normalized_actual),
                    );
                }
                return false;
            }
    
            return true;
        } else if let Value::Map { keys, values } = expected {
            let expected_hashmap: std::collections::HashMap<_, _> =
                keys.iter().cloned().zip(values.iter().cloned()).collect();
    
            match expected_hashmap.get(&Value::String("type_kind".to_string())) {
                Some(Value::String(kind)) if kind == "function" => {
                    if let Value::Function(_) = value {
                        // TODO: Check function signature
                        return true;
                    }
                    if error {
                        self.raise("TypeError", &format!("Expected type 'function', got '{}'", value.to_string()));
                    }
                    return false;
                }
                Some(Value::String(kind)) if kind == "indexed" => {
                    if matches!(value, Value::List(_) | Value::String(_) | Value::Bytes(_)) {
                        return true;
                    }
                }
                Some(Value::String(kind)) if kind == "union" => {
                    let types = expected_hashmap.get(&Value::String("types".to_string()));
                    for ty in types.unwrap_or(&Value::List(vec![])).iter() {
                        if self.check_type(value, &ty, false) {
                            return true;
                        }
                    }
                }
                Some(Value::String(kind)) if kind == "new" => {
                    let type_name = match expected_hashmap.get(&Value::String("name".to_string())) {
                        Some(Value::String(name)) => name,
                        _ => {
                            self.raise("TypeError", "Expected 'name' to be a string in 'new' type");
                            return false;
                        }
                    };
                    let base_type = match expected_hashmap.get(&Value::String("base".to_string())) {
                        Some(base) => base,
                        _ => {
                            self.raise("TypeError", "Expected 'base' to be a string in 'new' type");
                            return false;
                        }
                    };
                    let vars = match expected_hashmap.get(&Value::String("variables".to_string())) {
                        Some(Value::List(vars)) => vars,
                        _ => {
                            self.raise("TypeError", "Expected 'variables' to be a list in 'new' type");
                            return false;
                        }
                    };
                    let conds = match expected_hashmap.get(&Value::String("conditions".to_string())) {
                        Some(Value::List(conds)) => conds.iter()
                            .map(|v| v.convert_to_statement())
                            .collect::<Vec<_>>(),
                        _ => {
                            self.raise("TypeError", "Expected 'conditions' to be a list in 'new' type");
                            return false;
                        }
                    };
    
                    if !self.check_type(value, base_type, false) {
                        if error {
                            self.raise("TypeError", &format!(
                                "Type '{}' doesn't match inner type of '{}', which is '{}'",
                                value.to_string(),
                                type_name,
                                base_type.to_string()
                            ));
                        }
                        return false;
                    }
    
                    let mut new_interpreter = Interpreter::new(
                        self.config.clone(),
                        self.use_colors,
                        &self.file_path,
                        &self.cwd,
                        self.preprocessor_info.clone(),
                        &[],
                    );
                    new_interpreter.set_scope(&format!("{}+scope.{}", self.scope, base_type));
                    if !vars.is_empty() {
                        new_interpreter.variables.insert(
                            vars[0].to_string(),
                            Variable::new(
                                vars[0].to_string(),
                                value.clone(),
                                value.type_name().to_string(),
                                false,
                                true,
                                true,
                            ),
                        );
                    }
    
                    for (i, cond) in conds.iter().enumerate() {
                        new_interpreter.current_statement = Some(cond.clone());
                        if !new_interpreter.evaluate(cond.clone()).is_truthy() {
                            self.raise("TypeError", &format!(
                                "Conditions for type '{}' not met condition #{}",
                                type_name,
                                i + 1
                            ));
                            return false;
                        }
                    }
    
                    return true;
                }
                _ => {
                    self.raise("TypeError", &format!(
                        "Expected type 'function', 'indexed' or 'union', got '{}'",
                        value.to_string()
                    ));
                    return false;
                }
            }
        }
    
        if error {
            self.raise("TypeError", &format!(
                "Expected type '{}', got '{}'",
                expected.to_string(),
                value.to_string()
            ));
        }
        false
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
        
        let mut lexer = Lexer::new(&content, to_static(path.display().to_string()), None);
        let raw_tokens = lexer.tokenize();

        let (pr1, _, ep) = self.preprocessor_info.clone();
        let processed_tokens = if ep {
            let mut preprocessor = Preprocessor::new(
                pr1,
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
        let mut parser = Parser::new(processed_tokens);
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

        let shared_interpreter = Arc::new(Mutex::new(Interpreter::new(
            self.config.clone(),
            self.use_colors,
            path.display().to_string().as_str(),
            &parent_dir,
            self.preprocessor_info.clone(),
            &vec![],
        )));

        shared_interpreter.lock().unwrap().stack = self.stack.clone();
        let result = shared_interpreter.lock().unwrap().interpret(statements, true);
        self.stack = shared_interpreter.lock().unwrap().stack.clone();

        let properties = shared_interpreter.lock().unwrap().variables.clone();

        if let Some(err) = shared_interpreter.lock().unwrap().err.clone() {
            self.raise_with_ref(
                "ImportError",
                &format!("Error while importing '{}'", path.display()),
                err,
            );
            return HashMap::new();
        }

        let mut final_properties = HashMap::new();
        for (var_name, var) in properties {
            if var.is_public() && !var.is_native() {
                if let Value::Function(ref f) = var.value {
                    if let Function::Custom(func) = f {
                        final_properties.insert(
                            var_name.clone(),
                            Variable::new(
                                var_name,
                                Value::Function(Function::CustomMethod(
                                    Arc::new(UserFunctionMethod::new_from_func_with_interpreter(
                                        func.clone(),
                                        shared_interpreter.clone(),
                                    )),
                                )),
                                "function".to_string(),
                                var.is_static(),
                                var.is_public(),
                                var.is_final(),
                            ),
                        );
                    }
                } else {
                    final_properties.insert(var_name, var);
                }
            }
        }
    
        match result {
            Ok(_) => final_properties,
            Err(e) => {
                self.raise_with_ref(
                    "ImportError",
                    &format!("Error while importing '{}'", path.display()),
                    e.clone(),
                );
                return HashMap::new();
            },
        }
    }

    pub fn interpret(&mut self, mut statements: Vec<Statement>, deferable: bool) -> Result<Value, Error> {
        self.is_returning = false;
        self.return_value = NULL;
        self.err = None;
        self.current_statement = None;
        self.state = State::Normal;
        self.stack.clear();
        self.stack.push((
            self.file_path.clone(),
            None,
            StackType::File,
        ));

        loop {
            for statement in &statements {
                if let Some(err) = &self.err {
                    return Err(err.clone());
                }

                let stmt = match statement {
                    Statement::Statement { .. } => statement.clone(),
                    _ => {
                        return Err(Error::new(
                            "InvalidStatement",
                            "Expected a map. This is probably an issue with your installation. Try installing the latest stable version.",
                            &self.file_path.to_string(),
                        ));
                    }
                };

                self.current_statement = Some(stmt.clone());

                let value = self.evaluate(stmt);

                if let Value::Error(err_type, err_msg, referr) = &value {
                    if let Some(referr) = referr {
                        self.raise_with_ref(err_type, &err_msg, referr.clone());
                    } else {
                        self.raise(err_type, &err_msg);
                    }
                }

                if self.is_returning {
                    self.return_value = value;
                    return Ok(self.return_value.clone());
                }

                if let Some(err) = &self.err {
                    return Err(err.clone());
                }

                self.return_value = value;

                if let State::TCO(_) = self.state {
                    break;
                }
            }

            if let State::TCO(stmt) = std::mem::replace(&mut self.state, State::Normal) {
                statements = vec![stmt];
                continue;
            }

            break;
        }

        if deferable {
            let old_state = std::mem::replace(&mut self.state, State::Defer);

            if !self.defer_stack.is_empty() {
                let defer_statements = self.defer_stack.concat();
                for stmt in defer_statements {
                    self.current_statement = Some(stmt.clone());
                    self.evaluate(stmt);
                }
            }

            self.state = old_state;
        }

        self.stack.pop();

        Ok(self.return_value.clone())
    }

    fn get_location_from_current_statement(&self) -> Option<Location> {
        match &self.current_statement {
            Some(Statement::Statement { loc, .. }) => loc.clone(),
            _ => None,
        }
    }

    pub fn raise(&mut self, error_type: &str, msg: &str) -> Value {
        let loc = self.get_location_from_current_statement().unwrap_or_else(|| Location {
            file: self.file_path.clone(),
            line_string: "".to_string(),
            line_number: 0,
            range: (0, 0),
        });

        self.err = Some(Error {
            error_type: error_type.to_string(),
            msg: msg.to_string(),
            help: None,
            loc: Some(loc),
            ref_err: None,
        });

        NULL
    }

    pub fn raise_with_help(&mut self, error_type: &str, msg: &str, help: &str) -> Value {
        let loc = self.get_location_from_current_statement().unwrap_or_else(|| Location {
            file: self.file_path.clone(),
            line_string: "".to_string(),
            line_number: 0,
            range: (0, 0),
        });

        self.err = Some(Error {
            error_type: error_type.to_string(),
            msg: msg.to_string(),
            help: Some(help.to_string()),
            loc: Some(loc),
            ref_err: None,
        });

        NULL
    }

    pub fn raise_with_ref(&mut self, error_type: &str, msg: &str, ref_err: Error) -> Value {
        let loc = self.get_location_from_current_statement().unwrap_or_else(|| Location {
            file: self.file_path.clone(),
            line_string: "".to_string(),
            line_number: 0,
            range: (0, 0),
        });

        self.err = Some(Error {
            error_type: error_type.to_string(),
            msg: msg.to_string(),
            help: None,
            loc: Some(loc),
            ref_err: Some(Box::new(ref_err)),
        });

        NULL
    }

    fn check_stop_flag(&mut self) -> bool {
        if get_remaining_stack_size().unwrap_or(self.config.stack_size) < 60_000 {
            self.raise("StackOverflowError", &format!("Maximum stack size exceeded ({} Bytes)", self.config.stack_size));
            return true;
        }
        if let Some(stop_flag) = &self.stop_flag {
            if stop_flag.load(Ordering::Relaxed) {
                self.is_returning = true;
                return true;
            }
        }
        false
    }
    
    pub fn evaluate(&mut self, statement: Statement) -> Value {
        self.current_statement = Some(statement.clone());

        if self.check_stop_flag() {
            return NULL;
        }
    
        self.variables.entry("_".to_string()).or_insert_with(|| {
            Variable::new("_".to_string(), NULL.clone(), "any".to_string(), false, true, true)
        });
    
        self.variables.entry("_err".to_string()).or_insert_with(|| {
            Variable::new(
                "_err".to_string(),
                Value::Tuple(vec![]),
                "tuple".to_string(),
                false,
                true,
                true,
            )
        });
    
        if self.err.is_some() {
            return NULL;
        }

        if statement.is_empty() {
            return NULL;
        }
    
        let Statement::Statement { keys, values, .. } = &statement else {
            return self.raise("SyntaxError", to_static(format!("Expected a statement map, got {:?}", statement)));
        };

        if self.check_stop_flag() {
            return NULL;
        }
    
        if self.state == State::Break || self.state == State::Continue {
            return NULL;
        }
    
        let statement_map: HashMap<Value, Value> = keys.iter().cloned().zip(values.iter().cloned()).collect();
    
        static KEY_TYPE: once_cell::sync::Lazy<Value> = once_cell::sync::Lazy::new(|| Value::String("type".into()));

        let result = match statement_map.get(&*KEY_TYPE) {
            Some(Value::String(t)) => match t.as_str() {
                "IF" => self.handle_if(statement_map),
                "FOR" => self.handle_for_loop(statement_map),
                "WHILE" => self.handle_while(statement_map),
                "TRY_CATCH" | "TRY" => self.handle_try(statement_map),
                "THROW" => self.handle_throw(statement_map),
                "FORGET" => self.handle_forget(statement_map),
                "CONTINUE" | "BREAK" => self.handle_continue_and_break(statement_map),
                "DEFER" => self.handle_defer(statement_map),
                "SCOPE" => self.handle_scope(statement_map),
                "MATCH" => self.handle_match(statement_map),
                "GROUP" => self.handle_group(statement_map),
    
                "FUNCTION_DECLARATION" => self.handle_function_declaration(statement_map),
                "GENERATOR_DECLARATION" => self.handle_generator_declaration(statement_map),
                "RETURN" => self.handle_return(statement_map),
    
                "IMPORT" => self.handle_import(statement_map),
                "EXPORT" => self.handle_export(statement_map),

                "VARIABLE_DECLARATION" => self.handle_variable_declaration(statement_map),
                "VARIABLE" => self.handle_variable(statement_map),
                "ASSIGNMENT" => self.handle_assignment(statement_map),
                "UNPACK_ASSIGN" => self.handle_unpack_assignment(statement_map),
    
                "NUMBER" => self.handle_number(statement_map),
                "STRING" => self.handle_string(statement_map),
                "BOOLEAN" => self.handle_boolean(statement_map),
    
                "TUPLE" => self.handle_tuple(statement_map),
                "MAP" => self.handle_map(statement_map),
                "ITERABLE" => self.handle_iterable(statement_map),
    
                "OPERATION" => self.handle_operation(statement_map),
                "UNARY_OPERATION" => self.handle_unary_op(statement_map),
                "COMPOUND_ASSIGN" => self.handle_compound_assignment(statement_map),
    
                "CALL" => self.handle_call(statement_map),
                "METHOD_CALL" => self.handle_method_call(statement_map),
                "PROPERTY_ACCESS" => self.handle_property_access(statement_map),
                "INDEX_ACCESS" => self.handle_index_access(statement_map),
    
                "TYPE" => self.handle_type(statement_map),
                "TYPE_CONVERT" => self.handle_type_conversion(statement_map),
                "TYPE_DECLARATION" => self.handle_type_declaration(statement_map),
    
                "POINTER_REF" | "POINTER_DEREF" | "POINTER_ASSIGN" => self.handle_pointer(statement_map),
    
                _ => self.raise("NotImplemented", &format!("Unsupported statement type: {}", t)),
            },
            _ => self.raise("SyntaxError", "Missing or invalid 'type' in statement map"),
        };

        if self.check_stop_flag() {
            return NULL;
        }
    
        if let Some(err) = self.err.clone() {
            let mut tuple_vec = vec![
                Value::String(err.error_type),
                Value::String(err.msg),
            ];

            if err.help.is_some() {
                tuple_vec.push(Value::String(err.help.unwrap()));
            }

            let tuple = Value::Tuple(tuple_vec);
    
            if let Some(var) = self.variables.get_mut("_") {
                var.set_value(tuple.clone());
            }
    
            if let Some(var) = self.variables.get_mut("_err") {
                var.set_value(tuple);
            }
    
            return NULL;
        }

        if self.check_stop_flag() {
            return NULL;
        }
    
        if result != NULL {
            if let Some(var) = self.variables.get_mut("_") {
                var.set_value(result.clone());
            }
        }
    
        result
    }

    fn handle_export(&mut self, statement: HashMap<Value, Value>) -> Value {
        let name = match statement.get(&Value::String("name".to_string())) {
            Some(Value::String(n)) => n,
            _ => return self.raise("RuntimeError", "Missing or invalid 'name' in export"),
        };

        let alias = match statement.get(&Value::String("alias".to_string())) {
            Some(Value::String(a)) => a.clone(),
            _ => return self.raise("RuntimeError", "Missing or invalid 'alias' in export"),
        };

        let modifiers = match statement.get(&Value::String("modifiers".to_string())) {
            Some(Value::List(m)) => m,
            _ => return self.raise("RuntimeError", "Expected a list for 'modifiers' in export"),
        };

        let mut is_public = false;
        let mut is_static = None;
        let mut is_final = None;

        for modifier in modifiers {
            if let Value::String(modifier_str) = modifier {
                match modifier_str.as_str() {
                    "public" => is_public = true,
                    "static" => is_static = Some(true),
                    "final" => is_final = Some(true),
                    "non-static" => is_static = Some(false),
                    "mutable" => is_final = Some(false),
                    _ => return self.raise("ModifierError", &format!("Unknown modifier: {}", modifier_str)),
                }
            }
        }

        if self.variables.contains_key(name) {
            let mut var = self.variables.get(name).unwrap().clone();
            if !var.is_public() && !is_public {
                return self.raise_with_help("ModifierError", &format!("Variable '{}' is not public and cannot be exported", name), "Add 'public' modifier to the export statement to make it public");
            }
            var.set_public(true);
            if let Some(is_static) = is_static {
                var.set_static(is_static);
            }
            if let Some(is_final) = is_final {
                var.set_final(is_final);
            }
            let val = var.value.clone();
            self.variables.insert(
                alias,
                var
            );
            return val;
        } else {
            return self.raise("NameError", &format!("Variable '{}' is not defined therefore cannot be exported", name));
        }
    }

    fn handle_group(&mut self, statement: HashMap<Value, Value>) -> Value {
        let expressions = match statement.get(&Value::String("expressions".to_string())) {
            Some(Value::List(exprs)) => exprs,
            _ => return self.raise("RuntimeError", "Missing or invalid 'expressions' in group"),
        };

        let mut result = Value::Null;
        for expr in expressions {
            result = self.evaluate(expr.convert_to_statement());
        }
        if self.is_returning {
            result = self.return_value.clone();
            self.is_returning = false;
        }
        result
    }

    fn handle_generator_declaration(&mut self, statement: HashMap<Value, Value>) -> Value {
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
        
                let mods = match keys.iter().position(|k| k == &Value::String("modifiers".to_string())) {
                    Some(pos) => match &values[pos] {
                        Value::List(l) => l.iter().filter_map(|v| {
                            if let Value::String(s) = v {
                                Some(s.clone())
                            } else {
                                None
                            }
                        }).collect(),
                        _ => vec![],
                    },
                    None => vec![],
                };
        
                match &type_str_value {
                    Value::Map { .. } => {
                        parameters.push(Parameter::positional_pt(name.as_str(), &type_str_value).set_mods(mods));
                    }
                    Value::String(s) => {
                        parameters.push(Parameter::positional(name.as_str(), s.as_str()).set_mods(mods));
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
        
                let mods = match keys.iter().position(|k| k == &Value::String("modifiers".to_string())) {
                    Some(pos) => match &values[pos] {
                        Value::List(l) => l.iter().filter_map(|v| {
                            if let Value::String(s) = v {
                                Some(s.clone())
                            } else {
                                None
                            }
                        }).collect(),
                        _ => vec![],
                    },
                    None => vec![],
                };
        
                let type_eval = self.evaluate(type_val.convert_to_statement());
        
                let value = match keys.iter().position(|k| k == &Value::String("value".to_string())) {
                    Some(pos) => {
                        let val = &values[pos];
                        self.evaluate(val.convert_to_statement())
                    }
                    None => NULL,
                };
        
                match &type_eval {
                    Value::Map { .. } => {
                        parameters.push(Parameter::positional_optional_pt(
                            name_str.as_str(),
                            &type_eval,
                            value,
                        ).set_mods(mods));
                    }
                    Value::String(s) => {
                        parameters.push(Parameter::positional_optional(
                            name_str.as_str(),
                            s.as_str(),
                            value,
                        ).set_mods(mods));
                    }
                    _ => return self.raise("RuntimeError", "Invalid type for named argument 'type'"),
                }
            } else {
                return self.raise("TypeError", "Expected a map for named argument");
            }
        }        

        let body_formatted: Arc<Vec<Statement>> = Arc::new(body.iter().map(|v| v.convert_to_statement()).collect());
        let ret_type = Arc::new(return_type);

        let config = self.config.clone();
        let use_colors = self.use_colors;
        let file_path = self.file_path.clone();
        let cwd = self.cwd.clone();
        let preprocessor_info = self.preprocessor_info.clone();
        let scope = self.scope.clone();

        let gen_name = if is_static {
            format!("__static_gen_{}", name)
        } else {
            format!("__gen_{}", name)
        };
        let scope_str = format!("{}+scope.{}", scope, gen_name);
        let gen_name_arc = Arc::new(gen_name.clone());

        let generate_gen = move |args: &HashMap<String, Value>| -> Value {
            let mut gen_interpreter = Interpreter::new(
                config.clone(),
                use_colors,
                &file_path,
                &cwd,
                preprocessor_info.clone(),
                &[],
            );
            gen_interpreter.set_scope(&scope_str);
            gen_interpreter.variables.extend(args.iter().map(|(k, v)| {
                (k.clone(), Variable::new(k.clone(), v.clone(), v.type_name().to_string(), false, true, true))
            }));
            let gen_type = GeneratorType::Custom(CustomGenerator {
                body: (*body_formatted).clone(),
                interpreter: Box::new(gen_interpreter),
                ret_type: Box::new((*ret_type).clone()),
                iteration: 0,
                done: false,
                index: 0,
                loop_stack: Vec::new(),
            });
            let generator = Generator::new((*gen_name_arc).clone(), gen_type, is_static);
            Value::Generator(generator)
        };

        if self.variables.contains_key(name) && !name.starts_with("<") {
            if let Some(var) = self.variables.get(name) {
                if var.is_final() {
                    return self.raise("AssigmentError", &format!("Cannot redefine final generator '{}'", name));
                }
            }
        }
        
        self.variables.insert(
            name.to_string(),
            Variable::new(
                name.to_string(),
                Value::Function(Function::Native(Arc::new(NativeFunction::new(
                    name,
                    generate_gen,
                    parameters,
                    "generator",
                    is_public,
                    is_static,
                    is_final,
                    None,
                )))),
                "generator".to_string(),
                is_static,
                is_public,
                is_final,
            ),
        );
        self.variables.get(name)
            .map_or(NULL, |var| var.value.clone())
    }

    fn handle_type_declaration(&mut self, statement: HashMap<Value, Value>) -> Value {
        let name = match statement.get(&Value::String("name".to_string())) {
            Some(Value::String(n)) => n.clone(),
            _ => {
                self.raise("RuntimeError", "Expected a string for 'name' in type declaration");
                return NULL;
            }
        };

        let type_kind = match statement.get(&Value::String("kind".to_string())) {
            Some(Value::String(k)) => k.clone(),
            _ => {
                self.raise("RuntimeError", "Expected a string for 'kind' in type declaration");
                return NULL;
            }
        };

        let generics = match statement.get(&Value::String("generics".to_string())) {
            Some(Value::List(g)) => g.clone(),
            _ => vec![],
        };

        let variables = match statement.get(&Value::String("variables".to_string())) {
            Some(Value::List(v)) => v.clone(),
            _ => vec![],
        };

        let conditions = match statement.get(&Value::String("conditions".to_string())) {
            Some(Value::List(c)) => c.clone(),
            _ => vec![],
        };

        let modifiers = match statement.get(&Value::String("modifiers".to_string())) {
            Some(Value::List(m)) => m.clone(),
            _ => vec![],
        };

        let variants = match statement.get(&Value::String("variants".to_string())) {
            Some(Value::List(v)) => v.clone(),
            _ => vec![],
        };

        if type_kind == "alias" {
            if variants.len() != 1 {
                self.raise("RuntimeError", "Alias type must have exactly one variant");
                return NULL;
            }
            let base_variant = &variants[0];
            let base_type = match base_variant {
                Value::Map { .. } => base_variant.clone(),
                _ => {
                    self.raise("RuntimeError", "Expected variant to be a map for alias base type");
                    return NULL;
                }
            };

            self.internal_storage.types.insert(
                name.clone(),
                Value::Map {
                    keys: vec![
                        Value::String("type_kind".to_string()),
                        Value::String("name".to_string()),
                        Value::String("base".to_string()),
                        Value::String("generics".to_string()),
                        Value::String("variables".to_string()),
                        Value::String("conditions".to_string()),
                        Value::String("modifiers".to_string()),
                    ],
                    values: vec![
                        Value::String("alias".to_string()),
                        Value::String(name),
                        base_type,
                        Value::List(generics),
                        Value::List(variables),
                        Value::List(conditions),
                        Value::List(modifiers),
                    ],
                },
            );
        } else if type_kind == "enum" {
            self.internal_storage.types.insert(
                name.clone(),
                Value::Map {
                    keys: vec![
                        Value::String("type_kind".to_string()),
                        Value::String("name".to_string()),
                        Value::String("variants".to_string()),
                        Value::String("generics".to_string()),
                        Value::String("variables".to_string()),
                        Value::String("conditions".to_string()),
                        Value::String("modifiers".to_string()),
                    ],
                    values: vec![
                        Value::String("enum".to_string()),
                        Value::String(name),
                        Value::List(variants),
                        Value::List(generics),
                        Value::List(variables),
                        Value::List(conditions),
                        Value::List(modifiers),
                    ],
                },
            );
        } else {
            self.raise("RuntimeError", "Unknown type kind in type declaration");
            return NULL;
        }

        NULL
    }

    fn handle_match(&mut self, statement: HashMap<Value, Value>) -> Value {
        let condition = match statement.get(&Value::String("condition".to_string())) {
            Some(v) => v,
            _ => {
                self.raise("RuntimeError", "Missing 'condition' in match statement");
                return NULL;
            }
        };
        if self.err.is_some() {
            return NULL;
        }

        let cases = match statement.get(&Value::String("cases".to_string())) {
            Some(Value::List(c)) => c,
            _ => {
                self.raise("RuntimeError", "Expected a list for 'cases' in match statement");
                return NULL;
            }
        };

        for case in cases.iter() {
            if let Value::Map { keys, values } = case {
                let pattern = match keys.iter().position(|k| k == &Value::String("pattern".to_string())) {
                    Some(pos) => values.get(pos).unwrap_or(&Value::Null),
                    None => {
                        self.raise("RuntimeError", "Expected 'pattern' in match case");
                        return NULL;
                    }
                };

                let guard: Option<Statement> = match (
                    keys.iter().position(|k| k == &Value::String("guard".to_string())),
                    values.get(keys.iter().position(|k| k == &Value::String("guard".to_string())).unwrap_or(usize::MAX)),
                ) {
                    (Some(_), Some(val)) => match val {
                        Value::Map { .. } => Some(val.convert_to_statement()),
                        Value::Null => None,
                        _ => {
                            self.raise("RuntimeError", "Expected 'guard' to be a map or null in match case");
                            return NULL;
                        }
                    },
                    _ => None,
                };

                let body = match keys.iter().position(|k| k == &Value::String("body".to_string())) {
                    Some(pos) => match values.get(pos) {
                        Some(Value::List(b)) => b,
                        _ => {
                            self.raise("RuntimeError", "Expected 'body' to be a list in match case");
                            return NULL;
                        }
                    },
                    None => {
                        self.raise("RuntimeError", "Missing 'body' in match case");
                        return NULL;
                    }
                };

                let matched = match pattern {
                    Value::Null => true,
                    _ => {
                        let check = Statement::Statement {
                            keys: vec![
                                Value::String("type".to_string()),
                                Value::String("left".to_string()),
                                Value::String("operator".to_string()),
                                Value::String("right".to_string()),
                            ],
                            values: vec![
                                Value::String("OPERATION".to_string()),
                                condition.clone(),
                                Value::String("==".to_string()),
                                pattern.clone(),
                            ],
                            loc: self.get_location_from_current_statement(),
                        };
                        self.evaluate(check).is_truthy()
                    }
                };

                if matched {
                    if let Some(g) = guard {
                        if !self.evaluate(g.clone()).is_truthy() {
                            continue;
                        }
                    }

                    let _ = self.interpret(
                        body.iter()
                            .map(|item| item.clone().convert_to_statement())
                            .collect(),
                        true,
                    );
                    return NULL;
                }
            } else {
                self.raise("RuntimeError", "Invalid case in 'match' statement - expected map");
            }
        }

        NULL
    }

    fn handle_scope(&mut self, statement: HashMap<Value, Value>) -> Value {
        let body = match statement.get(&Value::String("body".to_string())) {
            Some(Value::List(b)) => b,
            _ => {
                self.raise("SyntaxError", "Expected a list for 'body' in scope statement");
                return NULL;
            }
        };
    
        let name = match statement.get(&Value::String("name".to_string())) {
            Some(Value::String(n)) => n.clone(),
            None => self.scope.clone(),
            _ => {
                self.raise("SyntaxError", "Expected a string for 'name' in scope statement");
                return NULL;
            }
        };

        let locals = match statement.get(&Value::String("locals".to_string())) {
            Some(Value::List(locals_list)) => locals_list,
            _ => {
                &vec![]
            }
        };

        let stmts: Vec<Statement> = body.iter()
            .filter_map(|v| {
                match v {
                    Value::Map { .. } => Some(v.convert_to_statement()),
                    Value::Null => Some(Statement::Null),
                    _ => {
                        self.raise("RuntimeError", "Invalid value in 'scope' body - expected map");
                        None
                    }
                }
            })
            .collect();

        let new_file_path = if statement.get(&Value::String("name".to_string())).is_some() {
            self.file_path.clone() + &format!("+scope.{}", name)
        } else {
            self.file_path.clone()
        };


        self.stack.push((
            new_file_path.clone(),
            self.get_location_from_current_statement(),
            StackType::Scope,
        ));

        debug_log(
            &format!("<Entering scope '{}'>", name),
            &self.config.clone(),
            Some(self.use_colors),
        );

        let mut scope_interpreter = Interpreter::new(
            self.config.clone(),
            self.use_colors,
            &new_file_path,
            &self.cwd.clone(),
            self.preprocessor_info.clone(),
            &[]
        );
        scope_interpreter.set_scope(&name.clone());

        for local in locals.iter() {
            if let Value::String(var_name) = local {
                if let Some(val) = self.variables.get(var_name) {
                    scope_interpreter.variables.insert(var_name.clone(), val.clone());
                } else {
                    self.stack.pop();
                    self.raise(
                        "NameError",
                        &format!("Variable '{}' is not defined in the outer scope", var_name),
                    );
                    return NULL;
                }
            }
        }

        if body.is_empty() {
            self.stack.pop();
            debug_log(
                &format!("<Exiting scope '{}'>", name),
                &self.config.clone(),
                Some(self.use_colors),
            );
            return NULL;
        }

        let _ = scope_interpreter.interpret(stmts, true);

        if let Some(err) = scope_interpreter.err {
            self.stack.pop();
            self.raise_with_ref(
                "RuntimeError",
                &format!("Error in scope '{}'", name),
                err,
            );
            return NULL;
        }

        debug_log(
            &format!("<Exiting scope '{}'>", name),
            &self.config.clone(),
            Some(self.use_colors),
        );

        self.stack.pop();
        if scope_interpreter.is_returning {
            return scope_interpreter.return_value.clone();
        }
        NULL
    }

    fn handle_defer(&mut self, statement: HashMap<Value, Value>) -> Value {
        let body = match statement.get(&Value::String("body".to_string())) {
            Some(Value::List(b)) => b,
            _ => {
                self.raise("RuntimeError", "Expected a list for 'body' in defer statement");
                return NULL;
            }
        };
    
        if body.is_empty() {
            return NULL;
        }
    
        let stmts: Vec<Statement> = body.iter()
            .filter_map(|v| {
                match v {
                    Value::Map { .. } => Some(v.convert_to_statement()),
                    Value::Null => Some(Statement::Null),
                    _ => {
                        self.raise("RuntimeError", "Invalid value in 'defer' body - expected map");
                        None
                    }
                }
            })
            .collect();
    
        self.defer_stack.push(stmts);
    
        NULL
    }

    fn handle_continue_and_break(&mut self, statement: HashMap<Value, Value>) -> Value {
        let control_type = match statement.get(&Value::String("type".to_string())) {
            Some(Value::String(t)) => t,
            _ => {
                self.raise("SyntaxError", "Expected 'type' to be a string in continue/break statement");
                return NULL;
            }
        };
    
        if control_type == "CONTINUE" {
            self.state = State::Continue;
            return NULL;
        } else if control_type == "BREAK" {
            self.state = State::Break;
            return NULL;
        } else {
            self.raise("SyntaxError", &format!("Unsupported control type: {}", control_type));
            return NULL;
        }
    }

    fn handle_while(&mut self, statement: HashMap<Value, Value>) -> Value {
        let condition = match statement.get(&Value::String("condition".to_string())) {
            Some(v) => v,
            _ => {
                self.raise("SyntaxError", "Missing 'condition' in while loop");
                return NULL;
            }
        };
    
        let body = match statement.get(&Value::String("body".to_string())) {
            Some(Value::List(b)) => b,
            _ => {
                self.raise("SyntaxError", "Expected a list for 'body' in while loop");
                return NULL;
            }
        };
    
        while self.evaluate(condition.convert_to_statement()).is_truthy() {
            for stmt in body.iter() {
                let result = self.evaluate(stmt.convert_to_statement());
                if self.err.is_some() {
                    return NULL;
                }
                
                if self.is_returning {
                    return result;
                }
            }
            match self.state {
                State::Break => {
                    self.state = State::Normal;
                    break;
                },
                State::Continue => {
                    self.state = State::Normal;
                    continue;
                },
                _ => {},
            }
        }
    
        NULL
    }
    
    fn handle_compound_assignment(&mut self, statement: HashMap<Value, Value>) -> Value {
        let target_str = match statement.get(&Value::String("target".to_string())) {
            Some(Value::String(s)) => s.clone(),
            _ => {
                self.raise("SyntaxError", "Expected a string for 'target' in compound assignment");
                return NULL;
            }
        };
    
        let value_expr = match statement.get(&Value::String("value".to_string())) {
            Some(v) => v,
            _ => {
                self.raise("SyntaxError", "Missing 'value' in compound assignment");
                return NULL;
            }
        };
    
        let operator = match statement.get(&Value::String("operator".to_string())) {
            Some(Value::String(op)) => op.clone(),
            _ => {
                self.raise("SyntaxError", "Expected a string for 'operator' in compound assignment");
                return NULL;
            }
        };
    
        let rhs_value = self.evaluate(value_expr.convert_to_statement());
        if self.err.is_some() {
            return NULL;
        }
    
        let (is_final, lhs_value) = match self.variables.get(to_static(target_str.clone())) {
            Some(var) => (var.is_final(), var.value.clone()),
            None => {
                self.raise("NameError", &format!("Variable '{}' is not defined", target_str));
                return NULL;
            }
        };
    
        if is_final {
            self.raise("PermissionError", &format!("Variable '{}' is final and cannot be modified", target_str));
            return NULL;
        }

        let operator = match operator.as_str() {
            "+=" => "+".to_string(),
            "-=" => "-".to_string(),
            "*=" => "*".to_string(),
            "/=" => "/".to_string(),
            "%=" => "%".to_string(),
            "^=" => "^".to_string(),
            _ => {
                self.raise("SyntaxError", &format!("Unsupported compound assignment operator: {}", operator));
                return NULL;
            }
        };
    
        let result = self.make_operation(lhs_value, rhs_value, &operator);
        if self.err.is_some() {
            return NULL;
        }
    
        if let Some(var_mut) = self.variables.get_mut(to_static(target_str.clone())) {
            var_mut.set_value(result.clone());
        }
    
        if self.err.is_some() {
            return NULL;
        }
    
        result
    }

    fn handle_pointer(&mut self, statement: HashMap<Value, Value>) -> Value {
        let pointer_type = statement.get(&Value::String("type".to_string())).unwrap_or(&NULL);
        let value_opt = statement.get(&Value::String("value".to_string())).unwrap_or(&NULL);

        if *pointer_type == Value::String("POINTER_ASSIGN".to_string()) {
            if self.err.is_some() {
                self.err = None;
            }
        }

        let value = self.evaluate(value_opt.convert_to_statement());

        if !self.config.allow_unsafe {
            return self.raise_with_help(
                "PermissionError",
                "Pointer operations are not allowed in this context",
                "Enable 'allow_unsafe' in the configuration to use pointers.",
            );
        }

        if self.err.is_some() {
            return NULL;
        }

        match pointer_type {
            Value::String(t) if t == "POINTER_REF" => {
                Value::Pointer(Arc::new(value))
            }

            Value::String(t) if t == "POINTER_DEREF" => {
                if let Value::Pointer(ptr_arc) = value {
                    (*ptr_arc).clone()
                } else {
                    self.raise("TypeError", "Expected a pointer reference for dereferencing");
                    NULL
                }
            }

            Value::String(t) if t == "POINTER_ASSIGN" => {
                let left = self.evaluate(
                    statement
                        .get(&Value::String("left".to_string()))
                        .unwrap_or(&NULL)
                        .convert_to_statement(),
                );
                let right = self.evaluate(
                    statement
                        .get(&Value::String("right".to_string()))
                        .unwrap_or(&NULL)
                        .convert_to_statement(),
                );

                if self.err.is_some() {
                    return NULL;
                }

                if let Value::Pointer(ref ptr_arc) = left {
                    let raw_ptr = Arc::as_ptr(&ptr_arc) as *mut Value;

                    unsafe {
                        raw_ptr.write(right.clone());
                    }

                    left
                } else {
                    self.raise("TypeError", "Expected pointer reference for assignment");
                    NULL
                }
            }

            _ => {
                self.raise("SyntaxError", "Invalid pointer type");
                NULL
            }
        }
    }

    fn handle_unpack_assignment(&mut self, statement: HashMap<Value, Value>) -> Value {
        let targets_opt = statement.get(&Value::String("targets".to_string())).unwrap_or_else(|| {
            self.raise("RuntimeError", "Missing 'targets' in unpack assignment");
            &NULL
        });
        let value_opt = statement.get(&Value::String("value".to_string())).unwrap_or_else(|| {
            self.raise("RuntimeError", "Missing 'value' in unpack assignment");
            &NULL
        });

        if self.err.is_some() {
            return NULL;
        }
        
        let value = self.evaluate(value_opt.convert_to_statement());

        if self.err.is_some() {
            return NULL;
        }

        let targets = match targets_opt {
            Value::List(target_list) => target_list,
            _ => {
                self.raise("RuntimeError", "Unpack assignment targets expected to be a list");
                return NULL;
            }
        };

        let value_list: Vec<Value> = match value {
            Value::List(target_list) => {
                if target_list.is_empty() {
                    self.raise("SyntaxError", "Unpack assignment target cannot be an empty list");
                    return NULL;
                }
                target_list.clone()
            },
            Value::Tuple(target_tuple) => {
                if target_tuple.is_empty() {
                    self.raise("SyntaxError", "Unpack assignment target cannot be an empty tuple");
                    return NULL;
                }
                target_tuple.clone()
            },
            Value::String(target_str) => {
                if target_str.is_empty() {
                    self.raise("SyntaxError", "Unpack assignment target cannot be an empty string");
                    return NULL;
                }
                if targets.len() != target_str.len() {
                    vec!(Value::String(target_str))
                } else {
                    target_str.chars()
                        .map(|c| Value::String(c.to_string()))
                        .collect::<Vec<Value>>()
                }
            },
            Value::Map { keys, values } => {
                if keys.is_empty() || values.is_empty() {
                    self.raise("SyntaxError", "Unpack assignment target cannot be an empty map");
                    return NULL;
                }
                keys.iter()
                    .cloned()
                    .zip(values.iter().cloned())
                    .map(|(k, v)| Value::Map {
                        keys: vec![k],
                        values: vec![v],
                    })
                    .collect()
            },
            Value::Bytes(bytes) => {
                if bytes.is_empty() {
                    self.raise("SyntaxError", "Unpack assignment target cannot be an empty bytes");
                    return NULL;
                }
                bytes
                    .iter()
                    .map(|b| Value::Int(Int::from_i64(*b as i64)))
                    .collect()
            },
            _ => {
                self.raise("TypeError", "Unpack assignment target must be a list, tuple, string, map or bytes");
                return NULL;
            }
        };

        if targets.len() != value_list.len() {
            self.raise("ValueError", "Unpack assignment targets and value must have the same length");
            return NULL;
        }

        let mut result_values = vec![];

        for (i, target) in targets.iter().enumerate() {
            let val = value_list[i].clone();
        
            match target {
                Value::Map { keys, values } => {
                    let mut map: HashMap<String, Value> = HashMap::new();
                    for (k, v) in keys.iter().zip(values.iter()) {
                        if let Value::String(key_str) = k {
                            map.insert(key_str.clone(), v.clone());
                        } else {
                            self.raise("TypeError", "Invalid key in unpack target map");
                            return NULL;
                        }
                    }
        
                    let typ = map.get("type");
                    let name = map.get("name");
        
                    match typ {
                        Some(Value::String(t)) if t == "VARIABLE" => {
                            if let Some(Value::String(var_name)) = name {
                                if let Some(var) = self.variables.get_mut(var_name) {
                                    var.set_value(val);
                                    result_values.push(var.value.clone());
                                } else {
                                    self.raise("TypeError", &format!(
                                        "Variable '{}' must be declared with a type in unpack assignment",
                                        var_name
                                    ));
                                    return NULL;
                                }
                            } else {
                                self.raise("TypeError", "Missing or invalid variable name in unpack target");
                                return NULL;
                            }
                        }
                        Some(Value::String(t)) if t == "VARIABLE_DECLARATION" => {
                            let decl_map: HashMap<Value, Value> = keys
                                .iter()
                                .cloned()
                                .zip(values.iter().cloned())
                                .collect();

                            let type_ = match decl_map.get(&Value::String("var_type".to_string())) {
                                Some(t) => self.evaluate(t.convert_to_statement()),
                                _ => {
                                    self.raise("TypeError", "Missing or invalid 'var_type' in variable declaration");
                                    return NULL;
                                }
                            };

                            if type_ != "auto".into() && !self.check_type(&val, &type_, false) {
                                self.raise("TypeError", &format!(
                                    "Value '{}' does not match the type '{}'",
                                    val, type_
                                ));
                                return NULL;
                            }
                            
                            if self.err.is_some() {
                                return NULL;
                            }

                            let _ = self.handle_variable_declaration(decl_map);
                            if self.err.is_some() {
                                return NULL;
                            }

                            if let Some(Value::String(var_name)) = map.get("name") {
                                if let Some(var) = self.variables.get_mut(var_name) {
                                    var.set_value(val.clone());
                                }
                            }
                        
                            result_values.push(val);
                        }                        
                        _ => {
                            self.raise("TypeError", "Unpack target must be of type VARIABLE or VARIABLE_DECLARATION");
                            return NULL;
                        }
                    }
                }
                _ => {
                    self.raise("TypeError", "Unpack target must be a map");
                    return NULL;
                }
            }
        }        
        
        if self.err.is_some() {
            return NULL;
        }

        Value::Tuple(result_values)
    }

    fn handle_map(&mut self, statement: HashMap<Value, Value>) -> Value {
        let keys_opt = statement.get(&Value::String("keys".to_string())).unwrap_or_else(|| {
            self.raise("RuntimeError", "Missing 'keys' in map statement");
            &NULL
        });
        let values_opt = statement.get(&Value::String("values".to_string())).unwrap_or_else(|| {
            self.raise("RuntimeError", "Missing 'values' in map statement");
            &NULL
        });
    
        let raw_keys = match keys_opt {
            Value::List(v) => v,
            _ => {
                self.raise("TypeError", "Expected list for map keys");
                return NULL;
            }
        };
    
        let raw_values = match values_opt {
            Value::List(v) => v,
            _ => {
                self.raise("TypeError", "Expected list for map values");
                return NULL;
            }
        };
    
        if raw_keys.len() != raw_values.len() {
            self.raise("RuntimeError", "Keys and values lists must have the same length");
            return NULL;
        }
    
        let mut keys = Vec::with_capacity(raw_keys.len());
        let mut values = Vec::with_capacity(raw_values.len());
    
        let mut seen = std::collections::HashSet::new();
    
        for (i, raw_key_entry) in raw_keys.iter().enumerate() {
            let (modifier, raw_key) = match raw_key_entry {
                Value::Map { keys: mk, values: mv } => {
                    let mut modifier = None;
                    let mut key = None;
                    for (k, v) in mk.iter().zip(mv.iter()) {
                        match k {
                            Value::String(s) if s == "modifier" => modifier = Some(v),
                            Value::String(s) if s == "key" => key = Some(v),
                            _ => {}
                        }
                    }
                    match (modifier, key) {
                        (Some(m), Some(k)) => (m.clone(), k.clone()),
                        _ => {
                            self.raise("RuntimeError", "Malformed key map entry (missing 'modifier' or 'key')");
                            return NULL;
                        }
                    }
                }
                _ => {
                    self.raise("TypeError", "Map key entry must be a map with 'modifier' and 'key'");
                    return NULL;
                }
            };

            if modifier != Value::Null && modifier == Value::String("final".to_string()) {
                self.raise("NotImplemented", "Final modifier for keys in maps is not implemented yet");
                return NULL;
            }
            
            let evaluated_key = self.evaluate(raw_key.convert_to_statement());
            let evaluated_value = self.evaluate(raw_values[i].convert_to_statement());
    
            if !seen.insert(evaluated_key.clone()) {
                self.raise("SyntaxError", &format!("Duplicate key in map: {}", evaluated_key));
                return NULL;
            }
            
            keys.push(evaluated_key);
            values.push(evaluated_value);
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
            Value::Map { keys, values } => {
                let hm = keys.iter().cloned().zip(values.iter().cloned()).collect::<HashMap<_, _>>();
                let name = match hm.get(&Value::String("name".to_string())) {
                    Some(Value::String(n)) => n.clone(),
                    _ => return self.raise("RuntimeError", "Missing 'name' in type conversion map"),
                };
                let base = match hm.get(&Value::String("base".to_string())) {
                    Some(b) => b.clone(),
                    _ => return self.raise("RuntimeError", "Missing 'base' in type conversion map"),
                };
                let variables = match hm.get(&Value::String("variables".to_string())) {
                    Some(Value::List(v)) => v.clone(),
                    _ => return self.raise("RuntimeError", "Missing 'variables' in type conversion map"),
                };
                let conditions = match hm.get(&Value::String("conditions".to_string())) {
                    Some(Value::List(c)) => c.clone(),
                    _ => return self.raise("RuntimeError", "Missing 'conditions' in type conversion map"),
                };
                let base_stmt = match base {
                    Value::String(s) => {
                        Statement::Statement {
                            keys: vec![
                                Value::String("type".to_string()),
                                Value::String("type_kind".to_string()),
                                Value::String("value".to_string())
                            ],
                            values: vec![
                                Value::String("TYPE".to_string()),
                                Value::String("simple".to_string()),
                                Value::String(s)
                            ],
                            loc: self.get_location_from_current_statement(),
                        }
                    },
                    Value::Map { keys, values } => {
                        Statement::Statement {
                            keys: keys.clone(),
                            values: values.clone(),
                            loc: self.get_location_from_current_statement(),
                        }
                    },
                    _ => return self.raise("RuntimeError", "Invalid 'base' in type conversion map"),
                };
                if self.err.is_some() {
                    return NULL;
                }
                let s = Statement::Statement {
                    keys: vec![
                        Value::String("type".to_string()),
                        Value::String("value".to_string()),
                        Value::String("to".to_string()),
                    ],
                    values: vec![
                        Value::String("TYPE_CONVERSION".to_string()),
                        value_opt.clone(),
                        base_stmt.convert_to_map(),
                    ],
                    loc: self.get_location_from_current_statement(),
                }.convert_to_hashmap();
                let converted_value = self.handle_type_conversion(s);
                if self.err.is_some() {
                    return NULL;
                }
                let mut new_interpreter = Interpreter::new(
                    self.config.clone(),
                    self.use_colors,
                    &self.file_path,
                    &self.cwd,
                    self.preprocessor_info.clone(),
                    &[],
                );
                new_interpreter.set_scope(&format!("{}+scope.{}", self.scope, name));
                if variables.len() > 0 {
                    new_interpreter.variables.insert(
                        variables[0].to_string(),
                        Variable::new(
                            variables[0].to_string(),
                            converted_value.clone(),
                            converted_value.type_name().to_string(),
                            false,
                            true,
                            true,
                        ),
                    );
                }
                let mut status: (bool, usize) = (true, 0);
                for (i, cond) in conditions.iter().enumerate() {
                    new_interpreter.current_statement = Some(cond.convert_to_statement());
                    let result = new_interpreter.evaluate(cond.convert_to_statement());
                    if !result.is_truthy() {
                        status = (false, i + 1);
                        break;
                    }
                }

                if !status.0 {
                    self.raise("TypeError", &format!(
                        "Conditions for type '{}' not met condition #{} in type conversion",
                        name,
                        status.1
                    ));
                    return NULL;
                }
                return converted_value;
            }
            _ => return self.raise("RuntimeError", &format!("Type '{}' is not a valid target type for conversion", value.type_name())),
        };

        if self.err.is_some() {
            return NULL;
        }

        let mut handle_type_conversion = |target_type: String, value: &Value| -> Value {
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
                            let rounded = f.round(0);
                            rounded.to_int().map_or_else(
                                |_| self.raise("ConversionError", "Failed to convert rounded float to int"),
                                |i| Value::Int(i),
                            )
                        }
                    } else if let Value::String(s) = value {
                        let t = Statement::Statement {
                            keys: vec![Value::String("type".to_string()), Value::String("value".to_string())],
                            values: vec![Value::String("NUMBER".to_string()), Value::String(s.clone())],
                            loc: self.get_location_from_current_statement(),
                        };
                        let result = self.evaluate(t);
                        if self.err.is_some() {
                            return NULL;
                        }
                        if let Value::Int(_) = result {
                            return result;
                        } else {
                            self.raise("ConversionError", &format!("Failed to convert string '{}' to int", s.to_string()));
                            NULL
                        }
                    } else if let Value::Boolean(b) = value {
                        if *b {
                            Value::Int(Int::from_i64(1))
                        } else {
                            Value::Int(Int::from_i64(0))
                        }
                    } else {
                        self.raise("TypeError", &format!("Cannot convert '{}' to int", value.type_name()))
                    }
                }
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
                    } else if let Value::Generator(generator) = value {
                        if !generator.is_infinite() {
                            let v = generator.to_vec();
                            if let Some(Value::Error(err_type, err_msg, ref_err)) = v.iter().find(|item| matches!(item, Value::Error(..))) {
                                self.err = Some(match ref_err {
                                    Some(re) => Error::with_ref(err_type, err_msg, re.clone(), &self.file_path),
                                    None => Error::new(err_type, err_msg, &self.file_path),
                                });
                                return NULL;
                            }
                            Value::List(v)
                        } else {
                            self.raise("TypeError", "Cannot convert infinite generator to list");
                            NULL
                        }
                    } else {
                        self.raise("TypeError", &format!("Cannot convert '{}' to list", value.type_name()))
                    }
                },
                "map" => {
                    if let Value::Map { keys, values } = value {
                        Value::Map { keys: keys.clone(), values: values.clone() }
                    } else if let Value::Module(obj, _) = value {
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
                        Value::Bytes(s.clone().into_bytes())
                    } else {
                        self.raise("TypeError", &format!("Cannot convert '{}' to bytes", value.type_name()))
                    }
                },
                "object" => {
                    if let Value::Module(obj, path) = value {
                        Value::Module(obj.clone(), path.clone())
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
        };

        if target_type.starts_with("&") {
            let new_type = target_type.trim_start_matches('&').to_string();
            if !VALID_TYPES.contains(&new_type.as_str()) {
                return self.raise("TypeError", &format!("Invalid target type '{}'", target_type));
            }
            let new_value = handle_type_conversion(new_type, &value);
            if self.err.is_some() {
                return NULL;
            }
            return Value::Pointer(Arc::new(new_value));
        }
        if target_type.starts_with("?") {
            let new_type = target_type.trim_start_matches('?').to_string();
            if !VALID_TYPES.contains(&new_type.as_str()) {
                return self.raise("TypeError", &format!("Invalid target type '{}'", target_type));
            }
            if value.is_null() {
                return value.clone();
            }
            let new_value = handle_type_conversion(new_type, &value);
            if self.err.is_some() {
                return NULL;
            }
            return new_value;
        }

        if !VALID_TYPES.contains(&target_type.as_str()) {
            return self.raise("TypeError", &format!("Invalid target type '{}'", target_type));
        }

        return handle_type_conversion(target_type, &value);
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

        if module_name == "42" {
            if !self.internal_storage.use_42.0 {
                self.internal_storage.use_42 = (true, false);
            }
            self.raise_with_help(
                "ImportError",
                "What do you get if you multiply six by nine?",
                "Six by nine. Forty two."
            );
            return NULL;
        }

        let alias = match statement.get(&Value::String("alias".to_string())) {
            Some(Value::String(a)) => a,
            Some(Value::Null) => &module_name,
            _ => {
                self.raise("RuntimeError", "Missing or invalid 'alias' in import statement");
                return NULL;
            },
        };

        let named_imports = match statement.get(&Value::String("named".to_string())) {
            Some(Value::List(l)) => l.clone(),
            Some(Value::Null) => Vec::new(),
            _ => {
                self.raise("RuntimeError", "Missing or invalid 'named' in import statement");
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

        if self.stack.iter().any(|(name, _, type_)| name == module_name && type_ == StackType::Import) {
            return self.raise(
                "RecursionError",
                &format!("Recursive import detected for module '{}'", module_name),
            );
        }

        let modifiers = match statement.get(&Value::String("modifiers".to_string())) {
            Some(Value::List(l)) => l.clone(),
            Some(Value::Null) => Vec::new(),
            _ => {
                self.raise("RuntimeError", "Missing or invalid 'modifiers' in import statement");
                return NULL;
            },
        };

        let mut is_public = false;
        let mut is_final = true;
        let mut is_static = true;

        for modifier in &modifiers {
            if let Value::String(modifier_str) = modifier {
                match modifier_str.as_str() {
                    "public" => is_public = true,
                    "final" => is_final = true,
                    "static" => is_static = true,
                    "private" => is_public = false,
                    "mutable" => is_final = false,
                    "non-static" => is_static = false,
                    _ => {
                        self.raise("SyntaxError", &format!("Unknown import modifier '{}'", modifier_str));
                        return NULL;
                    }
                }
            } else {
                self.raise("TypeError", "Import modifiers must be strings");
                return NULL;
            }
        }
    
        self.stack.push((
            module_name.clone(),
            self.get_location_from_current_statement(),
            StackType::Import
        ));
    
        if self.variables.contains_key(alias) && named_imports.is_empty() {
            self.stack.pop();
            if let Some(var) = self.variables.get(alias) {
                return var.value.clone();
            } else {
                return self.raise("ImportError", &format!("Module '{}' is already imported but not found in the current context", alias));
            }
        }
    
        let mut properties = HashMap::new();
        let mut module_path = PathBuf::from(self.config.home_dir.clone()).join("libs").join(&module_name);

        if let Some(lib_info) = STD_LIBS.get(module_name.as_str()) {
            debug_log(&format!("<Loading standard library module '{}', version {}, description: {}>", module_name, lib_info.version, lib_info.description), &self.config, Some(self.use_colors));

            let expected_lucia_version = lib_info.expected_lucia_version;

            if !expected_lucia_version.is_empty() && !check_version(&self.config.version, &expected_lucia_version) {
                self.stack.pop();
                return self.raise_with_help(
                    "ImportError",
                    &format!("Module '{}' requires Lucia version '{}', but current version is '{}'", module_name, expected_lucia_version, self.config.version),
                    &format!("Please update Lucia to match the required version: '{}'", expected_lucia_version),
                );
            }            

            match module_name.as_str() {
                "math" => {
                    use crate::env::libs::math::__init__ as math;
                    let math_module_props = math::register();
                    for (name, var) in math_module_props {
                        properties.insert(name, var);
                    }
                }
                "os" => {
                    use crate::env::libs::os::__init__ as os;
                    let os_module_props = os::register(&self.config.clone());
                    for (name, var) in os_module_props {
                        properties.insert(name, var);
                    }
                }
                "time" => {
                    use crate::env::libs::time::__init__ as time;
                    let time_module_props = time::register();
                    for (name, var) in time_module_props {
                        properties.insert(name, var);
                    }
                }
                "json" => {
                    use crate::env::libs::json::__init__ as json;
                    let json_module_props = json::register();
                    for (name, var) in json_module_props {
                        properties.insert(name, var);
                    }
                }
                "config" => {
                    use crate::env::libs::config::__init__ as config;
                    let arc_config = Arc::new(self.config.clone());
                    let config_module_props = config::register(arc_config);
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
                    for (name, var) in clib_module_props {
                        properties.insert(name, var);
                    }
                }
                "regex" => {
                    use crate::env::libs::regex::__init__ as regex;
                    let regex_module_props = regex::register();
                    for (name, var) in regex_module_props {
                        properties.insert(name, var);
                    }
                }
                "collections" => {
                    use crate::env::libs::collections::__init__ as collections;
                    let collections_module_props = collections::register();
                    for (name, var) in collections_module_props {
                        properties.insert(name, var);
                    }
                }
                "random" => {
                    use crate::env::libs::random::__init__ as random;
                    let random_module_props = random::register();
                    for (name, var) in random_module_props {
                        properties.insert(name, var);
                    }
                }
                "lasm" => {
                    use crate::env::libs::lasm::__init__ as lasm;
                    let lasm_module_props = lasm::register();
                    for (name, var) in lasm_module_props {
                        properties.insert(name, var);
                    }
                }
                "fs" => {
                    use crate::env::libs::fs::__init__ as fs;
                    let fs_module_props = fs::register();
                    for (name, var) in fs_module_props {
                        properties.insert(name, var);
                    }
                }
                "nest" => {
                    use crate::env::libs::nest::__init__ as nest;
                    let arc_config = Arc::new(self.config.clone());
                    let module_path = PathBuf::from(self.config.home_dir.clone()).join("libs").join("nest").join("__init__.rs").display().to_string();
                    let result = nest::init_nest(arc_config, module_path);
                    if let Err(e) = result {
                        self.stack.pop();
                        return self.raise_with_ref(
                            "ImportError",
                            "Failed to initialize nest module",
                            e,
                        );
                    }
                    let interpreter_arc = Arc::new(Mutex::new(&mut *self));
                    let nest_module_props = nest::register(interpreter_arc);
                    for (name, var) in nest_module_props {
                        properties.insert(name, var);
                    }
                }
                "libload" => {
                    use crate::env::libs::libload::__init__ as libload;
                    let arc_config = Arc::new(self.config.clone());
                    let module_path = PathBuf::from(self.config.home_dir.clone()).join("libs").join("libload").join("__init__.rs").display().to_string();
                    let result = libload::init_libload(arc_config, module_path);
                    if let Err(e) = result {
                        self.stack.pop();
                        return self.raise_with_ref(
                            "ImportError",
                            "Failed to initialize libload module",
                            e,
                        );
                    }
                    let libload_module_props = libload::register();
                    for (name, var) in libload_module_props {
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
    
            module_path = resolved_module_path.unwrap();
    
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
                let manifest_path = module_path.join("manifest.json");
                if manifest_path.exists() {
                    let manifest_file = fs::File::open(&manifest_path);
                    if let Ok(file) = manifest_file {
                        let reader = std::io::BufReader::new(file);
                        let manifest_json = match serde_json::from_reader::<_, serde_json::Value>(reader) {
                            Ok(json) => json,
                            Err(_) => {
                                self.stack.pop();
                                return self.raise("MalformedManifest", &format!("Manifest file '{}' is not valid JSON", fix_path(manifest_path.display().to_string())));
                            }
                        };
                        
                        let print_name = manifest_json.get("name").and_then(|v| v.as_str()).unwrap_or(&module_name);
                        let version = manifest_json.get("version").and_then(|v| v.as_str()).unwrap_or("unknown");
                        let required_lucia_version = manifest_json.get("required_lucia_version").and_then(|v| v.as_str()).unwrap_or(VERSION);
                        let description = manifest_json.get("description").and_then(|v| v.as_str()).unwrap_or("");
                        let authors = manifest_json.get("authors").and_then(|v| v.as_array()).map_or(vec![], |arr| arr.iter().filter_map(|v| v.as_str().map(String::from)).collect::<Vec<_>>());
                        let license = manifest_json.get("license").and_then(|v| v.as_str()).unwrap_or("GPLv3");
                        
                        debug_log(
                            &format!(
                                "<Manifest for module '{}': version {}, description: {}, authors: {:?}, license: {}>",
                                print_name, version, description, authors, license
                            ),
                            &self.config,
                            Some(self.use_colors)
                        );

                        for (field, _) in [("name", &print_name), ("version", &version), ("required_lucia_version", &required_lucia_version)] {
                            if manifest_json.get(field).is_some() && !manifest_json.get(field).unwrap().is_string() {
                                self.stack.pop();
                                return self.raise("MalformedManifest", &format!("Field '{}' in manifest '{}' must be a string", field, fix_path(manifest_path.display().to_string())));
                            }
                        }
                
                        if !check_version(&self.config.version, required_lucia_version) {
                            self.stack.pop();
                            return self.raise_with_help(
                                "ImportError",
                                &format!("Module '{}' requires Lucia version '{}', but current version is '{}'", print_name, required_lucia_version, self.config.version),
                                &format!("Please update Lucia to match the required version: '{}'", required_lucia_version),
                            );
                        }
                        
                        debug_log(
                            &format!(
                                "<Importing module '{}', version: {}{}>",
                                print_name,
                                version,
                                if description.is_empty() { "".to_string() } else { format!(", description: {}", description) }
                            ),
                            &self.config,
                            Some(self.use_colors)
                        );
                        
                        if let Some(deps) = manifest_json.get("dependencies").and_then(|v| v.as_object()) {
                            for (dep_name, dep_version_value) in deps {
                                let required_version = match dep_version_value.as_str() {
                                    Some(v) => v,
                                    None => {
                                        self.stack.pop();
                                        return self.raise("ImportError", &format!(
                                            "Invalid version format for dependency '{}'",
                                            dep_name
                                        ));
                                    }
                                };
                        
                                if let Some(std_lib) = STD_LIBS.get(dep_name.as_str()) {
                                    if !check_version(VERSION, &std_lib.expected_lucia_version) {
                                        self.stack.pop();
                                        return self.raise("ImportError", &format!(
                                            "Standard library '{}' requires Lucia version '{}', but incompatible.",
                                            dep_name,
                                            std_lib.expected_lucia_version
                                        ));
                                    }

                                    if !check_version(&std_lib.version, required_version) {
                                        self.stack.pop();
                                        return self.raise("ImportError", &format!(
                                            "Standard library '{}' version '{}' does not satisfy required '{}'",
                                            dep_name, std_lib.version, required_version
                                        ));
                                    }
                        
                                    continue;
                                }
                        
                                let dep_path = libs_dir.join(dep_name);
                                if !dep_path.exists() {
                                    self.stack.pop();
                                    return self.raise("ImportError", &format!(
                                        "Dependency '{}' listed in manifest not found in '{}'",
                                        dep_name,
                                        module_path.display()
                                    ));
                                }
                        
                                let dep_manifest_path = dep_path.join("manifest.json");
                                let dep_manifest_json = match std::fs::read_to_string(&dep_manifest_path)
                                    .ok()
                                    .and_then(|content| serde_json::from_str::<serde_json::Value>(&content).ok())
                                {
                                    Some(json) => json,
                                    None => {
                                        self.stack.pop();
                                        return self.raise("ImportError", &format!(
                                            "Failed to read manifest for dependency '{}'",
                                            dep_name
                                        ));
                                    }
                                };
                        
                                let current_version = match dep_manifest_json.get("version").and_then(|v| v.as_str()) {
                                    Some(v) => v,
                                    None => {
                                        self.stack.pop();
                                        return self.raise("ImportError", &format!(
                                            "No version field found in manifest for dependency '{}'",
                                            dep_name
                                        ));
                                    }
                                };
                        
                                if !check_version(current_version, required_version) {
                                    self.stack.pop();
                                    return self.raise("ImportError", &format!(
                                        "Dependency '{}' version '{}' does not satisfy required '{}'",
                                        dep_name, current_version, required_version
                                    ));
                                }
                            }
                        }
                
                        if let Some(config) = manifest_json.get("config").and_then(|v| v.as_object()) {
                            for (key, expected_json_value) in config.iter() {
                                let expected_value = Value::from_json(expected_json_value);
                                let actual_value = get_from_config(&self.config, key);
                                
                                if actual_value != expected_value {
                                    self.stack.pop();
                                    return self.raise("ImportError", &format!(
                                        "Config key '{}' value mismatch. Expected: {}, Found: {}",
                                        key,
                                        format_value(&expected_value),
                                        format_value(&actual_value)
                                    ));
                                }
                            }
                        }                            
                    } else {
                        self.stack.pop();
                        return self.raise("MalformedManifest", &format!("Could not open manifest file '{}'", fix_path(manifest_path.display().to_string())));
                    }
                } else {
                    debug_log(&format!("<Importing module '{}'>", module_name), &self.config, Some(self.use_colors));
                }                
                
                let init_path_lc = module_path.join("__init__.lc");
                let init_path_lucia = module_path.join("__init__.lucia");

                let has_init = (init_path_lc.exists() && init_path_lc.is_file()) || (init_path_lucia.exists() && init_path_lucia.is_file());

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
                                } else if !has_init {
                                    if ext != "lc" && ext != "lucia" {
                                        self.stack.pop();
                                        return self.raise("ImportError", &format!("Unsupported file extension '{}'", ext));
                                    }
                                } else {
                                    continue;
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
                Value::Module(..) => "object",
                Value::Function(_) if var.is_public() => "function",
                _ if var.is_final() => "constant",
                _ => "variable",
            };
            categorized.entry(category).or_default().push(name);
        }
    
        let order = ["object", "function", "constant", "variable"];

        if !named_imports.is_empty() {
            let mut names: HashMap<String, String> = HashMap::new();
            for named_import in named_imports {
                let named_import_hashmap = named_import.convert_to_hashmap().unwrap_or_else(|| {
                    self.stack.pop();
                    self.raise("TypeError", "Expected a map for named import");
                    return HashMap::new();
                });
                let name = named_import_hashmap
                    .get("name")
                    .unwrap_or(&Value::Null)
                    .to_string();

                let alias = named_import_hashmap
                    .get("alias")
                    .map(|v| v.to_string())
                    .unwrap_or_else(|| name.clone());

                if let Some(var) = properties.get(&name) {
                    self.variables.insert(
                        alias.to_string(),
                        Variable::new(alias.to_string(), var.value.clone(), var.type_name().to_string(), var.is_final(), var.is_public(), var.is_static()),
                    );
                    names.insert(name.clone(), alias.clone());
                } else {
                    self.stack.pop();
                    return self.raise("ImportError", &format!("Variable '{}' not found in module '{}'", name, module_name));
                }
            }
            for &category in &order {
                if let Some(n) = categorized.get(category) {
                    for name in n {
                        if !names.contains_key(name.as_str()) {
                            continue;
                        }
                        if let Some(alias) = names.get(name.as_str()) {
                            debug_log(&format!("<Importing {} '{}' from '{}' as '{}'>", category, name, module_name, alias), &self.config, Some(self.use_colors.clone()));
                        } else {
                            debug_log(&format!("<Importing {} '{}' from module '{}'>", category, name, module_name), &self.config, Some(self.use_colors.clone()));
                        }
                    }
                }
            }
            if self.err.is_some() {
                self.stack.pop();
                return NULL;
            }
            Value::Null
        } else {
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
                is_public,
                is_static,
                is_final,
                state: None,
            };
        
            let class = Class::new(module_name.clone(), module_meta.clone());
            let object = Object::Class(class);
        
            let module = Value::Module(object, PathBuf::from(module_path.clone()));
            self.variables.insert(
                alias.to_string(),
                Variable::new(alias.to_string(), module.clone(), "module".to_string(), is_static, is_public, is_final),
            );
        
            self.stack.pop();
        
            module
        }
    }

    fn handle_return(&mut self, statement: HashMap<Value, Value>) -> Value {
        let value = match statement.get(&Value::String("value".to_string())) {
            Some(v) => v,
            None => return self.raise("RuntimeError", "Missing 'value' in return statement"),
        };

        self.state = State::Defer;
        if !self.defer_stack.is_empty() {
            let defer_statements = self.defer_stack.concat();
            for stmt in defer_statements {
                self.current_statement = Some(stmt);
                self.evaluate(self.current_statement.clone().unwrap());
            }
        }
        self.state = State::Normal;

        let stmt = value.convert_to_statement();

        // Tailcall optimization (TCO)
        if stmt.get_type() == "CALL" {
            let func_name = stmt.get_value("name").unwrap_or(Value::Null).to_string();
            let len = self.stack.frames.len();
            for i in (0..len).rev() {
                let frame = &self.stack.frames[i];
                if frame.stack_type == StackType::FunctionCall && frame.file_path == func_name {
                    self.state = State::TCO(stmt);
                    return NULL;
                }
            }
        }

        let evaluated_value = self.evaluate(stmt);
        if self.err.is_some() {
            return NULL;
        }
    
        self.is_returning = true;
        self.return_value = evaluated_value.clone();

        evaluated_value
    }

    fn handle_function_declaration(&mut self, statement: HashMap<Value, Value>) -> Value {
        let mut name = match statement.get(&Value::String("name".to_string())) {
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

        let mut return_type_str = self.evaluate(return_type.convert_to_statement());

        if return_type_str == "auto".into() {
            return_type_str = "any".into();
        }

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
        
                let mods = match keys.iter().position(|k| k == &Value::String("modifiers".to_string())) {
                    Some(pos) => match &values[pos] {
                        Value::List(l) => l.iter().filter_map(|v| {
                            if let Value::String(s) = v {
                                Some(s.clone())
                            } else {
                                None
                            }
                        }).collect(),
                        _ => vec![],
                    },
                    None => vec![],
                };
        
                match &type_str_value {
                    Value::Map { .. } => {
                        parameters.push(Parameter::positional_pt(name.as_str(), &type_str_value).set_mods(mods));
                    }
                    Value::String(s) => {
                        parameters.push(Parameter::positional(name.as_str(), s.as_str()).set_mods(mods));
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
        
                let mods = match keys.iter().position(|k| k == &Value::String("modifiers".to_string())) {
                    Some(pos) => match &values[pos] {
                        Value::List(l) => l.iter().filter_map(|v| {
                            if let Value::String(s) = v {
                                Some(s.clone())
                            } else {
                                None
                            }
                        }).collect(),
                        _ => vec![],
                    },
                    None => vec![],
                };
        
                let type_eval = self.evaluate(type_val.convert_to_statement());
        
                let value = match keys.iter().position(|k| k == &Value::String("value".to_string())) {
                    Some(pos) => {
                        let val = &values[pos];
                        self.evaluate(val.convert_to_statement())
                    }
                    None => NULL,
                };
        
                match &type_eval {
                    Value::Map { .. } => {
                        parameters.push(Parameter::positional_optional_pt(
                            name_str.as_str(),
                            &type_eval,
                            value,
                        ).set_mods(mods));
                    }
                    Value::String(s) => {
                        parameters.push(Parameter::positional_optional(
                            name_str.as_str(),
                            s.as_str(),
                            value,
                        ).set_mods(mods));
                    }
                    _ => return self.raise("RuntimeError", "Invalid type for named argument 'type'"),
                }
            } else {
                return self.raise("TypeError", "Expected a map for named argument");
            }
        }        

        let body_formatted: Vec<Statement> = body.iter().map(|v| v.convert_to_statement()).collect();

        let lambda_name;  // stupid lifetimes
        if name == "<lambda#{}>" {
            let id: usize = self.internal_storage.lambda_counter;
            self.internal_storage.lambda_counter = id + 1;
            lambda_name = name.replace("{}", &id.to_string());
            name = &lambda_name;
        }

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

        if self.variables.contains_key(name) && !name.starts_with("<") {
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
                is_static,
                is_public,
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

                match self.variables.remove(name) {
                    Some(value) => {
                        debug_log(&format!("<Variable '{}' forgotten>", name), &self.config, Some(self.use_colors.clone()));
                        value.get_value().clone()
                    },
                    None => self.raise("NameError", &format!("Variable '{}' not found for forget", name)),
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

            "TUPLE" => {
                let items_val = match value_map.get(&Value::String("items".to_string())) {
                    Some(v) => v,
                    None => return self.raise("RuntimeError", "Missing 'items' in tuple forget"),
                };

                let items = match items_val {
                    Value::List(list) => list,
                    _ => return self.raise("RuntimeError", "'items' must be a list in tuple forget"),
                };

                let mut results = Vec::new();
                let mut names = Vec::new();
                
                fn extract_names(item: &Value, names: &mut Vec<String>) -> Result<(), String> {
                    let item_map = match item {
                        Value::Map { keys, values } => keys.iter().cloned().zip(values.iter().cloned()).collect::<HashMap<_, _>>(),
                        _ => return Err("Expected a map for tuple forget item".to_string()),
                    };

                    match item_map.get(&Value::String("type".to_string())) {
                        Some(Value::String(n)) => match n.as_str() {
                            "VARIABLE" => {
                                if let Some(Value::String(name)) = item_map.get(&Value::String("name".to_string())) {
                                    names.push(name.clone());
                                    Ok(())
                                } else {
                                    Err("Missing or invalid 'name' in tuple forget item".to_string())
                                }
                            }
                            "TUPLE" => {
                                if let Some(Value::List(items)) = item_map.get(&Value::String("items".to_string())) {
                                    for sub_item in items {
                                        extract_names(sub_item, names)?;
                                    }
                                    Ok(())
                                } else {
                                    Err("Missing or invalid 'items' in tuple forget item".to_string())
                                }
                            }
                            _ => Ok(()),
                        },
                        _ => Err("Missing or invalid 'type' in tuple forget item".to_string()),
                    }
                }

                for item in items {
                    let mut stmt = HashMap::new();
                    stmt.insert(Value::String("type".to_string()), Value::String("FORGET".to_string()));
                    stmt.insert(Value::String("value".to_string()), item.clone());
                    let name = match extract_names(&item, &mut names) {
                        Ok(_) => item,
                        Err(e) => return self.raise("RuntimeError", &e),
                    };
                    names.push(name.to_string());
                    let forgotten_value = self.handle_forget(stmt);
                    results.push(forgotten_value);
                }

                debug_log(&format!("<Variables '{}' forgotten>", names.join(", ")), &self.config, Some(self.use_colors.clone()));

                Value::Tuple(results)
            }

            _ => self.raise("RuntimeError", &format!("Unsupported forget value type: {}", value_type)),
        }
    }

    fn handle_type(&mut self, statement: HashMap<Value, Value>) -> Value {
        let default_type = Value::String("any".to_string());
    
        let valid_types: Vec<Value> = match self.internal_storage.types.values().cloned().collect::<Vec<_>>() {
            ref v if !v.is_empty() => v.clone(),
            _ => {
                self.raise("RuntimeError", "Type cache is not properly initialized");
                return NULL;
            }
        };

        let check_type_validity = |type_str: &str| -> bool {
            let trimmed = type_str.trim_start_matches('&').trim_start_matches('?');
            valid_types.iter().any(|val| {
                if let Value::String(s) = val {
                    s == trimmed
                } else {
                    false
                }
            })
        };
    
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
                self.raise("RuntimeError", "Invalid 'type_kind' for type statement");
                return NULL;
            }
        };
    
        match kind_str {
            "simple" => {
                let type_name = statement.get(&Value::String("value".to_string())).unwrap_or(&default_type);
    
                if let Value::String(s) = type_name {
                    if check_type_validity(s) {
                        return Value::String(s.clone());
                    }

                    let type_val = match self.internal_storage.types.get(s) {
                        Some(val) => val,
                        None => {
                            if self.variables.contains_key(s) {
                                return self.raise_with_help(
                                    "TypeError",
                                    &format!("Invalid type '{}'", s),
                                    &format!("'{}' is a variable name, not a type. If you meant to assign a value, use ':=' instead of ':'.", s),
                                );
                            }

                            self.raise(
                                "TypeError",
                                &format!(
                                    "Invalid type '{}'. Valid types are: {}, ...",
                                    s,
                                    VALID_TYPES[0..5].join(", ")
                                ),
                            );
                            return NULL;
                        }
                    };

                    let hm = match type_val.convert_to_hashmap() {
                        Some(hm) => hm,
                        None => {
                            self.raise("RuntimeError", "Type value is not a map");
                            return NULL;
                        }
                    };

                    let name = match hm.get("name") {
                        Some(Value::String(n)) => n.clone(),
                        _ => {
                            self.raise("RuntimeError", "Missing or invalid 'name' in new type statement");
                            return NULL;
                        }
                    };

                    let base = match hm.get("base") {
                        Some(Value::Map { keys, values }) => {
                            let base_map: HashMap<_, _> = keys.iter().cloned().zip(values.iter().cloned()).collect();
                            self.handle_type(base_map)
                        }
                        _ => {
                            self.raise("RuntimeError", "Missing or invalid 'base' in new type statement");
                            return NULL;
                        }
                    };

                    let variables = match hm.get("variables") {
                        Some(Value::List(vars)) => vars.clone(),
                        _ => {
                            self.raise("RuntimeError", "Expected a list for 'variables' in new type statement");
                            return NULL;
                        }
                    };

                    let conditions = match hm.get("conditions") {
                        Some(Value::List(conds)) => conds.clone(),
                        _ => {
                            self.raise("RuntimeError", "Expected a list for 'conditions' in new type statement");
                            return NULL;
                        }
                    };

                    Value::Map {
                        keys: vec![
                            Value::String("_note".to_string()),
                            Value::String("type_kind".to_string()),
                            Value::String("name".to_string()),
                            Value::String("base".to_string()),
                            Value::String("variables".to_string()),
                            Value::String("conditions".to_string()),
                        ],
                        values: vec![
                            Value::String(create_note(
                                "This map is for the interpreter's internal use to track types. You don't need to worry about it.",
                                Some(self.use_colors),
                                &self.config.color_scheme.note,
                            )),
                            Value::String("new".to_string()),
                            Value::String(name),
                            base,
                            Value::List(variables),
                            Value::List(conditions),
                        ],
                    }
                } else {
                    self.raise("TypeError", "Type value is not a string");
                    return NULL;
                }
            }
    
            "union" => {
                let types_val = match statement.get(&Value::String("types".to_string())) {
                    Some(t) => t,
                    None => {
                        self.raise("RuntimeError", "Missing 'types' in union type statement");
                        return NULL;
                    }
                };

                let types_list = match types_val {
                    Value::List(l) => l,
                    _ => {
                        self.raise("RuntimeError", "'types' in union must be a list");
                        return NULL;
                    }
                };

                let mut handled_types = Vec::new();
                for t in types_list {
                    match t {
                        Value::Map { keys, values } => {
                            let map: HashMap<_, _> = keys.iter().cloned().zip(values.iter().cloned()).collect();
                            let ht = self.handle_type(map);
                            if ht == NULL {
                                return NULL;
                            }
                            handled_types.push(ht);
                        }
                        Value::String(s) => {
                            if !check_type_validity(s) {
                                self.raise("TypeError", &format!("Invalid union member type '{}'", s));
                                return NULL;
                            }
                            handled_types.push(Value::String(s.clone()));
                        }
                        _ => {
                            self.raise("RuntimeError", "Each union member must be a type map or string");
                            return NULL;
                        }
                    }
                }

                let mut keys = vec![
                    Value::String("_note".to_string()),
                    Value::String("type_kind".to_string()),
                    Value::String("types".to_string())
                ];
                let mut values = vec![
                    Value::String(create_note(
                        "This map is for the interpreter's internal use to track union types. You don't need to worry about it.",
                        Some(self.use_colors),
                        &self.config.color_scheme.note,
                    )),
                    Value::String("union".to_string()),
                    Value::List(handled_types),
                ];

                for (k, v) in statement.iter() {
                    if k != &Value::String("types".to_string())
                        && k != &Value::String("type_kind".to_string())
                        && k != &Value::String("type".to_string())
                    {
                        keys.push(k.clone());
                        values.push(v.clone());
                    }
                }

                return Value::Map { keys, values };
            }
    
            "function" => {
                if let Some(Value::List(elements)) = statement.get(&Value::String("elements".to_string())) {
                    for el in elements {
                        match el {
                            Value::String(s) => {
                                if !check_type_validity(s) {
                                    self.raise("TypeError", &format!("Invalid function parameter type '{}'", s));
                                    return NULL;
                                }
                            }
                            Value::Map { keys, values } => {
                                let map: HashMap<_, _> = keys.iter().cloned().zip(values.iter().cloned()).collect();
                                let handled = self.handle_type(map);
                                if handled == NULL {
                                    return NULL;
                                }
                            }
                            _ => {
                                self.raise("RuntimeError", "Invalid function parameter type element");
                                return NULL;
                            }
                        }
                    }
                }

                if let Some(Value::Map { keys, values }) = statement.get(&Value::String("return_type".to_string())) {
                    let map: HashMap<_, _> = keys.iter().cloned().zip(values.iter().cloned()).collect();
                    let ret_val = self.handle_type(map);
                    if ret_val == NULL {
                        return NULL;
                    }
                } else if let Some(Value::String(s)) = statement.get(&Value::String("return_type".to_string())) {
                    if !check_type_validity(s) {
                        self.raise("TypeError", &format!("Invalid function return type '{}'", s));
                        return NULL;
                    }
                }

                let mut keys = vec![Value::String("_note".to_string())];
                let mut values = vec![Value::String(create_note(
                    "This map is for the interpreter's internal use to track types. You don't need to worry about it.",
                    Some(self.use_colors),
                    &self.config.color_scheme.note,
                ))];

                keys.extend(statement.keys().cloned());
                values.extend(statement.values().cloned());

                return Value::Map { keys, values };
            }
    
            "indexed" => {
                if let Some(Value::String(base)) = statement.get(&Value::String("base".to_string())) {
                    if !check_type_validity(base) {
                        self.raise("TypeError", &format!("Invalid base type '{}'", base));
                        return NULL;
                    }
                }

                let mut handled_elements = Vec::new();

                if let Some(Value::List(elements)) = statement.get(&Value::String("elements".to_string())) {
                    for el in elements {
                        match el {
                            Value::Map { keys, values } => {
                                let map: HashMap<_, _> = keys.iter().cloned().zip(values.iter().cloned()).collect();
                                let handled = self.handle_type(map);
                                if handled == NULL {
                                    return NULL;
                                }
                                handled_elements.push(handled);
                            }
                            Value::String(s) => {
                                if !check_type_validity(s) {
                                    self.raise("TypeError", &format!("Invalid type '{}'", s));
                                    return NULL;
                                }
                                handled_elements.push(Value::String(s.clone()));
                            }
                            _ => {
                                self.raise("RuntimeError", "Invalid element in indexed types");
                                return NULL;
                            }
                        }
                    }
                }

                let mut keys = vec![Value::String("_note".to_string())];
                let mut values = vec![Value::String(create_note(
                    "This map is for the interpreter's internal use to track types. You don't need to worry about it.",
                    Some(self.use_colors),
                    &self.config.color_scheme.note,
                ))];

                keys.extend(statement.keys().cloned());
                values.extend(statement.values().cloned());

                let elements_key = Value::String("elements".to_string());
                if let Some(pos) = keys.iter().position(|k| k == &elements_key) {
                    values[pos] = Value::List(handled_elements);
                }

                return Value::Map { keys, values };
            }

            _ => {
                self.raise("RuntimeError", "Invalid 'type_kind' for type statement");
                return NULL;
            }
        }
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
                        hex_to_ansi(&self.config.color_scheme.note, self.use_colors),
                        hex_to_ansi(&self.config.color_scheme.help, self.use_colors),
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
    
        let mut result = NULL;
        for stmt in body {
            if !stmt.is_statement() {
                continue;
            }
    
            result = self.evaluate(stmt.convert_to_statement());
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
    
                let mut err_result = NULL;
                for catch_stmt in catch_body {
                    if !catch_stmt.is_statement() {
                        continue;
                    }
                    err_result = self.evaluate(catch_stmt.convert_to_statement());
                }

                return err_result;
            }
        }
    
        if self.err.is_some() {
            return NULL;
        }
        result
    }

    // TODO: Fix nested index assignment and map index assign
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
                loc: None,
            }.convert_to_hashmap(),
            _ => return self.raise("RuntimeError", "Expected a map for assignment"),
        };
    
        let left_type = match left_hashmap.get(&Value::String("type".to_string())) {
            Some(Value::String(t)) => t,
            _ => return self.raise("RuntimeError", "Missing or invalid 'type' in assignment left"),
        };

        if self.err.is_some() {
            return NULL;
        }

        match left_type.as_str() {
            "VARIABLE" => {
                let name = match left_hashmap.get(&Value::String("name".to_string())) {
                    Some(Value::String(n)) => n,
                    _ => return self.raise("RuntimeError", "Missing or invalid 'name' in variable assignment"),
                };
    
                let expected_type = {
                    let var = match self.variables.get(name) {
                        Some(v) => v,
                        None => {
                            let suggestion = if let Value::Null = right_value {
                                format!(
                                    "Did you mean to use '{}' instead of '='?",
                                    wrap_in_help(":=", self.use_colors.clone(), &self.config)
                                )
                            } else {
                                format!(
                                    "Use this instead: '{}{}: {} = {}{}'",
                                    check_ansi("\x1b[4m", &self.use_colors),
                                    name,
                                    right_value.type_name(),
                                    format_value(&right_value),
                                    check_ansi("\x1b[24m", &self.use_colors),
                                )
                            };

                            return self.raise_with_help(
                                "NameError",
                                &format!("Variable '{}' is not defined", name),
                                &suggestion,
                            );
                        }
                    };
                    var.type_name().to_owned()
                };

                if self.err.is_some() {
                    return NULL;
                }
    
                if !self.check_type(&right_value, &Value::String(expected_type.clone()), false) {
                    if self.err.is_some() {
                        return NULL;
                    }
                    return self.raise(
                        "TypeError",
                        &format!(
                            "Invalid type for variable '{}': expected '{}', got '{}'",
                            name, format_type(&<String as Into<Value>>::into(expected_type)), format_type(&<String as Into<Value>>::into(right_value.type_name()))
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
                fn assign_index(
                    interpreter: &mut Interpreter,
                    variable_name: &str,
                    index_access: Value,
                    right_value: Value,
                ) -> Value {
                    let var = match interpreter.variables.get_mut(variable_name) {
                        Some(v) => v,
                        None => return interpreter.raise("NameError", &format!("Variable '{}' not found for index assignment", variable_name)),
                    };
                
                    if var.is_final() {
                        return interpreter.raise("AssignmentError", &format!("Cannot assign to final variable '{}'", variable_name));
                    }
                
                    match &mut var.value {
                        Value::List(l) => {
                            let index = match index_access {
                                Value::Int(i) => match i.to_i64() {
                                    Ok(i64_val) if i64_val >= 0 => i64_val as usize,
                                    _ => return interpreter.raise("TypeError", "List index must be a non-negative integer"),
                                },
                                Value::Float(f) => {
                                    match f.to_f64() {
                                        Ok(f64_val) if f64_val.fract() == 0.0 && f64_val >= 0.0 => f64_val as usize,
                                        _ => return interpreter.raise("TypeError", "List index must be a non-negative whole number"),
                                    }
                                },
                                _ => return interpreter.raise("TypeError", "List index must be an integer or whole float"),
                            };
                            if index >= l.len() {
                                return interpreter.raise("IndexError", "List index out of range");
                            }
                            l[index] = right_value;
                            var.value.clone()
                        }
                        Value::Bytes(b) => {
                            let index = match index_access {
                                Value::Int(i) => match i.to_i64() {
                                    Ok(i64_val) if i64_val >= 0 => i64_val as usize,
                                    _ => return interpreter.raise("TypeError", "Bytes index must be a non-negative integer"),
                                },
                                Value::Float(f) => {
                                    match f.to_f64() {
                                        Ok(f64_val) if f64_val.fract() == 0.0 && f64_val >= 0.0 => f64_val as usize,
                                        _ => return interpreter.raise("TypeError", "Bytes index must be a non-negative whole number"),
                                    }
                                },
                                _ => return interpreter.raise("TypeError", "Bytes index must be an integer or whole float"),
                            };
                            if index >= b.len() {
                                return interpreter.raise("IndexError", "Bytes index out of range");
                            }
                            let val_as_u8 = match right_value {
                                Value::Int(i) => match i.to_i64() {
                                    Ok(i64_val) => i64_val as u8,
                                    Err(_) => return interpreter.raise("TypeError", "Expected a numeric value for byte assignment"),
                                },
                                Value::Float(f) => match f.to_f64() {
                                    Ok(f64_val) => f64_val as u8,
                                    Err(_) => return interpreter.raise("TypeError", "Expected a numeric value for byte assignment"),
                                },
                                Value::String(s) => s.parse::<u8>().unwrap_or(0),
                                _ => return interpreter.raise("TypeError", "Expected a numeric value for byte assignment"),
                            };
                            b[index] = val_as_u8;
                            var.value.clone()
                        }
                        Value::String(s) => {
                            let index = match index_access {
                                Value::Int(i) => match i.to_i64() {
                                    Ok(i64_val) if i64_val >= 0 => i64_val as usize,
                                    _ => return interpreter.raise("TypeError", "String index must be a non-negative integer"),
                                },
                                Value::Float(f) => {
                                    match f.to_f64() {
                                        Ok(f64_val) if f64_val.fract() == 0.0 && f64_val >= 0.0 => f64_val as usize,
                                        _ => return interpreter.raise("TypeError", "String index must be a non-negative whole number"),
                                    }
                                },
                                _ => return interpreter.raise("TypeError", "String index must be an integer or whole float"),
                            };
                            if index >= s.len() {
                                return interpreter.raise("IndexError", "String index out of range");
                            }
                            let mut chars: Vec<char> = s.chars().collect();
                            let replacement_str = right_value.to_string();
                            if replacement_str.chars().count() != 1 {
                                return interpreter.raise("TypeError", "String assignment must be a single character");
                            }
                            chars[index] = replacement_str.chars().next().unwrap();
                            *s = chars.into_iter().collect();
                            var.value.clone()
                        }
                        Value::Map { keys, values } => {
                            let key = index_access;
                            match keys.iter().position(|k| k == &key) {
                                Some(idx) => values[idx] = right_value,
                                None => {
                                    keys.push(key);
                                    values.push(right_value);
                                }
                            }
                            var.value.clone()
                        }
                        _ => interpreter.raise("TypeError", "Object not indexable for index assignment"),
                    }
                }
                let mut get_single_index_from_map = |index_map: &Value| -> Result<Value, (String, String)> {
                    if let Value::Map { keys, values } = index_map {
                        let mut start_val = None;
                        let mut end_val = None;
                
                        for (k, v) in keys.iter().zip(values.iter()) {
                            if let Value::String(s) = k {
                                if s == "start" {
                                    start_val = Some(v.clone());
                                } else if s == "end" {
                                    end_val = Some(v.clone());
                                }
                            }
                        }
                
                        match (start_val, end_val) {
                            (Some(start), Some(end)) => {
                                if start == end {
                                    Ok(self.evaluate(start.convert_to_statement()))
                                } else {
                                    Err((
                                        "IndexError".to_string(),
                                        "Slice assignment requires 'start' and 'end' to be equal".to_string(),
                                    ))
                                }
                            }
                            _ => Err((
                                "RuntimeError".to_string(),
                                "Missing 'start' or 'end' in index access assignment".to_string(),
                            )),
                        }
                    } else {
                        Err((
                            "TypeError".to_string(),
                            "Index access must be a map with 'start' and 'end' keys".to_string(),
                        ))
                    }
                };           

                let mut object_val = match left_hashmap.get(&Value::String("object".to_string())) {
                    Some(v) => v,
                    None => return self.raise("RuntimeError", "Missing 'object' in index access assignment"),
                };

                let access_val = match left_hashmap.get(&Value::String("access".to_string())) {
                    Some(v) => v,
                    None => return self.raise("RuntimeError", "Missing 'access' in index access assignment"),
                };

                let index_access = match get_single_index_from_map(&access_val) {
                    Ok(idx) => idx,
                    Err((kind, msg)) => return self.raise(&kind, &msg),
                };

                loop {
                    let object_type = match object_val {
                        Value::Map { keys, values } => {
                            let obj_map: HashMap<_, _> = keys.iter().cloned().zip(values.iter().cloned()).collect();
                    
                            match obj_map.get(&Value::String("type".to_string())) {
                                Some(t) => match t {
                                    Value::String(s) => s.clone(),
                                    _ => {
                                        self.raise("RuntimeError", "Expected 'type' to be a String");
                                        "".to_string()
                                    }
                                },
                                None => {
                                    self.raise("RuntimeError", "Missing 'type' field");
                                    "".to_string()
                                }
                            }
                        }
                        _ => {
                            self.raise("RuntimeError", "Expected a Map value");
                            "".to_string()
                        }
                    };

                    if self.err.is_some() {
                        return NULL;
                    };

                    match object_type.as_str() {
                        "VARIABLE" => {
                            let variable_name = match object_val {
                                Value::Map { keys, values } => {
                                    let obj_map: HashMap<_, _> = keys.iter().cloned().zip(values.iter().cloned()).collect();
                                    match obj_map.get(&Value::String("name".to_string())) {
                                        Some(Value::String(name)) => name.clone(),
                                        _ => return self.raise("RuntimeError", "Missing or invalid 'name' in variable assignment"),
                                    }
                                }
                                _ => return self.raise("RuntimeError", "Expected a Map value for variable assignment"),
                            };

                            if self.err.is_some() {
                                return NULL;
                            }

                            return assign_index(self, &variable_name, index_access, right_value);
                        }
                        "INDEX_ACCESS" => {
                            object_val = match object_val {
                                Value::Map { keys, values } => {
                                    if let Some(pos) = keys.iter().position(|k| *k == Value::String("object".to_string())) {
                                        &values[pos]
                                    } else {
                                        return self.raise("RuntimeError", "Missing 'object' in nested index access assignment");
                                    }
                                }
                                _ => return self.raise("RuntimeError", "Expected a Map value for nested index access assignment"),
                            };
                            continue;     
                        }
                        "LIST" | "BYTES" | "STRING" | "TUPLE" => {
                            return assign_index(self, &object_val.to_string(), index_access, right_value);
                        }
                        _ => {
                            self.raise("TypeError", &format!("Unsupported object type for index assignment: {}", object_type));
                            return NULL;
                        }
                    };
                }
            }
            // fuck my parser
            "POINTER_DEREF" => {
                let left_value = left_hashmap
                    .get(&Value::String("value".to_string()))
                    .cloned()
                    .unwrap_or(NULL);

                let mut loc = Location {
                    file: self.file_path.clone(),
                    line_string: "".to_string(),
                    line_number: 0,
                    range: (0, 0),
                };

                if let Some(Value::Map { keys, values }) = left_hashmap.get(&Value::String("_loc".to_string())) {
                    let loc_map: std::collections::HashMap<_, _> =
                        keys.iter().cloned().zip(values.iter().cloned()).collect();

                    if let Some(Value::String(f)) = loc_map.get(&Value::String("_file".to_string())) {
                        loc.file = f.clone();
                    }
                    if let Some(Value::String(ls)) = loc_map.get(&Value::String("_line_string".to_string())) {
                        loc.line_string = ls.clone();
                    }
                    if let Some(Value::Int(i)) = loc_map.get(&Value::String("_line_number".to_string())) {
                        loc.line_number = i.to_i64().unwrap_or(0).max(0) as usize;
                    }
                    if let Some(Value::Tuple(vals)) = loc_map.get(&Value::String("_range".to_string())) {
                        if let (Some(Value::Int(start)), Some(Value::Int(end))) = (vals.get(0), vals.get(1)) {
                            let start = start.to_i64().unwrap_or(0).max(0) as usize;
                            let end = end.to_i64().unwrap_or(0).max(0) as usize;
                            loc.range = (start, end);
                        }                        
                    }
                }

                let result = self.evaluate(
                    Statement::Statement {
                        keys: vec![
                            Value::String("type".to_string()),
                            Value::String("left".to_string()),
                            Value::String("right".to_string()),
                        ],
                        values: vec![
                            Value::String("POINTER_ASSIGN".to_string()),
                            left_value,
                            right.clone(),
                        ],
                        loc: Some(loc),
                    }
                );

                if self.err.is_some() {
                    return NULL;
                }

                result
            }
            _ => self.raise("TypeError", &format!("Cannot assign to type '{}'", get_type_from_token_name(left_type))),
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
            .and_then(|val| if val == NULL { Some(Value::Int(0.into())) } else { Some(val) });
        let end_eval_opt = end_val_opt
            .map(|v| self.evaluate(v.convert_to_statement()))
            .and_then(|val| if val == NULL { Some(Value::Int(len.into())) } else { Some(val) });

        if self.err.is_some() {
            return NULL;
        }

        if let Value::Map { keys, values } = &object_val {
            match (start_eval_opt.as_ref(), end_eval_opt.as_ref()) {
                (Some(start_val), Some(end_val)) if start_val == end_val => {
                    for (k, v) in keys.iter().zip(values.iter()) {
                        if k == start_val {
                            return v.clone();
                        }
                    }
                    return self.raise("KeyError", &format!("Key '{}' not found in map", format_value(start_val)));
                }
                (Some(_), Some(_)) => {
                    return self.raise("TypeError", "Slicing maps is not supported");
                }
                (Some(start_val), None) => {
                    for (k, v) in keys.iter().zip(values.iter()) {
                        if k == start_val {
                            return v.clone();
                        }
                    }
                    return self.raise("KeyError", &format!("Key '{}' not found in map", format_value(start_val)));
                }
                _ => {
                    return self.raise("TypeError", "Invalid index access on map");
                }
            }
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
                    let start_idx = match to_index(&start_val) {
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
                    return self.raise("KeyError", &format!("'{}' not found in map", format_value(&key_val)));
                }
                _ => return self.raise("TypeError", "Start must be int or string"),
            },
            (start_opt, end_opt) => {
                let start_idx = match start_opt {
                    Some(ref v) => match to_index(v) {
                        Ok(i) => i,
                        Err(e) => return e,
                    },
                    None => 0,
                };
            
                let end_idx = match end_opt {
                    Some(ref v) => match to_index(v) {
                        Ok(i) => i,
                        Err(e) => return e,
                    },
                    None => len,
                };
            
                if end_idx > len {
                    return self.raise("IndexError", "End index out of range");
                }

                if end_idx == start_idx {
                    return match &object_val {
                        Value::String(s) => s.chars().nth(start_idx).map(|c| Value::String(c.to_string()))
                            .unwrap_or_else(|| self.raise("IndexError", "Index out of range")),
                        Value::List(l) => l.get(start_idx).cloned()
                            .unwrap_or_else(|| self.raise("IndexError", "Index out of range")),
                        Value::Bytes(b) => b.get(start_idx).map(|&b| Value::Int((b as i64).into()))
                            .unwrap_or_else(|| self.raise("IndexError", "Index out of range")),
                        Value::Tuple(t) => t.get(start_idx).cloned()
                            .unwrap_or_else(|| self.raise("IndexError", "Index out of range")),
                        _ => return self.raise("TypeError", "Object not indexable"),
                    };
                }                
            
                if start_idx > end_idx {
                    match &object_val {
                        Value::String(s) => {
                            let slice: String = s.chars()
                                .skip(end_idx)
                                .take(start_idx - end_idx)
                                .collect();
                            let rev_slice = slice.chars().rev().collect();
                            return Value::String(rev_slice);
                        }
                        Value::List(l) => {
                            let slice = l.get(end_idx..start_idx).unwrap_or(&[]).to_vec();
                            let rev_slice = slice.into_iter().rev().collect();
                            return Value::List(rev_slice);
                        }
                        Value::Bytes(b) => {
                            let slice = b.get(end_idx..start_idx).unwrap_or(&[]).to_vec();
                            let rev_slice = slice.into_iter().rev().collect();
                            return Value::Bytes(rev_slice);
                        }
                        Value::Tuple(t) => {
                            let slice = t.get(end_idx..start_idx).unwrap_or(&[]).to_vec();
                            let rev_slice = slice.into_iter().rev().collect();
                            return Value::Tuple(rev_slice);
                        }
                        _ => return self.raise("TypeError", "Object not sliceable"),
                    }
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
    
        let mut declared_type = match statement.get(&Value::String("var_type".to_string())) {
            Some(t) => self.evaluate(t.convert_to_statement()),
            _ => Value::String("any".to_string()),
        };

        if declared_type == "auto".into() {
            declared_type = value.type_name().into();
        }

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
            if self.err.is_some() {
                return NULL;
            }
            return self.raise("TypeError", &format!("Variable '{}' declared with type '{}', but value is of type '{}'", name, format_type(&declared_type), value.type_name()));
        }
    
        let variable = Variable::new(name.to_string(), value.clone(), declared_type.to_string(), is_static, is_public, is_final);
        self.variables.insert(name.to_string(), variable);

        debug_log(
            &format!("<Declared variable '{}': {} = {}>", name, value.type_name(), format_value(&value)),
            &self.config, Some(self.use_colors),
        );
    
        value   
    }

    fn handle_variable(&mut self, statement: HashMap<Value, Value>) -> Value {
        let name = match statement.get(&Value::String("name".to_string())) {
            Some(Value::String(s)) => s,
            _ => return self.raise("SyntaxError", "Expected a string for variable name"),
        };
    
        if let Some(var) = self.variables.get(name) {
            return var.get_value().clone();
        } else {
            let cache = &self.internal_storage.types;
            return match cache.get(&name.to_string()) {
                Some(v) => format_type(v).into(),
                None => {
                    let lib_dir = PathBuf::from(self.config.home_dir.clone()).join("libs").join(&name);
                    let extensions = ["lc", "lucia", "rs", ""];
                    for ext in extensions.iter() {
                        let candidate = lib_dir.with_extension(ext);
                        if candidate.exists() {
                            return self.raise_with_help(
                                "ImportError",
                                &format!("Variable '{}' is not defined.", name),
                                &format!(
                                    "Maybe you forgot to import '{}'? Use '{}import {} from \"{}\"{}'.",
                                    name,
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
                            &format!(
                                "Did you mean '{}{}{}'?",
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
        }
    }
    
    fn handle_for_loop(&mut self, statement: HashMap<Value, Value>) -> Value {
        static KEY_ITERABLE: once_cell::sync::Lazy<Value> = once_cell::sync::Lazy::new(|| Value::String("iterable".into()));
        static KEY_BODY: once_cell::sync::Lazy<Value> = once_cell::sync::Lazy::new(|| Value::String("body".into()));
        static KEY_VARIABLE: once_cell::sync::Lazy<Value> = once_cell::sync::Lazy::new(|| Value::String("variable".into()));

        let iterable = match statement.get(&*KEY_ITERABLE) {
            Some(v) => v,
            None => return self.raise("RuntimeError", "Missing 'iterable' in for loop statement"),
        };

        let iterable_value = self.evaluate(iterable.convert_to_statement());
        if self.err.is_some() {
            return NULL;
        }
        if !iterable_value.is_iterable() {
            return self.raise("TypeError", &format!("Expected an iterable for 'for' loop, got {}", iterable_value.type_name()));
        }

        let body = match statement.get(&*KEY_BODY) {
            Some(Value::List(body)) => body,
            _ => return self.raise("RuntimeError", "Expected a list for 'body' in for loop statement"),
        };

        let variable_value = match statement.get(&*KEY_VARIABLE) {
            Some(Value::String(name)) => Value::List(vec![Value::String(name.clone())]),
            Some(Value::List(names)) => Value::List(names.clone()),
            _ => return self.raise("RuntimeError", "Expected a string or list for 'variable' in for loop statement"),
        };

        let variable_names: Vec<String> = match &variable_value {
            Value::List(vars) => {
                let mut names = Vec::new();
                for v in vars {
                    if let Value::String(s) = v {
                        names.push(s.clone());
                    } else {
                        return self.raise("RuntimeError", "Variable names must be strings");
                    }
                }
                names
            }
            _ => unreachable!(),
        };

        let mut result = NULL;

        for item in iterable_value.iter() {
            let previous_vars: Vec<Option<Variable>> = variable_names.iter()
                .map(|name| self.variables.get(name).cloned())
                .collect();

            if self.check_stop_flag() {
                return NULL;
            }

            if let Value::Error(err_type, err_msg, ref_err) = item {
                if let Some(re) = ref_err {
                    self.err = Some(Error::with_ref(err_type, err_msg, re.clone(), &self.file_path.clone()));
                } else {
                    self.err = Some(Error::new(err_type, err_msg, &self.file_path.clone()));
                }
                return NULL;
            }

            // Destructure if item is a list or tuple, else single assignment if only one var name
            if variable_names.len() == 1 {
                // Single variable assignment
                self.variables.insert(
                    variable_names[0].clone(),
                    Variable::new(variable_names[0].clone(), item.clone(), item.type_name(), false, true, true),
                );
            } else {
                // Multiple variables: check item is list or tuple and match lengths
                let values_to_assign = match item {
                    Value::List(inner) => inner,
                    Value::Tuple(inner) => inner,
                    _ => {
                        return self.raise("TypeError", &format!("Cannot destructure non-list/tuple value into multiple variables"));
                    }
                };

                if values_to_assign.len() != variable_names.len() {
                    return self.raise("ValueError", &format!("Mismatched number of variables and values to destructure ({} vs {})", variable_names.len(), values_to_assign.len()));
                }

                for (name, val) in variable_names.iter().zip(values_to_assign.iter()) {
                    self.variables.insert(
                        name.clone(),
                        Variable::new(name.clone(), val.clone(), val.type_name(), false, true, true),
                    );
                }
            }

            for stmt in body {
                result = self.evaluate(stmt.convert_to_statement());

                if self.err.is_some() {
                    for (name, prev_var) in variable_names.iter().zip(previous_vars.iter()) {
                        match prev_var {
                            Some(var) => self.variables.insert(name.clone(), var.clone()),
                            None => self.variables.remove(name),
                        };
                    }
                    return NULL;
                }

                if self.is_returning {
                    for (name, prev_var) in variable_names.iter().zip(previous_vars.iter()) {
                        match prev_var {
                            Some(var) => self.variables.insert(name.clone(), var.clone()),
                            None => self.variables.remove(name),
                        };
                    }
                    return result;
                }
            }

            for (name, prev_var) in variable_names.iter().zip(previous_vars.iter()) {
                match prev_var {
                    Some(var) => self.variables.insert(name.clone(), var.clone()),
                    None => self.variables.remove(name),
                };
            }

            match self.state {
                State::Break => {
                    self.state = State::Normal;
                    break;
                }
                State::Continue => {
                    self.state = State::Normal;
                    continue;
                }
                _ => {}
            }
        }

        result
    }

    fn handle_iterable(&mut self, statement: HashMap<Value, Value>) -> Value {
        let Some(_) = statement.get(&Value::String("iterable_type".to_string())).cloned() else {
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
                    .unwrap_or(NULL);

                let pattern_flag = statement.get(&Value::String("pattern_reg".to_string()))
                    .cloned()
                    .unwrap_or(Value::Boolean(false));

                let seed: Vec<Value> = match &seed_raw {
                    Value::List(elements) => elements.clone(),
                    _ => {
                        self.raise("TypeError", "Expected 'seed' to be a list");
                        return NULL;
                    }
                };

                let range_mode = match statement.get(&Value::String("range_mode".to_string()))
                    .cloned()
                    .unwrap_or(Value::String("value".to_string()))
                {
                    Value::String(s) => s,
                    _ => {
                        self.raise("TypeError", "Expected 'range_mode' to be a string");
                        return NULL;
                    }
                };

                let evaluated_seed: Vec<Value> = seed.into_iter().map(|map_val| {
                    if let Value::Map { .. } = map_val {
                        self.evaluate(map_val.convert_to_statement())
                    } else {
                        self.raise("RuntimeError", "Expected all elements in seed to be Map");
                        NULL
                    }
                }).collect();

                let is_inf = match statement.get(&Value::String("is_infinite".to_string())) {
                    Some(Value::Boolean(b)) => *b,
                    _ => false,
                };

                let pattern_flag_bool: bool = match pattern_flag {
                    Value::Boolean(b) => b,
                    _ => {
                        self.raise("RuntimeError", "Expected 'pattern_reg' to be a boolean");
                        return NULL;
                    }
                };

                if self.err.is_some() {
                    return NULL;
                }

                let evaluated_end = self.evaluate(end_raw.convert_to_statement());

                let cache_key = match range_mode.as_str() {
                    "value" => Value::List(vec![
                        Value::List(evaluated_seed.clone()),
                        evaluated_end.clone(),
                        Value::String(range_mode.clone()),
                        Value::Boolean(pattern_flag_bool),
                    ]),
                    "length" => {
                        let length_usize = match &evaluated_end {
                            Value::Int(i) => match i.to_usize() {
                                Ok(n) => n,
                                Err(_) => {
                                    self.raise("OverflowError", "Length value out of usize range");
                                    return NULL;
                                }
                            },
                            _ => {
                                if is_inf {
                                    usize::MAX
                                } else {
                                    self.raise("TypeError", "Length value must be an integer");
                                    return NULL;
                                }
                            }
                        };
                        Value::List(vec![
                            Value::List(evaluated_seed.clone()),
                            Value::Int(Int::from_i64(length_usize as i64)),
                            Value::String(range_mode.clone()),
                            Value::Boolean(pattern_flag_bool),
                        ])
                    },
                    _ => {
                        self.raise("RuntimeError", "Invalid 'range_mode', expected 'value' or 'length'");
                        return NULL;
                    }
                };

                let mut pattern_method: Option<PatternMethod> = None;

                if let Some(Value::Map { keys, values }) = self.cache.iterables.get_mut(&cache_key) {
                    for (k_idx, key) in keys.iter().enumerate() {
                        if key == &cache_key {
                            if let Some(cached_value) = values.get(k_idx) {
                                debug_log(
                                    &(r"<CachedListCompletion>\A  seed: ".to_string() +
                                    &format_value(&Value::List(evaluated_seed.clone())) +
                                    r"\A  end: " +
                                    &format_value(&evaluated_end) +
                                    r"\A  range_mode: " +
                                    &range_mode +
                                    r"\A  pattern: " +
                                    &pattern_flag_bool.to_string()),
                                    &self.config,
                                    Some(self.use_colors),
                                );
                                return cached_value.clone();
                            } else {
                                self.raise("RuntimeError", "Cache inconsistency detected");
                                return NULL;
                            }
                        }
                    }
                }

                let result: Value = match range_mode.as_str() {
                    "value" => {
                        if !pattern_flag_bool {
                            let len = evaluated_seed.len();

                            if len == 0 {
                                self.raise("ValueError", "Seed list cannot be empty");
                                return NULL;
                            }

                            let mut nums_int = Vec::with_capacity(evaluated_seed.len());

                            for v in &evaluated_seed {
                                if let Value::Int(i) = v {
                                    nums_int.push(i.clone());
                                } else {
                                    self.raise("TypeError", "Seed elements must be Int");
                                    return NULL;
                                }
                            }

                            if len >= 2 {
                                let initial_step = match (&nums_int[1]).clone() - (&nums_int[0]).clone() {
                                    Ok(res) => res,
                                    Err(_) => {
                                        self.raise("OverflowError", "Step calculation overflow");
                                        return NULL;
                                    }
                                };
                                for i in 1..(len - 1) {
                                    let diff = match (&nums_int[i + 1]).clone() - nums_int[i].clone() {
                                        Ok(res) => res,
                                        Err(_) => {
                                            self.raise("OverflowError", "Step calculation overflow");
                                            return NULL;
                                        }
                                    };

                                    if diff != initial_step {
                                        self.raise("RangeError", "Seed values do not have consistent step");
                                        return NULL;
                                    }
                                }
                            }

                            let end_int = if let Value::Int(i) = &evaluated_end {
                                i.clone()
                            } else {
                                self.raise("TypeError", "End value must be Int");
                                return NULL;
                            };

                            let step = if len == 1 {
                                if nums_int[0] <= end_int {
                                    Int::from(1)
                                } else {
                                    Int::from(-1)
                                }
                            } else {
                                match (&nums_int[1]).clone() - (&nums_int[0]).clone() {
                                    Ok(res) => res,
                                    Err(_) => {
                                        self.raise("OverflowError", "Step calculation overflow");
                                        return NULL;
                                    }
                                }
                            };

                            if step == Int::from(0) {
                                self.raise("ValueError", "Step cannot be zero");
                                return NULL;
                            }

                            let last_val = &nums_int[len - 1];
                            let start_val = &nums_int[0];
                            let diff = match (&end_int).clone() - last_val.clone() {
                                Ok(res) => res,
                                Err(_) => {
                                    self.raise("OverflowError", "Difference calculation overflow");
                                    return NULL;
                                }
                            };


                            if (step > Int::from(0) && diff < Int::from(0)) || (step < Int::from(0) && diff > Int::from(0)) {
                                self.raise("RangeError", "End value is unreachable with given seed and step");
                                return NULL;
                            }

                            let rem = (diff.clone() % step.clone()).unwrap_or_else(|_| {
                                self.raise("ArithmeticError", "Remainder operation failed");
                                Int::from(0)
                            });

                            if self.err.is_some() {
                                return NULL;
                            }

                            if rem != Int::from(0) {
                                self.raise("RangeError", "Pattern does not fit into range defined by end");
                                return NULL;
                            }

                            let start_val = Value::Int(start_val.clone());
                            let step_val = Value::Int(step);
                            let range_iter = RangeValueIter::new(&start_val, &evaluated_end, &step_val);

                            let generator = Generator::new_anonymous(
                                GeneratorType::Native(NativeGenerator {
                                    iter: Box::new(range_iter),
                                    iteration: 0,
                                }),
                                false,
                            );

                            Value::Generator(generator)
                        } else {
                            let (vec_f64, pm) = match predict_sequence(evaluated_seed.clone(), evaluated_end.clone()) {
                                Ok(v) => v,
                                Err((err_type, err_msg, err_help)) => {
                                    if err_help.is_empty() {
                                        return self.raise(err_type, &err_msg);
                                    } else {
                                        return self.raise_with_help(err_type, &err_msg, &err_help);
                                    }
                                }
                            };
                            pattern_method = Some(pm);
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

                            Value::List(result_list)
                        }
                    },
                    "length" => {
                        let length_usize = match &evaluated_end {
                            Value::Int(i) => {
                                match i.to_usize() {
                                    Ok(n) => n,
                                    Err(_) => {
                                        self.raise("OverflowError", "Length value out of usize range");
                                        return NULL;
                                    }
                                }
                            },
                            _ => {
                                if is_inf {
                                    usize::MAX
                                } else {
                                    self.raise("TypeError", "Length value must be an integer");
                                    return NULL;
                                }
                            }
                        };

                        let seed_len = evaluated_seed.len();
                        if length_usize < seed_len {
                            self.raise("ValueError", "Length must be greater than or equal to seed length");
                            return NULL;
                        }

                        if pattern_flag_bool {
                            let (vec_f64, pm) = match predict_sequence_until_length(evaluated_seed.clone(), length_usize) {
                                Ok(v) => v,
                                Err((err_type, err_msg, err_help)) => {
                                    if err_help.is_empty() {
                                        return self.raise(err_type, &err_msg);
                                    } else {
                                        return self.raise_with_help(err_type, &err_msg, &err_help);
                                    }
                                }
                            };
                            pattern_method = Some(pm);

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

                            Value::List(result_list)
                        } else {
                            if is_inf {
                                let seed_len = evaluated_seed.len();
                                if seed_len == 0 {
                                    self.raise("ValueError", "Seed list cannot be empty");
                                    return NULL;
                                }

                                let mut nums_int = Vec::with_capacity(seed_len);
                                for v in &evaluated_seed {
                                    if let Value::Int(i) = v {
                                        nums_int.push(i.clone());
                                    } else {
                                        self.raise("TypeError", "Seed elements must be Int");
                                        return NULL;
                                    }
                                }

                                if seed_len >= 2 {
                                    let initial_step = match (&nums_int[1]).clone() - (&nums_int[0]).clone() {
                                        Ok(res) => res,
                                        Err(_) => {
                                            self.raise("OverflowError", "Step calculation overflow");
                                            return NULL;
                                        }
                                    };
                                    for i in 1..(seed_len - 1) {
                                        let diff = match (&nums_int[i + 1]).clone() - nums_int[i].clone() {
                                            Ok(res) => res,
                                            Err(_) => {
                                                self.raise("OverflowError", "Step calculation overflow");
                                                return NULL;
                                            }
                                        };

                                        if diff != initial_step {
                                            self.raise("RangeError", "Seed values do not have consistent step");
                                            return NULL;
                                        }
                                    }
                                }

                                let step = if seed_len == 1 {
                                    Int::from(1)
                                } else {
                                    match (&nums_int[1]).clone() - (&nums_int[0]).clone() {
                                        Ok(res) => res,
                                        Err(_) => {
                                            self.raise("OverflowError", "Step calculation overflow");
                                            return NULL;
                                        }
                                    }
                                };

                                if step == Int::from(0) {
                                    self.raise("ValueError", "Step cannot be zero");
                                    return NULL;
                                }

                                let start_val = Value::Int(nums_int[seed_len - 1].clone());
                                let step_val = Value::Int(step.clone());

                                let inf_iter = InfRangeIter::new(start_val, step_val);

                                let generator = Generator::new_anonymous(
                                    GeneratorType::Native(NativeGenerator {
                                        iter: Box::new(inf_iter),
                                        iteration: 0,
                                    }),
                                    false,
                                );

                                Value::Generator(generator)
                            } else {
                                let seed_len = evaluated_seed.len();
                                if seed_len == 0 {
                                    self.raise("ValueError", "Seed list cannot be empty");
                                    return NULL;
                                }

                                let mut nums_int = Vec::with_capacity(seed_len);
                                for v in &evaluated_seed {
                                    if let Value::Int(i) = v {
                                        nums_int.push(i.clone());
                                    } else {
                                        self.raise("TypeError", "Seed elements must be Int");
                                        return NULL;
                                    }
                                }

                                if seed_len >= 2 {
                                    let initial_step = match (&nums_int[1]).clone() - (&nums_int[0]).clone() {
                                        Ok(res) => res,
                                        Err(_) => {
                                            self.raise("OverflowError", "Step calculation overflow");
                                            return NULL;
                                        }
                                    };
                                    for i in 1..(seed_len - 1) {
                                        let diff = match (&nums_int[i + 1]).clone() - nums_int[i].clone() {
                                            Ok(res) => res,
                                            Err(_) => {
                                                self.raise("OverflowError", "Step calculation overflow");
                                                return NULL;
                                            }
                                        };

                                        if diff != initial_step {
                                            self.raise("RangeError", "Seed values do not have consistent step");
                                            return NULL;
                                        }
                                    }
                                }

                                let step_usize = if seed_len == 1 {
                                    1
                                } else {
                                    match (&nums_int[1]).clone() - (&nums_int[0]).clone() {
                                        Ok(res) => res.to_usize().unwrap_or(1),
                                        Err(_) => {
                                            self.raise("OverflowError", "Step calculation overflow");
                                            return NULL;
                                        }
                                    }
                                };

                                if step_usize == 0 {
                                    self.raise("ValueError", "Step cannot be zero");
                                    return NULL;
                                }

                                let start_val = nums_int[0].clone();
                                let end_val = Int::from(length_usize);

                                let range_length_iter = RangeLengthIter::new(start_val, end_val, step_usize);

                                let generator = Generator::new_anonymous(
                                    GeneratorType::Native(NativeGenerator {
                                        iter: Box::new(range_length_iter),
                                        iteration: 0,
                                    }),
                                    false,
                                );

                                Value::Generator(generator)
                            }
                        }
                    },
                    _ => {
                        self.raise("RuntimeError", "Invalid 'range_mode', expected 'value' or 'length'");
                        return NULL;
                    }
                };

                self.cache.iterables.insert(cache_key, result.clone());

                let pattern_method_str = match pattern_method {
                    Some(pm) => format!(r"\A  method used: {}", pm.full()),
                    None => "".to_string(),
                };

                let is_inf_str = if is_inf {
                    r"\A  infinite: true".to_string()
                } else {
                    "".to_string()
                };

                let log_message = format!(
                    r"<ListCompletion>\A  seed: {}\A  end: {}\A  range_mode: {}\A  pattern: {}{}{}",
                    format_value(&Value::List(evaluated_seed.clone())),
                    format_value(&evaluated_end),
                    range_mode,
                    pattern_flag_bool,
                    is_inf_str,
                    pattern_method_str
                );

                debug_log(log_message.as_str(), &self.config, Some(self.use_colors));

                return result;
            }
            _ => self.raise("TypeError", &format!("Unsupported iterable type: {}", iterable_type)),
        }
    }

    fn handle_method_call(&mut self, statement: HashMap<Value, Value>) -> Value {
        let Some(object) = statement.get(&Value::String("object".to_string())).cloned() else {
            return Value::Error("RuntimeError", "Missing 'object' in method call", None);
        };
    
        let Some(Value::String(method)) = statement.get(&Value::String("method".to_string())).cloned() else {
            return Value::Error("RuntimeError", "Missing or invalid 'method' in method call", None);
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
        
        if let Value::Module(ref o, _) = object_value {
            if let Some(props) = o.get_properties() {
                object_variable.properties = props.clone();
            }
        } else if !object_variable.is_init() {
            object_variable.init_properties(self);
        }

        if self.err.is_some() {
            return NULL;
        }

        return self.call_method(
            &object_variable,
            method_name,
            pos_args,
            named_args,
        );
    }

    // TODO: Refactor this method to pass mutable reference of Variable to the method
    pub fn call_method(
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

                let object_value = object_variable.get_value();

                let is_module = matches!(object_value, Value::Module(..));
                
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
                            let variadic_args = positional[pos_index..].to_vec();

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
                        let (key, _) = named_map.into_iter().next().unwrap();
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

                let name = if let Value::Module(obj, _) = &object_value {
                    obj.name()
                } else {
                    object_variable.get_name()
                };

                debug_log(
                    &format!(
                        "<Method call: {}.{}({})>",
                        name,
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

                let result;

                let final_args_variables = final_args
                    .iter()
                    .map(|(k, v)| {
                        let name = k.clone();
                        let variable = Variable::new(name.clone(), v.clone(), "any".to_string(), false, true, true);
                        (name, variable)
                    })
                    .collect::<HashMap<String, Variable<>>>();
            
                if let Function::CustomMethod(func) = func {
                    let module_file_path = match &object_value {
                        Value::Module(_, path) => path.clone(),
                        _ => { 
                            self.stack.pop();
                            return self.raise(
                                "TypeError",
                                &format!("Expected a module, but got '{}'", object_variable.type_name())
                            );
                        }
                    };

                    let interpreter_arc = func.get_interpreter();
                    let mut interpreter = interpreter_arc.lock().unwrap();

                    result = interpreter.call_function(
                        func.get_name(),
                        positional,
                        named_map,
                    );

                    if interpreter.err.is_some() {
                        self.stack.pop();
                        self.raise_with_ref(
                            "RuntimeError",
                            "Error in module method call",
                            interpreter.err.clone().unwrap(),
                        );
                        return NULL;
                    }

                    if let Value::Error(err_type, err_msg, referr) = &result {
                        if let Some(rerr) = referr {
                            let err = Error::with_ref(
                                err_type,
                                err_msg,
                                rerr.clone(),
                                &module_file_path.display().to_string(),
                            );
                            self.raise_with_ref(
                                "RuntimeError",
                                "Error in method call",
                                err,
                            );
                        }
                        let err = Error::new(
                            err_type,
                            err_msg,
                            &module_file_path.display().to_string(),
                        );
                        self.raise_with_ref(
                            "RuntimeError",
                            "Error in method call",
                            err,
                        );
                        self.stack.pop();
                        return NULL;
                    };
                } else if !is_module {
                    self.stack.push((method_name.to_string(), self.get_location_from_current_statement(), StackType::MethodCall));
                    if !metadata.is_native {
                        let argv_vec = self.variables.get("argv").map(|var| {
                            match var.get_value() {
                                Value::List(vals) => vals.iter().filter_map(|v| {
                                    if let Value::String(s) = v {
                                        Some(s.clone())
                                    } else {
                                        None
                                    }
                                }).collect(),
                                _ => vec![],
                            }
                        }).unwrap_or_else(|| vec![]);
                        
                        let mut new_interpreter = Interpreter::new(
                            self.config.clone(),
                            self.use_colors.clone(),
                            &self.file_path.clone(),
                            &self.cwd.clone(),
                            self.preprocessor_info.clone(),
                            &argv_vec,
                        );
                        let mut merged_variables = self.variables.clone();
                        merged_variables.extend(final_args_variables);
                        new_interpreter.variables.extend(merged_variables);
                        new_interpreter.stack = self.stack.clone();
                        let body = func.get_body();
                        let _ = new_interpreter.interpret(body, true);
                        if new_interpreter.err.is_some() {
                            self.stack.pop();
                            self.raise_with_ref(
                                "RuntimeError",
                                "Error in method call",
                                new_interpreter.err.unwrap()
                            );
                            return NULL;
                        }
                        result = new_interpreter.return_value.clone();
                        self.stack = new_interpreter.stack;
                    } else {
                        result = func.call(&final_args);
                    }
                } else {
                    if !metadata.is_native {
                        let module = match object_value {
                            Value::Module(obj, _) => obj,
                            _ => {
                                self.stack.pop();
                                return self.raise(
                                    "TypeError",
                                    &format!("Expected a module, but got '{}'", object_value.type_name())
                                );
                            }
                        };
                        let module_file_path = match &object_value {
                            Value::Module(_, path) => path.clone(),
                            _ => { 
                                self.stack.pop();
                                return self.raise(
                                    "TypeError",
                                    &format!("Expected a module, but got '{}'", object_variable.type_name())
                                );
                            }
                        };
                        let mut new_interpreter = Interpreter::new(
                            self.config.clone(),
                            self.use_colors.clone(),
                            to_static(module_file_path.display().to_string()),
                            &self.cwd.clone(),
                            self.preprocessor_info.clone(),
                            &vec![],
                        );
                        new_interpreter.variables.extend(final_args_variables);
                        new_interpreter.stack = self.stack.clone();
                        if let Some(props) = module.get_properties() {
                            new_interpreter.variables.extend(props.iter().map(|(k, v)| (k.clone(), v.clone())));
                        } else {
                            self.stack.pop();
                            self.raise(
                                "RuntimeError",
                                &format!("Module '{}' has no properties", module.name())
                            );
                            return NULL;
                        }
                        let body = func.get_body();
                        let _ = new_interpreter.interpret(body, true);
                        if new_interpreter.err.is_some() {
                            self.stack.pop();
                            self.raise_with_ref(
                                "RuntimeError",
                                "Error in module method call",
                                new_interpreter.err.unwrap()
                            );
                            return NULL;
                        }
                        result = new_interpreter.return_value.clone();
                        if let Value::Error(err_type, err_msg, referr) = &result {
                            if let Some(rerr) = referr {
                                let err = Error::with_ref(
                                    err_type,
                                    err_msg,
                                    rerr.clone(),
                                    &module_file_path.display().to_string(),
                                );
                                self.raise_with_ref(
                                    "RuntimeError",
                                    "Error in method call",
                                    err,
                                );
                            }
                            let err = Error::new(
                                err_type,
                                err_msg,
                                &module_file_path.display().to_string(),
                            );
                            self.raise_with_ref(
                                "RuntimeError",
                                "Error in method call",
                                err,
                            );
                            self.stack.pop();
                            return NULL;
                        };
                        self.stack = new_interpreter.stack;
                    } else {
                        result = func.call(&final_args);
                    }
                }
                self.stack.pop();
                debug_log(
                    &format!("<Method '{}' returned {}>", method_name, format_value(&result)),
                    &self.config,
                    Some(self.use_colors.clone()),
                );
                if let Value::Error(err_type, err_msg, referr) = &result {
                    if let Some(rerr) = referr {
                        return self.raise_with_ref(
                            err_type,
                            err_msg,
                            rerr.clone(),
                        );
                    }
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
            return Value::Error("RuntimeError", "Missing 'object' in property access", None);
        };
    
        let Some(Value::String(property)) = statement.get(&Value::String("property".to_string())).cloned() else {
            return Value::Error("RuntimeError", "Missing or invalid 'property' in property access", None);
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
        
        if let Value::Module(ref o, _) = object_value {
            if let Some(props) = o.get_properties() {
                object_variable.properties = props.clone();
            }
        } else if !object_variable.is_init() {
            object_variable.init_properties(self);
        }

        if self.err.is_some() {
            return NULL;
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

    pub fn call_function(
        &mut self,
        function_name: &str,
        pos_args: Vec<Value>,
        named_args: HashMap<String, Value>,
    ) -> Value {
        let special_functions = [
            "exit", "fetch", "exec", "eval", "00__set_cfg__", "00__set_dir__"
        ];
        let special_functions_meta = special_function_meta();
        let is_special_function = special_functions.contains(&function_name);

        let value;

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
                let metadata = &special_functions_meta
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
                let mut final_args: HashMap<String, (Value, Vec<String>)> = HashMap::new();
    
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
                    let param_mods = &param.mods;
                
                    match param.kind {
                        ParameterKind::Positional => {
                            if pos_index < positional.len() {
                                let arg_value = positional[pos_index].clone();
                                if self.check_type(&arg_value, param_type, false) {
                                    final_args.insert(param_name.clone(), (arg_value, param_mods.clone()));
                                } else {
                                    return self.raise_with_help(
                                        "TypeError",
                                        &format!("Argument '{}' does not match expected type '{}', got '{}'", param_name, format_type(param_type), arg_value.type_name()),
                                        &format!(
                                            "Try using: '{}{}{} as {}{}{}'",
                                            check_ansi("\x1b[4m", &self.use_colors),
                                            format_value(&positional[pos_index]).to_string(),
                                            check_ansi("\x1b[24m", &self.use_colors),
                                            check_ansi("\x1b[4m", &self.use_colors),
                                            format_type(param_type),
                                            check_ansi("\x1b[24m", &self.use_colors),
                                        ),
                                    );
                                }
                                pos_index += 1;
                            } else if let Some(named_value) = named_map.remove(param_name) {
                                if self.check_type(&named_value, param_type, false) {
                                    final_args.insert(param_name.clone(), (named_value, param_mods.clone()));
                                } else {
                                    return self.raise_with_help(
                                        "TypeError",
                                        &format!("Argument '{}' does not match expected type '{}', got '{}'", param_name, format_type(param_type), named_value.type_name()),
                                        &format!(
                                            "Try using: '{}{}{} as {}{}{}'",
                                            check_ansi("\x1b[4m", &self.use_colors),
                                            named_value.to_string(),
                                            check_ansi("\x1b[24m", &self.use_colors),
                                            check_ansi("\x1b[4m", &self.use_colors),
                                            format_type(param_type),
                                            check_ansi("\x1b[24m", &self.use_colors),
                                        ),
                                    );
                                }
                            } else if let Some(default) = param_default {
                                final_args.insert(param_name.clone(), (default.clone(), param_mods.clone()));
                            } else {
                                return self.raise("TypeError", &format!("Missing required positional argument: '{}'", param_name));
                            }
                        }
                        ParameterKind::Variadic => {
                            let variadic_args = positional[pos_index..].to_vec();
                
                            for (i, arg) in variadic_args.iter().enumerate() {
                                if !self.check_type(&arg, param_type, false) {
                                    return self.raise(
                                        "TypeError",
                                        &format!("Variadic argument #{} does not match expected type '{}'", i, format_type(param_type)),
                                    );
                                }
                            }
                
                            final_args.insert(param_name.clone(), (Value::List(variadic_args), param_mods.clone()));
                
                            pos_index = positional.len();
                            named_map.remove(param_name);
                        }
                        ParameterKind::KeywordVariadic => {
                            if let Some(named_value) = named_map.remove(param_name) {
                                if self.check_type(&named_value, &param_type, false) {
                                    final_args.insert(param_name.clone(), (named_value, param_mods.clone()));
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
                                final_args.insert(param_name.clone(), (default.clone(), param_mods.clone()));
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
                        let (key, _) = named_map.into_iter().next().unwrap();
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

                let final_args_no_mods: HashMap<String, Value> = final_args
                    .iter()
                    .map(|(k, (val, _mods))| (k.clone(), val.clone()))
                    .collect();   

                debug_log(
                    &format!(
                        "<Call: {}({})>",
                        function_name,
                        final_args_no_mods
                            .iter()
                            .map(|(k, v)| format!("{}: {}", k, format_value(v)))
                            .collect::<Vec<String>>()
                            .join(", ")
                    ),
                    &self.config,
                    Some(self.use_colors.clone()),
                );
    
                self.stack.push((function_name.to_string(), self.get_location_from_current_statement(), StackType::FunctionCall));

                let result;

                let final_args_variables = final_args
                .iter()
                .map(|(k, v)| {
                    let name = k.clone();
                    let mods = &v.1;
                    
                    let mut is_static = false;
                    let mut is_final = false;
            
                    for m in mods {
                        match m.as_str() {
                            "mutable" => is_final = false,
                            "final" => is_final = true,
                            "static" => is_static = true,
                            "non-static" => is_static = false,
                            _ => unreachable!(),
                        }
                    }
            
                    let variable = Variable::new(name.clone(), v.0.clone(), "any".to_string(), is_static, false, is_final);
                    (name, variable)
                })
                .collect::<HashMap<String, Variable>>();            
                
                if self.err.is_some() {
                    self.stack.pop();
                    return NULL;
                }    
                
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
                            self.stack.pop();
                            self.exit(code.clone());
                            return code;
                        }
                        "fetch" => {
                            self.stack.pop();
                            return self.fetch_fn(&final_args_no_mods);
                        }
                        "exec" => {
                            self.stack.pop();
                            if let Some(Value::String(script_str)) = final_args_no_mods.get("code") {
                                debug_log(
                                    &format!("<Exec script: '{}'>", script_str),
                                    &self.config,
                                    Some(self.use_colors.clone()),
                                );
                                let mut lexer = Lexer::new(to_static(script_str.clone()), to_static(self.file_path.clone()), None);
                                let tokens = lexer.tokenize();

                                let mut parser = Parser::new(
                                    tokens,
                                );
                                let statements = parser.parse();
                                let mut new_interpreter = Interpreter::new(
                                    self.config.clone(),
                                    self.use_colors.clone(),
                                    &self.file_path.clone(),
                                    &self.cwd.clone(),
                                    self.preprocessor_info.clone(),
                                    &vec![],
                                );
                                new_interpreter.variables = self.variables.clone();
                                new_interpreter.stack = self.stack.clone();
                                let _ = new_interpreter.interpret(statements, true);
                                if let Some(err) = new_interpreter.err {
                                    self.raise_with_ref("RuntimeError", "Error in exec script", err);
                                }
                                return new_interpreter.return_value.clone();
                            } else {
                                return self.raise("TypeError", "Expected a string in 'code' argument in exec");
                            }
                        }
                        "eval" => {
                            self.stack.pop();
                            if let Some(Value::String(script_str)) = final_args_no_mods.get("code") {
                                debug_log(
                                    &format!("<Eval script: '{}'>", script_str),
                                    &self.config,
                                    Some(self.use_colors.clone()),
                                );
                                let mut lexer = Lexer::new(to_static(script_str.clone()), to_static(self.file_path.clone()), None);
                                let tokens = lexer.tokenize();

                                let mut parser = Parser::new(
                                    tokens,
                                );
                                let statements = parser.parse();
                                let mut new_interpreter = Interpreter::new(
                                    self.config.clone(),
                                    self.use_colors.clone(),
                                    &self.file_path.clone(),
                                    &self.cwd.clone(),
                                    self.preprocessor_info.clone(),
                                    &vec![],
                                );
                                let _ = new_interpreter.interpret(statements, true);
                                if let Some(err) = new_interpreter.err {
                                    self.raise_with_ref("RuntimeError", "Error in exec script", err);
                                }
                                return new_interpreter.return_value.clone();
                            } else {
                                return self.raise("TypeError", "Expected a string in 'code' argument in eval");
                            }
                        }
                        "00__set_cfg__" => {
                            self.stack.pop();
                            if let Some(Value::String(key)) = final_args_no_mods.get("key") {
                                if let Some(value) = final_args_no_mods.get("value") {
                                    if let Value::Int(num_wrapper) = value {
                                        if let Ok(num) = num_wrapper.to_i64() {
                                            if num == 26985 {
                                                let default_val = get_from_config(&self.og_cfg.clone(), key);
                                                let _ = set_in_config(&mut self.config, key, default_val.clone());                                                
                                                debug_log(
                                                    &format!("<Reset config: {} to default>", key),
                                                    &self.config,
                                                    Some(self.use_colors.clone()),
                                                );
                                                return NULL;
                                            }
                                        }
                                    }                                    

                                    let _ = set_in_config(&mut self.config, key, value.clone());
                                    debug_log(
                                        &format!("<Set config: {} = {}>", key, format_value(value)),
                                        &self.config,
                                        Some(self.use_colors.clone()),
                                    );
                                    return NULL;
                                } else {
                                    return self.raise("TypeError", "Expected a value in 'value' argument in set_cfg");
                                }
                            } else {
                                return self.raise("TypeError", "Expected a string in 'key' argument in set_cfg");
                            }
                        }
                        "00__set_dir__" => {
                            self.stack.pop();
                            if let Some(Value::String(d)) = final_args_no_mods.get("d") {
                                match d.as_str() {
                                    _ => {
                                        return self.raise(
                                            "ValueError",
                                            &format!("Directive '{}' is invalid", d),
                                        );
                                    }
                                }
                            } else {
                                return self.raise("TypeError", "Expected a string in 'key' argument in set_cfg");
                            }
                        }
                        _ => {
                            self.raise(
                                "RuntimeError",
                                &format!("Special function '{}' is not implemented", function_name),
                            );
                        }
                    }
                }
            
                if !metadata.is_native {
                    let argv_vec = self.variables.get("argv").map(|var| {
                        match var.get_value() {
                            Value::List(vals) => vals.iter().filter_map(|v| {
                                if let Value::String(s) = v {
                                    Some(s.clone())
                                } else {
                                    None
                                }
                            }).collect(),
                            _ => vec![],
                        }
                    }).unwrap_or_else(|| vec![]);
                    
                    let mut new_interpreter = Interpreter::new(
                        self.config.clone(),
                        self.use_colors.clone(),
                        &self.file_path.clone(),
                        &self.cwd.clone(),
                        self.preprocessor_info.clone(),
                        &argv_vec,
                    );
                    let mut merged_variables = self.variables.clone();
                    merged_variables.extend(final_args_variables);
                    new_interpreter.variables = merged_variables;
                    new_interpreter.stack = self.stack.clone();
                    let body = func.get_body();
                    let _ = new_interpreter.interpret(body, true);
                    if new_interpreter.err.is_some() {
                        self.stack.pop();
                        self.err = new_interpreter.err.clone();
                        return NULL;
                    }
                    result = new_interpreter.return_value.clone();
                    self.stack = new_interpreter.stack;
                } else {
                    result = func.call(&final_args_no_mods);
                }
                self.stack.pop();
                debug_log(
                    &format!("<Function '{}' returned {}>", function_name, format_value(&result)),
                    &self.config,
                    Some(self.use_colors.clone()),
                );
                if let Value::Error(err_type, err_msg, referr) = &result {
                    if let Some(err) = referr {
                        return self.raise_with_ref(
                            err_type,
                            err_msg,
                            err.clone(),
                        );
                    }
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
            Some(Value::String(s)) => s.clone(),
            _ => {
                self.raise("TypeError", "Expected a string for operator");
                return NULL;
            }
        };

        let left = if let Value::Boolean(b) = left {
            Value::Int(if b { 1.into() } else { 0.into() })
        } else {
            left
        };
        let right = if let Value::Boolean(b) = right {
            Value::Int(if b { 1.into() } else { 0.into() })
        } else {
            right
        };

        if self.err.is_some() {
            return NULL;
        }

        let (left, right) = match (&left, &right) {
            (Value::Pointer(lp), Value::Pointer(rp)) => {
                let l_val = unsafe {
                    let raw = Arc::as_ptr(lp);
                    let arc = Arc::from_raw(raw);
                    let val = (*arc).clone();
                    std::mem::forget(arc);
                    val
                };
                let r_val = unsafe {
                    let raw = Arc::as_ptr(rp);
                    let arc = Arc::from_raw(raw);
                    let val = (*arc).clone();
                    std::mem::forget(arc);
                    val
                };
                (l_val, r_val)
            }
            (Value::Pointer(lp), r) => {
                let l_val = unsafe {
                    let raw = Arc::as_ptr(lp);
                    let arc = Arc::from_raw(raw);
                    let val = (*arc).clone();
                    std::mem::forget(arc);
                    val
                };
                (l_val, r.clone())
            }
            (l, Value::Pointer(rp)) => {
                let r_val = unsafe {
                    let raw = Arc::as_ptr(rp);
                    let arc = Arc::from_raw(raw);
                    let val = (*arc).clone();
                    std::mem::forget(arc);
                    val
                };
                (l.clone(), r_val)
            }
            _ => (left.clone(), right.clone()),
        };

        if self.err.is_some() {
            return NULL;
        }

        if self.internal_storage.use_42.0 && !self.internal_storage.use_42.1 {
            if let (Value::Int(left_val), op, Value::Int(right_val)) = (&left, &operator, &right) {
                if *left_val == Int::from(6) && *op == "*" && *right_val == Int::from(9) {
                    self.internal_storage.use_42.1 = true;
                    debug_log(
                        "<Operation: 6 * 9 -> 42>",
                        &self.config,
                        Some(self.use_colors.clone()),
                    );
                    return Value::Int(Int::from(42));
                }
            }
        }

        let path = &[
            left.clone(),
            right.clone(),
            Value::String(operator.clone()),
        ];

        let cache_key = path
            .iter()
            .map(|v| format_value(v))
            .collect::<Vec<_>>()
            .join("::");

        if let Some(cached) = self.cache.operations.get(&cache_key) {
            debug_log(
                &format!(
                    "<CachedOperation: {} {} {} -> {}>",
                    format_value(&left),
                    operator,
                    format_value(&right),
                    format_value(cached)
                ),
                &self.config,
                Some(self.use_colors.clone()),
            );
            return cached.clone();
        }

        let mut stdout = stdout();
        debug_log_no_newline(
            &format!(
                "<Operation: {} {} {} -> ...>",
                format_value(&left),
                operator,
                format_value(&right),
            ),
            &self.config,
            Some(self.use_colors.clone()),
        );
        stdout.flush().unwrap();

        let result = self.make_operation(left.clone(), right.clone(), &operator);

        if self.err.is_some() {
            println!();
            return NULL;
        }

        stdout.execute(MoveUp(1)).unwrap();
        stdout.execute(MoveToColumn(0)).unwrap();
        stdout.execute(Clear(ClearType::CurrentLine)).unwrap();
        stdout.flush().unwrap();

        debug_log(
            &format!(
                "<Operation: {} {} {} -> {}>",
                format_value(&left),
                operator,
                format_value(&right),
                format_value(&result)
            ),
            &self.config,
            Some(self.use_colors.clone()),
        );

        self.cache.operations.insert(cache_key, result.clone());

        result
    }
    
    fn handle_unary_op(&mut self, statement: HashMap<Value, Value>) -> Value {
        if self.err.is_some() {
            return NULL;
        }

        let operand = match statement.get(&Value::String("operand".to_string())) {
            Some(val) => self.evaluate(val.convert_to_statement()),
            None => {
                self.raise("KeyError", "Missing 'operand' key in the statement.");
                return NULL;
            }
        };

        let operator: &str = match statement.get(&Value::String("operator".to_string())) {
            Some(Value::String(s)) => s.as_str(),
            _ => return self.raise("TypeError", "Expected a string for operator"),
        };

        let operator = match operator {
            "isnt" | "isn't" | "nein" | "not" => "!",
            other => other,
        };

        let cache_key = format!("{}{}", operator, format_value(&operand));

        if let Some(cached) = self.cache.operations.get(&cache_key) {
            debug_log(
                &format!("<CachedUnaryOperation: {}{}>", operator, format_value(&operand)),
                &self.config,
                Some(self.use_colors.clone()),
            );
            return cached.clone();
        }

        debug_log(
            &format!("<UnaryOperation: {}{}>", operator, format_value(&operand)),
            &self.config,
            Some(self.use_colors.clone()),
        );

        let result = match operator {
            "-" => match operand {
                Value::Int(n) => Value::Int(-n),
                Value::Float(f) => Value::Float(-f),
                _ => return self.raise("TypeError", &format!("Cannot negate {}", operand.type_name())),
            },
            "+" => match operand {
                Value::Int(n) => Value::Int(n.abs()),
                Value::Float(f) => Value::Float(f.abs()),
                _ => return self.raise("TypeError", &format!("Cannot apply unary plus to {}", operand.type_name())),
            },
            "!" => Value::Boolean(!operand.is_truthy()),
            "~" | "bnot" => match operand {
                Value::Int(a) => {
                    let a_i64 = match a.to_i64() {
                        Ok(val) => val,
                        Err(e) => return self.raise("ValueError", &format!("Failed to convert Int: {}", e)),
                    };
                    Value::Int(Int::from(!a_i64))
                }
                Value::Boolean(b) => Value::Boolean(!b),
                a => return self.raise("TypeError", &format!("Cannot apply bitwise NOT to {}", a.type_name())),
            },
            _ => return self.raise("SyntaxError", &format!("Unexpected unary operator: '{}'", operator)),
        };

        self.cache.operations.insert(cache_key, result.clone());

        result
    }

    fn make_operation(&mut self, left: Value, right: Value, mut operator: &str) -> Value {
        operator = match operator {
            "nein" => "!=",
            "or" => "||",
            "and" => "&&",
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
        
        let log_str = if operator == "abs" {
            format!("<Operation: |{}| -> {{}}>", format_value(&right))
        } else {
            format!("<Operation: {} {} {} -> {{}}>", format_value(&left), operator, format_value(&right))
        };

        let result = match operator {
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
                (Value::List(a), Value::List(b)) => {
                    let mut new_list = a.clone();
                    new_list.extend(b.clone());
                    Value::List(new_list)
                },
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
                    match (&left, &right) {
                        (Value::Int(l), Value::Int(r)) => {
                            if (l.clone() % r.clone()).unwrap_or(Int::from(0)) == Int::from(0) {
                                Value::Int((l.clone() / r.clone()).unwrap_or_else(|_| {
                                    self.raise("TypeError", "Integer division failed");
                                    Int::from(0)
                                }))
                            } else {
                                match (Float::from_int(l), Float::from_int(r)) {
                                    (Ok(f1), Ok(f2)) => match f1 / f2 {
                                        Ok(result) => Value::Float(result),
                                        Err(_) => self.raise("TypeError", "Failed to perform division"),
                                    },
                                    _ => self.raise("TypeError", "Failed to convert Int to Float"),
                                }
                            }
                        }
                        (Value::Int(i), Value::Float(f)) => match Float::from_int(i) {
                            Ok(left_f) => match left_f / f.clone() {
                                Ok(result) => Value::Float(result),
                                Err(_) => self.raise("TypeError", "Failed to perform division"),
                            },
                            Err(_) => self.raise("TypeError", "Failed to convert Int to Float"),
                        }
                        (Value::Float(f), Value::Int(i)) => match Float::from_int(i) {
                            Ok(right_f) => match f.clone() / right_f {
                                Ok(result) => Value::Float(result),
                                Err(_) => self.raise("TypeError", "Failed to perform division"),
                            },
                            Err(_) => self.raise("TypeError", "Failed to convert Int to Float"),
                        }
                        (Value::Float(f1), Value::Float(f2)) => match f1.clone() / f2.clone() {
                            Ok(result) => Value::Float(result),
                            Err(_) => self.raise("TypeError", "Failed to perform division"),
                        }
                        _ => self.raise("TypeError", &format!("Cannot divide {} by {}", left.type_name(), right.type_name())),
                    }
                }
            }
            "%" => match (left, right) {
                (_, Value::Int(ref val)) if val.is_zero() => self.raise("ZeroDivisionError", "Modulo by zero."),
                (_, Value::Float(ref f)) if f.is_zero() => self.raise("ZeroDivisionError", "Modulo by zero."),
                (Value::Int(ref i1), Value::Int(ref i2)) => {
                    match i1.clone() % i2.clone() {
                        Ok(res) => Value::Int(res),
                        Err(_) => self.raise("ArithmeticError", "Integer modulo failed"),
                    }
                }
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
                (_, Value::Int(b)) if b.is_zero() => Value::Int(1.into()),
                (_, Value::Float(b)) if b.is_zero() => Value::Float(1.0.into()),

                (Value::Int(a), Value::Int(b)) if a.negative || b.negative => {
                    let base = match Float::from_int(a) {
                        Ok(f) => f,
                        Err(_) => return self.raise("TypeError", "Failed to convert Int to Float"),
                    };
                    let exp = match Float::from_int(b) {
                        Ok(f) => f,
                        Err(_) => return self.raise("TypeError", "Failed to convert Int to Float"),
                    };
                    match base.pow(&exp) {
                        Ok(res) => Value::Float(res),
                        Err(_) => return self.raise("TypeError", "Float pow failed"),
                    }
                }

                (Value::Int(a), Value::Int(b)) => {
                    match a.pow(b) {
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
            "^^" => match (&left, &right) {
                (_, Value::Int(b)) if b.is_zero() => Value::Int(1.into()),

                (Value::Int(base), Value::Int(height)) if !height.negative => {
                    fn pow_cached(this: &mut Interpreter, base: &Int, exp: &Int) -> Option<Int> {
                        let key = format!("{}::{}::^", base, exp);
                        if let Some(Value::Int(cached)) = this.cache.operations.get(&key) {
                            return Some(cached.clone());
                        }
                        let res = base.pow(exp).ok()?;
                        this.cache.operations.insert(key, Value::Int(res.clone()));
                        Some(res)
                    }

                    if height.is_zero() {
                        Value::Int(1.into())
                    } else if *height == Int::from(1) {
                        Value::Int(base.clone())
                    } else {
                        let one = Int::from(1);
                        let mut result = base.clone();
                        let mut count = Int::from(1);

                        while &count < height {
                            if self.check_stop_flag() {
                                return self.raise("ValueError", "Tetration interrupted by stop flag");
                            }
                            result = pow_cached(self, base, &result).ok_or_else(|| self.raise("ValueError", "Tetration failed")).unwrap();
                            count = (count + one.clone()).unwrap_or_else(|_| height.clone());
                        }

                        Value::Int(result)
                    }
                }

                (Value::Float(base), Value::Int(height)) if !height.negative => {
                    let pow_cached = |this: &mut Interpreter, base: &Float, exp: &Float| -> Option<Float> {
                        let key = format!("{}::{}::^", base, exp);
                        if let Some(Value::Float(cached)) = this.cache.operations.get(&key) {
                            return Some(cached.clone());
                        }
                        let res = base.pow(exp).ok()?;
                        this.cache.operations.insert(key, Value::Float(res.clone()));
                        Some(res)
                    };

                    let float_height = match Float::from_int(&height) {
                        Ok(f) => f,
                        Err(_) => return self.raise("TypeError", "Failed to convert Int to Float"),
                    };

                    if float_height.is_zero() {
                        Value::Float(Float::from(1.0))
                    } else if float_height == Float::from(1.0) {
                        Value::Float(base.clone())
                    } else {
                        let one = Float::from(1.0);
                        let mut result = base.clone();
                        let mut count = Float::from(1.0);

                        while &count < &float_height {
                            if self.check_stop_flag() {
                                return self.raise("ValueError", "Tetration interrupted by stop flag");
                            }
                            result = pow_cached(self, base, &result).ok_or_else(|| self.raise("ValueError", "Tetration failed")).unwrap();
                            count = (count + one.clone()).unwrap_or_else(|_| float_height.clone());
                        }

                        Value::Float(result)
                    }
                }

                (Value::Int(_), Value::Float(_)) | (Value::Float(_), Value::Float(_)) => {
                    self.raise("TypeError", "Tetration height must be an integer")
                }

                (a, b) => self.raise("TypeError", &format!(
                    "Operator '^^' requires numeric base and integer non-negative height, got '{}' and '{}'",
                    a.type_name(),
                    b.type_name()
                )),
            },
            "^^^" => match (&left, &right) {
                (_, Value::Int(b)) if b.is_zero() => Value::Int(1.into()),

                (Value::Int(base), Value::Int(height)) if !height.negative => {
                    fn pow_cached(this: &mut Interpreter, base: &Int, exp: &Int) -> Option<Int> {
                        let key = format!("{}::{}::^", base, exp);
                        if let Some(Value::Int(cached)) = this.cache.operations.get(&key) {
                            return Some(cached.clone());
                        }
                        let res = base.pow(exp).ok()?;
                        this.cache.operations.insert(key, Value::Int(res.clone()));
                        Some(res)
                    }

                    fn tetration(this: &mut Interpreter, base: &Int, height: &Int, pow_cached: &dyn Fn(&mut Interpreter, &Int, &Int) -> Option<Int>) -> Option<Int> {
                        if this.check_stop_flag() {
                            return None;
                        }
                        if height.is_zero() {
                            return Some(Int::from(1));
                        }
                        if *height == Int::from(1) {
                            return Some(base.clone());
                        }

                        let one = Int::from(1);
                        let mut result = base.clone();
                        let mut count = Int::from(1);

                        while &count < height {
                            if this.check_stop_flag() {
                                return None;
                            }
                            result = pow_cached(this, base, &result)?;
                            count = (count + one.clone()).unwrap_or_else(|_| height.clone());
                        }

                        Some(result)
                    }

                    if height.is_zero() {
                        Value::Int(1.into())
                    } else {
                        let one = Int::from(1);
                        let mut result = base.clone();
                        let mut count = Int::from(1);

                        while &count < height {
                            if self.check_stop_flag() {
                                return self.raise("ValueError", "Pentation interrupted by stop flag");
                            }
                            result = tetration(self, base, &result, &pow_cached).ok_or_else(|| self.raise("ValueError", "Pentation failed")).unwrap();
                            count = (count + one.clone()).unwrap_or_else(|_| height.clone());
                        }

                        Value::Int(result)
                    }
                }

                (Value::Float(base), Value::Int(height)) if !height.negative => {
                    let pow_cached = |this: &mut Interpreter, base: &Float, exp: &Float| -> Option<Float> {
                        let key = format!("{}::{}::^", base, exp);
                        if let Some(Value::Float(cached)) = this.cache.operations.get(&key) {
                            return Some(cached.clone());
                        }
                        let res = base.pow(exp).ok()?;
                        this.cache.operations.insert(key, Value::Float(res.clone()));
                        Some(res)
                    };

                    fn tetration(this: &mut Interpreter, base: &Float, height: &Float, pow_cached: &dyn Fn(&mut Interpreter, &Float, &Float) -> Option<Float>) -> Option<Float> {
                        if this.check_stop_flag() {
                            return None;
                        }
                        if height.is_zero() {
                            return Some(Float::from(1.0));
                        }
                        if *height == Float::from(1.0) {
                            return Some(base.clone());
                        }

                        let one = Float::from(1.0);
                        let mut result = base.clone();
                        let mut count = Float::from(1.0);

                        while &count < height {
                            if this.check_stop_flag() {
                                return None;
                            }
                            result = pow_cached(this, base, &result)?;
                            count = (count + one.clone()).unwrap_or_else(|_| height.clone());
                        }

                        Some(result)
                    }

                    let float_height = match Float::from_int(&height) {
                        Ok(f) => f,
                        Err(_) => return self.raise("TypeError", "Failed to convert Int to Float"),
                    };

                    if float_height.is_zero() {
                        Value::Float(Float::from(1.0))
                    } else {
                        let one = Float::from(1.0);
                        let mut result = base.clone();
                        let mut count = Float::from(1.0);

                        while &count < &float_height {
                            if self.check_stop_flag() {
                                return self.raise("ValueError", "Pentation interrupted by stop flag");
                            }
                            result = tetration(self, base, &result, &pow_cached).ok_or_else(|| self.raise("ValueError", "Pentation failed")).unwrap();
                            count = (count + one.clone()).unwrap_or_else(|_| float_height.clone());
                        }

                        Value::Float(result)
                    }
                }

                (Value::Int(_), Value::Float(_)) | (Value::Float(_), Value::Float(_)) => {
                    self.raise("TypeError", "Pentation height must be an integer")
                }

                (a, b) => self.raise("TypeError", &format!(
                    "Operator '^^^' requires numeric base and integer non-negative height, got '{}' and '{}'",
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
                (Value::Float(a), Value::Float(b)) => Value::Boolean(a <= b),
                (Value::String(a), Value::String(b)) => Value::Boolean(a <= b),
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
            "&" | "band" => match (left, right) {
                (Value::Int(a), Value::Int(b)) => {
                    let a_i64 = match a.to_i64() {
                        Ok(val) => val,
                        Err(e) => return self.raise("ValueError", &format!("Failed to convert left Int: {}", e)),
                    };
                    let b_i64 = match b.to_i64() {
                        Ok(val) => val,
                        Err(e) => return self.raise("ValueError", &format!("Failed to convert right Int: {}", e)),
                    };
                    let res = a_i64 & b_i64;
                    Value::Int(Int::from(res))
                }
                _ => self.raise("TypeError", "Bitwise AND requires two integers"),
            },
            "|" | "bor" => match (left, right) {
                (Value::Int(a), Value::Int(b)) => {
                    let a_i64 = match a.to_i64() {
                        Ok(val) => val,
                        Err(e) => return self.raise("ValueError", &format!("Failed to convert left Int: {}", e)),
                    };
                    let b_i64 = match b.to_i64() {
                        Ok(val) => val,
                        Err(e) => return self.raise("ValueError", &format!("Failed to convert right Int: {}", e)),
                    };
                    let res = a_i64 | b_i64;
                    Value::Int(Int::from(res))
                }
                _ => self.raise("TypeError", "Bitwise OR requires two integers"),
            },
            "<<" | "lshift" => match (left, right) {
                (Value::Int(a), Value::Int(b)) => {
                    let a_i64 = match a.to_i64() {
                        Ok(val) => val,
                        Err(e) => return self.raise("ValueError", &format!("Failed to convert left Int: {}", e)),
                    };
                    let b_i64 = match b.to_i64() {
                        Ok(val) => val,
                        Err(e) => return self.raise("ValueError", &format!("Failed to convert right Int: {}", e)),
                    };
                    let res = match a_i64.checked_shl(b_i64 as u32) {
                        Some(val) => val,
                        None => return self.raise("ValueError", "Shift amount too large"),
                    };
                    Value::Int(Int::from(res))
                }
                _ => self.raise("TypeError", "Left shift requires two integers"),
            },
            ">>" | "rshift" => match (left, right) {
                (Value::Int(a), Value::Int(b)) => {
                    let a_i64 = match a.to_i64() {
                        Ok(val) => val,
                        Err(e) => return self.raise("ValueError", &format!("Failed to convert left Int: {}", e)),
                    };
                    let b_i64 = match b.to_i64() {
                        Ok(val) => val,
                        Err(e) => return self.raise("ValueError", &format!("Failed to convert right Int: {}", e)),
                    };
                    let res = match a_i64.checked_shr(b_i64 as u32) {
                        Some(val) => val,
                        None => return self.raise("ValueError", "Shift amount too large"),
                    };
                    Value::Int(Int::from(res))
                }
                _ => self.raise("TypeError", "Right shift requires two integers"),
            },
            "abs" => {
                match right {
                    Value::Int(n) => Value::Int(n.abs()),
                    Value::Float(f) => Value::Float(f.abs()),
                    _ => self.raise("TypeError", &format!("Cannot apply 'abs' to {}", right.type_name())),
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
            "is" => {
                let res = self.check_type(&left, &right, false);
                if self.err.is_some() {
                    self.err = None;
                }
                Value::Boolean(res)
            },
            "isnt" | "isn't" => {
                let res = !self.check_type(&left, &right, false);
                if self.err.is_some() {
                    self.err = None;
                }
                Value::Boolean(res)
            },
            _ => self.raise("SyntaxError", &format!("Unknown operator '{}'", operator)),
        };

        debug_log(
            &log_str.replace("{}", &format_value(&result)),
            &self.config,
            Some(self.use_colors.clone()),
        );
        return result;
    }

    fn handle_number(&mut self, map: HashMap<Value, Value>) -> Value {
        let s = to_static(match map.get(&Value::String("value".to_string())) {
            Some(Value::String(s)) if !s.is_empty() => s.as_str(),
            Some(Value::String(_)) => return self.raise("RuntimeError", "Empty string provided for number"),
            _ => return self.raise("RuntimeError", "Missing 'value' in number statement"),
        }.replace('_', ""));
    
        if let Some(cached) = self.cache.constants.get(s) {
            debug_log(
                &format!("<CachedConstantNumber: {}>", s),
                &self.config,
                Some(self.use_colors.clone()),
            );
            return cached.clone();
        }

        fn parse_int_with_base(s: &str, base: u32) -> Result<Int, ()> {
            use std::ops::{Add, Mul};
            let zero = Int::from(0);
            let base_int = Int::from(base as usize);

            s.chars().try_fold(zero.clone(), |acc, c| {
                let digit = char_to_digit(c).ok_or(())?;
                if digit >= base {
                    return Err(());
                }
                let digit_int = Int::from(digit as usize);
                let product = acc.mul(base_int.clone()).map_err(|_| ())?;
                product.add(digit_int).map_err(|_| ())
            })
        }

        let result = if s.starts_with("0b") || s.starts_with("0B") {
            parse_int_with_base(&s[2..].to_lowercase(), 2)
                .map(Value::Int)
                .unwrap_or_else(|_| self.raise("RuntimeError", "Invalid format for binary integer literal."))
        } else if s.starts_with("0o") || s.starts_with("0O") {
            parse_int_with_base(&s[2..].to_lowercase(), 8)
                .map(Value::Int)
                .unwrap_or_else(|_| self.raise("RuntimeError", "Invalid format for octal integer literal."))
        } else if s.starts_with("0x") || s.starts_with("0X") {
            parse_int_with_base(&s[2..].to_lowercase(), 16)
                .map(Value::Int)
                .unwrap_or_else(|_| self.raise("RuntimeError", "Invalid format for hex integer literal."))
        } else if let Some(hash_pos) = s.find('#') {
            use std::str::FromStr;
            let (base_str, digits) = s.split_at(hash_pos);
            let digits = &digits[1..];
            let base = match u32::from_str(base_str) {
                Ok(b) if (1..=62).contains(&b) => b,
                _ => return self.raise("RuntimeError", "Invalid base for custom base literal (must be 1-62)"),
            };

            if base == 1 {
                if digits.chars().all(|c| c == '1') {
                    let val = Int::from(digits.len());
                    Ok(val)
                } else {
                    return self.raise("RuntimeError", "Invalid digits for base 1 literal, only '1's allowed");
                }
            } else {
                parse_int_with_base(digits, base)
            }
            .map(Value::Int)
            .unwrap_or_else(|_| self.raise("RuntimeError", &format!("Invalid digits for {} base integer literal.", base_str)))        
        } else if s.contains('.') || s.to_ascii_lowercase().contains('e') {
            let f = match Float::from_str(s) {
                Ok(f) => f,
                Err(_) => return self.raise("RuntimeError", "Invalid float format"),
            };            
    
            if s.to_ascii_lowercase().contains('e') && f.is_integer_like() {
                return f.to_int()
                    .map(Value::Int)
                    .unwrap_or_else(|_| self.raise("RuntimeError", "Failed to convert float to int"));
            }            
    
            Value::Float(f)
        } else {
            let mut s = s.trim().trim_start_matches('0').trim_start_matches('+');
            if s.is_empty() {
                s = "0";
            }
            Int::from_str(s)
                .map_err(|_| ())
                .map(Value::Int)
                .unwrap_or_else(|_| self.raise("RuntimeError", "Invalid integer format"))
        };
    
        let bit_limit = std::mem::size_of::<usize>() * 64;

        let cacheable = match &result {
            Value::Float(f) => {
                f.to_f64().map_or(false, |val| val.is_finite())
                    || (f.is_integer_like()
                        && f.to_int().is_ok()
                        && f.to_int().unwrap().to_string().len() * 4 < bit_limit)
            }
            Value::Int(i) => i.to_string().len() * 4 < bit_limit,
            _ => false,
        };        
    
        if cacheable {
            self.cache.constants.insert(s.to_string(), result.clone());
        }
    
        result
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
    
                                        let tokens = Lexer::new(&expr, &self.file_path.clone(), None).tokenize();
                                        if tokens.is_empty() {
                                            return self.raise("SyntaxError", "Empty expression inside {}");
                                        }
                                        let filtered = tokens
                                            .iter()
                                            .filter(|token| {
                                                let t = &token.0;
                                                t != "WHITESPACE" && !t.starts_with("COMMENT_") && t != "EOF"
                                            })
                                            .collect::<Vec<_>>();
                        
                                        debug_log(&format!("Generated f-string tokens: {:?}", filtered), &self.config, Some(self.use_colors));
                                        
                                        let parsed = match Parser::new(tokens).parse_safe() {
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
