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
    get_type_from_token_name,
    sanitize_alias,
    special_function_meta,
    check_version,
    fix_path,
    char_to_digit,
    get_remaining_stack_size,
    wrap_in_help,
    is_number,
    is_number_parentheses,
    gamma_lanczos,
    get_inner_type,
    check_pattern,
    get_enum_idx,
    unescape_string_premium_edition,
    apply_format_spec,
    remove_loc_keys,
    type_matches,
    generate_name_variants,
    escape_string,
    find_struct_method,
};

#[cfg(not(target_arch = "wasm32"))]
use crossterm::{
    cursor::{MoveUp, MoveToColumn},
    terminal::{Clear, ClearType},
    ExecutableCommand,
};

use crate::env::runtime::pattern_reg::{predict_sequence, predict_sequence_until_length};
use crate::env::runtime::types::{Int, Float, Type, VALID_TYPES};
use crate::env::runtime::value::Value;
use crate::env::runtime::errors::Error;
use crate::env::runtime::variables::Variable;
use crate::env::runtime::statements::Statement;
use crate::env::runtime::modules::Module;
use crate::env::runtime::native;
use crate::env::runtime::functions::{FunctionMetadata, Parameter, ParameterKind, Function, NativeFunction, UserFunctionMethod, UserFunction};
use crate::env::runtime::generators::{Generator, GeneratorType, NativeGenerator, CustomGenerator, RangeValueIter, InfRangeIter, RangeLengthIter};
use crate::env::runtime::libs::STD_LIBS;
use crate::env::runtime::internal_structs::{Cache, InternalStorage, State, PatternMethod, Stack, StackType, EffectFlags};
use crate::env::runtime::structs_and_enums::{Enum, Struct};
use std::sync::{Arc, atomic::{AtomicBool, Ordering}};
use std::path::{PathBuf, Path};
use std::fs;
use regex::Regex;
#[cfg(not(target_arch = "wasm32"))]
use tokio::runtime::Runtime;
#[cfg(not(target_arch = "wasm32"))]
use reqwest;
#[cfg(not(target_arch = "wasm32"))]
use serde_urlencoded;
use std::io::{stdout, Write};
use std::sync::Mutex;
use std::panic::Location as PanicLocation;
use rustc_hash::FxHashMap;
use serde::{Serialize, Deserialize};
use bincode::{Encode, Decode};
use super::VERSION;

use crate::lexer::Lexer;
use crate::env::runtime::preprocessor::Preprocessor;
use crate::parser::Parser;
use crate::env::runtime::tokens::{Token, Location};

#[derive(Debug, Clone, Serialize, Deserialize, Encode, Decode)]
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
    pub variables: FxHashMap<String, Variable>,
    file_path: String,
    cwd: PathBuf,
    preprocessor_info: (PathBuf, PathBuf, bool),
    cache: Cache,
    internal_storage: InternalStorage,
    defer_stack: Vec<Vec<Statement>>,
    scope: String,
    pub collected_effects: EffectFlags,
    
    #[serde(skip)]
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
            variables: FxHashMap::default(),
            file_path: file_path.to_owned(),
            cwd: cwd.clone(),
            preprocessor_info,
            cache: Cache {
                operations: FxHashMap::default(),
                constants: FxHashMap::default(),
                iterables: FxHashMap::default()
            },
            internal_storage: InternalStorage {
                lambda_counter: 0,
                use_42: (false, false),
                in_try_block: false,
                in_function: false,
            },
            defer_stack: vec![],
            scope: "main".to_owned(),
            stop_flag: None,
            collected_effects: EffectFlags::empty(),
        };


        for type_ in VALID_TYPES {
            this.variables.insert(
                type_.to_string(),
                Variable::new(type_.to_string(), Value::Type(Type::new_simple(type_)), "type".to_owned(), true, false, true),
            );
        }
        
        this.variables.insert(
            "argv".to_owned(),
            Variable::new(
                "argv".to_owned(),
                Value::List(argv.iter().cloned().map(Value::String).collect()),
                "list".to_owned(),
                true,
                false,
                true,
            ),
        );

        if file_path.starts_with("<") && file_path.ends_with(">") {
            this.variables.insert(
                "__dir__".to_owned(),
                Variable::new(
                    "__dir__".to_owned(),
                    Value::String(format!("{}/", fix_path(cwd.canonicalize().unwrap_or(".".into()).display().to_string()))),
                    "str".to_owned(),
                    true,
                    false,
                    true,
                ),
            );
            this.variables.insert(
                "__file__".to_owned(),
                Variable::new(
                    "__file__".to_owned(),
                    Value::String(file_path.to_owned()),
                    "str".to_owned(),
                    true,
                    false,
                    true,
                ),
            );
        } else {
            let mut dir = fix_path(Path::new(file_path).parent().unwrap_or_else(|| Path::new(".")).canonicalize().unwrap_or(".".into()).display().to_string());
            std::env::set_current_dir(&dir).ok();

            if !dir.ends_with('/') {
                dir.push('/');
            }

            this.variables.insert(
                "__dir__".to_owned(),
                Variable::new(
                    "__dir__".to_owned(),
                    Value::String(escape_string(&dir).unwrap_or_else(|_| dir)),
                    "str".to_owned(),
                    true,
                    false,
                    true,
                ),
            );
            this.variables.insert(
                "__file__".to_owned(),
                Variable::new(
                    "__file__".to_owned(),
                    Value::String(file_path.to_owned()),
                    "str".to_owned(),
                    true,
                    false,
                    true,
                ),
            );
        }
    
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
        insert_builtin("size_of", Value::Function(native::size_of_fn()));
        insert_builtin("sum", Value::Function(native::sum_fn()));
        insert_builtin("ord", Value::Function(native::ord_fn()));
        insert_builtin("char", Value::Function(native::char_fn()));
        insert_builtin("styledstr", Value::Function(native::styledstr_fn()));
        insert_builtin("array", Value::Function(native::array_fn()));
        insert_builtin("range", Value::Function(native::range_fn()));
        this.variables.insert(
            "00__placeholder__".to_owned(),
            Variable::new("__placeholder__".to_owned(), Value::Function(native::placeholder_fn()), "function".to_owned(), true, false, true),
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

    fn check_type_validity(&self, type_str: &str) -> bool {
        let trimmed = type_str.trim_start_matches('&').trim_start_matches('?');
        if let Some(t) = self.variables.get(trimmed) {
            matches!(t.value, Value::Type(_))
        } else {
            false
        }
    }

    fn to_index(&mut self, val: &Value, len: usize) -> Result<usize, Value> {
        let mut val = val.clone();
        if val.is_statement() {
            val = self.evaluate(&val.convert_to_statement());
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
                self.evaluate(&self.current_statement.clone().unwrap());
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

    // this is a lot cleaner than last time, glad i refactored it
    fn check_type(&mut self, value: &Value, expected: &Type) -> (bool, Option<Error>) {
        let mut err: Option<Error> = None;
        let mut status: bool = true;
        fn make_err(err_type: &str, err_msg: &str, loc: Option<Location>) -> Error {
            loc.map(|l| Error::with_location(err_type, err_msg, l))
                .unwrap_or_else(|| Error::new_anonymous(err_type, err_msg))
        }

        let value_type = value.get_type();

        match expected {
            Type::Simple { name: expected_type, is_reference: expected_ref, is_maybe_type: null_allowed } => {
                if expected_type != "any" {
                    match value_type {
                        Type::Simple { name: value_type_name, is_reference: value_type_ref, .. } => {
                            if !((*null_allowed && value_type_name == "void") || (value_type_name == *expected_type && value_type_ref == *expected_ref)) {
                                status = false;
                            }
                        }
                        _ => {
                            err = Some(make_err("TypeError", &format!("Expected type '{}', got '{}'", expected_type, value_type.display()), self.get_location_from_current_statement()));
                        }
                    }
                } else if *expected_ref == true {
                    match value_type {
                        Type::Simple { name: _, is_reference: value_type_ref, .. } => {
                            if value_type_ref != *expected_ref {
                                status = false;
                            }
                        }
                        _ => {}
                    }
                }
            }
            Type::Indexed { base_type: base, elements } => {
                if value_type != **base {
                    status = false;
                }
                match value {
                    Value::List(l) | Value::Tuple(l) => {
                        let val_type: &str = if matches!(value, Value::Tuple(_)) { "tuple" } else { "list" };
                        if elements.len() == 1 {
                            for (i, elem) in l.iter().enumerate() {
                                if !self.check_type(elem, &elements[0]).0 {
                                    err = Some(make_err("TypeError", &format!("Expected type '{}' for {} element #{}, got '{}' ({})", elements[0].display_simple(), val_type, i + 1, elem.get_type().display_simple(), format_value(elem)), self.get_location_from_current_statement()));
                                    status = false;
                                    break;
                                }
                            }
                        } else if l.len() != elements.len() {
                            err = Some(make_err("TypeError", &format!("Expected {} of length {}, got {}", val_type, elements.len(), l.len()), self.get_location_from_current_statement()));
                            status = false;
                        } else {
                            for (i, elem) in l.iter().enumerate() {
                                if !self.check_type(elem, &elements[i]).0 {
                                    err = Some(make_err("TypeError", &format!("Expected type '{}' for {} element #{}, got '{}' ({})", elements[i].display_simple(), val_type, i + 1, elem.get_type().display_simple(), format_value(elem)), self.get_location_from_current_statement()));
                                    status = false;
                                    break;
                                }
                            }
                        }
                    }
                    Value::Map { keys, values } => {
                        if elements.len() == 2 {
                            for (k, v) in keys.iter().zip(values.iter()) {
                                if !self.check_type(k, &elements[0]).0 {
                                    err = Some(make_err("TypeError", &format!("Expected type '{}' for map key, got '{}'", elements[0].display(), k.get_type().display()), self.get_location_from_current_statement()));
                                    status = false;
                                }
                                if !self.check_type(v, &elements[1]).0 {
                                    err = Some(make_err("TypeError", &format!("Expected type '{}' for map value, got '{}'", elements[1].display(), v.get_type().display()), self.get_location_from_current_statement()));
                                    status = false;
                                }
                            }
                        } else {
                            err = Some(make_err("TypeError", &format!("Expected type with 2 elements, got {}", elements.len()), self.get_location_from_current_statement()));
                            status = false;
                        }
                    }
                    Value::Pointer(ptr) => {
                        let ptr_val = (*ptr).clone();
                        self.check_type(&ptr_val, expected);
                    }
                    _ => {
                        err = Some(make_err("TypeError", &format!("Expected type '{}' for indexed type, got '{}'", base.display(), value.get_type().display()), self.get_location_from_current_statement()));
                        status = false;
                    }
                }
            }
            Type::Union(types) => {
                if types.is_empty() {
                    err = Some(make_err("TypeError", "Union type cannot be empty", self.get_location_from_current_statement()));
                    status = false;
                } else {
                    let mut matched = false;
                    for ty in types {
                        if self.check_type(value, ty).0 {
                            matched = true;
                            break;
                        }
                    }
                    if !matched {
                        err = Some(make_err("TypeError", &format!("Expected one of the union types: {}, got '{}'", types.iter().map(|t| t.display()).collect::<Vec<_>>().join(", "), value.get_type().display()), self.get_location_from_current_statement()));
                        status = false;
                    }
                }
            }
            Type::Function { return_type: expected_return_type, parameter_types: expected_parameter_types } => {
                if let Value::Function(f) = value {
                    status = true;
                    let return_type = f.get_return_type();
                    let parameter_types = f.get_parameter_types();
                    if !type_matches(&return_type, expected_return_type) {
                        err = Some(make_err("TypeError", &format!("Expected return type '{}', got '{}'", expected_return_type.display(), return_type.display()), self.get_location_from_current_statement()));
                        status = false;
                    }
                    if parameter_types.len() != expected_parameter_types.len() {
                        err = Some(make_err(
                            "TypeError",
                            &format!("Expected {} parameters, got {}", expected_parameter_types.len(), parameter_types.len()),
                            self.get_location_from_current_statement(),
                        ));
                        status = false;
                    } else {
                        for (i, (param_type, expected_param_type)) in parameter_types.iter().zip(expected_parameter_types.iter()).enumerate() {
                            if !type_matches(param_type, expected_param_type) {
                                err = Some(make_err(
                                    "TypeError",
                                    &format!("Expected parameter type '{}' for parameter #{}, got '{}'", expected_param_type.display(), i + 1, param_type.display()),
                                    self.get_location_from_current_statement(),
                                ));
                                status = false;
                            }
                        }
                    }
                } else {
                    err = Some(make_err("TypeError", &format!("Expected type 'function', got '{}'", value.get_type().display()), self.get_location_from_current_statement()));
                    status = false;
                }
            }
            Type::Generator { yield_type: expected_yield_type, parameter_types: expected_parameter_types } => {
                if let Value::Generator(g) = value {
                    status = true;

                    if let Some(yield_type) = g.get_yield_type() {
                        if yield_type != **expected_yield_type {
                            err = Some(make_err(
                                "TypeError",
                                &format!("Expected yield type '{}', got '{}'", expected_yield_type.display(), yield_type.display()),
                                self.get_location_from_current_statement(),
                            ));
                            status = false;
                        }
                    }

                    if let Some(parameter_types) = g.get_parameter_types() {
                        for (i, (param_type, expected_param_type)) in parameter_types.iter().zip(expected_parameter_types.iter()).enumerate() {
                            if !type_matches(param_type, expected_param_type) {
                                err = Some(make_err(
                                    "TypeError",
                                    &format!("Expected parameter type '{}' for parameter #{}, got '{}'", expected_param_type.display(), i + 1, param_type.display()),
                                    self.get_location_from_current_statement(),
                                ));
                                status = false;
                            }
                        }
                    }
                } else {
                    err = Some(make_err(
                        "TypeError",
                        &format!("Expected type 'generator', got '{}'", value.get_type().display()),
                        self.get_location_from_current_statement(),
                    ));
                    status = false;
                }
            }
            Type::Alias { name: alias_name, base_type, conditions, variables } => {
                let inner_type = match get_inner_type(&base_type) {
                    Ok((_, t)) => t,
                    Err(e) => {
                        status = false;
                        err = Some(make_err("TypeError", &e, self.get_location_from_current_statement()));
                        Type::new_simple("any")
                    }
                };
                let (status_check, err_check) = self.check_type(value, &inner_type);
                if !status_check {
                    status = false;
                    err = err_check.or(Some(make_err("TypeError", &format!("Alias '{}' expects type '{}', got '{}'", alias_name, inner_type.display(), value.get_type().display()), self.get_location_from_current_statement())));    
                }
                if variables.len() > 1 {
                    status = false;
                    err = Some(make_err("TypeError", &format!("Alias '{}' expects only one variable, got {}", alias_name, variables.len()), self.get_location_from_current_statement()));
                }
                let mut vars: HashMap<String, Variable> = HashMap::new();
                for var in variables.iter() {
                    if let Value::String(var_name) = var {
                        vars.insert(var_name.clone(), Variable::new(var_name.clone(), value.clone(), value.get_type().display().to_string(), false, true, true));
                    } else {
                        status = false;
                        err = Some(make_err("TypeError", &format!("Expected variable name to be a string, got '{}'", var.get_type().display()), self.get_location_from_current_statement()));
                    }
                }
                let mut new_interpreter = Interpreter::new(
                    self.config.clone(),
                    self.use_colors,
                    &self.file_path,
                    &self.cwd,
                    self.preprocessor_info.clone(),
                    &[],
                );
                new_interpreter.variables.extend(vars);
                new_interpreter.set_scope(&format!("{}+scope.{}", self.scope, alias_name));
                for (i, cond) in conditions.iter().enumerate() {
                    new_interpreter.current_statement = Some(cond.convert_to_statement());
                    if !new_interpreter.evaluate(&cond.convert_to_statement()).is_truthy() {
                        status = false;
                        err = Some(make_err("TypeError", &format!("Conditions for alias '{}' don't meet condition #{}", alias_name, i + 1), self.get_location_from_current_statement()));
                        break;
                    }
                }
            }
            Type::Enum { generics, .. } => {
                match value {
                    Value::Enum(e) => {
                        if type_matches(&e.ty, expected) {
                            if self.config.type_strict && generics.len() > 0 {
                                self.warn("TypeWarning: Strict type checking for Enums with generics is not implemented yet.");
                            }
                            status = true;
                        } else {
                            status = false;
                        }
                    }
                    _ => status = false,
                }
            }
            Type::Struct { .. } => {
                match value {
                    Value::Struct(s) => {
                        if type_matches(&s.ty, expected) {
                            status = true;
                        } else {
                            status = false;
                        }
                    }
                    _ => status = false,
                }
            }
            Type::Impl { implementations } => {
                if let Some((_, props)) = self.get_properties(value, true) {
                    let mut all_matched = true;
                    for (name, ty, mods) in implementations {
                        let mut is_static = false;
                        let mut is_final = true;
                        for m in mods {
                            match m.as_str() {
                                "static" => is_static = true,
                                "non-static" => is_static = false,
                                "final" => is_final = true,
                                "mutable" => is_final = false,
                                _ => {}
                            }
                        }
                        let t = match get_inner_type(ty) {
                            Ok((_, mut t)) => {
                                if let Type::Function { parameter_types, .. } = &mut t {
                                    if !is_static {
                                        parameter_types.insert(0, Type::new_simple("any"));
                                    }
                                }
                                t
                            }
                            Err(e) => {
                                err = Some(make_err("TypeError", &e,  self.get_location_from_current_statement()));
                                all_matched = false;
                                break;
                            }
                        };
                        if let Some(prop) = props.get(name) {
                            if let Value::Function(f) = &prop.value {
                                if is_final != f.is_final() {
                                    let expected = if is_final { "final" } else { "non-final" };
                                    let actual = if f.is_final() { "final" } else { "non-final" };
                                    err = Some(make_err(
                                        "TypeError",
                                        &format!("Method '{}' is {} but expected {}", name, actual, expected),
                                        self.get_location_from_current_statement(),
                                    ));
                                    all_matched = false;
                                    break;
                                }

                                if is_static != f.is_static() {
                                    let expected = if is_static { "static" } else { "non-static" };
                                    let actual = if f.is_static() { "static" } else { "non-static" };
                                    err = Some(make_err(
                                        "TypeError",
                                        &format!("Method '{}' is {} but expected {}", name, actual, expected),
                                        self.get_location_from_current_statement(),
                                    ));
                                    all_matched = false;
                                    break;
                                }
                            } else {
                                err = Some(make_err("TypeError", &format!("Property '{}' is not a function", name), self.get_location_from_current_statement()));
                                all_matched = false;
                                break;
                            }
                            let (status, e) = self.check_type(&prop.value, &t);
                            if !status {
                                err = e.or(Some(make_err("TypeError", &format!("Property '{}' expected to be of type '{}', got '{}'", name, ty.display(), prop.value.get_type().display()), self.get_location_from_current_statement())));
                                all_matched = false;
                                break;
                            }
                        } else {
                            err = Some(make_err("TypeError", &format!("Property '{}' not found in value", name), self.get_location_from_current_statement()));
                            all_matched = false;
                            break;
                        }
                    }
                    if !all_matched {
                        status = false;
                    }
                } else {
                    status = false;
                    err = Some(make_err("TypeError", &format!("Expected type compatible with 'impl', got incompatible '{}'", value.get_type().display()), self.get_location_from_current_statement()));
                }
            }
            _ => {
                status = false;
                err = Some(make_err("TypeError", &format!("Unsupported type check for '{}'", expected.display()), self.get_location_from_current_statement()));
            }
        }

        return (status, err);
    }

    #[cfg(not(target_arch = "wasm32"))]
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
    
    #[cfg(not(target_arch = "wasm32"))]
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

    #[cfg(target_arch = "wasm32")]
    fn fetch_fn(&mut self, args: &HashMap<String, Value>) -> Value {
        use wasm_bindgen_futures::{spawn_local, JsFuture};
        use web_sys::{Request, RequestInit, RequestMode, Response};
        use js_sys::{JsString, Promise};
        use wasm_bindgen::{JsCast, JsValue};
        use std::sync::mpsc::channel;
        use serde_wasm_bindgen::to_value;

        if !self.config.allow_fetch {
            return self.raise("PermissionError", "Fetching is not allowed in this context");
        }

        let url = match args.get("url") {
            Some(Value::String(s)) => s.clone(),
            _ => return self.raise("TypeError", "Expected 'url' to be a string"),
        };

        let method = match args.get("method") {
            Some(Value::String(s)) => s.clone(),
            _ => "GET".to_string(),
        };

        let headers_map = match args.get("headers") {
            Some(Value::Map { keys, values }) => {
                let mut map = HashMap::new();
                for (k, v) in keys.iter().zip(values.iter()) {
                    if let (Value::String(k), Value::String(v)) = (k, v) {
                        map.insert(k.clone(), v.clone());
                    }
                }
                Some(map)
            }
            _ => None,
        };

        let body = if let Some(json_val) = args.get("json") {
            to_value(json_val).ok().map(|jsv| JsString::from(jsv.as_string().unwrap_or_default()))
        } else if let Some(Value::Map { keys, values }) = args.get("data") {
            let mut form = vec![];
            for (k, v) in keys.iter().zip(values.iter()) {
                if let (Value::String(k), Value::String(v)) = (k, v) {
                    form.push(format!("{}={}", k, v));
                }
            }
            Some(JsString::from(form.join("&")))
        } else {
            None
        };

        let opts = RequestInit::new();
        opts.set_method(&method);
        opts.set_mode(RequestMode::Cors);
        if let Some(b) = &body {
            opts.set_body(&JsValue::from(b));
        }

        let request = Request::new_with_str_and_init(&url, &opts).unwrap();

        if let Some(headers_map) = headers_map {
            let headers = request.headers();
            for (k, v) in headers_map {
                headers.set(&k, &v).ok();
            }
        }

        let (tx, rx) = channel();
        let window = web_sys::window().unwrap();
        let fetch_promise: Promise = window.fetch_with_request(&request);
        let tx_clone = tx.clone();

        spawn_local(async move {
            let resp_value = wasm_bindgen_futures::JsFuture::from(fetch_promise)
                .await;
            match resp_value {
                Ok(rv) => {
                    let resp: Response = rv.dyn_into().unwrap();
                    let text = JsFuture::from(resp.text().unwrap())
                        .await
                        .map(|v| v.as_string().unwrap_or_default())
                        .unwrap_or_else(|_| "Failed to read response".to_string());
                    tx_clone.send(text).ok();
                }
                Err(_) => {
                    tx_clone.send("Fetch failed".to_string()).ok();
                }
            }
        });

        let result_text = rx.recv().unwrap_or_else(|_| "Fetch failed".to_string());
        Value::String(result_text)
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
        
        let lexer = Lexer::new(&content, to_static(path.display().to_string()));
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

    fn get_interpreter_as_method(&mut self, func: Function) -> Value {
        let arc_func = match func {
            Function::Custom(f) => f,
            _ => {
                return self.raise("TypeError", "Expected a custom function");
            }
        };
        Value::Function(Function::CustomMethod(Arc::new(UserFunctionMethod::new_from_func_with_interpreter(arc_func, Arc::new(Mutex::new(self.clone()))))))
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

                let value = self.evaluate(&stmt);

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
                    self.evaluate(&stmt);
                }
            }

            self.state = old_state;
        }

        self.stack.pop();

        Ok(self.return_value.clone())
    }

    #[track_caller]
    fn get_location_from_current_statement(&self) -> Option<Location> {
        match &self.current_statement {
            Some(Statement::Statement { loc, .. }) => match loc {
                Some(loc) => Some(loc.clone().set_lucia_source_loc(format!("{}:{}:{}", PanicLocation::caller().file(), PanicLocation::caller().line(), PanicLocation::caller().column()))),
                None => None,
            },
            _ => None,
        }
    }

    fn get_location_from_current_statement_caller(&self, caller: PanicLocation) -> Option<Location> {
        match &self.current_statement {
            Some(Statement::Statement { loc, .. }) => match loc {
                Some(loc) => Some(loc.clone().set_lucia_source_loc(format!("{}:{}:{}", caller.file(), caller.line(), caller.column()))),
                None => None,
            },
            _ => None,
        }
    }

    #[track_caller]
    pub fn raise(&mut self, error_type: &str, msg: &str) -> Value {
        let rust_loc = PanicLocation::caller();
        let loc = self.get_location_from_current_statement_caller(*rust_loc).unwrap_or_else(|| Location {
            file: self.file_path.clone(),
            line_string: "".to_string(),
            line_number: 0,
            range: (0, 0),
            lucia_source_loc: format!("{}:{}:{}", rust_loc.file(), rust_loc.line(), rust_loc.column()),
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

    #[track_caller]
    pub fn warn(&mut self, warning_str: &str) -> Value {
        let rust_loc = PanicLocation::caller();
        let loc = self.get_location_from_current_statement_caller(*rust_loc).unwrap_or_else(|| Location {
            file: fix_path(self.file_path.clone()),
            line_string: "".to_string(),
            line_number: 0,
            range: (0, 0),
            lucia_source_loc: format!("{}:{}:{}", rust_loc.file(), rust_loc.line(), rust_loc.column()),
        });
        let loc_str = format!("{}:{}:{}", fix_path(loc.file), loc.line_number, loc.range.0);


        if self.config.debug {
            println!("{}Warning raised from {}{}", hex_to_ansi(&self.config.color_scheme.debug, self.config.supports_color), loc.lucia_source_loc, hex_to_ansi("reset", self.config.supports_color));
        }

        if self.config.warnings {
            eprintln!("{}[{}] {}{}", hex_to_ansi(&self.config.color_scheme.warning, self.config.supports_color), loc_str, warning_str, hex_to_ansi("reset", self.config.supports_color));
        }

        NULL
    }

    #[track_caller]
    pub fn raise_with_help(&mut self, error_type: &str, msg: &str, help: &str) -> Value {
        let rust_loc = PanicLocation::caller();
        let loc = self.get_location_from_current_statement_caller(*rust_loc).unwrap_or_else(|| Location {
            file: self.file_path.clone(),
            line_string: "".to_string(),
            line_number: 0,
            range: (0, 0),
            lucia_source_loc: format!("{}:{}:{}", rust_loc.file(), rust_loc.line(), rust_loc.column()),
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

    #[track_caller]
    pub fn raise_with_ref(&mut self, error_type: &str, msg: &str, ref_err: Error) -> Value {
        let rust_loc = PanicLocation::caller();
        let loc = self.get_location_from_current_statement_caller(*rust_loc).unwrap_or_else(|| Location {
            file: self.file_path.clone(),
            line_string: "".to_string(),
            line_number: 0,
            range: (0, 0),
            lucia_source_loc: format!("{}:{}:{}", rust_loc.file(), rust_loc.line(), rust_loc.column()),
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

    #[track_caller]
    pub fn evaluate(&mut self, statement: &Statement) -> Value {
        #[cfg(target_arch = "wasm32")]
        if is_sleeping!() {
            use gloo_timers::future::TimeoutFuture;
            use wasm_bindgen_futures::spawn_local;
            spawn_local(async move {
                TimeoutFuture::new(100).await;
            });
        }

        self.current_statement = Some(statement.clone());

        if self.check_stop_flag() {
            return NULL;
        }

        self.variables.entry("_".to_string()).or_insert_with(|| {
            Variable::new("_".to_string(), NULL.clone(), "any".to_string(), true, false, false)
        });
    
        self.variables.entry("_err".to_string()).or_insert_with(|| {
            Variable::new(
                "_err".to_string(),
                Value::Tuple(vec![]),
                "tuple".to_string(),
                true,
                false,
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
                "VALUE" => self.handle_value(statement_map),
    
                "OPERATION" => self.handle_operation(statement_map),
                "UNARY_OPERATION" => self.handle_unary_op(statement_map),
                "PIPELINE" => self.handle_pipeline(statement_map),
    
                "CALL" => self.handle_call(statement_map),
                "METHOD_CALL" => self.handle_method_call(statement_map),
                "PROPERTY_ACCESS" => self.handle_property_access(statement_map),
                "INDEX_ACCESS" => self.handle_index_access(statement_map),
    
                "TYPE" => self.handle_type(statement_map),
                "TYPE_CONVERT" => self.handle_type_conversion(statement_map),
                "TYPE_DECLARATION" => self.handle_type_declaration(statement_map),

                "STRUCT_CREATION" => self.handle_struct_creation(statement_map),
                "STRUCT_METHODS" => self.handle_struct_methods(statement_map),

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

    fn handle_pipeline(&mut self, statement: HashMap<Value, Value>) -> Value {
        let initial_value = match statement.get(&Value::String("initial_value".to_string())) {
            Some(v) => v.clone(),
            None => return self.raise("RuntimeError", "Missing 'initial_value' in pipeline statement"),
        };
        let arguments = match statement.get(&Value::String("arguments".to_string())) {
            Some(v) => v,
            None => return self.raise("RuntimeError", "Missing 'arguments' in pipeline statement"),
        };

        let mut current_val = self.evaluate(&initial_value.convert_to_statement());
        if self.err.is_some() {
            return NULL;
        }

        for step_val in arguments.iter().map(|v| v.convert_to_statement()) {
            if self.err.is_some() {
                return NULL;
            }

            let step_type = step_val.get_type();
            if step_type.as_str() == "CALL" || step_type.as_str() == "METHOD_CALL" {
                let mut arg_map: HashMap<Value, Value> = step_val.convert_to_hashmap();

                let pos_key = if step_type.as_str() == "CALL" { "pos_arguments" } else { "pos_args" };
                let named_key = if step_type.as_str() == "CALL" { "named_arguments" } else { "named_args" };

                let val_wrapped = Statement::Statement {
                    keys: vec!["type".into(), "value".into()],
                    values: vec!["VALUE".into(), current_val.clone()],
                    loc: self.get_location_from_current_statement(),
                };

                arg_map.entry(Value::String(pos_key.to_string()))
                    .and_modify(|v| {
                        if let Value::List(args) = v {
                            let mut replaced_any = false;
                            for a in args.iter_mut() {
                                let stmt = a.convert_to_statement();
                                if stmt.get_type() == "VARIABLE" && stmt.get_name() == "_" {
                                    *a = val_wrapped.convert_to_map();
                                    replaced_any = true;
                                }
                            }
                            if !replaced_any {
                                args.insert(0, val_wrapped.convert_to_map());
                            }
                        } else {
                            *v = Value::List(vec![val_wrapped.convert_to_map()]);
                        }
                    })
                    .or_insert_with(|| {
                        Value::List(vec![val_wrapped.convert_to_map()])
                    });

                arg_map.entry(Value::String(named_key.to_string()))
                    .and_modify(|v| {
                        if let Value::Map { keys: _, values } = v {
                            for val_idx in 0..values.len() {
                                let stmt = values[val_idx].convert_to_statement();
                                if stmt.get_type() == "VARIABLE" && stmt.get_name() == "_" {
                                    values[val_idx] = val_wrapped.convert_to_map();
                                }
                            }
                        } else {
                            *v = Value::Map { keys: vec![], values: vec![] };
                        }
                    })
                    .or_insert_with(|| Value::Map { keys: vec![], values: vec![] });

                let new_statement = Statement::from_hashmap_values(&arg_map);
                current_val = self.evaluate(&new_statement);
            } else {
                self.variables.insert(
                    "_".to_string(),
                    Variable::new("_".to_string(), current_val.clone(), "any".to_string(), true, false, false)
                );
                current_val = self.evaluate(&step_val);
            }
            if self.err.is_some() {
                return NULL;
            }
        }

        current_val
    }

    fn handle_value(&mut self, statement: HashMap<Value, Value>) -> Value {
        match statement.get(&Value::String("value".to_string())) {
            Some(v) => v.clone(),
            None => self.raise("RuntimeError", "Missing 'value' in value statement"),
        }
    }

    fn handle_struct_methods(&mut self, statement: HashMap<Value, Value>) -> Value {
        let struct_name = match statement.get(&Value::String("struct_name".to_string())) {
            Some(Value::String(n)) => n,
            _ => return self.raise("RuntimeError", "Missing or invalid 'struct_name' in struct methods"),
        };

        let methods_ast = match statement.get(&Value::String("methods".to_string())) {
            Some(Value::List(methods)) => methods,
            _ => return self.raise("RuntimeError", "Missing or invalid 'methods' in struct methods"),
        };
        let mut methods: HashMap<String, Value> = HashMap::new();

        let mut struct_variable = match self.variables.get(struct_name) {
            Some(v) => v.clone(),
            None => return self.raise("NameError", &format!("Struct '{}' is not defined", struct_name)),
        };
        let mut struct_value;

        if let Value::Type(Type::Struct { .. }) = &struct_variable.value {
            struct_value = struct_variable.value.clone();
        } else {
            return self.raise("TypeError", &format!("'{}' is not a struct type", struct_name));
        }

        let saved_variables = self.variables.clone();
        self.variables.extend::<HashMap<String, Variable>>(HashMap::from_iter(vec![
            ("Self".to_string(), Variable::new(struct_name.clone(), struct_value.clone(), "any".to_string(), false, true, false)),
        ]));
        for method in methods_ast {
            let mut m = method.convert_to_hashmap_value();
            m.entry(Value::String("modifiers".to_string()))
                .and_modify(|v| {
                    if let Value::List(mods) = v {
                        mods.insert(0, Value::String("final".to_string()));
                    }
                })
                .or_insert_with(|| Value::List(vec![Value::String("final".to_string())]));
            let func = self.handle_function_declaration(m);
            if let Value::Function(f) = func {
                if !f.is_static() {
                    let mut func_params = f.get_parameters().to_vec();
                    if func_params.is_empty() || !(func_params[0].is_positional() && !func_params[0].is_positional_optional()) {
                        self.variables = saved_variables;
                        return self.raise(
                            "TypeError",
                            &format!("Non-static method '{}' must have the first parameter positional and not optional", f.get_name())
                        );
                    }
                    let instance = Parameter::instance(&func_params[0].name, &func_params[0].ty, func_params[0].mods.clone());
                    func_params[0] = instance;
                    let mut new_method = f.clone();
                    new_method.set_parameters(func_params);
                    methods.insert(new_method.get_name().to_string(), Value::Function(new_method));
                } else {
                    methods.insert(f.get_name().to_string(), Value::Function(f));
                }
            } else {
                return self.raise("TypeError", "Expected a function in struct methods");
            }
        }
        self.variables = saved_variables;

        if let Value::Type(Type::Struct { methods: existing_methods, .. }) = &mut struct_value {
            for (name, method) in methods {
                if existing_methods.iter().any(|(method_name, mmethod)| (name == *method_name) && mmethod.is_final()) {
                    return self.raise("NameError", &format!("Method '{}' is already defined in struct '{}'", name, struct_name));
                }
                let f = if let Value::Function(f) = method {
                    f
                } else {
                    return self.raise("TypeError", "Expected a function in struct methods");
                };
                existing_methods.push((name, f));
            }
        } else {
            return self.raise("TypeError", &format!("'{}' is not a struct type", struct_name));
        }

        struct_variable.set_value(struct_value);
        self.variables.insert(struct_name.clone(), struct_variable);

        Value::Null
    }

    fn handle_struct_creation(&mut self, statement: HashMap<Value, Value>) -> Value {
        let struct_name = match statement.get(&Value::String("name".to_string())) {
            Some(Value::String(n)) => n,
            _ => return self.raise("RuntimeError", "Missing or invalid 'name' in struct creation"),
        };

        let fields: HashMap<String, (Box<Value>, Type)> = match statement.get(&Value::String("fields".to_string())) {
            Some(Value::Map { keys, values }) => keys
                .iter()
                .zip(values.iter())
                .map(|(k, v)| (k.to_string(), (Box::new(v.clone()), Type::new_simple("any"))))
                .collect(),
            _ => return self.raise("RuntimeError", "Missing or invalid 'fields' in struct creation"),
        };

        let mut new_fields = HashMap::new();
        for (k, (v, ty)) in &fields {
            let val = self.evaluate(&v.convert_to_statement());
            new_fields.insert(k.clone(), (Box::new(val), ty.clone()));
            if self.err.is_some() {
                return NULL;
            }
        }
        let fields = new_fields;

        let is_null = match statement.get(&Value::String("is_null".to_string())) {
            Some(Value::Boolean(b)) => *b,
            _ => false,
        };

        let struct_ty = match self.variables.get(struct_name) {
            Some(Variable { value: Value::Type(ty), .. }) => match ty {
                Type::Struct { .. } => ty.clone(),
                _ => return self.raise("TypeError", &format!("'{}' is not a struct type. ({})", struct_name, ty.display_simple())),
            },
            Some(v) => {
                return self.raise("TypeError", &format!("'{}' is not a struct type. ({})", struct_name, v.get_type().display_simple()))
            }
            _ => return self.raise("TypeError", &format!("Struct '{}' does not exist", struct_name)),
        };

        if is_null {
            Value::Struct(Struct::new_as_null(struct_ty))
        } else {
            let type_fields = if let Type::Struct { fields, .. } = &struct_ty {
                fields
            } else {
                &vec![]
            };

            let type_fields_names: Vec<_> = type_fields.iter().map(|(name, _, _)| name).collect();

            for (name, _, _) in type_fields {
                if !fields.contains_key(name) {
                    return self.raise("ValueError", &format!("Missing field '{}' in struct '{}'", name, struct_name));
                }
                if !type_fields_names.contains(&name) {
                    return self.raise("NameError", &format!("Field '{}' is not defined in struct type '{}'", name, &struct_name));
                }
            }

            let mut final_fields = HashMap::new();

            for (field, (val, _)) in &fields {
                if type_fields_names.iter().all(|name| *name != field) {
                    let owned: Vec<String> = type_fields_names.iter().map(|s| s.to_string()).collect();
                    let closest_match = find_closest_match(field, &owned);
                    if let Some(closest_match) = closest_match {
                        return self.raise("NameError", &format!("Field '{}' is not defined in struct '{}'. Did you mean '{}'?", field, struct_name, closest_match));
                    }
                    return self.raise("NameError", &format!("Field '{}' is not defined in struct '{}'", field, &struct_name));
                }
                let ty_opt = type_fields
                    .iter()
                    .find(|(name, _, _)| name == field)
                    .map(|(_, ty, _)| ty.clone());

                
                let saved_variables = self.variables.clone();
                let generics = if let Type::Struct { generics, .. } = &struct_ty {
                    generics
                } else {
                    &vec![]
                };
                for generic in generics {
                    self.variables.insert(generic.clone(), Variable::new(generic.clone(), Value::Type(Type::new_simple("any")), "type".to_string(), true, false, true));
                }
                let expected_type = ty_opt.as_ref().map(|ty| self.evaluate(ty)).unwrap_or(Value::Null);
                self.variables = saved_variables;

                if self.err.is_some() {
                    return NULL;
                }

                let t = if let Value::Type(t) = expected_type {
                    if !self.check_type(&*val, &t).0 {
                        return self.raise("TypeError", &format!("Expected type '{}' for field '{}' of struct '{}' but got '{}'", t.display_simple(), field, &struct_name, val.get_type().display_simple()));
                    };
                    t
                } else {
                    return self.raise("TypeError", "Expected a type in struct field definition");
                };

                final_fields.insert(field.clone(), (val.clone(), t));
            }

            Value::Struct(Struct::new_with_fields(struct_ty, final_fields))
        }
    }

    fn handle_export(&mut self, statement: HashMap<Value, Value>) -> Value {
        let names = match statement.get(&Value::String("names".to_string())) {
            Some(Value::List(n)) => n,
            _ => return self.raise("RuntimeError", "Missing or invalid 'names' in export"),
        };

        let aliases = match statement.get(&Value::String("aliases".to_string())) {
            Some(Value::List(a)) => a,
            _ => return self.raise("RuntimeError", "Missing or invalid 'aliases' in export"),
        };

        let modifiers_list = match statement.get(&Value::String("modifiers".to_string())) {
            Some(Value::List(m)) => m,
            _ => return self.raise("RuntimeError", "Expected a list for 'modifiers' in export"),
        };

        if names.len() != aliases.len() || names.len() != modifiers_list.len() {
            return self.raise("RuntimeError", "Mismatched lengths of names, aliases, or modifiers");
        }

        let mut exported_values = vec![];

        for i in 0..names.len() {
            let name = match &names[i] {
                Value::String(s) => s,
                _ => return self.raise("RuntimeError", "Invalid name in export list"),
            };
            let alias = match &aliases[i] {
                Value::String(s) => s,
                _ => return self.raise("RuntimeError", "Invalid alias in export list"),
            };
            let modifiers = match &modifiers_list[i] {
                Value::List(l) => l,
                _ => return self.raise("RuntimeError", "Invalid modifiers list for export item"),
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

            if let Some(var) = self.variables.get(name).cloned() {
                if !var.is_public() && !is_public {
                    return self.raise_with_help(
                        "ModifierError",
                        &format!("Variable '{}' is not public and cannot be exported", name),
                        "Add 'public' modifier to the export statement to make it public"
                    );
                }

                let mut var = var;
                var.set_public(true);
                if let Some(is_static) = is_static {
                    var.set_static(is_static);
                }
                if let Some(is_final) = is_final {
                    var.set_final(is_final);
                }

                self.variables.insert(alias.clone(), var.clone());
                exported_values.push(var.value.clone());
            } else {
                return self.raise("NameError", &format!("Variable '{}' is not defined therefore cannot be exported", name));
            }
        }

        if exported_values.len() == 1 {
            return exported_values.first().cloned().unwrap_or(Value::Null);
        } else {
            return Value::Tuple(exported_values);
        }
    }

    fn handle_group(&mut self, statement: HashMap<Value, Value>) -> Value {
        let expressions = match statement.get(&Value::String("expressions".to_string())) {
            Some(Value::List(exprs)) => exprs,
            _ => return self.raise("RuntimeError", "Missing or invalid 'expressions' in group"),
        };

        let mut result = Value::Null;
        for expr in expressions {
            result = self.evaluate(&expr.convert_to_statement());
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
            _ => return self.raise("RuntimeError", "Missing or invalid 'name' in generator declaration"),
        };
    
        let pos_args = match statement.get(&Value::String("pos_args".to_string())) {
            Some(Value::List(p)) => p,
            _ => return self.raise("RuntimeError", "Expected a list for 'pos_args' in generator declaration"),
        };

        let named_args = match statement.get(&Value::String("named_args".to_string())) {
            Some(Value::Map { keys, values }) => {
                Value::Map {
                    keys: keys.clone(),
                    values: values.clone(),
                }
            }
            _ => return self.raise("RuntimeError", "Expected a list for 'named_args' in generator declaration"),
        };
    
        let body = match statement.get(&Value::String("body".to_string())) {
            Some(Value::List(b)) => b,
            _ => return self.raise("RuntimeError", "Expected a list for 'body' in generator declaration"),
        };

        let modifiers = match statement.get(&Value::String("modifiers".to_string())) {
            Some(Value::List(m)) => m,
            _ => return self.raise("RuntimeError", "Expected a list for 'modifiers' in generator declaration"),
        };
    
        let return_type = match statement.get(&Value::String("return_type".to_string())) {
            Some(Value::Map { keys, values } ) => Value::Map {
                keys: keys.clone(),
                values: values.clone(),
            },
            _ => return self.raise("RuntimeError", "Missing or invalid 'return_type' in generator declaration"),
        };

        let effect_flags: EffectFlags = match statement.get(&Value::String("effects".to_string())) {
            Some(Value::Int(i)) => match i.to_i64() {
                Ok(v) if v >= 0 => EffectFlags::from_u32(v as u32),
                _ => return self.raise("RuntimeError", "'effect_flags' must be a non-negative integer"),
            },
            _ => return self.raise("RuntimeError", "Expected an integer for 'effect_flags' in generator declaration"),
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
                    _ => return self.raise("SyntaxError", &format!("Unknown generator modifier: {}", modifier_str)),
                }
            } else {
                return self.raise("TypeError", "Generator modifiers must be strings");
            }
        }

        let mut parameters = Vec::new();

        for arg in pos_args {
            if let Value::Map { keys, values } = arg {
                let name = match keys.iter().position(|k| k == &Value::String("name".to_string())) {
                    Some(pos) => match &values[pos] {
                        Value::String(n) => n,
                        _ => return self.raise("RuntimeError", "'name' must be a string in generator parameter"),
                    },
                    None => return self.raise("RuntimeError", "Missing 'name' in generator parameter"),
                };

                let type_value = match keys.iter().position(|k| k == &Value::String("type".to_string())) {
                    Some(pos) => {
                        let type_val = &values[pos];
                        match self.evaluate(&type_val.convert_to_statement()) {
                            Value::Type(t) => t,
                            _ => {
                                self.raise("TypeError", "Expected 'var_type' to be a Type");
                                return NULL;
                            }
                        }
                    }
                    None => return self.raise("RuntimeError", "Missing 'type' in generator parameter"),
                };

                let mods = match keys.iter().position(|k| k == &Value::String("modifiers".to_string())) {
                    Some(pos) => match &values[pos] {
                        Value::List(l) => l.iter().filter_map(|v| {
                            if let Value::String(s) = v {
                                Some(s.clone())
                            } else { None }
                        }).collect(),
                        _ => vec![],
                    },
                    None => vec![],
                };

                // New: check if the parameter is variadic
                let is_variadic = match keys.iter().position(|k| k == &Value::String("variadic".to_string())) {
                    Some(pos) => matches!(values[pos], Value::Boolean(true)),
                    None => false,
                };

                if is_variadic {
                    parameters.push(Parameter::variadic_optional(
                        name.as_str(),
                        "any",
                        Value::List(vec![]),
                    ).set_mods(mods));
                } else {
                    parameters.push(Parameter::positional_pt(name.as_str(), &type_value).set_mods(mods));
                }
            } else {
                return self.raise("TypeError", "Expected a map for generator parameter");
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

                let type_eval = match self.evaluate(&type_val.convert_to_statement()) {
                    Value::Type(t) => t,
                    _ => {
                        self.raise("TypeError", "Expected 'var_type' to be a Type");
                        return NULL;
                    }
                };
        
                let value = match keys.iter().position(|k| k == &Value::String("value".to_string())) {
                    Some(pos) => {
                        let val = &values[pos];
                        self.evaluate(&val.convert_to_statement())
                    }
                    None => NULL,
                };
        
                parameters.push(Parameter::positional_optional_pt(
                    name_str.as_str(),
                    &type_eval,
                    value,
                ).set_mods(mods));
            } else {
                return self.raise("TypeError", "Expected a map for named argument");
            }
        }        

        let body_formatted: Arc<Vec<Statement>> = Arc::new(body.iter().map(|v| v.convert_to_statement()).collect());
        let ret_type = Arc::new(match self.evaluate(&return_type.convert_to_statement()) {
            Value::Type(t) => t,
            _ => {
                self.raise("TypeError", "Expected 'return_type' to be a Type");
                return NULL;
            }
        });

        if self.err.is_some() {
            return NULL;
        }

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
                ret_type: (*ret_type).clone(),
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
                    effect_flags,
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
            Some(Value::List(g)) => g.iter().filter_map(|v| {
                if let Value::String(s) = v {
                    Some(s.clone())
                } else { None }
            }).collect(),
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
                    _ => {
                        self.raise("ModifierError", &format!("Unknown type modifier: {}", modifier_str));
                        return NULL;
                    }
                }
            } else {
                self.raise("TypeError", "Type modifiers must be strings");
                return NULL;
            }
        }

        match type_kind.as_str() {
            "alias" => {
                let alias_type = match statement.get(&Value::String("alias".to_string())) {
                    Some(Value::Map { .. }) => statement.get(&Value::String("alias".to_string())).unwrap(),
                    _ => {
                        self.raise("RuntimeError", "Expected a map for 'alias' in type declaration");
                        return NULL;
                    }
                };
                let alias_evaluated = self.evaluate(&alias_type.convert_to_statement());
                if self.err.is_some() {
                    return NULL;
                }
                let base_type = if let Value::Type(t) = alias_evaluated {
                    t
                } else {
                    self.raise("TypeError", "Alias must evaluate to a Type");
                    return NULL;
                };
                let alias_type = Value::Type(Type::Alias {
                    name: name.clone(),
                    base_type: Box::new(base_type),
                    variables,
                    conditions,
                });
                if self.variables.contains_key(&name) {
                    if let Some(var) = self.variables.get(&name) {
                        if var.is_final() {
                            let s = if matches!(var.value, Value::Type(_)) { "type" } else { "variable" };
                            self.raise("AssigmentError", &format!("Cannot redefine final {} '{}'", s, &name));
                            return NULL;
                        }
                    }
                }
                self.variables.insert(
                    name.clone(),
                    Variable::new(
                        name.clone(),
                        alias_type,
                        "type".to_string(),
                        is_static,
                        is_public,
                        is_final,
                    ),
                );
                self.variables.get(&name)
                    .map_or(NULL, |var| var.value.clone())
            }
            "enum" => {
                let variants = match statement.get(&Value::String("variants".to_string())) {
                    Some(Value::List(v)) => v,
                    _ => {
                        self.raise("RuntimeError", "Expected a list for 'variants' in enum declaration");
                        return NULL;
                    }
                };
                if self.err.is_some() {
                    return NULL;
                }
                let wheres: Vec<(String, Value)> = match statement.get(&Value::String("wheres".to_string())) {
                    Some(Value::Map { keys, values }) => keys.iter().cloned().zip(values.iter().cloned()).map(|(k, v)| {
                        if let Value::String(s) = k {
                            (s, {
                                let val = self.evaluate(&v.convert_to_statement());
                                if self.err.is_some() {
                                    return ("".to_string(), NULL);
                                }
                                val
                            })
                        } else {
                            self.raise("RuntimeError", "Expected string keys in 'wheres' map");
                            ("".to_string(), NULL)
                        }
                    }).filter(|(k, _)| !k.is_empty()).collect(),
                    _ => vec![],
                };
                if self.err.is_some() {
                    return NULL;
                }
                let mut variants_processed: Vec<(String, Statement, usize)> = Vec::new();
                for v in variants {
                    match v {
                        Value::Map { keys, values } => {
                            let n = match keys.iter().position(|k| k == &Value::String("name".to_string())) {
                                Some(pos) => match values.get(pos).cloned().unwrap_or(Value::Null) {
                                    Value::String(s) => s,
                                    _ => {
                                        self.raise("RuntimeError", "Expected 'name' to be a string in enum variant");
                                        return NULL;
                                    }
                                },
                                None => {
                                    self.raise("RuntimeError", "Expected 'name' in enum variant");
                                    return NULL;
                                }
                            };
                            let t = match keys.iter().position(|k| k == &Value::String("type".to_string())) {
                                Some(pos) => match values.get(pos).cloned().unwrap_or(Value::Null) {
                                    Value::Map { keys, values } => {
                                        let m = Value::Map { keys, values };
                                        m.convert_to_statement()
                                    }
                                    Value::Null => Statement::Null,
                                    _ => {
                                        self.raise("RuntimeError", "Expected 'type' to be a map in enum variant");
                                        return NULL;
                                    }
                                },
                                None => {
                                    self.raise("RuntimeError", "Expected 'type' in enum variant");
                                    return NULL;
                                }
                            };
                            let discriminant = match keys.iter().position(|k| k == &Value::String("discriminant".to_string())) {
                                Some(pos) => values.get(pos).unwrap().convert_to_statement(),
                                None => return self.raise("RuntimeError", "Expected 'discriminant' in enum variant"),
                            };
                            let usize_disc = match self.evaluate(&discriminant) {
                                Value::Int(n) => match n.to_usize() {
                                    Ok(u) => u,
                                    Err(_) => {
                                        self.raise_with_help("ValueError", "Discriminant integer too large in enum variant", &format!("expected in range between 0 and {}", usize::MAX));
                                        return NULL;
                                    }
                                },
                                _ => {
                                    match discriminant {
                                        Statement::Statement { loc, .. } => {
                                            self.raise("TypeError", "Expected 'discriminant' to be an integer in enum variant");
                                            if let Some(mut err) = self.err.take() {
                                                err.loc = loc;
                                                self.err = Some(err);
                                            }
                                            NULL
                                        },
                                        _ => self.raise("TypeError", "Expected 'discriminant' to be an integer in enum variant")
                                    };
                                    return NULL
                                }
                            };
                            variants_processed.push((n, t, usize_disc));
                        }
                        _ => {
                            self.raise("RuntimeError", "Expected a map for enum variant");
                            return NULL;
                        }
                    };
                };
                let enum_type = Value::Type(Type::Enum {
                    name: name.clone(),
                    variants: variants_processed,
                    generics,
                    wheres,
                });
                if self.variables.contains_key(&name) {
                    if let Some(var) = self.variables.get(&name) {
                        if var.is_final() {
                            let s = if matches!(var.value, Value::Type(_)) { "type" } else { "variable" };
                            self.raise("AssigmentError", &format!("Cannot redefine final {} '{}'", s, &name));
                            return NULL;
                        }
                    }
                }
                self.variables.insert(
                    name.clone(),
                    Variable::new(
                        name.clone(),
                        enum_type,
                        "type".to_string(),
                        is_static,
                        is_public,
                        is_final,
                    ),
                );
                debug_log(&format!("<Enum '{}' registered>", name), &self.config.clone(), Some(self.use_colors));
                self.variables.get(&name)
                    .map_or(NULL, |var| var.value.clone())
            }
            "struct" => {
                let fields: Vec<(String, Statement, Vec<String>)> = match statement.get(&Value::String("properties".to_string())) {
                    Some(Value::Map { keys, values }) => {
                        keys.iter().cloned().zip(values.iter().cloned()).map(|(k, v)| {
                            if let Value::String(s) = k {
                                if let Value::Tuple(vec) = v {
                                    if vec.len() == 2 {
                                        let map_value = &vec[0];
                                        let list_value = &vec[1];

                                        if let (Value::Map { keys, values }, Value::List(list)) = (map_value.clone(), list_value.clone()) {
                                            let stmt = Value::Map { keys, values }.convert_to_statement();
                                            let strs: Vec<String> = list.into_iter().filter_map(|v| {
                                                if let Value::String(s) = v { Some(s) } else { None }
                                            }).collect();
                                            (s, stmt, strs)
                                        } else {
                                            self.raise("RuntimeError", "Expected (Map, List) in tuple");
                                            (s, Statement::Null, vec![])
                                        }
                                    } else {
                                        self.raise("RuntimeError", "Expected tuple of length 2");
                                        (s, Statement::Null, vec![])
                                    }
                                } else {
                                    self.raise("RuntimeError", "Expected a tuple in properties");
                                    (s, Statement::Null, vec![])
                                }
                            } else {
                                self.raise("RuntimeError", "Expected string keys in 'properties' map");
                                ("".to_string(), Statement::Null, vec![])
                            }
                        }).filter(|(k, _, _)| !k.is_empty()).collect()
                    },
                    _ => {
                        self.raise("RuntimeError", "Expected a map for 'properties' in struct declaration");
                        return NULL;
                    }
                };
                let wheres: Vec<(String, Value)> = match statement.get(&Value::String("wheres".to_string())) {
                    Some(Value::Map { keys, values }) => keys.iter().cloned().zip(values.iter().cloned()).map(|(k, v)| {
                        if let Value::String(s) = k {
                            (s, {
                                let val = self.evaluate(&v.convert_to_statement());
                                if self.err.is_some() {
                                    return ("".to_string(), NULL);
                                }
                                val
                            })
                        } else {
                            self.raise("RuntimeError", "Expected string keys in 'wheres' map");
                            ("".to_string(), NULL)
                        }
                    }).filter(|(k, _)| !k.is_empty()).collect(),
                    _ => vec![],
                };
                if self.err.is_some() {
                    return NULL;
                }
                let struct_type = Value::Type(Type::Struct {
                    name: name.clone(),
                    fields,
                    methods: vec![],
                    generics,
                    wheres,
                });
                if self.variables.contains_key(&name) {
                    if let Some(var) = self.variables.get(&name) {
                        if var.is_final() {
                            let s = if matches!(var.value, Value::Type(_)) { "type" } else { "variable" };
                            self.raise("AssigmentError", &format!("Cannot redefine final {} '{}'", s, &name));
                            return NULL;
                        }
                    }
                }
                self.variables.insert(
                    name.clone(),
                    Variable::new(
                        name.clone(),
                        struct_type,
                        "type".to_string(),
                        is_static,
                        is_public,
                        is_final,
                    ),
                );
                debug_log(&format!("<Struct '{}' registered>", name), &self.config.clone(), Some(self.use_colors));
                self.variables.get(&name)
                    .map_or(NULL, |var| var.value.clone())
            }
            _ => {
                self.raise("RuntimeError", "Unknown type kind in type declaration");
                return NULL;
            }
        }
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

        let mut match_result: (Option<Value>, bool) = (None, false);
        for case in cases.iter() {
            if match_result.1 {
                break;
            }
            if let Value::Map { keys, values } = case {
                let style = match keys.iter().position(|k| k == &Value::String("style".to_string())) {
                    Some(pos) => match values.get(pos) {
                        Some(Value::String(s)) => s.as_str(),
                        _ => {
                            self.raise("RuntimeError", "Expected 'style' to be a string in match case");
                            return NULL;
                        }
                    },
                    None => {
                        self.raise("RuntimeError", "Missing 'style' in match case");
                        return NULL;
                    }
                };

                match style {
                    "pattern" => {
                        let pattern = match keys.iter().position(|k| k == &Value::String("pattern".to_string())) {
                            Some(pos) => values.get(pos).unwrap_or(&Value::Null),
                            None => {
                                self.raise("RuntimeError", "Expected 'pattern' in match case");
                                return NULL;
                            }
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

                        let guard = match keys.iter().position(|k| k == &Value::String("guard".to_string())) {
                            Some(pos) => values.get(pos).unwrap_or(&Value::Null),
                            None => {
                                self.raise("RuntimeError", "Expected 'guard' in match case");
                                return NULL;
                            }
                        };

                        let eval_cond = self.evaluate(&condition.convert_to_statement());
                        if self.err.is_some() {
                            return NULL;
                        }
                        let (matched, variables) = match check_pattern(&eval_cond, &pattern) {
                            Ok(matched) => matched,
                            Err((err_ty, err_msg)) => {
                                self.raise(&err_ty, &err_msg);
                                return NULL;
                            }
                        };

                        let mut variables: FxHashMap<String, Variable> = FxHashMap::from_iter(variables.into_iter().map(|(k, v)| {
                            (k.clone(), Variable::new(k, v.clone(), v.type_name().to_string(), false, true, true))
                        }));
                        let mut merged = self.variables.clone();
                        merged.extend(variables);
                        variables = merged;


                        if matched {
                            let mut res = NULL;
                            let mut cont = false;
                            if guard != &Value::Null {
                                let mut guard_interp = Interpreter::new(
                                    self.config.clone(),
                                    self.use_colors,
                                    &self.file_path,
                                    &self.cwd,
                                    self.preprocessor_info.clone(),
                                    &[],
                                );
                                guard_interp.variables.extend(variables.clone());
                                let guard_result = guard_interp.evaluate(&guard.convert_to_statement());
                                if guard_interp.err.is_some() {
                                    self.err = guard_interp.err.clone();
                                    drop(guard_interp);
                                    return NULL;
                                }
                                drop(guard_interp);
                                if !guard_result.is_truthy() {
                                    continue;
                                }
                            }
                            let mut interp = Interpreter::new(
                                self.config.clone(),
                                self.use_colors,
                                &self.file_path,
                                &self.cwd,
                                self.preprocessor_info.clone(),
                                &[],
                            );
                            interp.variables = variables;
                            'outer: for stmt in body.iter() {
                                let result = interp.evaluate(&stmt.convert_to_statement());
                                if interp.err.is_some() {
                                    self.err = interp.err.clone();
                                    return NULL;
                                }
                                match interp.state {
                                    State::Continue => {
                                        cont = true;
                                        interp.state = State::Normal;
                                        break 'outer;
                                    }
                                    State::Break => {
                                        interp.state = State::Normal;
                                        self.variables.extend(interp.variables.clone());
                                        drop(interp);
                                        return NULL;
                                    }
                                    _ => {}
                                }
                                res = result;
                            }
                            self.variables.extend(interp.variables.clone());
                            drop(interp);
                            if cont {
                                match_result = (Some(NULL), false);
                            } else {
                                match_result = (Some(res), true);
                            }
                        }
                    }
                    "literal" => {
                        let pattern = match keys.iter().position(|k| k == &Value::String("value".to_string())) {
                            Some(pos) => values.get(pos).unwrap_or(&Value::Null),
                            None => {
                                self.raise("RuntimeError", "Expected 'pattern' in match case");
                                return NULL;
                            }
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

                        let matched = {
                            self.evaluate(&condition.convert_to_statement()) == self.evaluate(&pattern.convert_to_statement())
                        };

                        if self.err.is_some() {
                            return NULL;
                        }

                        if matched {
                            let mut res = NULL;
                            let mut cont = false;
                            'outer: for stmt in body.iter() {
                                let result = self.evaluate(&stmt.convert_to_statement());
                                if self.err.is_some() {
                                    return NULL;
                                }
                                match self.state {
                                    State::Continue => {
                                        cont = true;
                                        self.state = State::Normal;
                                        break 'outer;
                                    },
                                    State::Break => {
                                        self.state = State::Normal;
                                        return NULL
                                    },
                                    _ => {}
                                }
                                res = result;
                            }
                            if cont {
                                match_result = (Some(NULL), false);
                            } else {
                                match_result = (Some(res), true);
                            }
                        }
                    }
                    _ => return self.raise("RuntimeError", "Invalid style in 'match' statement"),
                }
            } else {
                self.raise("RuntimeError", "Invalid case in 'match' statement - expected map");
            }
        }

        if self.err.is_some() {
            return NULL;
        }

        if match_result.0.is_none() {
            return self.raise_with_help("ValueError", "No value matched cases in match statement", "Add a '_ -> ...' case");
        }

        match_result.0.unwrap_or(NULL)
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

        let is_local = match statement.get(&Value::String("is_local".to_string())) {
            Some(Value::Boolean(b)) => *b,
            _ => false,
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

        if is_local {
            let mut result = NULL;
            for stmt in stmts.iter() {
                result = self.evaluate(&stmt);
                if self.err.is_some() {
                    return NULL;
                }
                match self.state {
                    State::Break => {
                        self.state = State::Normal;
                        break;
                    }
                    _ => {}
                }
            }
            return result;
        }

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

        let res = scope_interpreter.interpret(stmts, true);

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
        match res {
            Ok(v) => v,
            Err(_) => NULL,
        }
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
        static CONDITION_KEY: &str = "condition";
        static BODY_KEY: &str = "body";

        let condition = match statement.get(&Value::String(CONDITION_KEY.to_string())) {
            Some(v) => v,
            _ => {
                self.raise("SyntaxError", "Missing 'condition' in while loop");
                return NULL;
            }
        };

        let body = match statement.get(&Value::String(BODY_KEY.to_string())) {
            Some(Value::List(b)) => b,
            _ => {
                self.raise("SyntaxError", "Expected a list for 'body' in while loop");
                return NULL;
            }
        };

        let body_statements: Vec<_> = body.iter().map(|stmt| stmt.convert_to_statement()).collect();

        while self.evaluate(&condition.convert_to_statement()).is_truthy() {
            for stmt in &body_statements {
                let result = self.evaluate(&stmt);
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
                }
                State::Continue => {
                    self.state = State::Normal;
                    continue;
                }
                _ => {}
            }
        }

        NULL
    }

    fn handle_pointer(&mut self, statement: HashMap<Value, Value>) -> Value {
        let pointer_type = statement.get(&Value::String("type".to_string())).unwrap_or(&NULL);
        let value_opt = statement.get(&Value::String("value".to_string())).unwrap_or(&NULL);

        if *pointer_type == Value::String("POINTER_ASSIGN".to_string()) {
            if self.err.is_some() {
                self.err = None;
            }
        }

        let value = self.evaluate(&value_opt.convert_to_statement());

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
                match value {
                    Value::Type(mut t) => {
                        Value::Type(t.set_reference(true).unmut())
                    }
                    _ => Value::Pointer(Arc::new(value)),
                }
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
                    &statement
                        .get(&Value::String("left".to_string()))
                        .unwrap_or(&NULL)
                        .convert_to_statement(),
                );
                if self.err.is_some() {
                    return NULL;
                }
                let right = self.evaluate(
                    &statement
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

        let value = self.evaluate(&value_opt.convert_to_statement());

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
                                Some(t) => match self.evaluate(&t.convert_to_statement()) {
                                    Value::Type(t) => t,
                                    _ => {
                                        self.raise("TypeError", "Expected 'var_type' to be a Type");
                                        return NULL;
                                    }
                                }
                                _ => {
                                    self.raise("TypeError", "Missing or invalid 'var_type' in variable declaration");
                                    return NULL;
                                }
                            };

                            if type_ != Type::new_simple("auto") && let (is_valid, err) = self.check_type(&val, &type_) && !is_valid {
                                if let Some(err) = err {
                                    self.raise_with_ref("TypeError", &format!(
                                        "Value '{}' does not match the type '{}'",
                                        val, type_.display_simple()
                                    ), err);
                                } else {
                                    self.raise("TypeError", &format!(
                                        "Value '{}' does not match the type '{}'",
                                        val, type_.display_simple()
                                    ));
                                }
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

            let evaluated_key = self.evaluate(&raw_key.convert_to_statement());
            if self.err.is_some() {
                return NULL;
            }
            let evaluated_value = self.evaluate(&raw_values[i].convert_to_statement());
            if self.err.is_some() {
                return NULL;
            }
            if !seen.insert(evaluated_key.clone()) {
                self.raise("SyntaxError", &format!("Duplicate key in map: {}", evaluated_key));
                return NULL;
            }
            
            keys.push(evaluated_key);
            values.push(evaluated_value);
        }
    
        Value::Map { keys, values }
    }

    fn convert_value_to_type(&mut self, target_type: &str, value: &Value) -> Value {
        match target_type {
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
                        match f.to_int() {
                            Ok(i) => Value::Int(i),
                            Err(_) => {
                                self.raise("ConversionError", "Failed to convert float to int");
                                NULL
                            }
                        }
                    } else {
                        let rounded = f.round(0);
                        match rounded.to_int() {
                            Ok(i) => Value::Int(i),
                            Err(_) => {
                                self.raise("ConversionError", "Failed to convert rounded float to int");
                                NULL
                            }
                        }
                    }
                } else if let Value::String(s) = value {
                    if !is_number(&s) {
                        return self.raise("ConversionError", &format!("Invalid number format: '{}'", s));
                    }
                    let stmt = Statement::Statement {
                        keys: vec![Value::String("type".to_string()), Value::String("value".to_string())],
                        values: vec![Value::String("NUMBER".to_string()), Value::String(s.clone())],
                        loc: self.get_location_from_current_statement(),
                    };
                    let result = self.evaluate(&stmt);
                    if self.err.is_some() {
                        return NULL;
                    }
                    if let Value::Int(_) = result {
                        result
                    } else if let Value::Float(_) = result {
                        self.raise_with_help("ConversionError", &format!("Failed to convert string '{}' to int", s), "Maybe you meant to use 'float'?");
                        NULL
                    } else {
                        self.raise("ConversionError", &format!("Failed to convert string '{}' to int", s));
                        NULL
                    }
                } else if let Value::Boolean(b) = value {
                    Value::Int(if *b { Int::from_i64(1) } else { Int::from_i64(0) })
                } else {
                    self.raise("TypeError", &format!("Cannot convert '{}' to int", value.type_name()));
                    NULL
                }
            },
            "float" => {
                if let Value::Float(f) = value {
                    Value::Float(f.clone())
                } else if let Value::Int(i) = value {
                    match i.to_float() {
                        Ok(f) => Value::Float(f),
                        Err(_) => {
                            self.raise("ConversionError", "Failed to convert int to float");
                            NULL
                        }
                    }
                } else if let Value::String(s) = value {
                    match Float::from_str(&s) {
                        Ok(f) => Value::Float(f),
                        Err(_) => {
                            self.raise("ConversionError", &format!("Failed to convert string '{}' to float", s));
                            NULL
                        }
                    }
                } else {
                    self.raise("TypeError", &format!("Cannot convert '{}' to float", value.type_name()));
                    NULL
                }
            },
            "bool" => {
                Value::Boolean(value.is_truthy())
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
                    Value::List(t.iter().cloned().collect())
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
                    self.raise("TypeError", &format!("Cannot convert '{}' to list", value.type_name()));
                    NULL
                }
            },
            "map" => {
                if let Value::Map { keys, values } = value {
                    Value::Map { keys: keys.clone(), values: values.clone() }
                } else if let Value::Module(obj) = value {
                    let mut keys = Vec::new();
                    let mut values = Vec::new();
                    for (name, var) in obj.get_properties().iter() {
                        keys.push(Value::String(name.clone()));
                        values.push(var.value.clone());
                    }
                    Value::Map { keys, values }
                } else {
                    self.raise("TypeError", &format!("Cannot convert '{}' to map", value.type_name()));
                    NULL
                }
            },
            "bytes" => {
                if let Value::Bytes(b) = value {
                    Value::Bytes(b.clone())
                } else if let Value::String(s) = value {
                    Value::Bytes(s.clone().into_bytes())
                } else {
                    self.raise("TypeError", &format!("Cannot convert '{}' to bytes", value.type_name()));
                    NULL
                }
            },
            "object" => {
                if let Value::Module(obj) = value {
                    Value::Module(obj.clone())
                } else {
                    self.raise("TypeError", &format!("Cannot convert '{}' to object", value.type_name()));
                    NULL
                }
            },
            "function" => {
                if let Value::Function(func) = value {
                    Value::Function(func.clone())
                } else {
                    self.raise("TypeError", &format!("Cannot convert '{}' to function", value.type_name()));
                    NULL
                }
            },
            "tuple" => {
                if let Value::Tuple(t) = value {
                    Value::Tuple(t.clone())
                } else if let Value::List(l) = value {
                    Value::Tuple(l.iter().cloned().collect())
                } else if let Value::String(s) = value {
                    Value::Tuple(s.chars().map(|c| Value::String(c.to_string())).collect())
                } else {
                    self.raise("TypeError", &format!("Cannot convert '{}' to tuple", value.get_type().display_simple()));
                    NULL
                }
            },
            _ => {
                self.raise("NotImplemented", &format!("Type conversion to '{}' is not implemented", target_type));
                NULL
            }
        }
    }

    pub fn handle_type_conversion(&mut self, statement: HashMap<Value, Value>) -> Value {
        if self.err.is_some() {
            return NULL;
        }
        
        let value_opt = match statement.get(&Value::String("value".to_string())) {
            Some(v) => v,
            None => return self.raise("RuntimeError", "Missing 'value' in type conversion statement"),
        };

        let value = self.evaluate(&value_opt.convert_to_statement());
        if self.err.is_some() {
            return NULL;
        }

        let target_type_opt = match statement.get(&Value::String("to".to_string())) {
            Some(Value::Map { keys, values }) => {
                Value::Map {
                    keys: keys.clone(),
                    values: values.clone(),
                }
            }
            _ => return self.raise("RuntimeError", "Missing or invalid 'to' in type conversion statement"),
        };

        let binding = self.evaluate(&target_type_opt.convert_to_statement());
        if self.err.is_some() {
            return NULL;
        }
        let (target_type, target_type_type) = match &binding {
            Value::Type(tt) => match get_inner_type(tt) {
                Ok((t, _)) => (t, tt),
                Err(e) => return self.raise("TypeError", &e),
            },
            _ => return self.raise("TypeError", &format!(
                    "Type '{}' is not a valid target type for conversion",
                    value.type_name()
                )),
        };

        if self.err.is_some() {
            return NULL;
        }

        match target_type_type {
            Type::Struct { .. } => {
                return self.raise("NotImplemented", "Conversion to struct types is not implemented yet");
            }
            Type::Enum { .. } => {
                return self.raise("NotImplemented", "Conversion to enum types is not implemented yet");
            }
            _ => {}
        }

        if target_type.starts_with("&") {
            let new_type = target_type.trim_start_matches('&').to_string();
            if !VALID_TYPES.contains(&new_type.as_str()) {
                return self.raise("TypeError", &format!("Invalid target type '{}'", target_type));
            }
            let new_value = self.convert_value_to_type(&new_type, &value);
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
            let new_value = self.convert_value_to_type(&new_type, &value);
            if self.err.is_some() {
                return NULL;
            }
            return new_value;
        }

        if !self.check_type_validity(&target_type) {
            return self.raise("TypeError", &format!("Invalid target type '{}'", target_type));
        }

        let result = self.convert_value_to_type(&target_type, &value);
        if self.err.is_some() {
            return NULL;
        }

        let (is_valid, err) = self.check_type(&result, &target_type_type);
        if !is_valid {
            if let Some(err) = err {
                self.raise_with_ref("TypeError", &format!("Value '{}' does not match the type '{}'", result, target_type_type.display_simple()), err);
            } else {
                self.raise("TypeError", &format!("Value '{}' does not match the type '{}'", result, target_type_type.display_simple()));
            }
            return NULL;
        }
        if self.err.is_some() {
            return NULL;
        }
        result
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

        let import_all = match statement.get(&Value::String("import_all".to_string())) {
            Some(Value::Boolean(b)) => *b,
            _ => {
                self.raise("RuntimeError", "Missing or invalid 'import_all' in import statement");
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
    
        if self.variables.contains_key(alias) && named_imports.is_empty() && !import_all {
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
                #[cfg(target_arch = "wasm32")]
                "clib" => {
                    self.stack.pop();
                    return self.raise(
                        "ImportError",
                        "The 'clib' module is not supported in WebAssembly builds",
                    );
                }
                #[cfg(not(target_arch = "wasm32"))]
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
                #[cfg(target_arch = "wasm32")]
                "fs" => {
                    self.stack.pop();
                    return self.raise(
                        "ImportError",
                        "The 'fs' module is not supported in WebAssembly builds",
                    );
                }
                #[cfg(not(target_arch = "wasm32"))]
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
                #[cfg(target_arch = "wasm32")]
                "libload" => {
                    self.stack.pop();
                    return self.raise(
                        "ImportError",
                        "The 'libload' module is not supported in WebAssembly builds",
                    );
                }
                #[cfg(not(target_arch = "wasm32"))]
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
                #[cfg(target_arch = "wasm32")]
                "elevator" => {
                    self.stack.pop();
                    return self.raise(
                        "ImportError",
                        "The 'elevator' module is not supported in WebAssembly builds",
                    );
                }
                #[cfg(not(target_arch = "wasm32"))]
                "elevator" => {
                    use crate::env::libs::elevator::__init__ as elevator;
                    let elevator_module_props = elevator::register(&self.config);
                    for (name, var) in elevator_module_props {
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
            if cfg!(target_arch = "wasm32") {
                self.stack.pop();
                return self.raise(
                    "ImportError",
                    &format!("Module '{}' not found in WebAssembly build", module_name),
                );
            }
            let module_path_opt = statement.get(&Value::String("path".to_string()));
    
            let libs_dir = PathBuf::from(self.config.home_dir.clone()).join("libs");
    
            let base_module_path = match module_path_opt {
                Some(Value::Map { keys, values }) => {
                    let map_statement = Value::Map {
                        keys: keys.clone(),
                        values: values.clone(),
                    }.convert_to_statement();

                    let path_eval = self.evaluate(&map_statement);
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
    
            let mut resolved_module_path: Option<PathBuf> = None;
            let module_variants = generate_name_variants(&module_name);
    
            for variant in module_variants {
                let candidate_dir = base_module_path.join(&variant);

                if candidate_dir.exists() && candidate_dir.is_dir() {
                    resolved_module_path = Some(candidate_dir);
                    break;
                } else {
                    let extensions = [".lc", ".lucia", ".rs", ""];
                    for ext in extensions.iter() {
                        let candidate_file = base_module_path.join(format!("{}{}", variant, ext));
                        if candidate_file.exists() && candidate_file.is_file() {
                            resolved_module_path = Some(candidate_file);
                            break;
                        }
                    }
                }

                if resolved_module_path.is_some() {
                    break;
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
                                    let help = if self.config.allow_inline_config {
                                        format!(
                                            "Use #config {} = {} to set the expected value.",
                                            key,
                                            format_value(&expected_value)
                                        )
                                    } else {
                                        format!(
                                            "You would use #config {} = {} to set the expected value, however, inline configuration is disabled.",
                                            key,
                                            format_value(&expected_value)
                                        )
                                    };
                                    return self.raise_with_help("ImportError", &format!(
                                        "Config key '{}' value mismatch. Expected: {}, Found: {}",
                                        key,
                                        format_value(&expected_value),
                                        format_value(&actual_value)
                                    ), &help);
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
    
        if self.config.debug {
            for (name, var) in properties.iter() {
                let category = match &var.value {
                    Value::Module(..) => "object",
                    Value::Function(_) if var.is_public() => "function",
                    _ if var.is_final() => "constant",
                    _ => "variable",
                };
                categorized.entry(category).or_default().push(name);
            }
        }
    
        let order = ["object", "function", "constant", "variable"];

        if import_all {
            for (name, var) in properties.iter() {
                self.variables.insert(
                    name.clone(),
                    Variable::new(name.clone(), var.value.clone(), var.type_name().to_string(), var.is_final(), var.is_public(), var.is_static()),
                );
            }
            if self.config.debug {
                for &category in &order {
                    if let Some(names) = categorized.get(category) {
                        for name in names {
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
        } else if !named_imports.is_empty() {
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
            if self.config.debug {
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
            }
            if self.err.is_some() {
                self.stack.pop();
                return NULL;
            }
            Value::Null
        } else {
            if self.config.debug {
                for &category in &order {
                    if let Some(names) = categorized.get(category) {
                        for name in names {
                            debug_log(&format!("<Importing {} '{}' from module '{}'>", category, name, module_name), &self.config, Some(self.use_colors.clone()));
                        }
                    }
                }
            }
            if self.err.is_some() {
                self.stack.pop();
                return NULL;
            }
        
            debug_log(&format!("<Module '{}' imported successfully>", module_name), &self.config, Some(self.use_colors.clone()));

            let module = Value::Module(Module {
                name: module_name.clone(),
                properties,
                parameters: Vec::new(),
                is_public,
                is_static,
                is_final,
                state: None,
                path: PathBuf::from(module_path.clone())
            });

            self.variables.insert(
                alias.to_string(),
                Variable::new(alias.to_string(), module, "module".to_string(), is_static, is_public, is_final),
            );
        
            self.stack.pop();

            self.variables.get(alias).unwrap().value.clone()
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
                self.evaluate(&stmt);
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

        let evaluated_value = self.evaluate(&stmt);
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

        let mut return_type_str = match self.evaluate(&return_type.convert_to_statement()) {
            Value::Type(t) => t,
            _ => return self.raise("RuntimeError", "Invalid 'return_type' in function declaration"),
        };

        let effect_flags: EffectFlags = match statement.get(&Value::String("effects".to_string())) {
            Some(Value::Int(i)) => EffectFlags::from_u32(match i.to_i64() {
                Ok(v) if v >= 0 => v as u32,
                _ => return self.raise("TypeError", "'effects' must be a non-negative integer"),
            }),
            _ => EffectFlags::empty(),
        };

        if return_type_str == Type::new_simple("auto") {
            return_type_str = Type::new_simple("any");
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

                let type_value = match keys.iter().position(|k| k == &Value::String("type".to_string())) {
                    Some(pos) => {
                        let type_val = &values[pos];
                        match self.evaluate(&type_val.convert_to_statement()) {
                            Value::Type(t) => t,
                            _ => {
                                if self.err.is_some() {
                                    return NULL;
                                }
                                return self.raise("RuntimeError", "Invalid 'type' in function parameter");
                            },
                        }
                    }
                    None => return self.raise("RuntimeError", "Missing 'type' in function parameter"),
                };

                let mods = match keys.iter().position(|k| k == &Value::String("modifiers".to_string())) {
                    Some(pos) => match &values[pos] {
                        Value::List(l) => l.iter().filter_map(|v| {
                            if let Value::String(s) = v {
                                Some(s.clone())
                            } else { None }
                        }).collect(),
                        _ => vec![],
                    },
                    None => vec![],
                };

                let is_variadic = match keys.iter().position(|k| k == &Value::String("variadic".to_string())) {
                    Some(pos) => matches!(values[pos], Value::Boolean(true)),
                    None => false,
                };

                if is_variadic {
                    parameters.push(Parameter::variadic_optional(
                        name.as_str(),
                        "any",
                        Value::List(vec![]),
                    ).set_mods(mods));
                } else {
                    parameters.push(Parameter::positional_pt(name.as_str(), &type_value).set_mods(mods));
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

                let type_eval = match self.evaluate(&type_val.convert_to_statement()) {
                    Value::Type(t) => t,
                    _ => return self.raise("RuntimeError", "Invalid 'type' in named argument"),
                };

                if self.err.is_some() {
                    return NULL;
                }
        
                let value = match keys.iter().position(|k| k == &Value::String("value".to_string())) {
                    Some(pos) => {
                        let val = &values[pos];
                        self.evaluate(&val.convert_to_statement())
                    }
                    None => NULL,
                };

                if self.err.is_some() {
                    return NULL;
                }
                
                parameters.push(Parameter::positional_optional_pt(
                    name_str.as_str(),
                    &type_eval,
                    value,
                ).set_mods(mods));
            } else {
                return self.raise("TypeError", "Expected a map for named argument");
            }
        }        

        let body_formatted: Vec<Statement> = body.iter().map(|v| v.convert_to_statement()).collect();

        let lambda_name;  // stupid lifetimes
        if name == "<lambda#{id}>" {
            let id: usize = self.internal_storage.lambda_counter;
            self.internal_storage.lambda_counter = id + 1;
            lambda_name = name.replace("{id}", &id.to_string());
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
            effects: effect_flags,
        };

        if self.variables.contains_key(name) && !name.starts_with("<") {
            if let Some(var) = self.variables.get(name) {
                if var.is_final() {
                    return self.raise("AssigmentError", &format!("Cannot redefine final function '{}'", name));
                }
            }
        }

        debug_log(
            &format!("<Defining function '{}'>", name),
            &self.config,
            Some(self.use_colors.clone())
        );

        let function = if name.starts_with("<") && name.ends_with(">") {
            Value::Function(Function::Lambda(Arc::new(UserFunction {
                meta: metadata.clone(),
                body: body_formatted,
            }), self.variables.clone()))
        } else {
            create_function(
                metadata.clone(),
                body_formatted,
            )
        };

        self.variables.insert(
            name.to_string(),
            Variable::new(
                name.to_string(),
                function,
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
        let mut prev_err = None;

        let error_type_val = match statement.get(&Value::String("from".to_string())) {
            Some(v) => self.evaluate(&Value::convert_to_statement(v)),
            None => return self.raise("RuntimeError", "Missing 'from' in throw statement"),
        };

        if self.err.is_some() {
            prev_err = self.err.clone();
            self.err = None;
        }
        
        let error_msg_val = match statement.get(&Value::String("message".to_string())) {
            Some(v) => self.evaluate(&Value::convert_to_statement(v)),
            None => return self.raise("RuntimeError", "Missing 'message' in throw statement"),
        };

        if self.err.is_some() {
            if prev_err.is_some() {
                self.raise_with_ref(
                    &self.err.clone().unwrap().error_type,
                    &self.err.clone().unwrap().msg,
                    prev_err.clone().unwrap(),
                );
                prev_err = self.err.clone();
                return self.raise_with_ref(
                    to_static(error_type_val.to_string()),
                    to_static(error_msg_val.to_string()),
                    prev_err.clone().unwrap(),
                );
            } else {
                return self.raise_with_ref(
                    to_static(error_type_val.to_string()),
                    to_static(error_msg_val.to_string()),
                    self.err.clone().unwrap(),
                );
            }
        }
        
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
                        debug_log(
                            &format!("<Variable '{}' forgotten>", name),
                            &self.config,
                            Some(self.use_colors.clone())
                        );

                        let dropped_value = value.get_value().clone();
                        drop(value);
                        dropped_value
                    },
                    None => self.raise("NameError", &format!("Variable '{}' not found for forget", name)),
                }
            }

            "INDEX_ACCESS" => {
                let object_val = match value_map.get(&Value::String("object".to_string())) {
                    Some(v) => self.evaluate(&Value::convert_to_statement(v)),
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
                    Some(v) => self.evaluate(&v.convert_to_statement()),
                    None => NULL,
                };
                let end_val = match end_val_opt {
                    Some(v) => self.evaluate(&v.convert_to_statement()),
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

                Value::Tuple(results)
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
                self.raise("RuntimeError", "Invalid 'type_kind' for type statement");
                return NULL;
            }
        };
    
        let type_: Type = match kind_str {
            "simple" => {
                let type_name = statement.get(&Value::String("value".to_string())).unwrap_or(&default_type);
    
                if let Value::String(s) = type_name {
                    let mut is_ref = false;
                    let mut is_maybe = false;
                    let mut type_str = s.clone();
                    while type_str.starts_with('&') || type_str.starts_with('?') {
                        if type_str.starts_with('&') {
                            is_ref = true;
                            type_str = type_str[1..].to_string();
                        } else if type_str.starts_with('?') {
                            is_maybe = true;
                            type_str = type_str[1..].to_string();
                        }
                    }
                    match self.variables.get(&type_str) {
                        Some(val) => match &val.value {
                            Value::Type(t) => t.clone().set_reference(is_ref).set_maybe_type(is_maybe).unmut(),
                            _ => {
                                self.raise_with_help("TypeError", &format!("'{}' is a variable name, not a type", s), "If you meant to assign a value, use ':=' instead of ':'");
                                return NULL;
                            }
                        },
                        None => {
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

                let mut types: Vec<Type> = Vec::new();

                for t in types_list {
                    let ty = self.handle_type(t.convert_to_hashmap_value());
                    if self.err.is_some() {
                        return NULL;
                    }
                    if let Value::Type(t) = ty {
                        types.push(t);
                    } else {
                        self.raise("TypeError", "Union type must be a valid type");
                        return NULL;
                    }
                }

                Type::Union(types)
            }
    
            "function" => {
                let elements_val = match statement.get(&Value::String("elements".to_string())) {
                    Some(e) => match e {
                        Value::List(l) => l,
                        _ => {
                            self.raise("RuntimeError", "'elements' in function type must be a list");
                            return NULL;
                        }
                    },
                    None => {
                        self.raise("RuntimeError", "Missing 'elements' in function type statement");
                        return NULL;
                    }
                };

                let mut elements: Vec<Type> = Vec::new();
                for el in elements_val {
                    let el_map = el.convert_to_hashmap_value();
                    if self.err.is_some() {
                        return NULL;
                    }
                    match self.handle_type(el_map) {
                        Value::Type(t) => elements.push(t),
                        _ => {
                            self.raise("TypeError", "Function parameter types must be valid types");
                            return NULL;
                        }
                    }
                };

                let return_type_val = match statement.get(&Value::String("return_type".to_string())) {
                    Some(rt) => rt,
                    None => {
                        self.raise("RuntimeError", "Missing 'return_type' in function type statement");
                        return NULL;
                    }
                };

                let return_type: Type = if let Value::Type(t) = self.handle_type(return_type_val.convert_to_hashmap_value()) {
                    t
                } else {
                    self.raise("TypeError", &format!("Invalid function return type '{}'", return_type_val));
                    return NULL;
                };

                Type::Function {
                    parameter_types: elements,
                    return_type: Box::new(return_type),
                }
            }

            "generator" => {
                let elements_val = match statement.get(&Value::String("elements".to_string())) {
                    Some(e) => match e {
                        Value::List(l) => l,
                        _ => {
                            self.raise("RuntimeError", "'elements' in generator type must be a list");
                            return NULL;
                        }
                    },
                    None => {
                        self.raise("RuntimeError", "Missing 'elements' in generator type statement");
                        return NULL;
                    }
                };

                let mut elements: Vec<Type> = Vec::new();
                for el in elements_val {
                    let el_map = el.convert_to_hashmap_value();
                    if self.err.is_some() {
                        return NULL;
                    }
                    match self.handle_type(el_map) {
                        Value::Type(t) => elements.push(t),
                        _ => {
                            self.raise("TypeError", "Function parameter types must be valid types");
                            return NULL;
                        }
                    }
                };

                let return_type_val = match statement.get(&Value::String("return_type".to_string())) {
                    Some(rt) => rt,
                    None => {
                        self.raise("RuntimeError", "Missing 'return_type' in function type statement");
                        return NULL;
                    }
                };

                let return_type: Type = if let Value::Type(t) = self.handle_type(return_type_val.convert_to_hashmap_value()) {
                    t
                } else {
                    self.raise("TypeError", &format!("Invalid generator return type '{}'", return_type_val));
                    return NULL;
                };

                Type::Generator {
                    parameter_types: elements,
                    yield_type: Box::new(return_type),
                }
            }
    
            "generics" => {
                let base_val = match statement.get(&Value::String("base".to_string())) {
                    Some(b) => b,
                    None => {
                        self.raise("RuntimeError", "Missing 'base' in generics type statement");
                        return NULL;
                    }
                };
                let elements_val = match statement.get(&Value::String("generics".to_string())) {
                    Some(e) => match e {
                        Value::List(l) => l,
                        _ => {
                            self.raise("RuntimeError", "'generics' in generics type must be a list");
                            return NULL;
                        }
                    },
                    None => {
                        self.raise("RuntimeError", "Missing 'generics' in generics type statement");
                        return NULL;
                    }
                };

                let base = match base_val {
                    Value::String(s) => {
                        let mut s = s.as_str();
                        let mut is_ref = false;
                        let mut is_maybe = false;
                        while s.starts_with('&') || s.starts_with('?') {
                            if s.starts_with('&') {
                                is_ref = true;
                                s = &s[1..];
                            } else if s.starts_with('?') {
                                is_maybe = true;
                                s = &s[1..];
                            }
                        }
                        match self.variables.get(s) {
                            Some(val) => match &val.value {
                                Value::Type(t) => t.clone().set_reference(is_ref).set_maybe_type(is_maybe).unmut(),
                                _ => {
                                    self.raise_with_help("TypeError", &format!("'{}' is a variable name, not a type", s), "Indexing is not valid in this context. Try using it in a different expression.");
                                    return NULL;
                                }
                            },
                            None => {
                                self.raise("TypeError", &format!("Invalid base type '{}'", s));
                                return NULL;
                            }
                        }
                    }
                    _ => {
                        self.raise("TypeError", "Base type for indexed type must be a string");
                        return NULL;
                    }
                };

                let mut elements = Vec::new();
                let mut elements_raw = Vec::new();
                for el in elements_val {
                    let el_map = el.convert_to_hashmap_value();
                    if self.err.is_some() {
                        return NULL;
                    }
                    match self.handle_type(el_map) {
                        Value::Type(t) => { elements_raw.push(el.convert_to_statement()); elements.push(t) },
                        _ => {
                            self.raise("TypeError", "Indexed type elements must be valid types");
                            return NULL;
                        }
                    }
                }

                match base {
                    Type::Simple { ref name, .. } if name == "list" || name == "tuple" || name == "map" => {
                        Type::Indexed {
                            base_type: Box::new(base),
                            elements: elements,
                        }
                    }
                    Type::Struct { ref name, fields, methods, generics: s_generics, wheres } => {
                        let generics_map: HashMap<String, Statement> = s_generics.iter()
                            .cloned()
                            .zip(elements_raw.iter().cloned())
                            .collect();

                        let wheres_keys = wheres.iter().map(|(k, _)| k).collect::<Vec<_>>();

                        let missing_keys: Vec<String> = s_generics.iter()
                            .filter(|k| !generics_map.contains_key(*k) && !wheres_keys.contains(k))
                            .cloned()
                            .collect();

                        let num_extra_values = if elements_raw.len() > s_generics.len() {
                            elements_raw.len() - s_generics.len()
                        } else {
                            0
                        };

                        if !missing_keys.is_empty() {
                            if missing_keys.len() == 1 {
                                self.raise("ValueError", &format!("Missing generic type argument '{}'", missing_keys[0]));
                            } else {
                                self.raise("ValueError", &format!("Missing generic type arguments: {}", missing_keys.join(", ")));
                            }
                            return NULL;
                        }
                        if num_extra_values > 0 {
                            return self.raise("ValueError", &format!("Expected {} generic type arguments, but got {}, {} extra provided", s_generics.len(), elements_raw.len(), num_extra_values));
                        }

                        let mut generics_evaluated: HashMap<String, Type> = HashMap::new();

                        for (name, stmt) in generics_map.iter() {
                            let ty = self.evaluate(&stmt.clone());
                            if self.err.is_some() {
                                return NULL;
                            }
                            match ty {
                                Value::Type(t) => {
                                    generics_evaluated.insert(name.clone(), t);
                                }
                                _ => {
                                    self.raise("TypeError", &format!("Generic type argument '{}' must be a valid type", name));
                                    return NULL;
                                }
                            }
                        }
                        for (name, value) in wheres.iter() {
                            if !generics_evaluated.contains_key(name) {
                                match value {
                                    Value::Type(t) => {
                                        generics_evaluated.insert(name.clone(), t.clone());
                                    }
                                    _ => {
                                        self.raise("TypeError", &format!("Where constraint '{}' must be a valid type", name));
                                        return NULL;
                                    }
                                }
                            }
                        }
                        let mut built_fields = Vec::new();
                        let mut interp = Interpreter::new(
                            self.config.clone(),
                            self.use_colors,
                            &self.file_path.clone(),
                            &self.cwd.clone(),
                            self.preprocessor_info.clone(),
                            &[]
                        );
                        interp.variables.extend(generics_evaluated.clone().into_iter().map(|(k, v)| {
                            (k.clone(), Variable::new(k, Value::Type(v), "type".to_string(), false, true, true))
                        }));
                        for (field_name, field_type, field_mods) in fields.iter() {
                            let v = match interp.evaluate(&field_type.clone()) {
                                Value::Type(t) => Value::Type(t),
                                _ => {
                                    self.raise("TypeError", &format!("Field '{}' in struct '{}' has invalid type", field_name, name));
                                    return NULL;
                                }
                            };
                            let new_field_type = Statement::Statement {
                                keys: vec![Value::String("type".to_string()), Value::String("value".to_string())],
                                values: vec![Value::String("VALUE".to_string()), v],
                                loc: self.get_location_from_current_statement(),
                            };
                            built_fields.push((field_name.clone(), new_field_type, field_mods.clone()));
                        }
                        drop(interp);
                        Type::Struct {
                            name: name.clone(),
                            fields: built_fields,
                            methods: methods.clone(),
                            generics: s_generics.clone(),
                            wheres: wheres.clone(),
                        }
                    }
                    Type::Enum { ref name, variants, generics: s_generics, wheres } => {
                        let generics_map: HashMap<String, Statement> = s_generics.iter()
                            .cloned()
                            .zip(elements_raw.iter().cloned())
                            .collect();

                        let wheres_keys = wheres.iter().map(|(k, _)| k).collect::<Vec<_>>();

                        let missing_keys: Vec<String> = s_generics.iter()
                            .filter(|k| !generics_map.contains_key(*k) && !wheres_keys.contains(k))
                            .cloned()
                            .collect();

                        let num_extra_values = if elements_raw.len() > s_generics.len() {
                            elements_raw.len() - s_generics.len()
                        } else {
                            0
                        };

                        if !missing_keys.is_empty() {
                            if missing_keys.len() == 1 {
                                self.raise("ValueError", &format!("Missing generic type argument '{}'", missing_keys[0]));
                            } else {
                                self.raise("ValueError", &format!("Missing generic type arguments: {}", missing_keys.join(", ")));
                            }
                            return NULL;
                        }
                        if num_extra_values > 0 {
                            return self.raise("ValueError", &format!("Expected {} generic type arguments, but got {}, {} extra provided", s_generics.len(), elements_raw.len(), num_extra_values));
                        }

                        let mut generics_evaluated: HashMap<String, Type> = HashMap::new();

                        for (name, stmt) in generics_map.iter() {
                            let ty = self.evaluate(&stmt.clone());
                            if self.err.is_some() {
                                return NULL;
                            }
                            match ty {
                                Value::Type(t) => {
                                    generics_evaluated.insert(name.clone(), t);
                                }
                                _ => {
                                    self.raise("TypeError", &format!("Generic type argument '{}' must be a valid type", name));
                                    return NULL;
                                }
                            }
                        }
                        for (name, value) in wheres.iter() {
                            if !generics_evaluated.contains_key(name) {
                                match value {
                                    Value::Type(t) => {
                                        generics_evaluated.insert(name.clone(), t.clone());
                                    }
                                    _ => {
                                        self.raise("TypeError", &format!("Where constraint '{}' must be a valid type", name));
                                        return NULL;
                                    }
                                }
                            }
                        }
                        let mut built_fields = Vec::new();
                        let mut interp = Interpreter::new(
                            self.config.clone(),
                            self.use_colors,
                            &self.file_path.clone(),
                            &self.cwd.clone(),
                            self.preprocessor_info.clone(),
                            &[]
                        );
                        interp.variables.extend(generics_evaluated.clone().into_iter().map(|(k, v)| {
                            (k.clone(), Variable::new(k, Value::Type(v), "type".to_string(), false, true, true))
                        }));
                        for (variant_name, variant_type, variant_disc) in variants.iter() {
                            if *variant_type == Statement::Null {
                                built_fields.push((variant_name.clone(), Statement::Null, variant_disc.clone()));
                                continue;
                            }
                            let v = match interp.evaluate(&variant_type.clone()) {
                                Value::Type(t) => Value::Type(t),
                                _ => {
                                    if interp.err.is_some() {
                                        self.err = interp.err;
                                        return NULL;
                                    }
                                    self.raise("TypeError", &format!("Variant '{}' in enum '{}' has invalid type", variant_name, name));
                                    return NULL;
                                }
                            };
                            let new_field_type = Statement::Statement {
                                keys: vec![Value::String("type".to_string()), Value::String("value".to_string())],
                                values: vec![Value::String("VALUE".to_string()), v],
                                loc: self.get_location_from_current_statement(),
                            };
                            built_fields.push((variant_name.clone(), new_field_type, variant_disc.clone()));
                        }
                        drop(interp);
                        Type::Enum {
                            name: name.clone(),
                            variants: built_fields,
                            generics: s_generics.clone(),
                            wheres: wheres.clone(),
                        }
                    }
                    _ => {
                        return self.raise("TypeError", &format!("Invalid generic type '{}'", base.display_simple()));
                    }
                }
            }

            "impl" => {
                let impls_ast = match statement.get(&Value::String("impls".to_string())) {
                    Some(Value::List(l)) => l,
                    _ => {
                        self.raise("RuntimeError", "Missing or invalid 'impls' in impl type statement");
                        return NULL;
                    }
                };
                let joins_ast = match statement.get(&Value::String("joins".to_string())) {
                    Some(Value::List(l)) => l,
                    _ => &vec![],
                };
                let mut impls: Vec<(String, Box<Type>, Vec<String>)> = Vec::new();
                for impl_ast in impls_ast {
                    let impl_map = match impl_ast {
                        Value::Map { keys, values } => keys.iter().cloned().zip(values.iter().cloned()).collect::<HashMap<_, _>>(),
                        _ => {
                            self.raise("RuntimeError", "Each impl must be a map");
                            return NULL;
                        }
                    };
                    let name = match impl_map.get(&Value::String("name".to_string())) {
                        Some(Value::String(n)) => n.clone(),
                        _ => {
                            self.raise("RuntimeError", "Missing or invalid 'name' in impl");
                            return NULL;
                        }
                    };
                    let args = match impl_map.get(&Value::String("args".to_string())) {
                        Some(Value::List(l)) => l,
                        _ => {
                            self.raise("RuntimeError", "Missing 'type' in impl");
                            return NULL;
                        }
                    };
                    let mods = match impl_map.get(&Value::String("modifiers".to_string())) {
                        Some(Value::List(l)) => l.iter().filter_map(|v| {
                            if let Value::String(s) = v {
                                Some(s.clone())
                            } else { None }
                        }).collect(),
                        _ => vec![],
                    };
                    let vars_clone = self.variables.clone();
                    self.variables.insert("Self".to_string(), Variable::new("Self".to_string(), Value::Type(Type::new_simple("any")), "type".to_string(), false, true, true));
                    
                    let return_type = match impl_map.get(&Value::String("return_type".to_string())) {
                        Some(rt) => match self.evaluate(&rt.convert_to_statement()) {
                            Value::Type(t) => t,
                            _ => {
                                self.variables = vars_clone;
                                self.raise("TypeError", "Impl return type must be a valid type");
                                return NULL;
                            }
                        },
                        None => {
                            self.variables = vars_clone;
                            self.raise("RuntimeError", "Missing 'return_type' in impl");
                            return NULL;
                        }
                    };

                    if self.err.is_some() {
                        self.variables = vars_clone;
                        return NULL;
                    }

                    let mut elements: Vec<Type> = Vec::new();
                    for arg in args {
                        let arg_map = arg.convert_to_hashmap_value();
                        if self.err.is_some() {
                            self.variables = vars_clone;
                            return NULL;
                        }
                        
                        match self.handle_type(arg_map) {
                            Value::Type(t) => elements.push(t),
                            _ => {
                                self.raise("TypeError", "Impl parameter types must be valid types");
                                self.variables = vars_clone;
                                return NULL;
                            }
                        }
                    }
                    self.variables = vars_clone;

                    impls.push((name, Box::new(Type::Function {
                        parameter_types: elements,
                        return_type: Box::new(return_type),
                    }), mods));
                }
                for join in joins_ast {
                    if let Value::String(s) = join {
                        if !self.variables.contains_key(s) {
                            self.raise("NameError", &format!("Unknown type '{}'", s));
                            return NULL;
                        }
                        if let Some(var) = self.variables.get(s) {
                            if let Value::Type(t) = &var.value {
                                if let Ok((_, inner)) = get_inner_type(t) {
                                    match inner {
                                        Type::Impl { implementations } => {
                                            for (name, func_type, mods) in implementations {
                                                impls.push((name.clone(), func_type.clone(), mods.clone()));
                                            }
                                        }
                                        _ => {
                                            self.raise("TypeError", &format!("Type '{}' is not an impl type", s));
                                            return NULL;
                                        }
                                    }
                                } else {
                                    self.raise("TypeError", &format!("Type '{}' is not an impl type", s));
                                    return NULL;
                                }
                            } else {
                                self.raise("TypeError", &format!("'{}' is a variable name, not a type", s));
                                return NULL;
                            }
                        }
                    } else {
                        self.raise("TypeError", "Join names must be strings");
                        return NULL;
                    }
                }
                Type::Impl {
                    implementations: impls,
                }
            }

            _ => {
                self.raise("RuntimeError", "Invalid 'type_kind' for type statement");
                return NULL;
            }
        };
        return Value::Type(type_);
    }

    fn handle_if(&mut self, statement: HashMap<Value, Value>) -> Value {
        let condition = match statement.get(&Value::String("condition".to_string())) {
            Some(v) => v,
            None => return self.raise("RuntimeError", "Missing 'condition' in if statement"),
        };

        let condition_value = self.evaluate(&condition.convert_to_statement());
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
            let mut result = NULL;
            for stmt in stmts {
                if !stmt.is_statement() {
                    continue;
                }
                result = self.evaluate(&stmt.convert_to_statement());
                if self.err.is_some() {
                    return NULL;
                }
            }
            return result;
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
            let value = self.evaluate(&item.convert_to_statement());
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
        let change_in_try = !self.internal_storage.in_try_block;
        if change_in_try {
            self.internal_storage.in_try_block = true;
        }
        for stmt in body {
            if !stmt.is_statement() {
                continue;
            }

            result = self.evaluate(&stmt.convert_to_statement());
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
                if change_in_try {
                    self.internal_storage.in_try_block = false;
                }
                for catch_stmt in catch_body {
                    if !catch_stmt.is_statement() {
                        continue;
                    }
                    err_result = self.evaluate(&catch_stmt.convert_to_statement());
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

        let right_value = self.evaluate(&right.convert_to_statement());

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
    
                let expected_type: Type = {
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
                                    right_value.get_type().display_simple(),
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
                    var.get_type()
                };

                if self.err.is_some() {
                    return NULL;
                }

                let (is_valid, err) = self.check_type(&right_value, &expected_type);
                if !is_valid {
                    if let Some(err) = err {
                        self.raise_with_ref(
                            "TypeError",
                            &format!(
                                "Invalid type for variable '{}': expected '{}', got '{}'",
                                name, expected_type.display(), right_value.type_name()
                            ),
                            err,
                        );
                    } else {
                        return self.raise(
                            "TypeError",
                            &format!(
                                "Invalid type for variable '{}': expected '{}', got '{}'",
                                name, expected_type.display(), right_value.type_name()
                            ),
                        );
                    }
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
                                    Ok(self.evaluate(&start.convert_to_statement()))
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
            "PROPERTY_ACCESS" => {
                let object_val = match left_hashmap.get(&Value::String("object".to_string())) {
                    Some(v) => self.evaluate(&v.convert_to_statement()),
                    None => return self.raise("RuntimeError", "Missing 'object' in property access assignment"),
                };
                if self.err.is_some() {
                    return NULL;
                }
                let property_name = match left_hashmap.get(&Value::String("property".to_string())) {
                    Some(Value::String(n)) => n,
                    _ => return self.raise("RuntimeError", "Missing or invalid 'property' in property access assignment"),
                };
                let var_name = match left_hashmap.get(&Value::String("object".to_string())) {
                    Some(v) => match v.convert_to_statement().get_value("name") {
                        Some(Value::String(n)) => n.clone(),
                        _ => return self.raise("ValueError", "Cannot determine variable name from object in property access assignment"),
                    },
                    None => return self.raise("RuntimeError", "Missing 'object' in property access assignment"),
                };
                let o = match object_val {
                    Value::Struct(mut s) => {
                        let is_final = s.get_field_mods(property_name)
                            .map_or(false, |(_, _, f)| f);

                        if is_final {
                            return self.raise(
                                "AssignmentError",
                                &format!("Cannot assign to final property '{}'", property_name),
                            );
                        }

                        if let Some((box_val, expected_type)) = s.fields.get_mut(property_name) {
                            let (is_valid, err) = self.check_type(&right_value, &expected_type);

                            if !is_valid {
                                if let Some(err) = err {
                                    self.raise_with_ref(
                                        "TypeError",
                                        &format!(
                                            "Invalid type for property '{}': expected '{}', got '{}'",
                                            property_name, expected_type.display(), right_value.type_name()
                                        ),
                                        err,
                                    );
                                } else {
                                    return self.raise(
                                        "TypeError",
                                        &format!(
                                            "Invalid type for property '{}': expected '{}', got '{}'",
                                            property_name, expected_type.display(), right_value.type_name()
                                        ),
                                    );
                                }
                            }

                            **box_val = right_value;
                            Value::Struct(s)
                        } else {
                            return self.raise(
                                "AttributeError",
                                &format!("Property '{}' not found in struct", property_name),
                            );
                        }
                    }
                    t => self.raise("TypeError", &format!("Property assignment not supported on type '{}'", t.get_type().display_simple())),
                };
                if self.err.is_some() {
                    return NULL;
                }
                if self.variables.contains_key(&var_name) {
                    let var = self.variables.get_mut(&var_name).unwrap();
                    var.set_value(o);
                }
                NULL
            }
            // fuck my parser
            "POINTER_DEREF" => {
                let left_value = left_hashmap
                    .get(&Value::String("value".to_string()))
                    .cloned()
                    .unwrap_or(NULL);

                let mut loc = Location {
                    file: self.file_path.clone(),
                    lucia_source_loc: "<unknown>".into(),
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
                    if let Some(Value::String(lucia_source_loc)) = loc_map.get(&Value::String("_lucia_source_loc".to_string())) {
                        loc.lucia_source_loc = lucia_source_loc.clone();
                    }
                }

                let result = self.evaluate(
                    &Statement::Statement {
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
            Some(v) => self.evaluate(&Value::convert_to_statement(v)),
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
            Value::Type(t) => if let Type::Enum { variants, ..} = t {
                variants.iter().max_by_key(|x| x.2).map(|x| x.2).unwrap_or(variants.len())
            }  else {
                return self.raise("TypeError", "Object not indexable");
            }
            _ => return self.raise("TypeError", "Object not indexable"),
        };
    
        let start_eval_opt = start_val_opt
            .map(|v| self.evaluate(&v.convert_to_statement()))
            .and_then(|val| if val == NULL { Some(Value::Int(0.into())) } else { Some(val) });
        let end_eval_opt = end_val_opt
            .map(|v| self.evaluate(&v.convert_to_statement()))
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
                        Value::Type(t) => {
                            if let Type::Enum { variants, name, .. } = t {
                                for (variant_name, ty, idx) in variants {
                                    if *idx == start_idx {
                                        if *ty != Statement::Null {
                                            match self.evaluate(&ty) {
                                                Value::Type(t) => self.raise_with_help("TypeError", "Discriminant index access works only for non-value variants", &format!("'{}.{}' accepts type '{}'", name, variant_name, t.display_simple())),
                                                _ => self.raise("TypeError", "Discriminant index access works only for non-value variants"),
                                            };
                                            let ref_err = self.err.clone().unwrap();
                                            return self.raise_with_ref("NotImplemented", "Discriminant index access for value variants is not supported yet", ref_err);
                                        }
                                        return Value::Enum(Enum::new(t.clone(), (*idx, Value::Null)));
                                    }
                                }
                                return self.raise("ValueError", &format!("No enum variant with value {}", start_idx));
                            } else {
                                return self.raise("TypeError", "Object not indexable");
                            }
                        }
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
    
        let value = self.evaluate(&value.convert_to_statement());
    
        let mut declared_type: Type = match statement.get(&Value::String("var_type".to_string())) {
            Some(t) => {
                match self.evaluate(&t.convert_to_statement()) {
                    Value::Type(t) => t,
                    Value::Null => return NULL,
                    t => return self.raise("TypeError", &format!("Expected a type for variable type, got {}", t.type_name())),
                }
            },
            _ => Type::new_simple("any"),
        };

        if declared_type == Type::new_simple("auto") {
            declared_type = value.get_type();
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
    
        let (is_valid, err) = self.check_type(&value, &declared_type);
        if !is_valid {
            if let Some(err) = err {
                return self.raise_with_ref("TypeError", &format!("Variable '{}' declared with type '{}', but got {} with invalid element types", name, declared_type.display_simple(), value.get_type().display_simple()), err);
            }
            return self.raise("TypeError", &format!("Variable '{}' declared with type '{}', but value is of type '{}'", name, declared_type.display_simple(), value.get_type().display_simple()));
        }
    
        let variable = Variable::new_pt(name.to_string(), value.clone(), declared_type, is_static, is_public, is_final);
        self.variables.insert(name.to_string(), variable);

        debug_log(
            &format!("<Declared variable '{}': {} = {}>", name, value.get_type().display_simple(), format_value(&value)),
            &self.config, Some(self.use_colors),
        );
    
        value   
    }

    fn handle_variable(&mut self, statement: HashMap<Value, Value>) -> Value {
        let name = match statement.get(&Value::String("name".to_string())) {
            Some(Value::String(s)) => s,
            _ => return self.raise("SyntaxError", "Expected a string for variable name"),
        };

        if self.err.is_some() {
            return NULL;
        }

        if let Some(var) = self.variables.get(name) {
            let val = var.get_value().clone();
            if let Value::Struct(s) = &val {
                let func = match find_struct_method(s, None, "op_get", &vec![s.get_type()], &Type::new_simple("any")) {
                    Ok(f) => f,
                    Err(_) => return val,
                };
                if func.is_static() {
                    return self.raise("TypeError", &format!("Cannot call static method 'op_get' on instance of struct '{}'", s.name()));
                }
                let mut object_variable = var.clone();

                let mut removed_method = None;

                if let Value::Struct(s) = &mut object_variable.get_value_mut() {
                    if let Type::Struct { methods, .. } = &mut s.ty {
                        if let Some(pos) = methods.iter().position(|m| m.0 == "op_get") {
                            removed_method = Some(methods.remove(pos));
                        }
                    }
                }

                let call_result = self.call_function(&func, vec![val.clone()], HashMap::new(), Some((None, Some(&mut object_variable))));
                if self.err.is_some() {
                    return NULL;
                }

                if let (Some(m), Value::Struct(s)) = (removed_method, &mut object_variable.get_value_mut()) {
                    if let Type::Struct { methods, .. } = &mut s.ty {
                        methods.push(m);
                    }
                }

                self.variables.insert(name.to_string(), object_variable);
                return call_result;

            }
            return val;
        } else {
            let mut candidates: Vec<(String, Type, Statement)> = vec![];
            let mut seen = std::collections::HashSet::new();

            // lazy my ass
            for (k, v) in &self.variables {
                if k == "_" { continue; }
                if let Value::Type(t) = &v.value {
                    if let Type::Enum { name: enum_name, variants, .. } = t {
                        for variant in variants {
                            if variant.0 == *name && variant.1 == Statement::Null {
                                if seen.insert((enum_name.clone(), variant.0.clone())) {
                                    candidates.push((variant.0.clone(), t.clone(), variant.1.clone()));
                                }
                            }
                        }
                    }
                }
            }
            if candidates.len() == 1 {
                if candidates[0].2 != Statement::Null {
                    let saved_variables = self.variables.clone();
                    let generics = match &candidates[0].1 {
                        Type::Enum { generics, .. } => generics.clone(),
                        _ => vec![],
                    };
                    self.variables.extend(generics.into_iter().map(|g| (g.clone(), Variable::new(g.clone(), Value::Type(Type::new_simple("any")), "type".to_string(), false, true, true))));
                    let eval_type = match self.evaluate(&candidates[0].2) {
                        Value::Type(t) => t,
                        e => {
                            self.variables = saved_variables;
                            if self.err.is_some() {
                                return NULL;
                            }
                            return self.raise("TypeError", &format!("Expected a type for '{}', but got: {}", candidates[0].0.clone(), e.to_string()))
                        },
                    };
                    self.variables = saved_variables;
                    if self.err.is_some() {
                        return NULL;
                    }
                    return self.raise_with_help(
                        "TypeError",
                        &format!("Missing argument for '{}.{}' of type '{}'", candidates[0].1.display_simple(), candidates[0].0.clone(), eval_type.display_simple()),
                        &format!("Did you mean to use '{}.{}({})'", candidates[0].1.display_simple(), candidates[0].0.clone(), eval_type.display()),
                    );
                }
                let idx = match get_enum_idx(&candidates[0].1, &candidates[0].0) {
                    Some(u) => u,
                    None => return self.raise("NameError", &format!("Enum variant '{}' is not defined.", candidates[0].0)),
                };
                return Value::Enum(Enum::new(candidates[0].1.clone(), (idx, Value::Null)));
            } else if candidates.len() > 1 {
                let variant_list = candidates.iter()
                    .map(|(variant, ty, _)| format!("'{}.{}'", ty.display_simple(), variant))
                    .collect::<Vec<_>>()
                    .join(", ");
                return self.raise_with_help("NameError", &format!("Variable '{}' is not defined.", name), &format!("However, it matches multiple enum variants: {}. Did you mean to use one of these?", variant_list));
            }
            let lib_dir = PathBuf::from(self.config.home_dir.clone()).join("libs").join(&name);
            let extensions = ["lc", "lucia", "rs", ""];
            for ext in extensions.iter() {
                let candidate = lib_dir.with_extension(ext);
                if candidate.exists() {
                    return self.raise_with_help(
                        "NameError",
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
    
    fn handle_for_loop(&mut self, statement: HashMap<Value, Value>) -> Value {
        static KEY_ITERABLE: once_cell::sync::Lazy<Value> = once_cell::sync::Lazy::new(|| Value::String("iterable".into()));
        static KEY_BODY: once_cell::sync::Lazy<Value> = once_cell::sync::Lazy::new(|| Value::String("body".into()));
        static KEY_VARIABLE: once_cell::sync::Lazy<Value> = once_cell::sync::Lazy::new(|| Value::String("variable".into()));

        let iterable = match statement.get(&*KEY_ITERABLE) {
            Some(v) => v,
            None => return self.raise("RuntimeError", "Missing 'iterable' in for loop statement"),
        };

        let iterable_value = self.evaluate(&iterable.convert_to_statement());
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

            if variable_names.len() == 1 {
                self.variables.insert(
                    variable_names[0].clone(),
                    Variable::new(variable_names[0].clone(), item.clone(), item.type_name(), false, true, true),
                );
            } else {
                let values_to_assign = match item {
                    Value::List(inner) => inner,
                    Value::Tuple(inner) => inner,
                    t => {
                        return self.raise("TypeError", &format!("Cannot destructure non-list/tuple ({}) value into multiple variables", t.get_type().display_simple()));
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
                result = self.evaluate(&stmt.convert_to_statement());

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
                        evaluated_elements.push(self.evaluate(&element.convert_to_statement()));
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
                        self.evaluate(&map_val.convert_to_statement())
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

                let evaluated_end = self.evaluate(&end_raw.convert_to_statement());

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

    fn get_properties(&mut self, object_value: &Value, auto_init: bool) -> Option<(String, HashMap<String, Variable>)> {
        match object_value {
            Value::Module(o) => {
                let props = o.get_properties();
                return Some((o.name().to_string(), props.clone()));
            }
            Value::Type(t) => {
                match t {
                    Type::Struct { name: s_name, methods, .. } => {
                        let props = {
                            let mut m: HashMap<String, Variable> = HashMap::new();
                            for (name, method) in methods {
                                if !method.is_static() {
                                    continue;
                                }
                                m.insert(name.clone(), Variable::new(
                                    name.clone(),
                                    Value::Function(method.clone()),
                                    "function".to_string(),
                                    false,
                                    true,
                                    true,
                                ));
                            }
                            m
                        };
                        return Some((s_name.to_string(), props));
                    }
                    _ => { return None; }
                }
            }
            Value::Struct(s) => {
                let props = if let Type::Struct { ref methods, .. } = s.ty {
                    let mut m: HashMap<String, Variable> = HashMap::new();
                    for (name, method) in methods {
                        if method.is_static() {
                            continue;
                        }
                        m.insert(name.clone(), Variable::new(
                            name.clone(),
                            Value::Function(method.clone()),
                            "function".to_string(),
                            false,
                            true,
                            true,
                        ));
                    }
                    m
                } else {
                    self.raise("TypeError", &format!("'{}' is not a struct type", s.ty.display_simple()));
                    return None;
                };
                return Some((s.clone().name().to_string(), props));
            }
            _ => {
                if !auto_init {
                    return None;
                }
                let mut tmp_var = Variable::new(
                    "_".to_string(),
                    object_value.clone(),
                    object_value.type_name(),
                    false,
                    true,
                    true,
                );
                tmp_var.init_properties(self);
                return Some((object_value.get_type().display_simple(), tmp_var.properties));
            }
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
        let object_value = self.evaluate(&object.convert_to_statement());
        let object_type = object_value.type_name();

        let mut object_variable = Variable::new(
            "_".to_string(),
            object_value.clone(),
            object_type.clone(),
            false,
            true,
            true,
        );
        let mut variable_name: Option<String> = if object.convert_to_statement().get_type() == "VARIABLE" {
            object.convert_to_statement().get_value("name").and_then(|v| v.to_string().into())
        } else {
            None
        };

        match object_value {
            Value::Type(ref t) => {
                match t {
                    Type::Enum { name, variants, generics, .. } => {
                        let variant_name = method_name;
                        let variant = match variants.iter().find(|(s, _, _)| s == variant_name) {
                            Some(variant) => variant,
                            None => return self.raise_with_help("TypeError", &format!("Variant '{}' not found in enum '{}'", variant_name, name), &format!("Available variants are: {}", { let v: Vec<_> = variants.iter().map(|(n, _, _)| n.clone()).collect(); if v.len() > 7 { format!("{}...", v[..5].join(", ")) } else { v.join(", ") }})),
                        };
                        if self.err.is_some() {
                            return NULL;
                        }
                        if !named_args.is_empty() {
                            return self.raise("SyntaxError", &format!("Unexpected named arguments in enum variant '{}.{}'", name, variant_name));
                        }
                        let (variant_name, variant_ty, _) = variant;
                        if *variant_ty == Statement::Null {
                            if pos_args.is_empty() {
                                return self.raise_with_help(
                                    "TypeError",
                                    &format!("Unexpected '()' for '{}.{}'", name, variant_name),
                                    &format!("Remove '()' from '{}.{}()'", name, variant_name),
                                );
                            } else {
                                return self.raise_with_help(
                                    "TypeError",
                                    &format!("Variant '{}.{}' doesn't accept any arguments", name, variant_name),
                                    &format!("Did you mean to use '{}.{}'", name, variant_name),
                                );
                            }
                        }
                        if self.err.is_some() {
                            return NULL;
                        }
                        let saved_variables = self.variables.clone();
                        self.variables.extend(generics.into_iter().map(|g| (g.clone(), Variable::new(g.clone(), Value::Type(Type::new_simple("any")), "type".to_string(), false, true, true))));
                        let eval_type = match self.evaluate(&variant_ty) {
                            Value::Type(t) => t,
                            e => {
                                self.variables = saved_variables;
                                if self.err.is_some() {
                                    return NULL;
                                }
                                return self.raise("TypeError", &format!("Expected a type for '{}', but got: {}", variant_name, e.to_string()))
                            },
                        };
                        self.variables = saved_variables;
                        if self.err.is_some() {
                            return NULL;
                        }
                        let variant_val = if pos_args.is_empty() {
                            NULL
                        } else if pos_args.len() == 1 {
                            pos_args[0].clone()
                        } else {
                            Value::Tuple(pos_args)
                        };
                        if self.check_type(&NULL, &eval_type).0 {
                            return self.raise_with_help(
                                "TypeError",
                                &format!("Expected a value of type '{}' for '{}.{}', but got none", eval_type.display_simple(), name, variant_name),
                                &format!("Did you mean to use '{}.{}({})'", name, variant_name, eval_type.display()),
                            );
                        }
                        if self.err.is_some() {
                            return NULL;
                        }
                        let idx = match get_enum_idx(&t, &variant_name) {
                            Some(u) => u,
                            None => return self.raise("NameError", &format!("Enum variant '{}' is not defined.", variant_name)),
                        };
                        let v = Enum::new(
                            t.clone(),
                            (idx, variant_val)
                        );
                        return Value::Enum(v);
                    }
                    _ => {}
                }
            }
            _ => {}
        }

        if let Some((var_name, props)) = self.get_properties(&object_value, false) {
            object_variable.properties = props;
            object_variable.set_name(var_name.clone());
        } else if !object_variable.is_init() {
            object_variable.init_properties(self);
            variable_name = None;
            object_variable.set_name(object_value.get_type().display_simple());
        }

        if self.err.is_some() {
            return NULL;
        }

        let func = match object_variable.properties.get(&method_name.to_string()) {
            Some(v) => match v.get_value() {
                Value::Function(f) => f.clone(),
                other => return self.raise_with_help(
                    "TypeError",
                    &format!("'{}' is not callable", method_name),
                    &format!("Expected a function, but got: {}", other.to_string()),
                ),
            },
            None => {
                if let Value::Struct(s) =  object_variable.get_value() {
                    match s.get_field(&method_name) {
                        Some(Value::Function(f)) => f.clone(),
                        Some(field_val) => {
                            if !pos_args.is_empty() || !named_args.is_empty() {
                                return self.raise_with_help(
                                    "TypeError",
                                    &format!("Field '{}' is not callable", method_name),
                                    &format!("Did you mean to access the field without '()'?"),
                                );
                            }
                            return field_val.clone();
                        }
                        None => {            
                            let available_names: Vec<String> = object_variable.properties.keys().cloned().collect();
                            if let Some(closest) = find_closest_match(method_name, &available_names) {
                                return self.raise_with_help(
                                    "NameError",
                                    &format!("No method '{}' in '{}'", method_name, object_variable.get_name()),
                                    &format!("Did you mean '{}{}{}'?",
                                        check_ansi("\x1b[4m", &self.use_colors),
                                        closest,
                                        check_ansi("\x1b[24m", &self.use_colors),
                                    ),
                                );
                            } else {
                                return self.raise("NameError", &format!("No method '{}' in '{}'", method_name, object_variable.get_name()));
                            }
                        }
                    }
                } else {
                    let available_names: Vec<String> = object_variable.properties.keys().cloned().collect();
                    if let Some(closest) = find_closest_match(method_name, &available_names) {
                        return self.raise_with_help(
                            "NameError",
                            &format!("No method '{}' in '{}'", method_name, object_variable.get_name()),
                            &format!("Did you mean '{}{}{}'?",
                                check_ansi("\x1b[4m", &self.use_colors),
                                closest,
                                check_ansi("\x1b[24m", &self.use_colors),
                            ),
                        );
                    } else {
                        return self.raise("NameError", &format!("No method '{}' in '{}'", method_name, object_variable.get_name()));
                    }
                }
            }
        };

        let result = self.call_function(
            &func,
            pos_args,
            named_args,
            Some((None, Some(&mut object_variable))),
        );
        if let Some(name) = variable_name {
            self.variables.insert(name, object_variable);
        }
        return result;
    }

    fn handle_property_access(&mut self, statement: HashMap<Value, Value>) -> Value {
        let Some(object) = statement.get(&Value::String("object".to_string())).cloned() else {
            return Value::Error("RuntimeError", "Missing 'object' in property access", None);
        };
    
        let Some(Value::String(property)) = statement.get(&Value::String("property".to_string())).cloned() else {
            return Value::Error("RuntimeError", "Missing or invalid 'property' in property access", None);
        };
    
        let property_name = property.as_str();
        let object_value = self.evaluate(&object.convert_to_statement());
        let object_type = object_value.type_name();

        let mut object_variable = Variable::new(
            "_".to_string(),
            object_value.clone(),
            object_type.clone(),
            false,
            true,
            true,
        );

        match object_value {
            Value::Module(ref o) => {
                let props = o.get_properties();
                object_variable.properties = props.clone();
                object_variable.set_name(o.name().to_string());
            }
            Value::Type(t) => {
                match t {
                    Type::Enum { ref name, ref variants, .. } => {
                        let variant_name = property_name;
                        let variant = match variants.iter().find(|(s, _, _)| s == variant_name) {
                            Some(variant) => variant,
                            None => return self.raise_with_help("TypeError", &format!("Variant '{}' not found in enum '{}'", variant_name, name), &format!("Available variants are: {}", { let v: Vec<_> = variants.iter().map(|(n, _, _)| n.clone()).collect(); if v.len() > 7 { format!("{}...", v[..5].join(", ")) } else { v.join(", ") }})),
                        };
                        if self.err.is_some() {
                            return NULL;
                        }
                        let (variant_name, variant_ty, _) = variant;
                        if *variant_ty != Statement::Null {
                            let saved_variables = self.variables.clone();
                            let generics = match &t {
                                Type::Enum { generics, .. } => generics.clone(),
                                _ => vec![],
                            };
                            self.variables.extend(generics.into_iter().map(|g| (g.clone(), Variable::new(g.clone(), Value::Type(Type::new_simple("any")), "type".to_string(), false, true, true))));
                            let eval_type = match self.evaluate(&variant_ty) {
                                Value::Type(t) => t,
                                e => {
                                    self.variables = saved_variables;
                                    if self.err.is_some() {
                                        return NULL;
                                    }
                                    return self.raise("TypeError", &format!("Expected a type for '{}', but got: {}", variant_name, e.to_string()))
                                },
                            };
                            self.variables = saved_variables;
                            if self.err.is_some() {
                                return NULL;
                            }
                            return self.raise_with_help(
                                "TypeError",
                                &format!("Missing argument for '{}.{}' of type '{}'", name, variant_name, eval_type.display_simple()),
                                &format!("Did you mean to use '{}.{}({})'", name, variant_name, eval_type.display()),
                            );
                        }
                        let idx = match get_enum_idx(&t, &variant_name) {
                            Some(u) => u,
                            None => return self.raise("NameError", &format!("Enum variant '{}' is not defined.", variant_name)),
                        };
                        return Value::Enum(Enum::new(
                            t.clone(),
                            (idx, NULL)
                        ));
                    }
                    Type::Struct { .. } => {}
                    _ => {}
                }
            }
            Value::Struct(s) => {
                object_variable.set_name(s.name().to_string());
                object_variable.set_type(s.ty.clone());
                object_variable.properties = s.get_properties();
            }
            _ if !object_variable.is_init() => {
                object_variable.init_properties(self);
                object_variable.set_name(object_value.get_type().display_simple());
            }
            _ => {}
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
                        &format!("No property '{}' in '{}'", property_name, object_variable.get_name()),
                        &format!("Did you mean '{}{}{}'?",
                            check_ansi("\x1b[4m", &self.use_colors),
                            closest,
                            check_ansi("\x1b[24m", &self.use_colors),
                        ),
                    );
                } else {
                    return self.raise("NameError", &format!("No property '{}' in '{}'", property_name, object_variable.get_name()));
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
                    .map(|stmt| self.evaluate(&stmt.convert_to_statement()))
                    .collect::<Vec<Value>>()
            }
            _ => return Err(self.raise("TypeError", "Expected a list for function arguments")),
        };
    
        let named_args = match named_args {
            Value::Map { keys, values } => {
                let mut map = HashMap::new();
                for (key, val_stmt) in keys.iter().zip(values.iter()) {
                    if let Value::String(key_str) = key {
                        let val = self.evaluate(&val_stmt.convert_to_statement());
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

        let (pos_args, named_args) = match self.translate_args(
            statement.get(&Value::String("pos_arguments".to_string())).cloned().unwrap_or(Value::List(vec![])),
            statement.get(&Value::String("named_arguments".to_string())).cloned().unwrap_or(Value::Map { keys: vec![], values: vec![] }),
        ) {
            Ok(args) => args,
            Err(err) => {
                return err;
            }
        };

        if self.err.is_some() {
            return NULL;
        }
        
        if self.err.is_some() {
            return NULL;
        }


        let special_functions = [
            "exit", "fetch", "exec", "eval", "warn", "as_method", "module", "00__set_cfg__", "00__set_dir__"
        ];

        let is_special_function = special_functions.contains(&function_name);
        let func = if is_special_function {
            Function::Custom(Arc::new(UserFunction {
                body: Vec::new(),
                meta: FunctionMetadata {
                    name: function_name.to_string(),
                    parameters: Vec::new(),
                    return_type: Type::new_simple("any"),
                    is_public: true,
                    is_static: false,
                    is_final: false,
                    is_native: true,
                    state: None,
                    effects: EffectFlags::empty(),
                },
            }))
        } else {
            match self.variables.get(function_name) {
                Some(v) => match v.get_value() {
                    Value::Function(f) => f.clone(),
                    other => return self.raise_with_help(
                        "TypeError",
                        &format!("'{}' is not callable", function_name),
                        &format!("Expected a function, but got: {}", other.to_string()),
                    ),
                },
                None => {
                    let mut candidates: Vec<(String, Type, Statement)> = vec![];
                    let mut seen = std::collections::HashSet::new();
                    let variant_name = function_name;

                    for (k, v) in &self.variables {
                        if k == "_" { continue; }
                        if let Value::Type(t) = &v.value {
                            if let Type::Enum { name: enum_name, variants, .. } = t {
                                for variant in variants {
                                    if variant.0 == *variant_name && variant.1 != Statement::Null {
                                        if seen.insert((enum_name.clone(), variant.0.clone())) {
                                            candidates.push((variant.0.clone(), t.clone(), variant.1.clone()));
                                        }
                                    }
                                }
                            }
                        }
                    }
                    if candidates.len() == 1 {
                        if !named_args.is_empty() {
                            self.raise("SyntaxError", &format!("Unexpected named arguments in enum variant '{}.{}'", candidates[0].1.display_simple(), variant_name));
                        }
                        if candidates[0].2 == Statement::Null {
                            if pos_args.is_empty() {
                                return self.raise_with_help(
                                    "TypeError",
                                    &format!("Unexpected '()' for '{}.{}'", candidates[0].1.display_simple(), variant_name),
                                    &format!("Remove '()' from '{}.{}()'", candidates[0].1.display_simple(), variant_name),
                                );
                            } else {
                                return self.raise_with_help(
                                    "TypeError",
                                    &format!("Variant '{}.{}' doesn't accept any arguments", candidates[0].1.display_simple(), variant_name),
                                    &format!("Did you mean to use '{}.{}'", candidates[0].1.display_simple(), variant_name),
                                );
                            }
                        }
                        let saved_variables = self.variables.clone();
                        let generics = match candidates[0].1 {
                            Type::Enum { ref generics, .. } => generics.clone(),
                            _ => vec![],
                        };
                        self.variables.extend(generics.iter().map(|k| {
                            (k.clone(), Variable::new(k.clone(), Value::Type(Type::new_simple("any")), "type".to_string(), false, true, true))
                        }));
                        let eval_type = match self.evaluate(&candidates[0].2) {
                            Value::Type(t) => t,
                            e => {
                                self.variables = saved_variables;
                                if self.err.is_some() {
                                    return NULL;
                                }
                                return self.raise("TypeError", &format!("Expected a type for '{}', but got: {}", candidates[0].0.clone(), e.to_string()))
                            },
                        };
                        self.variables = saved_variables;
                        if self.err.is_some() {
                            return NULL;
                        }
                        let variant_value = if pos_args.is_empty() {
                            NULL
                        } else if pos_args.len() == 1 {
                            pos_args[0].clone()
                        } else {
                            Value::Tuple(pos_args)
                        };
                        if self.check_type(&variant_value, &eval_type).0 {
                            let idx = match get_enum_idx(&candidates[0].1, &candidates[0].0) {
                                Some(u) => u,
                                None => return self.raise("NameError", &format!("Enum variant '{}' is not defined.", candidates[0].0)),
                            };
                            return Value::Enum(Enum::new(candidates[0].1.clone(), (idx, variant_value)));
                        } else {
                            return self.raise("TypeError", &format!("Expected '{}' for '{}.{}' value, but got: {}", eval_type.display_simple(), candidates[0].1.display_simple(), variant_name, variant_value.get_type().display_simple()));
                        }
                    } else if candidates.len() > 1 {
                        let variant_list = candidates.iter()
                            .map(|(variant, ty, _)| format!("'{}.{}'", ty.display_simple(), variant))
                            .collect::<Vec<_>>()
                            .join(", ");
                        return self.raise_with_help("NameError", &format!("Variable '{}' is not defined.", variant_name), &format!("However, it matches multiple enum variants: {}. Did you mean to use one of these?", variant_list));
                    }
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
            }
        };

        self.call_function(&func, pos_args, named_args, if is_special_function { Some((Some(function_name.to_string()), None)) } else { None })
    }

    pub fn call_function(
        &mut self,
        func: &Function,
        pos_args: Vec<Value>,
        named_args: HashMap<String, Value>,
        special: Option<(Option<String>, Option<&mut Variable>)>,
    ) -> Value {
        let is_special_function = if let Some((Some(_), _)) = &special {
            true
        } else {
            false
        };

        let metadata = if !is_special_function {
            func.metadata().clone()
        } else {
            special_function_meta().get(special.as_ref().unwrap().0.as_ref().unwrap().as_str()).unwrap().clone()
        };
        let function_name = metadata.name.as_str();
        let mut object_variable: Option<&mut Variable> = if let Some((_, obj_var)) = special {
            obj_var
        } else {
            None
        };
        let is_module: bool = if let Some(obj_var) = &object_variable {
            matches!(obj_var.get_value(), Value::Module(_))
        } else {
            false
        };

        if let Some(state) = metadata.state.as_ref() {
                if state == "deprecated" {
                    self.warn(&format!("Warning: Function '{}' is deprecated", function_name));
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
                } else if let Some(alt_info) = state.strip_prefix("deprecated: ") {
                    self.warn(&format!(
                        "Warning: Function '{}' is deprecated.",
                        function_name
                    ));
                    self.warn(&format!(
                        "{}",
                        alt_info
                    ));
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

        let mut effect_flags = metadata.effects;
        let mut effect_mask = EffectFlags::UNKNOWN;

        if effect_flags.contains(EffectFlags::MAY_FAIL) && self.internal_storage.in_try_block {
            effect_flags.remove(EffectFlags::MAY_FAIL);
            effect_mask.insert(EffectFlags::MAY_FAIL);
        }

        self.collected_effects |= effect_flags;

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
        let mut instance_variables: Vec<String> = vec![];
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
                        if self.check_type(&arg_value, param_type).0 {
                            final_args.insert(param_name.clone(), (arg_value, param_mods.clone()));
                        } else {
                            if let Type::Simple { .. } = param_type {
                                return self.raise_with_help(
                                    "TypeError",
                                    &format!("Argument '{}' does not match expected type '{}', got '{}'", param_name, param_type.display_simple(), arg_value.get_type().display_simple()),
                                    &format!(
                                        "Try using: '{}{}={}({}){}'",
                                        check_ansi("\x1b[4m", &self.use_colors),
                                        param_name,
                                        param_type.display_simple(),
                                        format_value(&positional[pos_index.saturating_sub(1)]).to_string(),
                                        check_ansi("\x1b[24m", &self.use_colors)
                                    ),
                                );
                            } else {
                                return self.raise(
                                    "TypeError",
                                    &format!("Argument '{}' does not match expected type for '{}'", param_name, param_type.display_simple()),
                                );
                            }
                        }
                        pos_index += 1;
                    } else if let Some(named_value) = named_map.remove(param_name) {
                        if self.check_type(&named_value, param_type).0 {
                            final_args.insert(param_name.clone(), (named_value, param_mods.clone()));
                        } else {
                            if let Type::Simple { .. } = param_type {
                                return self.raise_with_help(
                                    "TypeError",
                                    &format!("Argument '{}' does not match expected type '{}', got '{}'", param_name, param_type.display_simple(), named_value.get_type().display_simple()),
                                    &format!(
                                        "Try using: '{}{}={}({}){}'",
                                        check_ansi("\x1b[4m", &self.use_colors),
                                        param_name,
                                        param_type.display_simple(),
                                        format_value(&positional[pos_index - 1]).to_string(),
                                        check_ansi("\x1b[24m", &self.use_colors)
                                    ),
                                );
                            } else {
                                return self.raise(
                                    "TypeError",
                                    &format!("Argument '{}' does not match expected type for '{}'", param_name, param_type.display_simple()),
                                );
                            }
                        }
                    } else if let Some(default) = param_default {
                        final_args.insert(param_name.clone(), (default.clone(), param_mods.clone()));
                    } else {
                        return self.raise("TypeError", &format!("Missing required positional argument: '{}'", param_name));
                    }
                }
                ParameterKind::Variadic => {
                    let mut variadic_args = Vec::new();

                    for (i, arg) in positional[pos_index..].iter().enumerate() {
                        if let Value::Type(t) = arg {
                            if let Type::Unwrap(l) = t {
                                for val in l {
                                    let (is_valid, err) = self.check_type(val, param_type);
                                    if !is_valid {
                                        if let Some(err) = err {
                                            return self.raise_with_ref(
                                                "TypeError",
                                                &format!("Variadic argument #{} does not match expected type '{}'", i, param_type.display()),
                                                err,
                                            );
                                        }
                                        return self.raise(
                                            "TypeError",
                                            &format!("Variadic argument #{} does not match expected type '{}'", i, param_type.display()),
                                        );
                                    }
                                    variadic_args.push(val.clone());
                                }
                                continue;
                            }
                        }

                        let (is_valid, err) = self.check_type(arg, param_type);
                        if !is_valid {
                            if let Some(err) = err {
                                return self.raise_with_ref(
                                    "TypeError",
                                    &format!("Variadic argument #{} does not match expected type '{}'", i, param_type.display()),
                                    err,
                                );
                            }
                            return self.raise(
                                "TypeError",
                                &format!("Variadic argument #{} does not match expected type '{}'", i, param_type.display()),
                            );
                        }
                        variadic_args.push(arg.clone());
                    }

                    final_args.insert(param_name.clone(), (Value::List(variadic_args), param_mods.clone()));

                    pos_index = positional.len();
                    named_map.remove(param_name);
                }
                ParameterKind::KeywordVariadic => {
                    if let Some(named_value) = named_map.remove(param_name) {
                        if self.check_type(&named_value, &param_type).0 {
                            final_args.insert(param_name.clone(), (named_value, param_mods.clone()));
                        } else {
                            return self.raise(
                                "TypeError",
                                &format!(
                                    "Keyword argument '{}' does not match expected type '{}', got '{}'",
                                    param_name,
                                    param_type.display(),
                                    named_value.type_name()
                                ),
                            );
                        }
                    } else if let Some(default) = param_default {
                        final_args.insert(param_name.clone(), (default.clone(), param_mods.clone()));
                    }
                }
                ParameterKind::Instance => {
                    if let Some(ref object_variable) = object_variable {
                        let obj_val = object_variable.get_value().clone();
                        final_args.insert(param_name.clone(), (obj_val, param_mods.clone()));
                        instance_variables.push(param_name.clone());
                    } else {
                        return self.raise("TypeError", "Instance parameter without an object");
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
                    expect_one_of.push_str(&format!("{} ({})", param.name, param.ty.display_simple()));
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

        if let Some(ref obj_var) = object_variable {
            debug_log(
                &format!(
                    "<Call: {}.{}({})>",
                    obj_var.get_name(),
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
        } else {
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
        }

        self.stack.push((function_name.to_string(), self.get_location_from_current_statement(), StackType::FunctionCall));

        let change_in_function = !self.internal_storage.in_function;
        if change_in_function {
            self.internal_storage.in_function = true;
        }

        if change_in_function {
            effect_mask |= EffectFlags::ALL;
            if !self.config.allow_unsafe {
                if effect_flags.contains(EffectFlags::UNSAFE) {
                    return self.raise_with_help(
                        "RuntimeError",
                        &format!("Unsafe function '{}' called in safe mode.", function_name),
                        "Any function called in the global scope uses the configs state of 'allow_unsafe' to determine if unsafe functions are allowed.",
                    );
                }
            }
        }

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
            if change_in_function {
                self.internal_storage.in_function = false;
            }
            return NULL;
        }
        
        if is_special_function {      
            if change_in_function {
                self.internal_storage.in_function = false;
            }
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
                        let lexer = Lexer::new(to_static(script_str.clone()), to_static(self.file_path.clone()));
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
                        let lexer = Lexer::new(to_static(script_str.clone()), to_static(self.file_path.clone()));
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
                "warn" => {
                    self.stack.pop();
                    let msg = if let Some(msg) = named_args.get("message") {
                        msg.clone()
                    } else if !pos_args.is_empty() {
                        pos_args[0].clone()
                    } else {
                        Value::Error("ValueError", "Missing 'message' argument in warn", None)
                    };
                    if let Value::String(s) = msg {
                        self.warn(&s);
                        return NULL;
                    } else {
                        return self.raise("TypeError", "Expected a string in 'message' argument in warn");
                    }
                }
                "as_method" => {
                    self.stack.pop();
                    if let Some(func) = named_args.get("function") {
                        match func {
                            Value::Function(f) => {
                                if let Function::Custom(_) = f {
                                    return self.get_interpreter_as_method(f.clone());
                                } else {
                                    return self.raise("TypeError", "Expected a custom function in 'function' argument in as_method");
                                }
                            }
                            _ => {
                                return self.raise("TypeError", "Expected a function in 'function' argument in as_method");
                            }
                        }
                    } else if !pos_args.is_empty() {
                        let func = &pos_args[0];
                        match func {
                            Value::Function(f) => {
                                if let Function::Custom(_) = f {
                                    return self.get_interpreter_as_method(f.clone());
                                } else {
                                    return self.raise("TypeError", "Expected a custom function in 'function' argument in as_method");
                                }
                            }
                            _ => {
                                return self.raise("TypeError", "Expected a function in 'function' argument in as_method");
                            }
                        }
                    } else {
                        return self.raise("TypeError", "Missing 'function' argument in as_method");
                    }
                }
                "module" => {
                    self.stack.pop();
                    if let Some(Value::String(module_name)) = final_args_no_mods.get("path") {
                        let module_path = PathBuf::from(module_name);   
                        let properties = self.get_properties_from_file(&module_path);
                        let module = Value::Module(Module {
                            name: module_name.clone(),
                            properties,
                            parameters: Vec::new(),
                            is_public: false,
                            is_static: false,
                            is_final: false,
                            state: None,
                            path: PathBuf::from(module_path.clone())
                        });
                        return module;
                    } else {
                        return self.raise("TypeError", "Expected a string in 'path' argument in module");
                    }
                }
                "00__set_cfg__" => {
                    self.stack.pop();
                    if !self.config.allow_inline_config {
                        return self.raise("PermissionError", "Modifying configuration at runtime is disabled.");
                    }
                    if let Some(Value::String(key)) = final_args_no_mods.get("key") {
                        if let Some(value) = final_args_no_mods.get("value") {
                            if let Value::Int(num_wrapper) = value {
                                if let Ok(num) = num_wrapper.to_i64() {
                                    if num == 0x6969 {
                                        let default_val = get_from_config(&self.og_cfg.clone(), key);
                                        match set_in_config(&mut self.config, key, default_val.clone()) {
                                            Ok(_) => {}
                                            Err(err) => {
                                                return self.raise("KeyError", &err);
                                            }
                                        }
                                        debug_log(
                                            &format!("<Reset config: {} to default>", key),
                                            &self.config,
                                            Some(self.use_colors.clone()),
                                        );
                                        return NULL;
                                    } else if num == 0x6767 {
                                        let val = get_from_config(&self.config.clone(), key);
                                        debug_log(
                                            &format!("<Get config: {} = {}>", key, format_value(&val)),
                                            &self.config,
                                            Some(self.use_colors.clone()),
                                        );
                                        return val;
                                    }
                                }
                            }

                            match set_in_config(&mut self.config, key, value.clone()) {
                                Ok(_) => {}
                                Err(err) => {
                                    return self.raise("KeyError", &err);
                                }
                            }
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

        if self.err.is_some() {
            self.stack.pop();
            if change_in_function {
                self.internal_storage.in_function = false;
            }
            return NULL;
        }

        if let Function::Lambda(_, closure_vars) = func {
            let mut new_interpreter = Interpreter::new(
                self.config.clone(),
                self.use_colors.clone(),
                &self.file_path.clone(),
                &self.cwd.clone(),
                self.preprocessor_info.clone(),
                &vec![],
            );
            let mut merged_variables = self.variables.clone();
            merged_variables.extend(closure_vars.clone());
            merged_variables.extend(final_args_variables);
            new_interpreter.variables = merged_variables;
            new_interpreter.stack = self.stack.clone();
            new_interpreter.internal_storage.in_try_block = self.internal_storage.in_try_block;
            new_interpreter.internal_storage.in_function = true;
            let body = func.get_body();
            let _ = new_interpreter.interpret(body, true);
            result = new_interpreter.return_value.clone();
            if change_in_function {
                self.internal_storage.in_function = false;
            }
            if new_interpreter.err.is_some() {
                self.stack.pop();
                self.err = new_interpreter.err.clone();
                return NULL;
            }
            if let Value::Error(err_type, err_msg, referr) = &result {
                if let Some(rerr) = referr {
                    let err = Error::with_ref(
                        err_type,
                        err_msg,
                        rerr.clone(),
                        &self.file_path,
                    );
                    self.raise_with_ref(
                        "RuntimeError",
                        "Error in lambda call",
                        err,
                    );
                }
                let err = Error::new(
                    err_type,
                    err_msg,
                    &self.file_path,
                );
                self.raise_with_ref(
                    "RuntimeError",
                    "Error in lambda call",
                    err,
                );
                self.stack.pop();
                return NULL;
            };
            if !effect_flags.contains(EffectFlags::UNKNOWN) {
                match effect_flags.check_branches(new_interpreter.collected_effects, effect_mask) {
                    Some((false, extra_bits)) => {
                        let names = extra_bits.get_names();
                        return self.raise_with_help(
                            "EffectError",
                            &format!("Function '{}' has unexpected side effects: {}", function_name, names.join(", ")),
                            &format!("Consider adding '{}[{}, ...]{}' to the function's effect annotations.", hex_to_ansi(&self.config.color_scheme.note, self.use_colors), names.join(", "), hex_to_ansi(&self.config.color_scheme.help, self.use_colors)),
                        );
                    }
                    Some((true, missing_bits)) => {
                        let missing_bits = missing_bits & !(EffectFlags::STATE | EffectFlags::PURE);
                        if !missing_bits.is_empty() {
                            let names = missing_bits.get_names();
                            self.warn(
                                &format!("Warning: Function '{}' does not use some of the specified side effects: {}", function_name, names.join(", "))
                            );
                        }
                    }
                    None => {}
                }
            }
            for var in instance_variables {
                if let Some(v) = new_interpreter.variables.get_mut(&var) {
                    if let Some(ref mut object_variable) = object_variable {
                        object_variable.set_value(v.get_value().clone());
                    }
                }
            }
            self.stack = new_interpreter.stack;
        } else if let Function::CustomMethod(func) = func {
            let interpreter_arc = func.get_interpreter();
            let mut interpreter = interpreter_arc.lock().unwrap();
            interpreter.internal_storage.in_try_block = self.internal_storage.in_try_block;
            interpreter.internal_storage.in_function = true;

            result = interpreter.call_function(
                &Function::Custom(func.get_function()),
                positional,
                named_map,
                None,
            );
  
            if change_in_function {
                self.internal_storage.in_function = false;
            }

            for var in instance_variables {
                if let Some(v) = interpreter.variables.get_mut(&var) {
                    if let Some(ref mut object_variable) = object_variable {
                        object_variable.set_value(v.get_value().clone());
                    }
                }
            }

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
                        &self.file_path,
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
                    &self.file_path,
                );
                self.raise_with_ref(
                    "RuntimeError",
                    "Error in method call",
                    err,
                );
                self.stack.pop();
                return NULL;
            };

            if !effect_flags.contains(EffectFlags::UNKNOWN) {
                match effect_flags.check_branches(interpreter.collected_effects, effect_mask) {
                    Some((false, extra_bits)) => {
                        let names = extra_bits.get_names();
                        return self.raise_with_help(
                            "EffectError",
                            &format!("Function '{}' has unexpected side effects: {}", function_name, names.join(", ")),
                            &format!("Consider adding '{}[{}, ...]{}' to the function's effect annotations.", hex_to_ansi(&self.config.color_scheme.note, self.use_colors), names.join(", "), hex_to_ansi(&self.config.color_scheme.help, self.use_colors)),
                        );
                    }
                    Some((true, missing_bits)) => {
                        let missing_bits = missing_bits & !(EffectFlags::STATE | EffectFlags::PURE);
                        if !missing_bits.is_empty() {
                            let names = missing_bits.get_names();
                            self.warn(
                                &format!("Warning: Function '{}' does not use some of the specified side effects: {}", function_name, names.join(", "))
                            );
                        }
                    }
                    None => {}
                }
            }
        } else if !is_module {
            self.stack.push((function_name.to_string(), self.get_location_from_current_statement(), StackType::MethodCall));
            if !func.is_native() {
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
                new_interpreter.internal_storage.in_try_block = self.internal_storage.in_try_block;
                new_interpreter.internal_storage.in_function = true;
                new_interpreter.stack = self.stack.clone();
                let body = func.get_body();
                let _ = new_interpreter.interpret(body, true);
                if change_in_function {
                    self.internal_storage.in_function = false;
                }
                if new_interpreter.err.is_some() {
                    self.stack.pop();
                    self.raise_with_ref(
                        "RuntimeError",
                        "Error in function call",
                        new_interpreter.err.unwrap()
                    );
                    return NULL;
                }
                if !effect_flags.contains(EffectFlags::UNKNOWN) {
                    match effect_flags.check_branches(new_interpreter.collected_effects, effect_mask) {
                        Some((false, extra_bits)) => {
                            let names = extra_bits.get_names();
                            return self.raise_with_help(
                                "EffectError",
                                &format!("Function '{}' has unexpected side effects: {}", function_name, names.join(", ")),
                                &format!("Consider adding '{}[{}, ...]{}' to the function's effect annotations.", hex_to_ansi(&self.config.color_scheme.note, self.use_colors), names.join(", "), hex_to_ansi(&self.config.color_scheme.help, self.use_colors)),
                            );
                        }
                        Some((true, missing_bits)) => {
                            // ignore these in warnings
                            let missing_bits = missing_bits & !(EffectFlags::STATE | EffectFlags::PURE);
                            if !missing_bits.is_empty() {
                                let names = missing_bits.get_names();
                                self.warn(
                                    &format!("Warning: Function '{}' does not use some of the specified side effects: {}", function_name, names.join(", "))
                                );
                            }
                        }
                        None => {}
                    }
                }
                result = new_interpreter.return_value.clone();
                for var in instance_variables {
                    if let Some(v) = new_interpreter.variables.get_mut(&var) {
                        if let Some(ref mut object_variable) = object_variable {
                            object_variable.set_value(v.get_value().clone());
                        }
                    }
                }
                self.stack = new_interpreter.stack;
            } else {
                if func.is_natively_callable() {
                    result = func.call(&final_args_no_mods);
                } else {
                    result = func.call_shared(&final_args_no_mods, self);
                }
                if change_in_function {
                    self.internal_storage.in_function = false;
                }
            }
        } else {
            if !func.is_native() {
                let object_value = match &object_variable {
                    Some(var) => var.get_value().clone(),
                    None => {
                        self.stack.pop();
                        if change_in_function {
                            self.internal_storage.in_function = false;
                        }
                        return self.raise(
                            "TypeError",
                            "Method call without an object",
                        );
                    }
                };
                let module = match object_value {
                    Value::Module(ref obj) => obj,
                    _ => {
                        self.stack.pop();
                        if change_in_function {
                            self.internal_storage.in_function = false;
                        }
                        return self.raise(
                            "TypeError",
                            &format!("Expected a module, but got '{}'", object_value.type_name())
                        );
                    }
                };
                let module_file_path = match &object_value {
                    Value::Module(o) => o.path().clone(),
                    _ => {
                        self.stack.pop();
                        if change_in_function {
                            self.internal_storage.in_function = false;
                        }
                        return self.raise(
                            "TypeError",
                            &format!("Expected a module, but got '{}'", object_value.get_type().display_simple())
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
                new_interpreter.internal_storage.in_try_block = self.internal_storage.in_try_block;
                new_interpreter.internal_storage.in_function = true;
                let props = module.get_properties();
                new_interpreter.variables.extend(props.iter().map(|(k, v)| (k.clone(), v.clone())));
                let body = func.get_body();
                let _ = new_interpreter.interpret(body, true);
                if change_in_function {
                    self.internal_storage.in_function = false;
                }
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
                if !effect_flags.contains(EffectFlags::UNKNOWN) {
                    match effect_flags.check_branches(new_interpreter.collected_effects, effect_mask) {
                        Some((true, extra_bits)) => {
                            let names = extra_bits.get_names();
                            return self.raise_with_help(
                                "EffectError",
                                &format!("Function '{}' has unexpected side effects: {}", function_name, names.join(", ")),
                                &format!("Consider adding '{}[{}, ...]{}' to the function's effect annotations.", hex_to_ansi(&self.config.color_scheme.note, self.use_colors), names.join(", "), hex_to_ansi(&self.config.color_scheme.help, self.use_colors)),
                            );
                        }
                        Some((false, missing_bits)) => {
                            let missing_bits = missing_bits & !(EffectFlags::STATE | EffectFlags::PURE);
                            if !missing_bits.is_empty() {
                                let names = missing_bits.get_names();
                                self.warn(
                                    &format!("Warning: Function '{}' does not use some of the specified side effects: {}", function_name, names.join(", "))
                                );
                            }
                        }
                        None => {}
                    }
                }
                self.stack = new_interpreter.stack;
            } else {
                if func.is_natively_callable() {
                    result = func.call(&final_args_no_mods);
                } else {
                    result = func.call_shared(&final_args_no_mods, self);
                }
                if change_in_function {
                    self.internal_storage.in_function = false;
                }
            }
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
        let (is_valid, err) = self.check_type(&result, &metadata.return_type);
        if !is_valid {
            if let Some(err) = err {
                return self.raise_with_ref(
                    "TypeError",
                    &format!("Return value does not match expected type '{}', got '{}'", &metadata.return_type.display(), result.type_name()),
                    err,
                );
            }
            return self.raise(
                "TypeError",
                &format!("Return value does not match expected type '{}', got '{}'", &metadata.return_type.display(), result.type_name())
            );
        }
        return result
    }

    fn handle_operation(&mut self, statement: HashMap<Value, Value>) -> Value {
        let operator = match statement.get(&Value::String("operator".to_string())) {
            Some(Value::String(s)) => s.clone(),
            _ => {
                self.raise("TypeError", "Expected a string for operator");
                return NULL;
            }
        };

        let left = match statement.get(&Value::String("left".to_string())) {
            Some(val_left) => self.evaluate(&val_left.convert_to_statement()),
            None => {
                self.raise("KeyError", "Missing 'left' key in the statement.");
                return NULL;
            }
        };

        if self.err.is_some() {
            return NULL;
        }

        // Short-circuit for '&&' and '||'
        if operator == "&&" || operator == "and" || operator == "||" || operator == "or" {
            let mut left_test = if let Value::Boolean(b) = &left {
                Value::Int(if *b { 1.into() } else { 0.into() })
            } else {
                left.clone()
            };

            if let Value::Pointer(lp) = &left_test {
                let l_val = unsafe {
                    let raw = Arc::as_ptr(lp);
                    let arc = Arc::from_raw(raw);
                    let val = (*arc).clone();
                    std::mem::forget(arc);
                    val
                };
                left_test = l_val;
            }

            if (operator == "&&" || operator == "and") && !left_test.is_truthy() {
                return Value::Boolean(false);
            }
            if (operator == "||" || operator == "or") && left_test.is_truthy() {
                return Value::Boolean(true);
            }
        }

        let right = match statement.get(&Value::String("right".to_string())) {
            Some(val_right) => self.evaluate(&val_right.convert_to_statement()),
            None => {
                self.raise("KeyError", "Missing 'right' key in the statement.");
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

        
        let log_str = match operator.as_str() {
            "abs" => format!("|{}|", format_value(&right)),
            "!" => format!("{}{}", format_value(&right), "!".repeat(if let Value::Int(ref n) = left { n.to_usize().unwrap_or(1) } else { 1 })),
            _ => format!("{} {} {}", format_value(&left), operator, format_value(&right)),
        };

        if let Some(cached) = self.cache.operations.get(&cache_key) {
            debug_log(
                &format!(
                    "<CachedOperation: {} -> {}>",
                    log_str,
                    format_value(cached)
                ),
                &self.config,
                Some(self.use_colors.clone()),
            );
            return cached.clone();
        }

        let mut stdout = stdout();
        
        #[cfg(not(target_arch = "wasm32"))]
        debug_log(
            &format!(
                "<Operation: {} -> ...>",
                log_str
            ),
            &self.config,
            Some(self.use_colors.clone()),
        );
        stdout.flush().unwrap();

        let result = self.make_operation(left.clone(), right.clone(), &operator);

        if self.err.is_some() {
            return NULL;
        }

        #[cfg(not(target_arch = "wasm32"))]
        if self.config.debug {
            stdout.execute(MoveUp(1)).unwrap();
            stdout.execute(MoveToColumn(0)).unwrap();
            stdout.execute(Clear(ClearType::CurrentLine)).unwrap();
            stdout.flush().unwrap();
        }

        debug_log(
            &format!(
                "<Operation: {} -> {}>",
                log_str,
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
            Some(val) => self.evaluate(&val.convert_to_statement()),
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
            "~" if matches!(operand, Value::List(_) | Value::Tuple(_)) => {
                if let Value::List(l) | Value::Tuple(l) = operand {
                    return Value::Type(Type::Unwrap(l));
                }
                unreachable!()
            }
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

    fn handle_struct_operation(&mut self, left: Value, right: Value, operator: &str) -> Value {
        let (s1, s2_val) = match (&left, &right) {
            (Value::Struct(s1), Value::Struct(s2)) => (s1.clone(), Some(s2.clone())),
            (Value::Struct(s1), other) | (other, Value::Struct(s1)) => {
                return self.raise(
                    "TypeError",
                    &format!(
                        "No applicable struct operation found for types '{}' and '{}'",
                        s1.get_type().display_simple(),
                        other.get_type().display_simple()
                    ),
                );
            }
            _ => {
                return self.raise(
                    "TypeError",
                    &format!(
                        "Struct operations are only supported between struct instances, got '{}' and '{}'",
                        left.type_name(),
                        right.type_name()
                    ),
                );
            }
        };

        let struct_type = s1.get_type();

        if let Some(s2) = &s2_val {
            if struct_type != s2.get_type() {
                if let Type::Struct { methods, .. } = &struct_type {
                    for (method_name, func) in methods {
                        if !func.is_static() { continue; }

                        if let Type::Function { parameter_types, return_type } = func.get_type() {
                            if parameter_types.len() == 1 &&
                            type_matches(&parameter_types[0], &s2.get_type()) &&
                            type_matches(&*return_type, &struct_type) {
                                return self.raise_with_help(
                                    "TypeError",
                                    &format!(
                                        "Cannot perform struct operation between different struct types '{}' and '{}'",
                                        struct_type.display_simple(),
                                        s2.get_type().display_simple()
                                    ),
                                    &format!("Try calling '{}.{}({})'", s1.display(), method_name, s2.display()),
                                );
                            }
                        }
                    }
                }

                if let Type::Struct { methods, .. } = &s2.get_type() {
                    for (method_name, func) in methods {
                        if !func.is_static() { continue; }

                        if let Type::Function { parameter_types, return_type } = func.get_type() {
                            if parameter_types.len() == 1 &&
                            type_matches(&parameter_types[0], &struct_type) &&
                            type_matches(&*return_type, &s2.get_type()) {
                                return self.raise_with_help(
                                    "TypeError",
                                    &format!(
                                        "Cannot perform struct operation between different struct types '{}' and '{}'",
                                        s2.get_type().display_simple(),
                                        struct_type.display_simple()
                                    ),
                                    &format!("Try calling '{}.{}({})'", s2.display(), method_name, s1.display()),
                                );
                            }
                        }
                    }
                }

                return self.raise(
                    "TypeError",
                    &format!(
                        "Cannot perform struct operation between different struct types '{}' and '{}'",
                        struct_type.display_simple(),
                        s2.get_type().display_simple()
                    ),
                );
            }
        }

        let func_name = match operator {
            "+" => "op_add",
            "-" => "op_sub",
            "*" => "op_mul",
            "/" => "op_div",
            "%" => "op_mod",
            "==" => "op_eq",
            "!=" => "op_ne",
            ">" => "op_gt",
            "<" => "op_lt",
            ">=" => "op_ge",
            "<=" => "op_le",
            "and" | "&&" => "op_and",
            "or" | "||" => "op_or",
            _ => {
                return self.raise(
                    "SyntaxError",
                    &format!("Unsupported struct operator: '{}'", operator),
                );
            }
        };

        let ret_type = if ["==", "!=", ">", "<", ">=", "<="].contains(&operator) {
            Type::new_simple("bool")
        } else {
            struct_type.clone()
        };

        let expected_params = if let Some(s2) = &s2_val {
            vec![struct_type.clone(), s2.get_type()]
        } else {
            vec![struct_type.clone()]
        };

        let func = match find_struct_method(&s1, s2_val.as_ref(), func_name, &expected_params, &ret_type) {
            Ok(f) => f,
            Err((err_type, err_msg, err_help)) => {
                match operator {
                    "==" => return Value::Boolean(left == right),
                    "!=" => return Value::Boolean(left != right),
                    _ => {}
                } 
                return self.raise_with_help(&err_type, &err_msg, &err_help);
            }
        };

        let mut object_variable = Variable::new_pt(
            s1.name().to_string(),
            Value::Struct(s1.clone()),
            struct_type,
            false,
            false,
            true,
        );

        if let Some((_, props)) = self.get_properties(&left, true) {
            object_variable.properties = props;
        } else {
            return self.raise("RuntimeError", "Failed to get struct properties");
        }

        if func.is_static() {
            self.call_function(
                &func,
                if let Some(s2) = s2_val { vec![Value::Struct(s1), Value::Struct(s2)] } else { vec![Value::Struct(s1)] },
                HashMap::new(),
                Some((None, Some(&mut object_variable))),
            )
        } else {
            self.call_function(
                &func,
                if let Some(s2) = s2_val { vec![Value::Struct(s2)] } else { vec![] },
                HashMap::new(),
                Some((None, Some(&mut object_variable))),
            )
        }
    }

    fn make_operation(&mut self, left: Value, right: Value, mut operator: &str) -> Value {
        operator = match operator {
            "nein" => "!=",
            "or" => "||",
            "and" => "&&",
            other => other,
        };
    
        if (operator == "/" || operator == "%") && right.is_zero() {
            return self.raise("ZeroDivisionError", format!("{} by zero.", if operator == "/" { "Division" } else { "Modulo" }).as_str());
        }
    
        if left.is_nan() || right.is_nan() {
            return self.raise("MathError", "Operation with NaN is not allowed");
        }
    
        if left.is_infinity() || right.is_infinity() {
            return self.raise("MathError", "Operation with Infinity is not allowed");
        }

        match (&left, &right) {
            (Value::Struct(_), Value::Struct(_)) | (Value::Struct(_), _) | (_, Value::Struct(_)) => {
                let res = self.handle_struct_operation(left, right, operator);
                if self.err.is_some() {
                    return NULL;
                }
                return res;
            }
            _ => {}
        }

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
                            if (l % r).unwrap_or(Int::from(0)) == Int::from(0) {
                                Value::Int((l / r).unwrap_or_else(|_| {
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
                            Ok(left_f) => match &left_f / f {
                                Ok(result) => Value::Float(result),
                                Err(_) => self.raise("TypeError", "Failed to perform division"),
                            },
                            Err(_) => self.raise("TypeError", "Failed to convert Int to Float"),
                        }
                        (Value::Float(f), Value::Int(i)) => match Float::from_int(i) {
                            Ok(right_f) => match f / &right_f {
                                Ok(result) => Value::Float(result),
                                Err(_) => self.raise("TypeError", "Failed to perform division"),
                            },
                            Err(_) => self.raise("TypeError", "Failed to convert Int to Float"),
                        }
                        (Value::Float(f1), Value::Float(f2)) => match f1 / f2 {
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

                (Value::Int(a), Value::Int(b)) if a.is_negative() || b.is_negative() => {
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

                (Value::Int(base), Value::Int(height)) if !height.is_negative() => {
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
                            result = match pow_cached(self, base, &result) {
                                Some(res) => res,
                                None => return self.raise("ValueError", "Tetration failed"),
                            };
                            count = (&count + &one).unwrap_or_else(|_| height.clone());
                        }

                        Value::Int(result)
                    }
                }

                (Value::Float(base), Value::Int(height)) if !height.is_negative() => {
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
                            result = match pow_cached(self, base, &result) {
                                Some(res) => res,
                                None => return self.raise("ValueError", "Tetration failed"),
                            };
                            count = (&count + &one).unwrap_or_else(|_| float_height.clone());
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

                (Value::Int(base), Value::Int(height)) if !height.is_negative() => {
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
                            count = (&count + &one).unwrap_or_else(|_| height.clone());
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
                            result = tetration(self, base, &result, &pow_cached).ok_or_else(|| self.raise("ValueError", "Pentation failed")).unwrap_or(one.clone());
                            count = (&count + &one).unwrap_or_else(|_| height.clone());
                        }

                        Value::Int(result)
                    }
                }

                (Value::Float(base), Value::Int(height)) if !height.is_negative() => {
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
                            count = (&count + &one).unwrap_or_else(|_| height.clone());
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
                            count = (&count + &one).unwrap_or_else(|_| float_height.clone());
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
                } else if let Value::Map { keys, values: _ } = &right {
                    Value::Boolean(keys.contains(&left))
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
            "!" => match (left, right) {
                (Value::Int(level), Value::Int(a)) => {
                    let a_i64 = match a.to_i64() {
                        Ok(val) => val,
                        Err(e) => return self.raise("ValueError", &format!("Failed to convert Int: {}", e)),
                    };
                    if a_i64 < 0 {
                        return self.raise("ValueError", "Factorial is not defined for negative integers");
                    }
                    let level_usize = level.to_usize().unwrap_or(1);
                    if level_usize <= 0 {
                        return self.raise("ValueError", "Factorial level must be positive");
                    }

                    let mut res = Int::from(1);
                    let mut i = a_i64;
                    while i > 0 {
                        res *= Int::from(i);
                        i -= level_usize as i64;
                    }
                    Value::Int(res)
                }
                (Value::Int(level), Value::Float(a)) => {
                    if a.is_integer_like() {
                        let a_i64 = a.to_f64().unwrap_or_else(|e| {
                            self.raise("ValueError", &format!("Failed to convert Float: {}", e));
                            0.0
                        }) as i64;
                        let mut res = Int::from(1);
                        for i in 1..=a_i64 {
                            res *= Int::from(i);
                        }
                        return Value::Int(res);
                    }
                    let a_f64 = match a.to_f64() {
                        Ok(val) => val,
                        Err(e) => return self.raise("ValueError", &format!("Failed to convert Float: {}", e)),
                    };
                    if a_f64 < 0.0 {
                        return self.raise("ValueError", "Factorial is not defined for negative numbers");
                    }
                    let level_usize = level.to_usize().unwrap_or(1);
                    if level_usize <= 0 {
                        return self.raise("ValueError", "Factorial level must be positive");
                    }
                    let res = gamma_lanczos(a_f64 + 1.0, level_usize);
                    Value::Float(Float::from(res))
                }
                (_, t) => {
                    self.raise("TypeError", &format!("Factorial is not defined for {}", t.get_type().display_simple()))
                }
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
            "abs" => match right {
                Value::Int(n) => Value::Int(n.abs()),
                Value::Float(f) => Value::Float(f.abs()),
                _ => self.raise("TypeError", &format!("Cannot apply 'abs' to {}", right.type_name())),
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
                let t = if let Value::Type(t) = right {
                    t
                } else {
                    return self.raise_with_help("TypeError", &format!("Cannot apply 'is' to {} and {}", left.type_name(), right.type_name()), &format!("Expected a type, got {}", right.type_name()));
                };
                let res = self.check_type(&left, &t).0;
                Value::Boolean(res)
            },
            "isnt" | "isn't" => {
                let t = if let Value::Type(t) = right {
                    t
                } else {
                    return self.raise_with_help("TypeError", &format!("Cannot apply 'isnt' to {} and {}", left.type_name(), right.type_name()), &format!("Expected a type, got {}", right.type_name()));
                };
                let res = !self.check_type(&left, &t).0;
                Value::Boolean(res)
            },
            _ => self.raise("SyntaxError", &format!("Unknown operator '{}'", operator)),
        };
        return result;
    }

    fn handle_number(&mut self, map: HashMap<Value, Value>) -> Value {
        let s = to_static(match map.get(&Value::String("value".to_string())) {
            Some(Value::String(s)) if !s.is_empty() => s.as_str(),
            Some(Value::String(_)) => return self.raise("RuntimeError", "Empty string provided for number"),
            _ => return self.raise("RuntimeError", "Missing 'value' in number statement"),
        }.replace('_', ""));

        if s.is_empty() {
            return self.raise("RuntimeError", "Empty string provided for number");
        }
    
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
            if is_number_parentheses(s) {
                return Value::Float(imagnum::create_float(s));
            }

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
                                        let mut format_spec: Option<String> = None;
                                        let mut brace_level = 1;
                                        let mut in_string: Option<char> = None;

                                        while let Some(&next_c) = chars.peek() {
                                            chars.next();

                                            if let Some(quote) = in_string {
                                                expr.push(next_c);
                                                if next_c == quote {
                                                    in_string = None;
                                                }
                                                continue;
                                            } else if next_c == '"' || next_c == '\'' {
                                                in_string = Some(next_c);
                                                expr.push(next_c);
                                                continue;
                                            }

                                            if next_c == '{' {
                                                brace_level += 1;
                                            } else if next_c == '}' {
                                                brace_level -= 1;
                                                if brace_level == 0 {
                                                    break;
                                                }
                                            }

                                            if brace_level == 1
                                                && next_c == ':'
                                                && chars.peek() == Some(&':')
                                            {
                                                chars.next();
                                                let mut spec = String::new();
                                                while let Some(&spec_c) = chars.peek() {
                                                    if spec_c == '}' { break; }
                                                    chars.next();
                                                    spec.push(spec_c);
                                                }
                                                format_spec = Some(spec.trim().to_string());
                                                continue;
                                            }

                                            expr.push(next_c);
                                        }

                                        expr = match unescape_string_premium_edition(&expr) {
                                            Ok(unescaped) => unescaped,
                                            Err(err) => {
                                                return self.raise("UnescapeError", &err);
                                            }
                                        };

                                        if brace_level != 0 {
                                            return self.raise("SyntaxError", "Unmatched '{' in f-string");
                                        }

                                        let tokens = Lexer::new(&expr, &self.file_path.clone()).tokenize();

                                        let filtered = tokens.iter()
                                            .filter(|token| {
                                                let t = &token.0;
                                                t != "WHITESPACE" && !t.starts_with("COMMENT_") && t != "EOF"
                                            })
                                            .collect::<Vec<_>>();

                                        let formatted_toks = filtered.iter()
                                            .map(|token| (&token.0, &token.1))
                                            .collect::<Vec<_>>();

                                        debug_log(
                                            &format!("Generated f-string tokens: {:?}", formatted_toks),
                                            &self.config,
                                            Some(self.use_colors),
                                        );

                                        let tokens_no_loc: Vec<Token> = tokens
                                            .iter()
                                            .map(|tk| Token(tk.0.clone(), tk.1.clone(), None))
                                            .collect();

                                        let parsed = match Parser::new(tokens_no_loc).parse_safe() {
                                            Ok(parsed) => parsed,
                                            Err(error) => {
                                                return self.raise(
                                                    "SyntaxError",
                                                    &format!("Error parsing f-string expression: {}", error.msg),
                                                );
                                            }
                                        };

                                        debug_log(
                                            &format!(
                                                "Generated f-string statement: {}",
                                                parsed.iter().map(|stmt| {
                                                    let cleaned = remove_loc_keys(&stmt.convert_to_map());
                                                    format_value(&cleaned)
                                                }).collect::<Vec<String>>().join(", ")
                                            ),
                                            &self.config,
                                            Some(self.use_colors),
                                        );

                                        debug_log(
                                            &format!("<FString: {}>", expr.clone().trim()),
                                            &self.config,
                                            Some(self.use_colors),
                                        );

                                        let result_val = self.evaluate(&parsed[0]);
                                        if self.err.is_some() {
                                            return NULL;
                                        }
                                        let result;
                                        if let Some(spec) = format_spec {
                                            result = match apply_format_spec(&result_val, &spec) {
                                                Ok(res) => res,
                                                Err(e) => return self.raise("SyntaxError", &e),
                                            };
                                        } else {
                                            result = match escape_string(&native::format_value(&result_val)) {
                                                Ok(res) => res,
                                                Err(e) => return self.raise("UnicodeError", &e),
                                            };
                                        }

                                        output.push_str(&result);
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
                            "r" => is_raw = true,
                            "b" => is_bytes = true,
                            _ => return self.raise("RuntimeError", &format!("Unknown string modifier: {}", modifier)),
                        }
                    } else {
                        self.raise("TypeError", "Expected a string for string modifier");
                        return NULL;
                    }
                }
            }

            if is_raw {
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
                modified_string = unquoted;
            } else {
                modified_string = match unescape_string(&modified_string) {
                    Ok(unescaped) => unescaped,
                    Err(e) => return self.raise("UnicodeError", &e),
                };
            }

            if is_bytes {
                return Value::Bytes(modified_string.clone().into_bytes());
            }

            Value::String(modified_string)
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
