
use std::collections::{HashMap, BTreeMap};
use crate::env::runtime::config::{Config, get_from_config, set_in_config};
use crate::env::runtime::utils::{
    hex_to_ansi,
    format_value,
    find_closest_match,
    TRUE, FALSE, NULL,
    debug_log_internal,
    check_ansi,
    unescape_string,
    to_static,
    create_function,
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
    apply_format_spec,
    type_matches,
    generate_name_variants,
    escape_string,
    find_struct_method,
    diff_fields,
    is_valid_alias,
    get_pointer_depth_and_base_value,
    is_type_ident_available,
    get_object_type_ident,
    unalias_type,
    parse_doc,
};

use crate::env::runtime::pattern_reg::{predict_sequence, predict_sequence_until_length};
use crate::env::runtime::types::{Int, Float, Type, SimpleType, VALID_TYPES};
use crate::env::runtime::value::Value;
use crate::env::runtime::errors::Error;
use crate::env::runtime::variables::Variable;
use crate::env::runtime::statements::{Statement, Node, ThrowNode, MatchCase, IterableNode, RangeModeType, AccessType, alloc_loc, get_loc, TypeNode, TypeDeclNode, PtrNode, ParamAST, ForLoopNode};
use crate::env::runtime::modules::Module;
use crate::env::runtime::native::{self, get_default_type_method, get_type_method_names};
use crate::env::runtime::functions::{FunctionMetadata, Parameter, ParameterKind, Function, NativeFunction, UserFunctionMethod, UserFunction, Callable};
use crate::env::runtime::generators::{Generator, GeneratorType, NativeGenerator, CustomGenerator, RangeValueIter, InfRangeIter, RangeLengthIter};
use crate::env::runtime::libs::{STD_LIBS, get_lib_names};
use crate::env::runtime::internal_structs::{Cache, InternalStorage, State, PatternMethod, Stack, StackType, EffectFlags, PathElement, ScopeStack};
use crate::env::runtime::plugins::{PluginRuntime};
use crate::env::runtime::structs_and_enums::{Enum, Struct};
use std::sync::{Arc, atomic::{AtomicBool, Ordering}};
use std::path::{PathBuf, Path};
use std::fs;
#[cfg(not(target_arch = "wasm32"))]
use tokio::runtime::Runtime;
#[cfg(not(target_arch = "wasm32"))]
use reqwest;
#[cfg(not(target_arch = "wasm32"))]
use serde_urlencoded;

use parking_lot::Mutex;
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
    pub current_statement: Option<Statement>,
    pub statement_before: Option<Statement>,
    pub variables: FxHashMap<String, Variable>,
    file_path: String,
    cwd: PathBuf,
    preprocessor_info: (PathBuf, PathBuf, bool),
    cache: Cache,
    internal_storage: InternalStorage,
    defer_stack: Vec<Vec<Statement>>,
    scope: String,
    pub collected_effects: EffectFlags,
    scopes: ScopeStack,
    
    #[serde(skip)]
    pub stop_flag: Option<Arc<AtomicBool>>,
}

impl Interpreter {
    pub fn new(config: Config, file_path: &str, cwd: &PathBuf, preprocessor_info: (PathBuf, PathBuf, bool), argv: &[String]) -> Self {
        let mut this = Self {
            config: config.clone(),
            og_cfg: config.clone(),
            err: None,
            return_value: NULL,
            is_returning: false,
            state: State::Normal,
            stack: Stack::new(),
            current_statement: None,
            statement_before: None,
            variables: FxHashMap::with_capacity_and_hasher(32, Default::default()),
            file_path: file_path.to_owned(),
            cwd: cwd.clone(),
            preprocessor_info,
            cache: Cache {
                operations: FxHashMap::default(),
                constants: FxHashMap::default(),
                iterables: FxHashMap::default(),
                last_called_function: None,
            },
            internal_storage: InternalStorage {
                lambda_counter: 0,
                use_42: (false, false),
                in_try_block: false,
                in_function: false,
                is_the_main_thread: false,
                plugin_runtime: None,
                underscore_vars_initialized: false,
            },
            defer_stack: vec![],
            scope: "main".to_owned(),
            stop_flag: None,
            collected_effects: EffectFlags::empty(),
            scopes: ScopeStack::new(),
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
        insert_builtin("println", Value::Function(native::println_fn()));
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
        insert_builtin("complex", Value::Function(native::complex_fn()));
        insert_builtin("range", Value::Function(native::range_fn())); // added for u/Truite_Morte
        this.variables.insert(
            "00__placeholder__".to_owned(),
            Variable::new("__placeholder__".to_owned(), Value::Function(native::placeholder_fn()), "function".to_owned(), true, false, true),
        );
    
        this
    }

    #[inline]
    pub fn debug_log(&self, message: std::fmt::Arguments) {
        if self.config.debug && (self.config.debug_mode == "full" || self.config.debug_mode == "normal") {
            let message = format!("{}", message);
            debug_log_internal(message, &self.config);
        }
    }

    #[inline]
    pub fn set_scope(&mut self, scope: &str) {
        self.scope = scope.to_owned();
    }

    #[inline]
    pub fn set_plugin_runtime(&mut self, plugin_runtime: PluginRuntime) {
        self.internal_storage.plugin_runtime = Some(plugin_runtime);
    }

    #[inline]
    pub fn call_hook(&mut self, hook_name: &str, args: Vec<Value>) {
        if let Some(plugin_runtime) = &mut self.internal_storage.plugin_runtime {
            plugin_runtime.call_hook(hook_name, args);
        }
    }

    #[inline]
    pub fn is_stopped(&self) -> bool {
        self.state == State::Exit
    }

    #[inline]
    pub fn set_cache(&mut self, cache: Cache) {
        self.cache = cache;
    }

    #[inline]
    pub fn get_cache(&self) -> &Cache {
        &self.cache
    }

    pub fn get_repl_completions(&self) -> Vec<String> {
        let mut owned_keys: Vec<String> = self.variables.keys().cloned().collect();
        owned_keys.extend(STD_LIBS.keys());
        owned_keys = owned_keys.into_iter().filter(|k| !k.starts_with("00__")).collect();
        owned_keys.sort();
        owned_keys.dedup();
        owned_keys
    }

    #[inline]
    fn check_type_validity(&self, type_str: &str) -> bool {
        let trimmed = type_str.trim_start_matches('&').trim_start_matches('?');
        if let Some(t) = self.variables.get(trimmed) {
            matches!(t.value, Value::Type(_))
        } else {
            false
        }
    }

    #[inline]
    pub fn set_main_thread(&mut self, is_main: bool) {
        self.internal_storage.is_the_main_thread = is_main;
    }

    fn to_index(&mut self, val: &Value, len: usize) -> Result<usize, Value> {
        match val {
            Value::Int(i) => {
                let idx = i.to_i64().map_err(|_| { self.raise("ConversionError", "Failed to convert Int to isize") })? as isize;
                let adjusted = if idx < 0 { len as isize + idx } else { idx };
                if adjusted >= 0 && (adjusted as usize) < len {
                    Ok(adjusted as usize)
                } else {
                    Err(self.raise("IndexError", &format!("Index '{}' out of range", adjusted)))
                }
            }
            Value::String(s) => {
                let idx: isize = s.parse()
                    .map_err(|_| self.raise("ConversionError", "Failed to parse string to int"))?;
                let adjusted = if idx < 0 { len as isize + idx } else { idx };
                if adjusted >= 0 && (adjusted as usize) < len {
                    Ok(adjusted as usize)
                } else {
                    Err(self.raise("IndexError", &format!("Index '{}' out of range", adjusted)))
                }
            }            
            Value::Float(f) => {
                if !f.is_integer_like() {
                    return Err(self.raise("IndexError", "Float index must have zero fractional part"));
                }
                let idx = f.to_f64().map_err(|_| { self.raise("ConversionError", "Failed to convert Float to isize") })? as isize;
                let adjusted = if idx < 0 { len as isize + idx } else { idx };
                if adjusted >= 0 && (adjusted as usize) < len {
                    Ok(adjusted as usize)
                } else {
                    Err(self.raise("IndexError", &format!("Index '{}' out of range", adjusted)))
                }
            }
            _ => Err(self.raise("IndexError", "Index must be Int, String or Float with no fraction")),
        }
    }

    pub fn get_traceback(&self) -> BTreeMap<String, String> {
        let mut traceback = BTreeMap::new();
        for (file, loc, _) in self.stack.iter() {
            let location = loc.as_ref().map_or("unknown location".to_owned(), |l| format!("{}:{}:{}", l.file, l.line_number, l.range.0));
            traceback.insert(file.clone(), location);
        }
        traceback
    }

    pub fn eval(&mut self, code: &str) -> Value {
        self.debug_log(
            format_args!("<Eval script: '{}'>", code)
        );
        let lexer = Lexer::new(code, to_static(self.file_path.clone()));
        let tokens = lexer.tokenize();

        let mut parser = Parser::new(
            tokens,
        );
        let statements = parser.parse();
        let (return_value, _state, err, _variables) = self.run_isolated(self.variables.clone(), &statements, false);
        if let Some(err) = err {
            self.raise_with_ref("RuntimeError", "Error in exec script", err);
        }
        return return_value.clone();
    }

    #[track_caller]
    #[cfg(not(target_arch = "wasm32"))]
    pub fn breakpoint(&mut self, message: Option<&str>) -> Value {
        if self.config.debug {
            let caller = PanicLocation::caller();
            if let Some(loc) =self.get_location_from_current_statement_caller(*caller) {
                self.debug_log(format_args!("<{}: Reached breakpoint{}>", loc, message.map_or("".to_owned(), |m| format!(": {}", m))));
            } else {
                self.debug_log(format_args!("<Reached breakpoint{}>", message.map_or("".to_owned(), |m| format!(": {}", m))));
            }
        }
        let mut ret_val = NULL;
        println!("--- Breakpoint Reached ---");
        println!("Type 'continue' to resume execution or 'help' for more options.");
        if let Some(msg) = message {
            println!("Called with message: {}", msg);
        }
        println!("--------------------------");
        let completions = [
            "continue".to_owned(),
            "exit".to_owned(),
            "vars".to_owned(),
            "help".to_owned(),
        ];
        let mut history = vec![];
        loop {
            let input = crate::env::runtime::repl::read_input_breakpoint(
                "breakpoint> ",
                "..........> ",
                &mut history,
                &completions,
            );
            let trimmed = input.trim();
            if trimmed.is_empty() {
                continue;
            }
            if trimmed.starts_with("set_var(") && trimmed.ends_with(")") {
                let inner = &trimmed[8..trimmed.len()-1];
                let parts: Vec<&str> = inner.splitn(2, ',').map(|s| s.trim()).collect();
                if parts.len() == 2 {
                    let var_name = parts[0].trim_matches('"').trim_matches('\'');
                    let var_value_str = parts[1];
                    let var_value = self.eval(var_value_str);
                    if self.err.is_some() {
                        println!("Error evaluating value: {}", self.err.as_ref().unwrap().err_msg());
                        self.err = None;
                        continue;
                    }
                    self.variables.insert(
                        var_name.to_owned(),
                        Variable::new(var_name.to_owned(), var_value.clone(), "any".to_owned(), false, false, false),
                    );
                } else {
                    println!("Invalid syntax for set_var. Use: set_var(\"var_name\", value)");
                }
                continue;
            }
            if trimmed.starts_with("eval(") && trimmed.ends_with(")") {
                let inner = &trimmed[5..trimmed.len()-1];
                let eval_value = self.eval(inner);
                if self.err.is_some() {
                    println!("Error evaluating code: {}", self.err.as_ref().unwrap().err_msg());
                    self.err = None;
                    continue;
                } else {
                    println!("Eval result: {}", format_value(&eval_value));
                }
                continue;
            }
            if trimmed.starts_with("return ") {
                let inner = &trimmed[7..];
                ret_val = self.eval(inner);
                if self.err.is_some() {
                    println!("Error evaluating return value: {}", self.err.as_ref().unwrap().err_msg());
                    self.err = None;
                    continue;
                } else {
                    println!("Returning with value: {}", format_value(&ret_val));
                    break;
                }
            }

            match trimmed {
                "continue" => break,
                "exit" => {
                    self.state = State::Exit;
                    break;
                }
                "vars" => {
                    println!("--- Variables ---");
                    for (name, var) in &self.variables {
                        println!("{}: {}", name, format_value(&var.value));
                    }
                    println!("-----------------");
                }
                "help" => {
                    println!("Available commands:");
                    println!("  continue                 - Resume execution");
                    println!("  return value             - Return from breakpoint with a value");
                    println!("  exit                     - Exit the interpreter");
                    println!("  vars                     - List all variables in the current scope");
                    println!("  eval(...)                - Evaluate an expression");
                    println!("  set_var(\"name\", value) - Set a variable to a value");
                    println!("  help                     - Show this help message");
                }
                _ => {
                    println!("Unknown command: '{}'. Type 'help' for a list of commands.", trimmed);
                }
            }
        }
        ret_val
    }

    
    #[cfg(target_arch = "wasm32")]
    pub fn breakpoint(&mut self, message: Option<&str>) -> Value {
        println!("Breakpoint reached{}, but in WASM build. No-op.", message.map_or("".to_owned(), |m| format!("with message \"{}\"", m)));
        NULL
    }

    // this is a lot cleaner than last time, glad i refactored it
    fn check_type(&mut self, value: &Value, expected: &Type) -> (bool, Option<Error>) {
        if self.config.disable_runtime_type_checking {
            return (true, None);
        }

        let value_type = value.get_type();
        if std::ptr::eq(&value_type, expected) || value_type == *expected {
            return (true, None);
        }

        if let Type::Simple { ty: expected_type } = expected {
            if *expected_type == SimpleType::Any {
                return (true, None);
            }
        }

        let err: Option<Error> = None;
        let mut status: bool = true;
        
        #[inline]
        fn make_err(err_type: &str, err_msg: &str, loc: Option<Location>) -> Error {
            loc.map(|l| Error::with_location(err_type, err_msg, l))
                .unwrap_or_else(|| Error::new_anonymous(err_type, err_msg))
        }

        match expected {
            Type::Simple { ty: expected_type  } => {
                if *expected_type != SimpleType::Any {
                    match value_type {
                        Type::Simple { ty: value_type_name, .. } => {
                            if value_type_name != *expected_type {
                                status = false;
                            }
                        }
                        Type::Function { .. } => {
                            if *expected_type != SimpleType::Function {
                                status = false;
                            }
                        }
                        Type::Generator { .. } => {
                            if *expected_type != SimpleType::Generator {
                                status = false;
                            }
                        }
                        _ => {
                            return (false, Some(make_err("TypeError", 
                                &format!("Expected type '{}', got '{}'", expected_type, value_type.display()), 
                                self.get_location_from_current_statement())));
                        }
                    }
                }
            }
            Type::Maybe { base_type } => {
                if self.check_type(value, base_type).0 {
                    return (true, None);
                } else if matches!(value, Value::Null) {
                    return (true, None);
                } else {
                    return (false, Some(make_err("TypeError", 
                        &format!("Expected type '?{}', got '{}'", base_type.display_simple(), value_type.display_simple()), 
                        self.get_location_from_current_statement())));
                }
            }
            Type::Reference { base_type, ref_level } => {
                let (current_ref_level, current_value) = get_pointer_depth_and_base_value(value);
                if current_ref_level != *ref_level {
                    return (false, Some(make_err("TypeError", 
                        &format!("Expected reference level {}, got {}", ref_level, current_ref_level), 
                        self.get_location_from_current_statement())));
                }

                return self.check_type(&current_value, base_type);
            }
            Type::Indexed { base_type: base, elements } => {
                if value_type != **base {
                    return (false, None);
                }
                
                match value {
                    Value::List(l) | Value::Tuple(l) => {
                        let val_type: &str = if matches!(value, Value::Tuple(_)) { "tuple" } else { "list" };
                        
                        if elements.len() == 1 {
                            let element_type = &elements[0];
                            for (i, elem) in l.iter().enumerate() {
                                if !self.check_type(elem, element_type).0 {
                                    return (false, Some(make_err("TypeError", 
                                        &format!("Expected type '{}' for {} element #{}, got '{}' ({})", 
                                            element_type.display_simple(), val_type, i + 1, 
                                            elem.get_type().display_simple(), format_value(elem)), 
                                        self.get_location_from_current_statement())));
                                }
                            }
                        } else if l.len() != elements.len() {
                            return (false, Some(make_err("TypeError", 
                                &format!("Expected {} of length {}, got {}", val_type, elements.len(), l.len()), 
                                self.get_location_from_current_statement())));
                        } else {
                            for (i, (elem, expected_elem_type)) in l.iter().zip(elements.iter()).enumerate() {
                                if !self.check_type(elem, expected_elem_type).0 {
                                    return (false, Some(make_err("TypeError", 
                                        &format!("Expected type '{}' for {} element #{}, got '{}' ({})", 
                                            expected_elem_type.display_simple(), val_type, i + 1, 
                                            elem.get_type().display_simple(), format_value(elem)), 
                                        self.get_location_from_current_statement())));
                                }
                            }
                        }
                    }
                    Value::Map(map) => {
                        if elements.len() != 2 {
                            return (false, Some(make_err("TypeError", 
                                &format!("Expected type with 2 elements, got {}", elements.len()), 
                                self.get_location_from_current_statement())));
                        }
                        
                        let key_type = &elements[0];
                        let value_type = &elements[1];
                        
                        for (k, v) in map.iter() {
                            if !self.check_type(k, key_type).0 {
                                return (false, Some(make_err("TypeError", 
                                    &format!("Expected type '{}' for map key, got '{}'", key_type.display(), k.get_type().display()), 
                                    self.get_location_from_current_statement())));
                            }
                            if !self.check_type(v, value_type).0 {
                                return (false, Some(make_err("TypeError", 
                                    &format!("Expected type '{}' for map value, got '{}'", value_type.display(), v.get_type().display()), 
                                    self.get_location_from_current_statement())));
                            }
                        }
                    }
                    Value::Pointer(ptr) => {
                        let ptr_val = ptr.lock();
                        return self.check_type(&ptr_val.0, expected);
                    }
                    _ => {
                        return (false, Some(make_err("TypeError", 
                            &format!("Expected type '{}' for indexed type, got '{}'", base.display(), value_type.display()), 
                            self.get_location_from_current_statement())));
                    }
                }
            }
            Type::Union(types) => {
                if types.is_empty() {
                    return (false, Some(make_err("TypeError", "Union type cannot be empty", self.get_location_from_current_statement())));
                }
                
                for ty in types {
                    if value_type == *ty {
                        return (true, None);
                    }
                }
                
                for ty in types {
                    if self.check_type(value, ty).0 {
                        return (true, None);
                    }
                }
                
                let type_names: Vec<String> = types.iter().map(|t| t.display()).collect();
                return (false, Some(make_err("TypeError", 
                    &format!("Expected one of the union types: {}, got '{}'", type_names.join(", "), value_type.display()), 
                    self.get_location_from_current_statement())));
            }
            Type::Function { return_type: expected_return_type, parameter_types: expected_parameter_types } => {
                let Value::Function(f) = value else {
                    return (false, Some(make_err("TypeError", 
                        &format!("Expected type 'function', got '{}'", value_type.display()), 
                        self.get_location_from_current_statement())));
                };
                
                let return_type = f.get_return_type();
                let parameter_types = f.get_parameter_types();
                
                if !type_matches(&return_type, expected_return_type) {
                    return (false, Some(make_err("TypeError", 
                        &format!("Expected return type '{}', got '{}'", expected_return_type.display(), return_type.display()), 
                        self.get_location_from_current_statement())));
                }
                
                if parameter_types.len() != expected_parameter_types.len() {
                    return (false, Some(make_err("TypeError",
                        &format!("Expected {} parameters, got {}", expected_parameter_types.len(), parameter_types.len()),
                        self.get_location_from_current_statement())));
                }
                
                for (i, (param_type, expected_param_type)) in parameter_types.iter().zip(expected_parameter_types.iter()).enumerate() {
                    if !type_matches(param_type, expected_param_type) {
                        return (false, Some(make_err("TypeError",
                            &format!("Expected parameter type '{}' for parameter #{}, got '{}'", 
                                expected_param_type.display(), i + 1, param_type.display()),
                            self.get_location_from_current_statement())));
                    }
                }
            }
            Type::Generator { yield_type: expected_yield_type, parameter_types: expected_parameter_types } => {
                let Value::Generator(g) = value else {
                    return (false, Some(make_err("TypeError",
                        &format!("Expected type 'generator', got '{}'", value_type.display()),
                        self.get_location_from_current_statement())));
                };

                if let Some(yield_type) = g.get_yield_type() {
                    if yield_type != **expected_yield_type {
                        return (false, Some(make_err("TypeError",
                            &format!("Expected yield type '{}', got '{}'", expected_yield_type.display(), yield_type.display()),
                            self.get_location_from_current_statement())));
                    }
                }

                if let Some(parameter_types) = g.get_parameter_types() {
                    for (i, (param_type, expected_param_type)) in parameter_types.iter().zip(expected_parameter_types.iter()).enumerate() {
                        if !type_matches(param_type, expected_param_type) {
                            return (false, Some(make_err("TypeError",
                                &format!("Expected parameter type '{}' for parameter #{}, got '{}'", 
                                    expected_param_type.display(), i + 1, param_type.display()),
                                self.get_location_from_current_statement())));
                        }
                    }
                }
            }
            Type::Alias { name: alias_name, base_type, conditions, variables } => {
                if variables.len() > 1 {
                    return (false, Some(make_err("TypeError", 
                        &format!("Alias '{}' expects only one variable, got {}", alias_name, variables.len()), 
                        self.get_location_from_current_statement())));
                }
                
                let inner_type = match get_inner_type(&base_type) {
                    Ok((_, t)) => t,
                    Err(e) => {
                        return (false, Some(make_err("TypeError", &e, self.get_location_from_current_statement())));
                    }
                };
                
                let (status_check, err_check) = self.check_type(value, &inner_type);
                if !status_check {
                    return (false, err_check.or(Some(make_err("TypeError", 
                        &format!("Alias '{}' expects type '{}', got '{}'", 
                            alias_name, inner_type.display(), value_type.display()), 
                        self.get_location_from_current_statement()))));
                }
                
                if conditions.is_empty() {
                    return (true, None);
                }
                
                let mut vars: HashMap<String, Variable> = HashMap::with_capacity(variables.len());
                let value_type_str = value_type.display().to_string();
                for var_name in variables.iter() {
                    vars.insert(var_name.clone(), Variable::new(var_name.clone(), value.clone(), value_type_str.clone(), false, true, true));
                }
                
                let mut new_interpreter = Interpreter::new(
                    self.config.clone(),
                    &self.file_path,
                    &self.cwd,
                    self.preprocessor_info.clone(),
                    &[],
                );
                new_interpreter.variables.extend(vars);
                new_interpreter.set_scope(&format!("{}+scope.{}", self.scope, alias_name));
                
                for (i, cond) in conditions.iter().enumerate() {
                    if !new_interpreter.evaluate(&cond).is_truthy() {
                        return (false, Some(make_err("TypeError", 
                            &format!("Conditions for alias '{}' don't meet condition #{}", alias_name, i + 1), 
                            self.get_location_from_current_statement())));
                    }
                }
            }
            Type::Enum { generics, .. } => {
                let Value::Enum(e) = value else {
                    return (false, None);
                };
                
                if type_matches(&e.ty, expected) {
                    if self.config.debug && !generics.is_empty() {
                        self.warn("TypeWarning: Strict type checking for Enums with generics is not implemented yet.");
                    }
                    return (true, None);
                }
                return (false, None);
            }
            Type::Struct { .. } => {
                let Value::Struct(s) = value else {
                    return (false, None);
                };
                
                return (type_matches(&s.ty, expected), None);
            }
            Type::Impl { implementations } => {
                let Some((_, props)) = self.get_properties(value, true) else {
                    return (false, Some(make_err("TypeError", 
                        &format!("Expected type compatible with 'impl', got incompatible '{}'", value_type.display()), 
                        self.get_location_from_current_statement())));
                };
                
                for (name, ty, mods) in implementations {
                    let (is_static, is_final) = mods.iter().fold((false, true), |(mut static_val, mut final_val), m| {
                        match m.as_str() {
                            "static" => static_val = true,
                            "non-static" => static_val = false,
                            "final" => final_val = true,
                            "mutable" => final_val = false,
                            _ => {}
                        }
                        (static_val, final_val)
                    });
                    
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
                            return (false, Some(make_err("TypeError", &e, self.get_location_from_current_statement())));
                        }
                    };
                    
                    let Some(prop) = props.get(name) else {
                        return (false, Some(make_err("TypeError", 
                            &format!("Property '{}' not found in value", name), 
                            self.get_location_from_current_statement())));
                    };
                    
                    let Value::Function(f) = &prop.value else {
                        return (false, Some(make_err("TypeError", 
                            &format!("Property '{}' is not a function", name), 
                            self.get_location_from_current_statement())));
                    };
                    
                    if is_final != f.is_final() {
                        let expected = if is_final { "final" } else { "non-final" };
                        let actual = if f.is_final() { "final" } else { "non-final" };
                        return (false, Some(make_err("TypeError",
                            &format!("Method '{}' is {} but expected {}", name, actual, expected),
                            self.get_location_from_current_statement())));
                    }

                    if is_static != f.is_static() {
                        let expected = if is_static { "static" } else { "non-static" };
                        let actual = if f.is_static() { "static" } else { "non-static" };
                        return (false, Some(make_err("TypeError",
                            &format!("Method '{}' is {} but expected {}", name, actual, expected),
                            self.get_location_from_current_statement())));
                    }
                    
                    let (type_status, type_err) = self.check_type(&prop.value, &t);
                    if !type_status {
                        return (false, type_err.or(Some(make_err("TypeError", 
                            &format!("Property '{}' expected to be of type '{}', got '{}'", 
                                name, ty.display(), prop.value.get_type().display()), 
                            self.get_location_from_current_statement()))));
                    }
                }
            }
            _ => {
                return (false, Some(make_err("TypeError", 
                    &format!("Unsupported type check for '{}'", expected.display()), 
                    self.get_location_from_current_statement())));
            }
        }

        (status, err)
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

        self.debug_log(format_args!(
            "<Fetching {} {} with data: {}>",
            method,
            parsed_url.as_str(),
            body.as_deref().unwrap_or("null")
        ));
    
        let req = client
            .request(method, parsed_url)
            .headers(req_headers);
    
        let req = if let Some(body_str) = body {
            req.body(body_str)
        } else {
            req
        };
    
        let resp = req.send().await?;

        self.debug_log(format_args!("<Response status: {}>", resp.status()));

        let status = resp.status().as_u16();
        let headers = resp.headers().clone();
        let body = resp.text().await?;
    
        let headers_map = headers.iter()
            .map(|(k, v)| (Value::String(k.to_string()), Value::String(v.to_str().unwrap_or("").to_string())))
            .collect::<FxHashMap<_, _>>();
    
        Ok(Value::Map(
            HashMap::from_iter([
                (Value::String("status".to_owned()), Value::Int(Int::from_i64(status as i64))),
                (Value::String("headers".to_owned()), Value::Map(headers_map)),
                (Value::String("body".to_owned()), Value::String(body)),
            ])
        ))
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
            Some(Value::Map(map)) => {
                let mut new_map: HashMap<String, String> = HashMap::with_capacity(map.keys().len());
                for (k, v) in map.iter() {
                    new_map.insert(k.to_string(), v.to_string());
                }
                Some(new_map)
            }
            _ => None,
        };

        let params = match args.get("params") {
            Some(Value::Map(map)) => {
                let mut params_map = HashMap::with_capacity(map.keys().len());
                for (k, v) in map.iter() {
                    params_map.insert(k.to_string(), v.to_string());
                }
                Some(params_map)
            }
            _ => None,
        };

        let data = match args.get("data") {
            Some(Value::Map(map)) => {
                let mut data_map = HashMap::with_capacity(map.keys().len());
                for (k, v) in map.iter() {
                    data_map.insert(k.to_string(), v.to_string());
                }
                Some(data_map)
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
            _ => "GET".to_owned(),
        };

        let headers_map = match args.get("headers") {
            Some(Value::Map(map)) => {
                let mut new_map: HashMap<String, String> = HashMap::with_capacity(map.keys().len());
                for (k, v) in map.iter() {
                    new_map.insert(k.to_string(), v.to_string());
                }
                Some(new_map)
            }
            _ => None,
        };

        let body = if let Some(json_val) = args.get("json") {
            to_value(json_val).ok().map(|jsv| JsString::from(jsv.as_string().unwrap_or_default()))
        } else if let Some(Value::Map(map)) = args.get("data") {
            let mut form = vec![];
            for (k, v) in map.iter() {
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
                        .unwrap_or_else(|_| "Failed to read response".to_owned());
                    tx_clone.send(text).ok();
                }
                Err(_) => {
                    tx_clone.send("Fetch failed".to_owned()).ok();
                }
            }
        });

        let result_text = rx.recv().unwrap_or_else(|_| "Fetch failed".to_owned());
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

        self.get_properties_from_str(&content, Some(path))
    }

    fn get_properties_from_str(&mut self, code: &str, path: Option<&Path>) -> HashMap<String, Variable> {
        let path_str = path.map_or("<module>".to_owned(), |p| p.display().to_string());
        let path = path.unwrap_or(Path::new("."));

        let binding = path.display().to_string();
        let lexer = Lexer::new(&code, &binding);
        let raw_tokens = lexer.tokenize();

        let (pr1, _, ep) = self.preprocessor_info.clone();
        let processed_tokens = if ep {
            let mut preprocessor = Preprocessor::new(
                pr1,
                path.display().to_string().as_str(),
            );
            match preprocessor.process(raw_tokens, path.parent().unwrap_or(Path::new(""))) {
                Ok(tokens) => tokens,
                Err(e) => {
                    self.raise_with_ref(
                        "ImportError",
                        &format!("Error while preprocessing '{}'", path_str),
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
                    &format!("Error while parsing '{}'", path_str),
                    error,
                );
                return HashMap::new();
            }
        };
    
        let parent_dir = Path::new(path)
            .parent()
            .unwrap_or(Path::new("."))
            .canonicalize()
            .unwrap_or_else(|_| PathBuf::from("."));

        let shared_interpreter = Arc::new(Mutex::new(Interpreter::new(
            self.config.clone(),
            path.display().to_string().as_str(),
            &parent_dir,
            self.preprocessor_info.clone(),
            &vec![],
        )));

        shared_interpreter.lock().stack = self.stack.clone();
        let result = shared_interpreter.lock().interpret(statements, true);
        self.stack = shared_interpreter.lock().stack.clone();

        let properties = shared_interpreter.lock().variables.clone();

        if let Some(err) = shared_interpreter.lock().err.clone() {
            self.raise_with_ref(
                "ImportError",
                &format!("Error while importing '{}'", path_str),
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
                                "function".to_owned(),
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
                    &format!("Error while importing '{}'", path_str),
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

    pub fn interpret(&mut self, statements: Vec<Statement>, deferable: bool) -> Result<Value, Error> {
        self.is_returning = false;
        self.return_value = NULL;
        self.err = None;
        self.current_statement = None;
        self.statement_before = None;
        self.state = State::Normal;
        self.stack.clear();
        self.stack.push((
            self.file_path.clone(),
            None,
            StackType::File,
        ));

        for statement in &statements {
            if let Some(err) = &self.err {
                return Err(err.clone());
            }
            let value = self.evaluate(&statement);
            if let Value::Error(e) = &value {
                if let Some(referr) = &e.ref_err {
                    self.raise_with_ref(&e.err_type, &e.err_msg, *referr.clone());
                } else {
                    self.raise(&e.err_type, &e.err_msg);
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

    #[inline]
    #[track_caller]
    fn get_location_from_current_statement(&self) -> Option<Location> {
        match &self.current_statement {
            Some(Statement { loc, .. }) => match loc {
                Some(loc) => Some(get_loc(*loc).set_lucia_source_loc(format!("{}:{}:{}", PanicLocation::caller().file(), PanicLocation::caller().line(), PanicLocation::caller().column()))),
                None => None,
            },
            _ => None,
        }
    }

    #[inline]
    #[track_caller]
    fn get_location_from_statement_before(&self) -> Option<Location> {
        match &self.statement_before {
            Some(Statement { loc, .. }) => match loc {
                Some(loc) => Some(get_loc(*loc).set_lucia_source_loc(format!("{}:{}:{}", PanicLocation::caller().file(), PanicLocation::caller().line(), PanicLocation::caller().column()))),
                None => None,
            },
            _ => None,
        }
    }

    #[inline]
    fn get_location_from_current_statement_id(&self) -> Option<usize> {
        match &self.current_statement {
            Some(Statement { loc, .. }) => match loc {
                Some(loc) => Some(*loc),
                None => None,
            },
            _ => None,
        }
    }

    #[inline]
    fn get_location_from_current_statement_caller(&self, caller: PanicLocation) -> Option<Location> {
        match &self.current_statement {
            Some(Statement { loc, .. }) => match loc {
                Some(loc) => Some(get_loc(*loc).set_lucia_source_loc(format!("{}:{}:{}", caller.file(), caller.line(), caller.column()))),
                None => None,
            },
            _ => None,
        }
    }

    #[track_caller]
    pub fn raise(&mut self, error_type: &str, msg: &str) -> Value {
        let rust_loc = PanicLocation::caller();
        let loc = self.get_location_from_current_statement_caller(*rust_loc).unwrap_or_else(|| self.get_location_from_statement_before().unwrap_or_else(|| Location {
            file: fix_path(self.file_path.clone()),
            line_string: "".to_owned(),
            line_number: 0,
            range: (0, 0),
            lucia_source_loc: format!("{}:{}:{}", rust_loc.file(), rust_loc.line(), rust_loc.column()),
        }));

        self.err = Some(Error {
            err_type: error_type.to_string(),
            err_msg: msg.to_string(),
            help: None,
            loc: Some(loc),
            ref_err: None,
        });

        NULL
    }

    #[track_caller]
    pub fn warn(&mut self, warning_str: &str) -> Value {
        let rust_loc = PanicLocation::caller();
        let loc = self.get_location_from_current_statement_caller(*rust_loc).unwrap_or_else(|| self.get_location_from_statement_before().unwrap_or_else(|| Location {
            file: fix_path(self.file_path.clone()),
            line_string: "".to_owned(),
            line_number: 0,
            range: (0, 0),
            lucia_source_loc: format!("{}:{}:{}", rust_loc.file(), rust_loc.line(), rust_loc.column()),
        }));
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
    pub fn raise_err(&mut self, mut error: Error) -> Value {
        let rust_loc = PanicLocation::caller();
        let loc = self.get_location_from_current_statement_caller(*rust_loc).unwrap_or_else(|| self.get_location_from_statement_before().unwrap_or_else(|| Location {
            file: self.file_path.clone(),
            line_string: "".to_owned(),
            line_number: 0,
            range: (0, 0),
            lucia_source_loc: format!("{}:{}:{}", rust_loc.file(), rust_loc.line(), rust_loc.column()),
        }));

        error.loc = Some(loc);

        self.err = Some(error);
        NULL
    }

    #[track_caller]
    pub fn raise_with_help(&mut self, error_type: &str, msg: &str, help: &str) -> Value {
        let rust_loc = PanicLocation::caller();
        let loc = self.get_location_from_current_statement_caller(*rust_loc).unwrap_or_else(|| self.get_location_from_statement_before().unwrap_or_else(|| Location {
            file: self.file_path.clone(),
            line_string: "".to_owned(),
            line_number: 0,
            range: (0, 0),
            lucia_source_loc: format!("{}:{}:{}", rust_loc.file(), rust_loc.line(), rust_loc.column()),
        }));

        self.err = Some(Error {
            err_type: error_type.to_string(),
            err_msg: msg.to_string(),
            help: Some(help.to_string()),
            loc: Some(loc),
            ref_err: None,
        });

        NULL
    }

    #[track_caller]
    pub fn raise_with_ref(&mut self, error_type: &str, msg: &str, ref_err: Error) -> Value {
        let rust_loc = PanicLocation::caller();
        let loc = self.get_location_from_current_statement_caller(*rust_loc).unwrap_or_else(|| self.get_location_from_statement_before().unwrap_or_else(|| Location {
            file: self.file_path.clone(),
            line_string: "".to_owned(),
            line_number: 0,
            range: (0, 0),
            lucia_source_loc: format!("{}:{}:{}", rust_loc.file(), rust_loc.line(), rust_loc.column()),
        }));

        self.err = Some(Error {
            err_type: error_type.to_string(),
            err_msg: msg.to_string(),
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

        self.statement_before = self.current_statement.clone();
        self.current_statement = Some(statement.clone());

        if self.check_stop_flag() {
            return NULL;
        }

        self.variables.entry("_".to_owned()).or_insert_with(|| {
            Variable::new("_".to_owned(), NULL.clone(), "any".to_owned(), true, false, false)
        });
    
        self.variables.entry("_err".to_owned()).or_insert_with(|| {
            Variable::new(
                "_err".to_owned(),
                Value::Tuple(vec![]),
                "tuple".to_owned(),
                true,
                false,
                true,
            )
        });
    
        if self.err.is_some() || self.check_stop_flag() {
            return NULL;
        }
    
        if self.state == State::Break || self.state == State::Continue {
            return NULL;
        }
    
        let result = match statement.node.clone() {
            Node::If { condition, body, else_body } => self.handle_if(*condition, &body, else_body.as_deref()),
            Node::For { node, body } => self.handle_for_loop(node, &body),
            Node::While { condition, body } => self.handle_while(*condition, &body),
            Node::TryCatch { body, catch_body, exception_vars } => self.handle_try(body, catch_body, exception_vars),
            Node::Throw { node } => self.handle_throw(*node),
            Node::Forget { node } => self.handle_forget(*node),
            Node::Continue | Node::Break => self.handle_continue_and_break(&statement.node),
            Node::Defer { body } => self.handle_defer(body),
            Node::Scope { body, name, locals, is_local } => self.handle_scope(body, name, locals, is_local),
            Node::Match { condition, cases } => self.handle_match(*condition, &cases),
            Node::Group { body } => self.handle_group(body),

            Node::FunctionDeclaration { name, args, body, modifiers, return_type, effect_flags, docs } => 
                self.handle_function_declaration(name, args, body, modifiers, *return_type, effect_flags, docs, false),
            Node::GeneratorDeclaration { name, args, body, modifiers, return_type, effect_flags } => 
                self.handle_generator_declaration(name, args, body, modifiers, *return_type, effect_flags),
            Node::Return { value } => self.handle_return(*value),

            Node::Import { name, alias, named_imports, modifiers, import_all, module_path_opt } => self.handle_import(name, alias, named_imports, modifiers, import_all, module_path_opt.map(|b| *b)),
            Node::Export { names, aliases, modifiers_list } => self.handle_export(names, aliases, modifiers_list),

            Node::VariableDeclaration { name, val_stmt, var_type, modifiers, is_default: _ } => self.handle_variable_declaration(name, *val_stmt, *var_type, modifiers),
            Node::Variable { name } => self.handle_variable(&name),
            Node::Assignment { left, right } => self.handle_assignment(*left, *right),
            Node::UnpackAssignment { targets, stmt } => self.handle_unpack_assignment(targets, *stmt),

            Node::Number { value } => self.handle_number(value),
            Node::String { value, mods } => self.handle_string(value, mods),
            Node::Boolean { value } => self.handle_boolean(value),

            Node::Map { keys_stmts, values_stmts } => self.handle_map(keys_stmts, values_stmts),
            Node::Iterable { node } => self.handle_iterable(node),
            Node::Value { value } => self.handle_value(value),

            Node::Operation { left, operator, right } => self.handle_operation(*left, operator, *right),
            Node::UnaryOperation { operator, operand } => self.handle_unary_op(*operand, &operator),
            Node::PrefixOperation { operator, operand } => self.handle_prefix_op(*operand, &operator),
            Node::PostfixOperation { operator, operand } => self.handle_postfix_op(*operand, &operator),
            Node::Pipeline { initial_value, arguments } => self.handle_pipeline(*initial_value, arguments),

            Node::Call { name, pos_args, named_args } => self.handle_call(&name, pos_args, named_args),
            Node::MethodCall { object, method_name, pos_args, named_args } => self.handle_method_call(*object, &method_name, pos_args, named_args),
            Node::PropertyAccess { object, property_name } => self.handle_property_access(*object, &property_name),
            Node::IndexAccess { object, access } => self.handle_index_access(*object, *access),
            
            Node::Type { node} => self.handle_type(node),
            Node::TypeConvert { node, target_type } => self.handle_type_conversion(*node, *target_type),
            Node::TypeDeclaration { name, modifiers, node } => self.handle_type_declaration(name, modifiers, node),

            Node::StructCreation { name, fields_ast, is_null } => self.handle_struct_creation(name, fields_ast, is_null),
            Node::StructMethods { struct_name, methods_ast } => self.handle_struct_methods(struct_name, methods_ast),
            Node::Pointer { ptr_node, value } => self.handle_pointer(ptr_node.clone(), *value),

            Node::Null => return NULL,
            // t => self.raise("NotImplemented", &format!("Unsupported statement type: {}", t.name())),
        };

        if self.check_stop_flag() {
            return NULL;
        }
    
        if let Some(err) = self.err.clone() {
            let mut tuple_vec = vec![
                Value::String(err.err_type),
                Value::String(err.err_msg),
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

        if result != NULL {
            if let Some(var) = self.variables.get_mut("_") {
                var.set_value(result.clone());
            }
        }
    
        result
    }

    #[inline]
    fn run_function_body_fast(
        &mut self,
        func_args: FxHashMap<String, Variable>,
        body: &[Statement],
        func_ptr: *const (),
        func_meta: &FunctionMetadata,
    ) -> (Value, Option<Error>) {
        let saved_vars = std::mem::replace(&mut self.variables, func_args);
        let saved_is_returning = self.is_returning;
        let saved_return_value = std::mem::replace(&mut self.return_value, NULL);
        let saved_last_called = self.cache.last_called_function.take();
        let saved_state = std::mem::replace(&mut self.state, State::Normal);

        self.is_returning = false;
        
        let last_idx = body.len().saturating_sub(1);
        let return_value;
        
        'tco: loop {
            let mut last_value = NULL;
            
            for (i, stmt) in body.iter().enumerate() {
                if i == last_idx {
                    let call = match &stmt.node {
                        Node::Call { name, pos_args, named_args, .. } => {
                            Some((name, pos_args, named_args))
                        }
                        Node::Return { value } => {
                            if let Node::Call { ref name, ref pos_args, ref named_args, .. } = (&**value).node {
                                Some((name, pos_args, named_args))
                            } else {
                                None
                            }
                        }
                        _ => None,
                    };
                    
                    if let Some((name, pos_args, named_args)) = call {
                        if let Some(var) = self.variables.get(name) {
                            if let Value::Function(f) = var.get_value() {
                                if f.ptr() == func_ptr {
                                    let mut evaluated_pos_args = Vec::with_capacity(pos_args.len());
                                    for arg in pos_args {
                                        let val = self.evaluate(arg);
                                        if self.err.is_some() {
                                            break;
                                        }
                                        evaluated_pos_args.push(val);
                                    }
                                    
                                    if self.err.is_none() {
                                        let mut new_vars = self.variables.clone();
                                        for (idx, param) in func_meta.parameters.iter().enumerate() {
                                            if let Some(val) = evaluated_pos_args.get(idx) {
                                                new_vars.insert(
                                                    param.name.clone(),
                                                    Variable::new(param.name.clone(), val.clone(), "any".to_string(), false, false, false),
                                                );
                                            }
                                        }
                                        for (key, arg) in named_args {
                                            let val = self.evaluate(arg);
                                            if self.err.is_some() {
                                                break;
                                            }
                                            new_vars.insert(
                                                key.clone(),
                                                Variable::new(key.clone(), val.clone(), "any".to_string(), false, false, false),
                                            );
                                        }
                                        if self.err.is_none() {
                                            self.variables = new_vars;
                                            self.is_returning = false;
                                            self.cache.last_called_function = None;
                                            continue 'tco;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                
                last_value = self.evaluate(stmt);
                if self.err.is_some() || self.is_returning {
                    break;
                }
                
                if let State::TCO(ref tco_stmt) = self.state.clone() {
                    if let Node::Call { ref name, ref pos_args, ref named_args, .. } = tco_stmt.node {
                        if let Some(var) = self.variables.get(name) {
                            if let Value::Function(f) = var.get_value() {
                                if f.ptr() == func_ptr {
                                    let mut evaluated_pos_args = Vec::with_capacity(pos_args.len());
                                    for arg in pos_args {
                                        let val = self.evaluate(arg);
                                        if self.err.is_some() {
                                            break;
                                        }
                                        evaluated_pos_args.push(val);
                                    }
                                    
                                    if self.err.is_none() {
                                        let mut new_vars = self.variables.clone();
                                        for (idx, param) in func_meta.parameters.iter().enumerate() {
                                            if let Some(val) = evaluated_pos_args.get(idx) {
                                                new_vars.insert(
                                                    param.name.clone(),
                                                    Variable::new(param.name.clone(), val.clone(), "any".to_string(), false, false, false),
                                                );
                                            }
                                        }
                                        for (key, arg) in named_args {
                                            let val = self.evaluate(arg);
                                            if self.err.is_some() {
                                                break;
                                            }
                                            new_vars.insert(
                                                key.clone(),
                                                Variable::new(key.clone(), val.clone(), "any".to_string(), false, false, false),
                                            );
                                        }
                                        if self.err.is_none() {
                                            self.variables = new_vars;
                                            self.state = State::Normal;
                                            self.is_returning = false;
                                            self.cache.last_called_function = None;
                                            continue 'tco;
                                        }
                                    }
                                }
                            }
                        }
                    }
                    break;
                }
            }
            
            return_value = if self.is_returning {
                std::mem::replace(&mut self.return_value, saved_return_value)
            } else if self.err.is_none() {
                self.return_value = saved_return_value;
                last_value
            } else {
                self.return_value = saved_return_value;
                NULL
            };
            break;
        }

        let err = self.err.take();
        
        self.variables = saved_vars;
        self.is_returning = saved_is_returning;
        self.cache.last_called_function = saved_last_called;
        self.state = saved_state;

        (return_value, err)
    }

    #[track_caller]
    fn run_isolated(&mut self, temp_vars: FxHashMap<String, Variable>, body: &[Statement], keep_stuff: bool) -> (Value, State, Option<Error>, FxHashMap<String, Variable>) {
        let saved_vars = std::mem::replace(&mut self.variables, temp_vars);
        let saved_stack = std::mem::replace(&mut self.stack, Stack::new());
        let saved_state = std::mem::replace(&mut self.state, State::Normal);
        let saved_err = self.err.clone();
        let saved_defer = std::mem::replace(&mut self.defer_stack, Vec::new());
        let saved_is_returning = self.is_returning;
        let saved_return_value = self.return_value.clone();
        let saved_current_statement = std::mem::replace(&mut self.current_statement, None);
        let saved_statement_before = std::mem::replace(&mut self.statement_before, None);

        self.err = None;
        self.is_returning = false;
        self.return_value = NULL;

        let mut res = NULL;
        for stmt in body {
            res = self.evaluate(stmt);
            if self.err.is_some() {
                break;
            }
            if matches!(self.state, State::Break | State::Continue) {
                break;
            }
        }

        let err = self.err.clone();
        let end_state = self.state.clone();
        let resulting_vars = std::mem::replace(&mut self.variables, saved_vars);

        self.stack = saved_stack;
        self.state = saved_state;
        self.err = saved_err; 
        if !keep_stuff {
            self.return_value = saved_return_value;
            self.defer_stack = saved_defer;
            self.is_returning = saved_is_returning;
        }
        self.current_statement = saved_current_statement;
        self.statement_before = saved_statement_before;

        (res, end_state, err, resulting_vars)
    }

    fn handle_prefix_op(&mut self, operand_stmt: Statement, operator: &str) -> Value {
        let operand_val = self.evaluate(&operand_stmt);
        if self.err.is_some() {
            return NULL;
        }

        let mut is_var = false;
        let mut var_name = "";

        if let Node::Variable { name, .. } = &operand_stmt.node {
            is_var = true;
            var_name = name;
        }

        let mut operand_val_clone: Option<Value> = None;
        if self.config.debug {
            operand_val_clone = Some(operand_val.clone());
        }

        let result = (|| match &operator {
            &"++" => {
                if is_var {
                    if let Some(var) = self.variables.get_mut(var_name) {
                        match &var.value {
                            Value::Int(i) => {
                                let new = Value::Int((i + Int::from_i64(1)).expect("int overflow"));
                                var.set_value(new.clone());
                                return new;
                            }
                            Value::Float(f) => {
                                let new = Value::Float((f + Float::from_f64(1.0)).expect("float overflow"));
                                var.set_value(new.clone());
                                return new;
                            }
                            Value::Pointer(_) => {
                                let mut p = var.value.clone();
                                while let Value::Pointer(inner) = p {
                                    p = inner.lock().0.clone();
                                }
                                return p;
                            }
                            _ => {
                                return self.raise(
                                    "TypeError",
                                    "Prefix ++ can only be applied to integers and floats"
                                );
                            }
                        }
                    }
                    return self.raise(
                        "NameError",
                        &format!("Variable '{}' not found for prefix ++", var_name)
                    );
                }

                match operand_val {
                    Value::Int(i) => Value::Int(i.abs()),
                    Value::Float(f) => Value::Float(f.abs()),
                    mut p @ Value::Pointer(_) => {
                        while let Value::Pointer(inner) = p {
                            p = inner.lock().0.clone();
                        }
                        p
                    }
                    _ => self.raise(
                        "TypeError",
                        "Prefix ++ can only be applied to integers, floats and pointers"
                    ),
                }
            }

            &"--" => {
                if is_var {
                    if let Some(var) = self.variables.get_mut(var_name) {
                        let old = var.value.clone();
                        match &old {
                            Value::Int(i) => {
                                let new = Value::Int((i - Int::from_i64(1)).expect("int underflow"));
                                var.set_value(new);
                                return old;
                            }
                            Value::Float(f) => {
                                let new = Value::Float((f - Float::from_f64(1.0)).expect("float underflow"));
                                var.set_value(new);
                                return old;
                            }
                            _ => {
                                return self.raise(
                                    "TypeError",
                                    "Prefix -- can only be applied to integers and floats"
                                );
                            }
                        }
                    }
                    return self.raise(
                        "NameError",
                        &format!("Variable '{}' not found for prefix --", var_name)
                    );
                }

                match operand_val {
                    Value::Int(i) => Value::Int(-i),
                    Value::Float(f) => Value::Float(-f),
                    _ => self.raise(
                        "TypeError",
                        "Prefix -- can only be applied to integers and floats"
                    ),
                }
            }

            _ => self.raise("SyntaxError", &format!("Unknown prefix operator '{}'", operator)),
        })();
        self.debug_log(
            format_args!(
                "<{}{} -> {}>",
                operator,
                format_value(&operand_val_clone.unwrap_or(NULL)),
                format_value(&result)
            )
        );
        result
    }

    fn handle_postfix_op(&mut self, operand_stmt: Statement, operator: &str) -> Value {
        let operand_val = self.evaluate(&operand_stmt);
        if self.err.is_some() {
            return NULL;
        }

        let mut is_var = false;
        let mut var_name = "";

        if let Node::Variable { name, .. } = &operand_stmt.node {
            is_var = true;
            var_name = name;
        }

        let mut operand_val_clone: Option<Value> = None;
        if self.config.debug {
            operand_val_clone = Some(operand_val.clone());
        }

        let result = (|| match &operator {
            &"++" => {
                if is_var {
                    if let Some(var) = self.variables.get_mut(var_name) {
                        let old = var.value.clone();
                        match &old {
                            Value::Int(i) => {
                                let new = Value::Int((i + Int::from_i64(1)).expect("int overflow"));
                                var.set_value(new);
                                return old;
                            }
                            Value::Float(f) => {
                                let new = Value::Float((f + Float::from_f64(1.0)).expect("float overflow"));
                                var.set_value(new);
                                return old;
                            }
                            _ => {
                                return self.raise(
                                    "TypeError",
                                    "Prefix ++ can only be applied to integers and floats"
                                );
                            }
                        }
                    }
                    return self.raise(
                        "NameError",
                        &format!("Variable '{}' not found for prefix ++", var_name)
                    );
                }

                match operand_val {
                    Value::Int(i) => Value::Int(i.abs()),
                    Value::Float(f) => Value::Float(f.abs()),
                    _ => self.raise(
                        "TypeError",
                        "Prefix ++ can only be applied to integers, floats and pointers"
                    ),
                }
            }

            &"--" => {
                if is_var {
                    if let Some(var) = self.variables.get_mut(var_name) {
                        let old = var.value.clone();
                        match &old {
                            Value::Int(i) => {
                                let new = Value::Int((i - Int::from_i64(1)).expect("int underflow"));
                                var.set_value(new);
                                return old;
                            }
                            Value::Float(f) => {
                                let new = Value::Float((f - Float::from_f64(1.0)).expect("float underflow"));
                                var.set_value(new);
                                return old;
                            }
                            _ => {
                                return self.raise(
                                    "TypeError",
                                    "Prefix -- can only be applied to integers and floats"
                                );
                            }
                        }
                    }
                    return self.raise(
                        "NameError",
                        &format!("Variable '{}' not found for prefix --", var_name)
                    );
                }

                match operand_val {
                    Value::Int(i) => Value::Int(-i),
                    Value::Float(f) => Value::Float(-f),
                    _ => self.raise(
                        "TypeError",
                        "Prefix -- can only be applied to integers and floats"
                    ),
                }
            }

            _ => self.raise("SyntaxError", &format!("Unknown prefix operator '{}'", operator)),
        })();
        self.debug_log(
            format_args!(
                "<{}{} -> {}>",
                format_value(&operand_val_clone.unwrap_or(NULL)),
                operator,
                format_value(&result)
            )
        );
        result
    }

    fn handle_pipeline(&mut self, initial_value: Statement, arguments: Vec<Statement>) -> Value {
        let mut current_val = self.evaluate(&initial_value);
        if self.err.is_some() {
            return NULL;
        }

        for step_val in &arguments {
            if self.err.is_some() {
                return NULL;
            }

            let wrapped_stmt = Statement {
                node: Node::Value {
                    value: current_val.clone(),
                },
                loc: alloc_loc(self.get_location_from_current_statement()),
            };

            match &step_val.node {
                Node::Call { name, pos_args, named_args } => {
                    let mut new_pos = Vec::with_capacity(pos_args.len() + 1);
                    let mut replaced = false;

                    for arg in pos_args.iter() {
                        if let Node::Variable { name: var_name } = &arg.node {
                            if var_name == "_" {
                                new_pos.push(wrapped_stmt.clone());
                                replaced = true;
                                continue;
                            }
                        }
                        new_pos.push(arg.clone());
                    }

                    if !replaced {
                        new_pos.insert(0, wrapped_stmt.clone());
                    }

                    let mut new_named = HashMap::with_capacity(named_args.len());
                    for (k, v) in named_args.iter() {
                        if let Node::Variable { name: var_name } = &v.node {
                            if var_name == "_" {
                                new_named.insert(k.clone(), wrapped_stmt.clone());
                                continue;
                            }
                        }
                        new_named.insert(k.clone(), v.clone());
                    }

                    current_val = self.evaluate(&Statement {
                        node: Node::Call {
                            name: name.clone(),
                            pos_args: new_pos,
                            named_args: new_named,
                        },
                        loc: step_val.loc,
                    });
                }

                Node::MethodCall { object, method_name, pos_args, named_args } => {
                    let mut new_pos = Vec::with_capacity(pos_args.len() + 1);
                    let mut replaced = false;

                    for arg in pos_args.iter() {
                        if let Node::Variable { name: var_name } = &arg.node {
                            if var_name == "_" {
                                new_pos.push(wrapped_stmt.clone());
                                replaced = true;
                                continue;
                            }
                        }
                        new_pos.push(arg.clone());
                    }

                    if !replaced {
                        new_pos.insert(0, wrapped_stmt.clone());
                    }

                    let mut new_named = HashMap::with_capacity(named_args.len());
                    for (k, v) in named_args.iter() {
                        if let Node::Variable { name: var_name } = &v.node {
                            if var_name == "_" {
                                new_named.insert(k.clone(), wrapped_stmt.clone());
                                continue;
                            }
                        }
                        new_named.insert(k.clone(), v.clone());
                    }

                    current_val = self.evaluate(&Statement {
                        node: Node::MethodCall {
                            object: object.clone(),
                            method_name: method_name.clone(),
                            pos_args: new_pos,
                            named_args: new_named,
                        },
                        loc: step_val.loc,
                    });
                }

                _ => {
                    self.variables.insert(
                        "_".to_owned(),
                        Variable::new(
                            "_".to_owned(),
                            current_val.clone(),
                            "any".to_owned(),
                            true,
                            false,
                            false
                        ),
                    );
                    current_val = self.evaluate(step_val);
                }
            }

            if self.err.is_some() {
                return NULL;
            }
        }

        current_val
    }

    fn handle_value(&mut self, value: Value) -> Value {
        if let Value::Error(e) = &value {
            if let Some(ref_err) = &e.ref_err {
                self.raise_with_ref(&e.err_type, &e.err_msg, *ref_err.clone());
            } else {
                self.raise(&e.err_type, &e.err_msg);
            }
            return NULL;
        }
        value
    }

    fn handle_struct_methods(&mut self, struct_name: String, methods_ast: Vec<Statement>) -> Value {
        let mut struct_variable = match self.variables.get(&struct_name) {
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
        let mut methods: HashMap<String, Value> = HashMap::with_capacity(methods_ast.len());
        self.variables.extend::<HashMap<String, Variable>>(HashMap::from_iter(vec![
            ("Self".to_owned(), Variable::new(struct_name.clone(), struct_value.clone(), "any".to_owned(), false, true, false)),
        ]));
        for generic in if let Value::Type(Type::Struct { generics, .. }) = &struct_value {
            generics
        } else {
            to_static(vec![])
        } {
            self.variables.insert(generic.clone(), Variable::new(generic.clone(), Value::Type(Type::new_simple("any")), "type".to_owned(), false, true, false));
        }
        for method in methods_ast {
            let mut m = method.node;
            let mut func;
            if let Node::FunctionDeclaration { name, args: a, body, modifiers, return_type, effect_flags, docs } = &mut m {
                modifiers.insert(0, "final".to_owned());
                func = self.handle_function_declaration(name.to_owned(), a.to_vec(), body.to_vec(), modifiers.to_vec(), (**return_type).clone(), *effect_flags, docs.clone(), true);
                if let Value::Function(f) = &mut func {
                    f.promote_to_method(Arc::new(Mutex::new(self.clone())));
                }
                if self.err.is_some() {
                    self.variables = saved_variables;
                    return NULL;
                }
            } else {
                return self.raise("TypeError", "Expected a function declaration in struct methods");
            }
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
                return self.raise("TypeError", &format!("Expected a function in struct methods, got '{}'", func.get_type().display_simple()));
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

    fn handle_struct_creation(&mut self, struct_name: String, fields_ast: HashMap<String, Statement>, is_null: bool) -> Value {
        let fields: HashMap<String, (Statement, Type)> = fields_ast.iter().map(|(k, v)| {
            (k.clone(), (v.clone(), Type::new_simple("any")))
        }).collect();

        let mut new_fields = HashMap::with_capacity(fields.len());
        for (k, (v, ty)) in &fields {
            let val = self.evaluate(&v);
            if self.err.is_some() {
                return NULL;
            }
            new_fields.insert(k.clone(), (Box::new(val), ty.clone()));
        }
        let fields = new_fields;

        let struct_ty = match self.variables.get(&struct_name) {
            Some(Variable { value: Value::Type(ty), .. }) => {
                let unaliased_ty = unalias_type(ty);
                match unaliased_ty {
                    Type::Struct { .. } => unaliased_ty.clone(),
                    _ => return self.raise(
                        "TypeError",
                        &format!("'{}' is not a struct type. ({})", struct_name, ty.display_simple()),
                    ),
                }
            }
            Some(v) => {
                return self.raise(
                    "TypeError",
                    &format!("'{}' is not a struct type. ({})", struct_name, v.get_type().display_simple()),
                )
            }
            None => return self.raise("TypeError", &format!("Struct '{}' does not exist", struct_name)),
        };

        if is_null {
            Value::Struct(Box::new(Struct::new_as_null(struct_ty)))
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
                    self.variables.insert(generic.clone(), Variable::new(generic.clone(), Value::Type(Type::new_simple("any")), "type".to_owned(), true, false, true));
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

            Value::Struct(Box::new(Struct::new_with_fields(struct_ty, final_fields)))
        }
    }

    fn handle_export(&mut self, names: Vec<String>, aliases: Vec<String>, modifiers_list: Vec<Vec<String>>) -> Value {
        if names.len() != aliases.len() || names.len() != modifiers_list.len() {
            return self.raise("RuntimeError", "Mismatched lengths of names, aliases, or modifiers");
        }

        let mut exported_values = vec![];

        for i in 0..names.len() {
            let name = &names[i];
            let alias = &aliases[i];
            let modifiers = &modifiers_list[i];

            let mut is_public = false;
            let mut is_static = None;
            let mut is_final = None;

            for modifier in modifiers {
                match modifier.as_str() {
                    "public" => is_public = true,
                    "private" => is_public = false,
                    "static" => is_static = Some(true),
                    "final" => is_final = Some(true),
                    "non-static" => is_static = Some(false),
                    "mutable" => is_final = Some(false),
                    _ => return self.raise("ModifierError", &format!("Unknown modifier: {}", modifier)),
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

    fn handle_group(&mut self, body: Vec<Statement>) -> Value {
        let mut result = Value::Null;
        for expr in body {
            result = self.evaluate(&expr);
        }
        if self.is_returning {
            result = self.return_value.clone();
            self.is_returning = false;
        }
        result
    }

    fn handle_generator_declaration(
        &mut self,
        name: String,
        args: Vec<ParamAST>,
        body: Vec<Statement>, 
        modifiers: Vec<String>, 
        return_type: Statement,
        effect_flags: EffectFlags,
    ) -> Value {
        if self.err.is_some() {
            return NULL;
        }

        let mut is_public = false;
        let mut is_static = false;
        let mut is_final = false;

        for modifier in modifiers {
            match modifier.as_str() {
                "public" => is_public = true,
                "static" => is_static = true,
                "final" => is_final = true,
                "private" => is_public = false,
                "non-static" => is_static = false,
                "mutable" => is_final = false,
                _ => return self.raise("SyntaxError", &format!("Unknown generator modifier: {}", modifier)),
            }
        }

        
        let mut parameters = Vec::with_capacity(args.len());

        for arg in args {
            let type_value = match arg.ty {
                Some(ty_stmt) => match self.evaluate(&ty_stmt) {
                    Value::Type(t) => t,
                    _ => return self.raise("RuntimeError", "Invalid 'type' in function parameter"),
                },
                None => Type::new_simple("any"),
            };
            if self.err.is_some() {
                return NULL;
            }

            if arg.is_variadic {
                parameters.push(Parameter::variadic_optional(
                    arg.name.as_str(),
                    "any",
                    Value::List(vec![]),
                ).set_mods(arg.modifiers.clone()));
            } else if arg.default.is_some() {
                let default_value = self.evaluate(&arg.default.unwrap());
                if self.err.is_some() {
                    return NULL;
                }
                parameters.push(Parameter::positional_optional_pt(
                    arg.name.as_str(),
                    &type_value,
                    default_value,
                ).set_mods(arg.modifiers.clone()));
            } else {
                parameters.push(Parameter::positional_pt(arg.name.as_str(), &type_value).set_mods(arg.modifiers.clone()));
            }
        }

        let body_formatted: Arc<Vec<Statement>> = Arc::new(body);
        let ret_type = Arc::new(match self.evaluate(&return_type) {
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

        if self.variables.contains_key(&name) && !name.starts_with("<") {
            if let Some(var) = self.variables.get(&name) {
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
                    &name,
                    generate_gen,
                    parameters,
                    "generator",
                    is_public,
                    is_static,
                    is_final,
                    None,
                    effect_flags,
                    ""
                )))),
                "generator".to_owned(),
                is_static,
                is_public,
                is_final,
            ),
        );
        self.variables.get(&name)
            .map_or(NULL, |var| var.value.clone())
    }

    fn handle_type_declaration(&mut self, name: String, modifiers: Vec<String>, type_decl: TypeDeclNode) -> Value {
        let mut is_public = false;
        let mut is_static = false;
        let mut is_final = false;

        for modifier in modifiers {
            match modifier.as_str() {
                "public" => is_public = true,
                "static" => is_static = true,
                "final" => is_final = true,
                "private" => is_public = false,
                "non-static" => is_static = false,
                "mutable" => is_final = false,
                _ => return self.raise("ModifierError", &format!("Unknown type modifier: {}", modifier)),
            }
        }

        match type_decl {
            TypeDeclNode::Alias { conditions, variables, base_type: bs_ty } => {
                let base_type = match self.evaluate(&bs_ty) {
                    Value::Type(t) => t,
                    _ => {
                        if self.err.is_some() {
                            return self.raise_with_ref("TypeError", "Expected base type to be a type", self.err.clone().unwrap());
                        }
                        self.raise("TypeError", "Expected base type to be a type");
                        return NULL;
                    }
                };
                if self.err.is_some() {
                    return NULL;
                }
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
                        "type".to_owned(),
                        is_static,
                        is_public,
                        is_final,
                    ),
                );
                self.variables.get(&name)
                    .map_or(NULL, |var| var.value.clone())
            }
            TypeDeclNode::Enum { variants, wheres, generics } => {
                let eval_wheres: Vec<(String, Value)> = wheres.iter().map(|(k, v)| {
                    let val = self.evaluate(&v);
                    if self.err.is_some() {
                        return ("".to_owned(), NULL);
                    }
                    (k.clone(), val)
                }).filter(|(k, _)| !k.is_empty()).collect();
                if self.err.is_some() {
                    return NULL;
                }
                let enum_type = Value::Type(Type::Enum {
                    name: name.clone(),
                    variants,
                    generics,
                    wheres: eval_wheres,
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
                        "type".to_owned(),
                        is_static,
                        is_public,
                        is_final,
                    ),
                );
                self.debug_log(format_args!("<Enum '{}' registered>", name));
                self.variables.get(&name)
                    .map_or(NULL, |var| var.value.clone())
            }
            TypeDeclNode::Struct { fields, wheres, generics } => {
                let eval_wheres: Vec<(String, Value)> = wheres.iter().map(|(k, v)| {
                    let val = self.evaluate(&v);
                    if self.err.is_some() {
                        return ("".to_owned(), NULL);
                    }
                    (k.clone(), val)
                }).filter(|(k, _)| !k.is_empty()).collect();
                if self.err.is_some() {
                    return NULL;
                }
                let struct_type = Value::Type(Type::Struct {
                    name: name.clone(),
                    fields,
                    methods: vec![],
                    generics,
                    wheres: eval_wheres,
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
                        "type".to_owned(),
                        is_static,
                        is_public,
                        is_final,
                    ),
                );
                self.debug_log(format_args!("<Struct '{}' registered>", name));
                self.variables.get(&name)
                    .map_or(NULL, |var| var.value.clone())
            }
        }
    }

    fn handle_match(&mut self, condition: Statement, cases: &[MatchCase]) -> Value {
        if self.err.is_some() {
            return NULL;
        }

        let cond_val = self.evaluate(&condition);
        if self.err.is_some() {
            return NULL;
        }

        for case in cases {
            match case {
                MatchCase::Pattern { pattern, body, guard } => {
                    let (matched, variables) = match check_pattern(&cond_val, &pattern) {
                        Ok(m) => m,
                        Err((etype, emsg)) => return self.raise(&etype, &emsg),
                    };

                    if !matched {
                        continue;
                    }

                    let mut temp_vars = self.variables.clone();
                    for (k, v) in variables {
                        temp_vars.insert(k.clone(), Variable::new(k, v.clone(), v.type_name().to_string(), false, true, true));
                    }

                    if let Some(g) = guard {
                        let (guard_res, _guard_state, guard_err, _guard_vars) = self.run_isolated(temp_vars.clone(), std::slice::from_ref(g), false);
                        if guard_err.is_some() {
                            self.err = guard_err;
                            return NULL;
                        }
                        if !guard_res.is_truthy() {
                            continue;
                        }
                    }

                    let (res, state, err, resulting_vars) = self.run_isolated(temp_vars, body, true);
                    if err.is_some() {
                        self.err = err;
                        return NULL;
                    }

                    match state {
                        State::Continue => {
                            self.variables.extend(resulting_vars);
                        }
                        State::Break => {
                            self.variables.extend(resulting_vars);
                            return NULL;
                        }
                        _ => {
                            self.variables.extend(resulting_vars);
                            return res;
                        }
                    }
                }

                MatchCase::Literal { patterns, body } => {
                    let mut evaluated_patterns: Vec<Value> = Vec::with_capacity(patterns.len());
                    for p in patterns.iter() {
                        let v = self.evaluate(p);
                        if self.err.is_some() {
                            return NULL;
                        }
                        evaluated_patterns.push(v);
                    }

                    let matched = evaluated_patterns.iter().any(|pv| *pv == cond_val);

                    if matched {
                        let mut res = NULL;
                        let mut cont = false;

                        for stmt in body {
                            res = self.evaluate(stmt);
                            if self.err.is_some() {
                                return NULL;
                            }

                            match self.state {
                                State::Continue => {
                                    cont = true;
                                    self.state = State::Normal;
                                    break;
                                }
                                State::Break => {
                                    self.state = State::Normal;
                                    return NULL;
                                }
                                _ => {}
                            }
                        }

                        if !cont {
                            return res;
                        }
                    }
                }
            }
        }

        self.raise_with_help("ValueError", "No value matched cases in match statement", "Add a '_ -> ...' case")
    }

    fn handle_scope(&mut self, body: Vec<Statement>, name: Option<String>, locals: Vec<String>, is_local: bool) -> Value {
        let name = match name {
            Some(v) => v,
            None => self.scope.clone(),
        };

        if is_local {
            let mut result = NULL;
            for stmt in body.iter() {
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
            self.is_returning = false;
            return result;
        }

        let new_file_path = self.file_path.clone() + &format!("+scope.{}", name);

        self.stack.push((
            new_file_path.clone(),
            self.get_location_from_current_statement(),
            StackType::Scope,
        ));

        self.debug_log(
            format_args!("<Entering scope '{}'>", name),
        );

        let mut scope_interpreter = Interpreter::new(
            self.config.clone(),
            &new_file_path,
            &self.cwd.clone(),
            self.preprocessor_info.clone(),
            &[]
        );
        scope_interpreter.set_scope(&name.clone());

        for local in locals.iter() {
            if let Some(val) = self.variables.get(local.as_str()) {
                scope_interpreter.variables.insert(local.clone(), val.clone());
            } else {
                self.stack.pop();
                self.raise(
                    "NameError",
                    &format!("Variable '{}' is not defined in the outer scope", local),
                );
                return NULL;
            }
        }

        if body.is_empty() {
            self.stack.pop();
            self.debug_log(
                format_args!("<Exiting scope '{}'>", name),
            );
            return NULL;
        }

        let res = scope_interpreter.interpret(body, true);

        if let Some(err) = scope_interpreter.err {
            self.stack.pop();
            self.raise_with_ref(
                "RuntimeError",
                &format!("Error in scope '{}'", name),
                err,
            );
            return NULL;
        }

        self.debug_log(
            format_args!("<Exiting scope '{}'>", name),
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

    fn handle_defer(&mut self, body: Vec<Statement>) -> Value {
        if body.is_empty() {
            return NULL;
        }
    
        // yes thats all i was also suprised when refactoring
        self.defer_stack.push(body);
    
        NULL
    }

    fn handle_continue_and_break(&mut self, node: &Node) -> Value {
        match node {
            Node::Continue => {
                self.state = State::Continue;
            },
            Node::Break => {
                self.state = State::Break;
            },
            _ => {
                self.raise("SyntaxError", "Invalid control statement");
            }
        }
        return NULL;
    }

    #[inline]
    fn handle_while(&mut self, condition: Statement, body: &[Statement]) -> Value {
        loop {
            let cond_val = self.evaluate(&condition);
            if self.err.is_some() || !cond_val.is_truthy() {
                break;
            }

            for stmt in body {
                let result = self.evaluate(stmt);
                if self.err.is_some() {
                    return NULL;
                }
                if self.is_returning {
                    return result;
                }
                match self.state {
                    State::Break => { self.state = State::Normal; return NULL; },
                    State::Continue => { self.state = State::Normal; break; },
                    _ => {}
                }
            }
        }

        NULL
    }

    fn handle_pointer(&mut self, ptr_op_type: PtrNode, value_stmt: Statement) -> Value {
        // why? i have no idea
        if matches!(ptr_op_type, PtrNode::PointerAssign { .. }) && self.err.is_some() {
            self.err = None;
        }

        let value = self.evaluate(&value_stmt);

        if !self.config.allow_unsafe {
            return self.raise_with_help(
                "PermissionError",
                "Pointer operations are not allowed in this context",
                "Enable 'allow_unsafe' in the configuration to use pointers.",
            );
        }

        if self.err.is_some() {
            return Value::Null;
        }

        match ptr_op_type {
            PtrNode::PointerRef { ref_level } => {
                match value {
                    Value::Type(t) => Value::Type(t.make_reference(ref_level)),
                    Value::Pointer(ref ptr_arc) => {
                        let mut guard = ptr_arc.lock();
                        guard.1 += ref_level;
                        Value::Pointer(ptr_arc.clone())
                    }
                    _ => Value::Pointer(Arc::new(Mutex::new((value, ref_level)))),
                }
            }

            PtrNode::PointerDeref { deref_level } => {
                let mut current = value;
                let mut remaining = deref_level;

                while remaining > 0 {
                    let arc_clone = match current {
                        Value::Pointer(ref ptr_arc) => ptr_arc.clone(),
                        _ => {
                            self.raise("TypeError", "Expected a pointer reference for dereferencing");
                            return Value::Null;
                        }
                    };

                    let mut guard = arc_clone.lock();
                    let depth = guard.1;

                    if remaining > depth {
                        self.raise("TypeError", "Dereference depth exceeds pointer depth");
                        return Value::Null;
                    }

                    if remaining == depth {
                        return guard.0.clone();
                    }

                    guard.1 = depth - 1;
                    current = Value::Pointer(arc_clone.clone());

                    remaining -= 1;
                }

                current
            }

            PtrNode::PointerAssign { target: target_stmt, assign_level } => {
                let mut target = self.evaluate(&target_stmt);
                if self.err.is_some() {
                    return Value::Null;
                }

                let mut remaining = assign_level - 1;

                loop {
                    let arc_clone = match target {
                        Value::Pointer(ref ptr_arc) => ptr_arc.clone(),
                        _ => {
                            self.raise("TypeError", "Expected pointer reference for assignment");
                            return Value::Null;
                        }
                    };

                    let mut guard = arc_clone.lock();

                    if remaining > 1 {
                        remaining -= 1;
                        target = guard.0.clone();
                        drop(guard);
                    } else {
                        let old_count = guard.1;
                        *guard = (value.clone(), old_count);
                        drop(guard);
                        return Value::Pointer(arc_clone);
                    }
                }
            }
        }
    }

    fn handle_unpack_assignment(&mut self, targets: Vec<Statement>, stmt: Statement) -> Value {
        let value = self.evaluate(&stmt);

        if self.err.is_some() {
            return NULL;
        }

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
            Value::Map(map) => {
                if map.is_empty() {
                    self.raise("SyntaxError", "Unpack assignment target cannot be an empty map");
                    return NULL;
                }
                map.iter()
                    .map(|(k, v)| {
                        let mut single_map = FxHashMap::with_hasher(Default::default());
                        single_map.insert(k.clone(), v.clone());
                        Value::Map(single_map)
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
        
            match &target.node {
                Node::Variable { name } => {
                    if let Some(var) = self.variables.get_mut(name.as_str()) {
                        var.set_value(val);
                        result_values.push(var.value.clone());
                    } else {
                        self.raise("TypeError", &format!(
                            "Variable '{}' must be declared with a type in unpack assignment",
                            name
                        ));
                        return NULL;
                    }
                }
                Node::VariableDeclaration { var_type, name, val_stmt: _, modifiers, is_default: _ } => {
                    let type_ = match self.evaluate(&var_type) {
                        Value::Type(t) => t,
                        _ => {
                            self.raise("TypeError", "Expected a type in variable declaration for unpack assignment");
                            return NULL;
                        }
                    };
                    if self.err.is_some() {
                        return NULL;
                    }

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

                    let val_statement = Statement {
                        node: Node::Value { value: val.clone() },
                        loc: alloc_loc(self.get_location_from_current_statement()),
                    };

                    let _ = self.handle_variable_declaration(
                        name.clone(),
                        val_statement,
                        *var_type.clone(),
                        modifiers.to_vec(),
                    );
                    if self.err.is_some() {
                        return NULL;
                    }
                
                    result_values.push(val);
                }                        
                _ => {
                    self.raise("TypeError", "Unpack target must be of type VARIABLE or VARIABLE_DECLARATION");
                    return NULL;
                }
            }
        }
        
        if self.err.is_some() {
            return NULL;
        }

        Value::Tuple(result_values)
    }

    fn handle_map(&mut self, keys: Vec<Statement>, values: Vec<Statement>) -> Value {
        if keys.len() != values.len() {
            self.raise("RuntimeError", "Keys and values lists must have the same length");
            return NULL;
        }

        let mut map = FxHashMap::with_capacity_and_hasher(keys.len(), Default::default());

        for (key_stmt, value_stmt) in keys.into_iter().zip(values.into_iter()) {
            let key = self.evaluate(&key_stmt);
            if self.err.is_some() {
                return NULL;
            }

            let value = self.evaluate(&value_stmt);
            if self.err.is_some() {
                return NULL;
            }

            if map.contains_key(&key) {
                self.raise("SyntaxError", &format!("Duplicate key in map: {}", key));
                return NULL;
            }

            map.insert(key, value);
        }

        Value::Map(map)
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
                    let stmt = Statement {
                        node: Node::Number {
                            value: s.clone(),
                        },
                        loc: alloc_loc(self.get_location_from_current_statement()),
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
                        if let Some(Value::Error(e)) = v.iter().find(|item| matches!(item, Value::Error(..))) {
                            self.err = Some(match &e.ref_err {
                                Some(re) => Error::with_ref(e.err_type.as_str(), e.err_msg.as_str(), *re.clone(), &self.file_path),
                                None => Error::new(e.err_type.as_str(), e.err_msg.as_str(), &self.file_path),
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
                if let Value::Map(m) = value {
                    Value::Map(m.clone())
                } else if let Value::Module(obj) = value {
                    let mut new_map = FxHashMap::default();
                    for (name, var) in obj.get_properties().iter() {
                        new_map.insert(Value::String(name.clone()), var.value.clone());
                    }
                    Value::Map(new_map)
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
            "type" => {
                if let Value::String(s) = value {
                    let l = Lexer::new(&s, &self.file_path);
                    let tokens = l.tokenize();
                    let mut p = Parser::new(tokens);
                    let type_stmt = p.parse_type();
                    if p.err.is_some() {
                        self.raise_with_ref("SyntaxError", "Failed to parse type string", p.err.take().unwrap());
                        return NULL;
                    }
                    let type_value = self.evaluate(&type_stmt);
                    if self.err.is_some() {
                        return NULL;
                    }
                    if let Value::Type(t) = type_value {
                        Value::Type(t)
                    } else {
                        self.raise("TypeError", "Parsed value is not a type");
                        NULL
                    }
                } else {
                    self.raise("TypeError", &format!("Cannot convert '{}' to type", value.type_name()));
                    NULL
                }
            }
            _ => {
                self.raise("NotImplemented", &format!("Type conversion to '{}' is not implemented", target_type));
                NULL
            }
        }
    }

    pub fn handle_type_conversion(&mut self, stmt: Statement, target_type_opt: Statement) -> Value {
        if self.err.is_some() {
            return NULL;
        }

        let value = self.evaluate(&stmt);
        if self.err.is_some() {
            return NULL;
        }

        let binding = self.evaluate(&target_type_opt);
        if self.err.is_some() {
            return NULL;
        }

        let target_type_type = match &binding {
            Value::Type(tt) => tt,
            _ => return self.raise("TypeError", &format!(
                    "Type '{}' is not a valid target type for conversion",
                    value.type_name()
                )),
        };

        match value {
            Value::Struct(ref s) => {
                if let Ok(method) = find_struct_method(&s, None, "op_as", &[s.get_type(), Type::new_simple("type")], &Type::new_simple("any")) {
                    let mut var = Variable::new_pt(s.name().to_string(), Value::Struct(s.clone()), s.get_type(), false, false, false);
                    if method.is_static() {
                        return self.raise("TypeError", "Cannot call static method 'op_as' on struct instance");
                    }
                    let result = self.call_function(&method, vec![Value::Type(target_type_type.clone())], HashMap::new(), Some((None, None, Some(&mut var))));
                    if self.err.is_some() {
                        return NULL;
                    }
                    return result;
                }
            }
            _ => {}
        }

        match target_type_type {
            Type::Struct { .. } => {
                match value {
                    Value::Struct(s) => {
                        if let Ok(method) = find_struct_method(&s, None, "op_as", &[s.get_type(), Type::new_simple("type")], &Type::new_simple("any")) {
                            let mut var = Variable::new_pt(s.name().to_string(), Value::Struct(s.clone()), s.get_type(), false, false, false);
                            if method.is_static() {
                                return self.raise("TypeError", "Cannot call static method 'op_as' on struct instance");
                            }
                            let result = self.call_function(&method, vec![Value::Type(target_type_type.clone())], HashMap::new(), Some((None, None, Some(&mut var))));
                            if self.err.is_some() {
                                let err = self.err.clone().expect("Error should be present");
                                return self.raise(to_static(err.err_type), to_static(err.err_msg));
                            }
                            return result;
                        } else {
                            let s_ty = s.get_type();
                            let t_ty = target_type_type;
                            match (&s_ty, &t_ty) {
                                (Type::Struct { fields: s_fields, .. }, Type::Struct { fields: t_fields, .. }) => {
                                    match diff_fields(&s_ty.display_simple(), &t_ty.display_simple(), &s_fields, &t_fields) {
                                        Ok(hm) => {
                                            let field_values = s.get_fields();
                                            let mut new_fields = HashMap::with_capacity(t_fields.len());
                                            for (key, val) in field_values {
                                                if let Some(new_key) = hm.get(key) {
                                                    new_fields.insert(new_key.clone(), (val.0.clone(), val.1.clone()));
                                                }
                                            }
                                            return Value::Struct(Box::new(Struct::new_with_fields(t_ty.clone(), new_fields)))
                                        }
                                        Err(e) => {
                                            return self.raise_with_help("TypeError", &format!("Cannot convert struct of type '{}' to '{}'", s.get_type().display_simple(), target_type_type.display_simple()), &e);
                                        }
                                    }
                                }
                                _ => {
                                    self.raise("TypeError", &format!("Cannot convert struct of type '{}' to '{}'", s.get_type().display_simple(), target_type_type.display_simple()));
                                    return NULL;
                                }
                            }
                        }
                    }
                    _ => {
                        return self.raise("TypeError", &format!("Cannot convert '{}' to struct type '{}'", value.get_type().display_simple(), target_type_type.display_simple()));
                    }
                }
            }
            Type::Enum { .. } => {
                // TODO: Implement enum conversion
                return self.raise("NotImplemented", "Conversion to enum types is not implemented yet");
            }
            _ => {}
        }

        let target_type = match get_inner_type(target_type_type) {
            Ok((t, _)) => t,
            Err(e) => return self.raise("TypeError", &e),
        };

        if self.err.is_some() {
            return NULL;
        }

        if target_type.starts_with("&") {
            let ptr_level: usize = target_type.chars().take_while(|&c| c == '&').count();
            let new_type = target_type.trim_start_matches('&').to_string();
            if !VALID_TYPES.contains(&new_type.as_str()) {
                return self.raise("TypeError", &format!("Invalid target type '{}'", target_type));
            }
            let new_value = self.convert_value_to_type(&new_type, &value);
            if self.err.is_some() {
                return NULL;
            }
            return Value::Pointer(Arc::new(Mutex::new((new_value, ptr_level))));
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

    fn handle_import(&mut self, 
        name: String, 
        alias: Option<String>, 
        named_imports: Vec<Value>, 
        modifiers: Vec<String>,
        import_all: bool, 
        module_path_opt: Option<Statement>,
    ) -> Value {
        let (base_name, name_parts, module_name) = {
            let is_file_like = name.rsplit('.').next().map(|s| {
                matches!(s, "lc" | "lucia" | "rs")
            }).unwrap_or(false);

            if name.contains('/') || name.contains('\\') || is_file_like {
                let parts: Vec<&str> = vec![name.as_str()];
                let base = if let Some(idx) = name.rfind('.') { name[..idx].to_string() } else { name.clone() };
                (base, parts, name.clone())
            } else {
                let parts: Vec<&str> = name.split('.').collect();
                if parts.is_empty() {
                    self.raise("RuntimeError", "Invalid 'module_name' in import statement");
                    return NULL;
                }
                let base = parts[0].to_string();
                (base, parts.clone(), name.clone())
            }
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

        let alias_owned = match alias {
            Some(a) => a.clone(),
            None => name_parts.last().cloned().unwrap_or(&base_name).to_owned(),
        };
        let alias: &str = &alias_owned;

        if !is_valid_alias(&alias) {
            return self.raise_with_help(
                "ImportError",
                &format!("'{}' is an invalid name. Please use a different alias.", alias),
                &format!("Use 'import ... as <valid_alias>'. Suggested alias: '{}'", sanitize_alias(&alias)),
            );            
        }

        if self.stack.iter().any(|(name, _, type_)| name == module_name && type_ == StackType::Import) {
            return self.raise(
                "RecursionError",
                &format!("Recursive import detected for module '{}'", module_name),
            );
        }

        let mut is_public = false;
        let mut is_final = true;
        let mut is_static = true;

        for modifier in &modifiers {
            match modifier.as_str() {
                "public" => is_public = true,
                "final" => is_final = true,
                "static" => is_static = true,
                "private" => is_public = false,
                "mutable" => is_final = false,
                "non-static" => is_static = false,
                _ => {
                    self.raise("SyntaxError", &format!("Unknown import modifier '{}'", modifier));
                    return NULL;
                }
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

        if let Some(lib_info) = STD_LIBS.get(base_name.as_str()) {
            self.debug_log(format_args!("<Loading standard library module '{}', version {}, description: {}>", module_name, lib_info.version, lib_info.description));

            let expected_lucia_version = lib_info.expected_lucia_version;

            if !expected_lucia_version.is_empty() && !check_version(&self.config.version, &expected_lucia_version) {
                self.stack.pop();
                return self.raise_with_help(
                    "ImportError",
                    &format!("Module '{}' requires Lucia version '{}', but current version is '{}'", module_name, expected_lucia_version, self.config.version),
                    &format!("Please update Lucia to match the required version: '{}'", expected_lucia_version),
                );
            }

            match base_name.as_str() {
                #[cfg(feature = "math")]
                "math" => {
                    use crate::env::libs::math::main as math;
                    let math_module_props = math::register();
                    properties.extend(math_module_props);
                }
                #[cfg(feature = "os")]
                "os" => {
                    use crate::env::libs::os::main as os;
                    let os_module_props = os::register(&self.config.clone());
                    properties.extend(os_module_props);
                }
                #[cfg(feature = "time")]
                "time" => {
                    use crate::env::libs::time::main as time;
                    let time_module_props = time::register();
                    properties.extend(time_module_props);
                }
                #[cfg(feature = "json")]
                "json" => {
                    use crate::env::libs::json::main as json;
                    let json_module_props = json::register();
                    properties.extend(json_module_props);
                }
                #[cfg(feature = "clib")]
                #[cfg(target_arch = "wasm32")]
                "clib" => {
                    self.stack.pop();
                    return self.raise(
                        "ImportError",
                        "The 'clib' module is not supported in WebAssembly builds",
                    );
                }
                #[cfg(feature = "clib")]
                #[cfg(not(target_arch = "wasm32"))]
                "clib" => {
                    use crate::env::libs::clib::main as clib;
                    let arc_config = Arc::new(self.config.clone());
                    let module_path = PathBuf::from(self.config.home_dir.clone()).join("libs").join("clib").join("main.rs").display().to_string();
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
                    properties.extend(clib_module_props);
                }
                #[cfg(feature = "regex")]
                "regex" => {
                    use crate::env::libs::regex::main as regex;
                    let regex_module_props = regex::register();
                    properties.extend(regex_module_props);
                }
                #[cfg(feature = "collections")]
                "collections" => {
                    use crate::env::libs::collections::main as collections;
                    let collections_module_props = collections::register();
                    properties.extend(collections_module_props);
                }
                #[cfg(feature = "random")]
                "random" => {
                    use crate::env::libs::random::main as random;
                    let random_module_props = random::register();
                    properties.extend(random_module_props);
                }
                #[cfg(all(feature = "lasm", not(target_arch = "wasm32")))]
                "lasm" => {
                    use crate::env::libs::lasm::main as lasm;
                    let lasm_module_props = lasm::register();
                    properties.extend(lasm_module_props);
                }
                #[cfg(feature = "fs")]
                #[cfg(target_arch = "wasm32")]
                "fs" => {
                    self.stack.pop();
                    return self.raise(
                        "ImportError",
                        "The 'fs' module is not supported in WebAssembly builds",
                    );
                }
                #[cfg(feature = "fs")]
                #[cfg(not(target_arch = "wasm32"))]
                "fs" => {
                    use crate::env::libs::fs::main as fs;
                    let fs_module_props = fs::register();
                    properties.extend(fs_module_props);
                }
                #[cfg(feature = "nest")]
                "nest" => {
                    use crate::env::libs::nest::main as nest;
                    let arc_config = Arc::new(self.config.clone());
                    let module_path = PathBuf::from(self.config.home_dir.clone()).join("libs").join("nest").join("main.rs").display().to_string();
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
                    properties.extend(nest_module_props);
                }
                #[cfg(feature = "libload")]
                #[cfg(target_arch = "wasm32")]
                "libload" => {
                    self.stack.pop();
                    return self.raise(
                        "ImportError",
                        "The 'libload' module is not supported in WebAssembly builds",
                    );
                }
                #[cfg(feature = "libload")]
                #[cfg(not(target_arch = "wasm32"))]
                "libload" => {
                    use crate::env::libs::libload::main as libload;
                    let arc_config = Arc::new(self.config.clone());
                    let module_path = PathBuf::from(self.config.home_dir.clone()).join("libs").join("libload").join("main.rs").display().to_string();
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
                    properties.extend(libload_module_props);
                }
                #[cfg(feature = "elevator")]
                #[cfg(target_arch = "wasm32")]
                "elevator" => {
                    self.stack.pop();
                    return self.raise(
                        "ImportError",
                        "The 'elevator' module is not supported in WebAssembly builds",
                    );
                }
                #[cfg(feature = "elevator")]
                #[cfg(not(target_arch = "wasm32"))]
                "elevator" => {
                    use crate::env::libs::elevator::main as elevator;
                    let elevator_module_props = elevator::register(&self.config);
                    properties.extend(elevator_module_props);
                }
                #[cfg(feature = "hash")]
                "hash" => {
                    use crate::env::libs::hash::main as hash;
                    let hash_module_props = hash::register();
                    properties.extend(hash_module_props);
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

            let resolved_module_path: Option<PathBuf>;
            let mut found_in_dirs: Vec<PathBuf> = vec![];
            let module_variants = generate_name_variants(&base_name);
            let lib_names_map = get_lib_names(&self.config.libs_paths);

            if let Some(map_statement) = &module_path_opt {
                let path_eval = self.evaluate(map_statement);
                let path_str = path_eval.to_string();
                if path_str.is_empty() {
                    self.stack.pop();
                    return self.raise("RuntimeError", "Empty 'path' in import statement");
                }

                let mut base_path = PathBuf::from(path_str);
                if base_path.is_relative() {
                    base_path = self.cwd.join(base_path);
                }
                base_path = base_path.canonicalize().unwrap_or(base_path);

                for variant in &module_variants {
                    let candidate_dir = base_path.join(variant);
                    if candidate_dir.exists() && candidate_dir.is_dir() {
                        found_in_dirs.push(candidate_dir.clone());
                    } else {
                        for ext in &[".lc", ".lucia", ".rs", ""] {
                            let candidate_file = base_path.join(format!("{}{}", variant, ext));
                            if candidate_file.exists() && candidate_file.is_file() {
                                found_in_dirs.push(candidate_file.clone());
                            }
                        }
                    }
                }

                if found_in_dirs.is_empty() {
                    for variant in &module_variants {
                        if let Some(original_name) = lib_names_map.get(&Arc::from(variant.as_str())) {
                            let candidate_dir = base_path.join(&**original_name);
                            if candidate_dir.exists() && candidate_dir.is_dir() {
                                found_in_dirs.push(candidate_dir.clone());
                            }
                        }
                    }
                }
            } else {
                for lib_dir_str in &self.config.libs_paths {
                    let base_module_path = {
                        let p = PathBuf::from(lib_dir_str);
                        let abs_path = if p.is_absolute() {
                            p
                        } else {
                            PathBuf::from(&self.config.home_dir).join(p)
                        };
                        abs_path.canonicalize().unwrap_or(abs_path)
                    };

                    for variant in &module_variants {
                        let candidate_dir = base_module_path.join(variant);

                        if candidate_dir.exists() && candidate_dir.is_dir() {
                            found_in_dirs.push(candidate_dir.clone());
                        } else {
                            for ext in &[".lc", ".lucia", ".rs", ""] {
                                let candidate_file = base_module_path.join(format!("{}{}", variant, ext));
                                if candidate_file.exists() && candidate_file.is_file() {
                                    found_in_dirs.push(candidate_file.clone());
                                }
                            }
                        }
                    }

                    if found_in_dirs.is_empty() {
                        for variant in &module_variants {
                            if let Some(original_name) = lib_names_map.get(&Arc::from(variant.as_str())) {
                                let candidate_dir = base_module_path.join(&**original_name);
                                if candidate_dir.exists() && candidate_dir.is_dir() {
                                    found_in_dirs.push(candidate_dir.clone());
                                }
                            }
                        }
                    }
                }
            }

            match found_in_dirs.len() {
                0 => {
                    self.stack.pop();
                    return self.raise("ImportError", &format!("Module '{}' not found", &module_name));
                }
                1 => {
                    resolved_module_path = Some(found_in_dirs.remove(0));
                }
                _ => {
                    let paths_str = found_in_dirs.iter()
                        .map(|p| format!("'{}'", p.display()))
                        .collect::<Vec<_>>()
                        .join(", ");
                    self.stack.pop();
                    return self.raise(
                        "ImportError",
                        &format!("Module '{}' is ambiguous, found in multiple locations: {}", module_name, paths_str)
                    );
                }
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
                let mut entry_point: Option<PathBuf> = if module_path.join("main.lc").exists() && module_path.join("main.lc").is_file() {
                    Some(module_path.join("main.lc"))
                } else if module_path.join("main.lucia").exists() && module_path.join("main.lucia").is_file() {
                    Some(module_path.join("main.lucia"))
                } else {
                    None
                };
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
                        let package_type = manifest_json.get("package_type").and_then(|v| v.as_str()).unwrap_or("import");
                        match package_type {
                            "import" | "bindings" => {}
                            "include" => {
                                self.stack.pop();
                                return self.raise_with_help(
                                    "ImportError",
                                    &format!("Module '{}' is an include and cannot be imported as a regular module", print_name),
                                    &format!("Use '#include \"{}\"' to include it.", fix_path(module_path.display().to_string())),
                                );
                            }
                            "plugin" => {
                                self.stack.pop();
                                return self.raise(
                                    "ImportError",
                                    &format!("Module '{}' is a plugin and cannot be imported as a regular module", print_name),
                                );
                            }
                            _ => {
                                self.stack.pop();
                                return self.raise("MalformedManifest", &format!("Invalid 'package_type' '{}' in manifest '{}'", package_type, fix_path(manifest_path.display().to_string())));
                            }
                        }
                        if let Some(entry_point_path_str) = manifest_json.get("entry_point").and_then(|v| v.as_str()) {
                            let entry_point_path = module_path.join(entry_point_path_str);
                            if entry_point_path.exists() && entry_point_path.is_file() {
                                entry_point = Some(entry_point_path);
                            } else {
                                self.stack.pop();
                                return self.raise("ImportError", &format!(
                                    "Entry point file '{}' specified in manifest not found",
                                    fix_path(entry_point_path.display().to_string())
                                ));
                            }
                        }

                        self.debug_log(
                            format_args!(
                                "<Manifest for module '{}': version {}, description: {}, authors: {:?}, license: {}>",
                                print_name, version, description, authors, license
                            )
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

                        self.debug_log(
                            format_args!(
                                "<Importing module '{}', version: {}{}>",
                                print_name,
                                version,
                                if description.is_empty() { "".to_owned() } else { format!(", description: {}", description) }
                            )
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
                        
                                let mut dep_exists = false;
                                let mut dep_path = PathBuf::new();
                                let mut dep_matching_count = 0;
                                for lib_dir_str in &self.config.libs_paths {
                                    let candidate_path = PathBuf::from(lib_dir_str).join(dep_name);
                                    if candidate_path.exists() && candidate_path.is_dir() {
                                        dep_exists = true;
                                        dep_path = candidate_path;
                                        dep_matching_count += 1;
                                    }
                                }
                                if dep_matching_count > 1 {
                                    self.stack.pop();
                                    return self.raise_with_help(
                                        "ImportError",
                                        &format!("Dependency '{}' is ambiguous, found in multiple locations", dep_name),
                                        &format!("Remove or rename the library with the conflicting name to resolve the ambiguity."),
                                    );
                                }
                                if !dep_exists {
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
                    self.debug_log(format_args!("<Importing module '{}'>", module_name));
                }                
                
                if let Some(ref entry_point_path) = entry_point {
                    let props = self.get_properties_from_file(entry_point_path);
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
                                if (ext == "lc" || ext == "lucia")
                                    && Some(path.clone()) != entry_point
                                {
                                    let properties_result = self.get_properties_from_file(&path);
                                    if self.err.is_some() {
                                        self.stack.pop();
                                        return NULL;
                                    }
                                    properties.extend(properties_result);
                                } else if entry_point.is_none() {
                                    if ext != "lc" && ext != "lucia" {
                                        self.stack.pop();
                                        return self.raise(
                                            "ImportError",
                                            &format!("Unsupported file extension '{}'", ext)
                                        );
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

        if name_parts.len() > 1 {
            let mut cur_props = properties.clone();
            for seg in name_parts.iter().skip(1) {
                let next = if let Some(var) = cur_props.get(*seg) {
                    match &var.value {
                        Value::Module(m) => Some((m.properties.clone(), m.path.clone())),
                        _ => {
                            self.stack.pop();
                            return self.raise("ImportError", &format!("'{}' in '{}' is not a module", seg, name_parts[0]));
                        }
                    }
                } else {
                    self.stack.pop();
                    return self.raise("ImportError", &format!("Module '{}' not found inside '{}'", seg, name_parts[0]));
                };

                let (p, ppath) = next.unwrap();
                cur_props = p;
                module_path = ppath;
            }
            properties = cur_props;
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
                            self.debug_log(format_args!("<Importing {} '{}' from module '{}'>", category, name, module_name));
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
                                self.debug_log(format_args!("<Importing {} '{}' from '{}' as '{}'>", category, name, module_name, alias));
                            } else {
                                self.debug_log(format_args!("<Importing {} '{}' from module '{}'>", category, name, module_name));
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
                            self.debug_log(format_args!("<Importing {} '{}' from module '{}'>", category, name, module_name));
                        }
                    }
                }
            }
            if self.err.is_some() {
                self.stack.pop();
                return NULL;
            }

            self.debug_log(format_args!("<Module '{}' imported successfully>", module_name));

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
                Variable::new(alias.to_owned(), module, "module".to_owned(), is_static, is_public, is_final),
            );
        
            self.stack.pop();

            self.variables.get(alias).unwrap().value.clone()
        }
    }

    fn handle_return(&mut self, stmt: Statement) -> Value {
        self.state = State::Defer;
        if !self.defer_stack.is_empty() {
            let defer_statements = self.defer_stack.concat();
            for stmt in defer_statements {
                self.evaluate(&stmt);
            }
        }
        self.state = State::Normal;

        // Tailcall optimization (TCO)
        if let Node::Call { ref name, .. } = stmt.node {
            let len = self.stack.frames.len();
            for i in (0..len).rev() {
                let frame = &self.stack.frames[i];
                if frame.stack_type == StackType::FunctionCall && frame.file_path == *name {
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

    fn handle_function_declaration(
        &mut self, 
        name: String, 
        args: Vec<ParamAST>,
        body: Vec<Statement>, 
        modifiers: Vec<String>, 
        return_type: Statement,
        effect_flags: EffectFlags,
        docs: Option<String>,
        is_struct_method: bool,
    ) -> Value {
        let mut name = name;
        let mut return_type_str = match self.evaluate(&return_type) {
            Value::Type(t) => t,
            _ => return self.raise("RuntimeError", "Invalid 'return_type' in function declaration"),
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
            match modifier.as_str() {
                "public" => is_public = true,
                "static" => is_static = true,
                "final" => is_final = true,
                "private" => is_public = false,
                "non-static" => is_static = false,
                "mutable" => is_final = false,
                _ => return self.raise("SyntaxError", &format!("Unknown function modifier: {}", modifier)),
            }
        }

        let mut parameters = Vec::with_capacity(args.len());

        for arg in args {
            let type_value = match arg.ty {
                Some(ty_stmt) => match self.evaluate(&ty_stmt) {
                    Value::Type(t) => t,
                    _ => return self.raise("RuntimeError", "Invalid 'type' in function parameter"),
                },
                None => Type::new_simple("any"),
            };
            if self.err.is_some() {
                return NULL;
            }


            if arg.is_variadic {
                parameters.push(Parameter::variadic_optional(
                    arg.name.as_str(),
                    "any",
                    Value::List(vec![]),
                ).set_mods(arg.modifiers.clone()));
            } else if arg.default.is_some() {
                let default_value = self.evaluate(&arg.default.unwrap());
                if self.err.is_some() {
                    return NULL;
                }
                parameters.push(Parameter::positional_optional_pt(
                    arg.name.as_str(),
                    &type_value,
                    default_value,
                ).set_mods(arg.modifiers.clone()));
            } else {
                parameters.push(Parameter::positional_pt(arg.name.as_str(), &type_value).set_mods(arg.modifiers.clone()));
            }
        }

        let lambda_name;  // stupid lifetimes
        if name == "<lambda#{id}>" {
            let id: usize = self.internal_storage.lambda_counter;
            self.internal_storage.lambda_counter = id + 1;
            lambda_name = name.replace("{id}", &id.to_string());
            name = lambda_name;
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
            doc: docs.map(|v| parse_doc(&v)),
        };

        if self.variables.contains_key(&name) && !name.starts_with("<") && !is_struct_method {
            if let Some(var) = self.variables.get(&name) {
                if var.is_final() {
                    return self.raise("AssigmentError", &format!("Cannot redefine final function '{}'", name));
                }
            }
        }

        self.debug_log(
            format_args!("<Defining {} '{}'>", if is_struct_method { "struct method" } else { "function" }, name)
        );

        let function = if name.starts_with("<") && name.ends_with(">") {
            Value::Function(Function::Lambda(Arc::new(UserFunction {
                meta: metadata.clone(),
                body: body,
            }), self.variables.clone()))
        } else {
            create_function(
                metadata.clone(),
                body,
            )
        };

        if is_struct_method {
            return function;
        }
        self.variables.insert(
            name.to_string(),
            Variable::new(
                name.to_string(),
                function,
                "function".to_owned(),
                is_static,
                is_public,
                is_final,
            ),
        );
        self.variables.get(&name)
            .map_or(NULL, |var| var.value.clone())
    }

    #[track_caller]
    fn handle_throw(&mut self, throw_ast: ThrowNode) -> Value {
        let mut prev_err = None;

        match throw_ast {
            ThrowNode::Message { message, from } => {
                let error_type_val = match from {
                    Some(v) => self.evaluate(&v),
                    None => Value::String("LuciaError".to_owned()),
                };
                
                if self.err.is_some() {
                    prev_err = self.err.clone();
                    self.err = None;
                }
                
                let error_msg_val = self.evaluate(&message);
                
                self.debug_log(
                    format_args!(
                        "<Throwing error: '{}: {}'>",
                        error_type_val.to_string(),
                        error_msg_val.to_string()
                    )
                );
                
                if self.err.is_some() {
                    if prev_err.is_some() {
                        self.raise_with_ref(
                            &self.err.clone().unwrap().err_type,
                            &self.err.clone().unwrap().err_msg,
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
            ThrowNode::Tuple(stmt) => {
                let val_evaluated = self.evaluate(&stmt);
                if self.err.is_some() {
                    prev_err = self.err.clone();
                    self.err = None;
                }
                let items = if matches!(val_evaluated, Value::String(_) | Value::Bytes(_)) {
                    vec![val_evaluated.clone()]
                } else if val_evaluated.is_iterable() {
                    val_evaluated.iter().collect::<Vec<Value>>()
                } else {
                    return self.raise_with_help("TypeError", "Thrown tuple value must be iterable", &format!("It tried to throw from '{}'", format_value(&val_evaluated)));
                };
                match items.len() {
                    1 => {
                        let error_msg = items[0].to_string();
                        let error_type = "LuciaError";
                        self.debug_log(
                            format_args!(
                                "<Throwing error: '{}: {}'>",
                                error_type,
                                error_msg
                            )
                        );
                        if self.err.is_some() {
                            if prev_err.is_some() {
                                self.raise_with_ref(
                                    &self.err.clone().unwrap().err_type,
                                    &self.err.clone().unwrap().err_msg,
                                    prev_err.clone().unwrap(),
                                );
                                prev_err = self.err.clone();
                                return self.raise_with_ref(
                                    error_type,
                                    to_static(error_msg),
                                    prev_err.clone().unwrap(),
                                );
                            } else {
                                return self.raise_with_ref(
                                    error_type,
                                    to_static(error_msg),
                                    self.err.clone().unwrap(),
                                );
                            }
                        }
                        
                        self.raise(
                            error_type,
                            to_static(error_msg),
                        )
                    }
                    2 => {
                        let error_type = items[0].to_string();
                        let error_msg = items[1].to_string();
                        self.debug_log(
                            format_args!(
                                "<Throwing error: '{}: {}'>",
                                error_type,
                                error_msg
                            )
                        );
                        if self.err.is_some() {
                            if prev_err.is_some() {
                                self.raise_with_ref(
                                    &self.err.clone().unwrap().err_type,
                                    &self.err.clone().unwrap().err_msg,
                                    prev_err.clone().unwrap(),
                                );
                                prev_err = self.err.clone();
                                return self.raise_with_ref(
                                    to_static(error_type),
                                    to_static(error_msg),
                                    prev_err.clone().unwrap(),
                                );
                            } else {
                                return self.raise_with_ref(
                                    to_static(error_type),
                                    to_static(error_msg),
                                    self.err.clone().unwrap(),
                                );
                            }
                        }
                        
                        self.raise(
                            to_static(error_type),
                            to_static(error_msg),
                        )
                    }
                    3 => {
                        let error_type = items[0].to_string();
                        let error_msg = items[1].to_string();
                        let help_msg = items[2].to_string();
                        self.debug_log(
                            format_args!(
                                "<Throwing error: '{}: {}'>",
                                error_type,
                                error_msg
                            )
                        );
                        if self.err.is_some() {
                            if prev_err.is_some() {
                                self.raise_with_ref(
                                    &self.err.clone().unwrap().err_type,
                                    &self.err.clone().unwrap().err_msg,
                                    prev_err.clone().unwrap(),
                                );
                                prev_err = self.err.clone();
                                return self.raise_with_ref(
                                    to_static(error_type),
                                    to_static(error_msg),
                                    prev_err.clone().unwrap(),
                                );
                            } else {
                                return self.raise_with_ref(
                                    to_static(error_type),
                                    to_static(error_msg),
                                    self.err.clone().unwrap(),
                                );
                            }
                        }
                        self.raise_with_help(
                            to_static(error_type),
                            to_static(error_msg),
                            &help_msg,
                        )
                    }
                    _ => {
                        return self.raise_with_help("ValueError", "Thrown tuple must have 1 to 3 elements", &format!("It tried to throw from '{}'", format_value(&val_evaluated)));
                    }
                }
            }
        }
    }

    fn handle_forget(&mut self, stmt: Statement) -> Value {
        match stmt.node {
            Node::Variable { name, .. } => {
                match self.variables.remove(&name) {
                    Some(value) => {
                        self.debug_log(
                            format_args!("<Variable '{}' forgotten>", name)
                        );

                        let dropped_value = value.get_value().clone();
                        drop(value);
                        dropped_value
                    },
                    None => self.raise("NameError", &format!("Variable '{}' not found for forget", name)),
                }
            }

            Node::IndexAccess { object, access } => {
                let object_val = self.evaluate(&*object);
                if self.err.is_some() { return NULL; }

                let variable_name = match &object.node {
                    Node::Variable { name, .. } => Some(name.clone()),
                    _ => None,
                };

                let (start_idx, end_idx) = match &*access {
                    AccessType::Single { index } => {
                        let idx_val = self.evaluate(&index);
                        if self.err.is_some() { return NULL; }

                        let end_val = match self.to_index(&idx_val, match &object_val {
                            Value::String(s) => s.chars().count(),
                            Value::List(l) => l.len(),
                            Value::Bytes(b) => b.len(),
                            Value::Tuple(_) => return self.raise("TypeError", "Tuples are immutable, cannot forget indexes."),
                            Value::Map(m) => m.len(),
                            _ => return self.raise("TypeError", "Object not indexable"),
                        }) {
                            Ok(i) => i + 1,
                            Err(e) => return e,
                        };

                        let start = match self.to_index(&idx_val, end_val) {
                            Ok(i) => i,
                            Err(e) => return e,
                        };

                        (start, end_val)
                    }

                    AccessType::Range { start, end } => {
                        let s = self.evaluate(&start);
                        if self.err.is_some() { return NULL; }

                        let e = self.evaluate(&end);
                        if self.err.is_some() { return NULL; }

                        let start = if s == NULL { 0 } else { match self.to_index(&s, match &object_val {
                            Value::String(s) => s.chars().count(),
                            Value::List(l) => l.len(),
                            Value::Bytes(b) => b.len(),
                            Value::Tuple(_) => return self.raise("TypeError", "Tuples are immutable"),
                            Value::Map(m) => m.len(),
                            _ => return self.raise("TypeError", "Object not indexable"),
                        }) { Ok(i) => i, Err(e) => return e } };

                        let end = if e == NULL { start + 1 } else { match self.to_index(&e, match &object_val {
                            Value::String(s) => s.chars().count(),
                            Value::List(l) => l.len(),
                            Value::Bytes(b) => b.len(),
                            Value::Tuple(_) => return self.raise("TypeError", "Tuples are immutable"),
                            Value::Map(m) => m.len(),
                            _ => return self.raise("TypeError", "Object not indexable"),
                        }) { Ok(i) => i, Err(e) => return e } };

                        (start, end)
                    }

                    AccessType::ToEnd { start } => {
                        let s = self.evaluate(&start);
                        if self.err.is_some() { return NULL; }

                        let start_idx = match self.to_index(&s, match &object_val {
                            Value::String(s) => s.chars().count(),
                            Value::List(l) => l.len(),
                            Value::Bytes(b) => b.len(),
                            Value::Tuple(_) => return self.raise("TypeError", "Tuples are immutable"),
                            Value::Map(m) => m.len(),
                            _ => return self.raise("TypeError", "Object not indexable"),
                        }) {
                            Ok(i) => i,
                            Err(e) => return e,
                        };

                        let end_idx = match &object_val {
                            Value::String(s) => s.chars().count(),
                            Value::List(l) => l.len(),
                            Value::Bytes(b) => b.len(),
                            Value::Map(m) => m.len(),
                            _ => return self.raise("TypeError", "Object not indexable"),
                        };

                        (start_idx, end_idx)
                    }

                    AccessType::ToStart { end } => {
                        let e = self.evaluate(&end);
                        if self.err.is_some() { return NULL; }

                        let end_idx = match self.to_index(&e, match &object_val {
                            Value::String(s) => s.chars().count(),
                            Value::List(l) => l.len(),
                            Value::Bytes(b) => b.len(),
                            Value::Tuple(_) => return self.raise("TypeError", "Tuples are immutable"),
                            Value::Map(m) => m.len(),
                            _ => return self.raise("TypeError", "Object not indexable"),
                        }) {
                            Ok(i) => i,
                            Err(e) => return e,
                        };

                        (0, end_idx)
                    }

                    AccessType::Full => {
                        let end_idx = match &object_val {
                            Value::String(s) => s.chars().count(),
                            Value::List(l) => l.len(),
                            Value::Bytes(b) => b.len(),
                            Value::Tuple(_) => return self.raise("TypeError", "Tuples are immutable"),
                            Value::Map(m) => m.len(),
                            _ => return self.raise("TypeError", "Object not indexable"),
                        };
                        (0, end_idx)
                    }
                };

                if start_idx > end_idx {
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
                        return self.raise_with_help(
                            "TypeError",
                            "Cannot forget slice on immutable string",
                            to_static(format!(
                                "You can use bytes instead of a string, or do: {}",
                                wrap_in_help(&format!("new_string := old_string[..{}] + old_string[{}..]", start_idx, end_idx), &self.config)
                            ))
                        );
                    }
                    Value::Tuple(_) => return self.raise("TypeError", "Tuples are immutable"),
                    Value::Map(m) => {
                        let mut new_map = m.clone();
                        if start_idx < new_map.len() && end_idx <= new_map.len() {
                            let keys_to_remove: Vec<_> = new_map.keys().cloned().skip(start_idx).take(end_idx - start_idx).collect();
                            for key in keys_to_remove {
                                new_map.remove(&key);
                            }
                        }
                        Value::Map(new_map)
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

            Node::Iterable { node } => {
                let tuple_items = match node {
                    IterableNode::Tuple { elements } => elements,
                    IterableNode::List { .. } => {
                        return self.raise_with_help(
                            "ValueError",
                            "Cannot use type 'list' in forget",
                            "Did you mean to use 'tuple'?",
                        )
                    }
                    _ => return self.raise("RuntimeError", "Unsupported iterable type in forget"),
                };

                let mut names = Vec::with_capacity(tuple_items.len());
                let mut stack = Vec::with_capacity(8);

                for stmt in &tuple_items {
                    stack.push(stmt);
                }

                while let Some(stmt) = stack.pop() {
                    match &stmt.node {
                        Node::Variable { name, .. } => {
                            names.push(name.clone());
                        }
                        Node::Iterable { node } => {
                            match node {
                                IterableNode::Tuple { elements } => {
                                    for s in elements.iter().rev() {
                                        stack.push(to_static(s.clone()));
                                    }
                                }
                                _ => return self.raise("RuntimeError", "Unsupported iterable type in forget"),
                            }
                        }
                        _ => return self.raise("RuntimeError", "Invalid item in tuple forget, expected variable or iterable"),
                    }
                }

                let mut results = Vec::with_capacity(names.len());

                let mut name_iter = names.into_iter();

                fn walk_forget(
                    this: &mut Interpreter,
                    stmt: &Statement,
                    name_iter: &mut impl Iterator<Item=String>,
                    out: &mut Vec<Value>,
                ) {
                    match &stmt.node {
                        Node::Variable { .. } => {
                            let name = name_iter.next().unwrap();
                            let tmp = Statement {
                                node: Node::Variable { name },
                                loc: stmt.loc.clone(),
                            };
                            out.push(this.handle_forget(tmp));
                        }
                        Node::Iterable { node } => {
                            if let IterableNode::Tuple { elements } = node {
                                for sub in elements {
                                    walk_forget(this, sub, name_iter, out);
                                }
                            }
                        }
                        _ => {}
                    }
                }

                for stmt in &tuple_items {
                    walk_forget(self, &stmt, &mut name_iter, &mut results);
                }

                Value::Tuple(results)
            }

            Node::Pointer { ptr_node, value } => {
                let mut current = self.evaluate(&*value);
                if self.err.is_some() {
                    return NULL;
                }

                let deref_level = match ptr_node {
                    PtrNode::PointerDeref { deref_level } => deref_level,
                    PtrNode::PointerRef { .. } => {
                        return self.raise("TypeError", "Cannot forget a pointer reference");
                    }
                    PtrNode::PointerAssign { .. } => {
                        return self.raise("TypeError", "Cannot forget a pointer assignment");
                    }
                };

                let mut level = deref_level;

                while level > 0 {
                    match current {
                        Value::Pointer(ref arc_ptr) => {
                            current = {
                                let mut guard = arc_ptr.lock();

                                if guard.1 > 1 {
                                    guard.1 -= 1;
                                    Value::Pointer(arc_ptr.clone())
                                } else {
                                    std::mem::replace(&mut guard.0, Value::Null)
                                }
                            };
                        }
                        _ => {
                            return self.raise("TypeError", "Expected pointer for deref forget");
                        }
                    }

                    level -= 1;
                }

                Value::Null
            }

            n => self.raise("RuntimeError", &format!("Cannot forget {}", n.name())),
        }
    }

    fn handle_type(&mut self, type_kind: TypeNode) -> Value {
        let type_: Type = match type_kind {
            TypeNode::Simple { base: type_name } => {
                match self.variables.get(type_name.as_str()) {
                    Some(val) => match &val.value {
                        Value::Type(t) => { t.clone() }
                        _ => {
                            self.raise_with_help("TypeError", &format!("'{}' is a variable name, not a type", type_name), "If you meant to assign a value, use ':=' instead of ':'");
                            return NULL;
                        }
                    },
                    None => {
                        self.raise(
                            "TypeError",
                            &format!(
                                "Invalid type '{}'. Valid types are: {}, ...",
                                type_name,
                                VALID_TYPES[0..5].join(", ")
                            ),
                        );
                        return NULL;
                    }
                }
            }

            TypeNode::Reference { base_type, ref_level } => {
                let base = self.handle_type(*base_type);
                if self.err.is_some() {
                    return NULL;
                }
                match &base {
                    Value::Type(t) => {
                        Type::Reference {
                            base_type: Box::new(t.clone()),
                            ref_level,
                        }
                    }
                    _ => {
                        self.raise("TypeError", "Reference type must be a valid type");
                        return NULL;
                    }
                }
            }

            TypeNode::Maybe { base_type } => {
                let base = self.handle_type(*base_type);
                if self.err.is_some() {
                    return NULL;
                }
                match &base {
                    Value::Type(t) => {
                        Type::Maybe {
                            base_type: Box::new(t.clone()),
                        }
                    }
                    _ => {
                        self.raise("TypeError", "Maybe type must be a valid type");
                        return NULL;
                    }
                }
            }
    
            TypeNode::Union { types: types_list } => {
                let mut types: Vec<Type> = Vec::with_capacity(types_list.len());

                for t in types_list {
                    let ty = self.handle_type(t);
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
    
            TypeNode::Function { parameters_types, return_type }=> {
                let mut elements: Vec<Type> = Vec::with_capacity(parameters_types.len());
                for p in parameters_types {
                    match self.handle_type(p) {
                        Value::Type(t) => elements.push(t),
                        _ => {
                            self.raise("TypeError", "Function parameter types must be valid types");
                            return NULL;
                        }
                    }
                };
                let return_type: Type = match self.handle_type(*return_type) {
                    Value::Type(t) => t,
                    t => {
                        self.raise("TypeError", &format!("Invalid function return type '{}'", t.get_type().display_simple()));
                        return NULL;
                    }
                };

                Type::Function {
                    parameter_types: elements,
                    return_type: Box::new(return_type),
                }
            }

            TypeNode::Generator { parameters_types, yield_type } => {
                let mut elements: Vec<Type> = Vec::with_capacity(parameters_types.len());
                for p in parameters_types {
                    match self.handle_type(p) {
                        Value::Type(t) => elements.push(t),
                        _ => {
                            self.raise("TypeError", "Function parameter types must be valid types");
                            return NULL;
                        }
                    }
                };

                let yield_type: Box<Type> = match self.handle_type(*yield_type) {
                    Value::Type(t) => Box::new(t),
                    t => {
                        self.raise("TypeError", &format!("Invalid generator return type '{}'", t.get_type().display_simple()));
                        return NULL;
                    }
                };

                Type::Generator {
                    parameter_types: elements,
                    yield_type,
                }
            }
    
            TypeNode::Generics { base_type, generics_types } => {
                let base = self.handle_type(*base_type);
                if self.err.is_some() {
                    return NULL;
                }

                let mut elements = Vec::with_capacity(generics_types.len());
                let mut elements_raw = Vec::with_capacity(generics_types.len());
                for gt in generics_types {
                    match self.handle_type(gt.clone() ) {
                        Value::Type(t) => { elements_raw.push(Statement { node: Node::Type { node: gt }, loc: self.get_location_from_current_statement_id()}); elements.push(t) },
                        _ => {
                            self.raise("TypeError", "Indexed type elements must be valid types");
                            return NULL;
                        }
                    }
                }
                if self.err.is_some() {
                    return NULL;
                }

                let base_type = if let Value::Type(ref t) = base {
                    t
                } else {
                    self.raise("TypeError", &format!("Invalid base type '{}'", base.get_type().display_simple()));
                    return NULL;
                };

                match &base_type {
                    Type::Simple { ty, .. } if *ty == SimpleType::List || *ty == SimpleType::Tuple || *ty == SimpleType::Map => {
                        Type::Indexed {
                            base_type: Box::new(base_type.clone()),
                            elements: elements,
                        }
                    }
                    Type::Struct { name, fields, methods, generics: s_generics, wheres } => {
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
                            &self.file_path.clone(),
                            &self.cwd.clone(),
                            self.preprocessor_info.clone(),
                            &[]
                        );
                        interp.variables.extend(generics_evaluated.clone().into_iter().map(|(k, v)| {
                            (k.clone(), Variable::new(k, Value::Type(v), "type".to_owned(), false, true, true))
                        }));
                        for (field_name, field_type, field_mods) in fields.iter() {
                            let v = match interp.evaluate(&field_type.clone()) {
                                Value::Type(t) => Value::Type(t),
                                _ => {
                                    self.raise("TypeError", &format!("Field '{}' in struct '{}' has invalid type", field_name, name));
                                    return NULL;
                                }
                            };
                            let new_field_type = Statement {
                                node: Node::Value {
                                    value: v
                                },
                                loc: alloc_loc(self.get_location_from_current_statement()),
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
                    Type::Enum { name, variants, generics: s_generics, wheres } => {
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
                                let (v, _state, _err, _vars) = interp.run_isolated(interp.variables.clone(), &[variant_type.clone()], false);
                                let v = match v {
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
                                let new_field_type = Statement {
                                    node: Node::Value {
                                        value: v
                                    },
                                    loc: alloc_loc(self.get_location_from_current_statement()),
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
                        return self.raise("TypeError", &format!("Invalid generic type '{}'", base_type.display_simple()));
                    }
                }
            }

            TypeNode::Impl { impls: impls_ast, joins } => {
                let mut impls: Vec<(String, Box<Type>, Vec<String>)> = Vec::new();
                for (name, args, mods, return_type) in impls_ast {
                    let vars_clone = self.variables.clone();
                    self.variables.insert("Self".to_owned(), Variable::new("Self".to_owned(), Value::Type(Type::new_simple("any")), "type".to_owned(), false, true, true));
                    
                    let return_type = match self.handle_type(*return_type) {
                        Value::Type(t) => t,
                        _ => {
                            self.raise("TypeError", "Impl return type must be a valid type");
                            self.variables = vars_clone;
                            return NULL;
                        }
                    };
                    if self.err.is_some() {
                        self.variables = vars_clone;
                        return NULL;
                    }

                    let mut elements: Vec<Type> = Vec::with_capacity(args.len());
                    for arg in args {
                        match self.handle_type(arg) {
                            Value::Type(t) => elements.push(t),
                            _ => {
                                self.raise("TypeError", "Impl parameter types must be valid types");
                                self.variables = vars_clone;
                                return NULL;
                            }
                        }
                        if self.err.is_some() {
                            self.variables = vars_clone;
                            return NULL;
                        }
                    }
                    self.variables = vars_clone;

                    impls.push((name, Box::new(Type::Function {
                        parameter_types: elements,
                        return_type: Box::new(return_type),
                    }), mods));
                }
                for join in joins {
                    if !self.variables.contains_key(&join) {
                        self.raise("NameError", &format!("Type '{}' not defined", join));
                        return NULL;
                    }
                    if let Some(var) = self.variables.get(&join) {
                        if let Value::Type(t) = &var.value {
                            if let Ok((_, inner)) = get_inner_type(t) {
                                match inner {
                                    Type::Impl { implementations } => {
                                        for (name, func_type, mods) in implementations {
                                            impls.push((name.clone(), func_type.clone(), mods.clone()));
                                        }
                                    }
                                    _ => {
                                        self.raise("TypeError", &format!("Type '{}' is not an impl type", join));
                                        return NULL;
                                    }
                                }
                            } else {
                                self.raise("TypeError", &format!("Type '{}' is not an impl type", join));
                                return NULL;
                            }
                        } else {
                            self.raise("TypeError", &format!("'{}' is a variable name, not a type", join));
                            return NULL;
                        }
                    }
                }
                Type::Impl {
                    implementations: impls,
                }
            }

            TypeNode::Variadics { .. } => {
                self.raise("TypeError", "Variadic types cannot be used in type declarations");
                return NULL;
            }
        };
        return Value::Type(type_);
    }

    fn handle_if(&mut self, condition: Statement, body: &[Statement], else_body: Option<&[Statement]>) -> Value {
        let condition_value = self.evaluate(&condition);
        if self.err.is_some() {
            return NULL;
        }

        let stmts_to_run = if condition_value.is_truthy() {
            Some(body)
        } else {
            else_body
        };

        if let Some(stmts) = stmts_to_run {
            let mut result = NULL;
            for stmt in stmts {
                result = self.evaluate(stmt);
                if self.err.is_some() {
                    return NULL;
                }
            }
            result
        } else {
            NULL
        }
    }

    fn handle_try(
        &mut self,
        body: Vec<Statement>,
        catch_body_opt: Option<Vec<Statement>>,
        exception_vars_opt: Option<Vec<String>>,
    ) -> Value {
        let is_try_catch = catch_body_opt.is_some();
        let catch_body: &[Statement] = catch_body_opt.as_deref().unwrap_or(&[]);

        let exception_vars: &[String] = if is_try_catch {
            match exception_vars_opt.as_deref() {
                Some(vars) => {
                    if vars.len() > 3 {
                        return self.raise(
                            "SyntaxError",
                            "Too many exception variables (max 3: err_type, err_msg, err_help)",
                        );
                    }
                    if vars.is_empty() {
                        return self.raise_with_help(
                            "SyntaxError",
                            "No exception variables provided",
                            "Use '_' to ignore caught exception(s)",
                        );
                    }
                    let mut seen = std::collections::HashSet::with_capacity(vars.len());
                    for name in vars {
                        if !seen.insert(name) {
                            return self.raise("NameError", &format!("Duplicate exception variable name '{}'", name));
                        }
                    }
                    vars
                }
                None => {
                    return self.raise_with_help(
                        "RuntimeError",
                        "No exception variables provided for catch block",
                        "This should not happen",
                    );
                }
            }
        } else {
            &[]
        };

        let mut result = NULL;
        let change_in_try = !self.internal_storage.in_try_block;
        if change_in_try {
            self.internal_storage.in_try_block = true;
        }

        for stmt in &body {
            result = self.evaluate(stmt);
            if let Some(mut err) = self.err.take() {
                if !is_try_catch {
                    if change_in_try {
                        self.internal_storage.in_try_block = false;
                    }
                    return NULL;
                }

                self.err = None;
                while let Some(ref_err) = err.ref_err.take() {
                    err = *ref_err;
                }

                match exception_vars.len() {
                    1 => {
                        let mut tuple_vals = vec![
                            Value::String(err.err_type.clone()),
                            Value::String(err.err_msg.clone()),
                        ];
                        if let Some(help) = &err.help {
                            tuple_vals.push(Value::String(help.clone()));
                        }
                        let val = Value::Tuple(tuple_vals);
                        self.variables.insert(
                            exception_vars[0].clone(),
                            Variable::new(exception_vars[0].clone(), val, "tuple".to_string(), false, true, true),
                        );
                    }
                    2 => {
                        self.variables.insert(
                            exception_vars[0].clone(),
                            Variable::new(
                                exception_vars[0].clone(),
                                Value::String(err.err_type.clone()),
                                "str".to_string(),
                                false,
                                true,
                                true,
                            ),
                        );
                        self.variables.insert(
                            exception_vars[1].clone(),
                            Variable::new(
                                exception_vars[1].clone(),
                                Value::String(err.err_msg.clone()),
                                "str".to_string(),
                                false,
                                true,
                                true,
                            ),
                        );
                    }
                    3 => {
                        self.variables.insert(
                            exception_vars[0].clone(),
                            Variable::new(
                                exception_vars[0].clone(),
                                Value::String(err.err_type.clone()),
                                "str".to_string(),
                                false,
                                true,
                                true,
                            ),
                        );
                        self.variables.insert(
                            exception_vars[1].clone(),
                            Variable::new(
                                exception_vars[1].clone(),
                                Value::String(err.err_msg.clone()),
                                "str".to_string(),
                                false,
                                true,
                                true,
                            ),
                        );
                        self.variables.insert(
                            exception_vars[2].clone(),
                            Variable::new(
                                exception_vars[2].clone(),
                                err.help.clone().map_or(NULL, |h| Value::String(h)),
                                "str".to_string(),
                                false,
                                true,
                                true,
                            ),
                        );
                    }
                    _ => {}
                }

                let mut err_result = NULL;
                for catch_stmt in catch_body {
                    err_result = self.evaluate(catch_stmt);
                }

                if change_in_try {
                    self.internal_storage.in_try_block = false;
                }

                return err_result;
            }
        }

        if change_in_try {
            self.internal_storage.in_try_block = false;
        }

        result
    }

    fn handle_assignment(&mut self, left: Statement, right: Statement) -> Value {
        let right_value = self.evaluate(&right);
        let loc_id = left.loc.clone();

        if self.err.is_some() {
            return NULL;
        }

        match left.node {
            Node::Variable { name } => {
                let expected_type: Type = {
                    let var = match self.variables.get(&name) {
                        Some(v) => v,
                        None => {
                            let suggestion = if let Value::Null = right_value {
                                format!(
                                    "Did you mean to use '{}' instead of '='?",
                                    wrap_in_help(":=", &self.config)
                                )
                            } else {
                                format!(
                                    "Use this instead: '{}{}: {} = {}{}'",
                                    check_ansi("\x1b[4m", &self.config.supports_color),
                                    name,
                                    right_value.get_type().display_simple(),
                                    format_value(&right_value),
                                    check_ansi("\x1b[24m", &self.config.supports_color),
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
    
                let var = self.variables.get_mut(&name).unwrap();
    
                if var.is_final() {
                    return self.raise(
                        "AssignmentError",
                        &format!("Cannot assign to final variable '{}'", name),
                    );
                }
    
                var.set_value(right_value);
                var.value.clone()
            }
            Node::IndexAccess { .. } => {
                fn value_to_usize(val: &Value) -> Result<usize, Value> {
                    match val {
                        Value::Int(i) => {
                            let v = i.to_i64().map_err(|_| Value::new_error("TypeError", "Index must be integer", None))?;
                            if v >= 0 { Ok(v as usize) } else { Err(Value::new_error("TypeError", "Index must be non-negative", None)) }
                        }
                        Value::Float(f) => {
                            let v = f.to_f64().map_err(|_| Value::new_error("TypeError", "Index must be number", None))?;
                            if v.fract() == 0.0 && v >= 0.0 { Ok(v as usize) } else { Err(Value::new_error("TypeError", "Index must be non-negative whole number", None)) }
                        }
                        _ => Err(Value::new_error("TypeError", "Index must be integer or whole float", None)),
                    }
                }

                fn get_single_index(interpreter: &mut Interpreter, access: &AccessType) -> Result<Value, (String, String)> {
                    if let AccessType::Single { index } = access {
                        let val = interpreter.evaluate(index);
                        if let Some(err) = interpreter.err.take() {
                            return Err((err.err_type.to_owned(), err.err_msg.to_owned()));
                        }
                        Ok(val)
                    } else {
                        Err(("TypeError".to_string(), "Only single index access supported".to_string()))
                    }
                }

                fn assign_nested(container: &mut Value, indices: &[Value], right_value: Value) -> Result<(), Value> {
                    if indices.is_empty() {
                        *container = right_value;
                        return Ok(());
                    }

                    let current_index = &indices[0];

                    match container {
                        Value::List(l) => {
                            let idx = value_to_usize(current_index)?;
                            if idx >= l.len() { return Err(Value::new_error("IndexError", "List index out of range", None)); }
                            assign_nested(&mut l[idx], &indices[1..], right_value)?;
                        }
                        Value::Tuple(t) => {
                            let idx = value_to_usize(current_index)?;
                            if idx >= t.len() { return Err(Value::new_error("IndexError", "Tuple index out of range", None)); }
                            assign_nested(&mut t[idx], &indices[1..], right_value)?;
                        }
                        Value::Map(m) => {
                            let key = current_index.clone();
                            if !m.contains_key(&key) {
                                m.insert(key.clone(), Value::Null);
                            }
                            let val = m.get_mut(&key).unwrap();
                            assign_nested(val, &indices[1..], right_value)?;
                        }
                        Value::String(s) => {
                            if indices.len() != 1 { return Err(Value::new_error("TypeError", "Cannot index deeper into a string", None)); }
                            let idx = value_to_usize(current_index)?;
                            if idx >= s.len() { return Err(Value::new_error("IndexError", "String index out of range", None)); }
                            let replacement = right_value.to_string();
                            if replacement.chars().count() != 1 { return Err(Value::new_error("TypeError", "String assignment must be a single character", None)); }
                            let mut chars: Vec<char> = s.chars().collect();
                            chars[idx] = replacement.chars().next().unwrap();
                            *s = chars.into_iter().collect();
                        }
                        Value::Bytes(b) => {
                            if indices.len() != 1 { return Err(Value::new_error("TypeError", "Cannot index deeper into bytes", None)); }
                            let idx = value_to_usize(current_index)?;
                            if idx >= b.len() { return Err(Value::new_error("IndexError", "Bytes index out of range", None)); }
                            let byte_val = match right_value {
                                Value::Int(i) => i.to_i64().unwrap_or(0) as u8,
                                Value::Float(f) => f.to_f64().unwrap_or(0.0) as u8,
                                Value::String(s) => s.parse::<u8>().unwrap_or(0),
                                _ => return Err(Value::new_error("TypeError", "Expected numeric value for byte assignment", None)),
                            };
                            b[idx] = byte_val;
                        }
                        _ => return Err(Value::new_error("TypeError", "Object not indexable", None)),
                    }

                    Ok(())
                }

                fn collect_indices(interpreter: &mut Interpreter, stmt: &Statement) -> Result<(String, Vec<Value>), (String, String)> {
                    let mut indices = vec![];
                    let mut current_stmt = stmt;

                    loop {
                        match &current_stmt.node {
                            Node::Variable { name } => return Ok((name.clone(), indices)),
                            Node::IndexAccess { object, access } => {
                                let idx = get_single_index(interpreter, access)?;
                                indices.push(idx);
                                current_stmt = object;
                            }
                            _ => return Err(("TypeError".to_string(), "Invalid assignment target".to_string())),
                        }
                    }
                }

                let (var_name, mut indices) = match collect_indices(self, &left) {
                    Ok(v) => v,
                    Err((k, msg)) => return self.raise(&k, &msg),
                };

                indices.reverse();

                let var = match self.variables.get_mut(&var_name) {
                    Some(v) => v,
                    None => return self.raise("NameError", &format!("Variable '{}' not found", var_name)),
                };

                if var.is_final() {
                    return self.raise("AssignmentError", &format!("Cannot assign to final variable '{}'", var_name));
                }

                if let Err(err) = assign_nested(&mut var.value, &indices, right_value) {
                    return self.raise("TypeError", &format!("{}", err));
                }

                var.value.clone()
            }
            Node::PropertyAccess { object, property_name } => {
                let object_val = self.evaluate(&object);
                if self.err.is_some() {
                    return NULL;
                }
                let o = match object_val {
                    Value::Struct(mut s) => {
                        let is_final = s.get_field_mods(&property_name)
                            .map_or(false, |(_, _, f)| f);

                        if is_final {
                            return self.raise(
                                "AssignmentError",
                                &format!("Cannot assign to final property '{}'", property_name),
                            );
                        }

                        if let Some((box_val, expected_type)) = s.fields.get_mut(&property_name) {
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
                let var_name = match &object.node {
                    Node::Variable { name } => name.clone(),
                    _ => {
                        return self.raise("TypeError", "Can only assign to properties of variables");
                    }
                };
                if self.variables.contains_key(&var_name) {
                    let var = self.variables.get_mut(&var_name).unwrap();
                    var.set_value(o);
                }
                NULL
            }
            // fuck my parser
            Node::Pointer { value, ptr_node } => {
                match ptr_node {
                    PtrNode::PointerDeref { deref_level } => {
                        let result = self.evaluate(
                            &Statement {
                                node: Node::Pointer {
                                    value: Box::new(right.clone()),
                                    ptr_node: PtrNode::PointerAssign { 
                                        target: value,
                                        assign_level: deref_level,
                                    }
                                },
                                loc: loc_id,
                            }
                        );
                        if self.err.is_some() {
                            return NULL;
                        }
                        return result;
                    }
                    PtrNode::PointerRef { .. } => return self.raise("TypeError", "Cannot assign to pointer referencing"),
                    PtrNode::PointerAssign { .. } => return self.raise("TypeError", "Cannot assign to pointer assignment"),
                };
            }
            n => self.raise("TypeError", &format!("Cannot assign to {}", n.name())),
        }
    }

    fn handle_index_access(&mut self, object_stmt: Statement, access_type: AccessType) -> Value {
        let object_val = self.evaluate(&object_stmt);
        if self.err.is_some() { return NULL; }

        if let Value::Map(map)= &object_val {
            if let AccessType::Single { index } = access_type {
                let key_val = self.evaluate(&index);
                if self.err.is_some() { return NULL; }

                if let Some(val) = map.get(&key_val) {
                    return val.clone();
                } else {
                    return self.raise("KeyError", &format!("Key '{}' not found in map", format_value(&key_val)));
                }
            } else {
                return self.raise("TypeError", "Maps only support single key access");
            }
        }

        if let Value::Type(t) = &object_val {
            match t {
                Type::Enum { variants, .. } => {
                    if let AccessType::Single { index } = access_type {
                        let index_val = self.evaluate(&index);
                        if self.err.is_some() { return NULL; }
                        let idx = match self.to_index(&index_val, usize::MAX) {
                            Ok(i) => i,
                            Err(e) => return e,
                        };
                        for (name, ty, i) in variants {
                            if *i == idx {
                                if *ty != Statement::Null {
                                    return self.raise("TypeError", &format!("Enum variant '{}' requires associated value", name));
                                }
                                return Value::Enum(Box::new(Enum::new(t.clone(), (i.clone(), NULL))));
                            }
                        }
                        return self.raise("AttributeError", &format!("Enum variant not found at index '{}'", index_val));
                    } else {
                        return self.raise("TypeError", "Enum variant access requires a single variant name");
                    }
                }
                _ => {
                    return self.raise("TypeError", &format!("Type '{}' is not indexable", t.display_simple()));
                }
            }
        }

        let len = match &object_val {
            Value::String(s) => s.chars().count(),
            Value::List(l) => l.len(),
            Value::Bytes(b) => b.len(),
            Value::Tuple(t) => t.len(),
            _ => return self.raise("TypeError", &format!("'{}' not indexable", object_val.get_type().display_simple())),
        };

        let (start_idx, end_idx, is_single) = match access_type {
            AccessType::Single { index } => {
                let idx_val = self.evaluate(&index);
                if self.err.is_some() { return NULL; }
                let idx = match self.to_index(&idx_val, len) {
                    Ok(i) => i,
                    Err(e) => return e,
                };
                (idx, idx + 1, true)
            }
            AccessType::Range { start, end } => {
                let s_val = self.evaluate(&start);
                let e_val = self.evaluate(&end);
                if self.err.is_some() { return NULL; }
                let s_idx = if s_val == NULL { 0 } else { self.to_index(&s_val, len+1).unwrap_or(0) };
                let e_idx = if e_val == NULL { len } else { self.to_index(&e_val, len+1).unwrap_or(len) };
                if s_idx == e_idx {
                    (s_idx, e_idx, true)
                } else {
                    (s_idx, e_idx, false)
                }
            }
            AccessType::ToStart { end } => {
                let e_val = self.evaluate(&end);
                if self.err.is_some() { return NULL; }
                let e_idx = if e_val == NULL { len } else { self.to_index(&e_val, len+1).unwrap_or(len) };
                (0, e_idx, false)
            }
            AccessType::ToEnd { start } => {
                let s_val = self.evaluate(&start);
                if self.err.is_some() { return NULL; }
                let s_idx = if s_val == NULL { 0 } else { self.to_index(&s_val, len+1).unwrap_or(0) };
                (s_idx, len, false)
            }
            AccessType::Full => (0, len, false),
        };

        if start_idx > end_idx || end_idx > len {
            return match &object_val {
                Value::String(_) => self.raise("IndexError", &format!("String slice out of bounds: [{}..{}] in string of length {}", start_idx, end_idx, len)),
                Value::List(_) => self.raise("IndexError", &format!("List slice out of bounds: [{}..{}] in list of length {}", start_idx, end_idx, len)),
                Value::Bytes(_) => self.raise("IndexError", &format!("Bytes slice out of bounds: [{}..{}] in bytes of length {}", start_idx, end_idx, len)),
                Value::Tuple(_) => self.raise("IndexError", &format!("Tuple slice out of bounds: [{}..{}] in tuple of length {}", start_idx, end_idx, len)),
                _ => self.raise("TypeError", "Object not sliceable"),
            };
        }

        match &object_val {
            Value::String(s) => {
                let chars: Vec<char> = s.chars().collect();
                if is_single {
                    chars.get(start_idx).map(|&c| Value::String(c.to_string())).unwrap_or(NULL)
                } else {
                    let slice: String = chars.get(start_idx..end_idx).unwrap_or(&[]).iter().collect();
                    Value::String(slice)
                }
            }
            Value::List(l) => {
                let slice = l.get(start_idx..end_idx).unwrap_or(&[]).to_vec();
                if is_single { slice.into_iter().next().unwrap_or(NULL) } else { Value::List(slice) }
            }
            Value::Bytes(b) => {
                let slice = b.get(start_idx..end_idx).unwrap_or(&[]).to_vec();
                if is_single { slice.get(0).map(|&v| Value::Int(v.into())).unwrap_or(NULL) } else { Value::Bytes(slice) }
            }
            Value::Tuple(t) => {
                let slice = t.get(start_idx..end_idx).unwrap_or(&[]).to_vec();
                if is_single { slice.into_iter().next().unwrap_or(NULL) } else { Value::Tuple(slice) }
            }
            _ => self.raise("TypeError", "Object not sliceable"),
        }
    }

    fn handle_variable_declaration(&mut self, name: String, val_stmt: Statement, var_type: Statement, modifiers: Vec<String>) -> Value {
        let value = self.evaluate(&val_stmt);
        if self.err.is_some() {
            return NULL;
        }
    
        let mut declared_type: Type = match self.evaluate(&var_type) {
            Value::Type(t) => t,
            Value::Null => return NULL,
            t => return self.raise("TypeError", &format!("Expected a type for variable type, got {}", t.type_name())),
        };
        if self.err.is_some() {
            return NULL;
        }

        if declared_type == Type::new_simple("auto") {
            declared_type = value.get_type();
        }

        let mut is_public = false;
        let mut is_static = false;
        let mut is_final = false;

        for modifier in modifiers {
            match modifier.as_str() {
                "public" => is_public = true,
                "private" => is_public = false,
                "static" => is_static = true,
                "final" => is_final = true,
                "non-static" => is_static = false,
                "mutable" => is_final = false,
                _ => return self.raise("ModifierError", &format!("Unknown modifier: {}", modifier)),
            }
        }

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

        self.debug_log(
            format_args!("<Declared variable '{}': {} = {}>", name, value.get_type().display_simple(), format_value(&value))
        );
    
        value   
    }

    #[inline]
    fn handle_variable(&mut self, name: &str) -> Value {
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

                let call_result = self.call_function(&func, vec![val.clone()], HashMap::new(), Some((None, None, Some(&mut object_variable))));
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
                return Value::Enum(Box::new(Enum::new(candidates[0].1.clone(), (idx, Value::Null))));
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
                            check_ansi("\x1b[4m", &self.config.supports_color),
                            name,
                            candidate.display(),
                            check_ansi("\x1b[24m", &self.config.supports_color),
                        ),
                    );
                }
            }

            let available_names: Vec<String> = self.variables.keys().cloned().collect();
            if let Some(closest) = find_closest_match(&name, &available_names) {
                return self.raise_with_help(
                    "NameError",
                    &format!("Variable '{}' is not defined.", name),
                    &format!(
                        "Did you mean '{}{}{}'?",
                        check_ansi("\x1b[4m", &self.config.supports_color),
                        closest,
                        check_ansi("\x1b[24m", &self.config.supports_color),
                    ),
                );
            } else {
                return self.raise("NameError", &format!("Variable '{}' is not defined.", name));
            }
        }
    }
    
    #[inline]
    fn handle_for_loop(&mut self, node: ForLoopNode, body: &[Statement]) -> Value {
        match node {
            ForLoopNode::ForIn { variable, iterable } => {
                let iterable_value = self.evaluate(&iterable);
                if self.err.is_some() { return NULL; }
                if !iterable_value.is_iterable() {
                    return self.raise(
                        "TypeError",
                        &format!("Expected an iterable for 'for' loop, got {}", iterable_value.type_name())
                    );
                }

                let mut result = NULL;
                let iter_items = iterable_value.iter();

                let mut prev_vars: Vec<Option<Variable>> = Vec::new();
                let mut binding_names: Vec<String> = Vec::new();
                
                for item in iter_items {
                    if self.check_stop_flag() { return NULL; }

                    if let Value::Error(ref e) = item {
                        self.err = Some(match e.ref_err {
                            Some(ref re) => Error::with_ref(&e.err_type, &e.err_msg, *re.clone(), &self.file_path),
                            None => Error::new(&e.err_type, &e.err_msg, &self.file_path),
                        });
                        return NULL;
                    }

                    let bindings = match check_pattern(&item, &variable) {
                        Ok((true, vars)) => vars,
                        Ok((false, _)) => return self.raise("ValueError", "Item did not match for-loop variable pattern"),
                        Err((etype, emsg)) => return self.raise(&etype, &emsg),
                    };

                    if binding_names.is_empty() || binding_names.len() != bindings.len() {
                        binding_names.clear();
                        prev_vars.clear();
                        for name in bindings.keys() {
                            binding_names.push(name.clone());
                            prev_vars.push(self.variables.remove(name));
                        }
                    } else {
                        for (i, name) in binding_names.iter().enumerate() {
                            prev_vars[i] = self.variables.remove(name);
                        }
                    }

                    for (name, val) in &bindings {
                        self.variables.insert(name.clone(), Variable::new_pt(
                            name.clone(), val.clone(), val.get_type(), false, true, true,
                        ));
                    }

                    let mut should_break = false;
                    for stmt in body {
                        result = self.evaluate(stmt);
                        if self.err.is_some() || self.is_returning {
                            for (name, prev_var) in binding_names.iter().zip(prev_vars.iter()) {
                                if let Some(var) = prev_var {
                                    self.variables.insert(name.clone(), var.clone());
                                }
                            }
                            return result;
                        }
                        match self.state {
                            State::Break => { self.state = State::Normal; should_break = true; break; },
                            State::Continue => { self.state = State::Normal; break; },
                            _ => {}
                        }
                    }

                    for (name, prev_var) in binding_names.iter().zip(prev_vars.iter()) {
                        if let Some(var) = prev_var {
                            self.variables.insert(name.clone(), var.clone());
                        }
                    }

                    if should_break { break; }
                }

                result
            }

            ForLoopNode::Standard { initializer, condition, update } => {
                self.evaluate(&initializer);
                if self.err.is_some() { return NULL; }

                let mut result = NULL;
                loop {
                    if self.check_stop_flag() { return NULL; }

                    let cond_value = self.evaluate(&condition);
                    if self.err.is_some() { return NULL; }
                    if !cond_value.is_truthy() { break; }

                    let mut should_break = false;
                    for stmt in body {
                        result = self.evaluate(stmt);
                        if self.err.is_some() || self.is_returning { return result; }
                        match self.state {
                            State::Break => { self.state = State::Normal; should_break = true; break; },
                            State::Continue => { self.state = State::Normal; break; },
                            _ => {}
                        }
                    }
                    
                    if should_break { break; }

                    self.evaluate(&update);
                    if self.err.is_some() { return NULL; }
                }

                result
            }
        }
    }

    fn handle_iterable(&mut self, iterable: IterableNode) -> Value {
        match iterable {
            IterableNode::List { elements } => {
                let mut evaluated_elements = Vec::with_capacity(elements.len());
                for element in elements {
                    evaluated_elements.push(self.evaluate(&element));
                    if self.err.is_some() {
                        return NULL;
                    }
                }
                Value::List(evaluated_elements)
            }
            IterableNode::Tuple { elements } => {
                let mut evaluated_elements = Vec::with_capacity(elements.len());
                for element in elements {
                    evaluated_elements.push(self.evaluate(&element));
                    if self.err.is_some() {
                        return NULL;
                    }
                }
                Value::Tuple(evaluated_elements)
            }
            IterableNode::ListCompletion { seed, end, pattern_flag, range_mode, is_infinite: is_inf } => {
                let evaluated_seed: Vec<Value> = seed.into_iter().map(|map_val| {
                    let r = self.evaluate(&map_val);
                    if self.err.is_some() {
                        NULL
                    } else {
                        r
                    }
                }).collect();

                if self.err.is_some() {
                    return NULL;
                }

                let evaluated_end = self.evaluate(&end);

                let cache_key = match range_mode {
                    RangeModeType::Value => Value::List(vec![
                        Value::List(evaluated_seed.clone()),
                        evaluated_end.clone(),
                        Value::String(range_mode.to_string()),
                        Value::Boolean(pattern_flag),
                    ]),
                    RangeModeType::Length => {
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
                            Value::String(range_mode.to_string()),
                            Value::Boolean(pattern_flag),
                        ])
                    }
                };

                let mut pattern_method: Option<PatternMethod> = None;

                if let Some(Value::Map(map)) = self.cache.iterables.get(&cache_key) {
                    if let Some(cached_value) = map.get(&cache_key) {
                        if self.config.debug {
                            let seed_str = format_value(&Value::List(evaluated_seed.clone()));
                            let end_str = format_value(&evaluated_end);

                            self.debug_log(format_args!(
                                "<CachedListCompletion>\\A  seed: {}\\A  end: {}\\A  range_mode: {}\\A  pattern: {}",
                                seed_str,
                                end_str,
                                range_mode,
                                pattern_flag
                            ));
                        }

                        return cached_value.clone();
                    }
                }


                let result: Value = match range_mode {
                    RangeModeType::Value => {
                        if !pattern_flag {
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
                    RangeModeType::Length => {
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

                        if pattern_flag {
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
                };

                self.cache.iterables.insert(cache_key, result.clone());

                let pattern_method_str = match pattern_method {
                    Some(pm) => format!("\\A  method used: {}", pm.full()),
                    None => "".to_string(),
                };

                let is_inf_str = if is_inf {
                    "\\A  infinite: true".to_string()
                } else {
                    "".to_string()
                };

                if self.config.debug {
                    let seed_str = format_value(&Value::List(evaluated_seed.clone()));
                    let end_str = format_value(&evaluated_end);

                    self.debug_log(format_args!(
                        "<ListCompletion>\\A  seed: {}\\A  end: {}\\A  range_mode: {}\\A  pattern: {}{}{}",
                        seed_str,
                        end_str,
                        range_mode,
                        pattern_flag,
                        is_inf_str,
                        pattern_method_str
                    ));
                }

                return result;
            }
            IterableNode::ListComprehension { for_clauses, map_expression } => {
                fn eval_nested(
                    interpreter: &mut Interpreter,
                    clauses: &[(PathElement, Statement, Vec<Statement>)],
                    depth: usize,
                    result: &mut Vec<Value>,
                    map_expression: &Statement
                ) {
                    if depth >= clauses.len() {
                        let mapped_value = interpreter.evaluate(map_expression);
                        if interpreter.err.is_some() { return; }
                        result.push(mapped_value);
                        return;
                    }

                    let clause = &clauses[depth];

                    let pattern = &clause.0;
                    let iterable = interpreter.evaluate(&clause.1);
                    if interpreter.err.is_some() { return; }
                    if !iterable.is_iterable() {
                        interpreter.raise("TypeError", &format!("Value of type '{}' is not iterable", iterable.get_type().display_simple()));
                        return;
                    }

                    let filter_conditions: Vec<Statement> = clause.2.clone();

                    for item in iterable.iter() {
                        if interpreter.check_stop_flag() { return; }

                        let bindings = match check_pattern(&item, &pattern) {
                            Ok((true, new_bindings)) => new_bindings,
                            Ok((false, _)) => {
                                interpreter.raise("ValueError", "Pattern did not match item in list comprehension");
                                return;
                            }
                            Err((err_type, msg)) => {
                                interpreter.raise(&err_type, &msg);
                                return;
                            }
                        };

                        let prev_vars: Vec<Option<Variable>> = bindings.keys()
                            .map(|name| interpreter.variables.get(name).cloned())
                            .collect();

                        for (name, val) in &bindings {
                            interpreter.variables.insert(
                                name.clone(),
                                Variable::new(name.clone(), val.clone(), val.type_name(), false, true, true)
                            );
                        }

                        let mut skip = false;
                        for cond in &filter_conditions {
                            if !interpreter.evaluate(cond).is_truthy() || interpreter.err.is_some() {
                                skip = true;
                                break;
                            }
                        }

                        if skip {
                            for (name, prev) in bindings.keys().zip(prev_vars.iter()) {
                                match prev {
                                    Some(v) => { interpreter.variables.insert(name.clone(), v.clone()); },
                                    None => { interpreter.variables.remove(name); },
                                }
                            }
                            continue;
                        }

                        eval_nested(interpreter, clauses, depth + 1, result, map_expression);

                        for (name, prev) in bindings.keys().zip(prev_vars.iter()) {
                            match prev {
                                Some(v) => { interpreter.variables.insert(name.clone(), v.clone()); },
                                None => { interpreter.variables.remove(name); },
                            }
                        }

                        match interpreter.state {
                            State::Break => { interpreter.state = State::Normal; break; },
                            State::Continue => { interpreter.state = State::Normal; continue; },
                            _ => {}
                        }
                    }
                }
                let result = {
                    let mut flat_result = Vec::with_capacity(for_clauses.len() * 4);
                    eval_nested(self, &for_clauses, 0, &mut flat_result, &map_expression);
                    Value::List(flat_result)
                };

                result
            }
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

    fn handle_method_call(
        &mut self,
        object_stmt: Statement,
        method_name: &str,
        pos_args_stmt: Vec<Statement>,
        named_args_stmt: HashMap<String, Statement>,
    ) -> Value {
        let var_name_opt = match &object_stmt.node {
            Node::Variable { name } => Some(name.clone()),
            _ => None,
        };

        let mut object_value = match &object_stmt.node {
            Node::Variable { name } => {
                match self.variables.get(name) {
                    Some(v) => v.get_value().clone(),
                    None => return self.raise("NameError", &format!("Variable '{}' not found", name)),
                }
            }
            _ => {
                let value = self.evaluate(&object_stmt);
                if self.err.is_some() {
                    return NULL;
                }
                value
            }
        };

        if let Value::Type(t) = &object_value {
            if let Type::Enum { name, variants, generics, .. } = t {
                let variant_name = method_name;
                let variant = match variants.iter().find(|(s, _, _)| s == variant_name) {
                    Some(v) => v,
                    None => {
                        return self.raise_with_help(
                            "TypeError",
                            &format!("Variant '{}' not found in enum '{}'", variant_name, name),
                            &format!(
                                "Available variants are: {}",
                                {
                                    let v: Vec<_> = variants.iter().map(|(n, _, _)| n.clone()).collect();
                                    if v.len() > 7 { format!("{}...", v[..5].join(", ")) } else { v.join(", ") }
                                }
                            ),
                        )
                    }
                };

                if !named_args_stmt.is_empty() {
                    return self.raise(
                        "SyntaxError",
                        &format!("Unexpected named arguments in enum variant '{}.{}'", name, variant_name),
                    );
                }

                let (variant_name, variant_ty, _) = variant;

                if *variant_ty == Statement::Null {
                    return self.raise(
                        "TypeError",
                        &format!("Variant '{}.{}' takes no arguments", name, variant_name),
                    );
                }

                let saved_variables = std::mem::take(&mut self.variables);
                self.variables.extend(generics.iter().map(|g| {
                    (
                        g.clone(),
                        Variable::new(
                            g.clone(),
                            Value::Type(Type::new_simple("any")),
                            "type".to_string(),
                            false,
                            true,
                            true,
                        ),
                    )
                }));
                self.variables = saved_variables;

                let mut pos_args = Vec::with_capacity(pos_args_stmt.len());
                for s in pos_args_stmt {
                    pos_args.push(self.evaluate(&s));
                    if self.err.is_some() { return NULL; }
                }

                let variant_val = match pos_args.len() {
                    0 => NULL,
                    1 => pos_args.pop().unwrap(),
                    _ => Value::Tuple(pos_args),
                };

                let idx = match get_enum_idx(t, variant_name) {
                    Some(u) => u,
                    None => return self.raise("NameError", &format!("Enum variant '{}' not defined.", variant_name)),
                };

                return Value::Enum(Box::new(Enum::new(t.clone(), (idx, variant_val))));
            }
        }

        let pos_args: Vec<Value> = pos_args_stmt.iter().map(|s| self.evaluate(s)).collect();
        if self.err.is_some() { return NULL; }

        let named_args: HashMap<String, Value> =
            named_args_stmt.iter().map(|(k, v)| (k.clone(), self.evaluate(v))).collect();
        if self.err.is_some() { return NULL; }

        let is_object = !is_type_ident_available(&object_value);

        let func: Function = if !is_object {
            let it = get_object_type_ident(&object_value);
            let tf = get_default_type_method(method_name, &it);
            if let Some(f) = tf {
                f.clone()
            } else {
                let type_methods = get_type_method_names(&object_value);
                if let Some(m) = find_closest_match(method_name, &type_methods) {
                    return self.raise_with_help(
                        "NameError",
                        &format!("No method '{}' for type '{}'", method_name, object_value.get_type().display_simple()),
                        &format!("Did you mean '{}'?", m),
                    );
                } else {
                    return self.raise(
                        "NameError",
                        &format!("No method '{}' for type '{}'", method_name, object_value.get_type().display_simple()),
                    );
                }
            }
        } else {
            let mut temp_var = Variable::new(
                var_name_opt.clone().unwrap_or_else(|| "_".to_string()),
                object_value.clone(),
                object_value.type_name(),
                false,
                true,
                true,
            );

            if let Some((var_name, props)) = self.get_properties(temp_var.get_value(), false) {
                temp_var.properties = props;
                temp_var.set_name(var_name);
            }

            match temp_var.properties.get(method_name) {
                Some(v) => match v.get_value() {
                    Value::Function(f) => f.clone(),
                    other => return self.raise("TypeError", &format!("Expected function, got {}", other)),
                },
                None => {
                    return self.raise(
                        "NameError",
                        &format!("No method '{}' in '{}'", method_name, temp_var.get_name()),
                    )
                },
            }
        };

        let mut object_var = Variable::new(
            var_name_opt.clone().unwrap_or_else(|| "_".to_string()),
            object_value.clone(),
            object_value.type_name(),
            false,
            true,
            true,
        );

        let result = self.call_function(
            &func,
            pos_args,
            named_args,
            Some((Some(&mut object_value), None, Some(&mut object_var))),
        );

        if self.err.is_some() {
            return NULL;
        }

        // i hate my language
        if let Some(var_name) = var_name_opt {
            if let Some(original_var) = self.variables.get_mut(&var_name) {
                if is_object {
                    *original_var = object_var;
                } else {
                    original_var.set_value(object_value);
                }
            }
        }

        result
    }

    fn handle_property_access(&mut self, object: Statement, property_name: &str) -> Value {
        let object_value = self.evaluate(&object);
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
                            None => return self.raise_with_help("TypeError", &format!("Variant '{}' not found in enum '{}'", variant_name, name), &format!("Available variants are: {}", { let v: Vec<_> = variants.iter().map(|(n, _, _)| n.clone()).collect(); if v.len() > 7 { format!("{}... (and {} more)", v[..5].join(", "), v.len() - 5) } else { v.join(", ") }})),
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
                        return Value::Enum(Box::new(Enum::new(
                            t.clone(),
                            (idx, NULL)
                        )));
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
                            check_ansi("\x1b[4m", &self.config.supports_color),
                            closest,
                            check_ansi("\x1b[24m", &self.config.supports_color),
                        ),
                    );
                } else {
                    return self.raise("NameError", &format!("No property '{}' in '{}'", property_name, object_variable.get_name()));
                }
            }
        };
        var.get_value().clone()
    }
    
    #[inline]
    fn handle_call(&mut self, function_name: &str, pos_args_stmt: Vec<Statement>, named_args_stmt: HashMap<String, Statement>) -> Value {
        let fast_path_func = if named_args_stmt.is_empty() {
            if let Some((cached_name, cached_func)) = &self.cache.last_called_function {
                if cached_name == function_name && cached_func.is_native() {
                    let meta = cached_func.metadata();
                    let param_count = meta.parameters.len();
                    let arg_count = pos_args_stmt.len();
                    if arg_count == param_count && !meta.parameters.iter().any(|p| p.kind == ParameterKind::Variadic) {
                        Some((cached_func.clone(), meta.parameters.iter().map(|p| p.name.clone()).collect::<Vec<_>>()))
                    } else {
                        None
                    }
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        };
        
        if let Some((func, param_names)) = fast_path_func {
            let mut args = HashMap::with_capacity(param_names.len());
            for (i, stmt) in pos_args_stmt.into_iter().enumerate() {
                let val = self.evaluate(&stmt);
                if self.err.is_some() {
                    return NULL;
                }
                args.insert(param_names[i].clone(), val);
            }
            
            if func.is_natively_callable() {
                return func.call(&args);
            } else {
                return func.call_shared(&args, self);
            }
        }
        
        let special_functions = [
            "exit", "breakpoint", "fetch", "exec", "eval", "warn", "as_method", "module", "00__set_cfg__", "00__set_dir__"
        ];

        let mut pos_args = Vec::with_capacity(pos_args_stmt.len());
        for stmt in pos_args_stmt {
            let r = self.evaluate(&stmt);
            if self.err.is_some() {
                return NULL;
            }
            pos_args.push(r);
        }

        let mut named_args = HashMap::with_capacity(named_args_stmt.len());
        for (k, v) in named_args_stmt {
            let r = self.evaluate(&v);
            if self.err.is_some() {
                return NULL;
            }
            named_args.insert(k, r);
        }

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
                    doc: None,
                },
            }))
        } else {
            if self.cache.last_called_function.as_ref().map_or(false, |(name, _)| name == function_name) {
                self.cache.last_called_function.as_ref().unwrap().1.clone()
            } else {
                match self.variables.get(function_name) {
                    Some(v) => match v.get_value() {
                        Value::Function(f) => {
                            self.cache.last_called_function = Some((function_name.to_string(), f.clone()));
                            f.clone()
                        },
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
                                return Value::Enum(Box::new(Enum::new(candidates[0].1.clone(), (idx, variant_value))));
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
                                    check_ansi("\x1b[4m", &self.config.supports_color),
                                    closest,
                                    check_ansi("\x1b[24m", &self.config.supports_color),
                                ),
                            );
                        } else {
                            return self.raise("NameError", &format!("Function '{}' is not defined", function_name));
                        }
                    }
                }
            }
        };

        self.call_function(&func, pos_args, named_args, if is_special_function { Some((None, Some(function_name.to_string()), None)) } else { None })
    }

    pub fn call_function(
        &mut self,
        func: &Function,
        pos_args: Vec<Value>,
        named_args: HashMap<String, Value>,
        mut special: Option<(Option<&mut Value>, Option<String>, Option<&mut Variable>)>,
    ) -> Value {
        let (special_value_opt, special_name_opt, mut object_variable) = special.take()
            .unwrap_or((None, None, None));

        let (special_value, is_special_value): (&mut Value, bool) = match special_value_opt {
            Some(v) => (v, true),
            None => (&mut Value::Null, false),
        };

        let is_special_function = special_name_opt.is_some();

        let metadata = if !is_special_function {
            func.metadata().clone()
        } else {
            let special_name = special_name_opt
                .expect("special function name missing");
            special_function_meta()
                .get(special_name.as_str())
                .expect("special function metadata missing")
                .clone()
        };

        let function_name = metadata.name.as_str();

        let is_module = object_variable
            .as_ref()
            .map(|obj_var| matches!(obj_var.get_value(), Value::Module(_)))
            .unwrap_or(false);

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
                            check_ansi("\x1b[4m", &self.config.supports_color),
                            alt_name,
                            check_ansi("\x1b[24m", &self.config.supports_color)
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
                            check_ansi("\x1b[4m", &self.config.supports_color),
                            alt_name,
                            check_ansi("\x1b[24m", &self.config.supports_color)
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
                            check_ansi("\x1b[4m", &self.config.supports_color), alt_name, check_ansi("\x1b[24m", &self.config.supports_color)
                        ),
                    );
                } else {
                    self.warn(&state);
                }
        }
        
        let positional: Vec<Value> = pos_args;
        let mut named_map: HashMap<String, Value> = named_args;
        let mut final_args: HashMap<String, (Value, Vec<String>)> = HashMap::with_capacity(metadata.parameters.len());

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
                                        check_ansi("\x1b[4m", &self.config.supports_color),
                                        param_name,
                                        param_type.display_simple(),
                                        format_value(&positional[pos_index.saturating_sub(1)]).to_string(),
                                        check_ansi("\x1b[24m", &self.config.supports_color)
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
                                        check_ansi("\x1b[4m", &self.config.supports_color),
                                        param_name,
                                        param_type.display_simple(),
                                        format_value(&positional[pos_index - 1]).to_string(),
                                        check_ansi("\x1b[24m", &self.config.supports_color)
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
            self.debug_log(
                format_args!(
                    "<Call: {}.{}({})>",
                    obj_var.get_name(),
                    function_name,
                    final_args_no_mods
                        .iter()
                        .map(|(k, v)| format!("{}: {}", k, format_value(v)))
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            );
        } else {
            self.debug_log(
                format_args!(
                    "<Call: {}({})>",
                    function_name,
                    final_args_no_mods
                        .iter()
                        .map(|(k, v)| format!("{}: {}", k, format_value(v)))
                        .collect::<Vec<String>>()
                        .join(", ")
                )
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

        let mut result;

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
                    self.stack.pop();
                    self.state = State::Exit;
                }
                "breakpoint" => {
                    let condition = if let Some(condition) = final_args_no_mods.get("condition") {
                        condition.is_truthy()
                    } else {
                        true
                    };
                    let message = if let Some(message) = final_args_no_mods.get("message") {
                        message.clone()
                    } else {
                        Value::Null
                    };
                    let message = if let Value::String(s) = message {
                        Some(s)
                    } else {
                        None
                    };
                    self.stack.pop();
                    if !condition {
                        return NULL;
                    }
                    return self.breakpoint(message.as_deref());
                }
                "fetch" => {
                    self.stack.pop();
                    return self.fetch_fn(&final_args_no_mods);
                }
                "exec" => {
                    self.stack.pop();
                    if let Some(Value::String(script_str)) = final_args_no_mods.get("code") {
                        self.debug_log(
                            format_args!("<Exec script: '{}'>", script_str)
                        );
                        let lexer = Lexer::new(to_static(script_str.clone()), to_static(self.file_path.clone()));
                        let tokens = lexer.tokenize();

                        let mut parser = Parser::new(
                            tokens,
                        );
                        let statements = parser.parse();
                        let (return_value, _state, err, _variables) = self.run_isolated(self.variables.clone(), &statements, false);
                        if let Some(err) = err {
                            self.raise_with_ref("RuntimeError", "Error in exec script", err);
                        }
                        return return_value.clone();
                    } else {
                        return self.raise("TypeError", "Expected a string in 'code' argument in exec");
                    }
                }
                "eval" => {
                    self.stack.pop();
                    if let Some(Value::String(script_str)) = final_args_no_mods.get("code") {
                        self.eval(script_str);
                        if self.err.is_some() {
                            return NULL;
                        }
                    } else {
                        return self.raise("TypeError", "Expected a string in 'code' argument in eval");
                    }
                }
                "warn" => {
                    self.stack.pop();
                    let msg = if let Some(msg) = final_args_no_mods.get("message") {
                        msg.clone()
                    } else {
                        Value::new_error("ValueError", "Missing 'message' argument in warn", None)
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
                    if let Some(func) = final_args_no_mods.get("function") {
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
                    if let Some(Value::String(code)) = final_args_no_mods.get("code") {
                        let name = if let Some(Value::String(n)) = final_args_no_mods.get("name") {
                            n.clone()
                        } else {
                            "<module>".to_string()
                        };
                        let path = if let Some(Value::String(p)) = final_args_no_mods.get("path") {
                            p.clone()
                        } else {
                            "<module>".to_string()
                        };
                        let properties = self.get_properties_from_str(&code, Some(Path::new(&path)));
                        let module = Value::Module(Module {
                            name,
                            properties,
                            parameters: Vec::new(),
                            is_public: false,
                            is_static: false,
                            is_final: false,
                            state: None,
                            path: PathBuf::from(path)
                        });
                        return module;
                    } else {
                        return self.raise("TypeError", "Expected a string in 'path' argument in module");
                    }
                }
                "00__set_cfg__" => {
                    self.stack.pop();
                    if let Some(Value::String(key)) = final_args_no_mods.get("key") {
                        if let Some(value) = final_args_no_mods.get("value") {
                            if let Value::Int(num_wrapper) = value {
                                if let Ok(num) = num_wrapper.to_i64() {
                                    if !self.config.allow_inline_config && num != 0x6767 {
                                        return self.raise("PermissionError", "Modifying configuration at runtime is disabled.");
                                    }
                                    if num == 0x6969 {
                                        let default_val = get_from_config(&self.og_cfg.clone(), key);
                                        match set_in_config(&mut self.config, key, default_val.clone()) {
                                            Ok(_) => {}
                                            Err(err) => {
                                                return self.raise("KeyError", &err);
                                            }
                                        }
                                        self.debug_log(
                                            format_args!("<Reset config: {} to default>", key)
                                        );
                                        return NULL;
                                    } else if num == 0x6767 {
                                        let val = get_from_config(&self.config.clone(), key);
                                        self.debug_log(
                                            format_args!("<Get config: {} = {}>", key, format_value(&val))
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
                            self.debug_log(
                                format_args!("<Set config: {} = {}>", key, format_value(value))
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
            let mut merged_variables = self.variables.clone();
            merged_variables.extend(closure_vars.clone());
            merged_variables.extend(final_args_variables);
            let body = func.get_body();
            let (return_value, _state, err, variables) = self.run_isolated(merged_variables, &body, false);
            result = return_value.clone();
            if change_in_function {
                self.internal_storage.in_function = false;
            }
            if let Some(err) = err {
                self.stack.pop();
                self.err = Some(err);
                return NULL;
            }
            if let Value::Error(e) = &result {
                if let Some(rerr) = &e.ref_err {
                    let err = Error::with_ref(
                        &e.err_type,
                        &e.err_msg,
                        *(rerr.clone()),
                        &self.file_path,
                    );
                    self.raise_with_ref(
                        "RuntimeError",
                        "Error in lambda call",
                        err,
                    );
                }
                let err = Error::new(
                    &e.err_type,
                    &e.err_msg,
                    &self.file_path,
                );
                self.raise_with_ref(
                    "RuntimeError",
                    "Error in lambda call",
                    err,
                );
                self.stack.pop();
                return NULL;
            }
            if !effect_flags.contains(EffectFlags::UNKNOWN) {
                match effect_flags.check_branches(self.collected_effects.clone(), effect_mask) {
                    Some((false, extra_bits)) => {
                        let names = extra_bits.get_names();
                        return self.raise_with_help(
                            "EffectError",
                            &format!("Function '{}' has unexpected side effects: {}", function_name, names.join(", ")),
                            &format!("Consider adding '{}[{}, ...]{}' to the function's effect annotations.", hex_to_ansi(&self.config.color_scheme.note, self.config.supports_color), names.join(", "), hex_to_ansi(&self.config.color_scheme.help, self.config.supports_color)),
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
                if let Some(v) = variables.get(&var) {
                    if let Some(ref mut object_variable) = object_variable {
                        object_variable.set_value(v.get_value().clone());
                    }
                }
            }
            self.stack = self.stack.clone();
        } else if let Function::CustomMethod(func) = func {
            let interpreter_arc = func.get_interpreter();
            let mut interpreter = interpreter_arc.lock();
            interpreter.config = self.config.clone();
            interpreter.internal_storage.in_try_block = self.internal_storage.in_try_block;
            interpreter.internal_storage.in_function = true;

            let tmp_self_name = "__method_self_tmp".to_string();
            let tmp_self_val = if let Some(ref obj_var) = object_variable { obj_var.get_value().clone() } else { Value::Null };
            interpreter.variables.insert(
                tmp_self_name.clone(),
                Variable::new(tmp_self_name.clone(), tmp_self_val.clone(), tmp_self_val.type_name(), false, true, true),
            );

            let mut tmp_obj_local = interpreter.variables.get(&tmp_self_name).unwrap().clone();

            result = interpreter.call_function(
                &Function::Custom(func.get_function()),
                positional,
                named_map,
                Some((None, None, Some(&mut tmp_obj_local))),
            );

            interpreter.variables.insert(tmp_self_name.clone(), tmp_obj_local.clone());

            if change_in_function {
                self.internal_storage.in_function = false;
            }

            for var in instance_variables.iter() {
                if let Some(ref mut object_variable) = object_variable {
                    if let Some(v) = interpreter.variables.get_mut(var) {
                        object_variable.set_value(v.get_value().clone());
                        continue;
                    }
                    if let Some(vtmp) = interpreter.variables.get_mut(&tmp_self_name) {
                        object_variable.set_value(vtmp.get_value().clone());
                        continue;
                    }
                }
            }

            interpreter.variables.remove(&tmp_self_name);

            if interpreter.err.is_some() {
                self.stack.pop();
                self.raise_with_ref(
                    "RuntimeError",
                    "Error in module method call",
                    interpreter.err.clone().unwrap(),
                );
                return NULL;
            }

            if let Value::Error(e) = &result {
                if let Some(rerr) = &e.ref_err {
                    let err = Error::with_ref(
                        &e.err_type,
                        &e.err_msg,
                        *(rerr.clone()),
                        &self.file_path,
                    );
                    self.raise_with_ref(
                        "RuntimeError",
                        "Error in method call",
                        err,
                    );
                }
                let err = Error::new(
                    &e.err_type,
                    &e.err_msg,
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
                            &format!("Consider adding '{}[{}, ...]{}' to the function's effect annotations.", hex_to_ansi(&self.config.color_scheme.note, self.config.supports_color), names.join(", "), hex_to_ansi(&self.config.color_scheme.help, self.config.supports_color)),
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
        } else if let Function::NativeMethod(func) = func {
            result = func.call_method(
                special_value,
                &final_args_no_mods,
            );

            if change_in_function {
                self.internal_storage.in_function = false;
            }
        } else if !is_module {
            self.stack.push((function_name.to_string(), self.get_location_from_current_statement(), StackType::MethodCall));
            if !func.is_native() {
                let body = func.get_body();
                let func_ptr = func.ptr();
                let func_meta = func.metadata();
                
                let mut merged_variables = self.variables.clone();
                merged_variables.extend(final_args_variables);
                
                let use_fast_path = effect_flags.contains(EffectFlags::UNKNOWN) && instance_variables.is_empty();
                
                if use_fast_path {
                    self.scopes.push(function_name.to_string(), func_ptr);
                    
                    let (ret_val, err) = self.run_function_body_fast(
                        merged_variables,
                        &body,
                        func_ptr,
                        func_meta,
                    );
                    
                    self.scopes.pop();
                    
                    if let Some(e) = err {
                        self.stack.pop();
                        if change_in_function {
                            self.internal_storage.in_function = false;
                        }
                        self.raise_with_ref("RuntimeError", "Error in function call", e);
                        return NULL;
                    }
                    result = ret_val;
                } else {
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
                        &self.file_path.clone(),
                        &self.cwd.clone(),
                        self.preprocessor_info.clone(),
                        &argv_vec,
                    );
                    new_interpreter.variables.extend(merged_variables);
                    new_interpreter.internal_storage.in_try_block = self.internal_storage.in_try_block;
                    new_interpreter.internal_storage.in_function = true;
                    new_interpreter.stack = self.stack.clone();
                    let _ = new_interpreter.interpret(body.to_vec(), true);
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
                                    &format!("Consider adding '{}[{}, ...]{}' to the function's effect annotations.", hex_to_ansi(&self.config.color_scheme.note, self.config.supports_color), names.join(", "), hex_to_ansi(&self.config.color_scheme.help, self.config.supports_color)),
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
                    result = new_interpreter.return_value.clone();
                    for var in instance_variables {
                        if let Some(v) = new_interpreter.variables.get_mut(&var) {
                            if let Some(ref mut o) = object_variable {
                                o.set_value(v.get_value().clone());
                            }
                        }
                    }
                    self.stack = new_interpreter.stack;
                }
                
                if change_in_function {
                    self.internal_storage.in_function = false;
                }
            } else {
                if func.is_natively_callable() {
                    result = func.call(&final_args_no_mods);
                } else {
                    let mut new_args_no_mods = final_args_no_mods;
                    if is_special_value {
                        new_args_no_mods.insert(
                            "self".to_string(),
                            special_value.clone(),
                        );
                    }
                    result = func.call_shared(&new_args_no_mods, self);
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
                let _ = new_interpreter.interpret(body.to_vec(), true);
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
                if let Value::Error(e) = &result {
                    if let Some(rerr) = &e.ref_err {
                        let err = Error::with_ref(
                            &e.err_type,
                            &e.err_msg,
                            *(rerr.clone()),
                            &module_file_path.display().to_string(),
                        );
                        self.raise_with_ref(
                            "RuntimeError",
                            "Error in method call",
                            err,
                        );
                    }
                    let err = Error::new(
                        &e.err_type,
                        &e.err_msg,
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
                                &format!("Consider adding '{}[{}, ...]{}' to the function's effect annotations.", hex_to_ansi(&self.config.color_scheme.note, self.config.supports_color), names.join(", "), hex_to_ansi(&self.config.color_scheme.help, self.config.supports_color)),
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
                    let mut new_args_no_mods = final_args_no_mods;
                    if is_special_value {
                        new_args_no_mods.insert(
                            "self".to_string(),
                            special_value.clone(),
                        );
                    }
                    result = func.call_shared(&new_args_no_mods, self);
                }
                if change_in_function {
                    self.internal_storage.in_function = false;
                }
            }
        }
        self.stack.pop();
        self.debug_log(
            format_args!("<Function '{}' returned {}>", function_name, format_value(&result)),
        );
        if let Value::Error(e) = &result {
            return self.raise_err(e.as_ref().clone());
        }
        if let Value::Struct(s_box) = &mut result {
            if let Type::Struct { name, .. } = &s_box.ty {
                if let Some(var) = self.variables.get(name) {
                    if let Value::Type(ty) = &var.value {
                        if matches!(ty, Type::Struct { .. }) {
                            s_box.ty = ty.clone();
                        }
                    }
                }
            }
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
        return result;
    }

    #[inline]
    fn handle_operation(&mut self, left_stmt: Statement, operator: String, right_stmt: Statement) -> Value {
        let left = self.evaluate(&left_stmt);
        if self.err.is_some() {
            return NULL;
        }

        // Short-circuit for '&&' and '||'
        match operator.as_str() {
            "&&" | "and" => {
                if !left.is_truthy() {
                    return Value::Boolean(false);
                }
            }
            "||" | "or" => {
                if left.is_truthy() {
                    return Value::Boolean(true);
                }
            }
            _ => {}
        }

        let right = self.evaluate(&right_stmt);
        if self.err.is_some() {
            return NULL;
        }

        let left = if let Value::Boolean(b) = left && !(["&" , "|", "xnor", "xor", "<<", "lshift", ">>", "rshift"].contains(&operator.as_str())) {
            Value::Int(if b { 1.into() } else { 0.into() })
        } else {
            left
        };
        let right = if let Value::Boolean(b) = right && !(["&" , "|", "xnor", "xor", "<<", "lshift", ">>", "rshift"].contains(&operator.as_str())) {
            Value::Int(if b { 1.into() } else { 0.into() })
        } else {
            right
        };

        let (left, right) = match (&left, &right) {
            (Value::Pointer(lp), Value::Pointer(rp)) => {
                let (l_val, _) = lp.lock().clone();
                let (r_val, _) = rp.lock().clone();
                (l_val, r_val)
            }
            // fuck auto deref
            // (Value::Pointer(lp), r) => {
            //     let l_val = lp.lock().clone();
            //     (l_val, r.clone())
            // }
            // (l, Value::Pointer(rp)) => {
            //     let r_val = rp.lock().unwrap().clone();
            //     (l.clone(), r_val)
            // }
            _ => (left.clone(), right.clone()),
        };

        if self.err.is_some() {
            return NULL;
        }

        if self.internal_storage.use_42.0 && !self.internal_storage.use_42.1 {
            if let (Value::Int(left_val), op, Value::Int(right_val)) = (&left, &operator, &right) {
                if *left_val == Int::from(6) && *op == "*" && *right_val == Int::from(9) {
                    self.internal_storage.use_42.1 = true;
                    self.debug_log(
                        format_args!("<Operation: 6 * 9 -> 42>")
                    );
                    return Value::Int(Int::from(42));
                }
            }
        }

        let cache_key = format!("{}::{}::{}", format_value(&left), format_value(&right), &operator);

        if let Some(cached) = self.cache.operations.get(&cache_key) {
            return cached.clone();
        }

        let result = self.make_operation(left, right, &operator);

        if self.err.is_some() {
            return NULL;
        }

        self.cache.operations.insert(cache_key, result.clone());
        result
    }
    
    #[inline]
    fn handle_unary_op(&mut self, operand_stmt: Statement, operator: &str) -> Value {
        if self.err.is_some() {
            return NULL;
        }

        let operand = self.evaluate(&operand_stmt);
        if self.err.is_some() {
            return NULL;
        }

        let operator = match operator {
            "isnt" | "isn't" | "nein" | "not" => "!",
            other => other,
        };

        let cache_key = format!("{}{}", operator, format_value(&operand));

        if let Some(cached) = self.cache.operations.get(&cache_key) {
            return cached.clone();
        }

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

        let func = match find_struct_method(&s1, s2_val.as_ref().map(|v| &**v), func_name, &expected_params, &ret_type) {
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

        if func.is_static() {
            self.call_function(
                &func,
                if let Some(s2) = s2_val { vec![Value::Struct(s1), Value::Struct(s2)] } else { vec![Value::Struct(s1)] },
                HashMap::new(),
                Some((None, None, Some(&mut object_variable))),
            )
        } else {
            self.call_function(
                &func,
                if let Some(s2) = s2_val { vec![Value::Struct(s2)] } else { vec![] },
                HashMap::new(),
                Some((None, None, Some(&mut object_variable))),
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
                            Value::Int((l / r).unwrap_or_else(|_| {
                                self.raise("TypeError", "Integer division failed");
                                return Int::new();
                            }))
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
                if right.is_iterable() {
                    Value::Boolean(right.iter().any(|item| item == left))
                } else {
                    self.raise("TypeError", "Right operand must be iterable for 'in' operation");
                    NULL
                }
            },
            "&" | "band" => match (left, right) {
                (Value::Int(a), Value::Int(b)) => {
                    let res = match a & b {
                        Ok(val) => val,
                        Err(_) => return self.raise("ValueError", "Bitwise AND failed"),
                    };
                    Value::Int(res)
                }
                _ => self.raise("TypeError", "Bitwise AND requires two integers"),
            },
            "|" | "bor" => match (left, right) {
                (Value::Int(a), Value::Int(b)) => {
                    let res = match a | b {
                        Ok(val) => val,
                        Err(_) => return self.raise("ValueError", "Bitwise OR failed"),
                    };
                    Value::Int(res)
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
                    let res = match a << b {
                        Ok(val) => val,
                        Err(_) => return self.raise("ValueError", "Left shift amount too large"),
                    };
                    Value::Int(res)
                }
                _ => self.raise("TypeError", "Left shift requires two integers"),
            },
            ">>" | "rshift" => match (left, right) {
                (Value::Int(a), Value::Int(b)) => {
                    let res = match a >> b {
                        Ok(val) => val,
                        Err(_) => return self.raise("ValueError", "Right shift amount too large"),
                    };
                    Value::Int(res)
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
                    (Value::Int(a), Value::Int(b)) => {
                        let res = match a ^ b {
                            Ok(res) => res,
                            Err(_) => return self.raise("TypeError", "Int xor failed"),
                        };
                        Value::Int(res)
                    }
                    (Value::Float(a), Value::Float(b)) => {
                        let res = match a ^ b {
                            Ok(res) => res,
                            Err(_) => return self.raise("TypeError", "Float xor failed"),
                        };
                        Value::Float(res)
                    },
                    (Value::String(a), Value::String(b)) => Value::Boolean(a != b),
                    (a, b) => self.raise("TypeError", &format!("Cannot apply 'xor' to {} and {}", a.type_name(), b.type_name())),
                }
            },
            "xnor" => {
                match (left, right) {
                    (Value::Boolean(a), Value::Boolean(b)) => Value::Boolean(!(a ^ b)),
                    (Value::Int(a), Value::Int(b)) => {
                        let xor = match a ^ b {
                            Ok(res) => res,
                            Err(_) => return self.raise("TypeError", "Int xnor failed"),
                        };
                        Value::Int(!xor)
                    },
                    (Value::Float(a), Value::Float(b)) => {
                        let xor = match a ^ b {
                            Ok(res) => res,
                            Err(_) => return self.raise("TypeError", "Float xnor failed"),
                        };
                        Value::Float(!xor)
                    },
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
                let saved_drtch = self.config.disable_runtime_type_checking;
                self.config.disable_runtime_type_checking = false;
                let res = self.check_type(&left, &t).0;
                self.config.disable_runtime_type_checking = saved_drtch;
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

    fn handle_number(&mut self, value: String) -> Value {
        if !value.contains('_') && !value.contains('.') && !value.contains('e') && !value.contains('E') 
           && !value.starts_with("0x") && !value.starts_with("0X") 
           && !value.starts_with("0b") && !value.starts_with("0B")
           && !value.starts_with("0o") && !value.starts_with("0O")
           && !value.contains('#') && !value.ends_with('i') {
            
            let trimmed = value.trim().trim_start_matches('0').trim_start_matches('+');
            let final_str = if trimmed.is_empty() { "0" } else { trimmed };
            
            if let Ok(int_val) = Int::from_str(final_str) {
                if final_str.len() < 10 {
                    self.cache.constants.insert(value, Value::Int(int_val.clone()));
                }
                return Value::Int(int_val);
            }
        }

        let original_len = value.len();
        
        let mut s = if value.contains('_') {
            value.replace('_', "")
        } else {
            value
        };

        if s.is_empty() {
            return self.raise("RuntimeError", "Empty string provided for number");
        }
    
        if let Some(cached) = self.cache.constants.get(&s) {
            self.debug_log(
                format_args!("<CachedConstantNumber: {}>", s)
            );
            return cached.clone();
        }

        let is_imaginary = s.ends_with('i');
        if is_imaginary {
            s.truncate(s.len() - 1);
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

        let result = if s.len() > 2 {
            let bytes = s.as_bytes();
            if bytes[0] == b'0' {
                match bytes[1] {
                    b'b' | b'B' => {
                        parse_int_with_base(&s[2..].to_ascii_lowercase(), 2)
                            .map(Value::Int)
                            .unwrap_or_else(|_| self.raise("RuntimeError", "Invalid format for binary integer literal."))
                    }
                    b'o' | b'O' => {
                        parse_int_with_base(&s[2..].to_ascii_lowercase(), 8)
                            .map(Value::Int)
                            .unwrap_or_else(|_| self.raise("RuntimeError", "Invalid format for octal integer literal."))
                    }
                    b'x' | b'X' => {
                        parse_int_with_base(&s[2..].to_ascii_lowercase(), 16)
                            .map(Value::Int)
                            .unwrap_or_else(|_| self.raise("RuntimeError", "Invalid format for hex integer literal."))
                    }
                    _ => self.parse_number_fallback(&s)
                }
            } else {
                self.parse_number_fallback(&s)
            }
        } else {
            self.parse_number_fallback(&s)
        };
    
        let cacheable = match &result {
            Value::Float(f) => {
                f.to_f64().map_or(false, |val| val.is_finite() && val.abs() < 1e15)
            }
            Value::Int(_i) => {
                original_len < 50
            }
            _ => false,
        };        
    
        if cacheable {
            self.cache.constants.insert(s.clone(), result.clone());
        }
    
        if is_imaginary {
            return match result {
                Value::Int(i) => {
                    match Float::from_int(&i) {
                        Ok(f) => Value::Float(Float::Complex(Box::new(Float::from(0.0)), Box::new(f))),
                        Err(_) => self.raise("RuntimeError", "Failed to convert Int to Float for imaginary number"),
                    }
                }
                Value::Float(f) => Value::Float(Float::Complex(Box::new(Float::from(0.0)), Box::new(f))),
                _ => self.raise("RuntimeError", "Invalid imaginary number format"),
            };
        }

        result
    }

    fn parse_number_fallback(&mut self, s: &str) -> Value {
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

        if let Some(hash_pos) = s.find('#') {
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
                    return Value::Int(val);
                } else {
                    return self.raise("RuntimeError", "Invalid digits for base 1 literal, only '1's allowed");
                }
            } else {
                match parse_int_with_base(digits, base) {
                    Ok(val) => return Value::Int(val),
                    Err(_) => return self.raise("RuntimeError", &format!("Invalid digits for {} base integer literal.", base_str)),
                }
            }
        } else if s.contains('.') || s.to_ascii_lowercase().contains('e') {
            if is_number_parentheses(&s) {
                Value::Float(imagnum::create_float(&s))
            } else {
                let f = match Float::from_str(&s) {
                    Ok(f) => f,
                    Err(_) => return self.raise("RuntimeError", "Invalid float format"),
                };

                if s.to_ascii_lowercase().contains('e') && f.is_integer_like() {
                    match f.to_int() {
                        Ok(i) => Value::Int(i),
                        Err(_) => return self.raise("RuntimeError", "Failed to convert float to int"),
                    }
                } else {
                    Value::Float(f)
                }
            }
        } else {
            let trimmed = s.trim().trim_start_matches('0').trim_start_matches('+');
            let final_str = if trimmed.is_empty() { "0" } else { trimmed };
            match Int::from_str(final_str) {
                Ok(i) => Value::Int(i),
                Err(_) => self.raise("RuntimeError", "Invalid integer format"),
            }
        }
    }
    
    fn handle_string(&mut self, value: String, mods: Vec<char>) -> Value {
        if mods.is_empty() {
            return match unescape_string(&value) {
                Ok(unescaped) => Value::String(unescaped),
                Err(e) => self.raise("UnicodeError", &e),
            };
        }

        if mods.len() == 1 {
            match mods[0] {
                'r' => {
                    return if value.len() >= 2 {
                        let bytes = value.as_bytes();
                        let first = bytes[0];
                        let last = bytes[bytes.len() - 1];
                        if (first == b'"' && last == b'"') || (first == b'\'' && last == b'\'') {
                            Value::String(value[1..value.len() - 1].to_string())
                        } else {
                            Value::String(value)
                        }
                    } else {
                        Value::String(value)
                    };
                }
                'b' => {
                    let processed = match unescape_string(&value) {
                        Ok(unescaped) => unescaped,
                        Err(e) => return self.raise("UnicodeError", &e),
                    };
                    return Value::Bytes(processed.into_bytes());
                }
                _ => {}
            }
        }

        let mut modified_string = value;
        let mut is_raw = false;
        let mut is_bytes = false;

        for modifier in mods {
            match modifier {
                'f' => {
                    modified_string = match self.process_f_string(modified_string) {
                        Ok(result) => result,
                        Err(_) => return NULL,
                    };
                }
                'r' => is_raw = true,
                'b' => is_bytes = true,
                _ => return self.raise("RuntimeError", &format!("Unknown string modifier: {}", modifier)),
            }
        }

        if is_raw {
            if modified_string.len() >= 2 {
                let bytes = modified_string.as_bytes();
                let first = bytes[0];
                let last = bytes[bytes.len() - 1];
                if (first == b'"' && last == b'"') || (first == b'\'' && last == b'\'') {
                    modified_string = modified_string[1..modified_string.len() - 1].to_string();
                }
            }
        } else {
            modified_string = match unescape_string(&modified_string) {
                Ok(unescaped) => unescaped,
                Err(e) => return self.raise("UnicodeError", &e),
            };
        }

        if is_bytes {
            return Value::Bytes(modified_string.into_bytes());
        }

        Value::String(modified_string)
    }

    fn process_f_string(&mut self, input: String) -> Result<String, ()> {
        if !input.contains('{') {
            return Ok(input);
        }

        let mut output = String::with_capacity(input.len() * 2);
        let input_bytes = input.as_bytes();
        let mut i = 0;

        while i < input_bytes.len() {
            let c = input_bytes[i] as char;
            
            if c == '{' {
                if i + 1 < input_bytes.len() && input_bytes[i + 1] == b'{' {
                    output.push('{');
                    i += 2;
                    continue;
                }

                let expr_start = i + 1;
                let mut expr_end = expr_start;
                let mut brace_level = 1;
                let mut in_string: Option<u8> = None;
                let mut has_format_spec = false;
                let mut format_spec_start = 0;

                while expr_end < input_bytes.len() && brace_level > 0 {
                    let byte = input_bytes[expr_end];
                    
                    if let Some(quote) = in_string {
                        if byte == quote {
                            in_string = None;
                        }
                    } else if byte == b'"' || byte == b'\'' {
                        in_string = Some(byte);
                    } else if byte == b'{' {
                        brace_level += 1;
                    } else if byte == b'}' {
                        brace_level -= 1;
                        if brace_level == 0 {
                            break;
                        }
                    } else if brace_level == 1 && byte == b':' && 
                             expr_end + 1 < input_bytes.len() && input_bytes[expr_end + 1] == b':' {
                        has_format_spec = true;
                        format_spec_start = expr_end + 2;
                        expr_end += 1;
                    }
                    
                    expr_end += 1;
                }

                if brace_level != 0 {
                    self.raise("SyntaxError", "Unmatched '{' in f-string");
                    return Err(());
                }

                let actual_expr_end = if has_format_spec { format_spec_start - 2 } else { expr_end };
                let expr = std::str::from_utf8(&input_bytes[expr_start..actual_expr_end])
                    .map_err(|_| {
                        self.raise("UnicodeError", "Invalid UTF-8 in f-string expression");
                    })?;

                let format_spec = if has_format_spec {
                    let spec_bytes = &input_bytes[format_spec_start..expr_end];
                    Some(std::str::from_utf8(spec_bytes).map_err(|_| {
                        self.raise("UnicodeError", "Invalid UTF-8 in format spec");
                    })?.trim().to_string())
                } else {
                    None
                };

                let result = if expr.trim().chars().all(|c| c.is_alphanumeric() || c == '_') {
                    let var_name = expr.trim();
                    if let Some(var) = self.variables.get(var_name) {
                        var.get_value().clone()
                    } else {
                        self.evaluate_f_string_expression(expr)?
                    }
                } else {
                    self.evaluate_f_string_expression(expr)?
                };

                let formatted_result = if let Some(spec) = format_spec {
                    match apply_format_spec(&result, &spec, self) {
                        Ok(res) => res,
                        Err(e) => {
                            self.raise("SyntaxError", &e);
                            return Err(());
                        }
                    }
                } else {
                    match escape_string(&native::format_value(&result, self)) {
                        Ok(res) => res,
                        Err(e) => {
                            self.raise("UnicodeError", &e);
                            return Err(());
                        }
                    }
                };

                output.push_str(&formatted_result);
                i = expr_end + 1;
            } else if c == '}' {
                if i + 1 < input_bytes.len() && input_bytes[i + 1] == b'}' {
                    output.push('}');
                    i += 2;
                } else {
                    self.raise("SyntaxError", "Single '}' encountered in format string");
                    return Err(());
                }
            } else {
                output.push(c);
                i += 1;
            }
        }

        Ok(output)
    }

    fn evaluate_f_string_expression(&mut self, expr: &str) -> Result<Value, ()> {
        // let unescaped_expr = match unescape_string_premium_edition(expr) {
        //     Ok(unescaped) => unescaped,
        //     Err(err) => {
        //         self.raise("UnescapeError", &err);
        //         return Err(());
        //     }
        // };
        let unescaped_expr = expr.to_string();
        let tokens = Lexer::new(&unescaped_expr, &self.file_path.clone()).tokenize();

        let filtered = tokens.iter()
            .filter(|token| {
                let t = &token.0;
                t != "WHITESPACE" && !t.starts_with("COMMENT_") && t != "EOF"
            })
            .collect::<Vec<_>>();

        let formatted_toks = filtered.iter()
            .map(|token| (&token.0, &token.1))
            .collect::<Vec<_>>();

        self.debug_log(
            format_args!("Generated f-string tokens: {:?}", formatted_toks)
        );

        let tokens_no_loc: Vec<Token> = tokens
            .into_iter()
            .map(|tk| Token(tk.0, tk.1, None))
            .collect();

        let parsed = match Parser::new(tokens_no_loc).parse_safe() {
            Ok(parsed) => parsed,
            Err(error) => {
                self.raise("SyntaxError", &format!("Error parsing f-string expression: {}", error.err_msg));
                return Err(());
            }
        };

        self.debug_log(
            format_args!(
                "Generated f-string statement: {}",
                parsed.iter().map(|stmt| stmt.format_for_debug()).collect::<Vec<String>>().join(", ")
            )
        );

        self.debug_log(
            format_args!("<FString: {}>", expr.trim())
        );

        if parsed.is_empty() {
            self.raise("SyntaxError", "Empty f-string expression");
            return Err(());
        }

        let result = self.evaluate(&parsed[0]);
        if self.err.is_some() {
            return Err(());
        }

        Ok(result)
    }

    #[inline]
    fn handle_boolean(&mut self, value: Option<bool>) -> Value {
        match value {
            Some(true) => TRUE,
            Some(false) => FALSE,
            None => NULL,
        }
    }
}
