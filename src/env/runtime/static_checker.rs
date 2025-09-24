#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unused_variables)]

use crate::env::runtime::config::Config;
use crate::env::runtime::errors::Error;
use crate::env::runtime::value::Value;
use crate::env::runtime::statements::Statement;
use crate::env::runtime::utils::{fix_path, hex_to_ansi, to_static, special_function_meta};
use crate::env::runtime::types::{Type, VALID_TYPES};
use crate::env::runtime::tokens::Location;
use crate::env::runtime::internal_structs::State;
use crate::env::runtime::functions::FunctionMetadata;
use crate::env::runtime::modules::Module;
use crate::env::runtime::native;
use std::panic::Location as PanicLocation;
use std::collections::{HashSet, HashMap};

#[derive(Clone, Debug, PartialEq)]
pub enum ValueType {
    Float,
    Int,
    String,
    Boolean,
    Null,
    Map {
        keys: Vec<ValueType>,
    },
    Tuple(Vec<ValueType>),
    List(Vec<ValueType>),
    Bytes(usize),
    Type(Type),
    Function(FunctionMetadata),
    Generator(FunctionMetadata),
    Module(Module),
    Enum(String, String), // name, variant
    Struct(String), // name
    Pointer(Box<ValueType>),
    Error(String, String, Option<Error>), // type, msg, ref_err
    Any,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    pub value_type: ValueType,
    pub type_: Type,
    pub is_mutable: bool,
    pub is_static: bool,
    pub is_public: bool,
}

impl Variable {
    pub fn new(value_type: ValueType, type_: Type, is_mutable: bool, is_static: bool, is_public: bool) -> Self {
        Self { value_type, type_, is_mutable, is_static, is_public }
    }
}

#[derive(Debug, Clone)]
pub struct CheckerState {
    pub defined_vars: HashMap<String, Variable>,
    pub used_vars: HashSet<String>,
    pub assigned_vars: HashSet<String>,
    pub loop_vars: HashSet<String>,
}

#[derive(Debug, Clone)]
pub enum ErrorTypes {
    NotImplemented,
    RuntimeError,
    UndefinedVariable,
    TypeMismatch,
    SyntaxError,
}

#[derive(Debug, Clone)]
pub enum WarningTypes {
    UnusedVariable,
    UnusedFunction,
    UnusedValue,
    UnreachableCode,
}

#[derive(Debug, Clone)]
pub struct Checker {
    config: Config,
    og_cfg: Config,
    pub errors: Vec<Error>,
    pub is_returning: bool,
    pub return_value: Value,
    pub state: CheckerState,
    pub code_state: State,
    file_path: String,
    current_statement: Option<Statement>,

    // cannot be as an enum because we need to be in multiple states at once
    pub in_loop: bool,
    pub in_function: bool,
    pub in_generator: bool,
    pub in_struct: bool,
    pub in_try: bool,
    pub in_catch: bool,
    pub in_defer: bool,
    pub in_match: bool,
    pub in_scope: bool,
}

impl Checker {
    pub fn new(config: Config, file_path: String) -> Self {
        let og_cfg = config.clone();
        let mut this = Self {
            config,
            og_cfg,
            errors: vec![],
            is_returning: false,
            return_value: Value::Null,
            state: CheckerState {
                defined_vars: HashMap::new(),
                used_vars: HashSet::new(),
                assigned_vars: HashSet::new(),
                loop_vars: HashSet::new(),
            },
            code_state: State::Normal,
            file_path,
            current_statement: None,

            in_loop: false,
            in_function: false,
            in_generator: false,
            in_struct: false,
            in_catch: false,
            in_try: false,
            in_defer: false,
            in_match: false,
            in_scope: false,
        };
        let mut builtins: HashMap<String, Variable> = HashMap::new();
        builtins.insert("_".to_string(), Variable::new(ValueType::Any, Type::new_simple("any"), false, true, true));
        builtins.insert("_err".to_string(), Variable::new(ValueType::Tuple(vec![ValueType::String, ValueType::String, ValueType::String]), Type::new_simple("any"), false, true, true)); // (type, msg, ref_err)
        builtins.insert("__dir__".to_string(), Variable::new(ValueType::String, Type::new_simple("any"), false, true, true));
        builtins.insert("__file__".to_string(), Variable::new(ValueType::String, Type::new_simple("any"), false, true, true));
        builtins.insert("argv".to_string(), Variable::new(ValueType::List(vec![ValueType::String; 100]), Type::new_simple("any"), false, true, true)); // up to 100 args
        builtins.insert("print".to_string(), Variable::new(ValueType::Function(native::print_fn().metadata().clone()), Type::new_simple("any"), false, true, true));
        builtins.insert("styledprint".to_string(), Variable::new(ValueType::Function(native::styled_print_fn().metadata().clone()), Type::new_simple("any"), false, true, true));
        builtins.insert("input".to_string(), Variable::new(ValueType::Function(native::input_fn().metadata().clone()), Type::new_simple("any"), false, true, true));
        builtins.insert("len".to_string(), Variable::new(ValueType::Function(native::len_fn().metadata().clone()), Type::new_simple("any"), false, true, true));
        builtins.insert("help".to_string(), Variable::new(ValueType::Function(native::help_fn().metadata().clone()), Type::new_simple("any"), false, true, true));
        builtins.insert("type_of".to_string(), Variable::new(ValueType::Function(native::type_fn().metadata().clone()), Type::new_simple("any"), false, true, true));
        builtins.insert("size_of".to_string(), Variable::new(ValueType::Function(native::size_of_fn().metadata().clone()), Type::new_simple("any"), false, true, true));
        builtins.insert("sum".to_string(), Variable::new(ValueType::Function(native::sum_fn().metadata().clone()), Type::new_simple("any"), false, true, true));
        builtins.insert("ord".to_string(), Variable::new(ValueType::Function(native::ord_fn().metadata().clone()), Type::new_simple("any"), false, true, true));
        builtins.insert("char".to_string(), Variable::new(ValueType::Function(native::char_fn().metadata().clone()), Type::new_simple("any"), false, true, true));
        builtins.insert("styledstr".to_string(), Variable::new(ValueType::Function(native::styledstr_fn().metadata().clone()), Type::new_simple("any"), false, true, true));
        builtins.extend(special_function_meta().into_iter().map(|(k, v)| (k.to_string(), Variable::new(ValueType::Function(v), Type::new_simple("any"), false, true, true))));
        for (var, val) in builtins {
            this.state.defined_vars.insert(var.to_string(), val); // built-in vars are immutable and public and native
        }
        for t in VALID_TYPES.iter() {
            this.state.defined_vars.insert(t.to_string(), Variable::new(ValueType::Type(Type::new_simple(t)), Type::new_simple("any"), false, true, true)); // built-in types are immutable and public and native
        }
        this
    }

    fn check_err_type(&self, error_type: ErrorTypes) -> (bool, &str) {
        let (res, internal_defs) = match error_type {
            ErrorTypes::NotImplemented => ("NotImplementedError", vec!["not_implemented_error", "not_implemented"]),
            ErrorTypes::UndefinedVariable => ("UndefinedVariable", vec!["undefined_variable"]),
            ErrorTypes::TypeMismatch => ("TypeMismatch", vec!["type_mismatch"]),
            ErrorTypes::RuntimeError => ("RuntimeError", vec!["runtime_error"]),
            ErrorTypes::SyntaxError => ("SyntaxError", vec!["syntax_error"]),
        };
        if self.config.type_checker.ignore_errors.iter().any(|e| internal_defs.contains(&e.as_str())) {
            (false, res)
        } else {
            (true, res)
        }
    }

    fn check_warn_type(&self, error_type: WarningTypes) -> (bool, &str) {
        let (res, internal_defs) = match error_type {
            WarningTypes::UnusedVariable => ("UnusedVariableWarning", vec!["unused_variable_warning", "unused_variable"]),
            WarningTypes::UnusedFunction => ("UnusedFunctionWarning", vec!["unused_function_warning", "unused_function"]),
            WarningTypes::UnusedValue => ("UnusedValueWarning", vec!["unused_value_warning", "unused_value"]),
            WarningTypes::UnreachableCode => ("UnreachableCodeWarning", vec!["unreachable_code_warning", "unreachable_code"]),
        };
        if self.config.type_checker.ignore_warnings.iter().any(|e| internal_defs.contains(&e.as_str())) {
            (false, res)
        } else {
            (true, res)
        }
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
    pub fn raise(&mut self, error_type: ErrorTypes, msg: &str) {
        let rust_loc = PanicLocation::caller();
        let loc = self.get_location_from_current_statement_caller(*rust_loc).unwrap_or_else(|| Location {
            file: self.file_path.clone(),
            line_string: "".to_string(),
            line_number: 0,
            range: (0, 0),
            lucia_source_loc: format!("{}:{}:{}", rust_loc.file(), rust_loc.line(), rust_loc.column()),
        });

        let (should_raise, error_type) = self.check_err_type(error_type);
        if !should_raise {
            return;
        }

        self.errors.push(Error {
            error_type: error_type.to_string(),
            msg: msg.to_string(),
            help: None,
            loc: Some(loc),
            ref_err: None,
        });
    }

    #[track_caller]
    pub fn warn(&mut self, warning_type: WarningTypes, warning_str: &str) {
        let rust_loc = PanicLocation::caller();
        let loc = self.get_location_from_current_statement_caller(*rust_loc).unwrap_or_else(|| Location {
            file: fix_path(self.file_path.clone()),
            line_string: "".to_string(),
            line_number: 0,
            range: (0, 0),
            lucia_source_loc: format!("{}:{}:{}", rust_loc.file(), rust_loc.line(), rust_loc.column()),
        });
        let loc_str = format!("{}:{}:{}", fix_path(loc.file), loc.line_number, loc.range.0);

        let (should_warn, warning_type) = self.check_warn_type(warning_type);
        if !should_warn {
            return;
        }

        if self.config.debug {
            println!("{}Warning raised from {}{}", hex_to_ansi(&self.config.color_scheme.debug, self.config.supports_color), loc.lucia_source_loc, hex_to_ansi("reset", self.config.supports_color));
        }

        if self.config.warnings {
            eprintln!("{}[{}] {}: {}{}", hex_to_ansi(&self.config.color_scheme.warning, self.config.supports_color), loc_str, warning_type, warning_str, hex_to_ansi("reset", self.config.supports_color));
        }
    }

    #[track_caller]
    pub fn raise_with_help(&mut self, error_type: ErrorTypes, msg: &str, help: &str) {
        let rust_loc = PanicLocation::caller();
        let loc = self.get_location_from_current_statement_caller(*rust_loc).unwrap_or_else(|| Location {
            file: self.file_path.clone(),
            line_string: "".to_string(),
            line_number: 0,
            range: (0, 0),
            lucia_source_loc: format!("{}:{}:{}", rust_loc.file(), rust_loc.line(), rust_loc.column()),
        });

        let (should_raise, error_type) = self.check_err_type(error_type);
        if !should_raise {
            return;
        }

        self.errors.push(Error {
            error_type: error_type.to_string(),
            msg: msg.to_string(),
            help: Some(help.to_string()),
            loc: Some(loc),
            ref_err: None,
        });
    }

    #[track_caller]
    pub fn raise_with_ref(&mut self, error_type: ErrorTypes, msg: &str, ref_err: Error) {
        let rust_loc = PanicLocation::caller();
        let loc = self.get_location_from_current_statement_caller(*rust_loc).unwrap_or_else(|| Location {
            file: self.file_path.clone(),
            line_string: "".to_string(),
            line_number: 0,
            range: (0, 0),
            lucia_source_loc: format!("{}:{}:{}", rust_loc.file(), rust_loc.line(), rust_loc.column()),
        });

        let (should_raise, error_type) = self.check_err_type(error_type);
        if !should_raise {
            return;
        }

        self.errors.push(Error {
            error_type: error_type.to_string(),
            msg: msg.to_string(),
            help: None,
            loc: Some(loc),
            ref_err: Some(Box::new(ref_err)),
        });
    }

    pub fn check(&mut self, _statements: Vec<Statement>) -> Vec<Error> {
        self.raise(ErrorTypes::NotImplemented, "Static checking is not yet implemented");
        return self.errors.clone();
    }

    #[track_caller]
    pub fn check_statement(&mut self, statement: Statement) {
        self.current_statement = Some(statement.clone());

        if statement.is_empty() {
            return;
        }
    
        let Statement::Statement { keys, values, .. } = &statement else {
            return self.raise(ErrorTypes::SyntaxError, to_static(format!("Expected a statement map, got {:?}", statement)));
        };
    
        if self.code_state == State::Break || self.code_state == State::Continue {
            return;
        }
    
        let statement_map: HashMap<Value, Value> = keys.iter().cloned().zip(values.iter().cloned()).collect();
    
        static KEY_TYPE: once_cell::sync::Lazy<Value> = once_cell::sync::Lazy::new(|| Value::String("type".into()));

        let result = match statement_map.get(&*KEY_TYPE) {
            Some(Value::String(t)) => match t.as_str() {
                // "IF" => self.check_if(statement_map),
                // "FOR" => self.check_for_loop(statement_map),
                // "WHILE" => self.check_while(statement_map),
                // "TRY_CATCH" | "TRY" => self.check_try(statement_map),
                // "THROW" => self.check_throw(statement_map),
                // "FORGET" => self.check_forget(statement_map),
                // "CONTINUE" | "BREAK" => self.check_continue_and_break(statement_map),
                // "DEFER" => self.check_defer(statement_map),
                // "SCOPE" => self.check_scope(statement_map),
                // "MATCH" => self.check_match(statement_map),
                // "GROUP" => self.check_group(statement_map),

                // "FUNCTION_DECLARATION" => self.check_function_declaration(statement_map),
                // "GENERATOR_DECLARATION" => self.check_generator_declaration(statement_map),
                // "RETURN" => self.check_return(statement_map),
    
                // "IMPORT" => self.check_import(statement_map),
                // "EXPORT" => self.check_export(statement_map),

                "VARIABLE_DECLARATION" => self.check_variable_declaration(statement_map),
                // "VARIABLE" => self.check_variable(statement_map),
                // "ASSIGNMENT" => self.check_assignment(statement_map),
                // "UNPACK_ASSIGN" => self.check_unpack_assignment(statement_map),
    
                // "NUMBER" => self.check_number(statement_map),
                // "STRING" => self.check_string(statement_map),
                // "BOOLEAN" => self.check_boolean(statement_map),
    
                // "TUPLE" => self.check_tuple(statement_map),
                // "MAP" => self.check_map(statement_map),
                // "ITERABLE" => self.check_iterable(statement_map),
                // "VALUE" => self.check_value(statement_map),
    
                // "OPERATION" => self.check_operation(statement_map),
                // "UNARY_OPERATION" => self.check_unary_op(statement_map),
    
                // "CALL" => self.check_call(statement_map),
                // "METHOD_CALL" => self.check_method_call(statement_map),
                // "PROPERTY_ACCESS" => self.check_property_access(statement_map),
                // "INDEX_ACCESS" => self.check_index_access(statement_map),
    
                // "TYPE" => self.check_type(statement_map),
                // "TYPE_CONVERT" => self.check_type_conversion(statement_map),
                // "TYPE_DECLARATION" => self.check_type_declaration(statement_map),

                // "STRUCT_CREATION" => self.check_struct_creation(statement_map),
                // "STRUCT_METHODS" => self.check_struct_methods(statement_map),

                // "POINTER_REF" | "POINTER_DEREF" | "POINTER_ASSIGN" => self.check_pointer(statement_map),

                _ => {}, //self.raise(ErrorTypes::NotImplemented, &format!("Unsupported statement type: {}", t)),
            },
            _ => self.raise(ErrorTypes::SyntaxError, "Missing or invalid 'type' in statement map"),
        };
    
        result
    }

    fn check_variable_declaration(&mut self, statement_map: HashMap<Value, Value>) {
        let name = match statement_map.get(&Value::String("name".into())) {
            Some(Value::String(n)) => n.clone(),
            _ => return self.raise(ErrorTypes::SyntaxError, "Variable declaration missing 'name' or 'name' is not a string"),
        };
        
        let modifiers = match statement_map.get(&Value::String("modifiers".to_string())) {
            Some(Value::List(mods)) => mods,
            _ => &vec![],
        };

        let is_public = modifiers.iter().any(|m| m == &Value::String("public".to_string()));
        let is_final = modifiers.iter().any(|m| m == &Value::String("final".to_string()));
        let is_static = modifiers.iter().any(|m| m == &Value::String("static".to_string()));
    }
}