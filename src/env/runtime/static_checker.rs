#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unused_variables)]

use crate::env::runtime::config::Config;
use crate::env::runtime::errors::Error;
use crate::env::runtime::value::Value;
use crate::env::runtime::statements::Statement;
use crate::env::runtime::utils::{fix_path, hex_to_ansi, to_static};
use crate::env::runtime::types::{Type, VALID_TYPES};
use crate::env::runtime::tokens::Location;
use crate::env::runtime::internal_structs::State;
use std::panic::Location as PanicLocation;
use std::collections::{HashSet, HashMap};

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    pub value: Value,
    pub type_: Type,
    pub is_mutable: bool,
    pub is_static: bool,
    pub is_public: bool,
    pub is_native: bool,
}

impl Variable {
    pub fn new(value: Value, type_: Type, is_mutable: bool, is_static: bool, is_public: bool, is_native: bool) -> Self {
        Self { value, type_, is_mutable, is_static, is_public, is_native }
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
        };
        let builtins = vec![
            "_",
            "_err",
            "__dir__",
            "__file__",
            "argv",
            "print",
            "styledprint",
            "input",
            "len",
            "help",
            "type_of",
            "size_of",
            "sum",
            "ord",
            "char",
            "styledstr",
            "array",
            "exit",
            "fetch",
            "exec",
            "eval",
            "warn",
            "as_method",
            "00__set_cfg__",
            "00__set_dir__",
            "00__placeholder__",
        ];
        for var in builtins {
            this.state.defined_vars.insert(var.to_string(), Variable::new(Value::Null, Type::new_simple("any"), false, true, true, true)); // built-in vars are immutable and public and native
        }
        for t in VALID_TYPES.iter() {
            this.state.defined_vars.insert(t.to_string(), Variable::new(Value::Type(Type::new_simple(t)), Type::new_simple("any"), false, true, true, true)); // built-in types are immutable and public and native
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
    }
}