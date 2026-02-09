#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unused_variables)]

use crate::env::runtime::config::Config;
use crate::env::runtime::errors::Error;
use crate::env::runtime::value::Value;
use crate::env::runtime::statements::{Statement, Node, TypeNode, TypeDeclNode, PtrNode, ThrowNode, MatchCase, IterableNode, RangeModeType, AccessType, ParamAST, alloc_loc, get_loc};
use crate::env::runtime::utils::{fix_path, hex_to_ansi, to_static, special_function_meta, check_ansi, find_closest_match, unescape_string};
use crate::env::runtime::types::{Type, SimpleType, VALID_TYPES};
use crate::env::runtime::tokens::Location;
use crate::env::runtime::internal_structs::{State, PathElement, EffectFlags};
use crate::env::runtime::functions::FunctionMetadata;
use crate::env::runtime::modules::Module;
use crate::env::runtime::native;
use std::path::PathBuf;
use std::panic::Location as PanicLocation;
use std::collections::HashMap;
use rustc_hash::FxHashSet as HashSet;

#[derive(Clone, Debug, PartialEq)]
pub enum ValueType {
    Float(Option<f64>),
    Int(Option<i64>),
    String(Option<usize>),
    Boolean(bool),
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
    Module(String, PathBuf),
    Enum(String, String), // name, variant
    Struct(String), // name
    Pointer(Box<ValueType>),
    Error(String, String, Option<Error>), // type, msg, ref_err
    Any,
}

impl ValueType {
    pub fn type_name(&self) -> String {
        self.get_type().display_simple()
    }
    pub fn get_type(&self) -> Type {
        match self {
            ValueType::Float(_) => Type::new_simple("float"),
            ValueType::Int(_) => Type::new_simple("int"),
            ValueType::String(_) => Type::new_simple("str"),
            ValueType::Boolean(_) => Type::new_simple("bool"),
            ValueType::Null => Type::new_simple("void"),
            ValueType::Map { .. } => Type::new_simple("map"),
            ValueType::List(_) => Type::new_simple("list"),
            ValueType::Tuple(_) => Type::new_simple("tuple"),
            ValueType::Bytes(_) => Type::new_simple("bytes"),
            ValueType::Type(_) => Type::new_simple("type"),
            ValueType::Function(_) => Type::new_simple("function"),
            ValueType::Generator(_) => Type::new_simple("generator"),
            ValueType::Module(..) => Type::new_simple("module"),
            ValueType::Enum(name, variant) => Type::Enum {
                name: name.to_string(),
                variants: vec![],
                generics: vec![],
                wheres: vec![],
            },
            ValueType::Struct(name) => Type::Struct {
                name: name.to_string(),
                fields: vec![],
                methods: vec![],
                generics: vec![],
                wheres: vec![],
            },
            ValueType::Pointer(arc) => {
                let mut t = arc.get_type();
                t = t.make_reference(1);
                t
            }
            ValueType::Error(..) => Type::new_simple("error"),
            ValueType::Any => Type::new_simple("any"),
        }
    }
     pub fn to_string(&self) -> String {
        match self {
            ValueType::Float(f) => {
                match f {
                    Some(f) => f.to_string(),
                    None => "'float'".to_string(),
                }
            }
            ValueType::Int(i) => {
                match i {
                    Some(i) => i.to_string(),
                    None => "'int'".to_string(),
                }
            }
            ValueType::String(_) => "'str'".to_string(),
            ValueType::Boolean(b) => b.to_string(),
            ValueType::Null => "null".to_string(),
            ValueType::Map { .. } => "'map'".to_string(),
            ValueType::Tuple(items) => {
                let items: Vec<String> = items.iter().map(|item| item.to_string()).collect();
                format!("({})", items.join(", "))
            }
            ValueType::List(v) => {
                let items: Vec<String> = v.iter().map(|item| item.to_string()).collect();
                format!("[{}]", items.join(", "))
            }
            ValueType::Bytes(_) => "'bytes'".to_string(),
            ValueType::Pointer(ptr) => {
                let raw_ptr = ptr.as_ref() as *const _ as *const ();
                let addr = raw_ptr as usize;
                format!("<pointer to 0x{:X}>", addr)
            }
            ValueType::Type(t) => (*t).display_simple(),
            ValueType::Function(func) => "'function'".to_string(),
            ValueType::Generator(generator) => "'generator'".to_string(),
            ValueType::Module(name, path) => {
                format!("<module '{}' from '{}'>", name, fix_path(path.display().to_string()))
            }
            ValueType::Enum(name, variant) => format!("{}.{}", name, variant),
            ValueType::Struct(name) => name.to_string(),
            ValueType::Error(err_type, err_msg, _) => format!("<{}: {}>", err_type, err_msg),
            ValueType::Any => "'any'".to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    pub value_type: ValueType,
    pub type_: Type,
    pub is_mutable: bool,
    pub is_static: bool,
    pub is_public: bool,
    pub is_builtin: bool,
    pub loc: Option<Location>,
}

impl Variable {
    pub fn new(value_type: ValueType, type_: Type, is_mutable: bool, is_static: bool, is_public: bool, is_builtin: bool, loc: Option<Location>) -> Self {
        Self { value_type, type_, is_mutable, is_static, is_public, is_builtin, loc }
    }
    pub fn type_name(&self) -> String {
        self.type_.display_simple()
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
    TypeError,
    SyntaxError,
    UnicodeError,
}

#[derive(Debug, Clone)]
pub enum WarningTypes {
    UnusedVariable,
    UnusedFunction,
    UnusedValue,
    UnreachableCode,
    OverwrittenVariable,
    AlreadyDefined,
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
                used_vars: HashSet::default(),
                assigned_vars: HashSet::default(),
                loop_vars: HashSet::default(),
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
        builtins.insert("_".to_string(), Variable::new(ValueType::Any, Type::new_simple("any"), false, true, true, true, None));
        builtins.insert("_err".to_string(), Variable::new(ValueType::Tuple(vec![ValueType::String(None), ValueType::String(None), ValueType::String(None)]), Type::new_simple("any"), false, true, true, true, None)); // (type, msg, ref_err)
        builtins.insert("__dir__".to_string(), Variable::new(ValueType::String(None), Type::new_simple("any"), false, true, true, true, None));
        builtins.insert("__file__".to_string(), Variable::new(ValueType::String(None), Type::new_simple("any"), false, true, true, true, None));
        builtins.insert("argv".to_string(), Variable::new(ValueType::List(vec![ValueType::String(None); 100]), Type::new_simple("any"), false, true, true, true, None)); // up to 100 args
        builtins.insert("print".to_string(), Variable::new(ValueType::Function(native::print_fn().metadata().clone()), Type::new_simple("any"), false, true, true, true, None));
        builtins.insert("styledprint".to_string(), Variable::new(ValueType::Function(native::styled_print_fn().metadata().clone()), Type::new_simple("any"), false, true, true, true, None));
        builtins.insert("input".to_string(), Variable::new(ValueType::Function(native::input_fn().metadata().clone()), Type::new_simple("any"), false, true, true, true, None));
        builtins.insert("len".to_string(), Variable::new(ValueType::Function(native::len_fn().metadata().clone()), Type::new_simple("any"), false, true, true, true, None));
        builtins.insert("help".to_string(), Variable::new(ValueType::Function(native::help_fn().metadata().clone()), Type::new_simple("any"), false, true, true, true, None));
        builtins.insert("type_of".to_string(), Variable::new(ValueType::Function(native::type_fn().metadata().clone()), Type::new_simple("any"), false, true, true, true, None));
        builtins.insert("size_of".to_string(), Variable::new(ValueType::Function(native::size_of_fn().metadata().clone()), Type::new_simple("any"), false, true, true, true, None));
        builtins.insert("sum".to_string(), Variable::new(ValueType::Function(native::sum_fn().metadata().clone()), Type::new_simple("any"), false, true, true, true, None));
        builtins.insert("ord".to_string(), Variable::new(ValueType::Function(native::ord_fn().metadata().clone()), Type::new_simple("any"), false, true, true, true, None));
        builtins.insert("char".to_string(), Variable::new(ValueType::Function(native::char_fn().metadata().clone()), Type::new_simple("any"), false, true, true, true, None));
        builtins.insert("styledstr".to_string(), Variable::new(ValueType::Function(native::styledstr_fn().metadata().clone()), Type::new_simple("any"), false, true, true, true, None));
        builtins.insert("array".to_string(), Variable::new(ValueType::Function(native::array_fn().metadata().clone()), Type::new_simple("any"), false, true, true, true, None));
        builtins.insert("complex".to_string(), Variable::new(ValueType::Function(native::complex_fn().metadata().clone()), Type::new_simple("any"), false, true, true, true, None));
        builtins.extend(special_function_meta().into_iter().map(|(k, v)| (k.to_string(), Variable::new(ValueType::Function(v.clone()), Type::new_simple("any"), false, true, true, true, None))));
        for (var, val) in builtins {
            this.state.defined_vars.insert(var.to_string(), val);
        }
        for t in VALID_TYPES.iter() {
            this.state.defined_vars.insert(t.to_string(), Variable::new(ValueType::Type(Type::new_simple(t)), Type::new_simple("any"), false, true, true, true, None));
        }
        this
    }

    fn check_err_type(&self, error_type: ErrorTypes) -> (bool, &str) {
        let (res, internal_defs) = match error_type {
            ErrorTypes::NotImplemented => ("NotImplementedError", vec!["not_implemented_error", "not_implemented"]),
            ErrorTypes::UndefinedVariable => ("UndefinedVariable", vec!["undefined_variable", "undefined_var", "undefined_variable_error", "undefined_var_error"]),
            ErrorTypes::TypeMismatch => ("TypeMismatch", vec!["type_mismatch", "type_mismatch_error"]),
            ErrorTypes::TypeError => ("TypeError", vec!["type_error"]),
            ErrorTypes::RuntimeError => ("RuntimeError", vec!["runtime_error"]),
            ErrorTypes::SyntaxError => ("SyntaxError", vec!["syntax_error"]),
            ErrorTypes::UnicodeError => ("UnicodeError", vec!["unicode_error"]),
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
            WarningTypes::OverwrittenVariable => ("OverwrittenVariableWarning", vec!["overwritten_variable_warning", "overwritten_variable"]),
            WarningTypes::AlreadyDefined => ("AlreadyDefinedWarning", vec!["already_defined_warning", "already_defined"]),
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
            Some(stmt) => match &stmt.loc {
                Some(loc_id) => Some(get_loc(*loc_id).set_lucia_source_loc(format!("{}:{}:{}", PanicLocation::caller().file(), PanicLocation::caller().line(), PanicLocation::caller().column()))),
                None => None,
            },
            None => None,
        }
    }

    fn get_location_from_statement(&self, statement: &Statement) -> Option<Location> {
        match &statement.loc {
            Some(loc_id) => Some(get_loc(*loc_id).set_lucia_source_loc(format!("{}:{}:{}", PanicLocation::caller().file(), PanicLocation::caller().line(), PanicLocation::caller().column()))),
            None => None,
        }
    }

    fn get_location_from_current_statement_caller(&self, caller: PanicLocation) -> Option<Location> {
        match &self.current_statement {
            Some(stmt) => match &stmt.loc {
                Some(loc_id) => Some(get_loc(*loc_id).set_lucia_source_loc(format!("{}:{}:{}", caller.file(), caller.line(), caller.column()))),
                None => None,
            },
            None => None,
        }
    }

    #[track_caller]
    pub fn raise(&mut self, error_type: ErrorTypes, msg: &str) -> ValueType {
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
            return ValueType::Null;
        }

        self.errors.push(Error {
            err_type: error_type.to_string(),
            err_msg: msg.to_string(),
            help: None,
            loc: Some(loc),
            ref_err: None,
        });
        ValueType::Null
    }

    fn compare_type(&mut self, value: &ValueType, expected: &Type) -> (bool, Option<Error>) {
        let mut err: Option<Error> = None;
        let mut status: bool = true;
        fn make_err(err_type: &str, err_msg: &str, loc: Option<Location>) -> Error {
            loc.map(|l| Error::with_location(err_type, err_msg, l))
                .unwrap_or_else(|| Error::new_anonymous(err_type, err_msg))
        }

        let value_type = value.get_type();

        match expected {
            Type::Simple { ty: expected_type } => {
                if *expected_type != SimpleType::Any {
                    match value_type {
                        Type::Simple { ty: value_type_name, .. } => {
                            if value_type_name != *expected_type {
                                status = false;
                            }
                        }
                        _ => {
                            err = Some(make_err("TypeError", &format!("Expected type '{}', got '{}'", expected_type, value_type.display()), self.get_location_from_current_statement()));
                        }
                    }
                }
            }
            _ => {
                status = false;
                err = Some(make_err("TypeError", &format!("Unsupported type check for '{}'", expected.display()), self.get_location_from_current_statement()));
            }
        }

        return (status, err);
    }

    #[track_caller]
    pub fn warn(&mut self, warning_type: WarningTypes, warning_str: &str) -> ValueType {
        let rust_loc = PanicLocation::caller();
        let loc = self.get_location_from_current_statement_caller(*rust_loc).unwrap_or_else(|| Location {
            file: fix_path(self.file_path.clone()),
            line_string: "".to_string(),
            line_number: 0,
            range: (0, 0),
            lucia_source_loc: format!("{}:{}:{}", rust_loc.file(), rust_loc.line(), rust_loc.column()),
        });
        let (should_warn, warning_type) = self.check_warn_type(warning_type);
        if !should_warn {
            return ValueType::Null;
        }

        if self.config.type_checker.treat_warnings_as_errors {
            self.errors.push(Error {
                err_type: warning_type.to_string(),
                err_msg: warning_str.to_string(),
                help: None,
                loc: Some(loc),
                ref_err: None,
            });
            return ValueType::Null;
        }
        
        let loc_str = format!("{}:{}:{}", fix_path(loc.file), loc.line_number, loc.range.0);


        if self.config.debug {
            println!("{}Warning raised from {}{}", hex_to_ansi(&self.config.color_scheme.debug, self.config.supports_color), loc.lucia_source_loc, hex_to_ansi("reset", self.config.supports_color));
        }

        if self.config.warnings {
            eprintln!("{}[{}] {}: {}{}", hex_to_ansi(&self.config.color_scheme.warning, self.config.supports_color), loc_str, warning_type, warning_str, hex_to_ansi("reset", self.config.supports_color));
        }
        ValueType::Null
    }

    #[track_caller]
    pub fn warn_with_loc(&mut self, warning_type: WarningTypes, warning_str: &str, loc: Option<Location>) -> ValueType {
        let rust_loc = PanicLocation::caller();
        let loc = loc.unwrap_or_else(|| Location {
            file: fix_path(self.file_path.clone()),
            line_string: "".to_string(),
            line_number: 0,
            range: (0, 0),
            lucia_source_loc: "".to_string(),
        }).set_lucia_source_loc(format!("{}:{}:{}", rust_loc.file(), rust_loc.line(), rust_loc.column()));

        let loc_str = format!("{}:{}:{}", fix_path(loc.file), loc.line_number, loc.range.0);

        let (should_warn, warning_type) = self.check_warn_type(warning_type);
        if !should_warn {
            return ValueType::Null;
        }

        if self.config.debug {
            println!("{}Warning raised from {}{}", hex_to_ansi(&self.config.color_scheme.debug, self.config.supports_color), loc.lucia_source_loc, hex_to_ansi("reset", self.config.supports_color));
        }

        if self.config.warnings {
            eprintln!("{}[{}] {}: {}{}", hex_to_ansi(&self.config.color_scheme.warning, self.config.supports_color), loc_str, warning_type, warning_str, hex_to_ansi("reset", self.config.supports_color));
        }
        ValueType::Null
    }

    #[track_caller]
    pub fn raise_with_help(&mut self, error_type: ErrorTypes, msg: &str, help: &str) -> ValueType {
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
            return ValueType::Null;
        }

        self.errors.push(Error {
            err_type: error_type.to_string(),
            err_msg: msg.to_string(),
            help: Some(help.to_string()),
            loc: Some(loc),
            ref_err: None,
        });
        ValueType::Null
    }

    #[track_caller]
    pub fn raise_with_ref(&mut self, error_type: ErrorTypes, msg: &str, ref_err: Error) -> ValueType {
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
            return ValueType::Null;
        }

        self.errors.push(Error {
            err_type: error_type.to_string(),
            err_msg: msg.to_string(),
            help: None,
            loc: Some(loc),
            ref_err: Some(Box::new(ref_err)),
        });
        ValueType::Null
    }

    pub fn check(&mut self, statements: Vec<Statement>, in_repl: bool) -> Vec<Error> {
        for statement in statements {
            self.check_statement(statement);
        }
        if !in_repl {
            for (var, val) in &self.state.defined_vars.clone() {
                if !self.state.used_vars.contains(var)
                    && !self.state.loop_vars.contains(var)
                    && !SimpleType::can_be_uninitialized_str(&var.as_str())
                    && !var.starts_with('_')
                {
                    let builtin = val.is_builtin;
                    let loc = val.loc.clone();
                    match &val.value_type {
                        ValueType::Function(_) if !builtin => {
                            self.warn_with_loc(WarningTypes::UnusedFunction, &format!("Function '{}' is defined but never used", var), loc);
                        }
                        ValueType::Generator(_) if !builtin => {
                            self.warn_with_loc(WarningTypes::UnusedFunction, &format!("Generator '{}' is defined but never used", var), loc);
                        }
                        ValueType::Type(_) if !builtin => {
                            self.warn_with_loc(WarningTypes::UnusedFunction, &format!("Type '{}' is defined but never used", var), loc);
                        }
                        ValueType::Module(..) if !builtin => {
                            self.warn_with_loc(WarningTypes::UnusedFunction, &format!("Module '{}' is imported but never used", var), loc);
                        }
                        _ if !builtin => {
                            self.warn_with_loc(WarningTypes::UnusedVariable, &format!("Variable '{}' is defined but never used", var), loc);
                        }
                        _ => {}
                    }
                }
            }
        }
        self.errors.clone()
    }

    pub fn check_type(&mut self, type_node: &TypeNode) -> ValueType {
        match type_node {
            TypeNode::Simple { base  } => {
                match self.state.defined_vars.get(base) {
                    Some(val) => match &val.value_type {
                        ValueType::Type(t) => {
                            ValueType::Type(t.clone())
                        }
                        _ => {
                            self.raise_with_help(ErrorTypes::TypeError, &format!("'{}' is a variable name, not a type", base), "If you meant to assign a value, use ':=' instead of ':'");
                            return ValueType::Null;
                        }
                    },
                    None => {
                        self.raise(
                            ErrorTypes::TypeError,
                            &format!(
                                "Invalid type '{}'. Valid types are: {}, ...",
                                base,
                                VALID_TYPES[0..5].join(", ")
                            ),
                        );
                        return ValueType::Null;
                    }
                }
            },
            TypeNode::Reference { base_type, ref_level } => {
                let ty = self.check_type(base_type);
                if let ValueType::Type(t) = ty {
                    ValueType::Type(t.make_reference(*ref_level))
                } else {
                    self.raise(ErrorTypes::TypeError, "Reference type must be a valid type");
                    ValueType::Null
                }
            }
            TypeNode::Maybe { base_type } => {
                let ty = self.check_type(base_type);
                if let ValueType::Type(t) = ty {
                    ValueType::Type(t.make_maybe_type())
                } else {
                    self.raise(ErrorTypes::TypeError, "Maybe type must be a valid type");
                    ValueType::Null
                }
            }
            TypeNode::Union { types } => {
                let mut type_vec: Vec<Type> = Vec::new();

                for t in types {
                    let ty = self.check_type(t);
                    if let ValueType::Type(t) = ty {
                        type_vec.push(t);
                    } else {
                        self.raise(ErrorTypes::TypeError, "Union type must be a valid type");
                        return ValueType::Null;
                    }
                }

                ValueType::Type(Type::Union(type_vec))
            }
            TypeNode::Function { .. } => {
                ValueType::Type(Type::new_simple("function"))
            }
            TypeNode::Generator { .. } => {
                ValueType::Type(Type::new_simple("generator"))
            }
            TypeNode::Generics { .. } => {
                ValueType::Type(Type::new_simple("any"))
            }
            TypeNode::Impl { .. } => {
                ValueType::Type(Type::new_simple("any"))
            }
            TypeNode::Variadics { .. } => {
                ValueType::Type(Type::new_simple("any"))
            }
        }
    }

    #[track_caller]
    pub fn check_statement(&mut self, statement: Statement) -> ValueType {
        self.current_statement = Some(statement.clone());
    
        if self.code_state == State::Break || self.code_state == State::Continue {
            return ValueType::Null;
        }
    
        let result = match &statement.node {
            Node::If { .. } => self.raise(ErrorTypes::NotImplemented, "If statement checking not implemented"),
            Node::For { .. } => self.raise(ErrorTypes::NotImplemented, "For loop checking not implemented"),
            Node::While { .. } => self.raise(ErrorTypes::NotImplemented, "While loop checking not implemented"),
            Node::TryCatch { .. } => self.raise(ErrorTypes::NotImplemented, "Try-catch checking not implemented"),
            Node::Throw { .. } => self.raise(ErrorTypes::NotImplemented, "Throw checking not implemented"),
            Node::Forget { .. } => self.raise(ErrorTypes::NotImplemented, "Forget checking not implemented"),
            Node::Continue | Node::Break => self.raise(ErrorTypes::NotImplemented, "Continue/Break checking not implemented"),
            Node::Defer { .. } => self.raise(ErrorTypes::NotImplemented, "Defer checking not implemented"),
            Node::Scope { .. } => self.raise(ErrorTypes::NotImplemented, "Scope checking not implemented"),
            Node::Match { .. } => self.raise(ErrorTypes::NotImplemented, "Match checking not implemented"),
            Node::Group { .. } => self.raise(ErrorTypes::NotImplemented, "Group checking not implemented"),

            Node::FunctionDeclaration { .. } => self.raise(ErrorTypes::NotImplemented, "Function declaration checking not implemented"),
            Node::GeneratorDeclaration { .. } => self.raise(ErrorTypes::NotImplemented, "Generator declaration checking not implemented"),
            Node::Return { .. } => self.raise(ErrorTypes::NotImplemented, "Return checking not implemented"),

            Node::Import { .. } => self.raise(ErrorTypes::NotImplemented, "Import checking not implemented"),
            Node::Export { .. } => self.raise(ErrorTypes::NotImplemented, "Export checking not implemented"),

            Node::VariableDeclaration { name, val_stmt, var_type, modifiers, is_default } => 
                self.check_variable_declaration(name, val_stmt, var_type, modifiers, *is_default),
            Node::Variable { name } => self.check_variable(name),
            Node::Assignment { .. } => self.raise(ErrorTypes::NotImplemented, "Assignment checking not implemented"),
            Node::UnpackAssignment { .. } => self.raise(ErrorTypes::NotImplemented, "Unpack assignment checking not implemented"),

            Node::Number { value } => self.check_number(value),
            Node::String { value, mods } => self.check_string(value, mods),
            Node::Boolean { value } => self.check_boolean(*value),

            Node::Map { .. } => self.raise(ErrorTypes::NotImplemented, "Map checking not implemented"),
            Node::Iterable { .. } => self.raise(ErrorTypes::NotImplemented, "Iterable checking not implemented"),
            Node::Value { .. } => self.raise(ErrorTypes::NotImplemented, "Value checking not implemented"),

            Node::Operation { .. } => self.raise(ErrorTypes::NotImplemented, "Operation checking not implemented"),
            Node::UnaryOperation { .. } => self.raise(ErrorTypes::NotImplemented, "Unary operation checking not implemented"),
            Node::PrefixOperation { .. } => self.raise(ErrorTypes::NotImplemented, "Prefix operation checking not implemented"),
            Node::PostfixOperation { .. } => self.raise(ErrorTypes::NotImplemented, "Postfix operation checking not implemented"),
            Node::Pipeline { .. } => self.raise(ErrorTypes::NotImplemented, "Pipeline checking not implemented"),

            Node::Call { .. } => self.raise(ErrorTypes::NotImplemented, "Call checking not implemented"),
            Node::MethodCall { .. } => self.raise(ErrorTypes::NotImplemented, "Method call checking not implemented"),
            Node::PropertyAccess { .. } => self.raise(ErrorTypes::NotImplemented, "Property access checking not implemented"),
            Node::IndexAccess { .. } => self.raise(ErrorTypes::NotImplemented, "Index access checking not implemented"),
            
            Node::Type { node } => self.check_type(node),
            Node::TypeConvert { .. } => self.raise(ErrorTypes::NotImplemented, "Type conversion checking not implemented"),
            Node::TypeDeclaration { .. } => self.raise(ErrorTypes::NotImplemented, "Type declaration checking not implemented"),

            Node::StructCreation { .. } => self.raise(ErrorTypes::NotImplemented, "Struct creation checking not implemented"),
            Node::StructMethods { .. } => self.raise(ErrorTypes::NotImplemented, "Struct methods checking not implemented"),
            Node::Pointer { .. } => self.raise(ErrorTypes::NotImplemented, "Pointer checking not implemented"),
            Node::Null => ValueType::Null,
        };
    
        result
    }

    fn check_number(&mut self, value: &str) -> ValueType {
        let s = value.replace('_', "");

        if s.is_empty() {
            return self.raise(ErrorTypes::RuntimeError, "Empty string provided for number");
        }

        if s.contains('.') || s.to_ascii_lowercase().contains('e') {
            if let Some(f) = s.parse::<f64>().ok() {
                return ValueType::Float(Some(f));
            }
            return ValueType::Float(None);
        } else {
            if let Some(i) = s.parse::<i64>().ok() {
                return ValueType::Int(Some(i));
            }
            return ValueType::Int(None);
        }
    }

    fn check_boolean(&mut self, value: Option<bool>) -> ValueType {
        match value {
            Some(true) => ValueType::Boolean(true),
            Some(false) => ValueType::Boolean(false),
            None => ValueType::Null,
        }
    }

    fn check_string(&mut self, value: &str, mods: &[char]) -> ValueType {
        let mut modified_string = value.to_string();
        let mut is_raw = false;
        let mut is_bytes = false;

        for m in mods {
            match m {
                'f' => {
                    return ValueType::String(None);
                }
                'r' => is_raw = true,
                'b' => is_bytes = true,
                _ => {
                    self.raise(ErrorTypes::RuntimeError, &format!("Unknown string modifier: {}", m));
                }
            }
        }

        if is_raw {
            if modified_string.len() >= 2 {
                let first = modified_string.chars().next().unwrap();
                let last = modified_string.chars().last().unwrap();
                if (first == '"' && last == '"') || (first == '\'' && last == '\'') {
                    modified_string = modified_string[1..modified_string.len() - 1].to_string();
                }
            }
        } else {
            modified_string = match unescape_string(&modified_string) {
                Ok(unescaped) => unescaped,
                Err(e) => return self.raise(ErrorTypes::UnicodeError, &e),
            };
        }

        if is_bytes {
            ValueType::Bytes(modified_string.len())
        } else {
            ValueType::String(Some(modified_string.len()))
        }
    }

    fn check_variable_declaration(&mut self, name: &str, val_stmt: &Statement, var_type: &Statement, modifiers: &[String], is_default: bool) -> ValueType {
        let is_public = modifiers.iter().any(|m| m == "public");
        let is_final = modifiers.iter().any(|m| m == "final");
        let is_static = modifiers.iter().any(|m| m == "static");

        let mut declared_type = match self.check_statement(var_type.clone()) {
            ValueType::Type(t) => t.clone(),
            ValueType::Null => return ValueType::Null,
            t => {
                self.raise(ErrorTypes::TypeError, &format!("Expected a type for variable type, got {}", t.type_name()));
                Type::new_simple("any")
            },
        };

        if is_default {
            match declared_type {
                Type::Simple { ref ty, ..} if ty.can_be_uninitialized() => {},
                Type::Reference { .. } => {},
                _ => {
                    self.raise_with_help(ErrorTypes::TypeError, &format!("Cannot declare variable '{}' as default because its type '{}' cannot be uninitialized", name, declared_type.display_simple()), "Only variables of type 'any', 'map', 'list', 'tuple', 'void', or maybe types can be declared as default");
                    return ValueType::Null;
                }
            }
        }

        if self.state.defined_vars.contains_key(name) {
            if !self.state.used_vars.contains(name) {
                self.warn(WarningTypes::OverwrittenVariable, &format!("Value of '{}' was overwritten before use.", name));
            } else {
                self.warn(WarningTypes::AlreadyDefined, &format!("Variable '{}' is already defined in this scope", name));
            }
        };

        let var_value = self.check_statement(val_stmt.clone());

        if declared_type == Type::new_simple("auto") {
            declared_type = var_value.get_type();
        }

        let (is_valid, err) = self.compare_type(&var_value, &declared_type);
        if !is_valid {
            if let Some(err) = err {
                self.raise_with_ref(ErrorTypes::TypeError, &format!("Variable '{}' declared with type '{}', but got {} with invalid element types", name, declared_type.display_simple(), var_value.type_name()), err);
            } else {
                self.raise(ErrorTypes::TypeError, &format!("Variable '{}' declared with type '{}', but value is of type '{}'", name, declared_type.display_simple(), var_value.type_name()));
            }
        }

        let variable = Variable::new(var_value.clone(), declared_type, is_static, is_public, is_final, false, self.get_location_from_current_statement());
        self.state.defined_vars.insert(name.to_string(), variable);

        ValueType::Null
    }

    fn check_variable(&mut self, name: &str) -> ValueType {
        if let Some(var) = self.state.defined_vars.get(name) {
            self.state.used_vars.insert(name.to_string());
            return var.value_type.clone();
        } else {
            let mut candidates: Vec<(String, Type, Statement)> = vec![];
            let mut seen = HashSet::default();

            for (k, v) in &self.state.defined_vars {
                if k == "_" { continue; }
                if let ValueType::Type(t) = &v.value_type {
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
                    let saved_variables = self.state.defined_vars.clone();
                    let generics = match &candidates[0].1 {
                        Type::Enum { generics, .. } => generics.clone(),
                        _ => vec![],
                    };
                    self.state.defined_vars.extend(generics.into_iter().map(|g| (g, Variable::new(ValueType::Type(Type::new_simple("any")), Type::new_simple("type"), false, true, true, true, None))));
                    let eval_type = match self.check_statement(candidates[0].2.clone()) {
                        ValueType::Type(t) => t,
                        e => {
                            self.state.defined_vars = saved_variables;
                            return self.raise(ErrorTypes::TypeError, &format!("Expected a type for '{}', but got: {}", candidates[0].0.clone(), e.to_string()))
                        },
                    };
                    self.state.defined_vars = saved_variables;
                    return self.raise_with_help(
                        ErrorTypes::SyntaxError,
                        &format!("Missing argument for '{}.{}' of type '{}'", candidates[0].1.display_simple(), candidates[0].0.clone(), eval_type.display_simple()),
                        &format!("Did you mean to use '{}.{}({})'", candidates[0].1.display_simple(), candidates[0].0.clone(), eval_type.display()),
                    );
                }
                let enum_name = match &candidates[0].1 {
                    Type::Enum { name, .. } => name.clone(),
                    _ => "".to_string(),
                };
                return ValueType::Enum(enum_name, candidates[0].0.clone());
            } else if candidates.len() > 1 {
                let variant_list = candidates.iter()
                    .map(|(variant, ty, _)| format!("'{}.{}'", ty.display_simple(), variant))
                    .collect::<Vec<_>>()
                    .join(", ");
                return self.raise_with_help(ErrorTypes::UndefinedVariable, &format!("Variable '{}' is not defined.", name), &format!("However, it matches multiple enum variants: {}. Did you mean to use one of these?", variant_list));
            }
            let lib_dir = PathBuf::from(self.config.home_dir.clone()).join("libs").join(&name);
            let extensions = ["lc", "lucia", "rs", ""];
            for ext in extensions.iter() {
                let candidate = lib_dir.with_extension(ext);
                if candidate.exists() {
                    return self.raise_with_help(
                        ErrorTypes::UndefinedVariable,
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

            let available_names: Vec<String> = self.state.defined_vars.keys().cloned().collect();
            if let Some(closest) = find_closest_match(name, &available_names) {
                return self.raise_with_help(
                    ErrorTypes::UndefinedVariable,
                    &format!("Variable '{}' is not defined.", name),
                    &format!(
                        "Did you mean '{}{}{}'?",
                        check_ansi("\x1b[4m", &self.config.supports_color),
                        closest,
                        check_ansi("\x1b[24m", &self.config.supports_color),
                    ),
                );
            } else {
                return self.raise(ErrorTypes::UndefinedVariable, &format!("Variable '{}' is not defined.", name));
            }
        }
    }
}