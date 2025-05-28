use std::collections::HashMap;
use crate::env::core::config::{Config, CodeBlocks, ColorScheme};
use crate::env::core::utils::{print_colored, hex_to_ansi, format_value, find_closest_match, TRUE, FALSE, NULL, debug_log, check_ansi, unescape_string};
use crate::env::core::types::{Int, Float, VALID_TYPES};
use crate::env::core::value::Value;
use crate::env::core::errors::Error;
use crate::env::core::variables::Variable;
use crate::env::core::statements::Statement;
use std::ops::{Add, Sub, Mul, Div, Rem, Neg};
use num_traits::{Zero, self};
use lazy_static::lazy_static;
use crate::env::core::native;
use crate::env::core::functions::{Function, FunctionMetadata, NativeFunction, Parameter, ParameterKind, Callable, NativeCallable};
use std::sync::Arc;

use crate::lexer::Lexer;
use crate::parser::{Parser, Token};


pub struct Interpreter {
    config: Config,
    err: Option<Error>,
    is_returning: bool,
    return_value: Value,
    source: String,
    stack: Vec<Value>,
    use_colors: bool,
    current_statement: Option<Statement>,
    variables: HashMap<String, Variable>,
}

impl Interpreter {
    pub fn new(config: Config, source: String, use_colors: bool) -> Self {
        let mut this = Self {
            config,
            err: None,
            return_value: NULL,
            is_returning: false,
            source,
            stack: vec![],
            use_colors,
            current_statement: None,
            variables: HashMap::new(),
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
            "exit".to_string(),
            Variable::new("exit".to_string(), Value::Function(native::exit_fn()), "function".to_string(), false, true, true),
        );
        this.variables.insert(
            "len".to_string(),
            Variable::new("len".to_string(), Value::Function(native::len_fn()), "function".to_string(), false, true, true),
        );
        this.variables.insert(
            "help".to_string(),
            Variable::new("help".to_string(), Value::Function(native::help_fn()), "help".to_string(), false, true, true),
        );

        this
    }

    fn check_type(
        &mut self,
        type_: &str,
        expected: Option<&str>,
        return_value: Option<Value>,
        error: bool,
    ) -> bool {
        let valid_types = VALID_TYPES.to_vec();
        let mut types_mapping = std::collections::HashMap::new();
        types_mapping.insert("void", "null");
        types_mapping.insert("any", "any");
        types_mapping.insert("null", "null");
        types_mapping.insert("Decimal", "float");
        types_mapping.insert("method", "function");
        types_mapping.insert("int", "int");
        types_mapping.insert("float", "float");
        types_mapping.insert("bool", "bool");
        types_mapping.insert("str", "str");
        types_mapping.insert("list", "list");
        types_mapping.insert("map", "map");
        types_mapping.insert("function", "function");
        let normalized_type = types_mapping.get(type_).unwrap_or(&type_);
        let expected_type = expected
            .and_then(|e| types_mapping.get(e))
            .map_or(expected.unwrap_or(""), |v| *v);
    
        let normalized_type = *normalized_type;
        let expected_type = expected_type;
    
        if normalized_type == "bool" && expected_type == "null" && return_value == Some(NULL) {
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
            } else {
                return false;
            }
        }
    
        true
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
    

    pub fn interpret(&mut self, statements: Vec<Statement>) -> Result<Value, Error> {
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
                });
            } else {
                self.err = Some(Error {
                    error_type: error_type.to_string(),
                    msg: msg.to_string(),
                    help: None,
                    line: (0, "".to_string()),
                    column: 0,
                });
            }
        } else {
            self.err = Some(Error {
                error_type: error_type.to_string(),
                msg: msg.to_string(),
                help: None,
                line: (0, "".to_string()),
                column: 0,
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
                });
            } else {
                self.err = Some(Error {
                    error_type: error_type.to_string(),
                    msg: msg.to_string(),
                    help: Some(help.to_string()),
                    line: (0, "".to_string()),
                    column: 0,
                });
            }
        } else {
            self.err = Some(Error {
                error_type: error_type.to_string(),
                msg: msg.to_string(),
                help: Some(help.to_string()),
                line: (0, "".to_string()),
                column: 0,
            });
        }
        NULL
    }
    
    pub fn evaluate(&mut self, statement: Statement) -> Value {
        self.current_statement = Some(statement.clone());
        if self.err.is_some() {
            return NULL;
        }
    
        let Statement::Statement { keys, values, .. } = statement else {
            return self.raise("SyntaxError", "Expected a statement map");
        };
        
        let statement: HashMap<_, _> = keys.iter().cloned().zip(values.iter().cloned()).collect();
    
        match statement.get(&Value::String("type".to_string())) {
            Some(Value::String(t)) => match t.as_str() {
                "NUMBER" => self.handle_number(statement.clone()),
                "STRING" => self.handle_string(statement.clone()),
                "BOOLEAN" => self.handle_boolean(statement.clone()),
                "OPERATION" => self.handle_operation(statement.clone()),
                "UNARY_OPERATION" => self.handle_unary_op(statement.clone()),
                "CALL" => self.handle_call(statement.clone()),
                _ => self.raise("NotImplemented", &format!("Unsupported statement type: {}", t)),
            },
            _ => self.raise("SyntaxError", "Missing or invalid 'type' in statement map"),
        }
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
        let var = match self.variables.get(function_name) {
            Some(v) => v.clone(),
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
    
        let value = var.get_value();
    
        match value {
            Value::Function(func) => {
                if let Some(state) = func.metadata().state.as_ref() {
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
                            function_name,
                            passed_args_count
                        ),
                    );
                }
                
                if passed_args_count > expected_args_count {
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
                
                if provided_positional_count < required_positional_count {
                    return self.raise(
                        "TypeError",
                        &format!(
                            "Missing required positional argument{} for function '{}'. Expected at least {}, but got {}.",
                            if required_positional_count == 1 { "" } else { "s" },
                            function_name,
                            required_positional_count,
                            provided_positional_count
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
                                if self.check_type(param_type, Some(&arg_value.type_name()), Some(NULL), false) {
                                    final_args.insert(param_name.clone(), arg_value);
                                } else {
                                    return self.raise_with_help(
                                        "TypeError",
                                        &format!("Argument '{}' does not match expected type '{}', got '{}'", param_name, param_type, arg_value.type_name()),
                                        &format!(
                                            "Try using: '{}{}={}({}){}'",
                                            check_ansi("\x1b[4m", &self.use_colors),
                                            param_name,
                                            param_type,
                                            format_value(&positional[pos_index]).to_string(),
                                            check_ansi("\x1b[24m", &self.use_colors)
                                        ),
                                    );
                                }
                                pos_index += 1;
                            } else if let Some(named_value) = named_map.remove(param_name) {
                                if self.check_type(param_type, Some(&named_value.type_name()), Some(named_value.clone()), false) {
                                    final_args.insert(param_name.clone(), named_value);
                                } else {
                                    return self.raise_with_help(
                                        "TypeError",
                                        &format!("Argument '{}' does not match expected type '{}', got '{}'", param_name, param_type, named_value.type_name()),
                                        &format!(
                                            "Try using: '{}{}={}({}){}'",
                                            check_ansi("\x1b[4m", &self.use_colors),
                                            param_name,
                                            param_type,
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
                                if !self.check_type(param_type, Some(&arg.type_name()), Some(arg.clone()), false) {
                                    return self.raise(
                                        "TypeError",
                                        &format!("Variadic argument #{} does not match expected type '{}'", i, param_type),
                                    );
                                }
                            }
                    
                            final_args.insert(param_name.clone(), Value::List(variadic_args));
                    
                            pos_index = positional.len();
                        }
                        ParameterKind::KeywordVariadic => {
                            let mut keyword_args = HashMap::new();
                            for (key, value) in &named_map {
                                if self.check_type(param_type, Some(&value.type_name()), Some(value.clone()), true) {
                                    keyword_args.insert(key.clone(), value.clone());
                                } else {
                                    return self.raise(
                                        "TypeError",
                                        &format!("Keyword argument '{}' does not match expected type '{}'", key, param_type),
                                    );
                                }
                            }
                            let keys: Vec<Value> = keyword_args.keys()
                            .cloned()
                            .map(|k| Value::String(k))
                            .collect();
                        
                            let values: Vec<Value> = keyword_args.values()
                                .cloned()
                                .collect();
                            
                            final_args.insert(
                                param_name.clone(),
                                Value::Map { keys, values },
                            );
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
    
                return func.call(&final_args)
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
        let left_opt = statement.get(&Value::String("left".to_string()));
        let left = match left_opt {
            Some(val) => self.evaluate(val.convert_to_statement()),
            None => {
                self.raise("KeyError", "Missing 'left' key in the statement.");
                return NULL;
            }
        };

        let right_opt = statement.get(&Value::String("right".to_string()));
        let right = match right_opt {
            Some(val) => self.evaluate(val.convert_to_statement()),
            None => {
                self.raise("KeyError", "Missing 'right' key in the statement.");
                return NULL;
            }
        };


        let operator = match statement.get(&Value::String("operator".to_string())) {
            Some(Value::String(s)) => s,
            _ => return self.raise("TypeError", "Expected a string for operator"),
        };

        let mut left = left;
        let mut right = right;

        let mut precision = 0;
        if let Value::Float(l) = &left {
            precision = l.to_string().len();
        }
        if let Value::Float(r) = &right {
            precision = std::cmp::max(precision, r.to_string().len());
        }

        let prec = precision + 2;

        if let Value::Float(l) = left {
            let factor = Float::new(10f64.powi(prec as i32));
            left = Value::Float((l * factor.clone()).round() / factor);
        }
        if let Value::Float(r) = right {
            let factor = Float::new(10f64.powi(prec as i32));
            right = Value::Float((r * factor.clone()).round() / factor);
        }

        if let Value::List(ref list) = left {
            left = Value::List(list.clone());
        }
        if let Value::List(ref list) = right {
            right = Value::List(list.clone());
        }

        if let Value::Boolean(b) = left {
            left = Value::Float(if b { 1.0.into() } else { 0.0.into() });
        }
        if let Value::Boolean(b) = right {
            right = Value::Float(if b { 1.0.into() } else { 0.0.into() });
        }

        self.make_operation(left, right, &operator)
    }

    fn handle_unary_op(&mut self, statement: HashMap<Value, Value>) -> Value {
        let operand_opt = statement.get(&Value::String("operand".to_string()));
        let operand = match operand_opt {
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

    fn make_operation(&mut self, left: Value, right: Value, operator: &str) -> Value {
        debug_log(
            &format!(
                "<Operation: {} {} {}>",
                format_value(&left),
                operator,
                format_value(&right)
            ),
            &self.config,
            Some(self.use_colors.clone()),
        );

        match operator {
            "+" => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Value::Int(a + b),
                (Value::Float(a), Value::Float(b)) => Value::Float(a + b),
                (Value::Int(a), Value::Float(b)) => Value::Float((a + b.into()).into()),
                (Value::Float(a), Value::Int(b)) => Value::Float(a + b.into()),
                (Value::String(a), Value::String(b)) => Value::String(a + &b),
                (a, b) => self.raise("TypeError", &format!("Cannot add {} and {}", a.type_name(), b.type_name())),
            },

            "-" => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Value::Int(a - b),
                (Value::Float(a), Value::Float(b)) => Value::Float(a - b),
                (Value::Int(a), Value::Float(b)) => Value::Float((a - b.into()).into()),
                (Value::Float(a), Value::Int(b)) => Value::Float(a - b.into()),
                (a, b) => self.raise("TypeError", &format!("Cannot subtract {} and {}", a.type_name(), b.type_name())),
            },

            "*" => match (left, right) {
                (Value::Int(a), Value::Int(b)) => {
                    match a.checked_mulf(&Float::from(b)) {
                        Some(result) => Value::Int(result.into()),
                        None => {
                            self.raise("OverflowError", "Integer overflow during multiplication")
                        }
                    }
                }
                (Value::Float(a), Value::Float(b)) => Value::Float(a * b),
                (Value::Int(a), Value::Float(b)) => Value::Float((a * b.into()).into()),
                (Value::Float(a), Value::Int(b)) => Value::Float(a * b.into()),
                (a, b) => self.raise("TypeError", &format!("Cannot multiply {} and {}", a.type_name(), b.type_name())),
            }

            "/" => match &right {
                Value::Int(val) if val == &Int::new(0) => self.raise("ZeroDivisionError", "Division by zero."),
                Value::Float(f) if *f == 0.0.into() => self.raise("ZeroDivisionError", "Division by zero."),
                _ => match (left, right) {
                    (Value::Int(a), Value::Int(b)) => Value::Int(a / b),
                    (Value::Float(a), Value::Float(b)) => Value::Float(a / b),
                    (Value::Int(a), Value::Float(b)) => Value::Float((a / b.into()).into()),
                    (Value::Float(a), Value::Int(b)) => Value::Float((a / b.into()).into()),
                    (a, b) => self.raise("TypeError", &format!("Cannot divide {} by {}", a.type_name(), b.type_name())),
                }
            },

            "%" => match (left, right) {
                (_, Value::Int(val)) if val == 0.into() => self.raise("ZeroDivisionError", "Modulo by zero."),
                (_, Value::Float(f)) if f == 0.0.into() => self.raise("ZeroDivisionError", "Modulo by zero."),

                (Value::Int(a), Value::Int(b)) => Value::Int(a % b),
                (Value::Float(a), Value::Float(b)) => Value::Float((a % b.into()).into()),
                (Value::Int(a), Value::Float(b)) => Value::Float((a % b.into()).into()),
                (Value::Float(a), Value::Int(b)) => Value::Float(a % b.into()),

                (a, b) => self.raise("TypeError", &format!(
                    "Cannot modulo {} and {}", a.type_name(), b.type_name()
                )),
            }

            "^" => match (&left, &right) {
                (_, Value::Int(b)) if b.value.is_zero() => Value::Int(Int::new(1)),
                (_, Value::Float(b)) if b.value.is_zero() => Value::Int(Int::new(1)),

                (Value::Int(a), Value::Int(b)) => {
                    if b.value.sign() == num_bigint::Sign::Minus {
                        let base = Float::from_int(a.clone());
                        let exp = Float::from_int(b.clone());
                        Value::Float(base.powf(exp))
                    } else {
                        match b.to_u32() {
                            Some(exp_u32) => match a.checked_pow(exp_u32) {
                                Some(result) => Value::Int(result),
                                None => {
                                    let base = Float::from_int(a.clone());
                                    let exp = Float::from_int(b.clone());
                                    Value::Float(base.powf(exp))
                                }
                            },
                            None => {
                                let base = Float::from_int(a.clone());
                                let exp = Float::from_int(b.clone());
                                Value::Float(base.powf(exp))
                            }
                        }
                    }
                }

                (Value::Float(a), Value::Float(b)) => Value::Float(a.powf(b.clone())),

                (Value::Int(a), Value::Float(b)) => {
                    let base = Float::from_int(a.clone());
                    Value::Float(base.powf(b.clone()))
                }
                (Value::Float(a), Value::Int(b)) => {
                    let exp = b.clone();
                    Value::Float(a.powi(exp))
                }
                (a, b) => self.raise("TypeError", &format!(
                    "Operator '^' requires numeric operands, got '{}' and '{}'",
                    a.type_name(),
                    b.type_name()
                )),
            }

            "==" => Value::Boolean(left == right),
            "!=" => Value::Boolean(left != right),
            ">"  => Value::Boolean(left > right),
            "<"  => Value::Boolean(left < right),
            ">=" => Value::Boolean(left >= right),
            "<=" => Value::Boolean(left <= right),

            "&&" => Value::Boolean(left.is_truthy() && right.is_truthy()),
            "||" => Value::Boolean(left.is_truthy() || right.is_truthy()),
            "!"  => Value::Boolean(!right.is_truthy()),

            "~" | "in" => match &right {
                Value::List(list) => Value::Boolean(list.contains(&left)),
                Value::Map { keys, .. } => Value::Boolean(keys.contains(&left)),
                _ => self.raise("TypeError", &format!(
                    "Expected iterable (List or Map) on right side of '{}', got '{}'",
                    operator,
                    right.type_name()
                )),
            },

            "abs" => match right {
                Value::Int(n) => Value::Int(n.abs()),
                Value::Float(f) => Value::Float(f.abs()),
                _ => self.raise("TypeError", "Operator 'abs' requires a numeric operand."),
            },

            "xor" => Value::Boolean(
                (left.is_truthy() && !right.is_truthy()) || (!left.is_truthy() && right.is_truthy())
            ),
            "xnor" => Value::Boolean(
                (left.is_truthy() && right.is_truthy()) || (!left.is_truthy() && !right.is_truthy())
            ),

            _ => self.raise("SyntaxError", &format!("Unexpected operator: '{}'", operator)),
        }
    }

    fn handle_number(&mut self, map: HashMap<Value, Value>) -> Value {
        if let Some(Value::String(s)) = map.get(&Value::String("value".to_string())) {
            if s.contains('.') {
                match s.parse::<f64>() {
                    Ok(num) => Value::Float(Float::from(num)),
                    Err(_) => self.raise("RuntimeError", "Invalid float format"),
                }
            } else {
                match s.parse::<i64>() {
                    Ok(num) => Value::Int(Int::from(num)),
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
                                        let parsed = match Parser::new(tokens, self.config.clone(), expr.clone()).parse_safe() {
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
                    Err(e) => return self.raise("JSONDecodeError", &e),
                };
            }            
            
            let unquoted = if modified_string.len() >= 2 && modified_string.starts_with('"') && modified_string.ends_with('"') {
                modified_string[1..modified_string.len() - 1].to_string()
            } else {
                modified_string
            };
    
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
