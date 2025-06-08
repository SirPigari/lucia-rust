use std::collections::HashMap;
use crate::env::core::config::{Config, CodeBlocks, ColorScheme};
use crate::env::core::utils::{
    print_colored,
    hex_to_ansi,
    format_value,
    find_closest_match,
    TRUE, FALSE, NULL,
    debug_log,
    check_ansi,
    unescape_string,
    to_static,

};
use crate::env::core::pattern_reg::{extract_seed_end, predict_sequence};
use crate::env::core::types::{Int, Float, VALID_TYPES};
use crate::env::core::value::Value;
use crate::env::core::errors::Error;
use crate::env::core::variables::Variable;
use crate::env::core::statements::Statement;
use std::ops::{Add, Sub, Mul, Div, Rem, Neg};
use num_traits::{Zero, self};
use crate::env::core::native;
use crate::env::core::functions::{Function, FunctionMetadata, NativeFunction, Parameter, ParameterKind, Callable, NativeCallable};
use std::sync::Arc;
use num_bigint::BigInt;
use std::cmp::Ordering;

use crate::lexer::Lexer;
use crate::parser::{Parser, Token};


pub struct Interpreter {
    config: Config,
    err: Option<Error>,
    is_returning: bool,
    return_value: Value,
    source: String,
    stack: Vec<(String, HashMap<String, Value>, HashMap<String, Variable>)>,
    use_colors: bool,
    current_statement: Option<Statement>,
    variables: HashMap<String, Variable>,
}

impl Interpreter {
    pub fn new(config: Config, use_colors: bool) -> Self {
        let mut this = Self {
            config,
            err: None,
            return_value: NULL,
            is_returning: false,
            source: String::new(),
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
        error: Option<bool>,
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
            if error.unwrap_or(true) {
                self.raise(
                    "TypeError",
                    &format!("Expected type '{}', but got '{}'", expected_type, normalized_type),
                );
                return false;
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
                "ITERABLE" => self.handle_iterable(statement.clone()),
                "OPERATION" => self.handle_operation(statement.clone()),
                "UNARY_OPERATION" => self.handle_unary_op(statement.clone()),
                "CALL" => self.handle_call(statement.clone()),
                "METHOD_CALL" => self.handle_method_call(statement.clone()),
                "FOR" => self.handle_for_loop(statement.clone()),
                "VARIABLE" => self.handle_variable(statement.clone()),
                "VARIABLE_DECLARATION" => self.handle_variable_declaration(statement.clone()),
                "INDEX_ACCESS" => self.handle_index_access(statement.clone()),
                "ASSIGNMENT" => self.handle_assignment(statement.clone()),
                "TRY" => self.handle_try(statement.clone()),
                _ => self.raise("NotImplemented", &format!("Unsupported statement type: {}", t)),
            },
            _ => self.raise("SyntaxError", "Missing or invalid 'type' in statement map"),
        }
    }

    fn handle_try(&mut self, statement: HashMap<Value, Value>) -> Value {
        let body = match statement.get(&Value::String("body".to_string())) {
            Some(Value::List(body)) => body,
            _ => return self.raise("RuntimeError", "Expected a list for 'body' in try statement"),
        };
    
        let catch_body = match statement.get(&Value::String("catch_body".to_string())) {
            Some(Value::List(catch_body)) => catch_body,
            _ => return self.raise("RuntimeError", "Expected a list for 'catch' in try statement"),
        };
    
        let exception_vars = match statement.get(&Value::String("exception_vars".to_string())) {
            Some(Value::List(vars)) => vars,
            _ => return self.raise("RuntimeError", "Expected a list for 'exception_vars' in try statement"),
        };
    
        if exception_vars.len() > 3 {
            return self.raise("SyntaxError", "Too many exception variables (max is 3, err_msg, err_type, err_help)");
        }
    
        if exception_vars.is_empty() {
            return self.raise_with_help(
                "SyntaxError",
                "No exception variables provided",
                &format!("Use '_' if you want to ignore the caught exception(s): 'try: ... end catch ( {}_{} ): ... end'", 
                    hex_to_ansi("#1CC58B", Some(self.use_colors)), 
                    hex_to_ansi(&self.config.color_scheme.help, Some(self.use_colors)
                ))
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
    
        fn to_index(val: &Value, len: usize) -> Result<usize, String> {
            match val {
                Value::Int(i) => {
                    let idx = i.to_isize().ok_or("Failed to convert Int to isize")?;
                    let adjusted = if idx < 0 { len as isize + idx } else { idx };
                    if adjusted >= 0 && (adjusted as usize) < len {
                        Ok(adjusted as usize)
                    } else {
                        Err("Index out of range".to_string())
                    }
                }
                Value::String(s) => {
                    let idx: isize = s.parse().map_err(|_| "Failed to parse string to int")?;
                    let adjusted = if idx < 0 { len as isize + idx } else { idx };
                    if adjusted >= 0 && (adjusted as usize) < len {
                        Ok(adjusted as usize)
                    } else {
                        Err("Index out of range".to_string())
                    }
                }
                Value::Float(f) => {
                    if f.fract() != 0.0.into() {
                        return Err("Float index must have zero fractional part".to_string());
                    }
                    let idx = f.to_isize().ok_or("Failed to convert Float to isize")?;
                    let adjusted = if idx < 0 { len as isize + idx } else { idx };
                    if adjusted >= 0 && (adjusted as usize) < len {
                        Ok(adjusted as usize)
                    } else {
                        Err("Index out of range".to_string())
                    }
                }
                _ => Err("Index must be Int, String or Float with no fraction".to_string()),
            }
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
                        None => return self.raise_with_help("NameError", &format!("Variable '{}' is not defined", name), &format!("Use this instead: '{}{}: {} = {}{}'", check_ansi("\x1b[4m", &self.use_colors), name, right_value.type_name(), format_value(&right_value), check_ansi("\x1b[24m", &self.use_colors),)),
                    };
                    var.type_name().to_owned()
                };

                if !self.check_type(&expected_type, Some(&right_value.type_name().clone()), Some(right_value.clone()), Some(true)) {
                    return self.raise("TypeError", &format!(
                        "Invalid type for variable '{}': expected '{}', got '{}'",
                        name, expected_type, right_value.type_name()
                    ));
                }
                
                let var = self.variables.get_mut(name).unwrap();
                var.set_value(right_value);
                NULL
            }
            "INDEX_ACCESS" => {
                let object_val = match left_hashmap.get(&Value::String("object".to_string())) {
                    Some(v) => self.evaluate(Value::convert_to_statement(v)),
                    None => return self.raise("RuntimeError", "Missing 'object' in index access"),
                };
    
                let access_val = match left_hashmap.get(&Value::String("access".to_string())) {
                    Some(v) => v,
                    None => return self.raise("RuntimeError", "Missing 'access' in index access"),
                };
    
                let access_hashmap = match access_val {
                    Value::Map { keys, values } => keys.iter().cloned().zip(values.iter().cloned()).collect::<HashMap<_, _>>(),
                    _ => return self.raise("RuntimeError", "Expected a map for index access"),
                };
    
                if let Value::String(name) = &object_val {
                    let len = match self.variables.get(name) {
                        Some(var) => match &var.value {
                            Value::String(s) => s.chars().count(),
                            Value::List(l) => l.len(),
                            Value::Bytes(b) => b.len(),
                            _ => return self.raise("TypeError", "Object not indexable"),
                        },
                        None => return self.raise("NameError", &format!("Variable '{}' not found for index assignment", name)),
                    };
    
                    let start_val_opt = access_hashmap.get(&Value::String("start".to_string()));
                    let end_val_opt = access_hashmap.get(&Value::String("end".to_string()));
    
                    let start_idx = match start_val_opt {
                        Some(v) => match to_index(v, len) {
                            Ok(i) => i,
                            Err(e) => return self.raise("IndexError", &e),
                        },
                        None => 0,
                    };
    
                    let end_idx = match end_val_opt {
                        Some(v) => match to_index(v, len) {
                            Ok(i) => i,
                            Err(e) => return self.raise("IndexError", &e),
                        },
                        None => len,
                    };
    
                    if let Some(var) = self.variables.get_mut(name) {
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
            _ => self.raise("RuntimeError", &format!("Unsupported left type: {}", left_type)),
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
                    let idx = i.to_isize().ok_or_else(|| self.raise("ConversionError", "Failed to convert Int to isize"))?;
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
                    if f.fract() != 0.0.into() {
                        return Err(self.raise("ConversionError", "Float index must have zero fractional part"));
                    }
                    let idx = f.to_isize().ok_or_else(|| self.raise("ConversionError", "Failed to convert Float to isize"))?;
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
                Value::String(_) | Value::List(_) | Value::Bytes(_) => {
                    let start_idx = match to_index(&start_val) {
                        Ok(i) => i,
                        Err(e) => return e,
                    };
    
                    match &object_val {
                        Value::String(s) => s.chars().nth(start_idx).map(|c| Value::String(c.to_string())).unwrap_or_else(|| self.raise("IndexError", "Index out of range")),
                        Value::List(l) => l.get(start_idx).cloned().unwrap_or_else(|| self.raise("IndexError", "Index out of range")),
                        Value::Bytes(b) => b.get(start_idx).map(|&b| Value::Int((b as i64).into())).unwrap_or_else(|| self.raise("IndexError", "Index out of range")),
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
    
        if self.variables.contains_key(name) {
            return self.raise("NameError", &format!("Variable '{}' is already declared", name));
        }
    
        let value = match statement.get(&Value::String("value".to_string())) {
            Some(v) => v,
            None => return self.raise("RuntimeError", "Missing 'value' in variable declaration"),
        };
    
        let value = self.evaluate(value.convert_to_statement());
    
        let declared_type = match statement.get(&Value::String("var_type".to_string())) {
            Some(Value::String(t)) => t,
            _ => "any",
        };
    
        if !self.check_type(declared_type, Some(&value.type_name().clone()), Some(value.clone()), Some(true)) {
            return self.raise("TypeError", &format!("Variable '{}' declared with type '{}', but value is of type '{}'", name, declared_type, value.type_name()));
        }
    
        let variable = Variable::new(name.to_string(), value, declared_type.to_string(), false, true, true);
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
            return self.raise("NameError", &format!("Variable '{}' is not defined", name));
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
                    if evaluated_seed.is_empty() {
                        self.raise("ValueError", "Seed list cannot be empty");
                        return Value::Null;
                    }

                    for v in &evaluated_seed {
                        if let Value::Int(_) = v {} else {
                            self.raise("TypeError", "Seed elements must be Int");
                            if self.err.is_some() {
                                return Value::Null;
                            }
                        }
                    }

                    let nums: Vec<&Int> = evaluated_seed.iter().map(|v| {
                        if let Value::Int(i) = v {
                            i
                        } else {
                            unreachable!()
                        }
                    }).collect();

                    if self.err.is_some() {
                        return Value::Null;
                    }

                    if nums.len() >= 2 {
                        let initial_step = &nums[1].value - &nums[0].value;

                        for i in 1..(nums.len() - 1) {
                            let current_step = &nums[i + 1].value - &nums[i].value;
                            if current_step != initial_step {
                                self.raise("PatternCompletionError", "Seed values do not have consistent step");
                                if self.err.is_some() {
                                    return Value::Null;
                                }
                            }
                        }
                    }

                    let end_int = match &end {
                        Value::Int(i) => i,
                        _ => {
                            self.raise("TypeError", "End value must be Int");
                            return Value::Null;
                        }
                    };

                    let step = if nums.len() == 1 {
                        match nums[0].value.cmp(&end_int.value) {
                            std::cmp::Ordering::Less | std::cmp::Ordering::Equal => BigInt::from(1),
                            std::cmp::Ordering::Greater => BigInt::from(-1),
                        }
                    } else {
                        &nums[1].value - &nums[0].value
                    };

                    if step.is_zero() {
                        self.raise("ValueError", "Step cannot be zero");
                        return Value::Null;
                    }

                    let diff = &end_int.value - &nums.last().unwrap().value;

                    let step_sign_int = match step.sign() {
                        num_bigint::Sign::Plus => BigInt::from(1),
                        num_bigint::Sign::Minus => BigInt::from(-1),
                        num_bigint::Sign::NoSign => BigInt::from(0),
                    };
                    
                    if (&diff * &step_sign_int) < BigInt::zero() {
                        self.raise("PatternCompletionError", "End value is unreachable with given seed and step");
                        return Value::Null;
                    }
                    

                    if &diff % &step != BigInt::zero() {
                        self.raise("PatternCompletionError", "Pattern does not fit into range defined by end");
                        return Value::Null;
                    }

                    let mut result_list: Vec<Value> = evaluated_seed.clone();

                    let mut current = nums.last().unwrap().value.clone();

                    loop {
                        current = current + &step;

                        let done = if step > BigInt::from(0) {
                            current > end_int.value
                        } else {
                            current < end_int.value
                        };

                        if done {
                            break;
                        }

                        result_list.push(Value::Int(Int { value: current.clone() }));
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
                        .map(|v| Value::Float(Float { value: v.into() }))
                        .collect()
                } else {
                    vec_f64.into_iter()
                        .map(|v| Value::Int(Int::new(v as i64)))
                        .collect()
                };

                return Value::List(result_list);
            }
            _ => self.raise("TypeError", &format!("Unsupported iterable type: {}", iterable_type)),
        }
    }

    fn handle_method_call(&mut self, statement: HashMap<Value, Value>) -> Value {
        let Some(object) = statement.get(&Value::String("object".to_string())).cloned() else {
            return Value::Error("MethodCallError", "Missing 'object' in method call");
        };
    
        let Some(Value::String(method)) = statement.get(&Value::String("method".to_string())).cloned() else {
            return Value::Error("MethodCallError", "Missing or invalid 'method' in method call");
        };
    
        let (pos_args, named_args) = match self.translate_args(
            statement.get(&Value::String("pos_args".to_string())).cloned().unwrap_or(Value::List(vec![])),
            statement.get(&Value::String("named_args".to_string())).cloned().unwrap_or(Value::Map { keys: vec![], values: vec![] }),
        ) {
            Ok(args) => args,
            Err(err) => {
                return self.raise("MethodCallError", to_static(err.to_string()));
            }
        };
        
    
        let method_name = method.as_str();
        let object_value = self.evaluate(object.convert_to_statement());
        let object_type = object_value.type_name();

        let mut object_variable = Variable::new(
            "_".to_string(),
            object_value,
            object_type.clone(),
            false,
            true,
            true,
        );

        if !object_variable.is_init() {
            object_variable.init_properties();
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
                            "Method '{}' expects no arguments, but got {}.",
                            method_name,
                            passed_args_count
                        ),
                    );
                }
                
                if passed_args_count > expected_args_count {
                    return self.raise(
                        "TypeError",
                        &format!(
                            "Too many arguments for method '{}'. Expected at most {}, but got {}.",
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
                
                if provided_positional_count < required_positional_count {
                    return self.raise(
                        "TypeError",
                        &format!(
                            "Missing required positional argument{} for method '{}'. Expected at least {}, but got {}.",
                            if required_positional_count == 1 { "" } else { "s" },
                            method_name,
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
                                if self.check_type(param_type, Some(&arg_value.type_name()), Some(NULL), Some(false)) {
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
                                if self.check_type(param_type, Some(&named_value.type_name()), Some(named_value.clone()), Some(false)) {
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
                                if !self.check_type(param_type, Some(&arg.type_name()), Some(arg.clone()), Some(false)) {
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
                                if self.check_type(param_type, Some(&value.type_name()), Some(value.clone()), Some(false)) {
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
                        "<Method call: {}.{}({})>",
                        object_variable.type_name(),
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

                self.stack.push((format!("{}.{}", object_variable.type_name(), method_name).to_string(), final_args.clone(), self.variables.clone()));

                let result = func.call(&final_args);  // func.call(&final_args, &mut self.stack);
                self.stack.pop();
                if let Value::Error(err_type, err_msg) = &result {
                    return self.raise(err_type, err_msg);
                }
                if !self.check_type(&metadata.return_type, Some(&result.type_name()), Some(result.clone()), Some(false)) {
                    return self.raise_with_help(
                        "TypeError",
                        &format!("Return value does not match expected type '{}', got '{}'", metadata.return_type, result.type_name()),
                        &format!(
                            "Expected: '{}', but got: '{}'",
                            metadata.return_type,
                            result.type_name()
                        ),
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
                                if self.check_type(param_type, Some(&arg_value.type_name()), Some(NULL), Some(false)) {
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
                                if self.check_type(param_type, Some(&named_value.type_name()), Some(named_value.clone()), Some(false)) {
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
                                if !self.check_type(param_type, Some(&arg.type_name()), Some(arg.clone()), Some(false)) {
                                    return self.raise(
                                        "TypeError",
                                        &format!("Variadic argument #{} does not match expected type '{}'", i, param_type),
                                    );
                                }
                            }
                    
                            final_args.insert(param_name.clone(), Value::List(variadic_args));
                    
                            pos_index = positional.len();
                            named_map.remove(param_name);
                        }
                        ParameterKind::KeywordVariadic => {
                            if let Some(named_value) = named_map.remove(param_name) {
                                if self.check_type(param_type, Some(&named_value.type_name()), Some(named_value.clone()), Some(false)) {
                                    final_args.insert(param_name.clone(), named_value);
                                } else {
                                    return self.raise(
                                        "TypeError",
                                        &format!(
                                            "Keyword argument '{}' does not match expected type '{}', got '{}'",
                                            param_name,
                                            param_type,
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

                let result = func.call(&final_args);  // func.call(&final_args, &mut self.stack);
                self.stack.pop();
                if let Value::Error(err_type, err_msg) = &result {
                    return self.raise(err_type, err_msg);
                }
                if !self.check_type(&metadata.return_type, Some(&result.type_name()), Some(result.clone()), Some(false)) {
                    return self.raise_with_help(
                        "TypeError",
                        &format!("Return value does not match expected type '{}', got '{}'", metadata.return_type, result.type_name()),
                        &format!(
                            "Expected: '{}', but got: '{}'",
                            metadata.return_type,
                            result.type_name()
                        ),
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
        let left_opt = statement.get(&Value::String("left".to_string()));
        let left = match left_opt {
            Some(val_left) => self.evaluate(val_left.convert_to_statement()),
            None => {
                self.raise("KeyError", "Missing 'left' key in the statement.");
                return NULL;
            }
        };

        if self.err.is_some() {
            return NULL;
        }

        let right_opt = statement.get(&Value::String("right".to_string()));
        let right = match right_opt {
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

    fn make_operation(&mut self, left: Value, right: Value, mut operator: &str) -> Value {
        operator = match operator {
            "isnt" => "!=",
            "isn't" => "!=",
            "or" => "||",
            "and" => "&&",
            "is" => "==",
            "not" => "!",
            "nein" => "!=",
            _ => operator,
        };

        // Handle if both left and right are zero for division or modulo
        if left.is_zero() && right.is_zero() {
            match operator {
                "/" => {
                    return self.raise("MathError", "Division by zero: 0 / 0 is undefined");
                }
                "%" => {
                    return self.raise("MathError", "Modulo by zero: 0 % 0 is undefined");
                }
                _ => {}
            }
        }

        if left.is_nan() || right.is_nan() {
            return self.raise("MathError", "Operation with NaN is not allowed");
        }

        if left.is_infinity() || right.is_infinity() {
            return self.raise("MathError", "Operation with Infinity is not allowed");
        }

        if operator == "abs" {
            debug_log(
                &format!(
                    "<Operation: |{}|>",
                    format_value(&right),
                ),
                &self.config,
                Some(self.use_colors.clone()),
            );
        } else {
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
        }

        match operator {
            "+" => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Value::Int(a + b),
                (Value::Float(a), Value::Float(b)) => Value::Float(a + b),
                (Value::Int(a), Value::Float(b)) => Value::Float(Float::from_int(a) + b),
                (Value::Float(a), Value::Int(b)) => Value::Float(a + Float::from_int(b)),
                (Value::String(a), Value::String(b)) => Value::String(a + &b),
                (a, b) => self.raise("TypeError", &format!("Cannot add {} and {}", a.type_name(), b.type_name())),
            },

            "-" => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Value::Int(a - b),
                (Value::Float(a), Value::Float(b)) => Value::Float(a - b),
                (Value::Int(a), Value::Float(b)) => Value::Float(Float::from_int(a) - b),
                (Value::Float(a), Value::Int(b)) => Value::Float(a - Float::from_int(b)),
                (a, b) => self.raise("TypeError", &format!("Cannot subtract {} and {}", a.type_name(), b.type_name())),
            },

            "*" => match (left, right) {
                (Value::Int(a), Value::Int(b)) => {
                    let result = Float::from(a.clone()) * Float::from(b.clone());
                    Value::Float(result)
                }
                (Value::Float(a), Value::Float(b)) => Value::Float(a * b),
                (Value::Int(a), Value::Float(b)) => Value::Float(Float::from_int(a) * b),
                (Value::Float(a), Value::Int(b)) => Value::Float(a * Float::from_int(b)),
                (Value::String(a), Value::Int(b)) => {
                    let count = b.to_usize().unwrap_or(0);
                    Value::String(a.repeat(count))
                }
                (a, b) => self.raise("TypeError", &format!("Cannot multiply {} and {}", a.type_name(), b.type_name())),
            },

            "/" => match &right {
                Value::Int(val) if val.is_zero() => self.raise("ZeroDivisionError", "Division by zero."),
                Value::Float(f) if *f == 0.0.into() => self.raise("ZeroDivisionError", "Division by zero."),
                _ => match (left, right) {
                    (Value::Int(a), Value::Int(b)) => Value::Float(Float::from(a) / Float::from(b)),
                    (Value::Float(a), Value::Float(b)) => Value::Float(a / b),
                    (Value::Int(a), Value::Float(b)) => Value::Float(Float::from(a) / b),
                    (Value::Float(a), Value::Int(b)) => Value::Float(a / Float::from(b)),
                    (a, b) => self.raise("TypeError", &format!("Cannot divide {} by {}", a.type_name(), b.type_name())),
                }
            },        

            "%" => match (left, right) {
                (_, Value::Int(val)) if val == 0.into() => self.raise("ZeroDivisionError", "Modulo by zero."),
                (_, Value::Float(f)) if f == 0.0.into() => self.raise("ZeroDivisionError", "Modulo by zero."),

                (Value::Int(a), Value::Int(b)) => Value::Float((a % b).into()),
                (Value::Float(a), Value::Float(b)) => Value::Float((a % b.into()).into()),
                (Value::Int(a), Value::Float(b)) => Value::Float((a % b.into()).into()),
                (Value::Float(a), Value::Int(b)) => Value::Float(a % b.into()),

                (a, b) => self.raise("TypeError", &format!(
                    "Cannot modulo {} and {}", a.type_name(), b.type_name()
                )),
            }

            "^" => match (&left, &right) {
                (_, Value::Int(b)) if b.value.is_zero() => Value::Float(1.0.into()),
                (_, Value::Float(b)) if b.value.is_zero() => Value::Float(1.0.into()),

                (Value::Int(a), Value::Int(b)) => {
                    if b.value.sign() == num_bigint::Sign::Minus {
                        let base = Float::from_int(a.clone());
                        let exp = Float::from_int(b.clone());
                        Value::Float(base.powf(exp))
                    } else {
                        match b.to_u32() {
                            Some(exp_u32) => match a.checked_pow(exp_u32) {
                                Some(result) => Value::Float(result.into()),
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
                    Value::Float(a.powf(exp.into()))
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
                Value::Int(n) => Value::Float(n.abs().into()),
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
