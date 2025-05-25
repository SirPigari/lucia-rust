use std::collections::HashMap;
use crate::env::helpers::config::{Config, CodeBlocks, ColorScheme};
use crate::env::helpers::utils::{print_colored, hex_to_ansi, Value, Error, Variable, Statement, format_value, levenshtein_distance, TRUE, FALSE, NULL, debug_log, Int, Float};
use std::ops::{Add, Sub, Mul, Div, Rem, Neg};
use num_traits::Zero;
use lazy_static::lazy_static;
use crate::env::helpers::native;
use crate::env::helpers::functions::{Function, FunctionMetadata, NativeFunction, Parameter, ParameterKind, Callable, NativeCallable};
use std::sync::Arc;


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

        this
    }

    fn check_type(
        &mut self,
        type_: &str,
        expected: Option<&str>,
        return_value: Option<Value>,
        error: bool,
    ) -> bool {
        let valid_types = vec!["void", "any", "null", "int", "float", "bool", "string", "map"];

        let mut types_mapping = std::collections::HashMap::new();
        types_mapping.insert("void", "null");
        types_mapping.insert("any", "any");
        types_mapping.insert("null", "null");
        types_mapping.insert("Decimal", "float");
        types_mapping.insert("method", "function");
        types_mapping.insert("int", "int");
        types_mapping.insert("float", "float");
        types_mapping.insert("bool", "bool");
        types_mapping.insert("string", "string");
        types_mapping.insert("list", "list");
        types_mapping.insert("map", "map");

        let type_ = types_mapping.get(type_).unwrap_or(&type_);
        let binding = expected.unwrap_or("");  // Bind the value to a longer-lived variable.
        let expected = expected.and_then(|e| types_mapping.get(e)).unwrap_or(&binding);

        let mut type_ = type_;
        let mut expected = expected;

        //if let Some(return_value) = return_value {
        //    if return_value.is_boolean() {
        //        return_value = Some(return_value.to_boolean());
        //    }
        //}

        if *type_ == "bool" && *expected == "null" && return_value == Some(NULL) {
            return true;
        }

        if expected == expected {
            if ["any", type_].contains(&type_) {
                return true;
            }
            if !valid_types.contains(&type_) || !valid_types.contains(&expected) {
                return self._handle_invalid_type(expected, valid_types);
            }

            if *type_ == "int" && *expected == "float" {
                return true;
            }

            if type_ != expected {
                if error {
                    self.raise("TypeError", &format!("Expected type '{}', but got '{}'", expected, type_));
                } else {
                    return false;
                }
            }
            return true;
        }

        valid_types.contains(&type_) || self._handle_invalid_type(type_, valid_types)
    }

    fn _handle_invalid_type(&mut self, type_: &str, valid_types: Vec<&str>) -> bool {
        if let Some(closest_match) = self.find_closest_match(valid_types, type_) {
            self.raise("TypeError", &format!("Type '{}' is not supported. Did you mean: '{}'?", type_, closest_match));
        } else {
            self.raise("TypeError", &format!("Type '{}' is not supported.", type_));
        }
        false
    }

    fn find_closest_match(&self, valid_types: Vec<&str>, type_: &str) -> Option<String> {
        valid_types
            .iter()
            .min_by_key(|&valid_type| levenshtein_distance(valid_type, type_))
            .map(|&match_type| match_type.to_string())
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
                    line: (*line, "".to_string()),
                    column: *column,
                });
            } else {
                self.err = Some(Error {
                    error_type: error_type.to_string(),
                    msg: msg.to_string(),
                    line: (0, "".to_string()),
                    column: 0,
                });
            }
        } else {
            self.err = Some(Error {
                error_type: error_type.to_string(),
                msg: msg.to_string(),
                line: (0, "".to_string()),
                column: 0,
            });
        }
    
        NULL
    }
    
    pub fn evaluate(&mut self, statement: Statement) -> Value {
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
        let value_opt = self.variables.get(function_name).map(|var| var.get_value().clone());

        if let Some(Value::Function(func)) = value_opt {
            let metadata = func.metadata();

            let positional = pos_args.clone();

            let mut named_map = named_args.clone();

            let mut final_pos_args = Vec::new();
            let mut final_named_args = HashMap::new();

            let mut pos_index = 0;

            for param in &metadata.parameters {
                match param.kind {
                    ParameterKind::Positional => {
                        if pos_index < positional.len() {
                            final_pos_args.push(positional[pos_index].clone());
                            pos_index += 1;
                        } else if let Some(val) = named_map.remove(&param.name) {
                            final_pos_args.push(val);
                        } else {
                            return self.raise("ArgumentError", &format!("Missing argument: {}", param.name));
                        }
                    }
                    ParameterKind::Optional => {
                        if pos_index < positional.len() {
                            final_pos_args.push(positional[pos_index].clone());
                            pos_index += 1;
                        } else if let Some(val) = named_map.remove(&param.name) {
                            final_pos_args.push(val);
                        } else if let Some(default) = &param.default {
                            final_pos_args.push(default.clone());
                        } else {
                            return self.raise("ArgumentError", &format!("Missing optional argument: {}", param.name));
                        }
                    }
                    ParameterKind::Variadic => {
                        // Instead of wrapping in a List here, defer to native function's expected flattening
                        let rest = positional[pos_index..].to_vec();
                        final_pos_args.extend(rest);
                        pos_index = positional.len();
                    }
                    ParameterKind::KeywordVariadic => {
                        // Flatten named_map directly into final_named_args
                        for (k, v) in named_map.drain() {
                            final_named_args.insert(k, v);
                        }
                    }
                }
            }

            func.call(&final_pos_args, &final_named_args)
        } else {
            self.raise("NameError", &format!("Function '{}' is not defined", function_name))
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
            Value::String(s.clone()[1..s.len()-1].to_string())
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
