use std::collections::HashMap;
use crate::env::helpers::config::{Config, CodeBlocks, ColorScheme};
use crate::env::helpers::utils::{print_colored, hex_to_ansi, Value, Error};
use lazy_static::lazy_static;

pub struct Interpreter {
    config: Config,
    err: Option<Error>,
    is_returning: bool,
    return_value: Value,
    source: String,
    stack: Vec<Value>,
    use_colors: bool,
    current_statement: Option<Value>,
}

impl Interpreter {
    pub fn new(config: Config, source: String, use_colors: bool) -> Self {
        Self { 
            config,
            err: None,
            return_value: Value::Null,
            is_returning: false,
            source: source,
            stack: vec![],
            use_colors: use_colors,
            current_statement: None,
        }
    }

    pub fn interpret(&mut self, statements: Vec<Value>) -> Result<Value, Error> {
        for statement in statements {
            if let Some(err) = &self.err {
                println!("Error: {}", err.msg);
                return Err(err.clone());
            }

            if let Value::Map { keys, values, line, column } = statement.clone() {
                self.current_statement = Some(statement.clone());

                let value = self.evaluate(statement.clone());
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
            if let Value::Map { line, column, .. } = current_statement {
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
    
        Value::Null
    }
    
    pub fn evaluate(&mut self, statement: Value) -> Value {
        if self.err.is_some() {
            return Value::Null;
        }
    
        match &statement {
            Value::Map { keys, values, .. } => {
                let map: HashMap<_, _> = keys.iter().cloned().zip(values.iter().cloned()).collect();
    
                match map.get(&Value::String("type".to_string())) {
                    Some(Value::String(t)) => match t.as_str() {
                        "NUMBER" => self.handle_number(map),
                        _ => self.raise("NotImplemented", &format!("Unsupported statement type: {}", t)),
                    },
                    _ => self.raise("SyntaxError", "Missing or invalid 'type' in statement map"),
                }
            }
            _ => self.raise("SyntaxError", "Expected a statement map"),
        }
    }

    fn handle_number(&mut self, map: HashMap<Value, Value>) -> Value {
        if let Some(Value::String(s)) = map.get(&Value::String("value".to_string())) {
            if s.contains('.') {
                match s.parse::<f64>() {
                    Ok(num) => Value::Number(num),
                    Err(_) => self.raise("RuntimeError", "Invalid float format"),
                }
            } else {
                match s.parse::<i64>() {
                    Ok(num) => Value::Number(num as f64),
                    Err(_) => self.raise("RuntimeError", "Invalid integer format"),
                }
            }
        } else {
            self.raise("RuntimeError", "Missing 'value' in number statement")
        }
    }
}
