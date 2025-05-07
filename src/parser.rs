use std::collections::HashMap;
use crate::env::helpers::config::{Config, CodeBlocks, ColorScheme};
use crate::env::helpers::utils::{print_colored, hex_to_ansi, Value, Error};
use lazy_static::lazy_static;

lazy_static! {
    static ref DEFAULT_TOKEN: Token = Token("".to_string(), "".to_string());
}

fn get_type_default(type_: &str) -> Value {
    match type_ {
        "int" => Value::Number(0.0),
        "float" => Value::Number(0.0),
        "string" => Value::String(String::new()),
        "bool" => Value::Boolean(false),
        "any" => Value::Null,
        "map" => Value::Map { keys: vec![], values: vec![] },
        "list" => Value::List(vec![]),
        "list_completion" => Value::ListCompletion { pattern: vec![], end: None },
        _ => Value::Null,
    }
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct Token(pub String, pub String);

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    statements: Vec<Value>,
    aliases: HashMap<Token, Option<Token>>,
    config: Config, 
    include_whitespace: bool,
    source: String,
}

impl Parser {
    pub fn new(tokens: Vec<Token>, config: Config, source: String) -> Self {
        Self {
            tokens,
            pos: 0,
            statements: vec![],
            aliases: HashMap::new(),
            config,
            include_whitespace: false,
            source,
        }
    }

    fn token(&mut self) -> Option<&Token> {
        while self.pos < self.tokens.len() {
            let token = &self.tokens[self.pos];
            if self.include_whitespace || token.0 != "WHITESPACE" {
                return Some(self.apply_aliases(token));
            }
            self.pos += 1;
        }
        None
    }

    fn apply_aliases<'a>(&'a self, token: &'a Token) -> &'a Token {
        self.aliases.get(token).and_then(|v| v.as_ref()).unwrap_or(token)
    }

    pub fn raise(&self, error_type: &str, msg: &str) -> Result<(Vec<Value>, Vec<(Value, Value)>), Error> {
        Err(Error {
            error_type: error_type.to_string(),
            msg: msg.to_string(),
            line: (self.current_line(), self.source.clone()),
            column: self.get_line_column(),
        })
    }

    fn next(&mut self) -> Option<&Token> {
        self.pos += 1;
        self.token()
    }

    fn check_for(&mut self, expected_type: &str, expected_value: &str) {
        if let Some(token) = self.token() {
            if token.0 != expected_type || token.1 != expected_value {
                panic!("Expected token type: {}, value: {} but found: {:?}", expected_type, expected_value, token);
            }
        } else {
            panic!("Expected token type: {}, value: {} but found end of input", expected_type, expected_value);
        }
    }

    fn get_next(&mut self) -> Option<&Token> {
        let mut offset = 1;
        while self.pos + offset < self.tokens.len() {
            if self.tokens[self.pos + offset].0 != "WHITESPACE" || self.include_whitespace {
                return Some(self.apply_aliases(&self.tokens[self.pos + offset]));
            }
            offset += 1;
        }
        None
    }

    pub fn current_line(&self) -> usize {
        let index = self.tokens.iter().take(self.pos).map(|token| token.1.len()).sum::<usize>();
        self.source[..index].chars().filter(|&c| c == '\n').count() + 1
    }

    pub fn get_line_column(&self) -> usize {
        let mut index = self.tokens.iter().take(self.pos).map(|token| token.1.len()).sum::<usize>();
        let mut char_pos = 0;
        while index > 0 && &self.source[index - 1..index] != "\n" {
            char_pos += 1;
            index -= 1;
        }
        char_pos + 1
    }

    pub fn parse_safe(&mut self) -> Result<Vec<Value>, Error> {
        let mut statements = Vec::new();
        while let Some(_) = self.token() {
            match self.parse_expression() {
                Ok(stmt) => statements.push(stmt),
                Err(e) => return Err(e),
            }
        }
        Ok(statements)
    }

    pub fn parse(&mut self) -> Vec<Value> {
        while self.pos < self.tokens.len() {
            if let Some(expr) = self.parse_expression().ok() {
                self.statements.push(expr);
            }
            self.pos += 1;
        }
        self.statements.clone()
    }

    fn parse_expression(&mut self) -> Result<Value, Error> {
        let token = self.token().ok_or_else(|| Error::new("SyntaxError", "Expected token"))?.clone();
        let next_token = self.get_next();
        let token_type = &token.0;
        let token_value = &token.1;

        if token_type == "IDENTIFIER" && next_token.as_ref().map(|t| t.0 == "SEPARATOR" && t.1 == "(").unwrap_or(false) {
            return self.parse_function_call();
        }

        if token_type == "NUMBER" {
            self.next();
            return Ok(Value::Number(token_value.parse::<f64>().unwrap_or(0.0)));
        }

        self.pos += 1;
        Ok(Value::Null)
    }

    fn parse_function_call(&mut self) -> Result<Value, Error> {
        let name = self.token().ok_or_else(|| Error::new("SyntaxError", "Expected function name"))?.1.clone();
        self.next();
        self.check_for("SEPARATOR", "(");
        self.next();
        let (pos_args, named_args) = self.parse_arguments()?;
        self.check_for("SEPARATOR", ")");
        self.next();

        if self.token().unwrap().0 == "IDENTIFIER" && self.get_next() == Some(&Token("OPERATOR".to_string(), "=".to_string())) {
            return Err(Error::new("SyntaxError", "Unexpected '=' in function call"));
        }

        let (keys, values): (Vec<_>, Vec<_>) = named_args.into_iter().unzip();
        
        Ok(Value::Map {
            keys: vec![
                Value::String("type".to_string()),
                Value::String("name".to_string()),
                Value::String("pos_arguments".to_string()),
                Value::String("named_arguments".to_string()),
            ],
            values: vec![
                Value::String("CALL".to_string()),
                Value::String(name.to_string()),
                Value::List(pos_args),
                Value::Map { keys, values },
            ],
        })
    }

    fn parse_arguments(&mut self) -> Result<(Vec<Value>, Vec<(Value, Value)>), Error> {
        let mut pos_args = Vec::new();
        let mut named_args = Vec::new();
        let mut seen_named = false;
    
        while let Some(current_token) = self.token().cloned() {
            let next_token = self.get_next().cloned();
    
            if current_token.0 == "SEPARATOR" && current_token.1 == ")" {
                break;
            }
    
            if let Some(Token(t_type, t_val)) = &next_token {
                if t_type == "OPERATOR" && t_val == "=" {
                    let name = current_token.1;
                    self.next();
                    self.next();
                    let value = self.parse_expression()?;
                    named_args.push((Value::String(name), value));
                    seen_named = true;
                } else {
                    if seen_named {
                        return self.raise(
                            "ArgumentOrderError",
                            "Positional arguments cannot follow named arguments",
                        );
                    }
                    pos_args.push(self.parse_expression()?);
                }
            } else {
                if seen_named {
                    return self.raise(
                        "ArgumentOrderError",
                        "Positional arguments cannot follow named arguments",
                    );
                }
                pos_args.push(self.parse_expression()?);
            }
    
        
            if let Some(Token(t_type, t_val)) = self.token() {
                if t_type == "SEPARATOR" && t_val == "," {
                    self.next();
                }
            }
        }
    
        Ok((pos_args, named_args))
    }
    
    
    
    
}
