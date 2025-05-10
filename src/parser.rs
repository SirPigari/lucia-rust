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
        "map" => Value::Map { keys: vec![], values: vec![], line: 0, column: 0 },
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
    err: Option<Error>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>, config: Config, source: String) -> Self {
        Self {
            tokens,
            pos: 0,
            statements: vec![],
            aliases: HashMap::new(),
            config: config.clone(),
            include_whitespace: false,
            source,
            err: None,
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

    pub fn raise(&mut self, error_type: &str, msg: &str) -> Value {
        self.err = Some(Error {
            error_type: error_type.to_string(),
            msg: msg.to_string(),
            line: (self.current_line(), self.source.clone()),
            column: self.get_line_column(),
        });
        Value::Null
    }

    fn next(&mut self) -> Option<&Token> {
        self.pos += 1;
        self.token()
    }

    fn check_for(&mut self, expected_type: &str, expected_value: &str) -> (Vec<Value>, Vec<(Value, Value)>) {
        if let Some(token) = self.token() {
            let (token_type, token_value) = (token.0.clone(), token.1.clone());
            if token_type != expected_type || token_value != expected_value {
                self.raise("UEFError", &format!(
                    "Expected token type: {}, value: {} but found: {}",
                    expected_type, expected_value, token_value
                ));
            }
        } else {
            self.raise("UEFError", &format!(
                "Expected token type: {}, value: {} but found end of input",
                expected_type, expected_value
            ));
        }
        (vec![], vec![])
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
        while let Some(_) = self.token().cloned() {
            let stmt = self.parse_expression();
            if self.err.is_some() {
                return Err(self.err.clone().unwrap());
            }
            if let Value::Null = stmt {
                continue;
            } else {
                statements.push(stmt);
            }
        }
        Ok(statements)
    }

    pub fn parse(&mut self) -> Vec<Value> {
        while self.pos < self.tokens.len() {
            let expr = self.parse_expression();
            if expr != Value::Null {
                self.statements.push(expr);
            }
            self.pos += 1;
        }
        self.statements.clone()
    }
    
    fn parse_expression(&mut self) -> Value {
        let token = if let Some(tok) = self.token() {
            tok.clone()
        } else {
            self.raise("SyntaxError", "Expected token");
            return Value::Null;
        };

        let line = self.current_line();
        let column = self.get_line_column();
    
        let next_token = self.get_next();
        let token_type = &token.0;
        let token_value = &token.1;
    
        if token_type == "IDENTIFIER"
            && next_token
                .as_ref()
                .map(|t| t.0 == "SEPARATOR" && t.1 == "(")
                .unwrap_or(false)
        {
            return self.parse_function_call();
        }
    
        if token_type == "SEPARATOR" && token_value == "[" {
            self.next();
            let mut elements = Vec::new();
    
            while let Some(tok) = self.token() {
                if tok.0 == "SEPARATOR" && tok.1 == "]" {
                    break;
                }
    
                let element = self.parse_expression();
                elements.push(element);
    
                if let Some(tok) = self.token() {
                    if tok.0 == "SEPARATOR" && tok.1 == "]" {
                        break;
                    }
    
                    if tok.0 == "SEPARATOR" && tok.1 == "..." {
                        self.check_for("SEPARATOR", "...");
                        self.next();
                        let end = self.parse_expression();
                        self.check_for("SEPARATOR", "]");
                        self.next();
                        return Value::Map {
                            keys: vec![
                                Value::String("type".to_string()),
                                Value::String("iterable_type".to_string()),
                                Value::String("pattern".to_string()),
                                Value::String("end".to_string()),
                            ],
                            values: vec![
                                Value::String("ITERABLE".to_string()),
                                Value::String("LIST_COMPLETION".to_string()),
                                Value::List(elements),
                                end,
                            ],
                            line,
                            column,
                        };
                    }
                }
    
                if let Some(tok) = self.token() {
                    if tok.0 == "SEPARATOR" && tok.1 == "," {
                        self.next();
                    }
                }
            }
    
            self.next();
            return Value::Map {
                keys: vec![
                    Value::String("type".to_string()),
                    Value::String("iterable_type".to_string()),
                    Value::String("elements".to_string()),
                ],
                values: vec![
                    Value::String("ITERABLE".to_string()),
                    Value::String("LIST".to_string()),
                    Value::List(elements),
                ],
                line,
                column,
            };
        }
    
        if token_type == "NUMBER" {
            self.next();
            return Value::Map {
                keys: vec![
                    Value::String("type".to_string()),
                    Value::String("value".to_string()),
                ],
                values: vec![
                    Value::String("NUMBER".to_string()),
                    Value::String(token_value.clone()),
                ],
                line,
                column,
            };
        }
    
        if token_type == "STRING" {
            self.next();
            return Value::Map {
                keys: vec![
                    Value::String("type".to_string()),
                    Value::String("value".to_string()),
                ],
                values: vec![
                    Value::String("STRING".to_string()),
                    Value::String(token_value.clone()),
                ],
                line,
                column,
            };
        }
    
        if token_type == "BOOLEAN" {
            self.next();
            let literal_value = match token_value.as_str() {
                "true" => true,
                "false" => false,
                "null" => false,
                _ => false,
            };
    
            return Value::Map {
                keys: vec![
                    Value::String("type".to_string()),
                    Value::String("value".to_string()),
                    Value::String("literal_value".to_string()),
                ],
                values: vec![
                    Value::String("BOOLEAN".to_string()),
                    Value::String(token_value.clone()),
                    Value::String(literal_value.to_string()),
                ],
                line,
                column,
            };
        }
        
        self.raise("SyntaxError", &format!(
            "Invalid syntax. '{}' was unexpected.",
            token_value
        ));
        Value::Null
    }
    
    
    fn parse_function_call(&mut self) -> Value {
        let name = self.token().ok_or_else(|| Error::new("SyntaxError", "Expected function name")).unwrap().1.clone();
        if self.err.is_some() {
            return Value::Null;
        }

        let line = self.current_line();
        let column = self.get_line_column();

        self.next();
        self.check_for("SEPARATOR", "(");
        self.next();
        let (pos_args, named_args) = self.parse_arguments();
        if self.err.is_some() {
            return Value::Null;
        }
        self.check_for("SEPARATOR", ")");
        self.next();

        if pos_args.is_empty() && named_args.is_empty() {
            let (keys, values): (Vec<_>, Vec<_>) = named_args.into_iter().unzip();
            return Value::Map {
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
                    Value::Map { keys, values, line, column },
                ],
                line,
                column,
            }
        }

        let (keys, values): (Vec<_>, Vec<_>) = named_args.into_iter().unzip();
        
        Value::Map {
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
                Value::Map { keys, values , line, column },
            ],
            line,
            column,
        }
    }

    fn parse_arguments(&mut self) -> (Vec<Value>, Vec<(Value, Value)>) {
        let mut pos_args = Vec::new();
        let mut named_args = Vec::new();
        let mut seen_named = false;
    
        while let Some(current_token) = self.token().cloned() {
            let next_token = self.get_next().cloned();
    
            if self.token().is_none() {
                self.raise("UEFError", "Unexpected end of input");
                return (vec![], vec![]);
            }
    
            if current_token.0 == "SEPARATOR" && current_token.1 == ")" {
                break;
            }
    
            if let Some(Token(t_type, t_val)) = &next_token {
                if t_type == "OPERATOR" && t_val == "=" {
                    let name = current_token.1;
                    self.next();
                    self.next();
                    let value = self.parse_expression();
                    if self.err.is_some() {
                        return (vec![], vec![]);
                    }
                    named_args.push((Value::String(name), value));
                    seen_named = true;
                } else {
                    if seen_named {
                        self.raise("ArgumentOrderError", "Positional arguments cannot follow named arguments");
                        return (vec![], vec![]);
                    }
                    let expr = self.parse_expression();
                    if self.err.is_some() {
                        pos_args.push(Value::Null);
                        return (vec![], vec![]);
                    }
                    pos_args.push(expr);
                }
            } else {
                if seen_named {
                    self.raise("ArgumentOrderError", "Positional arguments cannot follow named arguments");
                    return (vec![], vec![]);
                }
                let expr = self.parse_expression();
                if self.err.is_some() {
                    return (vec![], vec![]);
                }
                pos_args.push(expr);
            }
    
            if let Some(Token(t_type, t_val)) = self.token() {
                if t_type == "SEPARATOR" && t_val == "," {
                    self.next();
                }
            }
        }
    
        (pos_args, named_args)
    }
}
