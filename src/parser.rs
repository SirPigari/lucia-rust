use std::collections::HashMap;
use crate::env::core::config::{Config, CodeBlocks, ColorScheme};
use crate::env::core::utils::{print_colored, hex_to_ansi, to_static};
use crate::env::core::value::Value;
use crate::env::core::errors::Error;
use crate::env::core::statements::Statement;
use crate::env::core::types::{Float, Int};
use lazy_static::lazy_static;

lazy_static! {
    static ref DEFAULT_TOKEN: Token = Token("".to_string(), "".to_string());
}

fn get_type_default(type_: &str) -> Value {
    match type_ {
        "int" => Value::Float(0.0.into()),
        "float" => Value::Int(0.into()),
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
    statements: Vec<Statement>,
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

    pub fn raise(&mut self, error_type: &str, msg: &str) -> Statement {
        self.err = Some(Error {
            error_type: error_type.to_string(),
            msg: msg.to_string(),
            help: None,
            line: (self.current_line(), self.source.clone()),
            column: self.get_line_column(),
        });
        Statement::Null
    }

    pub fn raise_with_help(&mut self, error_type: &str, msg: &str, help: &str) -> Statement {
        self.err = Some(Error {
            error_type: error_type.to_string(),
            msg: msg.to_string(),
            help: Some(help.to_string()),
            line: (self.current_line(), self.source.clone()),
            column: self.get_line_column(),
        });
        Statement::Null
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
                    "Expected token '{}' but found: {}",
                    expected_value, token_value
                ));
            }
        } else {
            if expected_type == "SEPARATOR" && [")", "]", "}", "end"].contains(&expected_value) {
                let mut opening = "".to_string();
                match expected_value {
                    ")" => opening = "(".to_string(),
                    "]" => opening = "[".to_string(),
                    "}" => opening = "{".to_string(),
                    "end" => opening = ":".to_string(),
                    _ => {}
                }
                self.raise_with_help("SyntaxError", &format!(
                    "\"{}\" was never closed",
                    opening
                ),
                &format!("Maybe you forgot '{}'?", expected_value));
                return (vec![], vec![]);
            }
            self.raise("UEFError", &format!(
                "Expected token '{}' but found end of input",
                expected_value
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

    fn peek(&self) -> Option<&Token> {
        if self.pos + 1 < self.tokens.len() {
            Some(self.apply_aliases(&self.tokens[self.pos + 1]))
        } else {
            None
        }
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

    pub fn parse_safe(&mut self) -> Result<Vec<Statement>, Error> {
        let mut statements = Vec::new();
        while let Some(_) = self.token().cloned() {
            let stmt = self.parse_expression();
            if self.err.is_some() {
                return Err(self.err.clone().unwrap());
            }
            if let Statement::Null = stmt {
                continue;
            } else {
                statements.push(stmt);
            }
        }
        Ok(statements)
    }

    pub fn parse(&mut self) -> Vec<Statement> {
        while self.pos < self.tokens.len() {
            let expr = self.parse_expression();
            if expr != Statement::Null {
                self.statements.push(expr);
            }
            self.pos += 1;
        }
        self.statements.clone()
    }
    
    fn parse_expression(&mut self) -> Statement {
        let line = self.current_line();
        let column = self.get_line_column();

        let next_token = self.get_next().cloned();
        let next_token_type = next_token.as_ref().map(|t| t.0.clone()).unwrap_or_else(|| "".to_string());
        let next_token_value = next_token.as_ref().map(|t| t.1.clone()).unwrap_or_else(|| "".to_string());
        

        let mut out = match self.token().cloned() {
            Some(token) => match token.0.as_str() {
                "OPERATOR" if ["+", "-", "!"].contains(&token.1.as_str()) => {
                    let operator = token.1.clone();
                    self.next();
                    let operand = self.parse_expression();
                    Statement::Statement {
                        keys: vec![
                            Value::String("type".to_string()),
                            Value::String("operator".to_string()),
                            Value::String("operand".to_string()),
                        ],
                        values: vec![
                            Value::String("UNARY_OPERATION".to_string()),
                            Value::String(operator),
                            operand.convert_to_map(),
                        ],
                        line,
                        column,
                    }
                }

                "IDENTIFIER" if ["f", "r"].iter().any(|m| token.1.as_str().starts_with(m)) => {
                    self.parse_operand()
                }

                "SEPARATOR" if token.1 == "(" => {
                    self.next();
                    let mut expr = Statement::Null;
                    while let Some(next_token) = self.token() {
                        if next_token.0 == "SEPARATOR" && next_token.1 == ")" {
                            break;
                        }
                        expr = self.parse_expression();
                        break;
                    }
                    self.check_for("SEPARATOR", ")");
                    self.next();
                    expr
                }

                "SEPARATOR" if token.1 == "[" => {
                    self.parse_list()
                }

                "IDENTIFIER" => {
                    let next_token = self.get_next().cloned();
                    if let Some(next_tok) = next_token {
                        if next_tok.0 == "SEPARATOR" && next_tok.1 == "(" {
                            self.parse_function_call()
                        } else {
                            self.parse_operand()
                        }
                    } else {
                        self.parse_operand()
                    }
                }

                "NUMBER" | "STRING" | "BOOLEAN" => {
                    self.parse_operand()
                }

                _ => {
                    self.raise("SyntaxError", &format!("Invalid syntax. '{}' was unexpected.", token.1));
                    Statement::Null
                }
            },
            None => {
                self.raise("SyntaxError", "Expected expression");
                Statement::Null
            }
        };

        while let Some(tok) = self.token() {
            if tok.0 != "OPERATOR" {
                break;
            }

            let operator = tok.1.clone();
            self.next();
            let right = self.parse_expression();

            out = Statement::Statement {
                keys: vec![
                    Value::String("type".to_string()),
                    Value::String("left".to_string()),
                    Value::String("operator".to_string()),
                    Value::String("right".to_string()),
                ],
                values: vec![
                    Value::String("OPERATION".to_string()),
                    out.convert_to_map(),
                    Value::String(operator),
                    right.convert_to_map(),
                ],
                line,
                column,
            };
        }

        out
    }

    fn parse_primary(&mut self) -> Statement {
        let token = self.token().cloned().unwrap_or(DEFAULT_TOKEN.clone());
        let line = self.current_line();
        let column = self.get_line_column();

        match token.0.as_str() {
            "NUMBER" | "STRING" | "BOOLEAN" => {
                self.next();
                return self.parse_operand();
            }
            "IDENTIFIER" => {
                if let Some(peek) = self.peek() {
                    if peek.0 == "SEPARATOR" && peek.1 == "(" {
                        return self.parse_function_call();
                    }
                }
                self.next();
                return self.parse_operand();
            }
            "SEPARATOR" if token.1 == "(" => {
                self.next();
                let expr = self.parse_expression();
                self.check_for("SEPARATOR", ")");
                self.next();
                return expr;
            }
            "SEPARATOR" if token.1 == "[" => {
                return self.parse_list();
            }
            _ => {
                self.raise("SyntaxError", &format!("Unexpected token '{}'", token.1));
                return Statement::Null;
            }
        }
    }

    fn parse_unary(&mut self) -> Statement {
        let token = self.token().cloned().unwrap_or(DEFAULT_TOKEN.clone());
        let line = self.current_line();
        let column = self.get_line_column();

        if token.0 == "OPERATOR" && ["-", "+", "!"].contains(&token.1.as_str()) {
            let op = token.1.clone();
            self.next();
            let operand = self.parse_unary();
            return Statement::Statement {
                keys: vec![
                    Value::String("type".to_string()),
                    Value::String("operator".to_string()),
                    Value::String("operand".to_string()),
                ],
                values: vec![
                    Value::String("UNARY_OPERATION".to_string()),
                    Value::String(op),
                    operand.convert_to_map(),
                ],
                line,
                column,
            };
        }

        self.parse_primary()
    }

    fn parse_list(&mut self) -> Statement {
        let line = self.current_line();
        let column = self.get_line_column();
        self.next();

        let mut elements = Vec::new();
        while let Some(token) = self.token() {
            if token.0 == "SEPARATOR" && token.1 == "]" {
                break;
            }

            let expr = self.parse_expression();
            if self.err.is_some() {
                return Statement::Null;
            }
            elements.push(expr.convert_to_map());

            if let Some(next_token) = self.token() {
                if next_token.0 == "SEPARATOR" && next_token.1 == "," {
                    self.next();
                } else if next_token.0 == "SEPARATOR" && next_token.1 == "]" {
                    break;
                } else {
                    self.raise("SyntaxError", "Expected ',' or ']'");
                    return Statement::Null;
                }
            }
        }

        self.check_for("SEPARATOR", "]");
        self.next();

        Statement::Statement {
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
        }
    }
    
    fn parse_function_call(&mut self) -> Statement {
        let name = self.token().ok_or_else(|| Error::new("SyntaxError", "Expected function name")).unwrap().1.clone();
        if self.err.is_some() {
            return Statement::Null;
        }

        let line = self.current_line();
        let column = self.get_line_column();

        self.next();
        self.check_for("SEPARATOR", "(");
        self.next();
        let (pos_args, named_args) = self.parse_arguments();
        if self.err.is_some() {
            return Statement::Null;
        }
        self.check_for("SEPARATOR", ")");
        self.next();
        if self.err.is_some() {
            return Statement::Null;
        }

        if pos_args.is_empty() && named_args.is_empty() {
            let (keys, values): (Vec<_>, Vec<_>) = named_args.into_iter().unzip();
            return Statement::Statement {
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
                line,
                column,
            }
        }

        let (keys, values): (Vec<_>, Vec<_>) = named_args.into_iter().unzip();
        
        Statement::Statement {
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
                    named_args.push((Value::String(name), value.convert_to_map()));
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
                    pos_args.push(expr.convert_to_map());
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
                pos_args.push(expr.convert_to_map());
            }
    
            if let Some(Token(t_type, t_val)) = self.token() {
                if t_type == "SEPARATOR" && t_val == "," {
                    self.next();
                }
            }
        }
    
        (pos_args, named_args)
    }

    fn parse_operand(&mut self) -> Statement {
        let mut token = self.token().cloned().unwrap_or_else(|| Token("".to_string(), "".to_string()));
        let mut line = self.current_line();
        let mut column = self.get_line_column();
        let mut token_type = token.0.clone();
        let mut token_value = token.1.clone();        
        let mut next_token = self.get_next();
        let mut next_token_type = next_token.map(|t| t.0.clone()).unwrap_or_else(|| "".to_string());
        let mut next_token_value = next_token.map(|t| t.1.clone()).unwrap_or_else(|| "".to_string());
        
        if token_type == "SEPARATOR" && token_value == "(" {
            self.next();
            let expr = self.parse_expression();
            if self.err.is_some() {
                return Statement::Null;
            }
            self.check_for("SEPARATOR", ")");
            self.next();
            return expr;
        }

        if token_type == "NUMBER" {
            self.next();
            return Statement::Statement {
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
            return Statement::Statement {
                keys: vec![
                    Value::String("type".to_string()),
                    Value::String("value".to_string()),
                    Value::String("mod".to_string()),
                ],
                values: vec![
                    Value::String("STRING".to_string()),
                    Value::String(token_value.clone()),
                    Value::String("".to_string()),
                ],
                line,
                column,
            };
        }

        if token_type == "IDENTIFIER" && ["f", "r"].iter().any(|m| token.1.as_str().starts_with(m)) {
            let valid_mods = ["f", "r"];
            let mut mods: Vec<Value> = Vec::new();
        
            loop {
                if token_type != "IDENTIFIER" {
                    break;
                }
        
                let mod_name = token.1.clone();
        
                for ch in mod_name.chars() {
                    let ch_str = ch.to_string();
                    if !valid_mods.contains(&ch_str.as_str()) {
                        let expected = valid_mods.join(", ");
                        self.raise_with_help(
                            "StringModifierError",
                            to_static(format!("Invalid modifier: '{}'.", ch)),
                            to_static(format!("Expected one of: {}", expected)),
                        );
                        return Statement::Null;
                    }
        
                    mods.push(Value::String(ch_str));
                }
        
                self.next();
        
                match self.token().cloned() {
                    Some(next_tok) => {
                        token_type = next_tok.0.clone();
                        token = next_tok;
                    }
                    None => {
                        self.raise(
                            "UEFError",
                            "Unexpected end of input. Expected 'str' after modifiers.",
                        );
                        return Statement::Null;
                    }
                }
            }

            token_value = token.1.clone();
            self.next();
        
            return Statement::Statement {
                keys: vec![
                    Value::String("type".to_string()),
                    Value::String("value".to_string()),
                    Value::String("mods".to_string()),
                ],
                values: vec![
                    Value::String("STRING".to_string()),
                    Value::String(token_value.clone()),
                    Value::List(mods),
                ],
                line,
                column,
            };
        }
    
        if token_type == "BOOLEAN" {
            self.next();
            return Statement::Statement {
                keys: vec![
                    Value::String("type".to_string()),
                    Value::String("value".to_string()),
                ],
                values: vec![
                    Value::String("BOOLEAN".to_string()),
                    Value::String(token_value.clone()),
                ],
                line,
                column,
            };
        };        
        self.raise("SyntaxError", &format!(
            "Invalid syntax. '{}' was unexpected.",
            token_value
        ));
        Statement::Null
    }

    fn parse_operation(&mut self) -> Statement {
        let mut left = self.parse_operand();
        let line = self.current_line();
        let column = self.get_line_column();
        if self.err.is_some() {
            return Statement::Null;
        }

        while let Some(token) = self.token() {
            if token.0 == "OPERATOR" {
                let operator = token.1.clone();
                self.next();
                let right = self.parse_operand();

                if self.err.is_some() {
                    return Statement::Null;
                }

                left = Statement::Statement {
                    keys: vec![
                        Value::String("type".to_string()),
                        Value::String("left".to_string()),
                        Value::String("operator".to_string()),
                        Value::String("right".to_string()),
                    ],
                    values: vec![
                        Value::String("OPERATION".to_string()),
                        left.convert_to_map(),
                        Value::String(operator),
                        right.convert_to_map(),
                    ],
                    line,
                    column,
                };
            } else {
                break;
            }
        }

        left
    }
}
