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
    config: Config, // Placeholder for config
    include_whitespace: bool,
}

impl Parser {
    pub fn new(tokens: Vec<Token>, config: Config) -> Self {
        Self {
            tokens,
            pos: 0,
            statements: vec![],
            aliases: HashMap::new(),
            config,
            include_whitespace: false,
        }
    }

    fn token(&mut self) -> Option<&Token> {
        if self.pos >= self.tokens.len() {
            return None
        }
        if self.include_whitespace {
            return Some(self.apply_aliases(&self.tokens[self.pos]));
        }
        while self.pos < self.tokens.len() && self.tokens[self.pos].0 == "WHITESPACE" {
            self.pos += 1;
        }
        if self.pos >= self.tokens.len() {
            return None;
        }
        Some(self.apply_aliases(&self.tokens[self.pos]))
    }

    fn apply_aliases<'a>(&'a self, token: &'a Token) -> &'a Token {
        self.aliases.get(token).and_then(|v| v.as_ref()).unwrap_or(token)
    }

    fn next(&mut self) -> Option<&Token> {
        self.pos += 1;
        self.token()
    }

    fn check_token(&mut self) {
        if self.token().is_none() {
            panic!("Unexpected end of input");
        }
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

    //def get_next(self):
    //    offset = 1
    //    if self.pos + offset >= len(self.tokens):
    //        return None
    //    while self.pos + offset < len(self.tokens) and self.tokens[self.pos + offset][0] == 'WHITESPACE' and (not self.include_whitespace):
    //        offset += 1
    //    if self.pos + offset >= len(self.tokens):
    //        return (None, None)
    //    return self.apply_aliases(self.tokens[self.pos + offset])

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

    fn current_line(&self) -> usize {
        self.tokens
            .get(self.pos)
            .and_then(|Token(_, val)| val.parse::<usize>().ok())
            .unwrap_or(1)
    }

    pub fn parse_safe(&mut self) -> Result<Vec<Value>, (String, usize)> {
        let mut statements = Vec::new();
        while self.token().is_some() {
            let line_number = self.current_line(); // You’ll need to track this
            match self.parse_expression() {
                Some(stmt) => statements.push(stmt),
                None => return Err(("Syntax error while parsing".into(), line_number)),
            }
        }
        Ok(statements)
    }    

    pub fn parse(&mut self) -> Vec<Value> {
        while self.pos < self.tokens.len() {
            if let Some(expr) = self.parse_expression() {
                self.statements.push(expr);
            }
            self.pos += 1;
        }
        self.statements.clone()
    }
    

    fn parse_expression(&mut self) -> Option<Value> {
        let token = self.token()?.clone();           // clone the (type, value) tuple
        let next_token = self.get_next();            // mutable borrow ends here
    
        let token_type = token.0;
        let token_value = token.1;
        let next_token_type = next_token.as_ref().map(|t| t.0.clone()).unwrap_or_else(|| "".to_string());
        let next_token_value = next_token.as_ref().map(|t| t.1.clone()).unwrap_or_else(|| "".to_string());        
    
        if token_type == "IDENTIFIER" && next_token_type == "SEPARATOR" && next_token_value == "(" {
            return self.parse_function_call();
        }

        if token_type == "NUMBER" {
            self.next(); // Move to the next token
            return Some(Value::Number(token_value.parse::<f64>().unwrap_or(0.0)));
        }
    
        self.pos += 1; // Move to the next token
        None
    }
    
    

    //def parse_function_call(self):
    //    name = self.token[1]
    //    self.next()
    //    self.check_for('SEPARATOR', '(')
    //    self.next()
    //    pos_arguments, named_arguments = self.parse_arguments()
    //    self.check_for('SEPARATOR', ')')
    //    self.next()
    //    return {
    //        "type": "CALL",
    //        "name": name,
    //        "pos_arguments": pos_arguments,
    //        "named_arguments": named_arguments
    //    }
    fn parse_function_call(&mut self) -> Option<Value> {
        let name = self.token()?.1.clone().to_string(); // Get the function name
        self.next(); // Skip the name
        self.check_for("SEPARATOR", "("); // Check for the next token
        self.next(); // Skip the '(' token
        let (pos_args, named_args) = self.parse_arguments().ok()?;
        self.check_for("SEPARATOR", ")"); // Check for the next token
        if self.token().unwrap().0 == "IDENTIFIER" && self.get_next() == Some(&Token("OPERATOR".to_string(), "=".to_string())) {
            return None; // Expected closing parenthesis
        }
        self.next(); // Skip the ')' token
        let (keys, values): (Vec<_>, Vec<_>) = named_args.into_iter().unzip();
        Some(Value::Map {
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

    // def parse_arguments(self):
    //     pos_args = []
    //     named_args = []
    //     while self.token != ('SEPARATOR', ')'):
    //         if self.token[0] == 'IDENTIFIER' and self.get_next() and self.get_next() == ('OPERATOR', '='):
    //             name = self.token[1]
    //             self.next()
    //             self.next()
    //             value = self.parse_expression()
    //             named_args.append({"type": "NAMEDARG", "name": name, "value": value})
    //         else:
    //             pos_args.append(self.parse_expression())
    //         if self.token == ('SEPARATOR', ')'):
    //             break
    //         self.check_for('SEPARATOR', ',')
    //         self.next()
    //     return pos_args, named_args
    fn parse_arguments(&mut self) -> Result<(Vec<Value>, Vec<(Value, Value)>), Error> {
        let mut pos_args = Vec::new();
        let mut named_args = Vec::new();
        let mut seen_named = false;
    
        while let Some(token) = self.token().cloned() {
            if let Some(Token(t_type, t_val)) = self.token() {
                if t_type == "SEPARATOR" && t_val == ")" {
                    break;
                }
            }
    
            if let Some(Token(t_type, t_val)) = self.get_next() {
                if t_type == "OPERATOR" && t_val == "=" {
                    let name = token.1;
                    self.next();
                    self.next();
                    let value = self.parse_expression().ok_or("Expected expression")?;
                    named_args.push((Value::String(name), value));
                    seen_named = true;
                } else {
                    if seen_named {
                        return Err(Error::new("ArgumentOrderError", "Positional arguments cannot follow named arguments"));
                    }
                    pos_args.push(self.parse_expression().ok_or("Expected expression")?);
                }
            } else {
                if seen_named {
                    return Err(Error::new("ArgumentOrderError", "Positional arguments cannot follow named arguments"));
                }
                pos_args.push(self.parse_expression().ok_or("Expected expression")?);
            }
    
            self.check_token();
            if let Some(Token(t_type, t_val)) = self.token() {
                if t_type == "SEPARATOR" && t_val == "," {
                    self.next();
                }
            }
        }
    
        Ok((pos_args, named_args))
    }    
}
