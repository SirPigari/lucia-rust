use std::collections::HashMap;
use crate::env::helpers::config::{Config, CodeBlocks, ColorScheme};
use crate::env::helpers::utils::{print_colored, hex_to_ansi};
use lazy_static::lazy_static;

lazy_static! {
    static ref DEFAULT_TOKEN: Token = Token("".to_string(), "".to_string());
}

#[derive(Debug, Clone)]
pub enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Null,
    Map { keys: Vec<Value>, values: Vec<Value> },
    List(Vec<Value>),
    ListCompletion { pattern: Vec<Value>, end: Option<Box<Value>> },
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
        if self.pos + offset >= self.tokens.len() {
            return None;
        }
        while self.pos + offset < self.tokens.len() && self.tokens[self.pos + offset].0 == "WHITESPACE" && !self.include_whitespace {
            offset += 1
        }
        if self.pos + offset >= self.tokens.len() {
            return None;
        }
        Some(self.apply_aliases(&self.tokens[self.pos + offset]))
    }

    pub fn parse(&mut self) -> Vec<Value> {
        while self.pos < self.tokens.len() {
            if let Some(expr) = self.parse_expression() {
                self.statements.push(expr);
            }
        }
        self.statements.clone()
    }

    fn parse_expression(&mut self) -> Option<Value> {
        if self.token().is_none() {
            return None;
        }
        let token = self.token().unwrap();
        let token_type = &token.0;
        let token_value = &token.1;
        let next_token = self.get_next();
        let next_token_type = next_token.map(|t| &t.0).unwrap_or(&"".to_string());
        let next_token_value = next_token.map(|t| &t.1).unwrap_or(&"".to_string());

        match token_type {
            "IDENTIFIER" => {
                if next_token_type == "SEPARATOR" && next_token_value == "(" {
                    return self.parse_function_call(token_value);
                }
            }
            None => {
                return None;
            }
        }        
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
    fn parse_function_call(&mut self, name: &str) -> Option<Value> {
        self.next(); // Skip the '(' token
        self.check_token(); // Check for the next token
        let pos_arguments = self.parse_arguments()?;
        self.check_token(); // Check for the next token
        if self.token().unwrap().0 == "IDENTIFIER" && self.get_next() == Some(&Token("OPERATOR".to_string(), "=".to_string())) {
            return None; // Expected closing parenthesis
        }
        self.next(); // Skip the ')' token
        Some(Value::Map { keys: vec![Value::String(name.to_string())], values: pos_arguments })
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
    fn parse_arguments(&mut self) -> Option<(Vec<Value>, Vec<Value>)> {
        let mut pos_args = vec![];
        let mut named_args = vec![];
        while self.token().is_some() && (self.token().unwrap().0 != "SEPARATOR" || self.token().unwrap().1 != ")") {
            if self.token().unwrap().0 == "IDENTIFIER" && self.get_next() == Some(&Token("OPERATOR".to_string(), "=".to_string())) {
                let name = self.token().unwrap().1.clone();
                self.next(); // Skip the '=' token
                self.next(); // Move to the value token
                let value = self.parse_expression()?;
                named_args.push(Value::Map { keys: vec![Value::String(name)], values: vec![value] });
            } else {
                pos_args.push(self.parse_expression()?);
            }
            if self.token().is_some() && (self.token().unwrap().0 == "SEPARATOR" && self.token().unwrap().1 == ")") {
                break;
            }
            self.check_token(); // Check for the next token
            if self.token().is_some() && (self.token().unwrap().0 == "SEPARATOR" && self.token().unwrap().1 == ",") {
                self.next(); // Skip the ',' token
            }
        }
        Some((pos_args, named_args))
    }

}
