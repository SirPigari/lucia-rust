use std::collections::HashMap;
use crate::env::core::config::{Config, CodeBlocks, ColorScheme};
use crate::env::core::utils::{print_colored, hex_to_ansi, to_static, get_type_default, get_type_default_as_statement};
use crate::env::core::value::Value;
use crate::env::core::errors::Error;
use crate::env::core::statements::Statement;
use crate::env::core::types::{Float, Int, VALID_TYPES};
use once_cell::sync::Lazy;

static DEFAULT_TOKEN: Lazy<Token> = Lazy::new(|| Token("".to_string(), "".to_string()));

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

    fn token_is(&mut self, kind: &str, value: &str) -> bool {
        self.token().map_or(false, |t| t.0 == kind && t.1 == value)
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
            let value_matches = expected_value.is_empty() || token_value == expected_value;

            if !(token_type == "EOF" && expected_type != "EOF") {
                if token_type != expected_type || !value_matches {
                    self.raise("UEFError", &format!(
                        "Expected token '{}' but found: {}",
                        if expected_value.is_empty() { expected_type } else { expected_value },
                        token_value
                    ));
                }
                return (vec![], vec![]);
            }
        }
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
        } else if expected_type == "OPERATOR" && ["|"].contains(&expected_value) {
            let mut opening = "".to_string();
            match expected_value {
                "|" => opening = "|".to_string(),
                _ => {}
            }
            self.raise_with_help("SyntaxError", &format!(
                "\"{}\" is missing its closing pair",
                opening
            ),
            &format!("Maybe you forgot '{}'?", expected_value));
            return (vec![], vec![]);
        }
        self.raise("UEFError", &format!(
            "Expected token '{}' but found end of input",
            if expected_value.is_empty() { expected_type } else { expected_value }
        ));
        (vec![], vec![])
    }

    fn is_token(&mut self, kind: &str, value: &str) -> bool {
        if let Some(token) = self.token() {
            token.0 == kind && token.1 == value
        } else {
            false
        }
    }

    fn get_next(&mut self) -> Option<&Token> {
        self.next();
        self.token()
    }

    fn peek(&self) -> Option<&Token> {
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
        let empty = String::new();
        let target_token = self.tokens.get(self.pos).map(|t| &t.1).unwrap_or(&empty);
    
        let mut byte_index = 0;
        let mut count = 0;
    
        for token in &self.tokens {
            if &token.1 == target_token {
                if count == self.pos {
                    if let Some(found_index) = self.source[byte_index..].find(target_token) {
                        byte_index += found_index;
                        break;
                    }
                }
                count += 1;
            } else {
                byte_index += token.1.len();
            }
        }
    
        if byte_index > self.source.len() {
            return 0;
        }
    
        self.source[..byte_index].chars().filter(|&c| c == '\n').count() + 1
    }
    
    pub fn get_line_column(&self) -> usize {
        let empty = String::new();
        let target_token = self.tokens.get(self.pos).map(|t| &t.1).unwrap_or(&empty);
    
        let mut byte_index = 0;
        let mut count = 0;
        for token in &self.tokens {
            if &token.1 == target_token {
                if count == self.pos {
                    if let Some(found_index) = self.source[byte_index..].find(target_token) {
                        byte_index += found_index;
                        break;
                    }
                }
                count += 1;
            } else {
                byte_index += token.1.len();
            }
        }
        if byte_index > self.source.len() {
            return 0;
        }
        let mut line_start = 0;
        for (i, ch) in self.source[..byte_index].char_indices().rev() {
            if ch == '\n' {
                line_start = i + ch.len_utf8();
                break;
            }
        }
    
        self.source[line_start..byte_index].chars().count() + 1
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
        let mut expr = self.parse_primary();
        if self.err.is_some() {
            return Statement::Null;
        }
    
        while let Some(tok_ref) = self.token() {
            let tok = tok_ref.clone();
        
            match (tok.0.as_str(), tok.1.as_str()) {
                ("SEPARATOR", ".") => {
                    self.next();
                    let property_token = self.token().cloned().unwrap_or_else(|| {
                        self.raise("SyntaxError", "Expected identifier after '.'");
                        Token("".to_string(), "".to_string())
                    });
        
                    if property_token.0 != "IDENTIFIER" {
                        self.raise("SyntaxError", &format!(
                            "Expected identifier after '.', found '{}'", property_token.1
                        ));
                        return Statement::Null;
                    }
        
                    self.next();
        
                    if let Some(next_tok) = self.token() {
                        if next_tok.0 == "SEPARATOR" && next_tok.1 == "(" {
                            self.next();
                            let (pos_args, named_args) = self.parse_arguments();
                            self.check_for("SEPARATOR", ")");
                            self.next();
        
                            if self.err.is_some() {
                                return Statement::Null;
                            }
        
                            expr = Statement::Statement {
                                keys: vec![
                                    Value::String("type".to_string()),
                                    Value::String("object".to_string()),
                                    Value::String("method".to_string()),
                                    Value::String("pos_args".to_string()),
                                    Value::String("named_args".to_string()),
                                ],
                                values: vec![
                                    Value::String("METHOD_CALL".to_string()),
                                    expr.convert_to_map(),
                                    Value::String(property_token.1),
                                    Value::List(pos_args),
                                    Value::Map {
                                        keys: named_args.iter().map(|(k, _)| k.clone()).collect(),
                                        values: named_args.iter().map(|(_, v)| v.clone()).collect(),
                                    },
                                ],
                                line: self.current_line(),
                                column: self.get_line_column(),
                            };
                        } else {
                            expr = Statement::Statement {
                                keys: vec![
                                    Value::String("type".to_string()),
                                    Value::String("object".to_string()),
                                    Value::String("property".to_string()),
                                ],
                                values: vec![
                                    Value::String("PROPERTY_ACCESS".to_string()),
                                    expr.convert_to_map(),
                                    Value::String(property_token.1),
                                ],
                                line: self.current_line(),
                                column: self.get_line_column(),
                            };
                        }
                    }
                }
        
                ("SEPARATOR", "[") => {
                    self.check_for("SEPARATOR", "[");
                    self.next();
        
                    let mut start_expr = None;
                    let mut end_expr = None;
        
                    let next_tok = self.token().cloned();
        
                    if let Some(t) = next_tok {
                        if t.0 == "SEPARATOR" && t.1 == "]" {
                            self.raise("SyntaxError", "Empty index access '[]' is not allowed");
                            return Statement::Null;
                        }
        
                        if t.0 == "SEPARATOR" && t.1 == ".." {
                            self.next();
                            if let Some(t2) = self.token().cloned() {
                                if t2.0 != "SEPARATOR" || t2.1 != "]" {
                                    end_expr = Some(self.parse_expression());
                                }
                            }
                        } else {
                            start_expr = Some(self.parse_expression());
        
                            if self.err.is_some() {
                                return Statement::Null;
                            }
        
                            if let Some(t2) = self.token().cloned() {
                                if t2.0 == "SEPARATOR" && t2.1 == ".." {
                                    self.next();
                                    if let Some(t3) = self.token().cloned() {
                                        if t3.0 != "SEPARATOR" || t3.1 != "]" {
                                            end_expr = Some(self.parse_expression());
                                        }
                                    }
                                }
                            }
                        }
                    }
        
                    self.check_for("SEPARATOR", "]");
                    self.next();
        
                    expr = Statement::Statement {
                        keys: vec![
                            Value::String("type".to_string()),
                            Value::String("object".to_string()),
                            Value::String("access".to_string()),
                        ],
                        values: vec![
                            Value::String("INDEX_ACCESS".to_string()),
                            expr.convert_to_map(),
                            Value::Map {
                                keys: vec![
                                    Value::String("start".to_string()),
                                    Value::String("end".to_string()),
                                ],
                                values: vec![
                                    start_expr
                                        .map(|e| e.convert_to_map())
                                        .unwrap_or(Value::Null),
                                    end_expr
                                        .map(|e| e.convert_to_map())
                                        .unwrap_or(Value::Null),
                                ],
                            },
                        ],
                        line: self.current_line(),
                        column: self.get_line_column(),
                    };
                }

                ("OPERATOR", op) if op == "=" => {
                    self.next();
                    let value = self.parse_expression();
                    if self.err.is_some() {
                        return Statement::Null;
                    }
        
                    expr = Statement::Statement {
                        keys: vec![
                            Value::String("type".to_string()),
                            Value::String("left".to_string()),
                            Value::String("right".to_string()),
                        ],
                        values: vec![
                            Value::String("ASSIGNMENT".to_string()),
                            expr.convert_to_map(),
                            value.convert_to_map(),
                        ],
                        line: self.current_line(),
                        column: self.get_line_column(),
                    };
                }
        
                ("OPERATOR", op) if op != "|" => {
                    let operator = tok.1.clone();
                    self.next();
                    let right = self.parse_primary();
                    if self.err.is_some() {
                        return Statement::Null;
                    }
        
                    expr = Statement::Statement {
                        keys: vec![
                            Value::String("type".to_string()),
                            Value::String("left".to_string()),
                            Value::String("operator".to_string()),
                            Value::String("right".to_string()),
                        ],
                        values: vec![
                            Value::String("OPERATION".to_string()),
                            expr.convert_to_map(),
                            Value::String(operator),
                            right.convert_to_map(),
                        ],
                        line: self.current_line(),
                        column: self.get_line_column(),
                    };
                }
        
                _ => break,
            }
        }
        
    
        expr
    }

    fn parse_primary(&mut self) -> Statement {
        let mut line = self.current_line();
        let mut column = self.get_line_column();

        match self.token().cloned() {
            Some(token) => match token.0.as_str() {
                "OPERATOR" if token.1 == "|" => {
                    self.next();
                    let expr = self.parse_expression();
                    if self.err.is_some() { return Statement::Null; }
                    self.check_for("OPERATOR", "|");
                    self.next();
                    Statement::Statement {
                        keys: vec![
                            Value::String("type".to_string()),
                            Value::String("operator".to_string()),
                            Value::String("left".to_string()),
                            Value::String("right".to_string()),
                        ],
                        values: vec![
                            Value::String("OPERATION".to_string()),
                            Value::String("abs".to_string()),
                            Value::Null,
                            expr.convert_to_map(),
                        ],
                        line: self.current_line(),
                        column: self.get_line_column(),
                    }
                }
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

                "SEPARATOR" | "IDENTIFIER" if token.1 == "..." || token.1 == "pass" => {
                    self.next();
                    Statement::Null
                }

                "IDENTIFIER" if token.1 == "for" => {
                    self.next();
                    self.check_for("SEPARATOR", "(");
                    self.next();
                    let variable = self.token().cloned().unwrap_or_else(|| {
                        self.raise("SyntaxError", "Expected identifier after 'for'");
                        DEFAULT_TOKEN.clone()
                    });
                    self.next();
                    self.check_for("OPERATOR", "in");
                    self.next();
                    let iterable = self.parse_expression();
                    if self.err.is_some() {
                        return Statement::Null;
                    }
                    self.check_for("SEPARATOR", ")");
                    self.next();
                    line = self.current_line();
                    column = self.get_line_column();
                    self.check_for("SEPARATOR", ":");
                    self.next();
                    let mut body = vec![];
                    while let Some(tok) = self.token() {
                        if tok.0 == "IDENTIFIER" && tok.1 == "end" {
                            break;
                        }
                        let stmt = self.parse_expression();
                        if self.err.is_some() {
                            return Statement::Null;
                        }
                        body.push(stmt);
                    }
                    self.check_for("IDENTIFIER", "end");
                    self.next();
                    Statement::Statement {
                        keys: vec![
                            Value::String("type".to_string()),
                            Value::String("variable".to_string()),
                            Value::String("iterable".to_string()),
                            Value::String("body".to_string()),
                        ],
                        values: vec![
                            Value::String("FOR".to_string()),
                            Value::String(variable.1),
                            iterable.convert_to_map(),
                            Value::List(body.into_iter().map(|s| s.convert_to_map()).collect()),
                        ],
                        line,
                        column,
                    }
                }

                "IDENTIFIER" if token.1 == "if" => {
                    self.next();
                    self.check_for("SEPARATOR", "(");
                    self.next();
                    let condition = self.parse_expression();
                    if self.err.is_some() {
                        return Statement::Null;
                    }
                    self.check_for("SEPARATOR", ")");
                    self.next();
                    line = self.current_line();
                    column = self.get_line_column();
                    self.check_for("SEPARATOR", ":");
                    self.next();
                    let mut body = vec![];
                    while let Some(tok) = self.token() {
                        if tok.0 == "IDENTIFIER" && tok.1 == "end" {
                            break;
                        }
                        let stmt = self.parse_expression();
                        if self.err.is_some() {
                            return Statement::Null;
                        }
                        body.push(stmt);
                    }
                    self.check_for("IDENTIFIER", "end");
                    self.next();
                    Statement::Statement {
                        keys: vec![
                            Value::String("type".to_string()),
                            Value::String("condition".to_string()),
                            Value::String("body".to_string()),
                        ],
                        values: vec![
                            Value::String("IF".to_string()),
                            condition.convert_to_map(),
                            Value::List(body.into_iter().map(|s| s.convert_to_map()).collect()),
                        ],
                        line,
                        column,
                    }
                }

                "IDENTIFIER" if token.1 == "try" => {
                    self.next();
                    let line = self.current_line();
                    let column = self.get_line_column();

                    self.check_for("SEPARATOR", ":");
                    self.next();

                    let mut body = vec![];
                    while let Some(tok) = self.token() {
                        if tok.0 == "IDENTIFIER" && tok.1 == "end" {
                            break;
                        }
                        let stmt = self.parse_expression();
                        if self.err.is_some() {
                            return Statement::Null;
                        }
                        body.push(stmt);
                    }

                    self.check_for("IDENTIFIER", "end");
                    self.next();

                    let mut exception_vars = vec![];
                    let mut catch_body = vec![];
                    let mut has_catch = false;

                    if let Some(tok) = self.token() {
                        if tok.0 == "IDENTIFIER" && tok.1 == "catch" {
                            has_catch = true;
                            self.next();

                            self.check_for("SEPARATOR", "(");
                            self.next();

                            loop {
                                if let Some(tok) = self.token() {
                                    if tok.0 == "IDENTIFIER" {
                                        exception_vars.push(tok.1.clone());
                                        self.next();

                                        if let Some(tok) = self.token() {
                                            if tok.0 == "SEPARATOR" && tok.1 == "," {
                                                self.next();
                                                continue;
                                            } else if tok.0 == "SEPARATOR" && tok.1 == ")" {
                                                break;
                                            } else {
                                                self.raise_with_help("SyntaxError", "Expected ',' or ')' in exception var list", "Separate variables with commas, and close with ')'");
                                                return Statement::Null;
                                            }
                                        } else {
                                            self.raise("SyntaxError", "Unexpected end of input in exception var list");
                                            return Statement::Null;
                                        }
                                    } else if tok.0 == "SEPARATOR" && tok.1 == ")" {
                                        break;
                                    } else {
                                        self.raise("SyntaxError", "Invalid token in exception var list");
                                        return Statement::Null;
                                    }
                                } else {
                                    self.raise("SyntaxError", "Unterminated exception var list");
                                    return Statement::Null;
                                }
                            }

                            self.next(); // consume ')'

                            if !self.is_token("SEPARATOR", ":") {
                                self.raise_with_help("SyntaxError", "Expected ':' after exception vars", "Did you forget to add ':'?");
                                return Statement::Null;
                            }
                            self.next();

                            while let Some(tok) = self.token() {
                                if tok.0 == "IDENTIFIER" && tok.1 == "end" {
                                    break;
                                }
                                let stmt = self.parse_expression();
                                if self.err.is_some() {
                                    return Statement::Null;
                                }
                                catch_body.push(stmt);
                            }

                            self.check_for("IDENTIFIER", "end");
                            self.next();
                        }
                    }

                    Statement::Statement {
                        keys: vec![
                            Value::String("type".to_string()),
                            Value::String("body".to_string()),
                            Value::String("exception_vars".to_string()),
                            Value::String("catch_body".to_string()),
                        ],
                        values: vec![
                            Value::String(if has_catch { "TRY_CATCH" } else { "TRY" }.to_string()),
                            Value::List(body.into_iter().map(|s| s.convert_to_map()).collect()),
                            Value::List(exception_vars.into_iter().map(Value::String).collect()),
                            Value::List(catch_body.into_iter().map(|s| s.convert_to_map()).collect()),
                        ],
                        line,
                        column,
                    }
                }

                "SEPARATOR" if token.1 == "(" => {
                    self.next();
                    let mut values = vec![];

                    if let Some(next_token) = self.token() {
                        if next_token.0 == "SEPARATOR" && next_token.1 == ")" {
                            self.next();
                            return Statement::Statement {
                                keys: vec![
                                    Value::String("type".to_string()),
                                    Value::String("values".to_string()),
                                ],
                                values: vec![
                                    Value::String("TUPLE".to_string()),
                                    Value::List(vec![]),
                                ],
                                line,
                                column,
                            };
                        }
                    }

                    let first_expr = self.parse_expression();
                    values.push(first_expr);

                    let mut is_tuple = false;
                    while let Some(next_token) = self.token() {
                        if next_token.0 == "SEPARATOR" && next_token.1 == "," {
                            is_tuple = true;
                            self.next();
                            let expr_item = self.parse_expression();
                            values.push(expr_item);
                        } else {
                            break;  // break here when next token isn’t a comma
                        }
                    }

                    self.check_for("SEPARATOR", ")");
                    self.next();

                    if is_tuple || values.len() != 1 {
                        Statement::Statement {
                            keys: vec![
                                Value::String("type".to_string()),
                                Value::String("values".to_string()),
                            ],
                            values: vec![
                                Value::String("TUPLE".to_string()),
                                Value::List(values.into_iter().map(|s| s.convert_to_map()).collect()),
                            ],
                            line,
                            column,
                        }
                    } else {
                        values.remove(0)
                    }
                }

                "SEPARATOR" if token.1 == "[" => {
                    self.parse_list()
                }

                "IDENTIFIER" => {
                    let next_token = self.peek().cloned();
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

                "NUMBER" => {
                    let next_token = self.peek().cloned();
                    if let Some(next_tok) = next_token {
                        if next_tok.0 == "SEPARATOR" && next_tok.1 == "(" {
                            let left = self.parse_operand();
                            let right = self.parse_expression();
                            if self.err.is_some() {
                                return Statement::Null;
                            }
                            Statement::Statement {
                                keys: vec![
                                    Value::String("type".to_string()),
                                    Value::String("operator".to_string()),
                                    Value::String("left".to_string()),
                                    Value::String("right".to_string()),
                                ],
                                values: vec![
                                    Value::String("OPERATION".to_string()),
                                    Value::String("*".to_string()),
                                    left.convert_to_map(),
                                    right.convert_to_map(),
                                ],
                                line,
                                column,
                            }
                        } else {
                            self.parse_operand()
                        }
                    } else {
                        self.parse_operand()
                    }
                }

                "STRING" | "BOOLEAN" | "RAW_STRING" => {
                    self.parse_operand()
                }

                "EOF" => {
                    self.next();
                    if self.pos == self.tokens.len() {
                        return Statement::Null;
                    }
                    self.raise("SyntaxError", "Unexpected end of file");
                    Statement::Null
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
        }
    }

    fn parse_variable(&mut self) -> Statement {
        let token = self.token().cloned().unwrap_or(DEFAULT_TOKEN.clone());
        let line = self.current_line();
        let column = self.get_line_column();

        if token.0 == "IDENTIFIER" {
            let name = token.1.clone();
            self.next();
            if self.token_is("SEPARATOR", ":") {
                self.next();
                let type_token = self.token().cloned().unwrap_or(DEFAULT_TOKEN.clone());
                if type_token.0 != "IDENTIFIER" {
                    self.raise("SyntaxError", "Expected type after ':'");
                    return Statement::Null;
                }
                let type_ = type_token.1.clone();
                self.next();
                if !VALID_TYPES.contains(&type_.as_str()) {
                    self.raise("TypeError", &format!("Invalid type '{}'", type_));
                    return Statement::Null;
                }
                let mut value = get_type_default_as_statement(&type_).convert_to_map();
                if self.err.is_some() {
                    return Statement::Null;
                }
                if self.token_is("OPERATOR", "=") {
                    self.next();
                    value = self.parse_expression().convert_to_map();
                    if self.err.is_some() {
                        return Statement::Null;
                    }
                }
                return Statement::Statement {
                    keys: vec![
                        Value::String("type".to_string()),
                        Value::String("name".to_string()),
                        Value::String("var_type".to_string()),
                        Value::String("value".to_string()),
                    ],
                    values: vec![
                        Value::String("VARIABLE_DECLARATION".to_string()),
                        Value::String(name),
                        Value::String(type_.clone()),
                        value,
                    ],
                    line,
                    column,
                };
            } else {
                return Statement::Statement {
                    keys: vec![
                        Value::String("type".to_string()),
                        Value::String("name".to_string()),
                    ],
                    values: vec![
                        Value::String("VARIABLE".to_string()),
                        Value::String(name),
                    ],
                    line,
                    column,
                };
            }
        }

        self.raise("SyntaxError", &format!("Invalid syntax. '{}' was unexpected.", token.1));
        Statement::Null
    }

    fn parse_unary(&mut self) -> Statement {
        let token = self.token().cloned().unwrap_or(DEFAULT_TOKEN.clone());
        let line = self.current_line();
        let column = self.get_line_column();

        if token.0 == "OPERATOR" && ["-", "+", "!", "not"].contains(&token.1.as_str()) {
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

    fn parse_operand(&mut self) -> Statement {
        let mut token = self.token().cloned().unwrap_or_else(|| Token("".to_string(), "".to_string()));
        let mut line = self.current_line();
        let mut column = self.get_line_column();
        let mut token_type = token.0.clone();
        let mut token_value = token.1.clone();        
        let mut next_token = self.peek();
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
            let valid_mods = ['f', 'b'];
            let token_value = token.1.clone();
        
            self.next();
        
            if let Some(first_quote_idx) = token_value.find(|c| c == '\'' || c == '"') {
                let (prefix_str, literal_str) = token_value.split_at(first_quote_idx);
        
                let mods: Vec<Value> = prefix_str
                    .chars()
                    .filter(|ch| valid_mods.contains(ch))
                    .map(|ch| Value::String(ch.to_string()))
                    .collect();
        
                return Statement::Statement {
                    keys: vec![
                        Value::String("type".to_string()),
                        Value::String("value".to_string()),
                        Value::String("mods".to_string()),
                    ],
                    values: vec![
                        Value::String("STRING".to_string()),
                        Value::String(literal_str.to_string()),
                        Value::List(mods),
                    ],
                    line,
                    column,
                };
            } else {
                return Statement::Statement {
                    keys: vec![
                        Value::String("type".to_string()),
                        Value::String("value".to_string()),
                        Value::String("mods".to_string()),
                    ],
                    values: vec![
                        Value::String("STRING".to_string()),
                        Value::String(token_value.clone()),
                        Value::List(Vec::new()),
                    ],
                    line,
                    column,
                };
            }
        }
        
        if token_type == "RAW_STRING" {
            let valid_mods = ['f', 'r', 'b'];
            let token_value = token.1.clone();
        
            self.next();
        
            if let Some(first_quote_idx) = token_value.find(|c| c == '\'' || c == '"') {
                let (prefix_str, literal_str) = token_value.split_at(first_quote_idx);
        
                let mods: Vec<Value> = prefix_str
                    .chars()
                    .filter(|ch| valid_mods.contains(ch))
                    .map(|ch| Value::String(ch.to_string()))
                    .collect();
        
                return Statement::Statement {
                    keys: vec![
                        Value::String("type".to_string()),
                        Value::String("value".to_string()),
                        Value::String("mods".to_string()),
                    ],
                    values: vec![
                        Value::String("STRING".to_string()),
                        Value::String(literal_str.to_string()),
                        Value::List(mods),
                    ],
                    line,
                    column,
                };
            } else {
                return Statement::Statement {
                    keys: vec![
                        Value::String("type".to_string()),
                        Value::String("value".to_string()),
                        Value::String("mods".to_string()),
                    ],
                    values: vec![
                        Value::String("STRING".to_string()),
                        Value::String(token_value.clone()),
                        Value::List(Vec::new()),
                    ],
                    line,
                    column,
                };
            }
        }

        if token_type == "IDENTIFIER" {
            if next_token_type == "SEPARATOR" && next_token_value == "(" {
                return self.parse_function_call();
            }
            return self.parse_variable();
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

    fn parse_list(&mut self) -> Statement {
        let line = self.current_line();
        let column = self.get_line_column();
    
        self.next();
    
        let mut elements = Vec::new();
        let mut found_range = false;
        let mut pattern_reg = false;
    
        while let Some(token) = self.token() {
            if token.0 == "SEPARATOR" && token.1 == "]" {
                break;
            }
    
            if token.0 == "SEPARATOR" && (token.1 == ".." || token.1 == "...") {
                found_range = true;
                pattern_reg = token.1 == "...";
                self.next();
    
                let end_expr = self.parse_expression();
                if self.err.is_some() {
                    return Statement::Null;
                }
    
                self.check_for("SEPARATOR", "]");
                self.next();
    
                return Statement::Statement {
                    keys: vec![
                        Value::String("type".to_string()),
                        Value::String("iterable_type".to_string()),
                        Value::String("seed".to_string()),
                        Value::String("end".to_string()),
                        Value::String("pattern_reg".to_string()),
                    ],
                    values: vec![
                        Value::String("ITERABLE".to_string()),
                        Value::String("LIST_COMPLETION".to_string()),
                        Value::List(elements),
                        end_expr.convert_to_map(),
                        Value::Boolean(pattern_reg),
                    ],
                    line,
                    column,
                };
            }
    
            let expr = self.parse_expression();
            if self.err.is_some() {
                return Statement::Null;
            }
    
            elements.push(expr.convert_to_map());
    
            if let Some(next_token) = self.token() {
                let token_str = next_token.1.clone();
            
                if next_token.0 == "SEPARATOR" && next_token.1 == "," {
                    self.next();
                } else if next_token.0 == "SEPARATOR" && next_token.1 == "]" {
                    break;
                } else if next_token.0 == "SEPARATOR" && (next_token.1 == ".." || next_token.1 == "...") {
                    continue;
                } else {
                    self.raise("UEFError", &format!("Expected token ',' but found: {}", token_str));
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
            let next_token = self.peek().cloned();
    
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
}
