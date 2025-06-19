use std::collections::HashMap;
use crate::env::runtime::config::{Config, CodeBlocks, ColorScheme};
use crate::env::runtime::utils::{unescape_string_literal, print_colored, hex_to_ansi, to_static, get_type_default, get_type_default_as_statement, get_type_default_as_statement_from_statement, check_ansi};
use crate::env::runtime::value::Value;
use crate::env::runtime::errors::Error;
use crate::env::runtime::statements::Statement;
use crate::env::runtime::types::{Float, Int, VALID_TYPES};
use once_cell::sync::Lazy;

static DEFAULT_TOKEN: Lazy<Token> = Lazy::new(|| Token("".to_string(), "".to_string()));

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct Token(pub String, pub String);

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    statements: Vec<Statement>,
    config: Config,
    use_colors: bool,
    include_whitespace: bool,
    source: String,
    err: Option<Error>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>, config: Config, source: String, use_colors: bool) -> Self {
        Self {
            tokens,
            pos: 0,
            statements: vec![],
            config: config.clone(),
            use_colors,
            include_whitespace: false,
            source,
            err: None,
        }
    }

    fn token(&mut self) -> Option<&Token> {
        while self.pos < self.tokens.len() {
            let token = &self.tokens[self.pos];
            if self.include_whitespace || token.0 != "WHITESPACE" {
                return Some(token);
            }
            self.pos += 1;
        }
        None
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

    fn get_next(&mut self) -> Option<&Token> {
        self.next();
        self.token()
    }

    fn peek(&self) -> Option<&Token> {
        let mut offset = 1;
        while self.pos + offset < self.tokens.len() {
            if self.tokens[self.pos + offset].0 != "WHITESPACE" || self.include_whitespace {
                return Some(&self.tokens[self.pos + offset]);
            }
            offset += 1;
        }
        None
    }

    pub fn current_line(&self) -> usize {
        let mut byte_index = 0;
    
        for (i, token) in self.tokens.iter().enumerate() {
            if i == self.pos {
                break;
            }
    
            byte_index += token.1.len();
        }
    
        if byte_index > self.source.len() {
            return 0;
        }
    
        self.source[..byte_index].chars().filter(|&c| c == '\n').count() + 1
    }
    
    pub fn get_line_column(&self) -> usize {
        let mut byte_index = 0;
    
        for (i, token) in self.tokens.iter().enumerate() {
            if i == self.pos {
                break;
            }
    
            byte_index += token.1.len();
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

        if expr.get_type() == "TYPE" {
            return expr;
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
                "IDENTIFIER" if VALID_TYPES.contains(&token.1.as_str()) => {
                    self.parse_type()
                }

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

                "IDENTIFIER" if token.1 == "import" => {
                    self.next();
                    let mut module_parts = vec![];

                    let first = self.token().cloned().unwrap_or_else(|| {
                        self.raise("SyntaxError", "Expected module name after 'import'");
                        DEFAULT_TOKEN.clone()
                    });
                    
                    let module_name = if first.0 == "IDENTIFIER" {
                        module_parts.push(first.1);
                        self.next();
                    
                        while self.token_is("SEPARATOR", ".") {
                            self.next();
                            let next_ident = self.token().cloned().unwrap_or_else(|| {
                                self.raise("SyntaxError", "Expected identifier after '.' in module name");
                                DEFAULT_TOKEN.clone()
                            });
                            if next_ident.0 != "IDENTIFIER" {
                                self.raise("SyntaxError", "Expected identifier after '.' in module name");
                                return Statement::Null;
                            }
                            module_parts.push(next_ident.1);
                            self.next();
                        }
                    
                        module_parts.join(".")
                    } else if first.0 == "STRING" {
                        let raw = &first.1;
                        let unescaped = unescape_string_literal(raw);
                        match unescaped {
                            Ok(inner) => {
                                self.next();
                                inner
                            }
                            Err(_) => {
                                self.raise("SyntaxError", "Malformed string literal for module name");
                                return Statement::Null;
                            }
                        }
                    } else {
                        self.raise("SyntaxError", "Expected identifier or string after 'import'");
                        return Statement::Null;
                    };

                    let mut path = None;
                    let mut alias: Option<Token> = None;
                    if self.token_is("IDENTIFIER", "from") {
                        self.next();
                        path = Some(self.parse_expression());
                        if self.err.is_some() {
                            return Statement::Null;
                        }
                    }
                    if self.token_is("IDENTIFIER", "as") {
                        self.next();
                        let next_token = self.token().cloned().unwrap_or_else(|| {
                            self.raise("SyntaxError", "Expected alias after 'as'");
                            DEFAULT_TOKEN.clone()
                        });
                        if next_token.0 != "IDENTIFIER" {
                            self.raise("SyntaxError", "Expected identifier after 'as'");
                            return Statement::Null;
                        }
                        alias = Some(next_token);
                        self.next();
                    }
                    line = self.current_line();
                    column = self.get_line_column();
                    Statement::Statement {
                        keys: vec![
                            Value::String("type".to_string()),
                            Value::String("module_name".to_string()),
                            Value::String("path".to_string()),
                            Value::String("alias".to_string()),
                        ],
                        values: vec![
                            Value::String("IMPORT".to_string()),
                            Value::String(module_name),
                            path.map_or(Value::Null, |p| p.convert_to_map()),
                            alias.map_or(Value::Null, |a| Value::String(a.1)),
                        ],
                        line,
                        column,
                    }
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

                "IDENTIFIER" if token.1 == "forget" => {
                    self.next();
                    let value = self.parse_expression();
                    if self.err.is_some() {
                        return Statement::Null;
                    }
                    Statement::Statement {
                        keys: vec![
                            Value::String("type".to_string()),
                            Value::String("value".to_string()),
                        ],
                        values: vec![
                            Value::String("FORGET".to_string()),
                            value.convert_to_map(),
                        ],
                        line,
                        column,
                    }
                }

                "IDENTIFIER" if token.1 == "throw" => {
                    self.next();
                    let message = self.parse_expression();
                    if self.err.is_some() {
                        return Statement::Null;
                    }
                    let mut from = Statement::Statement {
                        keys: vec![
                            Value::String("type".to_string()),
                            Value::String("value".to_string()),
                            Value::String("mods".to_string()),
                        ],
                        values: vec![
                            Value::String("STRING".to_string()),
                            Value::String("\"LuciaError\"".to_string()),
                            Value::List(vec![]),
                        ],
                        line,
                        column,
                    };
                    if self.token_is("IDENTIFIER", "from") {
                        self.next();
                        from = self.parse_expression();
                        if self.err.is_some() {
                            return Statement::Null;
                        }
                    }
                    line = self.current_line();
                    column = self.get_line_column();
                    Statement::Statement {
                        keys: vec![
                            Value::String("type".to_string()),
                            Value::String("message".to_string()),
                            Value::String("from".to_string()),
                        ],
                        values: vec![
                            Value::String("THROW".to_string()),
                            message.convert_to_map(),
                            from.convert_to_map(),
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
                    let line = self.current_line();
                    let column = self.get_line_column();
                    self.check_for("SEPARATOR", ":");
                    self.next();

                    let mut body = vec![];
                    while let Some(tok) = self.token() {
                        if tok.0 == "IDENTIFIER" && (tok.1 == "end" || tok.1 == "else") {
                            break;
                        }
                        let stmt = self.parse_expression();
                        if self.err.is_some() {
                            return Statement::Null;
                        }
                        body.push(stmt);
                    }

                    let mut else_body = None;

                    if self.token_is("IDENTIFIER", "end") {
                        self.next();
                        else_body = Some(vec![]);
                    } else if let Some(tok) = self.token() {
                        if tok.0 == "IDENTIFIER" && tok.1 == "else" {
                            self.next();
                    
                            if let Some(next_tok) = self.token() {
                                if next_tok.0 == "IDENTIFIER" && next_tok.1 == "if" {
                                    else_body = Some(vec![self.parse_expression()]);
                                } else {
                                    self.check_for("SEPARATOR", ":");
                                    self.next();
                    
                                    let mut else_stmts = vec![];
                                    while let Some(tok) = self.token() {
                                        if tok.0 == "IDENTIFIER" && tok.1 == "end" {
                                            break;
                                        }
                                        let stmt = self.parse_expression();
                                        if self.err.is_some() {
                                            return Statement::Null;
                                        }
                                        else_stmts.push(stmt);
                                    }
                                    else_body = Some(else_stmts);
                    
                                    // consume the `end` token after else block
                                    if self.token_is("IDENTIFIER", "end") {
                                        self.next();
                                    } else {
                                        self.raise("SyntaxError", "Expected 'end' after else block");
                                        return Statement::Null;
                                    }
                                }
                            } else {
                                self.raise("SyntaxError", "Unexpected end after 'else'");
                                return Statement::Null;
                            }
                        } else {
                            self.raise("SyntaxError", "Expected 'end' or 'else' after 'if' body");
                            return Statement::Null;
                        }
                    } else {
                        self.raise("SyntaxError", "Expected 'end' or 'else' after 'if' body");
                        return Statement::Null;
                    }

                    Statement::Statement {
                        keys: vec![
                            Value::String("type".to_string()),
                            Value::String("condition".to_string()),
                            Value::String("body".to_string()),
                            Value::String("else_body".to_string()),
                        ],
                        values: vec![
                            Value::String("IF".to_string()),
                            condition.convert_to_map(),
                            Value::List(body.into_iter().map(|s| s.convert_to_map()).collect()),
                            Value::List(match else_body {
                                Some(stmts) => stmts.into_iter().map(|s| s.convert_to_map()).collect(),
                                None => vec![],
                            })                            
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
                        if tok.0 == "IDENTIFIER" && (tok.1 == "end" || tok.1 == "catch") {
                            break;
                        }
                        let stmt = self.parse_expression();
                        if self.err.is_some() {
                            return Statement::Null;
                        }
                        body.push(stmt);
                    }

                    if !self.token_is("IDENTIFIER", "catch") {
                        self.check_for("IDENTIFIER", "end");
                        self.next();
                    }
                    
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

                            if !self.token_is("SEPARATOR", ":") {
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

                "IDENTIFIER" if token.1 == "fun" || ["public", "private", "static", "non-static", "final", "mutable"].contains(&token.1.as_str()) => {
                    let mut modifiers = vec![];
                    let mut name = "".to_string();
                    let mut is_function = false;
                
                    if ["public", "private", "static", "non-static", "final", "mutable"].contains(&token.1.as_str()) {
                        while let Some(tok) = self.token() {
                            if tok.0 == "IDENTIFIER" && ["public", "private", "static", "non-static", "final", "mutable"].contains(&tok.1.as_str()) {
                                modifiers.push(tok.1.clone());
                                self.next();
                            } else {
                                break;
                            }
                        }
                    }                    
                
                    if self.token_is("IDENTIFIER", "fun") {
                        is_function = true;
                        self.next();
                    } else {
                        is_function = false;
                    }
                    name = self.token().cloned().unwrap().1;
                    self.next();
                    
                    if is_function && name.is_empty() {
                        self.raise("SyntaxError", "Function declaration must have a name");
                        return Statement::Null;
                    }

                    if is_function {
                        self.check_for("SEPARATOR", "(");
                        self.next();
                        let mut pos_args = vec![];
                        let mut named_args = vec![];

                        while let Some(tok) = self.token() {
                            if tok.0 == "SEPARATOR" && tok.1 == ")" {
                                break;
                            } else if tok.0 == "IDENTIFIER" {
                                let arg_name = tok.1.clone();
                                self.next();
                        
                                let mut arg_type = Value::Map {
                                    keys: vec![
                                        Value::String("type".to_string()),
                                        Value::String("value".to_string()),
                                        Value::String("type_kind".to_string()),
                                    ],
                                    values: vec![
                                        Value::String("TYPE".to_string()),
                                        Value::String("any".to_string()),
                                        Value::String("simple".to_string()),
                                    ],
                                };
                                let mut default_value: Value;
                        
                                if self.token_is("SEPARATOR", ":") {
                                    self.next();
                                    let type_expr = self.parse_expression();
                                    if self.err.is_some() {
                                        return Statement::Null;
                                    }
                                    arg_type = type_expr.convert_to_map();
                                }
                        
                                if self.token_is("OPERATOR", "=") {
                                    self.next();
                                    let def_val = self.parse_expression();
                                    if self.err.is_some() {
                                        return Statement::Null;
                                    }
                        
                                    let wrapped_named_arg = Value::Map {
                                        keys: vec![
                                            Value::String("type".to_string()),
                                            Value::String("value".to_string()),
                                        ],
                                        values: vec![
                                            arg_type.clone(),
                                            def_val.convert_to_map(),
                                        ],
                                    };
                                    named_args.push((arg_name, wrapped_named_arg));
                                } else {
                                    let arg_stmt = Statement::Statement {
                                        keys: vec![
                                            Value::String("name".to_string()),
                                            Value::String("type".to_string()),
                                        ],
                                        values: vec![
                                            Value::String(arg_name.clone()),
                                            arg_type,
                                        ],
                                        line,
                                        column,
                                    };
                                    pos_args.push(arg_stmt);
                                }
                            } else if tok.0 == "SEPARATOR" && tok.1 == "," {
                                self.next();
                            } else {
                                let unexpected = tok.1.clone();
                                self.raise("SyntaxError", &format!("Unexpected token '{}'", unexpected));
                                return Statement::Null;
                            }
                        }                        

                        self.check_for("SEPARATOR", ")");
                        self.next();

                        if self.err.is_some() {
                            return Statement::Null;
                        }

                        let mut return_type = Value::Map {
                            keys: vec![
                                Value::String("type".to_string()),
                                Value::String("value".to_string()),
                                Value::String("type_kind".to_string()),
                            ],
                            values: vec![
                                Value::String("TYPE".to_string()),
                                Value::String("any".to_string()),
                                Value::String("simple".to_string()),
                            ],
                        }.convert_to_statement();

                        if self.token_is("OPERATOR", "->") {
                            self.next();
                            return_type = self.parse_expression();
                        }

                        if self.err.is_some() {
                            return Statement::Null;
                        }
                        if return_type.get_type() != "TYPE" {
                            self.raise("SyntaxError", "Expected a type after '->'");
                            return Statement::Null;
                        }

                        if self.token_is("SEPARATOR", ":") {
                            self.next();
                        } else if !self.token_is("IDENTIFIER", "end") {
                            self.raise("SyntaxError", "Expected ':' or 'end' after function parameters");
                            return Statement::Null;
                        }

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

                        return Statement::Statement {
                            keys: vec![
                                Value::String("type".to_string()),
                                Value::String("name".to_string()),
                                Value::String("modifiers".to_string()),
                                Value::String("pos_args".to_string()),
                                Value::String("named_args".to_string()),
                                Value::String("body".to_string()),
                                Value::String("return_type".to_string()),
                            ],
                            values: vec![
                                Value::String("FUNCTION_DECLARATION".to_string()),
                                Value::String(name),
                                Value::List(modifiers.into_iter().map(Value::String).collect()),
                                Value::List(pos_args.into_iter().map(|s| s.convert_to_map()).collect()),
                                Value::Map {
                                    keys: named_args.iter().map(|(k, _)| Value::String(k.clone())).collect(),
                                    values: named_args.iter().map(|(_, v)| v.clone()).collect(),
                                },
                                Value::List(body.into_iter().map(|s| s.convert_to_map()).collect()),
                                return_type.convert_to_map(),
                            ],
                            line,
                            column,
                        };
                    } else {
                        if self.token_is("SEPARATOR", ":") {
                            self.next();
                            let type_token = self.token().cloned().unwrap_or(DEFAULT_TOKEN.clone());
                            if type_token.0 != "IDENTIFIER" {
                                self.raise("SyntaxError", "Expected type after ':'");
                                return Statement::Null;
                            }
                            let type_ = self.parse_expression();
                            let mut value = get_type_default_as_statement_from_statement(&type_).convert_to_map();
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
                                    Value::String("modifiers".to_string()),
                                ],
                                values: vec![
                                    Value::String("VARIABLE_DECLARATION".to_string()),
                                    Value::String(name),
                                    type_.clone().convert_to_map(),
                                    value,
                                    Value::List(modifiers.into_iter().map(Value::String).collect()),
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
                }

                "IDENTIFIER" if token.1 == "return" => {
                    self.next();
                    if ["end"].contains(&self.token().map(|t| t.1.as_str()).unwrap_or("")) {
                        return Statement::Statement {
                            keys: vec![
                                Value::String("type".to_string()),
                                Value::String("value".to_string()),
                            ],
                            values: vec![
                                Value::String("RETURN".to_string()),
                                Value::Map {
                                    keys: vec![
                                        Value::String("type".to_string()),
                                        Value::String("value".to_string()),
                                    ],
                                    values: vec![
                                        Value::String("BOOLEAN".to_string()),
                                        Value::String("null".to_string()),
                                    ],
                                },
                            ],
                            line,
                            column,
                        };
                    }
                    let value = self.parse_expression();
                    if self.err.is_some() {
                        return Statement::Null;
                    }
                    Statement::Statement {
                        keys: vec![
                            Value::String("type".to_string()),
                            Value::String("value".to_string()),
                        ],
                        values: vec![
                            Value::String("RETURN".to_string()),
                            value.convert_to_map(),
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

    fn parse_type(&mut self) -> Statement {
        let mut line = self.current_line();
        let mut column = self.get_line_column();
        
        fn check_type(token: &Token) -> bool {
            token.0 == "IDENTIFIER" && VALID_TYPES.contains(&token.1.as_str())
        }

        let base = self.token().cloned().unwrap_or(DEFAULT_TOKEN.clone());
        if !check_type(&base) {
            self.raise("SyntaxError", &format!("Invalid type '{}'", base.1));
            return Statement::Null;
        }
        let base_str = base.1.clone();
        self.next();
        if self.token_is("SEPARATOR", "[") {
            self.next();
            let mut elements = vec![];
            let mut has_variadic_any = false;
            let mut has_variadic_typed = None;

            line = self.current_line();
            column = self.get_line_column();
            
            while let Some(token) = self.token().cloned() {
                if token.0 == "SEPARATOR" && token.1 == "]" {
                    break;
                }
                if token.1 == ".." {
                    if has_variadic_any || has_variadic_typed.is_some() {
                        self.raise("SyntaxError", "Multiple variadic markers are not allowed");
                        return Statement::Null;
                    }
                    has_variadic_any = true;
                    self.next();
                
                    if let Some(next_token) = self.token().cloned() {
                        if !(next_token.0 == "SEPARATOR" && next_token.1 == "]") {
                            if check_type(&next_token) {
                                self.raise_with_help(
                                    "SyntaxError",
                                    "'..' must be the last element in type list",
                                    to_static(format!("Did you mean to use '{}...{}{}'?", check_ansi("\x1b[4m", &self.use_colors), next_token.1, check_ansi("\x1b[24m", &self.use_colors))),
                                );
                            } else {
                                self.raise(
                                    "SyntaxError",
                                    "'..' must be the last element in type list",
                                );
                            }
                            return Statement::Null;
                        }
                    }
                    continue;
                }
                if token.1 == "..." {
                    if has_variadic_any || has_variadic_typed.is_some() {
                        self.raise("SyntaxError", "Multiple variadic markers are not allowed");
                        return Statement::Null;
                    }
                    self.next();
                    if let Some(next_token) = self.token().cloned() {
                        if !check_type(&next_token) {
                            self.raise("SyntaxError", "Expected a valid type after '...'");
                            return Statement::Null;
                        }
                        has_variadic_typed = Some(next_token.1.clone());
                        has_variadic_any = true;
                        self.next();
                        if let Some(next_after) = self.token() {
                            if !(next_after.0 == "SEPARATOR" && next_after.1 == "]") {
                                self.raise("SyntaxError", "'...type' must be the last element in type list");
                                return Statement::Null;
                            }
                        }
                        continue;
                    } else {
                        self.raise("SyntaxError", "Expected a type after '...'");
                        return Statement::Null;
                    }
                }
                if !check_type(&token) {
                    self.raise("SyntaxError", &format!("Invalid type '{}'", token.1));
                    return Statement::Null;
                }
                if has_variadic_any || has_variadic_typed.is_some() {
                    self.raise("SyntaxError", "No types allowed after variadic marker");
                    return Statement::Null;
                }
                elements.push(token.1.clone());
                self.next();
                if self.token_is("SEPARATOR", ",") {
                    self.next();
                }
            }
            self.check_for("SEPARATOR", "]");
            self.next();
            if elements.is_empty() && !has_variadic_any && has_variadic_typed.is_none() {
                self.raise("SyntaxError", "Empty type annotation is not allowed");
                return Statement::Null;
            }
            if base_str == "function" {
                line = self.current_line();
                column = self.get_line_column();
                let mut return_type = Value::Map {
                    keys: vec![
                        Value::String("type".to_string()),
                        Value::String("type_kind".to_string()),
                        Value::String("value".to_string()),
                    ],
                    values: vec![
                        Value::String("TYPE".to_string()),
                        Value::String("simple".to_string()),
                        Value::String("any".to_string()),
                    ],
                }.convert_to_statement();
                if self.token_is("OPERATOR", "->") {
                    self.next();
                    return_type = self.parse_expression();
                    if self.err.is_some() {
                        return Statement::Null;
                    }
                    if return_type.get_type() != "TYPE" {
                        self.raise("SyntaxError", "Expected a type after '->'");
                        return Statement::Null;
                    }
                }
                return Statement::Statement {
                    keys: vec![
                        Value::String("type".to_string()),
                        Value::String("type_kind".to_string()),
                        Value::String("elements".to_string()),
                        Value::String("return_type".to_string()),
                        Value::String("variadic".to_string()),
                        Value::String("variadic_type".to_string()),
                    ],
                    values: vec![
                        Value::String("TYPE".to_string()),
                        Value::String("function".to_string()),
                        Value::List(elements.into_iter().map(Value::String).collect()),
                        return_type.convert_to_map(),
                        Value::Boolean(has_variadic_any),
                        Value::String(has_variadic_typed.unwrap_or_else(|| "any".to_string())),
                    ],
                    line,
                    column,
                };
            } else if !["map", "tuple", "list"].contains(&base_str.as_str()) {
                self.raise("SyntaxError", &format!("Type '{}' cannot be indexed", base_str));
            }
            line = self.current_line();
            column = self.get_line_column();
            return Statement::Statement {
                keys: vec![
                    Value::String("type".to_string()),
                    Value::String("type_kind".to_string()),
                    Value::String("base".to_string()),
                    Value::String("elements".to_string()),
                    Value::String("variadic".to_string()),
                    Value::String("variadic_type".to_string()),
                ],
                values: vec![
                    Value::String("TYPE".to_string()),
                    Value::String("indexed".to_string()),
                    Value::String(base_str.clone()),
                    Value::List(elements.into_iter().map(Value::String).collect()),
                    Value::Boolean(has_variadic_any),
                    Value::String(has_variadic_typed.unwrap_or_else(|| "any".to_string())),
                ],
                line,
                column,
            };
        } else {
            line = self.current_line();
            column = self.get_line_column();
            return Statement::Statement {
                keys: vec![
                    Value::String("type".to_string()),
                    Value::String("type_kind".to_string()),
                    Value::String("value".to_string()),
                ],
                values: vec![
                    Value::String("TYPE".to_string()),
                    Value::String("simple".to_string()),
                    Value::String(base_str),
                ],
                line,
                column,
            }
        }
        Statement::Null
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
                let type_ = self.parse_expression();
                let mut value = get_type_default_as_statement_from_statement(&type_).convert_to_map();
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
                        Value::String("modifiers".to_string()),
                    ],
                    values: vec![
                        Value::String("VARIABLE_DECLARATION".to_string()),
                        Value::String(name),
                        type_.clone().convert_to_map(),
                        value,
                        Value::List(vec![]),
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
