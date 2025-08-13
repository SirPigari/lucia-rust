use crate::env::runtime::utils::{unescape_string_literal, get_type_default_as_statement, get_type_default_as_statement_from_statement, get_precedence, parse_usize_radix};
use crate::env::runtime::value::Value;
use crate::env::runtime::errors::Error;
use crate::env::runtime::statements::Statement;
use crate::env::runtime::types::{VALID_TYPES};
use crate::env::runtime::tokens::{Token, Location, DEFAULT_TOKEN};

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    statements: Vec<Statement>,
    include_whitespace: bool,
    err: Option<Error>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            pos: 0,
            statements: vec![],
            include_whitespace: false,
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

    fn token_is(&mut self, kind: &str, value: &str) -> bool {
        self.token().map_or(false, |t| t.0 == kind && t.1 == value)
    }

    #[cold]
    pub fn raise(&mut self, error_type: &str, msg: &str) -> Statement {
        self.err = Some(Error {
            error_type: error_type.to_string(),
            msg: msg.to_string(),
            help: None,
            loc: self.get_loc(),
            ref_err: None,
        });
        Statement::Null
    }

    #[cold]
    pub fn raise_with_help(&mut self, error_type: &str, msg: &str, help: &str) -> Statement {
        self.err = Some(Error {
            error_type: error_type.to_string(),
            msg: msg.to_string(),
            help: Some(help.to_string()),
            loc: self.get_loc(),
            ref_err: None,
        });
        Statement::Null
    }

    #[cold]
    pub fn raise_with_loc(&mut self, error_type: &str, msg: &str, loc: Location) -> Statement {
        self.err = Some(Error {
            error_type: error_type.to_string(),
            msg: msg.to_string(),
            help: None,
            loc: Some(loc),
            ref_err: None,
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

    pub fn get_loc(&mut self) -> Option<Location> {
        if let Some(token) = self.token() {
            if token.2.is_some() {
                token.2.clone()
            } else if self.pos > 0 {
                self.tokens.get(self.pos - 1).and_then(|t| t.2.clone())
            } else {
                None
            }
        } else if self.pos > 0 {
            self.tokens.get(self.pos - 1).and_then(|t| t.2.clone())
        } else {
            None
        }
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

    fn parse_postfix(&mut self, mut expr: Statement) -> Statement {
        if self.err.is_some() {
            return Statement::Null;
        }
        while let Some(tok_ref) = self.token() {
            let tok = tok_ref.clone();
            match (tok.0.as_str(), tok.1.as_str()) {
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
                                } else if t2.0 == "SEPARATOR" && t2.1 == "]" {
                                    end_expr = start_expr.clone();
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
                        loc: self.get_loc(),
                    };
                }

                ("SEPARATOR", ".") => {
                    self.next();
                    let property_token = self.token().cloned().unwrap_or_else(|| {
                        self.raise("SyntaxError", "Expected identifier after '.'");
                        DEFAULT_TOKEN.clone()
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
                                loc: self.get_loc(),
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
                                loc: self.get_loc(),
                            };
                        }
                    }
                }

                ("OPERATOR", "-li") => {
                    self.next();
                    let loc = self.get_loc();
                    if self.token_is("SEPARATOR", ":") {
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

                        if !self.token_is("IDENTIFIER", "end") {
                            self.raise_with_help("SyntaxError", "Expected 'end' after 'while' body", "Did you forget to add 'end'?");
                            return Statement::Null;
                        }
                        self.next();
                        return Statement::Statement {
                            keys: vec![
                                Value::String("type".to_string()),
                                Value::String("condition".to_string()),
                                Value::String("body".to_string()),
                                Value::String("else_body".to_string()),
                            ],
                            values: vec![
                                Value::String("IF".to_string()),
                                expr.convert_to_map(),
                                Value::List(body.into_iter().map(|e| e.convert_to_map()).collect()),
                                Value::List(vec![]),
                            ],
                            loc,
                        };
                    } else {
                        let then_body = self.parse_expression();
                        return Statement::Statement {
                            keys: vec![
                                Value::String("type".to_string()),
                                Value::String("condition".to_string()),
                                Value::String("body".to_string()),
                                Value::String("else_body".to_string()),
                            ],
                            values: vec![
                                Value::String("IF".to_string()),
                                expr.convert_to_map(),
                                Value::List(vec![then_body.convert_to_map()]),
                                Value::List(vec![]),
                            ],
                            loc,
                        };
                    }
                }

                ("OPERATOR", "?") => {
                    let loc = self.get_loc();
                    self.next();

                    while self.token_is("OPERATOR", "?") {
                        self.next();
                    }

                    if self.token_is("OPERATOR", "or") {
                        self.next();
                        let otherwise = self.parse_expression();
                        if self.err.is_some() {
                            return Statement::Null;
                        }
                        return Statement::Statement {
                            keys: vec![
                                Value::String("type".to_string()),
                                Value::String("body".to_string()),
                                Value::String("exception_vars".to_string()),
                                Value::String("catch_body".to_string()),
                            ],
                            values: vec![
                                Value::String("TRY_CATCH".to_string()),
                                Value::List(vec![expr.convert_to_map()]),
                                Value::List(vec![Value::String("_".to_string())]),
                                Value::List(vec![otherwise.convert_to_map()]),
                            ],
                            loc,
                        };
                    }

                    return Statement::Statement {
                        keys: vec![
                            Value::String("type".to_string()),
                            Value::String("body".to_string()),
                            Value::String("exception_vars".to_string()),
                            Value::String("catch_body".to_string()),
                        ],
                        values: vec![
                            Value::String("TRY".to_string()),
                            Value::List(vec![expr.convert_to_map()]),
                            Value::List(vec![]),
                            Value::List(vec![]),
                        ],
                        loc,
                    };
                }

                ("OPERATOR", "??") => {
                    let loc = self.get_loc();
                    self.next();

                    let mut multi = false;
                    while self.token_is("OPERATOR", "?") || self.token_is("OPERATOR", "??") {
                        multi = true;
                        self.next();
                    }

                    if self.token_is("OPERATOR", "or") || !multi {
                        if self.token_is("OPERATOR", "or") {
                            self.next();
                        }

                        let otherwise = self.parse_expression();
                        if self.err.is_some() {
                            return Statement::Null;
                        }

                        return Statement::Statement {
                            keys: vec![
                                Value::String("type".to_string()),
                                Value::String("body".to_string()),
                                Value::String("exception_vars".to_string()),
                                Value::String("catch_body".to_string()),
                            ],
                            values: vec![
                                Value::String("TRY_CATCH".to_string()),
                                Value::List(vec![expr.convert_to_map()]),
                                Value::List(vec![Value::String("_".to_string())]),
                                Value::List(vec![otherwise.convert_to_map()]),
                            ],
                            loc,
                        };
                    }

                    return Statement::Statement {
                        keys: vec![
                            Value::String("type".to_string()),
                            Value::String("body".to_string()),
                            Value::String("exception_vars".to_string()),
                            Value::String("catch_body".to_string()),
                        ],
                        values: vec![
                            Value::String("TRY".to_string()),
                            Value::List(vec![expr.convert_to_map()]),
                            Value::List(vec![]),
                            Value::List(vec![]),
                        ],
                        loc,
                    };
                }

                ("OPERATOR", op) if ["++", "--"].contains(&op) => {
                    let operator = match op {
                        "++" => "+",
                        "--" => "-",
                        _ => unreachable!(),
                    }.to_string();
                    self.next();
                    expr = Statement::Statement {
                        keys: vec![
                            Value::String("type".to_string()),
                            Value::String("left".to_string()),
                            Value::String("right".to_string()),
                        ],
                        values: vec![
                            Value::String("ASSIGNMENT".to_string()),
                            expr.convert_to_map(),
                            Statement::Statement {
                                keys: vec![
                                    Value::String("type".to_string()),
                                    Value::String("left".to_string()),
                                    Value::String("right".to_string()),
                                    Value::String("operator".to_string()),
                                ],
                                values: vec![
                                    Value::String("OPERATION".to_string()),
                                    expr.convert_to_map(),
                                    Value::Map {
                                        keys: vec![
                                            Value::String("type".to_string()),
                                            Value::String("value".to_string()),
                                        ],
                                        values: vec![
                                            Value::String("NUMBER".to_string()),
                                            Value::String("1".to_string()),
                                        ],
                                    },
                                    Value::String(operator),
                                ],
                                loc: self.get_loc(),
                            }.convert_to_map(),
                        ],
                        loc: self.get_loc(),
                    };
                }

                _ => break,
            }
        }

        expr
    }
    
    fn parse_expression(&mut self) -> Statement {
        let mut expr = self.parse_primary();
        if self.err.is_some() {
            return Statement::Null;
        }

        if expr.get_type() != "TYPE" {
            (expr, _) = self.parse_binops(expr, 0);
        }

        while let Some(tok_ref) = self.token() {
            let tok = tok_ref.clone();

            match (tok.0.as_str(), tok.1.as_str()) {
                ("IDENTIFIER", "as") => {
                    self.next();
                    let type_conv = self.parse_type();
                    if self.err.is_some() {
                        return Statement::Null;
                    }

                    expr = Statement::Statement {
                        keys: vec![
                            Value::String("type".to_string()),
                            Value::String("value".to_string()),
                            Value::String("to".to_string()),
                        ],
                        values: vec![
                            Value::String("TYPE_CONVERT".to_string()),
                            expr.convert_to_map(),
                            type_conv.convert_to_map(),
                        ],
                        loc: self.get_loc(),
                    };
                }

                ("OPERATOR", "=>") => {
                    self.next();
                    let body_expr = self.parse_expression();
                    if self.err.is_some() {
                        return Statement::Null;
                    }

                    let params = match &expr {
                        Statement::Statement { .. } if expr.get_type() == "TUPLE" => {
                            expr.get_value("items").unwrap_or(Value::List(vec![]))
                        }
                        _ => Value::List(vec![expr.convert_to_map()]),
                    };

                    let mut pos_args = vec![];
                    let mut named_args = vec![];

                    if let Value::List(items) = params {
                        for param in items {
                            match param {
                                Value::Map { keys, values } => {
                                    let mut name_opt = None;
                                    let mut type_val = Value::Map {
                                        keys: vec![
                                            Value::String("type".into()),
                                            Value::String("value".into()),
                                            Value::String("type_kind".into()),
                                        ],
                                        values: vec![
                                            Value::String("TYPE".into()),
                                            Value::String("any".into()),
                                            Value::String("simple".into()),
                                        ],
                                    };
                                    let mut value_opt = None;
                                    let mut is_decl = false;
                                    let mut is_default = false;

                                    for (k, v) in keys.iter().zip(values.iter()) {
                                        match (k, v) {
                                            (Value::String(k), Value::String(v))
                                                if k == "type" && v == "VARIABLE_DECLARATION" =>
                                            {
                                                is_decl = true;
                                            }
                                            (Value::String(k), Value::Boolean(b)) if k == "is_default" => {
                                                is_default = *b;
                                            }
                                            (Value::String(k), Value::String(n)) if k == "name" => {
                                                name_opt = Some(n.clone());
                                            }
                                            (Value::String(k), val) if k == "var_type" => {
                                                type_val = val.clone();
                                            }
                                            (Value::String(k), val) if k == "value" => {
                                                value_opt = Some(val.clone());
                                            }
                                            _ => {}
                                        }
                                    }

                                    if let Some(name) = name_opt {
                                        if is_decl {
                                            if is_default {
                                                pos_args.push(Value::Map {
                                                    keys: vec![
                                                        Value::String("name".into()),
                                                        Value::String("type".into()),
                                                        Value::String("modifiers".into()),
                                                    ],
                                                    values: vec![
                                                        Value::String(name),
                                                        type_val,
                                                        Value::List(vec![]),
                                                    ],
                                                });
                                            } else {
                                                if let Some(val) = value_opt {
                                                    named_args.push((
                                                        name,
                                                        Value::Map {
                                                            keys: vec![
                                                                Value::String("type".into()),
                                                                Value::String("value".into()),
                                                                Value::String("modifiers".into()),
                                                            ],
                                                            values: vec![
                                                                type_val,
                                                                val,
                                                                Value::List(vec![]),
                                                            ],
                                                        },
                                                    ));
                                                }
                                            }
                                        } else {
                                            pos_args.push(Value::Map {
                                                keys: vec![
                                                    Value::String("name".into()),
                                                    Value::String("type".into()),
                                                    Value::String("modifiers".into()),
                                                ],
                                                values: vec![
                                                    Value::String(name),
                                                    Value::Map {
                                                        keys: vec![
                                                            Value::String("type".into()),
                                                            Value::String("value".into()),
                                                            Value::String("type_kind".into()),
                                                        ],
                                                        values: vec![
                                                            Value::String("TYPE".into()),
                                                            Value::String("any".into()),
                                                            Value::String("simple".into()),
                                                        ],
                                                    },
                                                    Value::List(vec![]),
                                                ],
                                            });
                                        }
                                    }
                                }

                                Value::String(name) => {
                                    pos_args.push(Value::Map {
                                        keys: vec![
                                            Value::String("name".into()),
                                            Value::String("type".into()),
                                            Value::String("modifiers".into()),
                                        ],
                                        values: vec![
                                            Value::String(name),
                                            Value::Map {
                                                keys: vec![
                                                    Value::String("type".into()),
                                                    Value::String("value".into()),
                                                    Value::String("type_kind".into()),
                                                ],
                                                values: vec![
                                                    Value::String("TYPE".into()),
                                                    Value::String("any".into()),
                                                    Value::String("simple".into()),
                                                ],
                                            },
                                            Value::List(vec![]),
                                        ],
                                    });
                                }

                                _ => {}
                            }
                        }
                    }

                    let named_args_map = Value::Map {
                        keys: named_args.iter().map(|(k, _)| Value::String(k.clone())).collect(),
                        values: named_args.iter().map(|(_, v)| v.clone()).collect(),
                    };

                    let body = vec![body_expr];
                    let body_values: Vec<Value> = body.into_iter().map(|stmt| stmt.convert_to_map()).collect();

                    expr = Statement::Statement {
                        keys: vec![
                            Value::String("type".into()),
                            Value::String("name".into()),
                            Value::String("modifiers".into()),
                            Value::String("pos_args".into()),
                            Value::String("named_args".into()),
                            Value::String("body".into()),
                            Value::String("return_type".into()),
                        ],
                        values: vec![
                            Value::String("FUNCTION_DECLARATION".into()),
                            Value::String("<lambda#{id}>".into()),
                            Value::List(vec![Value::String("mutable".into())]),
                            Value::List(pos_args),
                            named_args_map,
                            Value::List(body_values),
                            Value::Map {
                                keys: vec![
                                    Value::String("type".into()),
                                    Value::String("value".into()),
                                    Value::String("type_kind".into()),
                                ],
                                values: vec![
                                    Value::String("TYPE".into()),
                                    Value::String("any".into()),
                                    Value::String("simple".into()),
                                ],
                            },
                        ],
                        loc: self.get_loc(),
                    };
                }

                ("OPERATOR", "=") => {
                    self.next();
                    let value = self.parse_expression();
                    if self.err.is_some() {
                        return Statement::Null;
                    }

                    let expr_type = expr.get_type();
                    if expr_type == "TUPLE" {
                        let targets = expr.get_value("items").unwrap_or(Value::Null);
                        return Statement::Statement {
                            keys: vec![
                                Value::String("type".to_string()),
                                Value::String("targets".to_string()),
                                Value::String("value".to_string()),
                            ],
                            values: vec![
                                Value::String("UNPACK_ASSIGN".to_string()),
                                targets.clone(),
                                value.convert_to_map(),
                            ],
                            loc: self.get_loc(),
                        };
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
                        loc: self.get_loc(),
                    };
                }

                _ => break,
            }
        }

        expr
    }

    fn parse_binops(&mut self, mut lhs: Statement, min_prec: u8) -> (Statement, bool) {
        loop {
            let tok_opt = self.token().cloned();

            let (tok_type, op_str) = match tok_opt {
                Some(Token(ref ttype, ref val, _)) => (ttype.as_str(), val.as_str()),
                None => break,
            };

            if ["=", "=>", "as", "++", "--", "|", "-li"].contains(&op_str) {
                break;
            }

            if ["=!", "=+", "=-", "=*", "=/", "=^"].contains(&op_str) {
                self.raise_with_help(
                    "SyntaxError",
                    &format!("Unexpected operator: '{}'", op_str),
                    &format!("Did you mean to use '{}'?", op_str.chars().rev().collect::<String>()),
                );
                return (Statement::Null, false);
            }

            if tok_type != "OPERATOR" {
                break;
            }

            let prec = get_precedence(op_str);
            if prec < min_prec {
                break;
            }

            let backup_pos = self.pos;

            self.next();

            let mut rhs = self.parse_primary();
            if self.err.is_some() {
                return (Statement::Null, false);
            }

            if matches!(rhs, Statement::Null) {
                self.pos = backup_pos;
                return (lhs, false);
            }

            loop {
                let next_tok_opt = self.token().cloned();

                let (next_tok_type, next_op_str) = match next_tok_opt {
                    Some(Token(ref ttype, ref val, _)) => (ttype.as_str(), val.as_str()),
                    None => break,
                };

                if next_tok_type != "OPERATOR" {
                    break;
                }

                let next_prec = get_precedence(next_op_str);

                let right_assoc_ops = ["^", "^^", "^^^"];

                if (right_assoc_ops.contains(&op_str) && next_prec >= prec)
                    || (!right_assoc_ops.contains(&op_str) && next_prec > prec)
                {
                    let success;
                    (rhs, success) = self.parse_binops(rhs, next_prec);
                    if self.err.is_some() {
                        return (Statement::Null, false);
                    }
                    if matches!(rhs, Statement::Null) || !success {
                        self.pos = backup_pos;
                        return (lhs, false);
                    }
                } else {
                    break;
                }
            }

            lhs = Statement::Statement {
                keys: vec![
                    Value::String("type".to_string()),
                    Value::String("left".to_string()),
                    Value::String("operator".to_string()),
                    Value::String("right".to_string()),
                ],
                values: vec![
                    Value::String("OPERATION".to_string()),
                    lhs.convert_to_map(),
                    Value::String(op_str.to_string()),
                    rhs.convert_to_map(),
                ],
                loc: self.get_loc(),
            };
        }

        (lhs, true)
    }

    fn parse_primary(&mut self) -> Statement {
        let expr = match self.token().cloned() {
            Some(token) => match token.0.as_str() {
                "IDENTIFIER" if VALID_TYPES.contains(&token.1.as_str()) => {
                    self.parse_type()
                }

                "SEPARATOR" if token.1 == "\\" => {
                    self.next();
                    let mut exprs = vec![];
                    while let Some(_) = self.token().cloned() {
                        if self.token_is("SEPARATOR", "\\") {
                            break;
                        }
                        let expr = self.parse_expression();
                        if matches!(expr, Statement::Null) {
                            break;
                        }
                        exprs.push(expr);
                        if self.err.is_some() {
                            return Statement::Null;
                        }
                    }
                    self.check_for("SEPARATOR", "\\");
                    self.next();
                    if exprs.is_empty() {
                        return Statement::Null;
                    }
                    Statement::Statement {
                        keys: vec![
                            Value::String("type".to_string()),
                            Value::String("expressions".to_string()),
                        ],
                        values: vec![
                            Value::String("GROUP".to_string()),
                            Value::List(exprs.into_iter().map(|e| e.convert_to_map()).collect()),
                        ],
                        loc: self.get_loc(),
                    }
                }

                "OPERATOR" if token.1 == "/" => {
                    self.raise_with_help(
                        "SyntaxError",
                        "Unexpected operator '/'",
                        "Did you mean to use '//' for comments or '\\' for groups?",
                    );
                    return Statement::Null;
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
                        loc: self.get_loc(),
                    }
                }

                "OPERATOR" if ["+", "-", "!", "~", "nein", "isnt", "isn't", "not", "bnot"].contains(&token.1.as_str()) => {
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
                        loc: self.get_loc(),
                    }
                }

                "OPERATOR" if token.1 == "&" => {
                    self.next();
                    let expr = self.parse_expression();
                    if self.err.is_some() { return Statement::Null; }
                    Statement::Statement {
                        keys: vec![
                            Value::String("type".to_string()),
                            Value::String("value".to_string()),
                        ],
                        values: vec![
                            Value::String("POINTER_REF".to_string()),
                            expr.convert_to_map(),
                        ],
                        loc: self.get_loc(),
                    }
                }

                "OPERATOR" if token.1 == "*" => {
                    self.next();
                    let expr = self.parse_primary();
                    if self.err.is_some() { return Statement::Null; }
                    if self.token_is("OPERATOR", "=") {
                        self.next();
                        let value = self.parse_expression();
                        if self.err.is_some() { return Statement::Null; }
                        return Statement::Statement {
                            keys: vec![
                                Value::String("type".to_string()),
                                Value::String("left".to_string()),
                                Value::String("right".to_string()),
                            ],
                            values: vec![
                                Value::String("POINTER_ASSIGN".to_string()),
                                expr.convert_to_map(),
                                value.convert_to_map(),
                            ],
                            loc: self.get_loc(),
                        };
                    }
                    Statement::Statement {
                        keys: vec![
                            Value::String("type".to_string()),
                            Value::String("value".to_string()),
                        ],
                        values: vec![
                            Value::String("POINTER_DEREF".to_string()),
                            expr.convert_to_map(),
                        ],
                        loc: self.get_loc(),
                    }
                }

                "SEPARATOR" if token.1 == "{" => {
                    let loc = self.get_loc();
                    self.next();
                    let mut keys = vec![];
                    let mut values = vec![];

                    while let Some(next_token) = self.token() {
                        if next_token.0 == "SEPARATOR" && next_token.1 == "}" {
                            self.next();
                            break;
                        }

                        let mut modifier = Value::String("mutable".to_string());
                        loop {
                            if let Some(tok) = self.token() {
                                if tok.0 == "IDENTIFIER" && (tok.1 == "final" || tok.1 == "mutable") {
                                    modifier = Value::String(tok.1.clone());
                                    self.next();
                                } else {
                                    break;
                                }
                            } else {
                                break;
                            }
                        }

                        let key = self.parse_expression();
                        if self.err.is_some() {
                            return Statement::Null;
                        }

                        let key_map = key.convert_to_map();

                        // if keys.iter().any(|k| {
                        //     if let Value::Map { keys: mk, values: mv } = k {
                        //         mk.iter().zip(mv).any(|(kk, vv)| {
                        //             kk == &Value::String("key".to_string()) && vv == &key_map
                        //         })
                        //     } else {
                        //         false
                        //     }
                        // }) {
                        //     self.raise("SyntaxError", "Duplicate key in map");
                        //     return Statement::Null;
                        // }

                        let value;
                        if let Some(next_token) = self.token() {
                            if next_token.0 == "SEPARATOR" && next_token.1 == ":" {
                                self.next();
                                value = self.parse_expression();
                                if self.err.is_some() {
                                    return Statement::Null;
                                }
                            } else {
                                self.raise("SyntaxError", "Expected ':' after key in map");
                                return Statement::Null;
                            }
                        } else {
                            self.raise("SyntaxError", "Unexpected end of input in map");
                            return Statement::Null;
                        }

                        keys.push(Value::Map {
                            keys: vec![
                                Value::String("modifier".to_string()),
                                Value::String("key".to_string()),
                            ],
                            values: vec![
                                modifier,
                                key_map,
                            ],
                        });                        

                        values.push(value.convert_to_map());

                        if let Some(next_token) = self.token() {
                            if next_token.0 == "SEPARATOR" && next_token.1 == "," {
                                self.next();
                            } else if next_token.0 == "SEPARATOR" && next_token.1 == "}" {
                                self.next();
                                break;
                            } else {
                                self.raise("SyntaxError", "Expected ',' or '}' in map");
                                return Statement::Null;
                            }
                        } else {
                            self.raise("SyntaxError", "Unexpected end of input in map");
                            return Statement::Null;
                        }
                    }

                    Statement::Statement {
                        keys: vec![
                            Value::String("type".to_string()),
                            Value::String("keys".to_string()),
                            Value::String("values".to_string()),
                        ],
                        values: vec![
                            Value::String("MAP".to_string()),
                            Value::List(keys),
                            Value::List(values),
                        ],
                        loc,
                    }
                }

                "SEPARATOR" | "IDENTIFIER" if token.1 == "..." || token.1 == "pass" => {
                    self.next();
                    Statement::Null
                }

                "IDENTIFIER" if token.1 == "for" => {
                    self.next();

                    if self.token_is("SEPARATOR", "else") {
                        self.raise("SyntaxError", "Unexpected 'else' after 'for'");
                        return Statement::Null;
                    }

                    let mut parentheses = false;
                    if self.token_is("SEPARATOR", "(") {
                        parentheses = true;
                        self.next();
                    }

                    let variable = if self.token_is("SEPARATOR", "(") {
                        self.next();
                        let mut vars = Vec::new();
                        loop {
                            if let Some(tok) = self.token() {
                                if tok.0 == "IDENTIFIER" {
                                    vars.push(tok.1.clone());
                                    self.next();
                                } else {
                                    self.raise("SyntaxError", "Expected identifier in tuple unpacking");
                                    return Statement::Null;
                                }

                                if self.token_is("SEPARATOR", ",") {
                                    self.next();
                                    continue;
                                } else if self.token_is("SEPARATOR", ")") {
                                    self.next();
                                    break;
                                } else {
                                    self.raise("SyntaxError", "Expected ',' or ')' in tuple unpacking");
                                    return Statement::Null;
                                }
                            } else {
                                self.raise("SyntaxError", "Unexpected end of input in tuple unpacking");
                                return Statement::Null;
                            }
                        }
                        Value::List(vars.into_iter().map(Value::String).collect())
                    } else {
                        let t = self.token().cloned().map(|tok| Value::String(tok.1)).unwrap_or_else(|| {
                            self.raise("SyntaxError", "Expected identifier after 'for'");
                            return Value::Null;
                        });
                        self.next();
                        t
                    };

                    if self.err.is_some() {
                        return Statement::Null;
                    }

                    self.check_for("OPERATOR", "in");
                    self.next();

                    let iterable = self.parse_expression();
                    if self.err.is_some() {
                        return Statement::Null;
                    }

                    if parentheses {
                        if !self.token_is("SEPARATOR", ")") {
                            self.raise_with_help("SyntaxError", "Expected ')' after 'for' iterable", "Did you forget to add ')'?");
                            return Statement::Null;
                        }
                        self.next();
                    }

                    if !self.token_is("SEPARATOR", ":") {
                        self.raise_with_help("SyntaxError", "Expected ':' after ')'", "Did you forget to add ':'?");
                        return Statement::Null;
                    }
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

                    if !self.token_is("IDENTIFIER", "end") {
                        self.raise_with_help("SyntaxError", "Expected 'end' after 'for' body", "Did you forget to add 'end'?");
                        return Statement::Null;
                    }
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
                            variable,
                            iterable.convert_to_map(),
                            Value::List(body.into_iter().map(|s| s.convert_to_map()).collect()),
                        ],
                        loc: self.get_loc(),
                    }
                }

                "IDENTIFIER" if token.1 == "while" => {
                    self.next();
                    let mut parentheses = false;
                    if self.token_is("SEPARATOR", "(") {
                        parentheses = true;
                        self.next();
                    }
                    let condition = self.parse_expression();
                    if self.err.is_some() {
                        return Statement::Null;
                    }
                    if parentheses {
                        if !self.token_is("SEPARATOR", ")") {
                            self.raise_with_help("SyntaxError", "Expected ')' after 'while' condition", "Did you forget to add ')'?");
                            return Statement::Null;
                        }
                        self.next();
                    }
                    if self.token_is("IDENTIFIER", "end") {
                        return Statement::Statement {
                            keys: vec![
                                Value::String("type".to_string()),
                                Value::String("condition".to_string()),
                                Value::String("body".to_string()),
                            ],
                            values: vec![
                                Value::String("WHILE".to_string()),
                                condition.convert_to_map(),
                                Value::List(vec![]),
                            ],
                            loc: self.get_loc(),
                        };
                    }
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

                    if !self.token_is("IDENTIFIER", "end") {
                        self.raise_with_help("SyntaxError", "Expected 'end' after 'while' body", "Did you forget to add 'end'?");
                        return Statement::Null;
                    }
                    self.next();

                    Statement::Statement {
                        keys: vec![
                            Value::String("type".to_string()),
                            Value::String("condition".to_string()),
                            Value::String("body".to_string()),
                        ],
                        values: vec![
                            Value::String("WHILE".to_string()),
                            condition.convert_to_map(),
                            Value::List(body.into_iter().map(|s| s.convert_to_map()).collect()),
                        ],
                        loc: self.get_loc(),
                    }
                }

                "IDENTIFIER" if token.1 == "continue" => {
                    self.next();
                    Statement::Statement {
                        keys: vec![
                            Value::String("type".to_string()),
                        ],
                        values: vec![
                            Value::String("CONTINUE".to_string()),
                        ],
                        loc: self.get_loc(),
                    }
                }

                "IDENTIFIER" if token.1 == "break" => {
                    self.next();
                    Statement::Statement {
                        keys: vec![
                            Value::String("type".to_string()),
                        ],
                        values: vec![
                            Value::String("BREAK".to_string()),
                        ],
                        loc: self.get_loc(),
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
                        loc: self.get_loc(),
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
                        loc: self.get_loc(),
                    };
                    if self.token_is("IDENTIFIER", "from") {
                        self.next();
                        from = self.parse_expression();
                        if self.err.is_some() {
                            return Statement::Null;
                        }
                    }
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
                        loc: self.get_loc(),
                    }                    
                }

                "IDENTIFIER" if token.1 == "if" => {
                    self.next();
                    if self.token_is("SEPARATOR", "else") {
                        self.raise("SyntaxError", "Unexpected 'else' after 'if'");
                        return Statement::Null;
                    }
                    let mut parentheses = false;
                    if self.token_is("SEPARATOR", "(") {
                        parentheses = true;
                        self.next();
                    }
                    let condition = self.parse_expression();
                    if self.err.is_some() {
                        return Statement::Null;
                    }
                    if parentheses {
                        if !self.token_is("SEPARATOR", ")") {
                            self.raise_with_help("SyntaxError", "Expected ')' after 'if' condition", "Did you forget to add ')'?");
                            return Statement::Null;
                        }
                        self.next();
                    }

                    if self.token_is("IDENTIFIER", "then") {
                        self.next();
                        let then_expr = self.parse_expression();
                        if self.err.is_some() {
                            return Statement::Null;
                        }
                        let else_expr = if self.token_is("IDENTIFIER", "else") {
                            self.next();
                            let else_expr = self.parse_expression();
                            if self.err.is_some() {
                                return Statement::Null;
                            }
                            Some(else_expr)
                        } else {
                            None
                        };
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
                                Value::List(vec![then_expr.convert_to_map()]),
                                Value::List(match else_expr {
                                    Some(e) => vec![e.convert_to_map()],
                                    None => vec![],
                                }),
                            ],
                            loc: self.get_loc(),
                        }
                    } else {
                        self.check_for("SEPARATOR", ":");
                        self.next();
                        let mut body = vec![];
                        while let Some(tok) = self.token() {
                            if tok.0 == "IDENTIFIER" && (tok.1 == "end" || tok.1 == "else" || tok.1 == "elif") {
                                break;
                            }
                            let stmt = self.parse_expression();
                            if self.err.is_some() {
                                return Statement::Null;
                            }
                            body.push(stmt);
                        }
                        let else_body: Option<Vec<Statement>>;
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
                            } else if tok.0 == "IDENTIFIER" && tok.1 == "elif" {
                                self.raise_with_help("SyntaxError", "Unexpected 'elif'.", "Did you mean to use 'else if'?");
                                return Statement::Null;
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
                                }),
                            ],
                            loc: self.get_loc(),
                        }
                    }
                }

                "IDENTIFIER" if token.1 == "try" => {
                    self.next();
                    let loc = self.get_loc();

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

                            self.next();

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
                        loc
                    }
                }

                "IDENTIFIER" if ["fun", "gen", "typedef", "import", "public", "private", "static", "non-static", "final", "mutable"].contains(&token.1.as_str()) => {
                    let mut modifiers: Vec<String> = vec![];

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

                    if matches!(self.token(), Some(Token(t, _, _)) if t == "EOF") || self.token().is_none() {
                        self.raise("SyntaxError", "Unexpected end of input after modifiers");
                        return Statement::Null;
                    }

                    match self.token().cloned().unwrap_or_default().1.as_str() {
                        x if ["fun", "gen"].contains(&x) => {
                            self.next();
                            let name = self.token().cloned().map(|tok|  if tok.0 == "IDENTIFIER".to_string() { tok.1 } else { "".to_string() }).unwrap_or_default();
                            self.next();
                            if name.is_empty() {
                                self.raise("SyntaxError", "Function or generator declaration cannot have a name when using modifiers");
                                return Statement::Null;
                            }
                            let is_function = x == "fun";
                            self.check_for("SEPARATOR", "(");
                            self.next();
                            let mut pos_args = vec![];
                            let mut named_args = vec![];

                            while let Some(mut current_tok) = self.token().cloned() {
                                let mut param_modifiers = vec![];

                                while current_tok.0 == "IDENTIFIER" &&
                                    (current_tok.1 == "mutable" || current_tok.1 == "final" || current_tok.1 == "static" || current_tok.1 == "non-static")
                                {
                                    param_modifiers.push(current_tok.1.clone());
                                    self.next();

                                    if let Some(next_tok) = self.token() {
                                        current_tok = next_tok.clone();
                                    } else {
                                        break;
                                    }
                                }

                                if current_tok.0 == "SEPARATOR" && current_tok.1 == ")" {
                                    break;
                                }

                                if current_tok.0 == "IDENTIFIER" {
                                    let arg_name = current_tok.1.clone();
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

                                    if self.token_is("SEPARATOR", ":") {
                                        self.next();
                                        let type_expr = self.parse_type();
                                        if self.err.is_some() {
                                            return Statement::Null;
                                        }
                                        arg_type = type_expr.convert_to_map();
                                    }

                                    let modifiers_value = Value::List(param_modifiers.into_iter().map(Value::String).collect());

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
                                                Value::String("modifiers".to_string()),
                                            ],
                                            values: vec![
                                                arg_type.clone(),
                                                def_val.convert_to_map(),
                                                modifiers_value.clone(),
                                            ],
                                        };
                                        named_args.push((arg_name, wrapped_named_arg));
                                    } else {
                                        let mut keys = vec![
                                            Value::String("name".to_string()),
                                            Value::String("type".to_string()),
                                        ];
                                        let mut values = vec![
                                            Value::String(arg_name.clone()),
                                            arg_type,
                                        ];

                                        if let Value::List(_) = modifiers_value {
                                            keys.push(Value::String("modifiers".to_string()));
                                            values.push(modifiers_value);
                                        }

                                        let arg_stmt = Statement::Statement {
                                            keys,
                                            values,
                                            loc: self.get_loc(),
                                        };
                                        pos_args.push(arg_stmt);
                                    }
                                } else if current_tok.0 == "SEPARATOR" && current_tok.1 == "," {
                                    self.next();
                                } else {
                                    self.raise("SyntaxError", &format!("Unexpected token '{}'", current_tok.1));
                                    return Statement::Null;
                                }
                            }

                            self.check_for("SEPARATOR", ")");
                            self.next();

                            if is_function && self.token_is("OPERATOR", "?") {
                                self.next();
                            }

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
                                return_type = self.parse_type();
                            }

                            if self.err.is_some() {
                                return Statement::Null;
                            }
                            if return_type.get_type() != "TYPE" {
                                self.raise("SyntaxError", if is_function { "Expected a type after '->'" } else { "Expected a type after '->' in generator" });
                                return Statement::Null;
                            }

                            if self.token_is("SEPARATOR", ":") {
                                self.next();
                            } else if !self.token_is("IDENTIFIER", "end") {
                                self.raise("SyntaxError", if is_function { "Expected ':' or 'end' after function parameters" } else { "Expected ':' or 'end' after generator parameters" });
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
                                    if is_function { Value::String("FUNCTION_DECLARATION".to_string()) } else { Value::String("GENERATOR_DECLARATION".to_string()) },
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
                                loc: self.get_loc(),
                            };
                        }
                        "typedef" => {
                            self.next();
                            let type_token = self.token().cloned().unwrap_or(DEFAULT_TOKEN.clone());
                            if type_token.0 != "IDENTIFIER" {
                                self.raise("SyntaxError", "Expected type name after 'type'");
                                return Statement::Null;
                            }
                            let name = type_token.1.clone();
                            let name_loc = type_token.2.clone();
                            self.next();

                            let mut generics = vec![];
                            if self.token_is("OPERATOR", "<") {
                                self.next();
                                while !self.token_is("OPERATOR", ">") {
                                    let gen_token = self.token().cloned().unwrap_or(DEFAULT_TOKEN.clone());
                                    if gen_token.0 != "IDENTIFIER" {
                                        self.raise("SyntaxError", "Expected generic parameter");
                                        return Statement::Null;
                                    }
                                    generics.push(gen_token.1);
                                    self.next();
                                    if self.token_is("SEPARATOR", ",") {
                                        self.next();
                                    } else if !self.token_is("OPERATOR", ">") {
                                        self.raise("SyntaxError", "Expected ',' or '>' in generics");
                                        return Statement::Null;
                                    }
                                }
                                self.next();
                            }

                            let mut vars = vec![];
                            if self.token_is("SEPARATOR", "[") {
                                self.next();
                                while !self.token_is("SEPARATOR", "]") {
                                    let var_token = self.token().cloned().unwrap_or(DEFAULT_TOKEN.clone());
                                    if var_token.0 != "IDENTIFIER" {
                                        self.raise("SyntaxError", "Expected variable name in type declaration");
                                        return Statement::Null;
                                    }
                                    vars.push(var_token.1);
                                    self.next();
                                    if self.token_is("SEPARATOR", ",") {
                                        self.next();
                                    } else if !self.token_is("SEPARATOR", "]") {
                                        self.raise("SyntaxError", "Expected ',' or ']' in type variables");
                                        return Statement::Null;
                                    }
                                }
                                self.next();
                            }

                            if !self.token_is("OPERATOR", "=") {
                                self.raise("SyntaxError", "Expected '=' after type name");
                                return Statement::Null;
                            }
                            self.next();

                            let mut variants = vec![];
                            let mut paren_locs = vec![];

                            loop {
                                let variant_token = self.token().cloned().unwrap_or(DEFAULT_TOKEN.clone());
                                if variant_token.0 != "IDENTIFIER" {
                                    break;
                                }
                                let ident_pos = self.pos;
                                let variant_name = variant_token.1.clone();
                                self.next();

                                let paren_loc = if self.token_is("SEPARATOR", "(") {
                                    let loc = self.get_loc();
                                    self.next();

                                    let mut variant_args = vec![];
                                    while !self.token_is("SEPARATOR", ")") {
                                        let arg_type = self.parse_type();
                                        if self.err.is_some() {
                                            return Statement::Null;
                                        }
                                        variant_args.push(arg_type.convert_to_map());
                                        if self.token_is("SEPARATOR", ",") {
                                            self.next();
                                        } else if !self.token_is("SEPARATOR", ")") {
                                            self.raise("SyntaxError", "Expected ',' or ')' in variant arguments");
                                            return Statement::Null;
                                        }
                                    }
                                    self.next();

                                    variants.push(Value::Map {
                                        keys: vec![Value::String("name".to_string()), Value::String("args".to_string())],
                                        values: vec![
                                            Value::String(variant_name),
                                            Value::List(variant_args),
                                        ],
                                    });

                                    Some(loc)
                                } else {
                                    self.pos = ident_pos;
                                    let type_ = self.parse_type();
                                    if self.err.is_some() {
                                        return Statement::Null;
                                    }
                                    variants.push(type_.convert_to_map());
                                    None
                                };

                                paren_locs.push(paren_loc);

                                if self.token_is("OPERATOR", "|") {
                                    self.next();
                                } else {
                                    break;
                                }
                            }

                            let (all_have_parens, none_have_parens) = paren_locs.iter().fold((true, true), |(all, none), loc| {
                                (all && loc.is_some(), none && loc.is_none())
                            });

                            if !all_have_parens && !none_have_parens {
                                let err_loc = paren_locs.iter().filter_map(|loc| loc.clone()).next().unwrap_or(self.get_loc());
                                match err_loc {
                                    Some(loc) => self.raise_with_loc("SyntaxError", "Mixed variant parens usage is not allowed", loc),
                                    None => self.raise("SyntaxError", "Mixed variant parens usage is not allowed"),
                                };
                                return Statement::Null;
                            }

                            let mut conditions = vec![];
                            if self.token_is("IDENTIFIER", "where") {
                                self.next();
                                if !self.token_is("SEPARATOR", "(") {
                                    self.raise("SyntaxError", "Expected '(' after 'where'");
                                    return Statement::Null;
                                }
                                self.next();
                                while !self.token_is("SEPARATOR", ")") {
                                    let condition = self.parse_expression();
                                    if self.err.is_some() {
                                        return Statement::Null;
                                    }
                                    conditions.push(condition);
                                    if self.token_is("SEPARATOR", ",") {
                                        self.next();
                                    } else if !self.token_is("SEPARATOR", ")") {
                                        self.raise("SyntaxError", "Expected ',' or ')' in type conditions");
                                        return Statement::Null;
                                    }
                                }
                                self.next();
                            }

                            let kind = if none_have_parens {
                                "alias"
                            } else {
                                "enum"
                            };

                            Statement::Statement {
                                keys: vec![
                                    Value::String("type".to_string()),
                                    Value::String("name".to_string()),
                                    Value::String("generics".to_string()),
                                    Value::String("variables".to_string()),
                                    Value::String("variants".to_string()),
                                    Value::String("conditions".to_string()),
                                    Value::String("modifiers".to_string()),
                                    Value::String("kind".to_string()),
                                ],
                                values: vec![
                                    Value::String("TYPE_DECLARATION".to_string()),
                                    Value::String(name),
                                    Value::List(generics.into_iter().map(Value::String).collect()),
                                    Value::List(vars.into_iter().map(Value::String).collect()),
                                    Value::List(variants),
                                    Value::List(conditions.into_iter().map(|c| c.convert_to_map()).collect()),
                                    Value::List(modifiers.iter().cloned().map(Value::String).collect()),
                                    Value::String(kind.to_string()),
                                ],
                                loc: name_loc,
                            }
                        }
                        "import" => {
                            self.next();

                            if self.token_is("NUMBER", "42") {
                                return Statement::Statement {
                                    keys: vec![
                                        Value::String("type".to_string()),
                                        Value::String("module_name".to_string()),
                                        Value::String("path".to_string()),
                                        Value::String("alias".to_string()),
                                        Value::String("named".to_string()),
                                        Value::String("modifiers".to_string()),
                                    ],
                                    values: vec![
                                        Value::String("IMPORT".to_string()),
                                        Value::String("42".to_string()),
                                        Value::String("".to_string()),
                                        Value::String("".to_string()),
                                        Value::Null,
                                        Value::List(modifiers.into_iter().map(Value::String).collect()),
                                    ],
                                    loc: self.get_loc(),
                                }
                            }

                            let mut named_imports = vec![];
                            let mut is_named_import = false;

                            if self.token_is("SEPARATOR", "(") {
                                is_named_import = true;
                                self.next();

                                loop {
                                    let ident = self.token().cloned().unwrap_or_else(|| {
                                        self.raise("SyntaxError", "Expected identifier in named import");
                                        DEFAULT_TOKEN.clone()
                                    });
                                    if ident.0 != "IDENTIFIER" {
                                        self.raise("SyntaxError", "Expected identifier in named import");
                                        return Statement::Null;
                                    }
                                    self.next();

                                    let mut alias = None;
                                    if self.token_is("IDENTIFIER", "as") {
                                        self.next();
                                        let alias_token = self.token().cloned().unwrap_or_else(|| {
                                            self.raise("SyntaxError", "Expected alias after 'as'");
                                            DEFAULT_TOKEN.clone()
                                        });
                                        if alias_token.0 != "IDENTIFIER" {
                                            self.raise("SyntaxError", "Expected identifier after 'as'");
                                            return Statement::Null;
                                        }
                                        alias = Some(alias_token.1);
                                        self.next();
                                    }

                                    named_imports.push((ident.1, alias));

                                    if self.token_is("SEPARATOR", ",") {
                                        self.next();
                                    } else {
                                        break;
                                    }
                                }

                                if !self.token_is("SEPARATOR", ")") {
                                    self.raise("SyntaxError", "Expected ')' after named import list");
                                    return Statement::Null;
                                }
                                self.next();
                            }

                            let first = self.token().cloned().unwrap_or_else(|| {
                                self.raise("SyntaxError", "Expected module name after 'import'");
                                DEFAULT_TOKEN.clone()
                            });

                            let module_name = if is_named_import {
                                String::new()
                            } else if first.0 == "IDENTIFIER" || first.0 == "STRING" {
                                let mut module_parts = vec![];

                                if first.0 == "IDENTIFIER" {
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
                                } else {
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
                                }
                            } else {
                                if !is_named_import {
                                    self.raise("SyntaxError", "Expected identifier or string after 'import'");
                                    return Statement::Null;
                                }
                                String::new()
                            };

                            let mut from_path: Option<Statement> = None;
                            let mut module_name_final = module_name;

                            if self.token_is("IDENTIFIER", "from") {
                                self.next();

                                if is_named_import {
                                    let first_mod = self.token().cloned().unwrap_or_else(|| {
                                        self.raise("SyntaxError", "Expected module name after 'from'");
                                        DEFAULT_TOKEN.clone()
                                    });

                                    module_name_final = if first_mod.0 == "IDENTIFIER" {
                                        let mut parts = vec![first_mod.1];
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
                                            parts.push(next_ident.1);
                                            self.next();
                                        }
                                        parts.join(".")
                                    } else if first_mod.0 == "STRING" {
                                        let raw = &first_mod.1;
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
                                        self.raise("SyntaxError", "Expected identifier or string after 'from'");
                                        return Statement::Null;
                                    };
                                }

                                if !self.token_is("IDENTIFIER", "as") && !self.token_is("EOF", "") {
                                    from_path = Some(self.parse_primary());
                                    if self.err.is_some() {
                                        return Statement::Null;
                                    }
                                }
                            } else if is_named_import {
                                self.raise("SyntaxError", "Expected 'from' after named import");
                                return Statement::Null;
                            }

                            let mut alias: Option<Token> = None;
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

                            Statement::Statement {
                                keys: vec![
                                    Value::String("type".to_string()),
                                    Value::String("module_name".to_string()),
                                    Value::String("path".to_string()),
                                    Value::String("alias".to_string()),
                                    Value::String("named".to_string()),
                                    Value::String("modifiers".to_string()),
                                ],
                                values: vec![
                                    Value::String("IMPORT".to_string()),
                                    Value::String(module_name_final),
                                    from_path.map_or(Value::Null, |p| p.convert_to_map()),
                                    alias.map_or(Value::Null, |a| Value::String(a.1)),
                                    if is_named_import {
                                        let converted = named_imports
                                            .into_iter()
                                            .map(|(name, alias)| {
                                                let mut keys = vec![Value::String("name".to_string())];
                                                let mut values = vec![Value::String(name)];

                                                if let Some(alias) = alias {
                                                    keys.push(Value::String("alias".to_string()));
                                                    values.push(Value::String(alias));
                                                }
                                                Value::Map { keys, values }
                                            })
                                            .collect();
                                        Value::List(converted)
                                    } else {
                                        Value::Null
                                    },
                                    Value::List(modifiers.into_iter().map(Value::String).collect()),
                                ],
                                loc: self.get_loc(),
                            }
                        }
                        _ => {
                            let name = self.token().cloned().map(|tok| tok.1).unwrap_or_default();
                            self.next();
                            if self.token_is("SEPARATOR", ":") {
                                self.next();
                                let type_token = self.token().cloned().unwrap_or(DEFAULT_TOKEN.clone());
                                if !(type_token.0 == "IDENTIFIER" || type_token.0 == "OPERATOR") {
                                    self.raise("SyntaxError", "Expected type after ':'");
                                    return Statement::Null;
                                }
                                let type_ = self.parse_type();
                                let (mut value, mut is_default) = (get_type_default_as_statement_from_statement(&type_).convert_to_map(), true);
                                if self.err.is_some() {
                                    return Statement::Null;
                                }
                                if self.token_is("OPERATOR", "=") {
                                    self.next();
                                    value = self.parse_expression().convert_to_map();
                                    is_default = false;
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
                                        Value::String("is_default".to_string()),
                                    ],
                                    values: vec![
                                        Value::String("VARIABLE_DECLARATION".to_string()),
                                        Value::String(name),
                                        type_.clone().convert_to_map(),
                                        value,
                                        Value::List(modifiers.into_iter().map(Value::String).collect()),
                                        Value::Boolean(is_default),
                                    ],
                                    loc: self.get_loc(),
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
                                    loc: self.get_loc(),
                                };
                            }
                        }
                    }
                }

                "IDENTIFIER" if token.1 == "export" => {
                    self.next();

                    let mut global_modifiers: Vec<String> = vec![];
                    while let Some(tok) = self.token() {
                        if tok.0 == "IDENTIFIER"
                            && ["public", "static", "non-static", "final", "mutable"].contains(&tok.1.as_str())
                        {
                            global_modifiers.push(tok.1.clone());
                            self.next();
                        } else {
                            break;
                        }
                    }

                    let mut names = vec![];
                    let mut modifiers_list = vec![];

                    if self.token_is("SEPARATOR", "(") {
                        self.next();

                        loop {
                            let mut item_modifiers = global_modifiers.clone();

                            while let Some(tok) = self.token() {
                                if tok.0 == "IDENTIFIER"
                                    && ["public", "static", "non-static", "final", "mutable"]
                                        .contains(&tok.1.as_str())
                                {
                                    item_modifiers.push(tok.1.clone());
                                    self.next();
                                } else {
                                    break;
                                }
                            }

                            let name_token = self.token().cloned().unwrap_or_else(|| {
                                self.raise("SyntaxError", "Expected identifier in tuple export");
                                DEFAULT_TOKEN.clone()
                            });
                            if name_token.0 != "IDENTIFIER" {
                                self.raise("SyntaxError", "Expected identifier in tuple export");
                                return Statement::Null;
                            }

                            names.push(name_token.1.clone());
                            modifiers_list.push(item_modifiers);
                            self.next();

                            if self.token_is("SEPARATOR", ")") {
                                break;
                            }
                            if !self.token_is("SEPARATOR", ",") {
                                self.raise("SyntaxError", "Expected ',' between tuple items");
                                return Statement::Null;
                            }
                            self.next();
                        }

                        self.next();
                    } else {
                        let mut item_modifiers = global_modifiers.clone();
                        while let Some(tok) = self.token() {
                            if tok.0 == "IDENTIFIER"
                                && ["public", "static", "non-static", "final", "mutable"]
                                    .contains(&tok.1.as_str())
                            {
                                item_modifiers.push(tok.1.clone());
                                self.next();
                            } else {
                                break;
                            }
                        }

                        let name_token = self.token().cloned().unwrap_or_else(|| {
                            self.raise("SyntaxError", "Expected identifier after 'export'");
                            DEFAULT_TOKEN.clone()
                        });
                        if name_token.0 != "IDENTIFIER" {
                            self.raise("SyntaxError", "Expected identifier after 'export'");
                            return Statement::Null;
                        }

                        names.push(name_token.1.clone());
                        modifiers_list.push(item_modifiers);
                        self.next();
                    }

                    let mut aliases = names.clone();
                    if self.token_is("IDENTIFIER", "as") {
                        self.next();
                        if self.token_is("SEPARATOR", "(") {
                            self.next();
                            let mut alias_names = vec![];
                            for i in 0..names.len() {
                                let alias_token = self.token().cloned().unwrap_or_else(|| {
                                    self.raise("SyntaxError", "Expected identifier in alias tuple");
                                    DEFAULT_TOKEN.clone()
                                });
                                if alias_token.0 != "IDENTIFIER" {
                                    self.raise("SyntaxError", "Expected identifier in alias tuple");
                                    return Statement::Null;
                                }
                                alias_names.push(alias_token.1.clone());
                                self.next();

                                if i != names.len() - 1 {
                                    if !self.token_is("SEPARATOR", ",") {
                                        self.raise("SyntaxError", "Expected ',' between alias items");
                                        return Statement::Null;
                                    }
                                    self.next();
                                }
                            }

                            if !self.token_is("SEPARATOR", ")") {
                                self.raise("SyntaxError", "Expected ')' at end of alias tuple");
                                return Statement::Null;
                            }
                            self.next();

                            aliases = alias_names;
                        } else {
                            let alias_token = self.token().cloned().unwrap_or_else(|| {
                                self.raise("SyntaxError", "Expected identifier after 'as'");
                                DEFAULT_TOKEN.clone()
                            });
                            if alias_token.0 != "IDENTIFIER" {
                                self.raise("SyntaxError", "Expected identifier after 'as'");
                                return Statement::Null;
                            }
                            aliases = vec![alias_token.1.clone()];
                            self.next();
                        }
                    }

                    if aliases.len() != names.len() {
                        self.raise("SyntaxError", "Number of aliases must match number of names in export");
                        return Statement::Null;
                    }

                    Statement::Statement {
                        keys: vec![
                            Value::String("type".to_string()),
                            Value::String("names".to_string()),
                            Value::String("aliases".to_string()),
                            Value::String("modifiers".to_string()),
                        ],
                        values: vec![
                            Value::String("EXPORT".to_string()),
                            Value::List(names.into_iter().map(Value::String).collect()),
                            Value::List(aliases.into_iter().map(Value::String).collect()),
                            Value::List(
                                modifiers_list
                                    .into_iter()
                                    .map(|mods| Value::List(mods.into_iter().map(Value::String).collect()))
                                    .collect(),
                            ),
                        ],
                        loc: self.get_loc(),
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
                            loc: self.get_loc(),
                        };
                    }
                    let mut parenthesis = false;
                    if self.token_is("SEPARATOR", "(") {
                        self.next();
                        parenthesis = true;
                    }
                    let value = self.parse_expression();
                    if self.err.is_some() {
                        return Statement::Null;
                    }
                    if !self.token_is("SEPARATOR", ")") && parenthesis {
                        self.raise_with_help(
                            "SyntaxError",
                            "Expected ')' after expression in 'return (...)'",
                            "Maybe you forgot to add ')'?"
                        );
                        return Statement::Null;
                    } else if parenthesis {
                        self.next();
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
                        loc: self.get_loc(),
                    }
                }

                "IDENTIFIER" if token.1 == "defer" => {
                    self.next();
                    match self.token() {
                        Some(next_token) if next_token.0 == "SEPARATOR" && next_token.1 == "(" => {
                            self.next();
                            let expr = self.parse_expression();
                            if !self.token_is("SEPARATOR", ")") {
                                self.raise_with_help(
                                    "SyntaxError",
                                    "Expected ')' after expression in 'defer(...)'",
                                    "Maybe you forgot to add ')'?"
                                );
                                return Statement::Null;
                            }
                            self.next();
                            Statement::Statement {
                                keys: vec![
                                    Value::String("type".to_string()),
                                    Value::String("body".to_string()),
                                ],
                                values: vec![
                                    Value::String("DEFER".to_string()),
                                    Value::List(vec![expr.convert_to_map()]),
                                ],
                                loc: self.get_loc(),
                            }
                        }
                        Some(next_token) if next_token.0 == "SEPARATOR" && next_token.1 == ":" => {
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

                            if !self.token_is("IDENTIFIER", "end") {
                                self.raise_with_help(
                                    "SyntaxError",
                                    "Missing 'end' after 'defer:' block",
                                    "Did you forget 'end'?"
                                );
                                return Statement::Null;
                            }

                            self.next();
                            Statement::Statement {
                                keys: vec![
                                    Value::String("type".to_string()),
                                    Value::String("body".to_string()),
                                ],
                                values: vec![
                                    Value::String("DEFER".to_string()),
                                    Value::List(body.into_iter().map(|s| s.convert_to_map()).collect()),
                                ],
                                loc: self.get_loc(),
                            }
                        }
                        _ => {
                            self.raise(
                                "SyntaxError",
                                "Unexpected token after 'defer'. Expected ':' or '('.",
                            );
                            Statement::Null
                        }
                    }
                }

                "IDENTIFIER" if token.1 == "scope" => {
                    self.next();

                    let name_opt = match self.token() {
                        Some(token) if token.0 == "IDENTIFIER" => {
                            let name = token.1.clone();
                            self.next();
                            Some(name)
                        }
                        _ => None,
                    };

                    let locals_opt = if self.token_is("SEPARATOR", "(") {
                        self.next();
                        let mut locals = Vec::new();

                        while !self.token_is("SEPARATOR", ")") {
                            match self.token() {
                                Some(tok) if tok.0 == "IDENTIFIER" => {
                                    locals.push(tok.1.clone());
                                    self.next();
                                }
                                Some(tok) if tok.0 == "SEPARATOR" && tok.1 == "," => {
                                    self.next();
                                }
                                Some(tok) if tok.0 == "SEPARATOR" && tok.1 == ")" => break,
                                _ => {
                                    self.raise_with_help(
                                        "SyntaxError",
                                        "Expected identifier or ',' or ')' in locals list",
                                        "Maybe you forgot an identifier or comma inside the parentheses?"
                                    );
                                    return Statement::Null;
                                }
                            }
                        }

                        if !self.token_is("SEPARATOR", ")") {
                            self.raise_with_help(
                                "SyntaxError",
                                "Expected ')' after locals list",
                                "Maybe you forgot to close the parentheses?"
                            );
                            return Statement::Null;
                        }
                        self.next();

                        Some(locals)
                    } else {
                        Some(Vec::new())
                    };

                    if !self.token_is("SEPARATOR", ":") {
                        self.raise_with_help(
                            "SyntaxError",
                            "Expected ':' after 'scope' or 'scope <name>' or 'scope <name> (locals)'",
                            "Did you forget ':' after scope or after locals list?"
                        );
                        return Statement::Null;
                    }
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

                    if !self.token_is("IDENTIFIER", "end") {
                        self.raise_with_help(
                            "SyntaxError",
                            "Missing 'end' after 'scope' block",
                            "Did you forget to close the scope block with 'end'?"
                        );
                        return Statement::Null;
                    }
                    self.next();

                    let mut keys = vec![
                        Value::String("type".to_string()),
                        Value::String("body".to_string()),
                    ];
                    let mut values = vec![
                        Value::String("SCOPE".to_string()),
                        Value::List(body.into_iter().map(|s| s.convert_to_map()).collect()),
                    ];

                    if let Some(name) = name_opt {
                        keys.push(Value::String("name".to_string()));
                        values.push(Value::String(name));
                    }

                    let locals = locals_opt.unwrap();
                    keys.push(Value::String("locals".to_string()));
                    values.push(Value::List(locals.into_iter().map(Value::String).collect()));

                    Statement::Statement {
                        keys,
                        values,
                        loc: self.get_loc(),
                    }
                }

                "IDENTIFIER" if token.1 == "match" => {
                    self.next();
                    if !self.token_is("SEPARATOR", "(") {
                        self.raise_with_help(
                            "SyntaxError",
                            "Expected '(' after 'match'",
                            "Did you forget to add '('?"
                        );
                        return Statement::Null;
                    }
                    self.next();
                    let condition = self.parse_expression();
                    if self.err.is_some() {
                        return Statement::Null;
                    }
                    if !self.token_is("SEPARATOR", ")") {
                        self.raise_with_help(
                            "SyntaxError",
                            "Expected ')' after match condition",
                            "Did you forget to close the parentheses?"
                        );
                        return Statement::Null;
                    }
                    self.next();
                    if !self.token_is("SEPARATOR", ":") {
                        self.raise_with_help(
                            "SyntaxError",
                            "Expected ':' after match condition",
                            "Did you forget to add ':'?"
                        );
                        return Statement::Null;
                    }
                    self.next();
                
                    let mut cases = vec![];
                
                    loop {
                        match self.token().cloned() {
                            Some(Token(_, ref tok_val, _)) if tok_val == "end" => {
                                self.next();
                                break;
                            }
                    
                            Some(Token(_, ref tok_val, _)) => {
                                let pattern = if tok_val == "_" {
                                    self.next();
                                    None
                                } else {
                                    let pat = self.parse_operand();
                                    if self.err.is_some() {
                                        return Statement::Null;
                                    }
                                    Some(pat)
                                };
                    
                                let guard = if self.token_is("IDENTIFIER", "if") {
                                    self.next();
                                    if !self.token_is("SEPARATOR", "(") {
                                        self.raise_with_help(
                                            "SyntaxError",
                                            "Expected '(' after 'if' in case guard",
                                            "Did you forget to add '('?"
                                        );
                                        return Statement::Null;
                                    }
                                    self.next();
                                    let cond_expr = self.parse_expression();
                                    if self.err.is_some() {
                                        return Statement::Null;
                                    }
                                    if !self.token_is("SEPARATOR", ")") {
                                        self.raise_with_help(
                                            "SyntaxError",
                                            "Expected ')' after if condition",
                                            "Did you forget to close the parentheses?"
                                        );
                                        return Statement::Null;
                                    }
                                    self.next();
                                    Some(cond_expr)
                                } else {
                                    None
                                };
                    
                                if !self.token_is("SEPARATOR", ":") {
                                    self.raise_with_help(
                                        "SyntaxError",
                                        "Expected ':' after pattern (and optional if guard)",
                                        "Did you forget to add ':'?"
                                    );
                                    return Statement::Null;
                                }
                                self.next();
                    
                                let mut body = vec![];
                                loop {
                                    match self.token() {
                                        Some(&Token(ref t, ref v, _)) if t == "IDENTIFIER" && v == "end" => {
                                            self.next();
                                            break;
                                        }
                                        Some(_) => {
                                            let stmt = self.parse_expression();
                                            if self.err.is_some() {
                                                return Statement::Null;
                                            }
                                            body.push(stmt);
                                        }
                                        None => {
                                            self.raise("SyntaxError", "Unexpected end of input inside case body");
                                            return Statement::Null;
                                        }
                                    }
                                }
                    
                                cases.push((pattern, guard, body));
                            }
                    
                            None => {
                                self.raise("SyntaxError", "Unexpected end of input inside match block");
                                return Statement::Null;
                            }
                        }
                    }
                    if cases.is_empty() {
                        self.raise_with_help(
                            "SyntaxError",
                            "Match statement must have at least one case",
                            "Add '_'",
                        );
                        return Statement::Null;
                    }                    
                    Statement::Statement {
                        keys: vec![
                            Value::String("type".to_string()),
                            Value::String("condition".to_string()),
                            Value::String("cases".to_string()),
                        ],
                        values: vec![
                            Value::String("MATCH".to_string()),
                            condition.convert_to_map(),
                            Value::List(cases.into_iter().map(|(p, g, b)| {
                                let pattern_value = p.map_or(Value::Null, |pat| pat.convert_to_map());
                                let guard_value = g.map_or(Value::Null, |guard| guard.convert_to_map());
                                Value::Map {
                                    keys: vec![
                                        Value::String("pattern".to_string()),
                                        Value::String("guard".to_string()),
                                        Value::String("body".to_string()),
                                    ],
                                    values: vec![pattern_value, guard_value, Value::List(b.into_iter().map(|s| s.convert_to_map()).collect())],
                                }
                            }).collect()),
                        ],
                        loc: self.get_loc(),
                    }
                }

                "SEPARATOR" if token.1 == "(" => {
                    self.next();
                    let mut values = vec![];

                    if let Some(next_token) = self.token() {
                        if next_token.0 == "SEPARATOR" && next_token.1 == ")" {
                            self.next();
                            let tuple_expr = Statement::Statement {
                                keys: vec![
                                    Value::String("type".to_string()),
                                    Value::String("items".to_string()),
                                ],
                                values: vec![
                                    Value::String("TUPLE".to_string()),
                                    Value::List(vec![]),
                                ],
                                loc: self.get_loc(),
                            };

                            if let Some(assign_token) = self.token() {
                                if assign_token.0 == "OPERATOR" && (assign_token.1 == ":=" || assign_token.1 == "=") {
                                    self.next();
                                    let value = self.parse_expression();

                                    return Statement::Statement {
                                        keys: vec![
                                            Value::String("type".to_string()),
                                            Value::String("targets".to_string()),
                                            Value::String("value".to_string()),
                                        ],
                                        values: vec![
                                            Value::String("UNPACK_ASSIGN".to_string()),
                                            Value::List(vec![]),
                                            value.convert_to_map(),
                                        ],
                                        loc: self.get_loc(),
                                    };
                                }
                            }

                            return tuple_expr;
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
                            break;
                        }
                    }

                    self.check_for("SEPARATOR", ")");
                    self.next();

                    if let Some(assign_token) = self.token() {
                        if assign_token.0 == "OPERATOR" && (assign_token.1 == ":=") {
                            self.next();
                            let value = self.parse_expression();

                            if self.err.is_some() {
                                return Statement::Null;
                            }

                            let targets = values.into_iter().map(|v| {
                                let Statement::Statement { keys, values, .. } = &v else {
                                    self.raise("SyntaxError", "Expected a variable statement in unpack assignment");
                                    return Value::Null;
                                };

                                let mut var_type = None;
                                let mut name_value = None;

                                for (k, v) in keys.iter().zip(values.iter()) {
                                    if let Value::String(k_str) = k {
                                        match k_str.as_str() {
                                            "type" => {
                                                if let Value::String(v_str) = v {
                                                    var_type = Some(v_str.as_str());
                                                }
                                            }
                                            "name" => {
                                                name_value = Some(v.clone());
                                            }
                                            _ => {}
                                        }
                                    }
                                }

                                match var_type {
                                    Some("VARIABLE_DECLARATION") => v.convert_to_map(),
                                    Some("VARIABLE") => {
                                        let name_value = name_value.unwrap_or(Value::String("_".to_string()));

                                        Value::Map {
                                            keys: vec![
                                                Value::String("type".to_string()),
                                                Value::String("name".to_string()),
                                                Value::String("var_type".to_string()),
                                                Value::String("value".to_string()),
                                                Value::String("modifiers".to_string()),
                                                Value::String("is_default".to_string()),
                                            ],
                                            values: vec![
                                                Value::String("VARIABLE_DECLARATION".to_string()),
                                                name_value,
                                                Value::Map {
                                                    keys: vec![
                                                        Value::String("type".to_string()),
                                                        Value::String("type_kind".to_string()),
                                                        Value::String("value".to_string()),
                                                    ],
                                                    values: vec![
                                                        Value::String("TYPE".to_string()),
                                                        Value::String("simple".to_string()),
                                                        Value::String("auto".to_string()),
                                                    ],
                                                },
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
                                                Value::List(vec![]),
                                                Value::Boolean(true),
                                            ],
                                        }
                                    }
                                    _ => {
                                        self.raise("SyntaxError", "Expected a variable or declaration in unpack assignment");
                                        Value::Null
                                    }
                                }
                            });

                            return Statement::Statement {
                                keys: vec![
                                    Value::String("type".to_string()),
                                    Value::String("targets".to_string()),
                                    Value::String("value".to_string()),
                                ],
                                values: vec![
                                    Value::String("UNPACK_ASSIGN".to_string()),
                                    Value::List(targets.collect()),
                                    value.convert_to_map(),
                                ],
                                loc: self.get_loc(),
                            };
                        }
                    }

                    if is_tuple || values.len() != 1 {
                        Statement::Statement {
                            keys: vec![
                                Value::String("type".to_string()),
                                Value::String("items".to_string()),
                            ],
                            values: vec![
                                Value::String("TUPLE".to_string()),
                                Value::List(values.into_iter().map(|s| s.convert_to_map()).collect()),
                            ],
                            loc: self.get_loc(),
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
                        if next_tok.0 == "OPERATOR" && matches!(next_tok.1.as_str(), "+=" | "-=" | "*=" | "/=" | "%=" | "^=") {
                            return self.parse_compound_assignment();
                        }
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
                            if self.err.is_some() {
                                return Statement::Null;
                            }
                            let old_pos = self.pos.clone();
                            self.next();
                            let right = self.parse_expression();
                            if !self.token_is("SEPARATOR", ")") {
                                self.pos = old_pos;
                                self.err = None;
                                return left;
                            }
                            self.next();
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
                                loc: self.get_loc(),
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
        };
        
        self.parse_postfix(expr)
    }

    fn parse_compound_assignment(&mut self) -> Statement {
        let identifier = self.token().cloned().unwrap();
        let loc = self.get_loc();
    
        self.next();
        let operator = self.token().cloned().unwrap();
        self.next();
    
        let expr = self.parse_expression();
    
        Statement::Statement {
            keys: vec![
                Value::String("type".to_string()),
                Value::String("target".to_string()),
                Value::String("operator".to_string()),
                Value::String("value".to_string()),
            ],
            values: vec![
                Value::String("COMPOUND_ASSIGN".to_string()),
                Value::String(identifier.1),
                Value::String(operator.1),
                expr.convert_to_map(),
            ],
            loc
        }
    }    

    fn parse_single_type(&mut self) -> Option<(String, Statement)> {
        let mut is_ptr = false;
        let mut is_maybe = false;
    
        while self.token_is("OPERATOR", "&") || self.token_is("OPERATOR", "?") {
            if self.token_is("OPERATOR", "&") {
                if is_ptr {
                    self.raise("SyntaxError", "Duplicate '&' in type prefix");
                    return None;
                }
                is_ptr = true;
            } else {
                if is_maybe {
                    self.raise("SyntaxError", "Duplicate '?' in type prefix");
                    return None;
                }
                is_maybe = true;
            }
            self.next();
        }
    
        let token = self.token().cloned().unwrap_or(DEFAULT_TOKEN.clone());
        if token.0 != "IDENTIFIER" {
            self.raise("SyntaxError", "Expected type identifier");
            return None;
        }
        self.next();
    
        let name = format!(
            "{}{}{}",
            if is_ptr { "&" } else { "" },
            if is_maybe { "?" } else { "" },
            token.1
        );
    
        Some((
            name.clone(),
            Statement::Statement {
                keys: vec![
                    Value::String("type".to_string()),
                    Value::String("type_kind".to_string()),
                    Value::String("value".to_string()),
                ],
                values: vec![
                    Value::String("TYPE".to_string()),
                    Value::String("simple".to_string()),
                    Value::String(name),
                ],
                loc: self.get_loc(),
            },
        ))
    }    

    fn parse_type(& mut self) -> Statement {
        let (base_str, base_stmt) = match self.parse_single_type() {
            Some(v) => v,
            None => return Statement::Null,
        };
    
        if self.token_is("SEPARATOR", "(") {
            let to_type = Value::Map {
                keys: vec![
                    Value::String("type".to_string()),
                    Value::String("type_kind".to_string()),
                    Value::String("value".to_string()),
                ],
                values: vec![
                    Value::String("TYPE".to_string()),
                    Value::String("simple".to_string()),
                    Value::String(base_str.clone()),
                ],
            };
    
            self.next();
            let value = if self.token_is("SEPARATOR", ")") {
                get_type_default_as_statement(&base_str)
            } else {
                let v = self.parse_expression();
                if self.err.is_some() {
                    return Statement::Null;
                }
                v
            };
    
            if !self.token_is("SEPARATOR", ")") {
                self.raise("SyntaxError", "Expected ')' after type conversion value");
                return Statement::Null;
            }
            self.next();
    
            return Statement::Statement {
                keys: vec![
                    Value::String("type".to_string()),
                    Value::String("to".to_string()),
                    Value::String("value".to_string()),
                ],
                values: vec![
                    Value::String("TYPE_CONVERT".to_string()),
                    to_type,
                    value.convert_to_map(),
                ],
                loc: self.get_loc(),
            };
        }
    
        if self.token_is("SEPARATOR", "[") {
            fn is_variadic_type(s: &Statement) -> bool {
                if let Statement::Statement { keys, values, .. } = s {
                    for (k, v) in keys.iter().zip(values.iter()) {
                        if let Value::String(key_str) = k {
                            if key_str == "type_kind" {
                                if let Value::String(kind_str) = v {
                                    return kind_str == "variadic";
                                }
                            }
                        }
                    }
                }
                false
            }

            self.next();
            let mut elements = vec![];
            let mut has_variadic_any = false;
            let mut has_variadic_typed = None;
            let inner_loc = self.get_loc();

            loop {
                let (_, elem_type) = match self.parse_type() {
                    Statement::Null => return Statement::Null,
                    s => ("".to_string(), s),
                };

                if self.token_is("SEPARATOR", ";") {
                    self.next();

                    let count = if let Some(Token(kind, value, _)) = self.token().cloned() {
                        if kind == "NUMBER" {
                            match parse_usize_radix(&value) {
                                Some(n) => n,
                                None => {
                                    self.raise("SyntaxError", "Expected a usize number after ';'");
                                    return Statement::Null;
                                }
                            }
                        } else {
                            self.raise("SyntaxError", "Expected a number after ';'");
                            return Statement::Null;
                        }
                    } else {
                        self.raise("SyntaxError", "Expected a number after ';'");
                        return Statement::Null;
                    };
                    self.next();

                    let elem_map = elem_type.convert_to_map();
                    for _ in 0..count {
                        elements.push(elem_map.clone());
                    }
                } else if elem_type.get_type() == "TYPE" && is_variadic_type(&elem_type) {
                    if has_variadic_any || has_variadic_typed.is_some() {
                        self.raise("SyntaxError", "Multiple variadic markers are not allowed");
                        return Statement::Null;
                    }
                    if let Statement::Statement { values, .. } = &elem_type {
                        if let Some(Value::String(s)) = values.get(5) {
                            has_variadic_typed = Some(s.clone());
                        }
                    }
                    has_variadic_any = true;
                    elements.push(elem_type.convert_to_map());
                } else {
                    elements.push(elem_type.convert_to_map());
                }

                if self.token_is("SEPARATOR", ",") {
                    self.next();
                    continue;
                }

                if self.token_is("SEPARATOR", "]") {
                    break;
                }

                self.raise("SyntaxError", "Expected ',' or ']' after type");
                return Statement::Null;
            }

            self.next();

            if elements.is_empty() && !has_variadic_any && has_variadic_typed.is_none() {
                self.raise("SyntaxError", "Empty type annotation is not allowed");
                return Statement::Null;
            }

            if base_str == "function" || base_str == "generator" {
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
                }
                .convert_to_statement();

                if self.token_is("OPERATOR", "->") {
                    self.next();
                    return_type = self.parse_type();
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
                        Value::String(base_str.clone()),
                        Value::List(elements),
                        return_type.convert_to_map(),
                        Value::Boolean(has_variadic_any),
                        Value::String(has_variadic_typed.unwrap_or("any".to_string())),
                    ],
                    loc: inner_loc,
                };
            } else if !["map", "tuple", "list"].contains(&base_str.as_str()) {
                self.raise("SyntaxError", &format!("Type '{}' cannot be indexed", base_str));
                return Statement::Null;
            }

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
                    Value::List(elements),
                    Value::Boolean(has_variadic_any),
                    Value::String(has_variadic_typed.unwrap_or("any".to_string())),
                ],
                loc: inner_loc,
            };
        }

        let mut union_types = vec![];
        union_types.push(base_stmt.clone());
        
        while self.token_is("OPERATOR", "|") {
            self.next();
            let next_type = self.parse_type();
            if next_type == Statement::Null {
                return Statement::Null;
            }
        
            if let Statement::Statement { keys, values, .. } = &next_type {
                let is_union = keys.iter().zip(values.iter()).any(|(k, v)| {
                    if let Value::String(k_str) = k {
                        if k_str == "type_kind" {
                            if let Value::String(v_str) = v {
                                return v_str == "union";
                            }
                        }
                    }
                    false
                });
        
                if is_union {
                    if let Some(types_index) = keys.iter().position(|k| k == &Value::String("types".to_string())) {
                        if let Value::List(nested_types) = &values[types_index] {
                            for t in nested_types {
                                union_types.push(t.convert_to_statement());
                            }
                            continue;
                        }
                    }
                }
            }
        
            union_types.push(next_type);
        }
        
        if union_types.len() > 1 {
            return Statement::Statement {
                keys: vec![
                    Value::String("type".to_string()),
                    Value::String("type_kind".to_string()),
                    Value::String("types".to_string()),
                ],
                values: vec![
                    Value::String("TYPE".to_string()),
                    Value::String("union".to_string()),
                    Value::List(union_types.into_iter().map(|s| s.convert_to_map()).collect()),
                ],
                loc: self.get_loc(),
            };
        }
        
        base_stmt        
    }

    fn parse_variable(&mut self) -> Statement {
        let token = self.token().cloned().unwrap_or(DEFAULT_TOKEN.clone());
        let loc = self.get_loc();

        if token.0 == "IDENTIFIER" {
            let name = token.1.clone();
            self.next();
            if self.token_is("SEPARATOR", ":") {
                self.next();
                let type_token = self.token().cloned().unwrap_or(DEFAULT_TOKEN.clone());
                if !(type_token.0 == "IDENTIFIER" || type_token.0 == "OPERATOR") {
                    self.raise("SyntaxError", "Expected type after ':'");
                    return Statement::Null;
                }
                let type_ = self.parse_type();
                let (mut value, mut is_default) = (get_type_default_as_statement_from_statement(&type_).convert_to_map(), true);
                if self.err.is_some() {
                    return Statement::Null;
                }
                if self.token_is("OPERATOR", "=") {
                    self.next();
                    value = self.parse_expression().convert_to_map();
                    is_default = false;
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
                        Value::String("is_default".to_string()),
                    ],
                    values: vec![
                        Value::String("VARIABLE_DECLARATION".to_string()),
                        Value::String(name),
                        type_.clone().convert_to_map(),
                        value,
                        Value::List(vec![]),
                        Value::Boolean(is_default),
                    ],
                    loc
                };
            } else if self.token_is("OPERATOR", ":=") {
                let name = token.1.clone();
                self.next();
                let type_ = Statement::Statement {
                    keys: vec![
                        Value::String("type".to_string()),
                        Value::String("type_kind".to_string()),
                        Value::String("value".to_string()),
                    ],
                    values: vec![
                        Value::String("TYPE".to_string()),
                        Value::String("simple".to_string()),
                        Value::String("auto".to_string()),
                    ],
                    loc: loc.clone()
                };
                let value = self.parse_expression().convert_to_map();
                if self.err.is_some() {
                    return Statement::Null;
                }
                return Statement::Statement {
                    keys: vec![
                        Value::String("type".to_string()),
                        Value::String("name".to_string()),
                        Value::String("var_type".to_string()),
                        Value::String("value".to_string()),
                        Value::String("modifiers".to_string()),
                        Value::String("is_default".to_string()),
                    ],
                    values: vec![
                        Value::String("VARIABLE_DECLARATION".to_string()),
                        Value::String(name),
                        type_.clone().convert_to_map(),
                        value,
                        Value::List(vec![]),
                        Value::Boolean(false),
                    ],
                    loc
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
                    loc
                };
            }
        }

        self.raise("SyntaxError", &format!("Invalid syntax. '{}' was unexpected.", token.1));
        Statement::Null
    }

    fn parse_operand(&mut self) -> Statement {
        let token = self.token().cloned().unwrap_or_else(|| DEFAULT_TOKEN.clone());
        let loc = self.get_loc();
        let token_type = token.0.clone();
        let token_value = token.1.clone();        
        let next_token = self.peek();
        let next_token_type = next_token.map(|t| t.0.clone()).unwrap_or_else(|| "".to_string());
        let next_token_value = next_token.map(|t| t.1.clone()).unwrap_or_else(|| "".to_string());
        
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
                loc
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
                    loc
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
                    loc
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
                    loc
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
                    loc
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
                loc
            };
        };        
        self.raise("SyntaxError", &format!(
            "Invalid syntax. '{}' was unexpected.",
            token_value
        ));
        Statement::Null
    }

    fn parse_list(&mut self) -> Statement {
        let loc = self.get_loc();
    
        self.next();
    
        let mut elements = Vec::new();
        let mut found_semicolon = false;
    
        while let Some(token) = self.token() {
            if token.0 == "SEPARATOR" && token.1 == "]" {
                break;
            }
    
            if token.0 == "SEPARATOR" && (token.1 == ".." || token.1 == "...") {
                let pattern_reg = token.1 == "...";
                self.next();
    
                let end_expr = if let Some(next_token) = self.token() {
                    if next_token.0 == "SEPARATOR" && next_token.1 == ";" {
                        found_semicolon = true;
                        self.next();

                        if self.token_is("SEPARATOR", "]") {
                            self.next();
                            return Statement::Statement {
                                keys: vec![
                                    Value::String("type".to_string()),
                                    Value::String("iterable_type".to_string()),
                                    Value::String("seed".to_string()),
                                    Value::String("end".to_string()),
                                    Value::String("pattern_reg".to_string()),
                                    Value::String("range_mode".to_string()),
                                    Value::String("is_infinite".to_string()),
                                ],
                                values: vec![
                                    Value::String("ITERABLE".to_string()),
                                    Value::String("LIST_COMPLETION".to_string()),
                                    Value::List(elements),
                                    Value::Null,
                                    Value::Boolean(pattern_reg),
                                    Value::String("length".to_string()),
                                    Value::Boolean(true),
                                ],
                                loc,
                            };
                        }
    
                        let expr = self.parse_expression();
                        if self.err.is_some() {
                            return Statement::Null;
                        }
                        expr.convert_to_map()
                    } else {
                        let expr = self.parse_expression();
                        if self.err.is_some() {
                            return Statement::Null;
                        }
                        expr.convert_to_map()
                    }
                } else {
                    self.raise("UEFError", "Unexpected end of input after range operator");
                    return Statement::Null;
                };
    
                self.check_for("SEPARATOR", "]");
                self.next();
    
                return Statement::Statement {
                    keys: vec![
                        Value::String("type".to_string()),
                        Value::String("iterable_type".to_string()),
                        Value::String("seed".to_string()),
                        Value::String("end".to_string()),
                        Value::String("pattern_reg".to_string()),
                        Value::String("range_mode".to_string()),
                    ],
                    values: vec![
                        Value::String("ITERABLE".to_string()),
                        Value::String("LIST_COMPLETION".to_string()),
                        Value::List(elements),
                        end_expr,
                        Value::Boolean(pattern_reg),
                        Value::String(if found_semicolon { "length" } else { "value" }.to_string()),
                    ],
                    loc,
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
            loc,
        }
    }
    
    fn parse_function_call(&mut self) -> Statement {
        let token = match self.token() {
            Some(t) => t,
            None => {
                self.raise("SyntaxError", "Expected function name");
                return Statement::Null;
            }
        };
        let name = token.1.clone();
        if self.err.is_some() {
            return Statement::Null;
        }

        let loc = self.get_loc();

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
                loc
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
            loc
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
    
            if let Some(Token(t_type, t_val, _)) = &next_token {
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
    
            if let Some(Token(t_type, t_val, _)) = self.token() {
                if t_type == "SEPARATOR" && t_val == "," {
                    self.next();
                }
            }
        }
    
        (pos_args, named_args)
    }
}
