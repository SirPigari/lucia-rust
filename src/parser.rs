use crate::env::runtime::utils::{hex_to_ansi, unescape_string_literal, get_type_default_as_statement, get_type_default_as_statement_from_statement, get_precedence, parse_usize_radix, is_valid_token, supports_color, to_static};
use crate::env::runtime::value::Value;
use crate::env::runtime::errors::Error;
use crate::env::runtime::statements::{Statement, Node, MatchCase, ThrowNode,  alloc_loc, RangeModeType, IterableNode, };
use crate::env::runtime::types::{VALID_TYPES, Int, Float};
use crate::env::runtime::tokens::{Token, Location, DEFAULT_TOKEN};
use crate::env::runtime::internal_structs::{PathElement, EffectFlags};
use crate::env::runtime::utils::RESERVED_KEYWORDS;
use std::collections::HashMap;

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    statements: Vec<Statement>,
    err: Option<Error>,
    ignore_pipe_count: usize,
    parse_var_decl: bool,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            pos: 0,
            statements: vec![],
            err: None,
            ignore_pipe_count: 0,
            parse_var_decl: true,
        }
    }

    fn token(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    fn token_is(&mut self, kind: &str, value: &str) -> bool {
        self.token().map_or(false, |t| t.0 == kind && t.1 == value)
    }

    #[cold]
    #[track_caller]
    pub fn raise(&mut self, error_type: &str, msg: &str) -> Statement {
        let rust_loc = std::panic::Location::caller();
        self.err = Some(Error {
            error_type: error_type.to_string(),
            msg: msg.to_string(),
            help: None,
            loc: self.get_loc_caller(*rust_loc),
            ref_err: None,
        });
        Statement::Null
    }

    #[cold]
    #[track_caller]
    pub fn raise_with_help(&mut self, error_type: &str, msg: &str, help: &str) -> Statement {
        let rust_loc = std::panic::Location::caller();
        self.err = Some(Error {
            error_type: error_type.to_string(),
            msg: msg.to_string(),
            help: Some(help.to_string()),
            loc: self.get_loc_caller(*rust_loc),
            ref_err: None,
        });
        Statement::Null
    }

    #[cold]
    #[track_caller]
    pub fn raise_with_loc(&mut self, error_type: &str, msg: &str, loc: Location) -> Statement {
        let rust_loc = std::panic::Location::caller();
        self.err = Some(Error {
            error_type: error_type.to_string(),
            msg: msg.to_string(),
            help: None,
            loc: Some(loc.set_lucia_source_loc(format!("{}:{}:{}", rust_loc.file(), rust_loc.line(), rust_loc.column()))),
            ref_err: None,
        });
        Statement::Null
    }

    fn next(&mut self) -> Option<&Token> {
        self.pos += 1;
        self.token()
    }

    #[track_caller]
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

    fn peek(&self, offset: usize) -> Option<&Token> {
        self.tokens.get(self.pos + offset)
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

    pub fn get_loc_caller(&mut self, rust_loc: std::panic::Location) -> Option<Location> {
        if let Some(token) = self.token() {
            if token.2.is_some() {
                Some(token.2.clone().unwrap().set_lucia_source_loc(format!("{}:{}:{}", rust_loc.file(), rust_loc.line(), rust_loc.column())))
            } else if self.pos > 0 {
                match self.tokens.get(self.pos - 1).and_then(|t| t.2.clone()) {
                    Some(loc) => Some(loc.set_lucia_source_loc(format!("{}:{}:{}", rust_loc.file(), rust_loc.line(), rust_loc.column()))),
                    None => None,
                }
            } else {
                None
            }
        } else if self.pos > 0 {
            match self.tokens.get(self.pos - 1).and_then(|t| t.2.clone()) {
                Some(loc) => Some(loc.set_lucia_source_loc(format!("{}:{}:{}", rust_loc.file(), rust_loc.line(), rust_loc.column()))),
                None => None,
            }
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

                ("SEPARATOR", "{") => {
                    if expr.get_type() != "VARIABLE" {
                        return expr;
                    }
                    let saved_pos = self.pos;
                    self.next();

                    let is_struct_def = if self.token_is("SEPARATOR", "}") {
                        true
                    } else if let Some(Token(ttype, _, _)) = self.token() {
                        if ttype == "IDENTIFIER" {
                            self.next();
                            self.token_is("OPERATOR", "=") || self.token_is("SEPARATOR", ",") || self.token_is("SEPARATOR", "}")
                        } else if ttype == "BOOLEAN" {
                            self.next();
                            self.token_is("SEPARATOR", "}")
                        } else {
                            false
                        }
                    } else {
                        false
                    };
                    self.pos = saved_pos;
                    if !is_struct_def {
                        return expr;
                    }
                    let struct_name = expr.get_value("name").unwrap();
                    self.check_for("SEPARATOR", "{");
                    self.next();
                    match self.token() {
                        Some(Token(token_type, token_value, _)) if token_type == "BOOLEAN" && token_value == "null" => {
                            self.next();
                            self.check_for("SEPARATOR", "}");
                            self.next();
                            return Statement::Statement {
                                keys: vec![
                                    Value::String("type".to_string()),
                                    Value::String("name".to_string()),
                                    Value::String("fields".to_string()),
                                    Value::String("is_null".to_string()),
                                ],
                                values: vec![
                                    Value::String("STRUCT_CREATION".to_string()),
                                    struct_name,
                                    Value::Map {
                                        keys: vec![],
                                        values: vec![],
                                    },
                                    Value::Boolean(true),
                                ],
                                loc: self.get_loc(),
                            };
                        }
                        Some(Token(token_type, token_value, _)) if token_type == "BOOLEAN" && (token_value == "true" || token_value == "false") => {
                            self.raise_with_help(
                                "SyntaxError",
                                "Unexpected boolean value",
                                "If you want to zeroe the struct use 'null'",
                            );
                        }
                        _ => {}
                    }
                    if self.token_is("SEPARATOR", "}") {
                        self.next();
                        return Statement::Statement {
                            keys: vec![
                                Value::String("type".to_string()),
                                Value::String("name".to_string()),
                                Value::String("fields".to_string()),
                                Value::String("is_null".to_string())
                            ],
                            values: vec![
                                Value::String("STRUCT_CREATION".to_string()),
                                Value::String(struct_name.to_string()),
                                Value::Map {
                                    keys: vec![],
                                    values: vec![],
                                },
                            ],
                            loc: self.get_loc(),
                        };
                    }
                    let mut fields = std::collections::HashMap::new();
                    while !self.token_is("SEPARATOR", "}") {
                        let name = match self.token().cloned() {
                            Some(Token(ty, id, _)) => if ty == "IDENTIFIER" {
                                id
                            } else {
                                self.raise("SyntaxError", "Expected identifier for a struct field");
                                return Statement::Null;
                            }
                            _ => {
                                self.raise("SyntaxError", "Expected identifier for a struct field");
                                return Statement::Null;
                            }
                        };
                        if self.err.is_some() {
                            return Statement::Null;
                        }
                        self.next();
                        if self.token_is("SEPARATOR", ":") {
                            return self.raise_with_help("SyntaxError", "Unexpected ':' after field name", "Did you mean to use '='?");
                        }
                        let value;
                        if self.token_is("SEPARATOR", ",") || self.token_is("SEPARATOR", "}") {
                            value = Statement {
                                node: Node::Variable {
                                    name: name.clone(),
                                },
                                loc: alloc_loc(self.get_loc())
                            };
                        } else {
                            if !self.token_is("OPERATOR", "=") {
                                return self.raise("SyntaxError", "Expected '=' after field name");
                            }
                            self.next();
                            value = self.parse_expression();
                            if self.err.is_some() {
                                return Statement::Null;
                            }
                        }
                        fields.insert(name, value);
                        if self.token_is("SEPARATOR", ",") {
                            self.next();
                        }
                    }
                    self.check_for("SEPARATOR", "}");
                    self.next();

                    let keys: Vec<Value> = fields
                        .keys()
                        .map(|k| Value::String(k.clone()))
                        .collect();

                    let values: Vec<Value> = fields
                        .values()
                        .map(|v| v.clone().convert_to_map())
                        .collect();

                    return Statement::Statement {
                        keys: vec![
                            Value::String("type".to_string()),
                            Value::String("name".to_string()),
                            Value::String("fields".to_string()),
                            Value::String("is_null".to_string()),
                        ],
                        values: vec![
                            Value::String("STRUCT_CREATION".to_string()),
                            Value::String(struct_name.to_string()),
                            Value::Map {
                                keys,
                                values,
                            },
                            Value::Boolean(false),
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
        
                            expr = Statement {
                                node: Node::MethodCall {
                                    object: Box::new(expr),
                                    method_name: property_token.1,
                                    pos_args,
                                    named_args,
                                },
                                loc: alloc_loc(self.get_loc()),
                            };
                        } else {
                            expr = Statement {
                                node: Node::PropertyAccess {
                                    object: Box::new(expr),
                                    property_name: property_token.1,
                                },
                                loc: alloc_loc(self.get_loc()),
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
                        return Statement {
                            node: Node::If {
                                condition: Box::new(expr),
                                body,
                                else_body: vec![],
                            },
                            loc: alloc_loc(loc),
                        };
                    } else {
                        let then_body = self.parse_expression();
                        return Statement {
                            node: Node::If {
                                condition: Box::new(expr),
                                body: vec![then_body],
                                else_body: vec![],
                            },
                            loc: alloc_loc(loc),
                        };
                    }
                }

                // fucking shit
                ("IDENTIFIER", "where") => {
                    self.next();
                    let loc = self.get_loc();
                    let mut body: Vec<Statement> = Vec::new();
                    enum ThenOp {
                        Call(String, Value, Value),
                        Op(String, Statement)
                    }
                    let mut then_body: Vec<ThenOp> = Vec::new();
                    let mut variable_names: Vec<String> = vec![];
                    if self.token_is("SEPARATOR", ":") {
                        self.next();
                    }
                    while let Some(tok) = self.token() {
                        if tok.1 == "end" || tok.1 == "then" {
                            break;
                        }
                        let stmt = self.parse_expression();
                        if self.err.is_some() {
                            return Statement::Null;
                        }
                        match stmt.node.clone() {
                            Node::Assignment { left, right, operator} => {
                                let var_name = match left.node {
                                    Node::Variable { name, ..} => {
                                        name
                                    }
                                    _ => return self.raise("SyntaxError", "Expected variable on the left side of assignment in 'where' clause"),
                                };
                                variable_names.push(var_name.clone());
                                body.push(
                                    Statement {
                                        node: Node::VariableDeclaration {
                                            name: var_name,
                                            var_type: Box::new(Statement {
                                                node: Node::Type {
                                                    node: TypeNode::Simple {
                                                        base: "any".to_owned(),
                                                        ptr_level: 0,
                                                        is_maybe: false,
                                                    }
                                                },
                                                loc: alloc_loc(loc.clone()),
                                            }),
                                            val_stmt: Box::new(right),
                                            modifiers: vec![],
                                            is_default: false,
                                        },
                                        loc: alloc_loc(loc.clone()),
                                    }
                                )
                            }
                            Node::VariableDeclaration { name, ..} => {
                                variable_names.push(name);
                                body.push(stmt);
                            }
                            _ => return self.raise("SyntaxError", "Expected variable declaration in 'where' clause"),
                        }
                    }

                    if !self.token_is("IDENTIFIER", "end") && !self.token_is("IDENTIFIER", "then") {
                        self.raise_with_help("SyntaxError", "Expected 'end' after 'where' body", "Did you forget to add 'end'?");
                        return Statement::Null;
                    }
                    if self.token_is("IDENTIFIER", "end") {
                        self.next();
                    }
                    while self.token_is("IDENTIFIER", "then") || self.token_is("IDENTIFIER", "end") {
                        if self.token_is("IDENTIFIER", "end") {
                            self.next();
                            break;
                        }
                        self.next();

                        if let Some(tok) = self.token() && tok.0 == "OPERATOR" {
                            let op = tok.1.clone();
                            self.next();
                            let right = self.parse_primary();
                            if self.err.is_some() {
                                return Statement::Null;
                            }
                            then_body.push(ThenOp::Op(op, right));
                        } else {
                            let then_expr = self.parse_expression();
                            if self.err.is_some() {
                                return Statement::Null;
                            }

                            if then_expr.get_type().as_str() != "CALL" {
                                self.raise("SyntaxError", "Expected function call after 'then' in 'where' clause");
                                return Statement::Null;
                            }

                            let name = then_expr.get_value("name").unwrap_or_else(|| {
                                self.raise("SyntaxError", "Expected 'name' in function call after 'then' in 'where' clause");
                                Value::Null
                            });
                            let pos_args = then_expr.get_value("pos_arguments").unwrap_or(Value::List(vec![]));
                            let named_args = then_expr.get_value("named_arguments").unwrap_or(Value::Map {
                                keys: vec![],
                                values: vec![],
                            });

                            then_body.push(ThenOp::Call(name.to_string(), pos_args, named_args));
                        }
                    }

                    // expr where x = y + 1
                    // =>
                    // \ x := y + 1 res := expr forget (x) return res \
                    // expr where x = y + 1 then call(<args>) end
                    // =>
                    // \ x := y + 1 res := expr forget (x) return call(res, <args>) \
                    let mangled_res_name = format!("where_res_{}", self.pos);
                    let mut group_body = vec![];
                    group_body.extend(body.into_iter().map(|e| e.convert_to_map()));
                    group_body.push(
                        Statement {
                            node: Node::VariableDeclaration {
                                name: mangled_res_name.clone(),
                                var_type: Box::new(Statement {
                                    node: Node::Type {
                                        node: TypeNode::Simple {
                                            base: "any".to_owned(),
                                            ptr_level: 0,
                                            is_maybe: false,
                                        }
                                    },
                                    loc: alloc_loc(loc.clone()),
                                }),
                                val_stmt: Box::new(expr),
                                modifiers: vec![],
                                is_default: false,
                            },
                            loc: loc.clone(),
                        }.convert_to_map(),
                    );
                    for then_op in then_body {
                        group_body.push(
                            Statement::Statement {
                                keys: vec![
                                    Value::String("type".to_string()),
                                    Value::String("left".to_string()),
                                    Value::String("right".to_string()),
                                ],
                                values: vec![
                                    Value::String("ASSIGNMENT".to_string()),
                                    Statement {
                                        node: Node::Variable {
                                            name: mangled_res_name.clone(),
                                        },
                                        loc: alloc_loc(loc.clone()),
                                    }.convert_to_map(),
                                    match then_op {
                                        ThenOp::Call(function_name, pos_args, named_args) => {
                                            Statement {
                                                node: Node::MethodCall {
                                                    object: Box::new(Statement {
                                                        node: Node::Variable {
                                                            name: mangled_res_name.clone(),
                                                        },
                                                        loc: alloc_loc(loc.clone()),
                                                    }),
                                                    method_name: function_name,
                                                    pos_args,
                                                    named_args,
                                                },
                                                loc: alloc_loc(loc.clone()),
                                            }
                                        }
                                        ThenOp::Op(op, val) => {
                                            Statement {
                                                node: Node::Operation {
                                                    left: Box::new(Statement {
                                                        node: Node::Variable {
                                                            name: mangled_res_name.clone(),
                                                        },
                                                        loc: alloc_loc(loc.clone()),
                                                    }),
                                                    operator: op.to_owned(),
                                                    right: Box::new(val),
                                                },
                                                loc: alloc_loc(loc.clone()),
                                            }
                                        }
                                    }
                                ],
                                loc: alloc_loc(loc.clone()),
                            }.convert_to_map()
                        );
                    }
                    for var_name in variable_names {
                        group_body.push(
                            Statement {
                                node: Node::Forget {
                                    node: Box::new(Statement {
                                        node: Node::Variable {
                                            name: var_name,
                                        },
                                        loc: alloc_loc(loc.clone()),
                                    }),
                                },
                                loc: alloc_loc(loc.clone()),
                            }.convert_to_map()
                        );
                    }
                    group_body.push(
                        Statement {
                            node: Node::Return {
                                value: Box::new(Statement {
                                    node: Node::Variable {
                                        name: mangled_res_name.clone(),
                                    },
                                    loc: alloc_loc(loc.clone()),
                                }),
                            },
                            loc: alloc_loc(loc.clone()),
                        }.convert_to_map()
                    );
                    return Statement {
                        node: Node::Group { body: group_body },
                        loc: alloc_loc(loc),
                    };
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
                        return Statement {
                            node: Node::TryCatch {
                                body: vec![expr],
                                catch_body: Some(vec![otherwise]),
                                exception_vars: Some(vec!["_".to_string()]),
                            },
                            loc: alloc_loc(loc),
                        };
                    }

                    return Statement {
                        node: Node::TryCatch {
                            body: vec![expr],
                            catch_body: None,
                            exception_vars: None,
                        },
                        loc: alloc_loc(loc),
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

                        return Statement {
                            node: Node::TryCatch {
                                body: vec![expr],
                                catch_body: Some(vec![otherwise]),
                                exception_vars: Some(vec!["_".to_string()]),
                            },
                            loc: alloc_loc(loc),
                        };
                    }

                    return Statement {
                        node: Node::TryCatch {
                            body: vec![expr],
                            catch_body: None,
                            exception_vars: None,
                        },
                        loc: alloc_loc(loc),
                    };
                }

                ("OPERATOR", "!") => {
                    let mut fact_level: usize = 0;
                    while self.token_is("OPERATOR", "!") {
                        if fact_level == usize::MAX {
                            self.raise("OverflowError", "Maximum factorial level reached");
                            return Statement::Null;
                        }
                        fact_level += 1;
                        self.next();
                    }
                    expr = Statement {
                        node: Node::Statement {
                            left: Box::new(expr.clone()),
                            operator: "!".to_owned(),
                            right: Box::new(Statement {
                                node: Node::Number {
                                    value: fact_level.to_owned(),
                                },
                                loc: alloc_loc(self.get_loc()),
                            }),
                        },
                        loc: alloc_loc(self.get_loc()),
                    };
                }

                ("OPERATOR", op) if ["++", "--"].contains(&op) => {
                    let operator = match op {
                        "++" => "+",
                        "--" => "-",
                        _ => unreachable!(),
                    }.to_string();
                    let loc = alloc_loc(self.get_loc());
                    self.next();
                    expr = Statement {
                        node: Node::Assignment {
                            left: Box::new(expr.clone()),
                            right: Box::new(Statement {
                                node: Node::Statement {
                                    left: Box::new(expr.clone()),
                                    operator: operator.to_owned(),
                                    right: Box::new(Statement {
                                        node: Node::Number {
                                            value: "1".to_owned(),
                                        },
                                        loc: alloc_loc(loc.clone()),
                                    }),
                                },
                                loc: alloc_loc(loc.clone()),
                            }),
                        },
                        loc,
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

        if self.err.is_some() {
            return Statement::Null;
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
                    if !is_valid_token(&self.token().cloned()) {
                        let loc = self.get_loc().unwrap();
                        self.raise_with_loc("SyntaxError", "Unexpected end of input after '=>'", loc);
                        return Statement::Null;
                    }
                    let body_expr = self.parse_expression();
                    if self.err.is_some() {
                        return Statement::Null;
                    }

                    let params = match &expr.node {
                        Node::Iterable { node: IterableNode::Tuple { elements } } => {
                            Value::List(elements.iter().map(|e| e.convert_to_map()).collect())
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

                    let body = vec![body_expr];

                    expr = Statement {
                        node: Node::FunctionDeclaration {
                            name: "<lambda#{id}>".to_owned(),
                            pos_args,
                            named_args,
                            body,
                            modifiers: vec!["mutable".to_string()],
                            return_type: Box::new(Value::Map {  // TODO: convert to Statement
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
                            }),
                            effect_flags: EffectFlags::empty(),
                        },
                        loc: alloc_loc(self.get_loc()),
                    };
                }

                ("OPERATOR", "=") => {
                    self.next();
                    if !is_valid_token(&self.token().cloned()) {
                        let loc = self.get_loc().unwrap();
                        self.raise_with_loc("SyntaxError", "Unexpected end of input after '='", loc);
                        return Statement::Null;
                    }
                    let value = self.parse_expression();
                    if self.err.is_some() {
                        return Statement::Null;
                    }

                    let expr_type = expr.get_type();
                    if let Node::Iterable { node: IterableNode::Tuple { elements: targets } } = &expr.node {
                        return Statement {
                            node: Node::UnpackAssignment {
                                targets: targets,
                                stmt: Box::new(value),
                            },
                            loc: alloc_loc(self.get_loc()),
                        };
                    }

                    expr = Statement {
                        node: Node::Assignment {
                            left: Box::new(expr),
                            right: Box::new(value),
                        },
                        loc: alloc_loc(self.get_loc()),
                    };
                }

                ("OPERATOR", "|>") => {
                    let mut next_exprs: Vec<Statement> = vec![];
                    while self.token_is("OPERATOR", "|>") {
                        self.next();
                        let next_expr = self.parse_primary();
                        if self.err.is_some() {
                            return Statement::Null;
                        }
                        next_exprs.push(next_expr);
                    }
                    if self.err.is_some() {
                        return Statement::Null;
                    }
                    expr = Statement {
                        node: Node::Pipeline {
                            initial_value: Box::new(expr),
                            arguments: next_exprs.clone(),
                        },
                        loc: alloc_loc(self.get_loc()),
                    };
                }
     
                ("OPERATOR", val) if ["+=", "-=", "*=", "/=", "%=", "^="].contains(&val) => {
                    let operator = match val {
                        "+=" => "+",
                        "-=" => "-",
                        "*=" => "*",
                        "/=" => "/",
                        "%=" => "%",
                        "^=" => "^",
                        _ => unreachable!(),
                    }.to_string();
                    self.next();
                    if !is_valid_token(&self.token().cloned()) {
                        let loc = self.get_loc().unwrap();
                        self.raise_with_loc("SyntaxError", &format!("Unexpected end of input after '{}'", val), loc);
                        return Statement::Null;
                    }
                    let right = self.parse_expression();
                    if self.err.is_some() {
                        return Statement::Null;
                    }
                    expr = Statement {
                        node: Node::Assignment {
                            left: Box::new(expr),
                            right: Box::new(Statement {
                                node: Node::Operation {
                                    left: Box::new(expr.clone()),
                                    operator,
                                    right: Box::new(right),
                                },
                                loc: alloc_loc(self.get_loc()),
                            }),
                        },
                        loc: alloc_loc(self.get_loc()),
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

            if ["=", "=>", "as", "++", "--", "-li", "->", "|>", ":="].contains(&op_str) {
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

            if ["+=", "-=", "*=", "/=", "^=", "%="].contains(&op_str) {
                break;
            }


            if tok_type != "OPERATOR" {
                break;
            }

            if op_str == "|" {
                if self.ignore_pipe_count > 0 {
                    break;
                }
                if self.pos == 0 {
                    break;
                }
                if let Some(prev_tok) = self.tokens.get(self.pos - 1) {
                    if prev_tok.0 == "SEPARATOR" {
                        if ["\\", "(", "[", "{"].contains(&prev_tok.1.as_str()) {
                            break;
                        }
                    }
                }
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

            lhs = Statement {
                node: Node::Operation {
                    left: Box::new(lhs),
                    operator: op_str.to_owned(),
                    right: Box::new(rhs),
                },
                loc: alloc_loc(self.get_loc()),
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
                        let before_pos = self.pos;
                        let expr = self.parse_expression();
                        if matches!(expr, Statement::Null) {
                            if self.err.is_some() {
                                return Statement::Null;
                            }
                            if self.pos == before_pos {
                                break;
                            }
                            continue;
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
                    Statement {
                        node: Node::Group { body: exprs },
                        loc: alloc_loc(self.get_loc()),
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
                    self.ignore_pipe_count = self.ignore_pipe_count.saturating_add(1);
                    let expr = self.parse_expression();
                    if self.ignore_pipe_count > 0 { self.ignore_pipe_count -= 1; }
                    if self.err.is_some() { return Statement::Null; }
                    self.check_for("OPERATOR", "|");
                    self.next();
                    Statement {
                        node: Node::Operation {
                            left: Box::new(expr),
                            operator: "abs".to_owned(),
                            right: Box::new(Statement::Null),
                        },
                        loc: alloc_loc(self.get_loc()),
                    }
                }

                "OPERATOR" if ["--", "++"].contains(&token.1.as_str()) => {
                    let operator = token.1.to_owned();
                    let loc = self.get_loc();
                    self.next();
                    if !is_valid_token(&self.token().cloned()) {
                        let loc = self.get_loc().unwrap();
                        self.raise_with_loc("SyntaxError", "Unexpected end of input after unary operator", loc);
                        return Statement::Null;
                    }
                    let operand = self.parse_expression();
                    if self.err.is_some() { return Statement::Null; }
                    Statement {
                        node: Node::PrefixOperator {
                            operator,
                            operand: Box::new(operand),
                        },
                        loc: alloc_loc(loc),
                    }
                }

                "OPERATOR" if ["+", "-", "!", "~", "nein", "isnt", "isn't", "not", "bnot"].contains(&token.1.as_str()) => {
                    let operator = token.1.clone();
                    self.next();
                    if !is_valid_token(&self.token().cloned()) {
                        let loc = self.get_loc().unwrap();
                        self.raise_with_loc("SyntaxError", "Unexpected end of input after unary operator", loc);
                        return Statement::Null;
                    }
                    let operand = if ["!"].contains(&operator.as_str()) {
                        self.parse_primary()
                    } else {
                        self.parse_expression()
                    };
                    if self.err.is_some() { return Statement::Null; }
                    Statement {
                        node: Node::UnaryOperation {
                            operator,
                            operand: Box::new(operand),
                        },
                        loc: alloc_loc(self.get_loc()),
                    }
                }

                "OPERATOR" if token.1 == "&" || token.1 == "&&" => {
                    let mut ptr_level: usize = 0;
                    while self.token_is("OPERATOR", "&") || self.token_is("OPERATOR", "&&") {
                        if self.token_is("OPERATOR", "&") {
                            ptr_level += 1;
                        } else {
                            ptr_level += 2;
                        }
                        self.next();
                    }
                    let expr = self.parse_expression();
                    if self.err.is_some() { return Statement::Null; }
                    Statement::Statement {
                        keys: vec![
                            Value::String("type".to_string()),
                            Value::String("value".to_string()),
                            Value::String("ptr_level".to_string()),
                        ],
                        values: vec![
                            Value::String("POINTER_REF".to_string()),
                            expr.convert_to_map(),
                            Value::Int(Int::from(ptr_level)),
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

                    let mut keys = Vec::new();
                    let mut values = Vec::new();

                    while let Some(next_token) = self.token() {
                        if next_token.0 == "SEPARATOR" && next_token.1 == "}" {
                            self.next();
                            break;
                        }

                        let key = self.parse_expression();
                        if self.err.is_some() { return Statement::Null; }

                        match self.token() {
                            Some(("SEPARATOR", ":")) => self.next(),
                            Some(_) => {
                                self.raise("SyntaxError", "Expected ':' after key in map");
                                return Statement::Null;
                            }
                            None => {
                                self.raise("SyntaxError", "Unexpected end of input in map");
                                return Statement::Null;
                            }
                        }

                        let value = self.parse_expression();
                        if self.err.is_some() { return Statement::Null; }

                        keys.push(key);
                        values.push(value);

                        match self.token() {
                            Some(Token("SEPARATOR", ",", _)) => self.next(),
                            Some(Token("SEPARATOR", "}", _)) => { self.next(); break; }
                            Some(_) => {
                                self.raise("SyntaxError", "Expected ',' or '}' in map");
                                return Statement::Null;
                            }
                            None => {
                                self.raise("SyntaxError", "Unexpected end of input in map");
                                return Statement::Null;
                            }
                        }
                    }

                    Statement {
                        node: Node::Map {
                            keys_stmts: keys,
                            values_stmts: values,
                        },
                        loc: alloc_loc(loc),
                    }
                }

                "SEPARATOR" | "IDENTIFIER" if token.1 == "..." || token.1 == "pass" => {
                    self.next();
                    Statement::Null
                }

                "IDENTIFIER" if token.1 == "for" => {
                    self.next();

                    let loc = self.get_loc();

                    if self.token_is("SEPARATOR", "else") {
                        self.raise("SyntaxError", "Unexpected 'else' after 'for'");
                        return Statement::Null;
                    }

                    if self.token_is("IDENTIFIER", "fun") {
                        self.raise_with_help("SyntaxError", "Unexpected keyword 'fun' after 'for'", "Yes, for fun.");
                        return Statement::Null;
                    }

                    let mut parentheses = false;
                    if self.token_is("SEPARATOR", "(") {
                        parentheses = true;
                        self.next();
                    }

                    let variable: PathElement = match self.parse_path() {
                        Some(var) => var,
                        None => {
                            self.raise("SyntaxError", "Expected variable or path after 'for'");
                            return Statement::Null;
                        }
                    };

                    if self.err.is_some() {
                        return Statement::Null;
                    }

                    if self.token_is("OPERATOR", "in") {
                        self.next();
                    } else if self.token_is("SEPARATOR", ":") {
                        self.next();
                        let mut functions: Vec<Statement> = Vec::new();
                        while !self.token_is("IDENTIFIER", "end") {
                            if !["public", "private", "static", "non-static", "final", "mutable", "fun"].contains(&self.token().cloned().unwrap_or(DEFAULT_TOKEN.clone()).1.as_str()) {
                                self.raise("SyntaxError", "Expected function definition in methods block");
                                return Statement::Null;
                            }
                            let func = self.parse_primary();
                            if self.err.is_some() {
                                return Statement::Null;
                            }
                            if !matches!(func.node, Node::FunctionDeclaration { .. }) {
                                if !matches!(func, Statement::VariableDeclaration { .. }) {
                                    self.raise_with_help("SyntaxError", "Expected function definition in methods block", "Did you mean to put this into the struct definition?");
                                } else {
                                    self.raise("SyntaxError", "Expected function definition in methods block");
                                }
                                return Statement::Null;
                            }
                            functions.push(func);
                            if self.token_is("SEPARATOR", ",") {
                                self.next();
                            }
                        }
                        self.next();
                        let var = match variable {
                            PathElement::Path { segments, args: _ } if segments.len() == 1 => Value::String(segments[0].clone()),
                            _ => {
                                self.raise("SyntaxError", &format!("'{}' is not a valid struct name", variable.display()));
                                return Statement::Null;
                            }
                        };
                        return Statement::Statement {
                            keys: vec![
                                Value::String("type".to_string()),
                                Value::String("struct_name".to_string()),
                                Value::String("methods".to_string()),
                            ],
                            values: vec![
                                Value::String("STRUCT_METHODS".to_string()),
                                var,
                                Value::List(functions.into_iter().map(|f| f.convert_to_map()).collect()),
                            ],
                            loc,
                        };
                    } else {
                        self.raise("SyntaxError", "Expected 'in' after 'for' variable or ':' for methods implementation for a struct");
                        return Statement::Null;
                    }

                    self.parse_var_decl = false;
                    let iterable = self.parse_expression();
                    self.parse_var_decl = true;
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

                    Statement {
                        node: Node::If {
                            iterable,
                            body,
                            variable,
                        },
                        loc: alloc_loc(self.get_loc()),
                    }
                }

                "IDENTIFIER" if token.1 == "while" => {
                    self.next();
                    let condition = self.parse_expression();
                    if self.err.is_some() {
                        return Statement::Null;
                    }
                    if self.token_is("IDENTIFIER", "end") {
                        return Statement {
                            node: Node::While {
                                condition: Box::new(condition),
                                body: vec![],
                            },
                            loc: alloc_loc(self.get_loc()),
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

                    Statement {
                        node: Node::While {
                            condition: Box::new(condition),
                            body,
                        },
                        loc: alloc_loc(self.get_loc()),
                    }
                }

                "IDENTIFIER" if token.1 == "continue" => {
                    self.next();
                    Statement {
                        node: Node::Continue,
                        loc: alloc_loc(self.get_loc()),
                    }
                }

                "IDENTIFIER" if token.1 == "break" => {
                    self.next();
                    Statement::Statement {
                        node: Node::Break,
                        loc: alloc_loc(self.get_loc()),
                    }
                }

                "IDENTIFIER" if token.1 == "forget" => {
                    self.next();
                    let value = self.parse_expression();
                    if self.err.is_some() {
                        return Statement::Null;
                    }
                    Statement {
                        node: Node::Forget {
                            value: Box::new(value),
                        },
                        loc: alloc_loc(self.get_loc()),
                    }
                }

                "IDENTIFIER" if token.1 == "throw" => {
                    self.next();

                    if self.token_is("SEPARATOR", "(") {
                        let loc = self.get_loc();
                        let val = self.parse_expression();
                        if self.err.is_some() {
                            return Statement::Null;
                        }
                        Statement {
                            node: Node::Throw {
                                node: ThrowNode::Tuple(val),
                            },
                            loc: alloc_loc(loc),
                        }
                    } else {
                        let message = self.parse_expression();
                        if self.err.is_some() {
                            return Statement::Null;
                        }
                        let mut from = None;
                        if self.token_is("IDENTIFIER", "from") {
                            self.next();
                            from = Some(self.parse_expression());
                            if self.err.is_some() {
                                return Statement::Null;
                            }
                        }
                        Statement {
                            node: Node::Throw {
                                node: ThrowNode::Message {
                                    message,
                                    from,
                                },
                            },
                            loc: alloc_loc(self.get_loc()),
                        }
                    }
                }

                "IDENTIFIER" if token.1 == "if" => {
                    self.next();
                    if self.token_is("SEPARATOR", "else") {
                        self.raise("SyntaxError", "Unexpected 'else' after 'if'");
                        return Statement::Null;
                    }
                    self.parse_var_decl = false;
                    let condition = self.parse_expression();
                    self.parse_var_decl = true;
                    if self.err.is_some() {
                        return Statement::Null;
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
                        Statement {
                            node: Node::If {
                                condition: Box::new(condition),
                                body: vec![then_expr],
                                else_body: match else_expr {
                                    Some(e) => Some(vec![e]),
                                    None => None,
                                },
                            },
                            loc: alloc_loc(self.get_loc()),
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
                        Statement {
                            node: Node::If {
                                condition: Box::new(condition),
                                body,
                                else_body,
                            },
                            loc: alloc_loc(self.get_loc()),
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

                    Statement {
                        node: Node::TryCatch {
                            body,
                            catch_body: if has_catch { Some(catch_body) } else { None },
                            exception_vars: if exception_vars.is_empty() { None } else { Some(exception_vars.clone()) },
                        },
                        loc: alloc_loc(loc),
                    }
                }

                "IDENTIFIER" if ["struct", "enum"].contains(&token.1.as_str()) && !([Some(":="), Some(":"), Some("="), Some(","), Some("\0"), Some(""), Some(" ")].contains(&self.peek(1).map(|tok| tok.1.as_str()))) => {
                    let next_name = self.peek(1)
                        .and_then(|tok| if tok.0.as_str() == "IDENTIFIER" { Some(tok.1.as_str()) } else { None })
                        .unwrap_or("Name");

                    return self.raise_with_help(
                        "SyntaxError",
                        &format!("Unexpected keyword '{}'", token.1),
                        &format!("Did you mean to use 'typedef {} {} = {{...}}'?", token.1, next_name),
                    );
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
                            let is_function = x == "fun";
                            if RESERVED_KEYWORDS.contains(&name.as_str()) {
                                self.raise("SyntaxError", &format!("'{}' is a reserved keyword and cannot be used as a {} name", name, if is_function { "function" } else { "generator" }));
                                return Statement::Null;
                            }
                            if name.is_empty() {
                                self.raise("SyntaxError", &format!("Expected {} name after '{}'", if is_function { "function" } else { "generator" }, x));
                                return Statement::Null;
                            }
                            self.next();
                            self.check_for("SEPARATOR", "(");
                            self.next();
                            let mut pos_args = vec![];
                            let mut named_args = vec![];
                            let mut effect_flags: EffectFlags = EffectFlags::empty();

                            while let Some(mut current_tok) = self.token().cloned() {
                                let mut param_modifiers = vec![];
                                let mut is_variadic = false;

                                while current_tok.0 == "IDENTIFIER" &&
                                    ["mutable", "final", "static", "non-static"].contains(&current_tok.1.as_str())
                                {
                                    param_modifiers.push(current_tok.1.clone());
                                    self.next();
                                    if let Some(tok) = self.token() {
                                        current_tok = tok.clone();
                                    } else { break; }
                                }

                                if self.token_is("OPERATOR", "*") {
                                    is_variadic = true;
                                    self.next();
                                    if let Some(tok) = self.token() {
                                        current_tok = tok.clone();
                                        if current_tok.0 == "IDENTIFIER" &&
                                        ["mutable", "final", "static", "non-static"].contains(&current_tok.1.as_str())
                                        {
                                            param_modifiers.push(current_tok.1.clone());
                                            self.next();
                                            if let Some(tok2) = self.token() { current_tok = tok2.clone(); }
                                        }
                                    } else { self.raise("SyntaxError", "Expected identifier after '*'"); return Statement::Null; }
                                }

                                if current_tok.0 == "IDENTIFIER" {
                                    let arg_name = current_tok.1.clone();
                                    self.next();

                                    let mut arg_type = Statement {
                                        node: Node::Type {
                                            node: TypeNode::Simple {
                                                base: "any".to_string(),
                                                ptr_level: 0,
                                                is_maybe: false,
                                            }
                                        },
                                        loc: alloc_loc(self.get_loc()),
                                    };

                                    if self.token_is("SEPARATOR", ":") {
                                        self.next();
                                        let type_expr = self.parse_type();
                                        if self.err.is_some() { return Statement::Null; }
                                        arg_type = type_expr;
                                    }

                                    let modifiers_value = Value::List(param_modifiers.into_iter().map(Value::String).collect());

                                    if self.token_is("OPERATOR", "=") && !is_variadic {
                                        self.next();
                                        let def_val = self.parse_expression();
                                        if self.err.is_some() { return Statement::Null; }
                                        named_args.push((arg_name.clone(), Value::Map {
                                            keys: vec!["type".into(), "value".into(), "modifiers".into(), "variadic".into()],
                                            values: vec![arg_type.clone(), def_val.convert_to_map(), modifiers_value.clone(), Value::Boolean(is_variadic)],
                                        }));
                                    } else {
                                        pos_args.push(Statement::Statement {
                                            keys: vec!["name".into(), "type".into(), "variadic".into(), "modifiers".into()],
                                            values: vec![arg_name.into(), arg_type, Value::Boolean(is_variadic), modifiers_value],
                                            loc: self.get_loc(),
                                        });
                                    }

                                    if is_variadic { break; }

                                    if self.token_is("SEPARATOR", ",") { self.next(); }
                                } else if current_tok.0 == "SEPARATOR" && current_tok.1 == ")" {
                                    break;
                                } else {
                                    self.raise("SyntaxError", &format!("Unexpected token '{}'", current_tok.1));
                                    return Statement::Null;
                                }
                            }

                            self.check_for("SEPARATOR", ")");
                            self.next();

                            if is_function && self.token_is("OPERATOR", "?") {
                                effect_flags |= EffectFlags::MAY_FAIL;
                                self.next();
                            }

                            if self.err.is_some() {
                                return Statement::Null;
                            }

                            let mut return_type = Statement {
                                node: Node::Type {
                                    node: TypeNode::Simple {
                                        base: "any".to_string(),
                                        ptr_level: 0,
                                        is_maybe: false,
                                    }
                                },
                                loc: alloc_loc(self.get_loc())
                            };

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

                            let mut uses_the_goofy_ahh_arrow_just_to_separate_type_from_the_exlamation_mark_so_the_preprocessor_understands = false;
                            if self.token_is("OPERATOR", "<") && self.peek(1).map(|t| t.1.as_str()) == Some("!") {
                                uses_the_goofy_ahh_arrow_just_to_separate_type_from_the_exlamation_mark_so_the_preprocessor_understands = true;
                                self.next();
                            }
                            if self.token_is("OPERATOR", "!") {
                                self.next();
                                if self.token_is("SEPARATOR", "[") {
                                    while !self.token_is("SEPARATOR", "]") {
                                        self.next();
                                        let effect_token = self.token().cloned().unwrap_or(DEFAULT_TOKEN.clone());
                                        if effect_token.0 != "IDENTIFIER" {
                                            self.raise("SyntaxError", "Expected effect name in effect specification");
                                            return Statement::Null;
                                        }
                                        match effect_token.1.to_lowercase().as_str() {
                                            "pure" => { effect_flags |= EffectFlags::PURE; },
                                            "io" => { effect_flags |= EffectFlags::IO; },
                                            "state" => { effect_flags |= EffectFlags::STATE; },
                                            "unsafe" => { effect_flags |= EffectFlags::UNSAFE; },
                                            "throw" | "fail" => { effect_flags |= EffectFlags::MAY_FAIL; },
                                            "all" => { effect_flags |= EffectFlags::ALL; },
                                            "unknown" => { effect_flags |= EffectFlags::UNKNOWN; },
                                            _ => {
                                                self.raise_with_help(
                                                    "SyntaxError",
                                                    &format!("Unknown effect '{}'", effect_token.1),
                                                    "Valid effects are: PURE, IO, STATE, UNSAFE and FAIL",
                                                );
                                                return Statement::Null;
                                            }
                                        }
                                        self.next();
                                        if !(self.token_is("SEPARATOR", ",") || self.token_is("SEPARATOR", "]")) {
                                            self.raise("SyntaxError", "Expected ',' or ']' in effect specification");
                                            return Statement::Null;
                                        }
                                    }
                                    self.next();
                                    if uses_the_goofy_ahh_arrow_just_to_separate_type_from_the_exlamation_mark_so_the_preprocessor_understands {
                                        if !self.token_is("OPERATOR", ">") {
                                            self.raise_with_help(
                                                "SyntaxError",
                                                "Expected '>' after effect specification",
                                                "Did you forget to close the effect specification with '>'?",
                                            );
                                        }
                                        self.next();
                                    }
                                } else {
                                    if self.token_is("SEPARATOR", "(") {
                                        return self.raise_with_help(
                                            "SyntaxError",
                                            "Expected '[' after '!' for effect specification",
                                            &format!("Did you mean to use '-> ![...]' instead of '-> !(...)'?"),
                                        )
                                    }
                                    return self.raise_with_help(
                                        "SyntaxError",
                                        "Expected '[' after '!' for effect specification",
                                        &format!("Specify effects like '-> ![IO, STATE]', or use '-> ![PURE]' for no effects"),
                                    )
                                }
                            } else {
                                effect_flags |= EffectFlags::UNKNOWN;
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

                            return Statement {
                                node: if is_function {
                                    Node::FunctionDeclaration {
                                        pos_args,
                                        named_args,
                                        body,
                                        modifiers,
                                        return_type: Box::new(return_type),
                                        effect_flags,
                                    }
                                } else {
                                    Node::GeneratorDeclaration {
                                        pos_args,
                                        named_args,
                                        body,
                                        modifiers,
                                        return_type: Box::new(return_type),
                                        effect_flags,
                                    }
                                },
                                loc: alloc_loc(self.get_loc()),
                            };
                        }
                        "typedef" => {
                            self.next();

                            let mut kind = "alias".to_string();
                            if self.token_is("IDENTIFIER", "enum") {
                                kind = "enum".to_string();
                                self.next();
                            } else if self.token_is("IDENTIFIER", "struct") {
                                kind = "struct".to_string();
                                self.next();
                            }

                            let type_token = self.token().cloned().unwrap_or(DEFAULT_TOKEN.clone());
                            if type_token.0 != "IDENTIFIER" {
                                self.raise("SyntaxError", "Expected type name after 'typedef'");
                                return Statement::Null;
                            }
                            let name = type_token.1.clone();
                            if RESERVED_KEYWORDS.contains(&name.as_str()) {
                                self.raise("SyntaxError", &format!("'{}' is a reserved keyword and cannot be used as a type name", name));
                                return Statement::Null;
                            }
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
                                    generics.push(gen_token.1.clone());
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
                            let vars_start_loc = self.get_loc();
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

                            if kind != "alias" && !vars.is_empty() {
                                let help = if generics.is_empty() {
                                    "Did you mean to use '<>' for generics?"
                                } else {
                                    &format!("Did you mean to use 'typedef {}[{}]<{}> = ...' instead?", name, generics.join(", "), vars.join(", "))
                                };
                                self.raise_with_help("SyntaxError", "Variables not allowed in this context", help);
                                self.err.as_mut().unwrap().loc = vars_start_loc;
                                return Statement::Null;
                            }

                            if !self.token_is("OPERATOR", "=") && kind == "alias" {
                                self.raise("SyntaxError", "Expected '=' after type name");
                                return Statement::Null;
                            }
                            if self.token_is("OPERATOR", "=") {
                                self.next();
                            }

                            let mut body = Value::Null;
                            let mut variants = vec![];

                            if kind == "alias" {
                                let aliased_type = self.parse_type();
                                if self.err.is_some() {
                                    return Statement::Null;
                                }
                                body = aliased_type.convert_to_map();
                            } else if kind == "enum" {
                                if !self.token_is("SEPARATOR", "{") {
                                    self.raise("SyntaxError", "Expected '{' in enum declaration");
                                    return Statement::Null;
                                }
                                self.next();

                                let mut discriminants_counter = 0;
                                while !self.token_is("SEPARATOR", "}") {
                                    let variant_token = self.token().cloned().unwrap_or(DEFAULT_TOKEN.clone());
                                    if variant_token.0 != "IDENTIFIER" {
                                        self.raise("SyntaxError", "Expected variant name in enum");
                                        return Statement::Null;
                                    }
                                    let variant_name = variant_token.1.clone();
                                    self.next();

                                    let mut var_type = Value::Null;
                                    let mut discriminant = Statement {
                                        node: Node::Number {
                                            value: discriminants_counter.to_owned()
                                        },
                                        loc: alloc_loc(self.get_loc()),
                                    }.convert_to_map();
                                    discriminants_counter += 1;

                                    if self.token_is("SEPARATOR", "(") {
                                        self.next();
                                        let mut elements = Vec::new();
                                        while !self.token_is("SEPARATOR", ")") {
                                            let elem = self.parse_type();
                                            if self.err.is_some() {
                                                return Statement::Null;
                                            }
                                            elements.push(elem);
                                            if self.token_is("SEPARATOR", ",") {
                                                self.next();
                                            }
                                        }
                                        if !self.token_is("SEPARATOR", ")") {
                                            self.raise("SyntaxError", "Expected ')' after tuple type elements");
                                            return Statement::Null;
                                        }
                                        self.next();
                                        if elements.len() == 1 {
                                            var_type = elements[0];
                                        } else {
                                            let mut generics = Vec::with_capacity(elements.len());
                                            for el in elements {
                                                if let Node::Type { node } = el.node {
                                                    generics.push(node);
                                                } else {
                                                    return self.raise("SyntaxError", "Expected type in generics")
                                                }
                                            }
                                            var_type =  Statement {
                                                node: Node::Type {
                                                    node: TypeNode::Generics {
                                                        base: "tuple".to_owned(),
                                                        generics_types: generics,
                                                    }
                                                },
                                                loc: alloc_loc(self.get_loc()),
                                            };
                                        }
                                    }

                                    if self.token_is("OPERATOR", "=") {
                                        self.next();
                                        let expr = self.parse_expression();
                                        if self.err.is_some() {
                                            return Statement::Null;
                                        }
                                        discriminant = expr.convert_to_map()
                                    }

                                    let mut keys = vec![Value::String("name".to_string()), Value::String("type".to_string())];
                                    let mut values = vec![Value::String(variant_name), var_type];

                                    keys.push(Value::String("discriminant".to_string()));
                                    values.push(discriminant);

                                    variants.push(Value::Map { keys, values });

                                    if self.token_is("SEPARATOR", ",") {
                                        self.next();
                                    } else if !self.token_is("SEPARATOR", "}") {
                                        self.raise("SyntaxError", "Expected ',' or '}' in enum declaration");
                                        return Statement::Null;
                                    }
                                }
                                self.next();

                                body = Value::List(variants);
                            } else if kind == "struct" {
                                if !self.token_is("SEPARATOR", "{") {
                                    self.raise("SyntaxError", "Expected '{' in struct declaration");
                                    return Statement::Null;
                                }
                                self.next();

                                let mut props_map = std::collections::HashMap::new();

                                while !self.token_is("SEPARATOR", "}") {
                                    let mut mods = vec![];
                                    while self.token().map_or(false, |t| t.0 == "IDENTIFIER" && ["public", "private", "static", "non-static", "final", "mutable"].contains(&t.1.as_str())) {
                                        mods.push(self.token().unwrap().1.clone());
                                        self.next();
                                    }

                                    let field_token = self.token().cloned().unwrap_or(DEFAULT_TOKEN.clone());
                                    if field_token.0 != "IDENTIFIER" {
                                        self.raise("SyntaxError", "Expected field name in struct");
                                        return Statement::Null;
                                    }
                                    let field_name = field_token.1.clone();
                                    self.next();

                                    if !self.token_is("SEPARATOR", ":") {
                                        self.raise("SyntaxError", "Expected ':' after field name in struct");
                                        return Statement::Null;
                                    }
                                    self.next();

                                    let field_type = self.parse_type();
                                    if self.err.is_some() {
                                        return Statement::Null;
                                    }

                                    props_map.insert(
                                        field_name.clone(),
                                        Value::Tuple(vec![
                                            field_type.convert_to_map(),
                                            Value::List(mods.into_iter().map(Value::String).collect())
                                        ])
                                    );

                                    if self.token_is("SEPARATOR", ",") {
                                        self.next();
                                    } else if !self.token_is("SEPARATOR", "}") {
                                        self.raise("SyntaxError", "Expected ',' or '}' in struct declaration");
                                        return Statement::Null;
                                    }
                                }
                                self.next();

                                body = Value::Map {
                                    keys: props_map.keys().cloned().map(Value::String).collect(),
                                    values: props_map.values().cloned().collect()
                                };
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
                                    if kind == "alias" {
                                        let condition = self.parse_expression();
                                        if self.err.is_some() {
                                            return Statement::Null;
                                        }
                                        conditions.push(condition.convert_to_map());
                                    } else {
                                        let ident_token = self.token().cloned().unwrap_or(DEFAULT_TOKEN.clone());
                                        if ident_token.0 != "IDENTIFIER" {
                                            self.raise("SyntaxError", "Expected identifier in where statement");
                                            return Statement::Null;
                                        }
                                        let name = ident_token.1.clone();
                                        self.next();

                                        if !self.token_is("SEPARATOR", ":") {
                                            self.raise("SyntaxError", "Expected ':' after identifier in where statement");
                                            return Statement::Null;
                                        }
                                        self.next();

                                        let type_ = self.parse_type();
                                        if self.err.is_some() {
                                            return Statement::Null;
                                        }

                                        conditions.push(Value::List(vec![
                                            Value::String(name),
                                            type_.convert_to_map(),
                                        ]));
                                    }

                                    if self.token_is("SEPARATOR", ",") {
                                        self.next();
                                    } else if !self.token_is("SEPARATOR", ")") {
                                        self.raise("SyntaxError", "Expected ',' or ')' in type conditions");
                                        return Statement::Null;
                                    }
                                }
                                self.next();
                            }

                            let conditions = Value::List(conditions);


                            let mut keys = vec![
                                Value::String("type".to_string()),
                                Value::String("name".to_string()),
                                Value::String("generics".to_string()),
                                Value::String("variables".to_string()),
                                Value::String("kind".to_string()),
                            ];
                            let mut values = vec![
                                Value::String("TYPE_DECLARATION".to_string()),
                                Value::String(name),
                                Value::List(generics.into_iter().map(Value::String).collect()),
                                Value::List(vars.into_iter().map(Value::String).collect()),
                                Value::String(kind.clone()),
                            ];

                            match kind.as_str() {
                                "alias" => {
                                    keys.push(Value::String("alias".to_string()));
                                    keys.push(Value::String("conditions".to_string()));
                                    values.push(body);
                                    values.push(conditions);
                                }
                                "enum" => {
                                    keys.push(Value::String("variants".to_string()));
                                    keys.push(Value::String("wheres".to_string()));
                                    values.push(body);
                                    values.push(conditions);
                                }
                                "struct" => {
                                    keys.push(Value::String("properties".to_string()));
                                    keys.push(Value::String("wheres".to_string()));
                                    values.push(body);
                                    values.push(conditions);
                                }
                                _ => {}
                            }

                            Statement::Statement {
                                keys,
                                values,
                                loc: name_loc,
                            }
                        }
                        "import" => {
                            self.next();

                            if self.token_is("NUMBER", "42") {
                                return Statement::Statement {
                                    node: Node::Import {
                                        name: "42".to_string(),
                                        alias: None,
                                        named_imports: vec![],
                                        modifiers: vec![],
                                        import_all: false,
                                        module_path_opt: None,
                                    },
                                    loc: alloc_loc(self.get_loc()),
                                }
                            }

                            let mut named_imports = vec![];
                            let mut is_named_import = false;
                            let mut import_all = false;

                            if self.token_is("SEPARATOR", "(") {
                                is_named_import = true;
                                self.next();

                                loop {
                                    let ident = self.token().cloned().unwrap_or_else(|| {
                                        self.raise("SyntaxError", "Expected identifier in named import");
                                        DEFAULT_TOKEN.clone()
                                    });
                                    if ident.0 == "OPERATOR" && ident.1 == "*" {
                                        import_all = true;
                                        self.next();
                                        break;
                                    }
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
                                    
                                    if self.token_is("IDENTIFIER", "from") {
                                        self.next();
                                        from_path = Some(self.parse_primary());
                                        if self.err.is_some() {
                                            return Statement::Null;
                                        }
                                    }
                                } else {
                                    if !is_named_import && !self.token_is("IDENTIFIER", "as") && !self.token_is("EOF", "") {
                                        from_path = Some(self.parse_primary());
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

                            Statement {
                                node: Node::Import {
                                    name: module_name_final,
                                    alias: alias.map(|a| a.1),
                                    named_imports: if is_named_import {
                                        let converted: Vec<Value> = named_imports
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
                                        Some(converted)
                                    } else {
                                        None
                                    },
                                    modifiers,
                                    import_all,
                                    module_path_opt: from_path.map(Box::new),
                                },
                                loc: alloc_loc(self.get_loc()),
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
                                let (mut value, mut is_default) = (get_type_default_as_statement_from_statement(&type_), true);
                                if self.err.is_some() {
                                    return Statement::Null;
                                }
                                if self.token_is("OPERATOR", "=") {
                                    self.next();
                                    value = self.parse_expression();
                                    is_default = false;
                                    if self.err.is_some() {
                                        return Statement::Null;
                                    }
                                }
                                return Statement {
                                    node: Node::VariableDeclaration {
                                        name,
                                        var_type: type_,
                                        value,
                                        modifiers,
                                        is_default,
                                    },
                                    loc: alloc_loc(self.get_loc()),
                                };
                            } else if self.token_is("OPERATOR", ":=") {
                                return self.raise_with_help("SyntaxError", "':=' cannot be used in variable declaration with modifiers.", &format!("Use '{}{}{}: {} ={} ...' instead.", if modifiers.is_empty() { "" } else { to_static(format!("{} ", modifiers.join(" "))) }, name, hex_to_ansi("#1CC58B", supports_color()), "auto", hex_to_ansi("#21B8DB", supports_color())));
                            } else {
                                return Statement {
                                    node: Node::Variable {
                                        name,
                                    },
                                    loc: alloc_loc(self.get_loc()),
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

                    Statement {
                        node: Node::Export {
                            names,
                            aliases,
                            modifiers_list,
                        },
                        loc: alloc_loc(self.get_loc()),
                    }
                }

                "IDENTIFIER" if token.1 == "return" => {
                    self.next();
                    if ["end"].contains(&self.token().map(|t| t.1.as_str()).unwrap_or("")) {
                        return Statement {
                            node: Node::Return {
                                value: Box::new(
                                    Statement {
                                        node: Node::Boolean {
                                            value: None,
                                        },
                                        loc: alloc_loc(self.get_loc()),
                                    }
                                ),
                            },
                            loc: alloc_loc(self.get_loc()),
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
                    Statement {
                        node: Node::Return {
                            value: Box::new(value),
                        },
                        loc: alloc_loc(self.get_loc()),
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
                            Statement {
                                node: Node::Defer {
                                    body: vec![expr],
                                },
                                loc: alloc_loc(self.get_loc()),
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
                            Statement {
                                node: Node::Defer { body },
                                loc: alloc_loc(self.get_loc()),
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

                    let mut is_local = false;

                    let name_opt = match self.token() {
                        Some(token) if token.0 == "IDENTIFIER" => {
                            let name = token.1.clone();
                            self.next();
                            Some(name)
                        }
                        _ => None,
                    };

                    let locals = if self.token_is("SEPARATOR", "(") {
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
                                Some(tok) if tok.0 == "OPERATOR" && tok.1 == "*" => {
                                    is_local = true;
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

                        locals
                    } else {
                        Vec::new()
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

                    Statement {
                        node: Node::Scope {
                            body,
                            name: name_opt,
                            locals,
                            is_local,
                        },
                        loc: alloc_loc(self.get_loc()),
                    }
                }

                "IDENTIFIER" if token.1 == "match" => {
                    self.next();

                    self.parse_var_decl = false;
                    let condition = self.parse_expression();
                    self.parse_var_decl = true;
                    if self.err.is_some() {
                        return Statement::Null;
                    }

                    if self.token_is("IDENTIFIER", "from") {
                        self.next();
                        let pat_expr = match self.parse_path() {
                            Some(p) => p,
                            None => {
                                return Statement::Null;
                            }
                        };
                        let guard = if self.token_is("IDENTIFIER", "if") {
                            self.next();
                            let cond_expr = self.parse_expression();
                            if self.err.is_some() {
                                return Statement::Null;
                            }
                            Some(cond_expr)
                        } else {
                            None
                        };
                        let cases: Vec<MatchCase> = vec![
                            MatchCase::Pattern {
                                pattern: pat_expr,
                                guard: guard,
                                body: vec![
                                    Statement {
                                        node: Node::Boolean {
                                            value: Some(true),
                                        },
                                        loc: alloc_loc(self.get_loc()),
                                    }
                                ],
                            },
                            Value::Pattern {
                                pattern: PathElement::Path {
                                    segments: Vec::new("_"),
                                    args: Vec::new(),
                                },
                                guard: None,
                                body: vec![
                                    Statement {
                                        node: Node::Boolean {
                                            value: Some(false),
                                        },
                                        loc: alloc_loc(self.get_loc()),
                                    }
                                ]
                            }
                        ];
                        Statement {
                            node: Node::Match {
                                value: condition,
                                cases: cases,
                            },
                            loc: alloc_loc(self.get_loc()),
                        }
                    } else if self.token_is("SEPARATOR", ":") {
                        self.next();

                        let mut cases = vec![];

                        loop {
                            if !is_valid_token(&self.token().cloned()) {
                                self.raise("SyntaxError", "Expected 'end' or a valid case body");
                                return Statement::Null;
                            }
                            match self.token().cloned() {
                                Some(Token(_, ref tok_val, _)) if tok_val == "end" => {
                                    self.next();
                                    break;
                                }

                                Some(Token(..)) => {
                                    let (style, guard);
                                    let mut patterns_literal = vec![];
                                    let mut pattern = None;

                                    let saved_pos = self.pos;
                                    while !(self.token_is("SEPARATOR", ":") || self.token_is("OPERATOR", "->") || self.token_is("IDENTIFIER", "if")) {
                                        self.next();
                                    }
                                    match self.token().cloned() {
                                        Some(Token(_, ref tok_val, _)) if tok_val == "->" || tok_val == "if" => {
                                            self.pos = saved_pos;
                                            style = "pattern".to_string();
                                        }
                                        Some(Token(_, ref tok_val, _)) if tok_val == ":" => {
                                            self.pos = saved_pos;
                                            style = "literal".to_string();
                                        }
                                        _ => {
                                            self.pos = saved_pos;
                                            self.raise(
                                                "SyntaxError",
                                                "Expected ':', '->' or 'if' in match case",
                                            );
                                            return Statement::Null;
                                        }
                                    }

                                    if &style == "pattern" {
                                        let pat_expr = match self.parse_path() {
                                            Some(p) => p,
                                            None => {
                                                return Statement::Null;
                                            }
                                        };

                                        guard = if self.token_is("IDENTIFIER", "if") {
                                            self.next();
                                            let cond_expr = self.parse_expression();
                                            if self.err.is_some() {
                                                return Statement::Null;
                                            }
                                            Some(cond_expr)
                                        } else {
                                            None
                                        };

                                        if !self.token_is("OPERATOR", "->") {
                                            self.raise(
                                                "SyntaxError",
                                                "Expected '->' after pattern (and optional if guard)",
                                            );
                                            return Statement::Null;
                                        }
                                        self.next();

                                        pattern = Some(pat_expr);
                                    } else {
                                        let tok_val = self.token().unwrap().1.clone();
                                        patterns_literal = if tok_val == "_" {
                                            self.next();
                                            return self.raise_with_help("SyntaxError", "'_' is not allowed in 'match' literal case", "Did you mean to use pattern case? ('->' instead of ':')");
                                        } else {
                                            let mut pats = vec![];
                                            while !self.token_is("SEPARATOR", ":") {
                                                self.parse_var_decl = false;
                                                let p = self.parse_operand();
                                                self.parse_var_decl = true;
                                                if self.err.is_some() {
                                                    return Statement::Null;
                                                }
                                                pats.push(p.convert_to_map());
                                                if self.token_is("OPERATOR", "|") {
                                                    self.next();
                                                } else {
                                                    break;
                                                }
                                            };
                                            pats
                                        };

                                        if !self.token_is("SEPARATOR", ":") {
                                            self.raise(
                                                "SyntaxError",
                                                "Expected ':' after literal pattern",
                                            );
                                            return Statement::Null;
                                        }
                                        self.next();

                                        guard = None;
                                    }

                                    let mut body = vec![];
                                    loop {
                                        match self.token() {
                                            Some(&Token(ref t, ref v, _)) if t == "IDENTIFIER" && v == "end" => {
                                                self.next();
                                                break;
                                            }
                                            Some(&Token(ref t, ref v, _)) if t == "SEPARATOR" && v == "," && body.len() == 1 => {
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

                                    let case_map = if style == "literal" {
                                        MatchCase::Literal {
                                            patterns: patterns_literal,
                                            body
                                        }
                                    } else {
                                        MatchCase::Pattern {
                                            pattern: pattern.take().unwrap(),
                                            guard,
                                            body,
                                        }
                                    };

                                    cases.push(case_map);
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
                                "Add '_ -> null'",
                            );
                            return Statement::Null;
                        }

                        Statement {
                            node: Node::Match {
                                value: condition,
                                cases: cases,
                            },
                            loc: alloc_loc(self.get_loc()),
                        }
                    } else {
                        self.raise(
                            "SyntaxError",
                            "Expected ':' after match condition",
                        );
                        return Statement::Null;
                    }
                }

                "IDENTIFIER" if token.1 == "impl" => {
                    self.parse_impl()
                }

                "SEPARATOR" if token.1 == "(" => {
                    self.next();
                    let mut values = vec![];

                    if let Some(next_token) = self.token() {
                        if next_token.0 == "SEPARATOR" && next_token.1 == ")" {
                            self.next();
                            let tuple_expr = Statement {
                                node: Node::Iterable {
                                    node: IterableNode::Tuple {
                                        elements: vec![],
                                    }
                                },
                                loc: alloc_loc(self.get_loc())
                            };

                            if let Some(assign_token) = self.token() {
                                if assign_token.0 == "OPERATOR" && (assign_token.1 == ":=" || assign_token.1 == "=") {
                                    self.next();
                                    if !is_valid_token(&self.token().cloned()) {
                                        let loc = self.get_loc().unwrap();
                                        self.raise_with_loc("SyntaxError", "Unexpected end of input after ':='", loc);
                                        return Statement::Null;
                                    }
                                    let value = self.parse_expression();

                                    return Statement {
                                        node: Node::UnpackAssignment {
                                            targets: vec![],
                                            stmt: Box::new(value),
                                        },
                                        loc: alloc_loc(self.get_loc()),
                                    };
                                }
                            }

                            return tuple_expr;
                        }
                    }

                    self.parse_var_decl = true;
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
                            if !is_valid_token(&self.token().cloned()) {
                                let loc = self.get_loc().unwrap();
                                self.raise_with_loc("SyntaxError", "Unexpected end of input after '='", loc);
                                return Statement::Null;
                            }
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

                                        Statement {
                                            node: Node::VariableDeclaration {
                                                name: match &name_value {
                                                    Value::String(s) => s.clone(),
                                                    _ => {
                                                        self.raise("SyntaxError", "Variable name must be a string");
                                                        "_".to_string()
                                                    }
                                                },
                                                var_type: Statement {
                                                    node: Node::Type {
                                                        type_node: Box::new(TypeNode::Simple {
                                                            base: "auto".to_owned(),
                                                            ptr_level: 0,
                                                            is_maybe: false,
                                                        }),
                                                    },
                                                    loc: alloc_loc(self.get_loc()),
                                                },
                                                value: Box::new(Statement {
                                                    node: Node::Boolean {
                                                        value: None,
                                                    },
                                                    loc: alloc_loc(self.get_loc()),
                                                }),
                                                modifiers: vec![],
                                                is_default: true,
                                            },
                                            loc: alloc_loc(self.get_loc()),
                                        }
                                    }
                                    _ => {
                                        self.raise("SyntaxError", "Expected a variable or declaration in unpack assignment");
                                        Value::Null
                                    }
                                }
                            });

                            return Statement {
                                node: Node::UnpackAssignment {
                                    targets: targets.collect(),
                                    stmt: Box::new(value),
                                },
                                loc: alloc_loc(self.get_loc()),
                            };
                        }
                    }

                    if is_tuple || values.len() != 1 {
                        Statement {
                            node: Node::Iterable {
                                node: IterableNode::Tuple {
                                    elements: values,
                                }
                            },
                            loc: alloc_loc(self.get_loc()),
                        }
                    } else {
                        values.remove(0)
                    }
                }

                "SEPARATOR" if token.1 == "[" => {
                    self.parse_list()
                }

                "IDENTIFIER" => {
                    let next_token = self.peek(1).cloned();
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
                    let next_token = self.peek(1).cloned();

                    if let Some(next_tok) = next_token {
                        if next_tok.0 == "SEPARATOR" && next_tok.1 == "(" {
                            if token.1.contains('.') {
                                if let Some(inner) = self.peek(2).cloned() {
                                    let inner_type = inner.0;
                                    let inner_value = inner.1;
                                    let third_tok = self.peek(3).cloned();

                                    if inner_type == "NUMBER" 
                                        && inner_value.chars().all(|c| c.is_ascii_digit())
                                        && third_tok.as_ref().map(|t| t.0 == "SEPARATOR" && t.1 == ")") == Some(true)
                                    {
                                        self.next();
                                        self.next();
                                        self.next();
                                        self.next();

                                        Statement {
                                            node: Node::Number {
                                                value: format!("{}({})", token.1, inner_value),
                                            },
                                            loc: alloc_loc(self.get_loc()),
                                        }
                                    } else {
                                        let left = self.parse_operand();
                                        if self.err.is_some() {
                                            Statement::Null
                                        } else {
                                            let old_pos = self.pos.clone();
                                            self.next();
                                            let right = self.parse_expression();
                                            if !self.token_is("SEPARATOR", ")") {
                                                self.pos = old_pos;
                                                self.err = None;
                                                left
                                            } else {
                                                self.next();
                                                if self.err.is_some() {
                                                    Statement::Null
                                                } else {
                                                    Statement {
                                                        node: Node::Operation {
                                                            left: Box::new(left),
                                                            operator: "*".to_owned(),
                                                            right: Box::new(right),
                                                        },
                                                        loc: alloc_loc(self.get_loc()),
                                                    }
                                                }
                                            }
                                        }
                                    }
                                } else {
                                    self.parse_operand()
                                }
                            } else {
                                let left = self.parse_operand();
                                if self.err.is_some() {
                                    Statement::Null
                                } else {
                                    let old_pos = self.pos.clone();
                                    self.next();
                                    let right = self.parse_expression();
                                    if !self.token_is("SEPARATOR", ")") {
                                        self.pos = old_pos;
                                        self.err = None;
                                        left
                                    } else {
                                        self.next();
                                        if self.err.is_some() {
                                            Statement::Null
                                        } else {
                                            Statement {
                                                node: Node::Operation {
                                                    left: Box::new(left),
                                                    operator: "*".to_owned(),
                                                    right: Box::new(right),
                                                },
                                                loc: alloc_loc(self.get_loc()),
                                            }
                                        }
                                    }
                                }
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

                "SEPARATOR" if token.1 == "," => {
                    self.next();
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

    pub fn parse_path(&mut self) -> Option<PathElement> {
        let mut left = match self.token().cloned() {
            Some(Token(kind, val, _)) if kind == "IDENTIFIER" => {
                let mut segments: Vec<String> = vec![val.clone()];
                self.next();

                while self.token_is("SEPARATOR", ".") {
                    self.next();
                    if let Some(Token(kind, seg, _)) = self.token().cloned() {
                        if kind == "IDENTIFIER" {
                            self.next();
                            segments.push(seg.clone());
                        } else {
                            self.raise("SyntaxError", "Expected identifier after '.'");
                            return None;
                        }
                    } else {
                        self.raise("SyntaxError", "Expected identifier after '.'");
                        return None;
                    }
                }

                if self.token_is("SEPARATOR", "(") {
                    self.next();
                    let mut args = Vec::new();
                    while !self.token_is("SEPARATOR", ")") {
                        let arg = self.parse_path()?;
                        args.push(arg);
                        if self.token_is("SEPARATOR", ",") {
                            self.next();
                        }
                    }
                    if !self.token_is("SEPARATOR", ")") {
                        self.raise("SyntaxError", "Expected ')' in path args");
                        return None;
                    }
                    self.next();

                    PathElement::Path { segments, args }
                } else {
                    PathElement::Path { segments, args: vec![] }
                }
            }

            Some(Token(kind, val, _)) if kind == "SEPARATOR" && val == "(" => {
                self.next();
                let mut elems = Vec::new();
                while !self.token_is("SEPARATOR", ")") {
                    let e = self.parse_path()?;
                    elems.push(e);
                    if self.token_is("SEPARATOR", ",") {
                        self.next();
                    }
                }
                if !self.token_is("SEPARATOR", ")") {
                    self.raise("SyntaxError", "Expected ')' after tuple");
                    return None;
                }
                self.next();
                if elems.len() == 1 {
                    elems.remove(0)
                } else {
                    PathElement::Tuple(elems)
                }
            }

            Some(Token(kind, val, _)) if kind == "SEPARATOR" && val == "[" => {
                self.next();
                let mut elems = Vec::new();
                while !self.token_is("SEPARATOR", "]") {
                    let e = self.parse_path()?;
                    elems.push(e);
                    if self.token_is("SEPARATOR", ",") {
                        self.next();
                    }
                }
                if !self.token_is("SEPARATOR", "]") {
                    self.raise("SyntaxError", "Expected ']' after list");
                    return None;
                }
                self.next();
                if elems.len() == 1 {
                    elems.remove(0)
                } else {
                    PathElement::List(elems)
                }
            }

            Some(Token(kind, val, _)) if kind == "NUMBER" => {
                self.next();
                match val.parse::<i64>() {
                    Ok(n) => PathElement::Literal(Value::Int(Int::from_i64(n))),
                    Err(_) => match val.parse::<f64>() {
                        Ok(f) => PathElement::Literal(Value::Float(Float::from(f))),
                        Err(_) => {
                            self.raise("SyntaxError", "Invalid number format");
                            return None;
                        }
                    },
                }
            }

            Some(Token(kind, val, _)) if kind == "STRING" => {
                self.next();
                if val.len() >= 2
                    && ((val.starts_with('"') && val.ends_with('"'))
                        || (val.starts_with('\'') && val.ends_with('\'')))
                {
                    let unescaped = val[1..val.len() - 1]
                        .replace("\\n", "\n")
                        .replace("\\t", "\t")
                        .replace("\\r", "\r")
                        .replace("\\\"", "\"")
                        .replace("\\'", "'")
                        .replace("\\\\", "\\");
                    PathElement::Literal(Value::String(unescaped))
                } else {
                    self.raise("SyntaxError", &format!("Invalid string format {:?}", val));
                    return None;
                }
            }

            Some(Token(kind, val, _)) if kind == "BOOLEAN" => {
                self.next();
                match val.as_str() {
                    "true" => PathElement::Literal(Value::Boolean(true)),
                    "false" => PathElement::Literal(Value::Boolean(false)),
                    "null" => PathElement::Literal(Value::Null),
                    _ => {
                        self.raise("SyntaxError", "Invalid boolean value");
                        return None;
                    }
                }
            }

            _ => {
                self.raise("SyntaxError", "Expected path");
                return None;
            }
        };

        if self.token_is("OPERATOR", "|") {
            let mut union_elems = vec![left];
            while self.token_is("OPERATOR", "|") {
                self.next();
                let rhs = self.parse_path()?;
                union_elems.push(rhs);
            }
            left = PathElement::Union(union_elems);
        }

        Some(left)
    }

    fn parse_single_type(&mut self) -> Option<(String, Statement)> {
        let mut ptr_level: usize = 0;
        let mut is_maybe = false;
    
        while self.token_is("OPERATOR", "&") || self.token_is("OPERATOR", "?") || self.token_is("OPERATOR", "&&") || self.token_is("OPERATOR", "??") {
            match self.token().cloned().unwrap().1.as_str() {
                "&" => {
                    ptr_level += 1;
                }
                "&&" => {
                    ptr_level += 2;
                }
                "?" => {
                    if is_maybe {
                        self.raise("SyntaxError", "Type can only be marked as maybe once");
                        return None;
                    }
                    is_maybe = true;
                }
                "??" => {
                    if is_maybe {
                        self.raise("SyntaxError", "Type can only be marked as maybe once");
                        return None;
                    }
                    is_maybe = true;
                }
                _ => {}
            }
            self.next();
        }
    
        let token = self.token().cloned().unwrap_or(DEFAULT_TOKEN.clone());
        let is_tuple = token.0 == "SEPARATOR" && token.1 == "(";

        if is_tuple {
            self.next();
            let mut elements = Vec::new();
            while !self.token_is("SEPARATOR", ")") {
                let elem = self.parse_single_type();
                if self.err.is_some() {
                    return None;
                }
                if let Some((_, stmt)) = elem {
                    if let Node::Type { node } = &stmt.node {
                        elements.push(node.clone());
                    } else {
                        return self.raise("SyntaxError", "Expected type in tuple elements");
                    }
                } else {
                    elements.push(TypeNode::Simple {
                        base: "any".to_string(),
                        ptr_level: 0,
                        is_maybe: false,
                    });
                }
                if self.token_is("SEPARATOR", ",") {
                    self.next();
                }
            }
            if !self.token_is("SEPARATOR", ")") {
                self.raise("SyntaxError", "Expected ')' after tuple type elements");
                return None;
            }
            self.next();
            let r =  Some(("tuple".to_string(), Statement {
                node: Node::Type {
                    node: TypeNode::Generics {
                        base: Box::new(TypeNode::Simple {
                            base: "tuple".to_string(),
                            ptr_level: 0,
                            is_maybe: false,
                        }),
                        generics: elements,
                    }
                },
                loc: alloc_loc(self.get_loc()),
            }));
            if self.err.is_some() {
                return None;
            }
            return r;
        }

        if token.0 != "IDENTIFIER" {
            self.raise("SyntaxError", "Expected type identifier");
            return None;
        }
        self.next();
    
        let name = token.1;
    
        Some((
            name.clone(),
            Statement {
                node: Node::Type {
                    node: TypeNode::Simple {
                        base: name.clone(),
                        ptr_level,
                        is_maybe,
                    }
                },
                loc: alloc_loc(self.get_loc()),
            },
        ))
    }

    fn parse_impl(&mut self) -> Statement {
        self.check_for("IDENTIFIER", "impl");
        self.next();
        let mut impls = vec![];
        let mut joins = vec![];

        loop {
            let mut mods: Vec<String> = vec![];
            while is_valid_token(&self.token().cloned()) {
                match self.token().cloned() {
                    Some(Token(ref t, ref v, _)) if t == "IDENTIFIER" && (v == "static" || v == "final" || v == "mutable" || v == "non-static") => {
                        mods.push(v.clone());
                        self.next();
                    }
                    _ => break,
                }
            }
            let name_token = self.token().cloned();
            if name_token.is_none() || name_token.as_ref().unwrap().0 != "IDENTIFIER" {
                self.raise("SyntaxError", "Expected function name after 'impl'");
                return Statement::Null;
            }
            let func_name = name_token.unwrap().1;
            self.next();

            if !self.token_is("SEPARATOR", "[") {
                joins.push(func_name.clone());
                if self.token_is("OPERATOR", "+") {
                    self.next();
                    continue;
                } else {
                    break;
                }
            }
            self.next();

            let mut arg_types = vec![];
            while !self.token_is("SEPARATOR", "]") {
                let arg_type = self.parse_type();
                if arg_type == Statement::Null {
                    return Statement::Null;
                }
                if let Node::Type { node } = arg_type.node {
                    arg_types.push(node);
                } else {
                    return self.raise("SyntaxError", "Expected type in argument list");
                }

                if self.token_is("SEPARATOR", ",") {
                    self.next();
                } else if self.token_is("SEPARATOR", "]") {
                    break;
                } else {
                    self.raise("SyntaxError", "Expected ',' or ']' in argument list");
                    return Statement::Null;
                }
            }
            self.next();

            let ret_type = if self.token_is("OPERATOR", "->") {
                self.next();
                let r = self.parse_type();
                if r == Statement::Null {
                    return Statement::Null;
                }
                if let Node::Type { node } = r.node {
                    node
                } else {
                    return self.raise("SyntaxError", "Expected return type after '->'");
                }
            } else {
                Statement {
                    node: Node::Type {
                        node: TypeNode::Simple {
                            base: "any".to_string(),
                            ptr_level: 0,
                            is_maybe: false,
                        }
                    },
                    loc: alloc_loc(self.get_loc()),
                }
            };

            impls.push((
                func_name,
                arg_types,
                ret_type,
                mods,
            ));

            if self.token_is("OPERATOR", "+") {
                self.next();
                continue;
            } else {
                break;
            }
        }

        Statement {
            node: Node::Type {
                node: TypeNode::Impl {
                    impls,
                    joins,
                }
            },
            loc: alloc_loc(self.get_loc()),
        }
    }

    fn parse_type(&mut self) -> Statement {
        if self.token_is("IDENTIFIER", "impl") {
            return self.parse_impl();
        }

        let (base_str, base_stmt) = match self.parse_single_type() {
            Some(v) => v,
            None => return Statement::Null,
        };

        if self.token_is("SEPARATOR", "(") {
            let to_type = Statement {
                node: Node::Type {
                    node: TypeNode::Simple {
                        base: base_str.to_owned(),
                        ptr_level: 0,
                        is_maybe: false,
                    }
                },
                loc: alloc_loc(self.get_loc())
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
                if self.token_is("SEPARATOR", "]") {
                    break;
                }

                if self.token_is("OPERATOR", "...") {
                    if has_variadic_any || has_variadic_typed.is_some() {
                        self.raise("SyntaxError", "Multiple variadic markers are not allowed");
                        return Statement::Null;
                    }
                    self.next();

                    has_variadic_any = true;
                    has_variadic_typed = Some("any".to_string());

                    elements.push(
                        Statement {
                            node: Node::Type {
                                node: TypeNode::Variadics {
                                    base: TypeNode::Simple {
                                        base: "any".to_string(),
                                        ptr_level: 0,
                                        is_maybe: false,
                                    }
                                }
                            },
                            loc: alloc_loc(self.get_loc()),
                        }
                        .convert_to_map(),
                    );

                    if self.token_is("SEPARATOR", "]") {
                        break;
                    }
                    if self.token_is("SEPARATOR", ",") {
                        self.next();
                        continue;
                    }
                    self.raise("SyntaxError", "Expected ',' or ']' after variadic");
                    return Statement::Null;
                }

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

            if elements.is_empty() && base_str != "function" && base_str != "generator"
                && !has_variadic_any && has_variadic_typed.is_none()
            {
                self.raise("SyntaxError", "Empty type annotation is not allowed");
                return Statement::Null;
            }

            if base_str == "function" || base_str == "generator" {
                let mut return_type = Statement {
                    node: Node::Type {
                        node: TypeNode::Simple {
                            base: "any".to_owned(),
                            ptr_level: 0,
                            is_maybe: false,
                        }
                    },
                    loc: alloc_loc(self.get_loc())
                };

                if self.token_is("OPERATOR", "->") {
                    self.next();
                    return_type = self.parse_type();
                    if return_type.get_type() != "TYPE" {
                        self.raise("SyntaxError", "Expected a type after '->'");
                        return Statement::Null;
                    }
                }

                return Statement {
                    node: Node::Type {
                        node: if base_str == "function" {
                            TypeNode::Function {
                                parameter_types: elements.iter().map(|e| {
                                    if let Statement::Statement { .. } = e.convert_to_statement().node {
                                        e.convert_to_statement()
                                    } else {
                                        self.raise("SyntaxError", "Expected type in function parameters");
                                        Statement::Null
                                    }
                                }).collect(),
                                return_type: Box::new(return_type),
                            }
                        } else {
                            TypeNode::Generator {
                                parameter_types: elements.iter().map(|e| {
                                    if let Statement::Statement { .. } = e.convert_to_statement().node {
                                        e.convert_to_statement()
                                    } else {
                                        self.raise("SyntaxError", "Expected type in generator parameters");
                                        Statement::Null
                                    }
                                }).collect(),
                                yield_type: Box::new(return_type),
                            }
                        }
                    },
                    loc: alloc_loc(inner_loc),
                };
            }

            let mut generics_types = Vec::with_capacity(elements.len());
            for el in elements {
                if let Node::Type { node } = el.node {
                    generics_types.push(node);
                } else {
                    self.raise("SyntaxError", "Expected type in generics");
                    return Statement::Null;
                }
            }

            return Statement {
                node: Node::Type {
                    node: TypeNode::Generic {
                        base: base_str.to_owned(),
                        generics_types,
                    }
                },
                loc: alloc_loc(inner_loc),
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
                    if let Some(types_index) =
                        keys.iter().position(|k| k == &Value::String("types".to_string()))
                    {
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

        let mut union_types_nodes = Vec::with_capacity(union_types.len());
        for ut in union_types.iter() {
            if let Node::Type { node } = &ut.node {
                union_types_nodes.push(node.clone());
            } else {
                self.raise("SyntaxError", "Expected type in union");
                return Statement::Null;
            }
        }

        if union_types.len() > 1 {
            return Statement {
                node: Node::Type {
                    node: TypeNode::Union {
                        types: union_types_nodes,
                    }
                },
                loc: alloc_loc(self.get_loc())
            };
        }

        base_stmt
    }

    fn parse_variable(&mut self) -> Statement {
        let token = self.token().cloned().unwrap_or(DEFAULT_TOKEN.clone());
        let loc = self.get_loc();

        if token.0 == "IDENTIFIER" {
            let name = token.1.clone();
            if RESERVED_KEYWORDS.contains(&name.as_str()) {
                self.raise("SyntaxError", &format!("'{}' is a reserved keyword and cannot be used as a variable name", name));
                return Statement::Null;
            }
            self.next();
            if self.token_is("SEPARATOR", ":") && self.parse_var_decl {
                self.next();
                let type_token = self.token().cloned().unwrap_or(DEFAULT_TOKEN.clone());
                if !(type_token.0 == "IDENTIFIER" || type_token.0 == "OPERATOR" || (type_token.0 == "SEPARATOR" && ["(", "<", "["].contains(&type_token.1.as_str()))) {
                    self.raise("SyntaxError", "Expected type after ':'");
                    return Statement::Null;
                }
                let type_ = self.parse_type();
                let (mut value, mut is_default) = (get_type_default_as_statement_from_statement(&type_), true);
                if self.err.is_some() {
                    return Statement::Null;
                }
                if self.token_is("OPERATOR", "=") {
                    self.next();
                    value = self.parse_expression();
                    is_default = false;
                    if self.err.is_some() {
                        return Statement::Null;
                    }
                }
                return Statement {
                    node: Node::VariableDeclaration {
                        name,
                        var_type: type_,
                        value,
                        modifiers: vec![],
                        is_default,
                    },
                    loc: alloc_loc(loc),
                };
            } else if self.token_is("OPERATOR", ":=") && self.parse_var_decl {
                let name = token.1.clone();
                self.next();
                let type_ = Statement {
                    node: Node::Type {
                        node: TypeNode::Simple {
                            base: "auto".to_owned(),
                            ptr_level: 0,
                            is_maybe: false,
                        }
                    },
                    loc: alloc_loc(loc.clone())
                };
                if !is_valid_token(&self.token().cloned()) {
                    let loc = self.get_loc().unwrap();
                    self.raise_with_loc("SyntaxError", "Unexpected end of input after ':='", loc);
                    return Statement::Null;
                }
                let value = self.parse_expression();
                if self.err.is_some() {
                    return Statement::Null;
                }
                return Statement {
                    node: Node::VariableDeclaration {
                        name,
                        var_type: type_,
                        value: value,
                        modifiers: vec![],
                        is_default: false,
                    },
                    loc: alloc_loc(loc),
                };
            } else {
                return Statement {
                    node: Node::Variable {
                        name: name.clone(),
                    },
                    loc: alloc_loc(loc),
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
        let next_token = self.peek(1);
        let next_token_type = next_token.map(|t| t.0.clone()).unwrap_or_else(|| "".to_string());
        let next_token_value = next_token.map(|t| t.1.clone()).unwrap_or_else(|| "".to_string());
        
        if token_type == "SEPARATOR" && token_value == "(" {
            let mut values = Vec::new();
            self.next();
            while !self.token_is("SEPARATOR", ")") {
                let expr = self.parse_expression();
                if self.err.is_some() {
                    return Statement::Null;
                }
                values.push(expr);
                if self.token_is("SEPARATOR", ",") {
                    self.next();
                } else {
                    break;
                }
            }
            self.check_for("SEPARATOR", ")");
            self.next();
            if values.len() == 1 {
                return values[0];
            }

            return Statement {
                node: Node::Iterable {
                    node: IterableNode::Tuple {
                        elements: values,
                    }
                },
                loc: alloc_loc(loc),
            };
        }

        if token_type == "NUMBER" {
            self.next();
            return Statement {
                node: Node::Number {
                    value: token_value.to_owned(),
                },
                loc: alloc_loc(loc),
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
                    .map(|ch| ch.to_owned())
                    .collect();
        
                return Statement {
                    node: Node::String {
                        value: literal_str.to_string(),
                        mods,
                    },
                    loc: alloc_loc(loc),
                };
            } else {
                return Statement {
                    node: Node::String {
                        value: token_value.to_owned(),
                        mods: Vec::new(),
                    },
                    loc: alloc_loc(loc),
                };
            }
        }
        
        if token_type == "RAW_STRING" {
            let valid_mods = ['f', 'r', 'b'];
            let token_value = token.1.clone();
        
            self.next();
        
            if let Some(first_quote_idx) = token_value.find(|c| c == '\'' || c == '"') {
                let (prefix_str, literal_str) = token_value.split_at(first_quote_idx);
        
                let mods: Vec<String> = prefix_str
                    .chars()
                    .filter(|ch| valid_mods.contains(ch))
                    .map(|ch| ch.to_owned())
                    .collect();
        
                return Statement {
                    node: Node::String {
                        value: literal_str.to_string(),
                        mods
                    },
                    loc: alloc_loc(loc),
                };
            } else {
                return Statement {
                    node: Node::String {
                        value: literal_str.to_owned(),
                        mods: Vec::new(),
                    },
                    loc: alloc_loc(loc),
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
            return Statement {
                node: Node::Boolean {
                    value: match token_value.as_str() {
                        "true" => Some(true),
                        "false" => Some(false),
                        "null" => None,
                        _ => {
                            self.raise("SyntaxError", "Invalid boolean value");
                            return Statement::Null;
                        }
                    },
                },
                loc: alloc_loc(loc),
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
                            return Statement {
                                node: Node::Iterable {
                                    node: IterableNode::ListCompletion {
                                        seed: elements,
                                        end: Statement::Null,
                                        pattern_flag: pattern_reg,
                                        range_mode: RangeModeType::Length,
                                        is_infinite: true,
                                    },
                                },
                                loc: alloc_loc(loc),
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
    
                return Statement {
                    node: Node::Iterable {
                        node: IterableNode::ListCompletion {
                            seed: elements,
                            end: end_expr,
                            pattern_flag: pattern_reg,
                            range_mode: if found_semicolon { RangeModeType::Length } else { RangeModeType::Value },
                            is_infinite: false,
                        },
                    },
                    loc: alloc_loc(loc),
                };
            }
    
            let expr = self.parse_expression();
            if self.err.is_some() {
                return Statement::Null;
            }

            if self.token_is("IDENTIFIER", "for") {
                let mut for_clauses = Vec::new();

                while self.token_is("IDENTIFIER", "for") {
                    self.next();

                    let variables: PathElement = match self.parse_path() {
                        Some(v) => v,
                        None => {
                            self.raise("PathError", "Failed to parse variable path in list comprehension");
                            return Statement::Null;
                        }
                    };

                    if self.err.is_some() { return Statement::Null; }

                    self.check_for("OPERATOR", "in");
                    self.next();
                    let iterable = self.parse_expression();
                    if self.err.is_some() { return Statement::Null; }

                    let mut filters = Vec::new();
                    while self.token_is("IDENTIFIER", "if") {
                        self.next();
                        let cond = self.parse_expression();
                        if self.err.is_some() { return Statement::Null; }
                        filters.push(cond.convert_to_map());
                    }

                    for_clauses.push((variables, iterable.convert_to_map(), Value::List(filters)));
                }

                self.check_for("SEPARATOR", "]");
                self.next();

                return Statement {
                    node: Node::Iterable {
                        node: IterableNode::ListComprehension {
                            for_clauses,
                            map_expression: expr,
                        },
                    },
                    loc: alloc_loc(loc),
                };
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
    
        Statement {
            node: Node::Iterable {
                node: IterableNode::List {
                    elements,
                },
            },
            loc: alloc_loc(loc),
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
        let name = token.1.to_owned();
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
            return Statement {
                node: Node::Call {
                    name,
                    pos_args: Vec::new(),
                    named_args: HashMap::new(),
                },
                loc: alloc_loc(loc),
            }
        }

        Statement {
            node: Node::Call {
                name,
                pos_args,
                named_args,
            },
            loc: alloc_loc(loc),
        }
    }

    fn parse_arguments(&mut self) -> (Vec<Statement>, HashMap<String, Statement>) {
        let mut pos_args = Vec::new();
        let mut named_args = HashMap::new();
        let mut seen_named = false;
    
        while let Some(current_token) = self.token().cloned() {
            let next_token = self.peek(1).cloned();
    
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
                    if RESERVED_KEYWORDS.contains(&name.as_str()) {
                        self.raise("SyntaxError", &format!("'{}' is a reserved keyword and cannot be used as a named argument", name));
                        return (vec![], vec![]);
                    }
                    self.next();
                    self.next();
                    let value = self.parse_expression();
                    if self.err.is_some() {
                        return (vec![], vec![]);
                    }
                    named_args.insert(name, value);
                    seen_named = true;
                } else {
                    if seen_named {
                        self.raise("ArgumentOrderError", "Positional arguments cannot follow named arguments");
                        return (vec![], vec![]);
                    }
                    let expr = self.parse_expression();
                    if self.err.is_some() {
                        return (vec![], HashMap::new());
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
                    return (vec![], HashMap::new());
                }
                pos_args.push(expr);
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
