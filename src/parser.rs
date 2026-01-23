use crate::env::runtime::utils::{hex_to_ansi, unescape_string_literal, get_type_default_as_statement, get_type_default_as_statement_from_statement, get_precedence, is_valid_token, supports_color, to_static, parse_usize_radix};
use crate::env::runtime::value::Value;
use crate::env::runtime::errors::Error;
use crate::env::runtime::statements::{Statement, Node, MatchCase, ThrowNode,  alloc_loc, RangeModeType, IterableNode, TypeNode, TypeDeclNode, AccessType, ParamAST, PtrNode, ForLoopNode};
use crate::env::runtime::types::{VALID_TYPES, Int, Float};
use crate::env::runtime::tokens::{Token, Location, DEFAULT_TOKEN};
use crate::env::runtime::internal_structs::{PathElement, EffectFlags};
use crate::env::runtime::utils::RESERVED_KEYWORDS;
use std::collections::HashMap;
use std::borrow::Cow;
use rustc_hash::FxHashMap;

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    statements: Vec<Statement>,
    pub err: Option<Error>,
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
            err_type: error_type.to_string(),
            err_msg: msg.to_string(),
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
            err_type: error_type.to_string(),
            err_msg: msg.to_string(),
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
            err_type: error_type.to_string(),
            err_msg: msg.to_string(),
            help: None,
            loc: Some(loc.set_lucia_source_loc(format!("{}:{}:{}", rust_loc.file(), rust_loc.line(), rust_loc.column()))),
            ref_err: None,
        });
        Statement::Null
    }

    #[inline]
    fn next(&mut self) -> Option<&Token> {
        self.pos += 1;
        self.token()
    }

    #[track_caller]
    fn check_for(&mut self, expected_type: &str, expected_value: &str) {
        if let Some(token) = self.token() {
            let token_type = token.0.as_ref();
            let token_value = token.1.as_ref();
            let value_matches = expected_value.is_empty() || token_value == expected_value;

            if !(token_type == "EOF" && expected_type != "EOF") {
                if token_type != expected_type || !value_matches {
                    self.raise("UEFError", &format!(
                        "Expected token '{}' but found: {}",
                        if expected_value.is_empty() { expected_type } else { expected_value },
                        token_value
                    ));
                }
                return;
            }
        }
        if expected_type == "SEPARATOR" && [")", "]", "}", "end"].contains(&expected_value) {
            let opening = match expected_value {
                ")" => "(",
                "]" => "[",
                "}" => "{",
                "end" => ":",
                _ => "",
            };
            self.raise_with_help("SyntaxError", &format!("\"{}\" was never closed", opening), &format!("Maybe you forgot '{}'?", expected_value));
            return;
        } else if expected_type == "OPERATOR" && ["|"].contains(&expected_value) {
            let opening = match expected_value { "|" => "|", _ => "", };
            self.raise_with_help("SyntaxError", &format!("\"{}\" is missing its closing pair", opening), &format!("Maybe you forgot '{}'?", expected_value));
            return;
        }
        self.raise("UEFError", &format!(
            "Expected token '{}' but found end of input",
            if expected_value.is_empty() { expected_type } else { expected_value }
        ));
        return;
    }

    #[inline]
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
        let mut statements = Vec::with_capacity(self.tokens.len() / 4);
        while self.token().is_some() {
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
            match (tok.0.as_ref(), tok.1.as_ref()) {
                ("SEPARATOR", "[") => {
                    self.next();

                    let mut start_expr: Option<Statement> = None;
                    let mut end_expr: Option<Statement> = None;

                    let mut has_range = false;

                    if let Some(Token(tk, tv, _)) = self.token().cloned() {
                        if tk == "SEPARATOR" && tv == "]" {
                            self.raise("SyntaxError", "Empty index access '[]' is not allowed");
                            return Statement::Null;
                        }

                        if tk == "SEPARATOR" && tv == ".." {
                            has_range = true;
                            self.next();

                            if let Some(Token(t2k, t2v, _)) = self.token().cloned() {
                                if t2k == "SEPARATOR" && t2v == "]" {
                                    self.next();
                                    expr = Statement {
                                        node: Node::IndexAccess {
                                            object: Box::new(expr),
                                            access: Box::new(AccessType::Full),
                                        },
                                        loc: alloc_loc(self.get_loc()),
                                    };
                                    return expr;
                                } else {
                                    end_expr = Some(self.parse_expression());
                                }
                            }
                        } else {
                            start_expr = Some(self.parse_expression());
                            if self.err.is_some() { return Statement::Null; }

                            if let Some(Token(t2k, t2v, _)) = self.token().cloned() {
                                if t2k == "SEPARATOR" && t2v == ".." {
                                    has_range = true;
                                    self.next();

                                    if let Some(Token(t3k, t3v, _)) = self.token().cloned() {
                                        if !(t3k == "SEPARATOR" && t3v == "]") {
                                            end_expr = Some(self.parse_expression());
                                        }
                                    }
                                }
                            }
                        }
                    }

                    self.check_for("SEPARATOR", "]");
                    self.next();

                    let access = match (start_expr, end_expr, has_range) {
                        (Some(s), Some(e), _) => AccessType::Range { start: s, end: e },
                        (Some(s), None, true) => AccessType::ToEnd { start: s },
                        (None, Some(e), _) => AccessType::ToStart { end: e },
                        (Some(s), None, false) => AccessType::Single { index: s },
                        (None, None, _) => AccessType::Full,
                    };

                    expr = Statement {
                        node: Node::IndexAccess {
                            object: Box::new(expr),
                            access: Box::new(access),
                        },
                        loc: alloc_loc(self.get_loc()),
                    };
                }

                ("SEPARATOR", "{") => {
                    let struct_name = match expr.node {
                        Node::Variable { ref name } => name,
                        _ => return expr,
                    };
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
                    self.check_for("SEPARATOR", "{");
                    self.next();
                    match self.token() {
                        Some(Token(token_type, token_value, _)) if token_type == "BOOLEAN" && token_value == "null" => {
                            self.next();
                            self.check_for("SEPARATOR", "}");
                            self.next();
                            return Statement {
                                node: Node::StructCreation {
                                    name: struct_name.clone(),
                                    fields_ast: HashMap::new(),
                                    is_null: true,
                                },
                                loc: alloc_loc(self.get_loc()),
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
                        return Statement {
                            node: Node::StructCreation {
                                name: struct_name.clone(),
                                fields_ast: HashMap::new(),
                                is_null: false,
                            },
                            loc: alloc_loc(self.get_loc()),
                        };
                    }
                    let mut fields = HashMap::new();
                    while !self.token_is("SEPARATOR", "}") {
                        if self.token_is("SEPARATOR", ".") {
                            if self.peek(1).is_some() && self.peek(1).unwrap().0 == "IDENTIFIER" {
                                self.raise_with_help("SyntaxError", "Unexpected '.' in struct definition", "Struct fields don't need a '.' prefix. You can safely remove it.");
                                return Statement::Null;
                            }
                            self.raise("SyntaxError", "Unexpected '.' in struct definition");
                            return Statement::Null;
                        }
                        let name = match self.token() {
                            Some(Token(ty, id, _)) if ty == "IDENTIFIER" => id.clone(),
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
                                    name: name.clone().into_owned(),
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
                        fields.insert(name.to_string(), value);
                        if self.token_is("SEPARATOR", ",") {
                            self.next();
                        }
                    }
                    self.check_for("SEPARATOR", "}");
                    self.next();

                    return Statement {
                        node: Node::StructCreation {
                            name: struct_name.clone(),
                            fields_ast: fields,
                            is_null: false,
                        },
                        loc: alloc_loc(self.get_loc()),
                    };
                }

                ("SEPARATOR", ".") => {
                    self.next();
                    let property_name = match self.token() {
                        Some(t) if t.0.as_ref() == "IDENTIFIER" => t.1.clone().into_owned(),
                        Some(_) => { self.raise("SyntaxError", &format!("Expected identifier after '.', found '{}'", self.token().unwrap().1.as_ref())); return Statement::Null; },
                        None => { self.raise("SyntaxError", "Expected identifier after '.'"); return Statement::Null; }
                    };

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
                                    method_name: property_name.clone(),
                                    pos_args,
                                    named_args,
                                },
                                loc: alloc_loc(self.get_loc()),
                            };
                        } else {
                            expr = Statement {
                                node: Node::PropertyAccess {
                                    object: Box::new(expr),
                                    property_name: property_name.clone(),
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
                        let mut body = Vec::with_capacity(8);
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
                                else_body: None,
                            },
                            loc: alloc_loc(loc),
                        };
                    } else {
                        let then_body = self.parse_expression();
                        return Statement {
                            node: Node::If {
                                condition: Box::new(expr),
                                body: vec![then_body],
                                else_body: None,
                            },
                            loc: alloc_loc(loc),
                        };
                    }
                }

                // fucking shit
                ("IDENTIFIER", "where") => {
                    self.next();
                    let loc = self.get_loc();
                    let mut body: Vec<Statement> = Vec::with_capacity(8);
                    enum ThenOp {
                        Call(String, Vec<Statement>, HashMap<String, Statement>),
                        Op(String, Statement)
                    }
                    let mut then_body: Vec<ThenOp> = Vec::with_capacity(8);
                    let mut variable_names: Vec<String> = Vec::with_capacity(8);
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
                            Node::Assignment { left, right } => {
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
                                                    }
                                                },
                                                loc: alloc_loc(loc.clone()),
                                            }),
                                            val_stmt: right,
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
                            then_body.push(ThenOp::Op(op.into_owned(), right));
                        } else {
                            let then_expr = self.parse_expression();
                            if self.err.is_some() {
                                return Statement::Null;
                            }

                            match then_expr.node {
                                Node::Call { name, pos_args, named_args, .. } => {
                                    then_body.push(ThenOp::Call(name, pos_args, named_args));
                                }
                                _ => {
                                    self.raise("SyntaxError", "Expected function call after 'then' in 'where' clause");
                                    return Statement::Null;
                                }
                            }
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
                    group_body.extend(body);
                    group_body.push(
                        Statement {
                            node: Node::VariableDeclaration {
                                name: mangled_res_name.clone(),
                                var_type: Box::new(Statement {
                                    node: Node::Type {
                                        node: TypeNode::Simple {
                                            base: "any".to_owned(),
                                        }
                                    },
                                    loc: alloc_loc(loc.clone()),
                                }),
                                val_stmt: Box::new(expr),
                                modifiers: vec![],
                                is_default: false,
                            },
                            loc: alloc_loc(loc.clone()),
                        }
                    );
                    for then_op in then_body {
                        group_body.push(
                            Statement {
                                node: Node::Assignment {
                                    left: Box::new(Statement {
                                        node: Node::Variable {
                                            name: mangled_res_name.clone(),
                                        },
                                        loc: alloc_loc(loc.clone()),
                                    }),
                                    right: match then_op {
                                        ThenOp::Call(function_name, pos_args, named_args) => {
                                            Box::new(Statement {
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
                                            })
                                        }
                                        ThenOp::Op(op, val) => {
                                            Box::new(Statement {
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
                                            })
                                        }
                                    }
                                },
                                loc: alloc_loc(loc.clone()),
                            }
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
                            }
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
                        }
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
                        node: Node::Operation {
                            right: Box::new(expr.clone()),
                            operator: "!".to_owned(),
                            left: Box::new(Statement {
                                node: Node::Number {
                                    value: fact_level.to_string(),
                                },
                                loc: alloc_loc(self.get_loc()),
                            }),
                        },
                        loc: alloc_loc(self.get_loc()),
                    };
                }

                ("OPERATOR", op) if ["++", "--"].contains(&op) => {
                    let loc = alloc_loc(self.get_loc());
                    self.next();
                    expr = Statement {
                        node: Node::PostfixOperation {
                            operator: op.to_string(),
                            operand: Box::new(expr.clone()),
                        },
                        loc,
                    }
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

            match (tok.0.as_ref(), tok.1.as_ref()) {
                ("IDENTIFIER", "as") => {
                    self.next();
                    let type_conv = self.parse_type();
                    if self.err.is_some() {
                        return Statement::Null;
                    }

                    expr = Statement {
                        node: Node::TypeConvert {
                            node: Box::new(expr),
                            target_type: Box::new(type_conv),
                        },
                        loc: alloc_loc(self.get_loc()),
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

                    let raw_params: Vec<Statement> = match &expr.node {
                        Node::Iterable { node: IterableNode::Tuple { elements } } => elements.clone(),
                        _ => vec![expr.clone()],
                    };

                    let mut parameters: Vec<ParamAST> = Vec::with_capacity(raw_params.len());

                    for param_stmt in raw_params.into_iter() {
                        match param_stmt.node {
                            Node::Variable { name } => {
                                parameters.push(ParamAST {
                                    name,
                                    ty: None,
                                    default: None,
                                    modifiers: vec![],
                                    is_variadic: false,
                                });
                            }
                            Node::VariableDeclaration { name, val_stmt, var_type, modifiers, is_default } => {
                                let default = if is_default { Some(*val_stmt) } else { None };
                                parameters.push(ParamAST {
                                    name,
                                    ty: Some(*var_type),
                                    default,
                                    modifiers,
                                    is_variadic: false,
                                });
                            }
                            Node::Assignment { left, right } => {
                                if let Node::Variable { name } = left.node {
                                    parameters.push(ParamAST {
                                        name,
                                        ty: None,
                                        default: Some(*right),
                                        modifiers: vec![],
                                        is_variadic: false,
                                    });
                                } else {
                                    self.raise("SyntaxError", "Lambda parameter assignment must have a variable on the left-hand side");
                                    return Statement::Null;
                                }
                            }
                            _ => {
                                self.raise("SyntaxError", "Lambda parameters must be variable declarations or assignments");
                                return Statement::Null;
                            }
                        }
                    }

                    let return_type_stmt = Statement {
                        node: Node::Type {
                            node: TypeNode::Simple {
                                base: "any".to_owned(),
                            }
                        },
                        loc: alloc_loc(self.get_loc()),
                    };

                    expr = Statement {
                        node: Node::FunctionDeclaration {
                            name: "<lambda#{id}>".to_owned(),
                            args: parameters,
                            body: vec![body_expr],
                            modifiers: vec!["mutable".to_string()],
                            return_type: Box::new(return_type_stmt),
                            effect_flags: EffectFlags::UNKNOWN,
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

                    if let Node::Iterable { node: IterableNode::Tuple { elements: targets } } = &expr.node {
                        return Statement {
                            node: Node::UnpackAssignment {
                                targets: targets.to_vec(),
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
                            left: Box::new(expr.clone()),
                            right: Box::new(Statement {
                                node: Node::Operation {
                                    left: Box::new(expr),
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
                Some(Token(ref ttype, ref val, _)) => (ttype.as_ref(), val.as_ref()),
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
                        if ["\\", "(", "[", "{"].contains(&prev_tok.1.as_ref()) {
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
                    Some(Token(ref ttype, ref val, _)) => (ttype.as_ref(), val.as_ref()),
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
            Some(token) => match token.0.as_ref() {
                "IDENTIFIER" if VALID_TYPES.contains(&token.1.as_ref()) => {
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
                            right: Box::new(expr),
                            operator: "abs".to_owned(),
                            left: Box::new(Statement::Null),
                        },
                        loc: alloc_loc(self.get_loc()),
                    }
                }

                "OPERATOR" if ["--", "++"].contains(&token.1.as_ref()) => {
                    let operator = token.1.to_string();
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
                        node: Node::PrefixOperation {
                            operator,
                            operand: Box::new(operand),
                        },
                        loc: alloc_loc(loc),
                    }
                }

                "OPERATOR" if ["+", "-", "!", "~", "nein", "isnt", "isn't", "not", "bnot"].contains(&token.1.as_ref()) => {
                    let operator = token.1.to_string();
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
                    Statement {
                        node: Node::Pointer {
                            value: Box::new(expr),
                            ptr_node: PtrNode::PointerRef { 
                                ref_level: ptr_level,
                            },
                        },
                        loc: alloc_loc(self.get_loc()),
                    }
                }

                "OPERATOR" if token.1 == "*" => {
                    let mut deref_level: usize = 0;
                    while self.token_is("OPERATOR", "*") {
                        if deref_level == usize::MAX {
                            self.raise("OverflowError", "Maximum dereference level reached");
                            return Statement::Null;
                        }
                        deref_level += 1;
                        self.next();
                    }
                    let expr = self.parse_primary();
                    if self.err.is_some() { return Statement::Null; }
                    if self.token_is("OPERATOR", "=") {
                        self.next();
                        let value = self.parse_expression();
                        if self.err.is_some() { return Statement::Null; }
                        return Statement {
                            node: Node::Pointer {
                                ptr_node: PtrNode::PointerAssign {
                                    target: Box::new(expr),
                                    assign_level: deref_level,
                                },
                                value: Box::new(value),
                            },
                            loc: alloc_loc(self.get_loc()),
                        };
                    }
                    Statement {
                        node: Node::Pointer {
                            ptr_node: PtrNode::PointerDeref {
                                deref_level,
                            },
                            value: Box::new(expr),
                        },
                        loc: alloc_loc(self.get_loc()),
                    }
                }

                "SEPARATOR" if token.1 == "{" => {
                    let loc = self.get_loc();
                    self.next();

                    let mut keys = Vec::with_capacity(8);
                    let mut values = Vec::with_capacity(8);

                    while let Some(next_token) = self.token() {
                        if next_token.0 == "SEPARATOR" && next_token.1 == "}" {
                            self.next();
                            break;
                        }

                        let key = self.parse_expression();
                        if self.err.is_some() { return Statement::Null; }

                        match self.token() {
                            Some(Token(t, v, _)) if t == "SEPARATOR" && v == ":" => self.next(),
                            Some(_) => {
                                self.raise("SyntaxError", "Expected ':' after key in map");
                                return Statement::Null;
                            }
                            None => {
                                self.raise("SyntaxError", "Unexpected end of input in map");
                                return Statement::Null;
                            }
                        };

                        let value = self.parse_expression();
                        if self.err.is_some() { return Statement::Null; }

                        keys.push(key);
                        values.push(value);

                        match self.token() {
                            Some(Token(t, v, _)) if t == "SEPARATOR" && v == "," => self.next(),
                            Some(Token(t, v, _)) if t == "SEPARATOR" && v == "}" => { self.next(); break; }
                            Some(_) => {
                                self.raise("SyntaxError", "Expected ',' or '}' in map");
                                return Statement::Null;
                            }
                            None => {
                                self.raise("SyntaxError", "Unexpected end of input in map");
                                return Statement::Null;
                            }
                        };
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
                    let mut is_cstyle = false;

                    if self.token_is("SEPARATOR", "(") {
                        parentheses = true;
                        self.next();

                        let mut depth:  usize = 1;
                        let mut commas: usize = 0;

                        let mut i: usize = 0;
                        while let Some(Token(k, v, _)) = self.peek(i) && depth > 0 {
                            if k == "SEPARATOR" {
                                match v.as_ref() {
                                    "(" => {
                                        depth += 1;
                                    }
                                    ")" => {
                                        depth -= 1;
                                    }
                                    "," if depth == 1 => {
                                        commas += 1;
                                    }
                                    _ => {}
                                }
                            }
                            i += 1;
                        }

                        let colon_at_top = match self.peek(i) {
                            Some(Token(k, v, _)) if k == "SEPARATOR" && v == ":" => true,
                            _ => false,
                        };

                        if commas == 2 && colon_at_top {
                            is_cstyle = true;
                        }

                        if is_cstyle {
                            let decl = self.parse_expression();
                            if self.err.is_some() {
                                return Statement::Null;
                            }

                            if !self.token_is("SEPARATOR", ",") {
                                self.raise("SyntaxError", "Expected ',' after initializer");
                                return Statement::Null;
                            }
                            self.next();

                            let cond = self.parse_expression();
                            if self.err.is_some() {
                                return Statement::Null;
                            }

                            if !self.token_is("SEPARATOR", ",") {
                                self.raise("SyntaxError", "Expected ',' after condition");
                                return Statement::Null;
                            }
                            self.next();

                            let update = self.parse_expression();
                            if self.err.is_some() {
                                return Statement::Null;
                            }

                            if !self.token_is("SEPARATOR", ")") {
                                self.raise_with_help("SyntaxError", "Expected ')'", "Did you forget the ')' ?");
                                return Statement::Null;
                            }
                            self.next();

                            if !self.token_is("SEPARATOR", ":") {
                                self.raise("SyntaxError", "Expected ':' after C-style for header");
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
                                self.raise("SyntaxError", "Expected 'end' after C-style for body");
                                return Statement::Null;
                            }
                            self.next();

                            return Statement {
                                node: Node::For {
                                    node: ForLoopNode::Standard {
                                        initializer: Box::new(decl),
                                        condition: Box::new(cond),
                                        update: Box::new(update),
                                    },
                                    body,
                                },
                                loc: alloc_loc(loc),
                            };
                        }
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
                            if ![
                                "public","private","static","non-static","final","mutable","fun"
                            ].contains(&self.token().cloned().unwrap_or(DEFAULT_TOKEN.clone()).1.as_ref()) {
                                self.raise("SyntaxError", "Expected function definition in methods block");
                                return Statement::Null;
                            }
                            let func = self.parse_primary();
                            if self.err.is_some() {
                                return Statement::Null;
                            }
                            if !matches!(func.node, Node::FunctionDeclaration { .. }) {
                                if !matches!(func.node, Node::VariableDeclaration { .. }) {
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
                            PathElement::Path { segments, args: _ } if segments.len() == 1 => segments[0].clone(),
                            _ => {
                                self.raise("SyntaxError", &format!("'{}' is not a valid struct name", variable.display()));
                                return Statement::Null;
                            }
                        };
                        return Statement {
                            node: Node::StructMethods {
                                struct_name: var,
                                methods_ast: functions,
                            },
                            loc: alloc_loc(loc),
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
                        node: Node::For {
                            node: ForLoopNode::ForIn {
                                iterable: Box::new(iterable),
                                variable,
                            },
                            body,
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
                    Statement {
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
                            node: Box::new(value),
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
                                node: Box::new(ThrowNode::Tuple(val)),
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
                                node: Box::new(ThrowNode::Message {
                                    message,
                                    from,
                                }),
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
                                        exception_vars.push(tok.1.clone().into_owned());
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
                            exception_vars: if exception_vars.is_empty() { None } else { Some(exception_vars.iter().map(|s| s.to_string()).collect()) },
                        },
                        loc: alloc_loc(loc),
                    }
                }

                "IDENTIFIER" if ["struct", "enum"].contains(&token.1.as_ref()) && !([Some(":="), Some(":"), Some("="), Some(","), Some("\0"), Some(""), Some(" ")].contains(&self.peek(1).map(|tok| tok.1.as_ref()))) => {
                    let next_name = self.peek(1)
                        .and_then(|tok| if tok.0.as_ref() == "IDENTIFIER" { Some(tok.1.as_ref()) } else { None })
                        .unwrap_or("Name");

                    return self.raise_with_help(
                        "SyntaxError",
                        &format!("Unexpected keyword '{}'", token.1),
                        &format!("Did you mean to use 'typedef {} {} = {{...}}'?", token.1, next_name),
                    );
                }

                "IDENTIFIER" if ["fun", "gen", "typedef", "import", "public", "private", "static", "non-static", "final", "mutable"].contains(&token.1.as_ref()) => {
                    let mut modifiers: Vec<String> = vec![];

                    if ["public", "private", "static", "non-static", "final", "mutable"].contains(&token.1.as_ref()) {
                        while let Some(tok) = self.token() {
                                if tok.0 == "IDENTIFIER" && ["public", "private", "static", "non-static", "final", "mutable"].contains(&tok.1.as_ref()) {
                                modifiers.push(tok.1.clone().into_owned());
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

                    match self.token().cloned().unwrap_or_default().1.as_ref() {
                        x if ["fun", "gen"].contains(&x) => {
                            self.next();
                            let name = self.token().map(|tok| if tok.0.as_ref() == "IDENTIFIER" { tok.1.clone().into_owned() } else { "".to_string() }).unwrap_or_default();
                            let is_function = x == "fun";
                            if RESERVED_KEYWORDS.contains(&name.as_ref()) {
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
                            let mut parameters: Vec<ParamAST> = vec![];
                            let mut effect_flags: EffectFlags = EffectFlags::empty();

                            while let Some(mut current_tok) = self.token().cloned() {
                                let mut param_modifiers = vec![];
                                let mut is_variadic = false;

                                while current_tok.0 == "IDENTIFIER" &&
                                    ["mutable", "final", "static", "non-static"].contains(&current_tok.1.as_ref())
                                {
                                    param_modifiers.push(current_tok.1.clone().into_owned());
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
                                        ["mutable", "final", "static", "non-static"].contains(&current_tok.1.as_ref())
                                        {
                                            param_modifiers.push(current_tok.1.clone().into_owned());
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

                                    if self.token_is("OPERATOR", "=") && !is_variadic {
                                        self.next();
                                        let def_val = self.parse_expression();
                                        if self.err.is_some() { return Statement::Null; }
                                        parameters.push(ParamAST {
                                            name: arg_name.to_string(),
                                            ty: Some(arg_type),
                                            default: Some(def_val),
                                            modifiers: param_modifiers,
                                            is_variadic,
                                        });
                                    } else {
                                        parameters.push(ParamAST {
                                            name: arg_name.to_string(),
                                            ty: Some(arg_type),
                                            default: None,
                                            modifiers: param_modifiers,
                                            is_variadic,
                                        });
                                    }

                                    if self.token_is("SEPARATOR", ",") { self.next(); }
                                    if is_variadic { 
                                        if !self.token_is("SEPARATOR", ")") {
                                            self.raise("SyntaxError", "Variadic parameter must be the last parameter");
                                            return Statement::Null;
                                        }
                                    }
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
                            if self.token_is("OPERATOR", "<") && self.peek(1).map(|t| t.1.as_ref()) == Some("!") {
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
                                        match effect_token.1.to_lowercase().as_ref() {
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
                                        name,
                                        args: parameters,
                                        body,
                                        modifiers,
                                        return_type: Box::new(return_type),
                                        effect_flags,
                                    }
                                } else {
                                    Node::GeneratorDeclaration {
                                        name,
                                        args: parameters,
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

                            let kind = if self.token_is("IDENTIFIER", "enum") {
                                self.next();
                                "enum"
                            } else if self.token_is("IDENTIFIER", "struct") {
                                self.next();
                                "struct"
                            } else {
                                "alias"
                            };

                            let type_token = self.token().cloned().unwrap_or(DEFAULT_TOKEN.clone());
                            if type_token.0 != "IDENTIFIER" {
                                self.raise("SyntaxError", "Expected type name after 'typedef'");
                                return Statement::Null;
                            }

                            let name = type_token.1.clone().into_owned();
                            let name_loc = type_token.2.clone();

                            if RESERVED_KEYWORDS.contains(&name.as_ref()) {
                                self.raise("SyntaxError", &format!("'{}' is a reserved keyword and cannot be used as a type name", name));
                                return Statement::Null;
                            }

                            self.next();

                            let mut generics = Vec::<String>::new();
                            if self.token_is("OPERATOR", "<") {
                                self.next();
                                while !self.token_is("OPERATOR", ">") {
                                    let gen_token = self.token().cloned().unwrap_or(DEFAULT_TOKEN.clone());
                                    if gen_token.0 != "IDENTIFIER" {
                                        self.raise("SyntaxError", "Expected generic parameter");
                                        return Statement::Null;
                                    }
                                    generics.push(gen_token.1.clone().into_owned());
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

                            let mut variables = Vec::<String>::new();
                            let vars_start_loc = self.get_loc();

                            if self.token_is("SEPARATOR", "[") {
                                self.next();
                                while !self.token_is("SEPARATOR", "]") {
                                    let var_token = self.token().cloned().unwrap_or(DEFAULT_TOKEN.clone());
                                    if var_token.0 != "IDENTIFIER" {
                                        self.raise("SyntaxError", "Expected variable name in type declaration");
                                        return Statement::Null;
                                    }

                                    variables.push(var_token.1.clone().into_owned());
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

                            if kind != "alias" && !variables.is_empty() {
                                let help = if generics.is_empty() {
                                    "Did you mean to use '<>' for generics?"
                                } else {
                                    &format!(
                                        "Did you mean to use 'typedef {}[{}]<{}> = ...' instead?",
                                        name,
                                        generics.join(", "),
                                        variables.join(", ")
                                    )
                                };

                                self.raise_with_help("SyntaxError", "Variables not allowed in this context", help);
                                self.err.as_mut().unwrap().loc = vars_start_loc;
                                return Statement::Null;
                            }

                            if kind == "alias" && !self.token_is("OPERATOR", "=") {
                                self.raise("SyntaxError", "Expected '=' after type name");
                                return Statement::Null;
                            }
                            if self.token_is("OPERATOR", "=") {
                                self.next();
                            }

                            let mut decl = match kind {
                                "alias" => {
                                    let aliased_type = self.parse_type();
                                    if self.err.is_some() {
                                        return Statement::Null;
                                    }

                                    TypeDeclNode::Alias {
                                        base_type: Box::new(aliased_type),
                                        conditions: Vec::new(),
                                        variables,
                                    }
                                }

                                "enum" => {
                                    if !self.token_is("SEPARATOR", "{") {
                                        self.raise("SyntaxError", "Expected '{' in enum declaration");
                                        return Statement::Null;
                                    }
                                    self.next();

                                    let mut variants = Vec::<(String, Statement, usize)>::new();
                                    let mut discriminants_counter = 0;

                                    while !self.token_is("SEPARATOR", "}") {
                                        let variant_token = self.token().cloned().unwrap_or(DEFAULT_TOKEN.clone());
                                        if variant_token.0 != "IDENTIFIER" {
                                            self.raise("SyntaxError", "Expected variant name in enum");
                                            return Statement::Null;
                                        }

                                        let variant_name = variant_token.1.clone();
                                        self.next();

                                        let variant_type = if self.token_is("SEPARATOR", "(") {
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
                                                elements.remove(0)
                                            } else {
                                                let tuple_types = elements.into_iter().map(|s| {
                                                    if let Node::Type { node } = s.node {
                                                        node
                                                    } else {
                                                        self.raise("SyntaxError", "Expected type in generics");
                                                        return TypeNode::Simple { 
                                                            base: "any".to_owned(),
                                                        }
                                                    }
                                                }).collect();
                                                if self.err.is_some() {
                                                    return Statement::Null;
                                                }

                                                Statement {
                                                    node: Node::Type {
                                                        node: TypeNode::Generics {
                                                            base_type: Box::new(TypeNode::Simple { 
                                                                base: "tuple".to_owned(),
                                                            }),
                                                            generics_types: tuple_types,
                                                        }
                                                    },
                                                    loc: alloc_loc(self.get_loc()),
                                                }
                                            }
                                        } else {
                                            Statement::Null
                                        };

                                        let discriminant: usize = if self.token_is("OPERATOR", "=") {
                                            self.next();
                                            let expr = self.parse_expression();
                                            if self.err.is_some() {
                                                return Statement::Null;
                                            }
                                            match expr.node {
                                                Node::Number { value } => {
                                                    if let Some(i) = parse_usize_radix(&value) {
                                                        if i == discriminants_counter + 1 {
                                                            discriminants_counter = i + 1;
                                                            i
                                                        } else {
                                                            i
                                                        }
                                                    } else {
                                                        self.raise("TypeError", "Enum discriminant must be a non-negative integer");
                                                        return Statement::Null;
                                                    }
                                                }
                                                _ => {
                                                    self.raise("TypeError", "Enum discriminant must be a non-negative integer");
                                                    return Statement::Null;
                                                }
                                            }
                                        } else {
                                            let value = discriminants_counter;
                                            discriminants_counter += 1;
                                            value
                                        };

                                        variants.push((variant_name.to_string(), variant_type, discriminant));

                                        if self.token_is("SEPARATOR", ",") {
                                            self.next();
                                        } else if !self.token_is("SEPARATOR", "}") {
                                            self.raise("SyntaxError", "Expected ',' or '}' in enum declaration");
                                            return Statement::Null;
                                        }
                                    }

                                    self.next();

                                    TypeDeclNode::Enum {
                                        variants,
                                        wheres: Vec::new(),
                                        generics,
                                    }
                                }

                                "struct" => {
                                    if !self.token_is("SEPARATOR", "{") {
                                        self.raise("SyntaxError", "Expected '{' in struct declaration");
                                        return Statement::Null;
                                    }
                                    self.next();

                                    let mut fields = Vec::<(String, Statement, Vec<String>)>::new();

                                    while !self.token_is("SEPARATOR", "}") {

                                        let mut mods = Vec::<String>::new();
                                        while self.token().map_or(false, |t| {
                                            t.0 == "IDENTIFIER" &&
                                            ["public","private","static","non-static","final","mutable"]
                                                .contains(&t.1.as_ref())
                                        }) {
                                            mods.push(self.token().unwrap().1.clone().into_owned());
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

                                        fields.push((field_name.into_owned(), field_type, mods));

                                        if self.token_is("SEPARATOR", ",") {
                                            self.next();
                                        } else if !self.token_is("SEPARATOR", "}") {
                                            self.raise("SyntaxError", "Expected ',' or '}' in struct declaration");
                                            return Statement::Null;
                                        }
                                    }

                                    self.next();

                                    TypeDeclNode::Struct {
                                        fields,
                                        wheres: Vec::new(),
                                        generics,
                                    }
                                }

                                _ => unreachable!(),
                            };

                            let mut where_list = Vec::<(String, Statement)>::new();

                            if self.token_is("IDENTIFIER", "where") {
                                self.next();

                                if !self.token_is("SEPARATOR", "(") {
                                    self.raise("SyntaxError", "Expected '(' after 'where'");
                                    return Statement::Null;
                                }
                                self.next();

                                while !self.token_is("SEPARATOR", ")") {
                                    if kind == "alias" {
                                        let cond = self.parse_expression();
                                        if self.err.is_some() {
                                            return Statement::Null;
                                        }
                                        where_list.push(("".to_string(), cond));
                                    } else {
                                        let ident_token = self.token().cloned().unwrap_or(DEFAULT_TOKEN.clone());
                                        if ident_token.0 != "IDENTIFIER" {
                                            self.raise("SyntaxError", "Expected identifier in where statement");
                                            return Statement::Null;
                                        }
                                        let ident = ident_token.1.clone();
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

                                        where_list.push((ident.to_string(), type_));
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

                            decl = match decl {
                                TypeDeclNode::Alias { base_type, conditions: _, variables } => {
                                    TypeDeclNode::Alias {
                                        base_type,
                                        conditions: where_list.into_iter().map(|(_, s)| s).collect(),
                                        variables,
                                    }
                                }
                                TypeDeclNode::Enum { variants, wheres: _, generics } => {
                                    TypeDeclNode::Enum {
                                        variants,
                                        wheres: where_list,
                                        generics,
                                    }
                                }
                                TypeDeclNode::Struct { fields, wheres: _, generics } => {
                                    TypeDeclNode::Struct {
                                        fields,
                                        wheres: where_list,
                                        generics,
                                    }
                                }
                            };

                            Statement {
                                node: Node::TypeDeclaration {
                                    name,
                                    modifiers,
                                    node: decl,
                                },
                                loc: alloc_loc(name_loc),
                            }
                        }
                        "import" => {
                            self.next();

                            if self.token_is("NUMBER", "42") {
                                return Statement {
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
                                    alias: alias.map(|a| a.1.to_string()),
                                    named_imports: if is_named_import {
                                        let converted: Vec<Value> = named_imports
                                            .into_iter()
                                            .map(|(name, alias)| {
                                                let mut map = FxHashMap::from_iter([(
                                                    Value::String("name".to_string()),
                                                    Value::String(name.to_string()),
                                                )]);

                                                if let Some(alias) = alias {
                                                    map.insert(Value::String("alias".to_string()), Value::String(alias.to_string()));
                                                }
                                                Value::Map(map)
                                            })
                                            .collect();
                                        converted
                                    } else {
                                        Vec::new()
                                    },
                                    modifiers,
                                    import_all,
                                    module_path_opt: from_path.map(Box::new),
                                },
                                loc: alloc_loc(self.get_loc()),
                            }
                        }
                        _ => {
                            let name = self.token().cloned().map(|tok| tok.1.to_string()).unwrap_or_default();
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
                                        name: name.to_string(),
                                        var_type: Box::new(type_),
                                        val_stmt: Box::new(value),
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
                                        name: name.to_string(),
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
                            && ["public", "static", "non-static", "final", "mutable"].contains(&tok.1.as_ref())
                        {
                            global_modifiers.push(tok.1.to_string());
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
                                        .contains(&tok.1.as_ref())
                                {
                                    item_modifiers.push(tok.1.to_string());
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

                            names.push(name_token.1.to_string());
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
                                    .contains(&tok.1.as_ref())
                            {
                                item_modifiers.push(tok.1.to_string());
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

                        names.push(name_token.1.to_string());
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
                                alias_names.push(alias_token.1.to_string());
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
                            aliases = vec![alias_token.1.to_string()];
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
                    if ["end"].contains(&self.token().map(|t| t.1.as_ref()).unwrap_or("")) {
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
                    let value = self.parse_expression();
                    if self.err.is_some() {
                        return Statement::Null;
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
                            let name = token.1.to_string();
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
                                    locals.push(tok.1.to_string());
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
                            MatchCase::Pattern {
                                pattern: PathElement::Path {
                                    segments: vec!["_".to_owned()],
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
                                condition: Box::new(condition),
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
                                                pats.push(p);
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
                                condition: Box::new(condition),
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
                    let mut values = Vec::new();

                    if let Some(t) = self.token() {
                        if t.0 == "SEPARATOR" && t.1 == ")" {
                            self.next();

                            if let Some(assign) = self.token() {
                                if assign.0 == "OPERATOR" && (assign.1 == ":=" || assign.1 == "=") {
                                    self.raise("SyntaxError", "Cannot unpack into an empty tuple");
                                    return Statement::Null;
                                }
                            }

                            return Statement {
                                node: Node::Iterable {
                                    node: IterableNode::Tuple { elements: vec![] }
                                },
                                loc: alloc_loc(self.get_loc()),
                            };
                        }
                    }

                    self.parse_var_decl = true;
                    let first = self.parse_expression();
                    values.push(first);

                    let mut is_tuple = false;
                    while let Some(t) = self.token() {
                        if t.0 == "SEPARATOR" && t.1 == "," {
                            is_tuple = true;
                            self.next();

                            if let Some(next) = self.token() {
                                if next.0 == "SEPARATOR" && next.1 == ")" {
                                    break;
                                }
                            }

                            let expr = self.parse_expression();
                            values.push(expr);
                        } else {
                            break;
                        }
                    }

                    self.check_for("SEPARATOR", ")");
                    self.next();

                    if let Some(assign) = self.token() {
                        if assign.0 == "OPERATOR" && assign.1 == ":=" {
                            self.next();

                            if !is_valid_token(&self.token().cloned()) {
                                let loc = self.get_loc().unwrap();
                                self.raise_with_loc("SyntaxError", "Unexpected end of input after ':='", loc);
                                return Statement::Null;
                            }

                            let value = self.parse_expression();
                            if self.err.is_some() {
                                return Statement::Null;
                            }

                            let mut targets = Vec::with_capacity(values.len());

                            for v in values.into_iter() {
                                match &v.node {
                                    Node::Variable { name } => {
                                        targets.push(Statement {
                                            node: Node::VariableDeclaration {
                                                name: name.clone(),
                                                val_stmt: Box::new(Statement {
                                                    node: Node::Boolean {
                                                        value: None,
                                                    },
                                                    loc: alloc_loc(self.get_loc()),
                                                }),
                                                var_type: Box::new(Statement {
                                                    node: Node::Type {
                                                        node: TypeNode::Simple { base: "any".to_owned() }
                                                    },
                                                    loc: alloc_loc(self.get_loc()),
                                                }),
                                                modifiers: Vec::new(),
                                                is_default: false,
                                            },
                                            loc: v.loc,
                                        });
                                    }

                                    Node::VariableDeclaration { .. } => {
                                        targets.push(v);
                                    }

                                    _ => {
                                        self.raise("SyntaxError", "Unpack targets must be variables or variable declarations");
                                        return Statement::Null;
                                    }
                                }
                            }

                            return Statement {
                                node: Node::UnpackAssignment {
                                    targets,
                                    stmt: Box::new(value),
                                },
                                loc: alloc_loc(self.get_loc()),
                            };
                        }
                    }

                    if is_tuple || values.len() != 1 {
                        Statement {
                            node: Node::Iterable {
                                node: IterableNode::Tuple { elements: values },
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
                let mut segments: Vec<String> = vec![val.to_string()];
                self.next();

                while self.token_is("SEPARATOR", ".") {
                    self.next();
                    if let Some(Token(kind, seg, _)) = self.token().cloned() {
                        if kind == "IDENTIFIER" {
                            self.next();
                            if RESERVED_KEYWORDS.contains(&seg.as_ref()) {
                                self.raise("SyntaxError", &format!("'{}' is a reserved keyword and cannot be used in paths", seg));
                                return None;
                            }
                            segments.push(seg.to_string());
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
                match val.as_ref() {
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
        let token = self.token().cloned().unwrap_or(DEFAULT_TOKEN.clone());
        let is_tuple = token.0 == "SEPARATOR" && token.1 == "(";

        if is_tuple {
            self.next();
            let mut elements = Vec::new();
            while !self.token_is("SEPARATOR", ")") {
                let elem = self.parse_type();
                if self.err.is_some() {
                    return None;
                }
                if let Node::Type { node } = &elem.node {
                    elements.push(node.clone());
                } else {
                    self.raise("SyntaxError", "Expected type in tuple elements");
                    return None;
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
                        base_type: Box::new(TypeNode::Simple {
                            base: "tuple".to_string(),
                        }),
                        generics_types: elements,
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
    
        let name = token.1.to_string();
    
        Some((
            name.clone(),
            Statement {
                node: Node::Type {
                    node: TypeNode::Simple {
                        base: name.clone(),
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
                        mods.push(v.to_string());
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
            let func_name = name_token.unwrap().1.to_string();
            self.next();

            if !self.token_is("SEPARATOR", "[") {
                joins.push(func_name.to_string());
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

            let ret_type_node: TypeNode = if self.token_is("OPERATOR", "->") {
                self.next();
                let r = self.parse_type();
                if r == Statement::Null {
                    return Statement::Null;
                }
                match r.node {
                    Node::Type { node } => node,
                    _ => return self.raise("SyntaxError", "Expected return type after '->'"),
                }
            } else {
                TypeNode::Simple {
                    base: "any".to_string(),
                }
            };

            impls.push((
                func_name.to_string(),
                arg_types,
                mods,
                Box::new(ret_type_node),
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

    pub fn parse_type(&mut self) -> Statement {
        let mut modifiers: Vec<isize> = vec![];
        let mut current_ref_level: usize = 0;
        while self.token_is("OPERATOR", "&") || self.token_is("OPERATOR", "?") || self.token_is("OPERATOR", "&&") || self.token_is("OPERATOR", "??") {
            if self.token_is("OPERATOR", "&") {
                current_ref_level += 1;
            } else if self.token_is("OPERATOR", "&&") {
                current_ref_level += 2;
            } else if self.token_is("OPERATOR", "?") || self.token_is("OPERATOR", "??") {
                if current_ref_level > 0 {
                    modifiers.push(current_ref_level as isize);
                    current_ref_level = 0;
                }
                if modifiers.iter().any(|m| *m == -1) {
                    self.raise("SyntaxError", "Type can only be marked as maybe once");
                    return Statement::Null;
                }
                modifiers.push(-1);
            }
            self.next();
        }
        if current_ref_level > 0 {
            modifiers.push(current_ref_level as isize);
        }

        let (base_str, base_stmt) = if self.token_is("IDENTIFIER", "impl") {
            ("impl".to_string(), self.parse_impl())
        } else {
            match self.parse_single_type() {
                Some(v) => v,
                None => return Statement::Null,
            }
        };

        if self.token_is("SEPARATOR", "(") {
            let mut to_type_node = TypeNode::Simple {
                base: base_str.to_owned(),
            };
            for modifier in modifiers.clone().into_iter().rev() {
                to_type_node = match modifier {
                    m if m > 0 => TypeNode::Reference { base_type: Box::new(to_type_node), ref_level: m as usize },
                    -1 => TypeNode::Maybe { base_type: Box::new(to_type_node) },
                    _ => to_type_node,
                };
            }
            let to_type = Statement {
                node: Node::Type {
                    node: to_type_node,
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

            return Statement {
                node: Node::TypeConvert {
                    node: Box::new(value),
                    target_type: Box::new(to_type),
                },
                loc: alloc_loc(self.get_loc()),
            };
        }

        let mut current_type_node = if let Node::Type { node } = &base_stmt.node {
            node.clone()
        } else {
            self.raise("SyntaxError", "Expected type");
            return Statement::Null;
        };

        if self.token_is("SEPARATOR", "[") {
            let inner_loc = alloc_loc(self.get_loc());

            if base_str == "function" || base_str == "generator" {
                let mut param_types = Vec::new();

                if self.token_is("SEPARATOR", "[") {
                    self.next();
                    while !self.token_is("SEPARATOR", "]") {
                        let arg_type = self.parse_type();
                        let tt = match arg_type.node {
                            Node::Type { node } => node,
                            _ => {
                                self.raise("SyntaxError", "Expected parameter type");
                                return Statement { node: Node::Type { node: TypeNode::Simple { base: "any".to_string() } }, loc: inner_loc };
                            }
                        };
                        param_types.push(tt);

                        if self.token_is("SEPARATOR", ",") {
                            self.next();
                        } else if self.token_is("SEPARATOR", "]") {
                            break;
                        } else {
                            self.raise("SyntaxError", "Expected ',' or ']' in parameter list");
                            return Statement { node: Node::Type { node: TypeNode::Simple { base: "any".to_string() } }, loc: inner_loc };
                        }
                    }
                    self.next();
                }

                let return_type = if self.token_is("OPERATOR", "->") {
                    self.next();
                    let rt = self.parse_type();
                    let tt = match rt.node {
                        Node::Type { node } => node,
                        _ => {
                            self.raise("SyntaxError", "Expected return type after '->'");
                            return Statement { node: Node::Type { node: TypeNode::Simple { base: "any".to_string() } }, loc: inner_loc };
                        }
                    };
                    Box::new(tt)
                } else {
                    Box::new(TypeNode::Simple { base: "any".to_string() })
                };

                current_type_node = if base_str == "function" {
                    TypeNode::Function { parameters_types: param_types, return_type }
                } else {
                    TypeNode::Generator { parameters_types: param_types, yield_type: return_type }
                };
            } else {
                self.next();
                let mut elements = Vec::new();

                while !self.token_is("SEPARATOR", "]") {
                    if self.token_is("OPERATOR", "...") {
                        self.next();
                        elements.push(TypeNode::Variadics {
                            base: Box::new(TypeNode::Simple {
                                base: "any".to_string(),
                            }),
                        });

                        if self.token_is("SEPARATOR", "]") {
                            break;
                        }
                        if self.token_is("SEPARATOR", ",") {
                            self.next();
                            continue;
                        }
                        self.raise("SyntaxError", "Expected ',' or ']' after variadic marker");
                        return Statement::Null;
                    }

                    let elem_type_stmt = self.parse_type();
                    if elem_type_stmt == Statement::Null {
                        return Statement::Null;
                    }

                    let elem_type_node = if let Node::Type { node } = elem_type_stmt.node {
                        node
                    } else {
                        self.raise("SyntaxError", "Expected a type inside type list");
                        return Statement::Null;
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
                        
                        for _ in 0..count {
                            elements.push(elem_type_node.clone());
                        }
                    } else if self.token_is("SEPARATOR", ",") {
                        elements.push(elem_type_node);
                        self.next();
                    } else if self.token_is("SEPARATOR", "]") {
                        elements.push(elem_type_node);
                        break;
                    } else {
                        self.raise("SyntaxError", "Expected ',' or ']' after type element");
                        return Statement::Null;
                    }
                }

                self.next();

                current_type_node = TypeNode::Generics {
                    base_type: Box::new(current_type_node),
                    generics_types: elements,
                };
            }
        }

        let mut union_types = vec![current_type_node.clone()];

        while self.token_is("OPERATOR", "|") {
            self.next();
            let next_type = self.parse_type();
            if next_type == Statement::Null {
                return Statement::Null;
            }

            match &next_type.node {
                Node::Type { node: TypeNode::Union { types } } => {
                    for t in types {
                        union_types.push(t.clone());
                    }
                }
                Node::Type { node } => {
                    union_types.push(node.clone());
                }
                _ => {
                    self.raise("SyntaxError", "Expected type in union");
                    return Statement::Null;
                }
            }
        }

        let final_type_node = if union_types.len() > 1 {
            TypeNode::Union {
                types: union_types,
            }
        } else {
            current_type_node
        };

        let mut wrapped_type_node = final_type_node;
        for modifier in modifiers.into_iter().rev() {
            wrapped_type_node = match modifier {
                m if m > 0 => TypeNode::Reference { base_type: Box::new(wrapped_type_node), ref_level: m as usize },
                -1 => TypeNode::Maybe { base_type: Box::new(wrapped_type_node) },
                _ => wrapped_type_node,
            };
        }

        Statement {
            node: Node::Type {
                node: wrapped_type_node,
            },
            loc: alloc_loc(self.get_loc()),
        }
    }

    fn parse_variable(&mut self) -> Statement {
        let token = self.token().cloned().unwrap_or(DEFAULT_TOKEN.clone());
        let loc = self.get_loc();

        if token.0 == "IDENTIFIER" {
            let name = token.1.to_string();
            if RESERVED_KEYWORDS.contains(&name.as_ref()) {
                self.raise("SyntaxError", &format!("'{}' is a reserved keyword and cannot be used as a variable name", name));
                return Statement::Null;
            }
            self.next();
            if self.token_is("SEPARATOR", ":") && self.parse_var_decl {
                self.next();
                let type_token = self.token().cloned().unwrap_or(DEFAULT_TOKEN.clone());
                if !(type_token.0 == "IDENTIFIER" || type_token.0 == "OPERATOR" || (type_token.0 == "SEPARATOR" && ["(", "<", "["].contains(&type_token.1.as_ref()))) {
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
                        var_type: Box::new(type_),
                        val_stmt: Box::new(value),
                        modifiers: vec![],
                        is_default,
                    },
                    loc: alloc_loc(loc),
                };
            } else if self.token_is("OPERATOR", ":=") && self.parse_var_decl {
                let name = token.1.to_string();
                self.next();
                let type_ = Statement {
                    node: Node::Type {
                        node: TypeNode::Simple {
                            base: "auto".to_owned(),
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
                        name: name.to_string(),
                        var_type: Box::new(type_),
                        val_stmt: Box::new(value),
                        modifiers: vec![],
                        is_default: false,
                    },
                    loc: alloc_loc(loc),
                };
            } else {
                return Statement {
                    node: Node::Variable {
                        name: name.to_string(),
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
        let next_token_type = next_token.map(|t| t.0.clone()).unwrap_or_else(|| Cow::Borrowed(""));
        let next_token_value = next_token.map(|t| t.1.clone()).unwrap_or_else(|| Cow::Borrowed(""));
        
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
                return values[0].clone();
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
                    value: token_value.to_string(),
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
        
                let mods: Vec<char> = prefix_str
                    .chars()
                    .filter(|ch| valid_mods.contains(ch))
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
                        value: token_value.to_string(),
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
                let mods: Vec<char> = prefix_str
                    .chars()
                    .filter(|ch| valid_mods.contains(ch))
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
                        value: token_value.to_string(),
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
                    value: match token_value.as_ref() {
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
    
        let mut elements = Vec::with_capacity(4); // lets say 4
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
                                        end: Box::new(Statement::Null),
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
                        expr
                    } else {
                        let expr = self.parse_expression();
                        if self.err.is_some() {
                            return Statement::Null;
                        }
                        expr
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
                            end: Box::new(end_expr),
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
                        filters.push(cond);
                    }

                    for_clauses.push((variables, iterable, filters));
                }

                self.check_for("SEPARATOR", "]");
                self.next();

                return Statement {
                    node: Node::Iterable {
                        node: IterableNode::ListComprehension {
                            for_clauses,
                            map_expression: Box::new(expr),
                        },
                    },
                    loc: alloc_loc(loc),
                };
            }
            
            elements.push(expr);
    
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
        let name = token.1.to_string();
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
                    name: name.to_string(),
                    pos_args: Vec::new(),
                    named_args: HashMap::new(),
                },
                loc: alloc_loc(loc),
            }
        }

        Statement {
            node: Node::Call {
                name: name.to_string(),
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
                return (vec![], HashMap::default());
            }
    
            if current_token.0 == "SEPARATOR" && current_token.1 == ")" {
                break;
            }
    
            if let Some(Token(t_type, t_val, _)) = &next_token {
                if t_type == "OPERATOR" && t_val == "=" {
                    let name = current_token.1.to_string();
                    if RESERVED_KEYWORDS.contains(&name.as_ref()) {
                        self.raise("SyntaxError", &format!("'{}' is a reserved keyword and cannot be used as a named argument", name));
                        return (vec![], HashMap::default());
                    }
                    self.next();
                    self.next();
                    let value = self.parse_expression();
                    if self.err.is_some() {
                        return (vec![], HashMap::default());
                    }
                    named_args.insert(name.to_string(), value);
                    seen_named = true;
                } else {
                    if seen_named {
                        self.raise("ArgumentOrderError", "Positional arguments cannot follow named arguments");
                        return (vec![], HashMap::default());
                    }
                    let expr = self.parse_expression();
                    if self.err.is_some() {
                        return (vec![], HashMap::default());
                    }
                    pos_args.push(expr);
                }
            } else {
                if seen_named {
                    self.raise("ArgumentOrderError", "Positional arguments cannot follow named arguments");
                    return (vec![], HashMap::default());
                }
                let expr = self.parse_expression();
                if self.err.is_some() {
                    return (vec![], HashMap::default());
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
