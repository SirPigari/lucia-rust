use crate::env::runtime::tokens::{Token, Location};
use crate::env::runtime::value::Value;
use crate::env::runtime::errors::Error;
use crate::interpreter::Interpreter;
use crate::parser::Parser;
use crate::lexer::Lexer;
use crate::env::runtime::config::Config;

use std::path::PathBuf;
use once_cell::sync::OnceCell;
use std::sync::Mutex;

static INTERPRETER: OnceCell<Mutex<Interpreter>> = OnceCell::new();

pub fn interpret(input: &str) -> Result<Value, Error> {
    let mut lexer = Lexer::new(input, "<internal>", None);
    let tokens = lexer.tokenize();
    
    let mut parser = Parser::new(tokens);
    let ast = parser.parse_safe()?;
    
    let interpreter_mutex = INTERPRETER.get_or_init(|| Mutex::new(Interpreter::new(
        Config::default(),
        true,
        "<internal>",
        &PathBuf::from("<internal>"),
        (PathBuf::from("<internal>"), PathBuf::from("<internal>"), true),
        &[],
    )));
    
    let mut interpreter = interpreter_mutex.lock().unwrap();
    interpreter.interpret(ast, false)
}

pub fn precompile(tokens: Vec<Token>) -> Result<Vec<Token>, Error> {
    if tokens.is_empty() {
        return Err(Error::new(
            "PrecompileError",
            "precompile: empty expression",
            "<internal>",
        ));
    }

    let loc = tokens[0].2.clone();

    let ast = {
        let mut parser = Parser::new(tokens.clone());
        match parser.parse_safe() {
            Ok(ast) => ast,
            Err(e) => {
                return Err(Error::with_ref(
                    "PrecompileError",
                    "precompile: parse error",
                    e,
                    "<internal>",
                ));
            }
        }
    };

    let internal_path = PathBuf::from("<internal>");
    let internal_path_ref = &internal_path;

    let result = {
        let interpreter_mutex = INTERPRETER.get_or_init(|| Mutex::new(Interpreter::new(
            Config::default(),
            true,
            "<internal>",
            internal_path_ref,
            (internal_path_ref.clone(), internal_path_ref.clone(), true),
            &[],
        )));
        let mut interpreter = interpreter_mutex.lock().unwrap();
        match interpreter.interpret(ast, false) {
            Ok(value) => value,
            Err(e) => {
                return Err(Error::with_ref(
                    "PrecompileError",
                    "precompile: interpretation error",
                    e,
                    "<internal>",
                ));
            }
        }
    };

    fn get_token(value: &Value, loc: Option<Location>) -> Option<Token> {
        match value {
            Value::Int(n) => Some(Token("NUMBER".into(), n.to_string(), loc)),
            Value::Float(f) => Some(Token("NUMBER".into(), f.to_string(), loc)),
            Value::Boolean(b) => Some(Token("BOOLEAN".into(), b.to_string(), loc)),
            Value::String(s) => Some(Token("STRING".into(), format!("{:?}", s), loc)),
            Value::Null => Some(Token("BOOLEAN".into(), "null".into(), loc)),
            _ => None,
        }
    }

    let token = match result {
        Value::Int(_) | Value::Float(_) | Value::Boolean(_) | Value::String(_) | Value::Null => {
            if let Some(token) = get_token(&result, loc) {
                vec![token]
            } else {
                return Err(Error::new(
                    "PrecompileError",
                    "precompile: unsupported value type",
                    "<internal>",
                ));
            }
        }
        Value::List(list) => {
            let mut tokens = Vec::new();
            tokens.push(Token("SEPARATOR".into(), "[".into(), loc.clone()));
            for (i, item) in list.iter().enumerate() {
                if let Some(token) = get_token(item, loc.clone()) {
                    tokens.push(token);
                } else {
                    return Err(Error::new(
                        "PrecompileError",
                        "precompile: unsupported value type in list",
                        "<internal>",
                    ));
                }
                if i != list.len() - 1 {
                    tokens.push(Token("SEPARATOR".into(), ",".into(), loc.clone()));
                }
            }
            tokens.push(Token("SEPARATOR".into(), "]".into(), loc));
            tokens
        }
        Value::Tuple(tuple) => {
            let mut tokens = Vec::new();
            tokens.push(Token("SEPARATOR".into(), "(".into(), loc.clone()));
            for (i, item) in tuple.iter().enumerate() {
                if let Some(token) = get_token(item, loc.clone()) {
                    tokens.push(token);
                } else {
                    return Err(Error::new(
                        "PrecompileError",
                        "precompile: unsupported value type in tuple",
                        "<internal>",
                    ));
                }
                if i != tuple.len() - 1 {
                    tokens.push(Token("SEPARATOR".into(), ",".into(), loc.clone()));
                }
            }
            tokens.push(Token("SEPARATOR".into(), ")".into(), loc));
            tokens
        }
        _ => {
            return Err(Error::new(
                "PrecompileError",
                "precompile: unsupported value type",
                "<internal>",
            ));
        }
    };

    Ok(token)
}
