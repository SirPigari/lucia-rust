#![cfg_attr(target_arch = "wasm32", allow(dead_code))]
use crate::env::runtime::tokens::{Token, Location};
use crate::env::runtime::value::Value;
use crate::env::runtime::errors::Error;
use crate::env::runtime::utils::{sanitize_alias, fix_path};
use crate::interpreter::Interpreter;
use crate::parser::Parser;
use crate::lexer::Lexer;
use crate::env::runtime::config::Config;

use std::fs;
use regex::Regex;
use std::path::PathBuf;
use once_cell::sync::OnceCell;
use std::sync::Mutex;


static INTERPRETER: OnceCell<Mutex<Interpreter>> = OnceCell::new();

// private final __<link_name>__<library>__cdef: &int = libload.get_fn(<library>, <link_name>, <link_parameters>, <link_return_type>)
// final fun <name>(<parameters>) -> <return_type>: 
//     if (<__<link_name>__<library>__cdef> == null):
//         throw "Function '<name>' not found in the <library> library." from "FunctionNotFoundError"
//     end
//     result: any = libload.call_fn(<__<link_name>__<library>__cdef>, [<parameters>])
//     ... // process result to convert to <return_type>
//     return result as <return_type>
// end
#[derive(Debug, Clone)]
pub struct CHeaderSignature {
    pub name: String,
    pub link_name: String,
    pub return_type: String,
    pub link_return_type: String,
    pub parameters: Vec<(String, String)>, // (name, type)
    pub link_parameters: Vec<String>,  // type
    pub library: String,
    pub library_name: String,
}

pub fn map_ctyle_to_cltype(ctype: &str) -> Result<String, String> {
    match ctype {
        "int" | "long" | "short" | "unsigned int" | "unsigned long" | "unsigned short" => Ok("int".into()),
        "float" | "double" => Ok("float".into()),
        "char*" | "const char*" => Ok("str".into()),
        "bool" | "_Bool" => Ok("bool".into()),
        "char" => Ok("int".into()),
        "void" => Ok("void".into()),
        _ => Err(format!("Unsupported C type: {}", ctype)),
    }
}

pub fn transform_result_to_type(result: &str, return_type: &str) -> Result<String, String> {
    match return_type {
        "int" => Ok(format!("{} as int", result)),
        "float" => Ok(format!("{} as float", result)),
        "str" => Ok(format!("libload.parse_str_ptr({}) as str", result)),
        "bool" => Ok(format!("{} != 0", result)),
        "void" => Ok("null".into()),
        _ => Err(format!("Unsupported return type: {}", return_type)),
    }
}

pub fn transform_param_to_type(param: &str, param_type: &str) -> Result<String, String> {
    match param_type {
        "int" => Ok(format!("{} as int", param)),
        "float" => Ok(format!("{} as float", param)),
        "str" => Ok(format!("libload.create_str_ptr({})", param)),
        "bool" => Ok(param.to_string()),
        "void" => Ok("null".into()),
        _ => Err(format!("Unsupported parameter type: {}", param_type)),
    }
}

pub fn generate_library_definition(library_name: &str, library_path: &PathBuf, loc: &Option<Location>) -> Vec<Token> {
    let lib = library_path.canonicalize().unwrap_or_else(|_| library_path.clone());
    let lib_def = format!(
        "private final libload_{}: &int = libload.load_lib(\"{}\")",
        library_name,
        fix_path(lib.display().to_string())
    );

    let tokens_lib_def = Lexer::new(&lib_def, "<internal>").tokenize().iter().filter(|t| t.0 != "EOF").cloned().collect::<Vec<Token>>();
    tokens_lib_def.into_iter().map(|mut t| { t.2 = loc.clone(); t }).collect()
}

pub fn generate_lsign_from_c_sign(signature: &CHeaderSignature) -> (Vec<Token>, Vec<Token>) {
    let cdef_name = format!("__{}__{}__cdef", signature.link_name, signature.library_name);
    
    let get_fn_def = format!(
        "private final {}: &int = libload.get_fn(libload_{}, \"{}\", [{}], \"{}\")",
        cdef_name,
        signature.library_name,
        signature.link_name,
        signature.link_parameters.join(", "),
        signature.link_return_type
    );

    let final_fun_def = {
        let params: Vec<String> = signature
            .parameters
            .iter()
            .map(|(name, ctype)| format!("{}: {}", name, ctype))
            .collect();
        let call_params: Vec<String> = signature.parameters.iter().map(|(name, _)| name.clone()).collect();
        let mut call_params_formatted: Vec<String> = Vec::new();
        for (i, param) in call_params.iter().enumerate() {
            call_params_formatted.push(format!("{}", transform_param_to_type(param, &signature.link_parameters[i]).unwrap_or(param.to_string())));
        }
        let transformed_result =
            transform_result_to_type("result", &signature.return_type).unwrap_or("result".into());
        format!(
            "final fun {}({}) -> {}:\n
                if ({} == null):\n
                    throw \"Function '{}' not found in the {} library.\" from \"FunctionNotFoundError\"\n
                end\n
                result: any = libload.call_fn({}, [{}])\n
                return {}\n
            end",
            signature.name,
            params.join(", "),
            signature.return_type,
            cdef_name,
            signature.name,
            signature.library,
            cdef_name,
            call_params_formatted.join(", "),
            transformed_result
        )
    };

    let tokens_get_fn_def = Lexer::new(&get_fn_def, "<internal>").tokenize().iter().filter(|t| t.0 != "EOF").cloned().collect();
    let tokens_final_fun_def = Lexer::new(&final_fun_def, "<internal>").tokenize().iter().filter(|t| t.0 != "EOF").cloned().collect();

    (tokens_get_fn_def, tokens_final_fun_def)
}

pub fn get_defs_from_c_header(path: &PathBuf, library: &PathBuf) -> Result<Vec<CHeaderSignature>, String> {
    let content = fs::read_to_string(path)
        .map_err(|e| format!("Failed to read file: {}", e))?;

    let func_regex = Regex::new(r"(?m)^\s*(?P<ret>[a-zA-Z_][a-zA-Z0-9_ \*]*)\s+(?P<name>[a-zA-Z_][a-zA-Z0-9_]*)\s*\((?P<params>[^\)]*)\)\s*;")
        .map_err(|e| format!("Failed to compile regex: {}", e))?;

    let mut signatures = Vec::new();

    for cap in func_regex.captures_iter(&content) {
        let ret = cap.name("ret").unwrap().as_str().trim();
        let name = cap.name("name").unwrap().as_str().trim();
        let params_str = cap.name("params").unwrap().as_str().trim();

        let mut parameters = Vec::new();
        let mut link_parameters = Vec::new();

        if params_str != "void" && !params_str.is_empty() {
            for param in params_str.split(',') {
                let param = param.trim();
                if param.is_empty() { continue; }

                if let Some(last_space) = param.rfind(' ') {
                    let type_part = &param[..last_space].trim();
                    let name_part = &param[last_space+1..].trim();

                    parameters.push((
                        name_part.to_string(),
                        map_ctyle_to_cltype(type_part).unwrap_or(type_part.to_string())
                    ));
                    link_parameters.push(map_ctyle_to_cltype(type_part)?);
                } else {
                    parameters.push((
                        "".to_string(),
                        map_ctyle_to_cltype(param).unwrap_or(param.to_string())
                    ));
                    link_parameters.push(map_ctyle_to_cltype(param)?);
                }
            }
        }

        let library_canonicalized = fs::canonicalize(&library)
            .map_err(|e| format!("Failed to canonicalize library path {}: {}", library.display(), e))?;

        let library_str = fix_path(library_canonicalized.display().to_string());

        signatures.push(CHeaderSignature {
            name: name.to_string(),
            link_name: name.to_string(),
            return_type: map_ctyle_to_cltype(ret)?,
            link_return_type: map_ctyle_to_cltype(ret)?,
            parameters,
            link_parameters,
            library: library_str,
            library_name: sanitize_alias(&library.file_stem().unwrap().to_string_lossy()).to_string(),
        });
    }

    Ok(signatures)
}

pub fn interpret(input: &str) -> Result<Value, Error> {
    let lexer = Lexer::new(input, "<internal>");
    let tokens = lexer.tokenize();
    
    let mut parser = Parser::new(tokens);
    let ast = parser.parse_safe()?;
    
    let interpreter_mutex = INTERPRETER.get_or_init(|| Mutex::new(Interpreter::new(
        Config::default(),
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