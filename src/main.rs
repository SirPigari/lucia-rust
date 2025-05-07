use std::env as std_env;
use std::fs::{self, File};
use std::io::{self, Read};
use std::path::{Path, PathBuf};
use serde::{Serialize, Deserialize};
use std::process::exit;
use std::collections::HashMap;

mod env {
    pub mod helpers {
        pub mod utils;
        pub mod config;
    }
}

mod interpreter;
mod parser;
mod lexer;

use crate::env::helpers::utils;
use crate::env::helpers::config::{Config, CodeBlocks, ColorScheme};
use crate::utils::{hex_to_ansi, Value, get_line_info, Error};
use crate::parser::{Parser, Token};
use crate::lexer::Lexer;
use crate::interpreter::Interpreter;

const VERSION: &str = "1.3.1";

fn handle_error(error: &Error, source: &str, line_number: usize, config: &Config, use_colors: bool, file_name: Option<&str>) {
    //-> File '<stdin>' got traceback:
    //    NameError: Name 'asdas' is not defined.
    let file_name = file_name.unwrap_or("<stdin>");
    let line = get_line_info(source, line_number)
        .unwrap_or_else(|| "<line not found>".to_string());

    let formatted = format!(
        "{}File '{}' got tracebock:\n\t{}{}",
        hex_to_ansi(&config.color_scheme.exception, Some(use_colors)),
        line_number,
        hex_to_ansi("reset", Some(use_colors)),
        line.trim()
    );

    eprintln!("{}", formatted);
}



fn format_value(value: &Value) -> String {
    match value {
        Value::Number(n) => format!("{}", n),
        Value::String(s) => format!("\"{}\"", s),
        Value::Boolean(b) => format!("{}", b),
        Value::Null => "null".to_string(),
        Value::Map { keys, values } => {
            let formatted_pairs: Vec<String> = keys
                .iter()
                .zip(values.iter()) // Zips keys with corresponding values
                .map(|(key, value)| format!("{}: {}", format_value(key), format_value(value))) // Formats each pair
                .collect();
            format!("{{{}}}", formatted_pairs.join(", ")) // Joins all pairs with commas
        },
        Value::List(values) => {
            if values.is_empty() {
                "[]".to_string() // Format empty lists as "[]"
            } else {
                let formatted_values: Vec<String> = values.iter().map(|v| format_value(v)).collect();
                format!("[{}]", formatted_values.join(", ")) // Format non-empty lists as "[value1, value2, ...]"
            }
        },
        Value::ListCompletion { pattern, end } => {
            let formatted_pattern: Vec<String> = pattern.iter().map(|v| format_value(v)).collect();
            let formatted_end = match end {
                Some(e) => format_value(e),
                None => "None".to_string(),
            };
            format!(
                "ListCompletion {{ pattern: [{}], end: {} }}",
                formatted_pattern.join(", "),
                formatted_end
            )
        }
    }
}



fn debug_log(message: &str, config: &Config, use_colors: Option<bool>) {
    let use_colors = use_colors.unwrap_or(true);
    if config.debug {
        utils::print_colored(message, &config.color_scheme.debug, Some(use_colors));
    }
}

fn load_config(path: &Path) -> Result<Config, String> {
    let mut file = File::open(path).map_err(|_| "Config file not found")?;
    let mut contents = String::new();
    let _bytes_read = file.read_to_string(&mut contents).map_err(|_| "Failed to read config file")?;

    match serde_json::from_str::<Config>(&contents) {
        Ok(config) => Ok(config),
        Err(e) => Err(format!("Failed to deserialize JSON: {}", e)),
    }
}

fn activate_environment(env_path: &Path) -> io::Result<()> {
    if !env_path.exists() {
        fs::create_dir_all(env_path)?;
    }
    if !env_path.is_dir() {
        return Err(io::Error::new(io::ErrorKind::InvalidInput, "Path is not a directory"));
    }

    let mut env_path_str = env_path.to_str().unwrap_or(".").to_string();
    if env_path_str.starts_with("\\\\?\\") {
        env_path_str = env_path_str[4..].to_string();
    }
    
    let default_config = Config {
        version: VERSION.to_string(),
        moded: false,
        debug: false,
        debug_mode: "normal".to_string(),
        supports_color: true,
        use_lucia_traceback: true,
        warnings: true,
        use_predefs: true,
        print_comments: false,
        allow_fetch: true,
        execute_code_blocks: CodeBlocks {
            C: true,
            ASM: true,
            PY: true,
        },
        lucia_file_extensions: vec![
            ".lucia".to_string(),
            ".luc".to_string(),
            ".lc".to_string(),
            ".l".to_string(),
        ],
        home_dir: env_path_str,
        recursion_limit: 9999,
        color_scheme: ColorScheme {
            exception: "#F44350".to_string(),
            warning: "#FFC107".to_string(),
            debug: "#434343".to_string(),
            comment: "#757575".to_string(),
            input_arrows: "#136163".to_string(),
            input_text: "#BCBEC4".to_string(),
            output_text: "#BCBEC4".to_string(),
            info: "#9209B3".to_string(),
        },
    };

    let config_path = env_path.join("config.json");

    let config_str = serde_json::to_string_pretty(&default_config)
        .map_err(|_| io::Error::new(io::ErrorKind::InvalidData, "Failed to serialize config"))?;

    fs::write(&config_path, config_str)
        .map_err(|_| io::Error::new(io::ErrorKind::Other, "Failed to write config file"))?;

    Ok(())
}

fn main() {
    if let Ok(exec_path) = std_env::current_exe() {
        if let Some(ext) = exec_path.extension() {
            if ext == "exe" {
                if !exec_path.components().any(|comp| comp.as_os_str() == "bin") {
                    eprintln!("Executable is not located in a 'bin' directory. Exiting.");
                    exit(1);
                }
            }
        }
    }
    let working_dir = std_env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
    if let Err(e) = std_env::set_current_dir(&working_dir) {
        eprintln!("Failed to change the directory: {}", e);
    }

    let env_path = working_dir.join("..").canonicalize().unwrap_or_else(|_| working_dir.clone());

    let args: Vec<String> = std_env::args().collect();
    let activate_flag = args.contains(&"--activate".to_string());
    let no_color_flag = args.contains(&"--no-color".to_string());

    let use_colors = !no_color_flag;

    let config_path = env_path.join("config.json");

    let config = if config_path.exists() {
        match load_config(&config_path) {
            Ok(config) => config,
            Err(_) => {
                eprintln!("Failed to read config file, activating environment...");
                if let Err(err) = activate_environment(&env_path) {
                    eprintln!("Failed to activate environment: {}", err);
                    exit(1);
                }
                load_config(&config_path).unwrap()
            }
        }
    } else {
        eprintln!("Config file does not exist and no --activate flag passed.");
        exit(1);
    };
    if activate_flag {
        if let Err(err) = activate_environment(&env_path) {
            eprintln!("Failed to activate environment: {}", err);
            exit(1);
        }
        load_config(&config_path).unwrap();
    };

    if let Err(e) = std_env::set_current_dir(&config.home_dir) {
        eprintln!("Failed to change the directory: {}", e);
    }



    let args: Vec<String> = std_env::args().collect();

    let non_flag_args: Vec<String> = args.iter()
        .skip(1)
        .filter(|arg| !arg.starts_with('-'))
        .cloned()
        .filter_map(|arg| {

            let mut path: PathBuf = Path::new(&arg).to_path_buf();

            if path.is_relative() {
                path = env_path.join(path);
            }

            match fs::canonicalize(path) {
                Ok(canon_path) => {
                    Some(canon_path.to_string_lossy().to_string())
                },
                Err(_) => {
                    Some(arg.clone())
                },
            }
        })
        .collect();
    
    if !non_flag_args.is_empty() {
        for file_path in non_flag_args {
            let path = Path::new(&file_path);
    
            if path.exists() && path.is_file() {
                debug_log(&format!("Executing file: {:?}", path), &config, Some(use_colors));

                let file_content = fs::read_to_string(path).expect("Failed to read file");
                let lexer = Lexer::new(&file_content);
                let raw_tokens = lexer.tokenize(config.print_comments);
                debug_log(
                    &format!(
                        "Tokens: {:?}",
                        raw_tokens
                            .iter()
                            .filter(|token| token.0 != "WHITESPACE")  // Filter out whitespace tokens
                            .collect::<Vec<_>>()  // Collect filtered tokens into a new Vec
                    ),
                    &config,
                    Some(use_colors),
                );
                let tokens: Vec<Token> = raw_tokens.into_iter()
                    .map(|(t, v)| Token(t, v))
                    .collect();
                let mut parser = Parser::new(tokens, config.clone());
                let statements = match parser.parse_safe() {
                    Ok(stmts) => stmts,
                    Err((error, line)) => {
                        handle_error(&error, &file_content, line, &config, use_colors, Some(file_path.as_str()));
                        continue;
                    }
                };                
                debug_log(
                    &format!(
                        "Statements: [{}]",
                        statements
                            .iter()
                            .map(|stmt| format_value(stmt))
                            .collect::<Vec<String>>()
                            .join(", ")
                    ),
                    &config,
                    Some(use_colors),
                );
                //let interpreter = Interpreter::new(config.clone());
                //interpreter.interpret(statements);
            } else {
                eprintln!("Error: File '{}' does not exist or is not a valid file", file_path);
            }
        }
    } else {
        println!(
            "{}Lucia-{} REPL\nType 'exit()' to exit or 'help()' for help.{}", 
            hex_to_ansi(&config.color_scheme.info, Some(use_colors)), 
            config.version, 
            hex_to_ansi("reset", Some(use_colors))
        );        
        let interpreter = Interpreter::new(config.clone());
        loop {
            print!("{}{}{} ", 
                hex_to_ansi(&config.color_scheme.input_arrows, Some(use_colors)), 
                ">>> ", 
                hex_to_ansi(&config.color_scheme.input_text, Some(use_colors))
            );
            let input = utils::read_input("");
            if input == "exit" {
                println!("Use 'exit()' to exit.");
                continue;
            }
    
            let lexer = Lexer::new(&input);
            let raw_tokens = lexer.tokenize(config.print_comments);
            debug_log(
                &format!(
                    "Tokens: {:?}",
                    raw_tokens
                        .iter()
                        .filter(|token| token.0 != "WHITESPACE")  // Filter out whitespace tokens
                        .collect::<Vec<_>>()  // Collect filtered tokens into a new Vec
                ),
                &config,
                Some(use_colors),
            );         
            let tokens: Vec<Token> = raw_tokens.into_iter()
                .map(|(t, v)| Token(t, v))
                .collect();
            let mut parser = Parser::new(tokens, config.clone());
            let statements = parser.parse();
            debug_log(
                &format!(
                    "Statements: [{}]",
                    statements
                        .iter()
                        .map(|stmt| format_value(stmt))
                        .collect::<Vec<String>>()
                        .join(", ")
                ),
                &config,
                Some(use_colors),
            );            
            // interpreter.interpret(statements);
        }
    }
}
