use std::env as std_env;
use std::fs::{self, File};
use std::io::{self, Read};
use std::path::{Path, PathBuf};
use serde::{Serialize, Deserialize};
use std::process::exit;
use std::collections::HashMap;

mod env {
    pub mod core {
        pub mod utils;
        pub mod config;
        pub mod types;
        pub mod errors;
        pub mod value;
        pub mod functions;
        pub mod variables;
        pub mod native;
        pub mod statements;
    }
}

mod interpreter;
mod parser;
mod lexer;

use crate::env::core::utils;
use crate::env::core::config::{Config, CodeBlocks, ColorScheme};
use crate::utils::{hex_to_ansi, get_line_info, format_value, check_ansi, clear_terminal};
use crate::env::core::types::{Int, Float, Boolean};
use crate::env::core::errors::Error;
use crate::env::core::value::Value;
use crate::parser::{Parser, Token};
use crate::lexer::Lexer;
use crate::interpreter::Interpreter;

const VERSION: &str = "1.0.0-rust";

fn handle_error(error: &Error, source: &str, line: (usize, String), config: &Config, use_colors: bool, file_name: Option<&str>) {
    let mut file_name: &str = file_name.unwrap_or("<stdin>");

    if file_name.starts_with("\\\\?\\") {
        file_name = &file_name[4..];
    }

    let line_number = line.0;
    let error_type = error.error_type();
    let error_msg = error.msg();
    let error_help = error.help();

    let current_line = get_line_info(source, line_number).unwrap_or_else(|| "".to_string());
    let prev_line = if line_number > 1 { get_line_info(source, line_number - 1) } else { None };
    let next_line = get_line_info(source, line_number + 1);

    let indent = " ".repeat(line_number.to_string().len());

    let mut arrows_under = String::new();

    let lexer = Lexer::new(&current_line);
    let line_tokens = lexer.tokenize(true);

    let mut trace = String::new();
    
    if !(error.column == 0 && error.line.0 == 0) {
        let mut current_pos = 1;
        let mut found = false;

        for (token_type, token_value) in line_tokens {
            let token_len = token_value.len();

            if token_type.starts_with("COMMENT") {
                current_pos += token_len;
                break;
            }
        
            if !found && current_pos == error.column {
                arrows_under.push_str(&"^".repeat(token_len));
                found = true;
            } else {
                arrows_under.push_str(&"~".repeat(token_len));
            }

            current_pos += token_len;
        }

        if !found {
            arrows_under = " ".repeat(error.column.saturating_sub(1)) + "^";
        }

        trace.push_str(&format!(
            "{}-> File '{}:{}:{}' got error:\n",
            hex_to_ansi(&config.color_scheme.exception, Some(use_colors)),
            file_name,
            line_number,
            error.column
        ));

        if prev_line.is_some() {
            trace.push_str(&format!("\t{} ...\n", indent));
        }

        trace.push_str(&format!("\t{} | {}\n", line_number, current_line));

        trace.push_str(&format!("\t{} | {}\n", indent, arrows_under));

        if next_line.is_some() {
            trace.push_str(&format!("\t{} ...\n", indent));
        }
    } else {
        if line_number > 0 {
            trace.push_str(&format!(
                "{}-> File '{}:{}' got error:\n",
                hex_to_ansi(&config.color_scheme.exception, Some(use_colors)),
                file_name,
                line_number
            ));
        } else {
            trace.push_str(&format!(
                "{}-> File '{}' got error:\n",
                hex_to_ansi(&config.color_scheme.exception, Some(use_colors)),
                file_name
            ));
        }
    }

    trace.push_str(&format!(
        "\t{} | {}: {}",
        indent,
        error_type,
        error_msg,
    ));

    if let Some(help) = error_help {
        if !help.is_empty() {
            trace.push_str(&format!(
                "\n\t{} ...\n\t{}{} | {}Help:{} {}{}",
                indent,
                indent,
                hex_to_ansi(&config.color_scheme.help, Some(use_colors)),
                check_ansi("\x1b[1m", &use_colors),
                check_ansi("\x1b[22m", &use_colors),
                help,
                hex_to_ansi(&config.color_scheme.exception, Some(use_colors))
            ));
        }
    }
 
    if !(file_name == "<stdin>") {
        trace.push_str("\n");
    }

    trace.push_str(&format!("{}", hex_to_ansi("reset", Some(use_colors))));

    eprintln!("{}", trace);
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
            help: "#21B8DB".to_string(),
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
    let working_dir = std_env::current_dir()
    .unwrap_or_else(|_| PathBuf::from("."))
    .display()
    .to_string()
    .replace("\\", "/");

    let working_dir_path = PathBuf::from(working_dir.clone());
    if let Err(e) = std_env::set_current_dir(&working_dir_path) {
        eprintln!("Failed to change the directory: {}", e);
    }

    if working_dir.ends_with(".exe") && !working_dir_path.components().any(|comp| comp.as_os_str() == "bin") {
        eprintln!("Executable is not located in a 'bin' directory. Exiting.");
        exit(1);
    }
    let env_path = working_dir_path.join("..").canonicalize().unwrap_or_else(|_| working_dir_path.clone());

    let args: Vec<String> = std_env::args().collect();
    let activate_flag = args.contains(&"--activate".to_string());
    let no_color_flag = args.contains(&"--no-color".to_string());

    let use_colors = !no_color_flag;

    let config_path = env_path.join("config.json");

    let mut config = if config_path.exists() {
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
        eprintln!("Config file not found, activating environment...");
        if let Err(err) = activate_environment(&env_path) {
            eprintln!("Failed to activate environment: {}", err);
            exit(1);
        }
        load_config(&config_path).unwrap()
    };
    if activate_flag {
        if let Err(err) = activate_environment(&env_path) {
            eprintln!("Failed to activate environment: {}", err);
            exit(1);
        }
        config = load_config(&config_path).unwrap();
    };

    let home_dir = config.home_dir
    .to_string()
    .replace("\\", "/");

    let home_dir_path = PathBuf::from(home_dir.clone());

    if let Err(e) = std_env::set_current_dir(&home_dir_path) {
        if let Err(err) = activate_environment(&env_path) {
            eprintln!("Failed to activate environment: {}", err);
            exit(1);
        }
        config = load_config(&config_path).unwrap();
    }

    let home_dir = config.home_dir
    .to_string()
    .replace("\\", "/");

    let home_dir_path = PathBuf::from(home_dir.clone());

    if let Err(e) = std_env::set_current_dir(&home_dir_path) {
        eprintln!("Failed to change the directory: {}", e);
        exit(1);
    }



    let args: Vec<String> = std_env::args().collect();

    let non_flag_args: Vec<String> = args.iter()
        .skip(1)
        .filter(|arg| !arg.starts_with('-'))
        .cloned()
        .filter_map(|arg| {
            let mut path: PathBuf = PathBuf::from(&arg);

            if path.is_relative() {
                path = working_dir_path.join(path);
            }

            path = match path.canonicalize() {
                Ok(p) => p,
                Err(e) => {
                    eprintln!("Error: File '{}' does not exist or is not a valid file", path.display());
                    exit(1);
                }
            };

            Some(path.display().to_string())
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
                            .filter(|token| token.0 != "WHITESPACE")
                            .collect::<Vec<_>>()
                    ),
                    &config,
                    Some(use_colors),
                );
                let tokens: Vec<Token> = raw_tokens.into_iter()
                    .map(|(t, v)| Token(t, v))
                    .collect();
                let mut parser = Parser::new(tokens, config.clone(), file_content.to_string());
                let statements = match parser.parse_safe() {
                    Ok(stmts) => stmts,
                    Err(error) => {
                        handle_error(&error.clone(), &file_content, error.line, &config, use_colors, Some(file_path.as_str()));
                        return;
                    }
                };
                debug_log(
                    &format!(
                        "Statements: [{}]",
                        statements
                            .iter()
                            .map(|stmt| format_value(&stmt.convert_to_map()))
                            .collect::<Vec<String>>()
                            .join(", ")
                    ),
                    &config,
                    Some(use_colors),
                );
                let mut interpreter = Interpreter::new(config.clone(), file_content.to_string(), use_colors);
                interpreter.interpret(statements);
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
        loop {
            print!("{}{}{} ", 
                hex_to_ansi(&config.color_scheme.input_arrows, Some(use_colors)), 
                ">>>", 
                hex_to_ansi(&config.color_scheme.input_text, Some(use_colors))
            );
            let input = utils::read_input("");
            if input == "exit" {
                println!("Use 'exit()' to exit.");
                continue;
            }
            if input == "clear" || input == "cls" || input == "cls()" {
                println!("Use 'clear()' to clear.");
                continue;
            }

            if input == "help" || input == "?" {
                println!("Use 'help()' for help.");
                continue;
            }

            if input == "clear()" {
                if let Err(e) =  clear_terminal() {
                    handle_error(
                        &Error::new("IOError", "Failed to clear screen"),
                        &input,
                        (0, "".to_string()),
                        &config,
                        use_colors,
                        Some("<stdin>"),
                    );
                }
                println!(
                    "{}Lucia-{} REPL\nType 'exit()' to exit or 'help()' for help.{}", 
                    hex_to_ansi(&config.color_scheme.info, Some(use_colors)), 
                    config.version, 
                    hex_to_ansi("reset", Some(use_colors))
                );
                continue;
            }
    
            let lexer = Lexer::new(&input);
            let raw_tokens = lexer.tokenize(config.print_comments);
            debug_log(
                &format!(
                    "Tokens: {:?}",
                    raw_tokens
                        .iter()
                        .filter(|token| token.0 != "WHITESPACE")
                        .collect::<Vec<_>>()
                ),
                &config,
                Some(use_colors),
            );
            let tokens: Vec<Token> = raw_tokens.into_iter()
            .map(|(t, v)| Token(t, v))
            .collect();
            let mut parser = Parser::new(tokens, config.clone(), input.to_string());
            let statements = match parser.parse_safe() {
                Ok(stmts) => stmts,
                Err(error) => {
                    debug_log(
                        "Error while parsing:",
                        &config,
                        Some(use_colors),
                    );
                    handle_error(&error.clone(), &input, error.line, &config, use_colors, Some("<stdin>"));
                    continue;
                }
            };
            debug_log(
                &format!(
                    "Statements: [{}]",
                    statements
                        .iter()
                        .map(|stmt| format_value(&stmt.convert_to_map()))
                        .collect::<Vec<String>>()
                        .join(", ")
                ),
                &config,
                Some(use_colors),
            );
            let mut interpreter = Interpreter::new(config.clone(), input.to_string(), use_colors);
            let out: Value = match interpreter.interpret(statements) {
                Ok(out) => out,
                Err(error) => {
                    debug_log(
                        "Error while interpreting:",
                        &config,
                        Some(use_colors),
                    );
                    handle_error(&error.clone(), &input, error.line, &config, use_colors, Some("<stdin>"));
                    continue;
                }
            };
            if !matches!(out, Value::Null) {
                println!(
                        "{}",
                        format_value(&out)
                );
            }
        }
    }
}
