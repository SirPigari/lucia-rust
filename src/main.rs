use std::env as std_env;
use std::fs::{self, File};
use std::io::{self, Read};
use std::path::{Path, PathBuf};
use serde::{Serialize, Deserialize};
use std::process::exit;
use std::collections::HashMap;
use colored::*;

mod env {
    pub mod runtime {
        pub mod utils;
        pub mod config;
        pub mod types;
        pub mod errors;
        pub mod value;
        pub mod functions;
        pub mod variables;
        pub mod native;
        pub mod statements;
        pub mod build;
        pub mod pattern_reg;
        pub mod preprocessor;
    }
}

mod interpreter;
mod parser;
mod lexer;

use crate::env::runtime::utils;
use crate::env::runtime::config::{Config, CodeBlocks, ColorScheme};
use crate::utils::{hex_to_ansi, get_line_info, format_value, check_ansi, clear_terminal, to_static};
use crate::env::runtime::types::{Int, Float, Boolean};
use crate::env::runtime::errors::Error;
use crate::env::runtime::value::Value;
use crate::env::runtime::build::{BuildInfo, get_build_info};
use crate::env::runtime::preprocessor::Preprocessor;
use crate::parser::{Parser, Token};
use crate::lexer::Lexer;
use crate::interpreter::Interpreter;

const VERSION: &str = "2.0.0-rust";

fn handle_error(error: &Error, source: &str, line: (usize, String), config: &Config, use_colors: bool, file_name: Option<&str>) {
    let mut file_name: &str = file_name.unwrap_or("<stdin>");

    if file_name.starts_with("\\\\?\\") {
        file_name = &file_name[4..];
    }

    let line_number = line.0;
    let error_type = error.error_type();
    let error_msg = error.msg();
    let error_help = error.help();

    let use_lucia_traceback = config.use_lucia_traceback;

    let current_line = get_line_info(source, line_number).unwrap_or_else(|| "".to_string());
    let prev_line = if line_number > 1 { get_line_info(source, line_number - 1) } else { None };
    let next_line = get_line_info(source, line_number + 1);

    let indent = " ".repeat(line_number.to_string().len());

    let mut arrows_under = String::new();

    let lexer = Lexer::new(&current_line);
    let line_tokens = lexer.tokenize(true);

    let mut trace = String::new();

    if !use_lucia_traceback {
        let location = match (line_number, error.column) {
            (0, 0) => format!("{}", file_name),
            (line, 0) => format!("{}:{}", file_name, line),
            (0, col) => format!("{}", file_name),
            (line, col) => format!("{}:{}:{}", file_name, line, col),
        };
    
        eprintln!(
            "{}{} -> {}: {}{}{}{}",
            hex_to_ansi(&config.color_scheme.exception, Some(use_colors)),
            location,
            error_type,
            error_msg,
            hex_to_ansi(&config.color_scheme.exception, Some(use_colors)),
            match error_help {
                Some(help) if !help.is_empty() => format!(
                    "   {}({}){}",
                    hex_to_ansi(&config.color_scheme.help, Some(use_colors)),
                    help,
                    hex_to_ansi("reset", Some(use_colors))
                ),
                _ => "".to_string(),
            },
            hex_to_ansi("reset", Some(use_colors))
        );
        return;
    }
    
    
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

fn get_config_path() -> std::io::Result<PathBuf> {
    let exe_path = std_env::current_exe()?;

    let exe_dir = exe_path.parent()
        .expect("Executable must be in a directory");

    let config_path = exe_dir.join("../").join("config.json");

    let config_path = config_path.canonicalize()?;

    Ok(config_path)
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

fn activate_environment(env_path: &Path, respect_existing_moded: bool) -> io::Result<()> {
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

    let config_path = env_path.join("config.json");

    if respect_existing_moded && config_path.exists() {
        let config_data = fs::read_to_string(&config_path)?;
        let mut config: Config = serde_json::from_str(&config_data)
            .map_err(|_| io::Error::new(io::ErrorKind::InvalidData, "Failed to deserialize config"))?;

        if config.moded {
            config.home_dir = env_path_str;
            let config_str = serde_json::to_string_pretty(&config)
                .map_err(|_| io::Error::new(io::ErrorKind::InvalidData, "Failed to serialize config"))?;
            fs::write(&config_path, config_str)
                .map_err(|_| io::Error::new(io::ErrorKind::Other, "Failed to write config file"))?;
            return Ok(());
        }
    }

    let default_config = Config {
        version: VERSION.to_string(),
        moded: false,
        debug: false,
        debug_mode: "normal".to_string(),
        supports_color: true,
        use_lucia_traceback: true,
        warnings: true,
        use_preprocessor: true,
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
        exit(1);
    }

    if working_dir.ends_with(".exe") && !working_dir_path.components().any(|comp| comp.as_os_str() == "bin") {
        eprintln!("Executable is not located in a 'bin' directory. Exiting.");
        exit(1);
    }
    let env_path = working_dir_path.join("..").canonicalize().unwrap_or_else(|_| working_dir_path.clone());

    let args: Vec<String> = std_env::args().collect();
    let activate_flag = args.contains(&"--activate".to_string());
    let no_color_flag = args.contains(&"--no-color".to_string());
    let quiet_flag = args.contains(&"--quiet".to_string()) || args.contains(&"-q".to_string());
    let debug_flag = args.contains(&"--debug".to_string()) || args.contains(&"-d".to_string());
    let exit_flag = args.contains(&"--exit".to_string()) || args.contains(&"-e".to_string());
    let help_flag = args.contains(&"--help".to_string()) || args.contains(&"-h".to_string());
    let version_flag = args.contains(&"--version".to_string()) || args.contains(&"-v".to_string());
    let disable_preprocessor = args.contains(&"--disable-preprocessor".to_string()) || args.contains(&"-dp".to_string());

    if help_flag {
        println!("Usage: lucia [options] [files...]\n");
        println!("Options:");
        println!("  --activate, -a       Activate the environment");
        println!("  --no-color           Disable colored output");
        println!("  --quiet, -q          Suppress debug and warning messages");
        println!("  --debug, -d          Enable debug mode");
        println!("  --debug-mode=<mode>  Set debug mode (full, normal, minimal)");
        println!("  --exit, -e           Exit after executing files");
        println!("  --help, -h           Show this help message");
        println!("  --version, -v        Show version information");
        println!("  --build-info         Show build information");
        exit(0);
    }

    if version_flag {
        println!("Lucia-{}", VERSION);
        exit(0);
    }

    

    if args.contains(&"--build-info".to_string()) {
        let info = get_build_info();

        println!("{}", serde_json::to_string_pretty(&info).unwrap());
        exit(0);
    }
    
    let debug_mode = if debug_flag {
        if let Some(arg) = args.iter().find(|arg| arg.starts_with("--debug-mode=")) {
            if let Some(mode) = arg.split('=').nth(1) {
                if ["full", "normal", "minimal"].contains(&mode) {
                    mode.to_string()
                } else {
                    eprintln!("Invalid debug mode: '{}'. Valid modes are 'full', 'normal', or 'minimal'. Defaulting to 'normal'.", mode);
                    exit(1);
                    "normal".to_string()
                }
            } else {
                "normal".to_string()
            }
        } else {
            "normal".to_string()
        }
    } else {
        "normal".to_string()
    };

    let use_colors = !no_color_flag;

    let config_path: PathBuf = match get_config_path() {
        Ok(path) if path.exists() => path,
        _ => {
            let fallback_path = Path::new(file!())
                .parent()
                .map(|p| Path::new(p).join("..").join("config.json"))
                .map(|p| fs::canonicalize(&p).unwrap_or(p))
                .unwrap_or_else(|| {
                    eprintln!("Failed to determine fallback config path");
                    exit(1);
                });
    
            if let Some(parent) = fallback_path.parent() {
                if let Err(e) = fs::create_dir_all(parent) {
                    eprintln!("Failed to create fallback config directory {}: {}", parent.display(), e);
                    exit(1);
                }
            }

            if !fallback_path.exists() {
                if let Err(e) = fs::write(&fallback_path, b"{}") {
                    eprintln!("Failed to create fallback config file {}: {}", fallback_path.display(), e);
                    exit(1);
                }
            }
    
            fallback_path
        }
    };

    let mut config = if config_path.exists() {
        match load_config(&config_path) {
            Ok(config) => config,
            Err(_) => {
                eprintln!("Failed to read config file, activating environment...");
                if let Err(err) = activate_environment(&env_path, true) {
                    eprintln!("Failed to activate environment: {}", err);
                    exit(1);
                }
                match load_config(&config_path) {
                    Ok(config) => config,
                    Err(e) => {
                        eprintln!("Failed to load config file again after activation: {}", e);
                        eprintln!("Please rerun the program to try again after fixing the config file.");
                        exit(0);
                    }
                }
            }
        }
    } else {
        eprintln!("Config file not found, activating environment...");
        if let Err(err) = activate_environment(&env_path, true) {
            eprintln!("Failed to activate environment: {}", err);
            exit(1);
        }
        match load_config(&config_path) {
            Ok(config) => config,
            Err(e) => {
                eprintln!("Failed to load config file after activation: {}", e);
                eprintln!("Please rerun the program to try again after fixing the config file.");
                exit(0);
            }
        }
    };
    
    if activate_flag {
        println!("{}", format!("Activating environment at: {}", env_path.display()).cyan().bold());
        if let Err(err) = activate_environment(&env_path, false) {
            eprintln!("{}", format!("Failed to activate environment: {}", err).red().bold());
            exit(1);
        }
        config = load_config(&config_path).unwrap();
    }

    let home_dir = config.home_dir
    .to_string()
    .replace("\\", "/");

    let home_dir_path = PathBuf::from(home_dir.clone());

    if let Err(e) = std_env::set_current_dir(&home_dir_path) {
        if let Err(err) = activate_environment(&env_path, true) {
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

    if !home_dir_path.exists() {
        eprintln!("Home directory does not exist: {}", home_dir_path.display());
        exit(1);
    }

    if debug_flag {
        config.debug = true;
        config.debug_mode = debug_mode;
    }
    if quiet_flag {
        config.debug = false;
        config.use_lucia_traceback = false;
        config.warnings = false;
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

    if exit_flag {
        if non_flag_args.is_empty() {
            eprintln!("No files provided to execute. Exiting.");
            exit(0);
        }
    }

    if !non_flag_args.is_empty() {
        for file_path in non_flag_args {
            let path = Path::new(&file_path);
    
            if path.exists() && path.is_file() {
                debug_log(&format!("Executing file: {:?}", path), &config, Some(use_colors));

                let file_content = fs::read_to_string(path).expect("Failed to read file");
                let lexer = Lexer::new(&file_content);
                let lexer = Lexer::new(&file_content);
                let raw_tokens = lexer.tokenize(config.print_comments);
                
                let processed_tokens = if !disable_preprocessor {
                    let mut preprocessor = Preprocessor::new(
                        home_dir_path.join("libs"),
                        config_path.clone(),
                    );
                    match preprocessor.process(raw_tokens, path.parent().unwrap_or(Path::new(""))) {
                        Ok(tokens) => tokens,
                        Err(e) => {
                            handle_error(
                                &e.clone(),
                                &file_content,
                                (0, "".to_string()),
                                &config,
                                use_colors,
                                Some(file_path.as_str()),
                            );
                            exit(1);
                        }
                    }
                } else {
                    raw_tokens
                };
                
                debug_log(
                    &format!(
                        "Tokens: {:?}",
                        processed_tokens
                            .iter()
                            .filter(|token| token.0 != "WHITESPACE")
                            .collect::<Vec<_>>()
                    ),
                    &config,
                    Some(use_colors),
                );
                
                let tokens: Vec<Token> = processed_tokens
                    .into_iter()
                    .map(|(t, v)| Token(t, v))
                    .collect();                
                let mut parser = Parser::new(tokens, config.clone(), file_content.to_string());
                let statements = match parser.parse_safe() {
                    Ok(stmts) => stmts,
                    Err(error) => {
                        debug_log(
                            "Error while parsing:",
                            &config,
                            Some(use_colors),
                        );
                        handle_error(&error.clone(), &file_content, error.line, &config, use_colors, Some(file_path.as_str()));
                        exit(1);
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
                let mut interpreter = Interpreter::new(config.clone(), use_colors);
                let out: Value = match interpreter.interpret(statements, file_content.clone()) {
                    Ok(out) => out,
                    Err(error) => {
                        debug_log(
                            "Error while interpreting:",
                            &config,
                            Some(use_colors),
                        );
                        handle_error(&error.clone(), &file_content, error.line, &config, use_colors, Some(file_path.as_str()));
                        exit(1);
                    }
                };
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
        let mut interpreter = Interpreter::new(config.clone(), use_colors);
        let mut preprocessor = Preprocessor::new(
            home_dir_path.join("libs"),
            config_path.clone(),
        );
        loop {
            print!("{}{}{} ", 
                hex_to_ansi(&config.color_scheme.input_arrows, Some(use_colors)), 
                ">>>", 
                hex_to_ansi(&config.color_scheme.input_text, Some(use_colors))
            );
            let input = utils::read_input("");
            if input.is_empty() {
                continue;
            }
            
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

            if input == "\x03" {
                exit(0);
            }

            if input == "clear()" || input == "\x11" || input == "\x0F" {
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
            
            let current_dir = std_env::current_dir().unwrap_or_else(|_| PathBuf::from("."));

            let processed_tokens = if !disable_preprocessor {
                match preprocessor.process(raw_tokens, &current_dir) {
                    Ok(toks) => toks,
                    Err(e) => {
                        handle_error(
                            &e.clone(),
                            &input,
                            (0, "".to_string()),
                            &config,
                            use_colors,
                            Some("<stdin>"),
                        );
                        continue;
                    }
                }
            } else {
                raw_tokens
            };
            
            debug_log(
                &format!(
                    "Tokens: {:?}",
                    processed_tokens
                        .iter()
                        .filter(|token| {
                            token.0 != "WHITESPACE"
                            && token.0 != "COMMENT_INLINE"
                            && token.0 != "COMMENT_SINGLE"
                            && token.0 != "COMMENT_MULTI"
                            && token.0 != "EOF"
                        })
                        .collect::<Vec<_>>()
                ),
                &config,
                Some(use_colors),
            );
            
            let tokens: Vec<Token> = processed_tokens
                .into_iter()
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
            let out: Value = match interpreter.interpret(statements, input.clone()) {
                Ok(out) => {
                    if interpreter.is_stopped() {
                        exit(0);
                    }
                    out
                },
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
