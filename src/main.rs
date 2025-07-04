use std::env as std_env;
use std::fs::{self, File};
use std::io::{self, Read, Write};
use std::{thread, panic};
use std::path::{Path, PathBuf};
use serde::{Serialize, Deserialize};
use std::process::exit;
use std::collections::HashMap;
use serde_json::to_string_pretty;
use colored::*;
use sys_info;

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
        pub mod pattern_reg;
        pub mod preprocessor;
        pub mod objects;
        pub mod libs;
        pub mod tokens;
    }
    pub mod libs {
        pub mod math {
            pub mod __init__;
        }
        pub mod os {
            pub mod __init__;
        }
        pub mod time {
            pub mod __init__;
        }
        pub mod json {
            pub mod __init__;
        }
        pub mod config {
            pub mod __init__;
        }
        pub mod clib {
            pub mod __init__;
            pub mod _tcc;
        }
        pub mod regex {
            pub mod __init__;
        }
        pub mod collections {
            pub mod __init__;
        }
        pub mod random {
            pub mod __init__;
        }
        pub mod lasm {
            pub mod __init__;
            pub mod cpu;
        }
    }
}

mod interpreter;
mod parser;
mod lexer;

use crate::env::runtime::utils;
use crate::env::runtime::config::{Config, ColorScheme};
use crate::utils::{hex_to_ansi, get_line_info, format_value, check_ansi, clear_terminal, to_static, print_colored, unescape_string};
use crate::env::runtime::types::{Int, Float, Boolean};
use crate::env::runtime::errors::Error;
use crate::env::runtime::value::Value;
use crate::env::runtime::preprocessor::Preprocessor;
use crate::env::runtime::statements::Statement;
use crate::parser::Parser;
use crate::env::runtime::tokens::{Token, Location};
use crate::lexer::Lexer;
use crate::interpreter::Interpreter;

const VERSION: &str = env!("VERSION");

#[derive(Serialize)]
pub struct BuildInfo {
    pub name: &'static str,
    pub version: &'static str,
    pub uuid: &'static str,
    pub rustc_version: &'static str,
    pub rustc_channel: &'static str,
    pub target: &'static str,
    pub repository: &'static str,
    pub git_hash: &'static str,
    pub file_hash: &'static str,
    pub profile: &'static str,
    pub ci: &'static str,
    pub build_date: &'static str,
    pub dependencies: &'static str,
}

pub fn get_build_info() -> BuildInfo {
    BuildInfo {
        name: env!("CARGO_PKG_NAME"),
        version: env!("VERSION"),
        uuid: env!("BUILD_UUID"),
        rustc_version: env!("RUSTC_VERSION"),
        rustc_channel: env!("RUSTC_CHANNEL"),
        target: env!("TARGET_TRIPLE"),
        repository: env!("REPO"),
        git_hash: env!("GIT_HASH"),
        file_hash: env!("FILE_HASH"),
        profile: env!("PROFILE"),
        ci: env!("CI"),
        build_date: env!("BUILD_DATE"),
        dependencies: env!("DEPS"),
    }
}

pub fn handle_error(
    error: &Error,
    source: &str,
    config: &Config,
    use_colors: bool,
) {
    let use_lucia_traceback = config.use_lucia_traceback;

    let format_location = |loc: &Location| -> String {
        match loc {
            Location { range: (0, 0), .. } => loc.file.clone(),
            Location { range: (line, 0), .. } => format!("{}:{}", loc.file, line),
            Location { range: (0, col), .. } => format!("{}:{}", loc.file, col),
            Location { range: (line, col), .. } => format!("{}:{}:{}", loc.file, line, col),
        }
    };

    let print_single_error = |err: &Error| -> String {
        let mut trace = String::new();

        let loc = match &err.loc {
            Some(loc) => loc,
            None => {
                trace.push_str(&format!(
                    "{}-> Error: {}{}\n",
                    hex_to_ansi(&config.color_scheme.exception, Some(use_colors)),
                    err.msg,
                    hex_to_ansi("reset", Some(use_colors))
                ));
                return trace;
            }
        };

        let file_name = loc.file.strip_prefix(r"\\?\").unwrap_or(&loc.file);
        let line_number = loc.line_number;
        let range = loc.range;
        let col = range.0;
        let reset = hex_to_ansi("reset", Some(use_colors));

        let current_line = get_line_info(source, line_number).unwrap_or_default();
        let prev_line = if line_number > 1 {
            get_line_info(source, line_number - 1)
        } else {
            None
        };
        let next_line = get_line_info(source, line_number + 1);
        let indent = " ".repeat(line_number.to_string().len());

        let mut arrows_under = String::new();
        let line_len = current_line.len();
        let start = range.0.min(line_len);
        let end = range.1.min(line_len);

        if start >= line_len || end == 0 || start >= end {
            arrows_under = " ".repeat(col.saturating_sub(1)) + "^";
        } else {
            arrows_under = "~".repeat(line_len);
            let len = end.saturating_sub(start);
            arrows_under.replace_range(start..end, &"^".repeat(len.max(1)));
        }


        trace.push_str(&format!(
            "{}-> File '{}:{}:{}' got error:\n",
            hex_to_ansi(&config.color_scheme.exception, Some(use_colors)),
            file_name,
            line_number,
            col
        ));

        if prev_line.is_some() {
            trace.push_str(&format!("\t{} ...\n", indent));
        }

        trace.push_str(&format!("\t{} | {}\n", line_number, current_line));
        trace.push_str(&format!("\t{} | {}\n", indent, arrows_under));

        if next_line.is_some() {
            trace.push_str(&format!("\t{} ...\n", indent));
        }

        trace.push_str(&format!(
            "\t{} | {}: {}",
            indent, err.error_type, err.msg
        ));

        if let Some(help) = &err.help {
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

        trace.push('\n');
        trace.push_str(&reset);
        trace
    };

    if !use_lucia_traceback {
        let mut depth = 0;
        let mut current = error;
        while let Some(inner) = &current.ref_err {
            depth += 1;
            current = inner;
        }

        let location_str = current
            .loc
            .as_ref()
            .map_or("<unknown>".to_string(), format_location);

        let depth_note = if depth > 0 {
            format!(" (nested depth: {})", depth)
        } else {
            "".into()
        };

        let help_msg = match &current.help {
            Some(help) if !help.is_empty() => format!(
                "   {}({}){}",
                hex_to_ansi(&config.color_scheme.help, Some(use_colors)),
                help,
                hex_to_ansi("reset", Some(use_colors))
            ),
            _ => String::new(),
        };

        eprintln!(
            "{}[err] {} -> {}: {}{}{}{}",
            hex_to_ansi(&config.color_scheme.exception, Some(use_colors)),
            location_str,
            current.error_type,
            current.msg,
            depth_note,
            help_msg,
            hex_to_ansi("reset", Some(use_colors))
        );
        return;
    }

    let mut trace = String::new();
    let mut current_error = Some(error);

    while let Some(err) = current_error {
        trace.push_str(&print_single_error(err));
        if let Some(ref inner) = err.ref_err {
            trace.push_str(&format!(
                "\t{}^-- caused by:\n",
                hex_to_ansi(&config.color_scheme.exception, Some(use_colors))
            ));
            trace.push_str(&hex_to_ansi("reset", Some(use_colors)));
            current_error = Some(inner);
        } else {
            current_error = None;
        }
    }

    eprintln!("{}", trace);
}

fn debug_log(message: &str, config: &Config, use_colors: Option<bool>) {
    let use_colors = use_colors.unwrap_or(true);
    if config.debug {
        utils::print_colored(message, &config.color_scheme.debug, Some(use_colors));
    }
}

fn dump_pp(tokens: Vec<&Token>, file_path: &str, config: &Config, use_colors: bool) {
    let mut i_buffer = String::from("[\n");
    let mut pp_buffer = String::from("// This file was generated by lucia preprocessor dump\n// You can just rename this to .lc file and it will work just fine\n\n");

    for (i, token) in tokens.iter().enumerate() {
        let Token(kind, val, _) = token;

        if kind == "EOF" {
            break;
        }

        let escaped_val = if kind == "STRING" {
            unescape_string(val).unwrap_or_else(|_| val.clone()).replace("%", "%%")
        } else {
            val.clone()
        };

        let line = format!("  (\"{}\", \"{}\")", kind, escaped_val);
        if i != tokens.len() - 1 {
            i_buffer.push_str(&format!("{},\n", line));
        } else {
            i_buffer.push_str(&format!("\n{}\n", line));
        }

        pp_buffer.push_str(val);
        pp_buffer.push(' ');
    }
    i_buffer.push(']');

    let base_path = Path::new(file_path)
        .with_extension("")
        .to_string_lossy()
        .to_string();

    if let Err(err) = File::create(format!("{base_path}.i"))
        .and_then(|mut f| f.write_all(i_buffer.as_bytes()))
    {
        eprintln!(
            "{} Failed to write to {base_path}.i: {err}",
            hex_to_ansi(&config.color_scheme.exception, Some(use_colors))
        );
    }

    if let Err(err) = File::create(format!("{base_path}.pp"))
        .and_then(|mut f| f.write_all(pp_buffer.as_bytes()))
    {
        eprintln!(
            "{} Failed to write to {base_path}.pp: {err}",
            hex_to_ansi(&config.color_scheme.exception, Some(use_colors))
        );
    }

    print_colored(
        &format!(
            "{}Preprocessed source code dumped to {}.i and {}.pp",
            hex_to_ansi(&config.color_scheme.debug, Some(use_colors)),
            base_path,
            base_path
        ),
        &config.color_scheme.debug,
        Some(use_colors),
    );
}

fn dump_ast(tokens: Vec<&Statement>, file_path: &str, config: &Config, use_colors: bool) {
    let base_path = Path::new(file_path)
        .with_extension("")
        .to_string_lossy()
        .to_string();

    let ast_path = format!("{}.ast.json", base_path);
    let i_path = format!("{}.i.json", base_path);

    let full_data: Vec<Value> = tokens.iter()
        .map(|stmt| stmt.convert_to_map())
        .collect();

    let cleaned_data: Vec<Value> = full_data.iter()
        .map(|value| {
            match value {
                Value::Map { keys, values } => {
                    let mut new_keys = Vec::new();
                    let mut new_values = Vec::new();
                    for (k, v) in keys.iter().zip(values.iter()) {
                        if let Value::String(s) = k {
                            if s == "_loc" {
                                continue;
                            }
                        }
                        let cleaned_value = match v {
                            Value::Map { .. } | Value::List(_) | Value::Tuple(_) => {
                                let mut stack = vec![v.clone()];
                                let mut cleaned = v.clone();
                                while let Some(cur) = stack.pop() {
                                    cleaned = match cur {
                                        Value::Map { keys, values } => {
                                            let mut k2 = Vec::new();
                                            let mut v2 = Vec::new();
                                            for (kk, vv) in keys.iter().zip(values.iter()) {
                                                if let Value::String(ss) = kk {
                                                    if ss == "_line" || ss == "_column" {
                                                        continue;
                                                    }
                                                }
                                                k2.push(kk.clone());
                                                v2.push(vv.clone());
                                                stack.push(vv.clone());
                                            }
                                            Value::Map { keys: k2, values: v2 }
                                        }
                                        Value::List(list) => {
                                            Value::List(list.iter().map(|x| x.clone()).collect())
                                        }
                                        Value::Tuple(tuple) => {
                                            Value::Tuple(tuple.iter().map(|x| x.clone()).collect())
                                        }
                                        _ => cur.clone(),
                                    };
                                }
                                cleaned
                            }
                            _ => v.clone(),
                        };

                        new_keys.push(k.clone());
                        new_values.push(cleaned_value);
                    }
                    Value::Map { keys: new_keys, values: new_values }
                }
                _ => value.clone(),
            }
        })
        .collect();

    let json_i = match serde_json::to_string_pretty(&full_data) {
        Ok(j) => j,
        Err(e) => {
            print_colored(
                &format!(
                    "{}Failed to serialize full AST: {}",
                    hex_to_ansi(&config.color_scheme.exception, Some(use_colors)),
                    e
                ),
                &config.color_scheme.exception,
                Some(use_colors),
            );
            return;
        }
    };

    let json_ast = match serde_json::to_string_pretty(&cleaned_data) {
        Ok(j) => j,
        Err(e) => {
            print_colored(
                &format!(
                    "{}Failed to serialize cleaned AST: {}",
                    hex_to_ansi(&config.color_scheme.exception, Some(use_colors)),
                    e
                ),
                &config.color_scheme.exception,
                Some(use_colors),
            );
            return;
        }
    };

    let write_file = |path: &str, content: &str| {
        match File::create(path).and_then(|mut f| f.write_all(content.as_bytes())) {
            Ok(_) => {
                print_colored(
                    &format!(
                        "{}Dumped AST to {}",
                        hex_to_ansi(&config.color_scheme.debug, Some(use_colors)),
                        path
                    ),
                    &config.color_scheme.debug,
                    Some(use_colors),
                );
            }
            Err(e) => {
                print_colored(
                    &format!(
                        "{}Failed to write {}: {}",
                        hex_to_ansi(&config.color_scheme.exception, Some(use_colors)),
                        path,
                        e
                    ),
                    &config.color_scheme.exception,
                    Some(use_colors),
                );
            }
        }
    };

    write_file(&i_path, &json_i);
    write_file(&ast_path, &json_ast);
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
        allow_unsafe: false,
        home_dir: env_path_str,
        recursion_limit: 9999,
        color_scheme: ColorScheme {
            exception: "#F44350".to_string(),
            warning: "#FFC107".to_string(),
            help: "#21B8DB".to_string(),
            debug: "#434343".to_string(),
            comment: "#757575".to_string(),
            input_arrows: "#136163".to_string(),
            note: "#1CC58B".to_string(),
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

fn lucia(args: Vec<String>) {
    let cwd = std_env::current_dir()
    .and_then(|p| p.canonicalize())
    .unwrap_or_else(|e| {
        eprintln!("Failed to get canonicalized current directory: {}", e);
        exit(1);
    });

    let cwd_str = cwd.to_string_lossy().replace('\\', "/");

    let exe_path = std_env::current_exe()
        .and_then(|p| p.canonicalize())
        .unwrap_or_else(|e| {
            eprintln!("Failed to get canonicalized path to executable: {}", e);
            exit(1);
        });

    let exe_file = exe_path
        .display()
        .to_string()
        .replace("\\", "/");

    let mut config_path = exe_path
        .parent()
        .map(|p| p.join("..").join("config.json"))
        .ok_or_else(|| {
            eprintln!("Failed to resolve parent of executable path.");
            exit(1);
        })
        .and_then(|p| p.canonicalize().or_else(|e| {
            eprintln!("Failed to canonicalize config path: {}", e);
            exit(1);
        }))
        .unwrap();

    let activate_flag = args.contains(&"--activate".to_string());
    let no_color_flag = args.contains(&"--no-color".to_string());
    let quiet_flag = args.contains(&"--quiet".to_string()) || args.contains(&"-q".to_string());
    let debug_flag = args.contains(&"--debug".to_string()) || args.contains(&"-d".to_string());
    let exit_flag = args.contains(&"--exit".to_string()) || args.contains(&"-e".to_string());
    let help_flag = args.contains(&"--help".to_string()) || args.contains(&"-h".to_string());
    let version_flag = args.contains(&"--version".to_string()) || args.contains(&"-v".to_string());
    let disable_preprocessor = args.contains(&"--disable-preprocessor".to_string()) || args.contains(&"-dp".to_string());
    let config_arg = args.iter().find(|arg| arg.starts_with("--config="));
    let dump_pp_flag = args.contains(&"--dump-pp".to_string()) || args.contains(&"--dump".to_string());
    let dump_ast_flag = args.contains(&"--dump-ast".to_string()) || args.contains(&"--dump".to_string());
    let allow_unsafe = args.contains(&"--allow-unsafe".to_string());
    let argv_arg = args.iter()
        .find(|arg| arg.starts_with("--argv="))
        .map(|arg| arg.trim_start_matches("--argv="));

    let mut argv = if let Some(val) = argv_arg {
        let val = val.trim_start_matches('[').trim_end_matches(']');
        val.split(',')
            .map(|s| s.trim().to_string())
            .filter(|s| !s.is_empty())
            .collect::<Vec<String>>()
    } else {
        vec![]
    };

    if let Some(exe_path) = args.get(0) {
        argv.insert(0, exe_path.clone());
    }

    if version_flag {
        println!("Lucia-{}", VERSION);
        exit(0);
    }

    if help_flag {
        println!("{}", "Usage:".bold());
        println!("  lucia [options] [files...]\n");
    
        println!("{}", "Options:".bold());
        let options = [
            ("--activate, -a", "Activate the environment"),
            ("--no-color", "Disable colored output"),
            ("--quiet, -q", "Suppress debug and warning messages"),
            ("--debug, -d", "Enable debug mode"),
            ("--debug-mode=<mode>", "Set debug mode (full, normal, minimal)"),
            ("--exit, -e", "Exit after execution"),
            ("--help, -h", "Show this help message"),
            ("--version, -v", "Show version information"),
            ("--build-info", "Show build information"),
            ("--disable-preprocessor, -dp", "Disable preprocessor"),
            ("--config=<path>", "Specify a custom config file path"),
            ("--dump-pp", "Dump source code after preprocessing"),
            ("--dump-ast", "Dump AST after parsing"),
            ("--dump", "Dump both source code and AST (equivalent to --dump-pp and --dump-ast)"),
            ("--allow-unsafe", "Allow unsafe operations"),
            ("--stack-size=<size>", "Set the stack size for the interpreter, default: 8388608 (8MB)"),
            ("--argv=<args>", "Pass additional arguments to the interpreter as a JSON array"),
        ];
    
        for (flag, desc) in options {
            println!("  {:<32} {}", flag.cyan(), desc);
        }
    
        exit(0);
    }    

    if args.contains(&"--build-info".to_string()) {
        let info = get_build_info();

        println!("{}", serde_json::to_string_pretty(&info).unwrap());
        exit(0);
    }

    if config_arg.is_some() {
        let config_path_str = config_arg.unwrap().split('=').nth(1).unwrap_or("");
        if config_path_str.is_empty() {
            eprintln!("No config path provided. Use --config=<path> to specify a config file.");
            exit(1);
        }
        config_path = PathBuf::from(config_path_str);
        if !config_path.exists() {
            eprintln!("Config file does not exist: {}", config_path.display());
            exit(1);
        }
    }

    let enviroment_dir = exe_path
        .parent()
        .map(|p| p.join(".."))
        .ok_or_else(|| {
            eprintln!("Failed to resolve parent of executable path.");
            exit(1);
        })
        .and_then(|p| p.canonicalize().or_else(|e| {
            eprintln!("Failed to canonicalize environment path: {}", e);
            exit(1);
        }))
        .unwrap();


    let use_colors = !no_color_flag;

    let mut config = if config_path.exists() {
        match load_config(&config_path) {
            Ok(config) => config,
            Err(_) => {
                eprintln!("Failed to read config file, activating environment...");
                if let Err(err) = activate_environment(&enviroment_dir, true) {
                    eprintln!("Failed to activate environment: {}", err);
                    exit(1);
                }
                match load_config(&config_path) {
                    Ok(config) => config,
                    Err(e) => {
                        eprintln!("Failed to load config file again after activation: {}", e);
                        eprintln!("Creating new empty config file at {}", config_path.display());
                        if let Err(err) = fs::write(&config_path, b"{}") {
                            eprintln!("Failed to create new config file: {}", err);
                            exit(1);
                        }
                        load_config(&config_path).unwrap_or_else(|e| {
                            eprintln!("Failed to load new config file: {}", e);
                            exit(1);
                        })
                    }
                }
            }
        }
    } else {
        eprintln!("Config file not found, activating environment...");
        if let Err(err) = activate_environment(&enviroment_dir, true) {
            eprintln!("Failed to activate environment: {}", err);
            exit(1);
        }
        match load_config(&config_path) {
            Ok(config) => config,
            Err(e) => {
                eprintln!("Failed to load config file after activation: {}", e);
                eprintln!("Creating new empty config file at {}", config_path.display());
                if let Err(err) = fs::write(&config_path, b"{}") {
                    eprintln!("Failed to create new config file: {}", err);
                    exit(1);
                }
                load_config(&config_path).unwrap_or_else(|e| {
                    eprintln!("Failed to load new config file: {}", e);
                    exit(1);
                })
            }
        }
    };    
    
    if activate_flag {
        println!("{}", format!("Activating environment at: {}", enviroment_dir.display()).cyan().bold());
        if let Err(err) = activate_environment(&enviroment_dir, false) {
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
        if let Err(err) = activate_environment(&enviroment_dir, true) {
            eprintln!("Failed to activate environment: {}", err);
            exit(1);
        }
        config = load_config(&config_path).unwrap();
    }

    let home_dir = config.home_dir
    .to_string()
    .replace("\\", "/");

    let home_dir_path = PathBuf::from(home_dir.clone());

    let debug_mode: String = if debug_flag {
        args.iter()
            .find(|arg| arg.starts_with("--debug-mode="))
            .and_then(|arg| arg.split('=').nth(1))
            .map(|mode| match mode {
                "full" | "normal" | "minimal" => mode.into(),
                _ => {
                    eprintln!("Invalid debug mode: '{}'. Valid modes are 'full', 'normal', or 'minimal'.", mode);
                    exit(1);
                }
            })
            .unwrap_or("normal".into())
    } else {
        config.debug_mode.clone()
    };

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
        config.debug_mode = "full".to_string();
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
                path = cwd.join(path);
            }

            if !path.exists() {
                eprintln!("Error: File '{}' does not exist or is not a valid file", path.display());
                exit(1);
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

    let moded = config.moded.clone();
    if allow_unsafe {
        config.allow_unsafe = true;
    }

    if config.version != VERSION {
        if !moded {
            handle_error(
                &Error::with_help(
                    "VersionMismatchError",
                    to_static(format!(
                        "Lucia version mismatch: expected {}, got {}. Please update your Lucia installation.",
                        VERSION, config.version
                    )),
                    "Set 'moded' to true in your config file to ignore this error.",
                    to_static(exe_path.display().to_string()),
                ),
                "",
                &config,
                use_colors,
            );
            exit(1);
        } else {
            debug_log(
                &format!(
                    "Warning: Lucia version mismatch: expected {}, got {}. Running in moded mode.",
                    VERSION, config.version
                ),
                &config,
                Some(use_colors),
            );
        }
    }

    if !non_flag_args.is_empty() {
        for file_path in non_flag_args {
            let path = Path::new(&file_path);

            let debug_mode_some = if config.debug {
                Some(config.debug_mode.clone())
            } else {
                None
            };
            
            execute_file(path, file_path.clone(), &config, use_colors, disable_preprocessor, home_dir_path.clone(), config_path.clone(), debug_mode_some, &argv, &dump_pp_flag, &dump_ast_flag);
        }
    } else {
        let debug_mode_some = if config.debug {
            Some(config.debug_mode.clone())
        } else {
            None
        };

        repl(config, use_colors, disable_preprocessor, home_dir_path, config_path, debug_mode_some, cwd.clone(), &argv, &dump_pp_flag, &dump_ast_flag);
    }
}

fn execute_file(
    path: &Path,
    file_path: String,
    config: &Config,
    use_colors: bool,
    disable_preprocessor: bool,
    home_dir_path: PathBuf,
    config_path: PathBuf,
    debug_mode: Option<String>,
    argv: &Vec<String>,
    dump_pp_flag: &bool,
    dump_ast_flag: &bool,
) {
    if path.exists() && path.is_file() {
        debug_log(&format!("Executing file: {:?}", path), &config, Some(use_colors));

        let file_content = fs::read_to_string(path).expect("Failed to read file");
        let lexer = Lexer::new(&file_content, file_path.clone());
        let raw_tokens = lexer.tokenize();

        let print_start_debug = debug_mode
            .as_ref()
            .map_or(false, |mode| mode == "full" || mode == "minimal");

        let print_intime_debug = debug_mode
            .as_ref()
            .map_or(false, |mode| mode == "full" || mode == "normal");

        let processed_tokens = if !disable_preprocessor {
            let mut preprocessor = Preprocessor::new(
                home_dir_path.join("libs"),
                config_path.clone(),
                file_path.as_str(),
            );
            match preprocessor.process(raw_tokens.clone(), path.parent().unwrap_or(Path::new(""))) {
                Ok(tokens) => tokens,
                Err(e) => {
                    handle_error(&e, &file_content, &config, use_colors);
                    exit(1);
                }
            }
        } else {
            raw_tokens.clone()
        };

        if *dump_pp_flag && !processed_tokens.is_empty() {
            dump_pp(
                processed_tokens.iter().collect(),
                path.to_str().unwrap_or(""),
                &config,
                use_colors,
            );
        }

        if print_start_debug {
            debug_log(
                &format!(
                    "Raw Tokens: {:?}",
                    processed_tokens
                        .iter()
                        .filter(|token| {
                            let t = token.0.as_str();
                            t != "WHITESPACE"
                                && t != "COMMENT_INLINE"
                                && t != "COMMENT_SINGLE"
                                && t != "COMMENT_MULTI"
                                && t != "EOF"
                        })
                        .map(|t| (t.0.clone(), t.1.clone()))
                        .collect::<Vec<_>>()
                ),
                &config,
                Some(use_colors),
            );
        }

        let tokens: Vec<Token> = processed_tokens;
        let mut parser =
            Parser::new(tokens.clone(), config.clone(), file_content.to_string(), use_colors, file_path.as_str());

        let statements = match parser.parse_safe() {
            Ok(stmts) => stmts,
            Err(error) => {
                debug_log("Error while parsing:", &config, Some(use_colors));
                handle_error(&error, &file_content, &config, use_colors);
                exit(1);
            }
        };

        if *dump_ast_flag && !statements.is_empty() {
            let file_path = path.with_extension("ast");
            dump_ast(
                statements.iter().collect(),
                file_path.to_str().unwrap_or(""),
                &config,
                use_colors,
            );
        }

        if print_start_debug {
            debug_log(
                &format!(
                    "Parsed Statements: [{}]",
                    statements
                        .iter()
                        .map(|stmt| format_value(&stmt.convert_to_map()))
                        .collect::<Vec<String>>()
                        .join(", ")
                ),
                &config,
                Some(use_colors),
            );
        }

        let parent_dir = if file_path.is_empty() {
            std_env::current_dir()
                .and_then(|p| p.canonicalize())
                .unwrap_or_else(|_| PathBuf::from("."))
        } else {
            PathBuf::from(file_path.clone())
                .parent()
                .map(Path::to_path_buf)
                .unwrap_or_else(|| PathBuf::from("."))
                .canonicalize()
                .unwrap_or_else(|_| PathBuf::from("."))
        };
        let mut interpreter = Interpreter::new(
            config.clone(),
            use_colors,
            file_path.as_str(),
            &parent_dir,
            (home_dir_path.join("libs"), config_path.clone(), !disable_preprocessor),
            argv,
        );
        let out: Value = match interpreter.interpret(statements, file_content.clone()) {
            Ok(out) => out,
            Err(error) => {
                debug_log("Error while interpreting:", &config, Some(use_colors));
                handle_error(&error, &file_content, &config, use_colors);
                exit(1);
            }
        };
    } else {
        eprintln!("Error: File '{}' does not exist or is not a valid file", file_path);
    }
}

fn repl(config: Config, use_colors: bool, disable_preprocessor: bool, home_dir_path: PathBuf, config_path: PathBuf, debug_mode: Option<String>, cwd: PathBuf, argv: &Vec<String>, dump_pp_flag: &bool, dump_ast_flag: &bool) {
    println!(
        "{}Lucia-{} REPL\nType 'exit()' to exit or 'help()' for help.{}",
        hex_to_ansi(&config.color_scheme.info, Some(use_colors)),
        config.version,
        hex_to_ansi("reset", Some(use_colors))
    );

    let mut interpreter = Interpreter::new(
        config.clone(),
        use_colors,
        "<stdin>",
        &cwd,
        (
            home_dir_path.join("libs"),
            config_path.clone(),
            !disable_preprocessor,
        ),
        argv,
    );

    let mut preprocessor = Preprocessor::new(
        home_dir_path.join("libs"),
        config_path.clone(),
        "<stdin>",
    );

    let print_start_debug = matches!(debug_mode.as_deref(), Some("full" | "minimal"));
    let print_intime_debug = matches!(debug_mode.as_deref(), Some("full" | "normal"));

    let mut line_number = 0;

    loop {
        line_number += 1;
        print!(
            "{}{}{} ",
            hex_to_ansi(&config.color_scheme.input_arrows, Some(use_colors)),
            ">>>",
            hex_to_ansi("reset", Some(use_colors))
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
            if let Err(_) = clear_terminal() {
                handle_error(
                    &Error::new("IOError", "Failed to clear screen", "<stdin>"),
                    &input,
                    &config,
                    use_colors,
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

        let lexer = Lexer::new(&input, "<stdin>".into());
        let raw_tokens = lexer.tokenize();

        let current_dir = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));

        let processed_tokens = if !disable_preprocessor {
            match preprocessor.process(raw_tokens.clone(), &current_dir) {
                Ok(toks) => toks,
                Err(e) => {
                    handle_error(&e.clone(), &input, &config, use_colors);
                    continue;
                }
            }
        } else {
            raw_tokens
        };

        if *dump_pp_flag && !processed_tokens.is_empty() {
            let file_path = cwd.join(format!("stdin-{}", line_number));
            dump_pp(
                processed_tokens.iter().collect(),
                file_path.to_str().unwrap_or("stdin"),
                &config,
                use_colors,
            );
        }

        if print_start_debug {
            let filtered = processed_tokens
                .iter()
                .filter(|token| {
                    let t = &token.0;
                    t != "WHITESPACE" && !t.starts_with("COMMENT_") && t != "EOF"
                })
                .map(|token| (&token.0, &token.1))
                .collect::<Vec<_>>();
        
            debug_log(&format!("Tokens: {:?}", filtered), &config, Some(use_colors));
        }        

        let tokens = processed_tokens;

        let mut parser = Parser::new(
            tokens,
            config.clone(),
            input.to_string(),
            use_colors,
            "<stdin>",
        );

        let statements = match parser.parse_safe() {
            Ok(stmts) => stmts,
            Err(error) => {
                if print_intime_debug {
                    debug_log(
                        &format!("Error while parsing: {}", error.msg()),
                        &config,
                        Some(use_colors),
                    );
                }
                handle_error(&error.clone(), &input, &config, use_colors);
                continue;
            }
        };

        if *dump_ast_flag && !statements.is_empty() {
            let file_path = cwd.join(format!("stdin-{}", line_number));
            dump_ast(
                statements.iter().collect(),
                file_path.to_str().unwrap_or("stdin"),
                &config,
                use_colors,
            );
        }

        if print_start_debug {
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
        }

        let out = match interpreter.interpret(statements, input.clone()) {
            Ok(out) => {
                if interpreter.is_stopped() {
                    exit(0);
                }
                out
            }
            Err(error) => {
                if print_intime_debug {
                    debug_log("Error while interpreting:", &config, Some(use_colors));
                }
                handle_error(&error.clone(), &input, &config, use_colors);
                continue;
            }
        };

        if !matches!(out, Value::Null) {
            println!("{}", format_value(&out));
        }
    }
}

fn main() {
    let args: Vec<String> = std_env::args().collect();

    panic::set_hook(Box::new(|panic_info| {
        let msg = if let Some(s) = panic_info.payload().downcast_ref::<&str>() {
            *s
        } else if let Some(s) = panic_info.payload().downcast_ref::<String>() {
            s.as_str()
        } else {
            "Unknown panic message"
        };

        let location = panic_info.location()
            .map(|loc| format!("at {}:{}:{}", loc.file(), loc.line(), loc.column()))
            .unwrap_or_else(|| "at unknown location".to_string());

        let build_info = get_build_info();

        eprintln!("{}", "Oops! The program panicked!".red().bold());
        eprintln!("Message: {}", msg.red());
        eprintln!("{}", location.red());
        eprintln!();
        eprintln!("{}", "--- Build info ---".dimmed());
        eprintln!("Version: {}", build_info.version.cyan());
        eprintln!("Rustc Version: {}", build_info.rustc_version.cyan());
        eprintln!("Rustc Channel: {}", build_info.rustc_channel.cyan());
        eprintln!("Target: {}", build_info.target.cyan());
        eprintln!("Git commit: {}", build_info.git_hash.cyan());
        eprintln!("Build profile: {}", build_info.profile.cyan());
        eprintln!("CI: {}", build_info.ci.cyan());
        eprintln!("Dependencies: {}", build_info.dependencies.cyan());
        eprintln!("{}", "------------------".dimmed());
        eprintln!("{}", "Please report this bug with the above information:".yellow());
        eprintln!("{}", "https://github.com/SirPigari/lucia-rust/issues/new".blue().underline());
        eprintln!("{}", "------------------".dimmed());
        exit(101);        
    }));

    let stack_size = args.iter()
        .find(|arg| arg.starts_with("--stack-size="))
        .and_then(|arg| arg.split('=').nth(1))
        .and_then(|size| size.parse::<usize>().ok())
        .unwrap_or_else(|| {
            match sys_info::mem_info() {
                Ok(mem) => {
                    let total_ram_bytes = mem.total * 1024;

                    if total_ram_bytes > 16 * 1024 * 1024 * 1024 {
                        64 * 1024 * 1024
                    } else if total_ram_bytes > 8 * 1024 * 1024 * 1024 {
                        32 * 1024 * 1024
                    } else {
                        8 * 1024 * 1024
                    }
                }
                Err(_) => {
                    8 * 1024 * 1024
                }
            }
        });

    let handle = thread::Builder::new()
        .stack_size(stack_size)
        .name(format!("Lucia-{}", VERSION))
        .spawn(|| {
            lucia(args);
        })
        .unwrap();

    handle.join().unwrap();
}