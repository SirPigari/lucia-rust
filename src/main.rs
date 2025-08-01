use std::env as std_env;
use std::fs::{self, File};
use std::io::{self, Read, Write};
use std::{thread, panic};
use std::path::{Path, PathBuf};
use std::process::{exit, Command};
use std::collections::HashMap;
use colored::*;
use std::sync::{
    atomic::{AtomicBool, Ordering},
    Arc,
};
use std::time::Duration;
use sys_info;

mod env {
    pub mod runtime {
        pub mod utils;
        pub mod config;
        pub mod types;
        pub mod errors;
        pub mod value;
        pub mod functions;
        pub mod generators;
        pub mod variables;
        pub mod native;
        pub mod statements;
        pub mod pattern_reg;
        pub mod preprocessor;
        pub mod objects;
        pub mod libs;
        pub mod tokens;
        pub mod internal_structs;
        pub mod precompile;
        pub mod cache;
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
        pub mod fs {
            pub mod __init__;
        }
        pub mod nest {
            pub mod __init__;
        }
        pub mod libload {
            pub mod __init__;
            pub mod ffi;
        }
    }
    pub mod transpiler {
        pub mod transpiler;
    }
}

mod lexer;
mod parser;
mod interpreter;

use crate::env::runtime::config::{Config, ColorScheme, Libs};
use crate::env::runtime::utils::{find_closest_match, supports_color, ctrl_t_pressed, fix_path, read_input, hex_to_ansi, get_line_info, format_value, check_ansi, clear_terminal, to_static, print_colored, escape_string, remove_loc_keys, unique_temp_name, KEYWORDS};
use crate::env::runtime::types::VALID_TYPES;
use crate::env::runtime::errors::Error;
use crate::env::runtime::value::Value;
use crate::env::runtime::preprocessor::Preprocessor;
use crate::env::runtime::statements::Statement;
use crate::env::runtime::internal_structs::{BuildInfo, CacheFormat};
use crate::env::runtime::tokens::{Token, Location};
use crate::env::runtime::libs::load_std_libs;
use crate::env::runtime::cache::{save_tokens_to_cache, load_tokens_from_cache, save_interpreter_cache, load_interpreter_cache};
use crate::parser::Parser;
use crate::lexer::{Lexer, SyntaxRule};
use crate::interpreter::Interpreter;
use crate::env::transpiler::transpiler::Transpiler;

const VERSION: &str = env!("VERSION");

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

#[cold]
pub fn handle_error(
    error: &Error,
    source: &str,
    config: &Config,
) {
    let use_colors = config.supports_color;
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

        let dummy_loc;
        let loc = match &err.loc {
            Some(loc) => loc,
            None => {
                dummy_loc = Location {
                    file: "<unknown>".into(),
                    line_string: String::new(),
                    line_number: 0,
                    range: (0, 0),
                };
                &dummy_loc
            }
        };

        let file_name = loc.file.strip_prefix(r"\\?\").unwrap_or(&loc.file);
        let line_number = loc.line_number;
        let range = loc.range;
        let col = range.0;
        let reset = hex_to_ansi("reset", use_colors);

        let current_line = if line_number > 0 {
            get_line_info(source, line_number).unwrap_or_default()
        } else {
            String::new()
        };

        let prev_line = if line_number > 1 {
            get_line_info(source, line_number - 1)
        } else {
            None
        };

        let next_line = if line_number > 0 {
            get_line_info(source, line_number + 1)
        } else {
            None
        };

        let indent = " ".repeat(line_number.to_string().len());

        let mut arrows_under = String::new();
        if line_number > 0 {
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
        }

        trace.push_str(&format!(
            "{}-> File '{}:{}:{}' got error:\n",
            hex_to_ansi(&config.color_scheme.exception, use_colors),
            file_name,
            line_number,
            col
        ));

        if line_number > 0 {
            if prev_line.is_some() {
                trace.push_str(&format!("\t{} ...\n", indent));
            }

            trace.push_str(&format!("\t{} | {}\n", line_number, current_line));
            trace.push_str(&format!("\t{} | {}\n", indent, arrows_under));

            if next_line.is_some() {
                trace.push_str(&format!("\t{} ...\n", indent));
            }
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
                    hex_to_ansi(&config.color_scheme.help, use_colors),
                    check_ansi("\x1b[1m", &use_colors),
                    check_ansi("\x1b[22m", &use_colors),
                    help,
                    hex_to_ansi(&config.color_scheme.exception, use_colors)
                ));
            }
        }

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
                hex_to_ansi(&config.color_scheme.help, use_colors),
                help,
                hex_to_ansi("reset", use_colors)
            ),
            _ => String::new(),
        };

        eprintln!(
            "{}[err] {} -> {}: {}{}{}{}",
            hex_to_ansi(&config.color_scheme.exception, use_colors),
            location_str,
            current.error_type,
            current.msg,
            depth_note,
            help_msg,
            hex_to_ansi("reset", use_colors)
        );
        return;
    }

    let mut trace = String::new();
    let mut current_error = Some(error);

    while let Some(err) = current_error {
        trace.push_str(&print_single_error(err));
        if let Some(ref inner) = err.ref_err {
            trace.push_str(&format!(
                "\n\t{}^-- caused by:\n",
                hex_to_ansi(&config.color_scheme.exception, use_colors)
            ));
            trace.push_str(&hex_to_ansi("reset", use_colors));
            current_error = Some(inner);
        } else {
            current_error = None;
        }
    }

    if let Some(loc) = error.loc.as_ref() {
        if !(loc.file == "<stdin>") {
            trace.push_str("\n");
        }
    }

    eprintln!("{}", trace);
}

fn debug_log(message: &str, config: &Config) {
    let use_colors = config.supports_color;
    if config.debug {
        print_colored(message, &config.color_scheme.debug, Some(use_colors));
    }
}

// TODO: Fix indentation and formatting for pp dump
fn dump_pp(tokens: Vec<&Token>, dump_dir: &str, filename: &str, config: &Config) {
    let use_colors = config.supports_color;
    let new_line_keywords = [
        "match", "import", "throw", "try", "catch", "for", "while"
    ];

    let mut i_buffer = String::from("[\n");
    let mut pp_buffer = String::from("// This file was generated by lucia preprocessor dump\n// You can just rename this to .lc file and it will work just fine\n\n");

    let mut prev_kind: Option<&str> = None;
    let mut prev_val: Option<&str> = None;
    let mut indent_level: usize = 0;
    let indent_str = "    "; // 4 spaces

    fn write_indent(buf: &mut String, level: usize, indent_str: &str) {
        for _ in 0..level {
            buf.push_str(indent_str);
        }
    }

    for (i, token) in tokens.iter().enumerate() {
        let Token(kind, val, _) = token;

        if kind == "EOF" {
            break;
        }

        let escaped_val = if kind == "STRING" {
            escape_string(val).unwrap_or_else(|_| val.clone()).replace("%", "%%")
        } else {
            val.clone()
        };

        let line = format!("  (\"{}\", \"{}\")", kind, escaped_val);
        if i != tokens.len() - 1 {
            i_buffer.push_str(&format!("{},\n", line));
        } else {
            i_buffer.push_str(&format!("\n{}\n", line));
        }

        if new_line_keywords.contains(&val.as_str()) && pp_buffer.chars().last() != Some('\n') {
            pp_buffer.push('\n');
            write_indent(&mut pp_buffer, indent_level, indent_str);
        }

        match (kind.as_str(), val.as_str()) {
            ("IDENTIFIER", "end") => {
                indent_level = indent_level.saturating_sub(1);
                if !pp_buffer.ends_with('\n') {
                    pp_buffer.push('\n');
                }
                write_indent(&mut pp_buffer, indent_level, indent_str);
                pp_buffer.push_str("end\n");

                prev_kind = Some(kind);
                prev_val = Some(val);
                continue;
            }
            ("SEPARATOR", ":") => {
                let next_token = tokens.get(i + 1);
                let next_val = next_token.map(|t| t.1.as_str());

                let last_char = pp_buffer.chars().rev().find(|c| !c.is_whitespace());

                pp_buffer.push(':');

                let block_starter = next_val.map(|v| !VALID_TYPES.contains(&v)).unwrap_or(false);

                if block_starter && last_char == Some(')') {
                    pp_buffer.push('\n');
                    indent_level += 1;
                } else {
                    pp_buffer.push(' ');
                }

                prev_kind = Some(kind);
                prev_val = Some(val);
                continue;
            }
            _ => {}
        }

        let last_char = pp_buffer.chars().last();

        if last_char == Some('\n') {
            write_indent(&mut pp_buffer, indent_level, indent_str);
        } else {
            let need_space = if kind == "IDENTIFIER" {
                match prev_val {
                    Some("(") | Some("[") | Some("{") | Some("!") => false,
                    _ => match (prev_kind, kind.as_str()) {
                        (_, "SEPARATOR") if val == "(" && KEYWORDS.contains(&prev_val.unwrap_or(&String::new())) => true,
                        (_, "SEPARATOR") if val == ")" => false,
                        (Some("OPERATOR"), _) if val != "&" && val != "|" => true,
                        (_, "OPERATOR") if val != "&" && val != "|" => true,
                        (Some("SEPARATOR"), _) if val == "(" || val == "[" => false,
                        (_, "SEPARATOR") if val == ")" || val == "]" || val == "," || val == ":" => false,
                        (Some("IDENTIFIER"), "SEPARATOR") if val == "(" => false,
                        (Some(prev), _) if prev != "SEPARATOR" => true,
                        _ => true,
                    },
                }
            } else {
                match (prev_kind, kind.as_str()) {
                    (Some("SEPARATOR"), "BOOLEAN") if ["[", "{", "("].contains(&prev_val.unwrap_or(&String::new())) => false,
                    (_, "SEPARATOR") if val == "(" && KEYWORDS.contains(&prev_val.unwrap_or(&String::new())) => true,
                    (_, "SEPARATOR") if val == ")" => false,
                    (Some("OPERATOR"), _) if val != "&" && val != "|" => true,
                    (_, "OPERATOR") if val != "&" && val != "|" => true,
                    (Some("SEPARATOR"), _) if val == "(" || val == "[" => false,
                    (_, "SEPARATOR") if val == ")" || val == "]" || val == "," || val == ":" => false,
                    (Some("IDENTIFIER"), "SEPARATOR") if val == "(" => false,
                    (Some(prev), _) if prev != "SEPARATOR" => true,
                    _ => true,
                }
            };

            if need_space {
                pp_buffer.push(' ');
            }
        }

        match kind.as_str() {
            "SEPARATOR" if val == "," => {
                pp_buffer.push_str(", ");
            }
            _ => {
                pp_buffer.push_str(val);
            }
        }

        prev_kind = Some(kind);
        prev_val = Some(val);
    }

    i_buffer.push(']');

    let base_path = Path::new(dump_dir)
        .join(filename)
        .with_extension("")
        .to_string_lossy()
        .to_string();

    if let Err(err) = File::create(format!("{base_path}.i"))
        .and_then(|mut f| f.write_all(i_buffer.as_bytes()))
    {
        eprintln!(
            "{} Failed to write to {base_path}.i: {err}",
            hex_to_ansi(&config.color_scheme.exception, use_colors)
        );
    }

    if let Err(err) = File::create(format!("{base_path}.pp"))
        .and_then(|mut f| f.write_all(pp_buffer.trim_end().as_bytes()))
    {
        eprintln!(
            "{} Failed to write to {base_path}.pp: {err}",
            hex_to_ansi(&config.color_scheme.exception, use_colors)
        );
    }

    print_colored(
        &format!(
            "{}Preprocessed source code dumped to {}.i and {}.pp",
            hex_to_ansi(&config.color_scheme.debug, use_colors),
            base_path,
            base_path
        ),
        &config.color_scheme.debug,
        Some(use_colors),
    );
}

fn dump_ast(tokens: Vec<&Statement>, dump_dir: &str, filename: &str, config: &Config) {
    let use_colors = config.supports_color;
    let base_path = Path::new(dump_dir)
        .join(filename)
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
                    hex_to_ansi(&config.color_scheme.exception, use_colors),
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
                    hex_to_ansi(&config.color_scheme.exception, use_colors),
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
                        hex_to_ansi(&config.color_scheme.debug, use_colors),
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
                        hex_to_ansi(&config.color_scheme.exception, use_colors),
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

fn load_config(config_path: &Path) -> Result<Config, String> {
    let mut cfg_file = File::open(config_path).map_err(|_| "Config file not found")?;
    let mut cfg_contents = String::new();
    let _bytes_read = cfg_file.read_to_string(&mut cfg_contents).map_err(|_| "Failed to read config file")?;

    let config: Config = match serde_json::from_str::<Config>(&cfg_contents) {
        Ok(config) => config,
        Err(e) => return Err(format!("Failed to deserialize JSON: {}", e)),
    };
    Ok(config)
}

fn create_config_file(path: &Path, env_path: &Path) -> io::Result<()> {
    let default_config = Config {
        version: VERSION.to_string(),
        moded: false,
        debug: false,
        debug_mode: "normal".to_string(),
        supports_color: true,
        use_lucia_traceback: true,
        warnings: true,
        use_preprocessor: true,
        cache_format: CacheFormat::NoCache,
        allow_fetch: true,
        allow_unsafe: false,
        home_dir: fix_path(env_path.to_str().unwrap_or(".").to_string()),
        stack_size: 16777216,
        color_scheme: ColorScheme {
            exception: "#F44350".to_string(),
            warning: "#F5F534".to_string(),
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

    fs::write(&path, config_str)
        .map_err(|_| io::Error::new(io::ErrorKind::Other, "Failed to write config file"))?;
    Ok(())
}

fn create_libs_file(path: &Path) -> io::Result<()> {
    let mut default_libs = Libs::new();
    default_libs.set_std_libs(
        crate::env::runtime::libs::_STD_LIBS
            .iter()
            .map(|(k, v)| (k.to_string(), v.clone()))
            .collect()
    );

    let libs_str = serde_json::to_string_pretty(&default_libs)
        .map_err(|_| io::Error::new(io::ErrorKind::InvalidData, "Failed to serialize libs"))?;

    fs::write(&path, libs_str)
        .map_err(|_| io::Error::new(io::ErrorKind::Other, "Failed to write libs file"))?;
    Ok(())
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
        let config_res: Result<Config, _> = serde_json::from_str(&config_data);

        match config_res {
            Ok(mut config) => {
                if config.moded {
                    config.home_dir = env_path_str;
                    let config_str = serde_json::to_string_pretty(&config)
                        .map_err(|_| io::Error::new(io::ErrorKind::InvalidData, "Failed to serialize config"))?;
                    fs::write(&config_path, config_str)
                        .map_err(|_| io::Error::new(io::ErrorKind::Other, "Failed to write config file"))?;
                    return Ok(());
                }
            }
            Err(_) => {
                eprintln!("Warning: Failed to deserialize config, overwriting with default config.");
            }
        }
    }

    create_config_file(&config_path, env_path)?;
    let libs_path = env_path.join("libs.json");
    if !libs_path.exists() {
        create_libs_file(&libs_path)?;
    }

    Ok(())
}

fn lucia(args: Vec<String>) {
    let cwd = std_env::current_dir()
    .and_then(|p| p.canonicalize())
    .unwrap_or_else(|e| {
        eprintln!("Failed to get canonicalized current directory: {}", e);
        exit(1);
    });

    let exe_path = std_env::current_exe()
        .and_then(|p| p.canonicalize())
        .unwrap_or_else(|e| {
            eprintln!("Failed to get canonicalized path to executable: {}", e);
            exit(1);
        });

    let mut config_path = exe_path
        .parent()
        .map(|p| p.join("..").join("config.json"))
        .ok_or_else(|| {
            eprintln!("Failed to resolve parent of executable path.");
            exit(1);
        })
        .unwrap();
    
    let libs_path = exe_path
        .parent()
        .map(|p| p.join("..").join("libs.json"))
        .ok_or_else(|| {
            eprintln!("Failed to resolve parent of executable path.");
            exit(1);
        })
        .unwrap();

    
    if !config_path.exists() {
        eprintln!("Config file not found at {}, creating empty config.", config_path.display());
        if let Err(e) = std::fs::write(&config_path, "{}") {
            eprintln!("Failed to create empty config file: {}", e);
            exit(1);
        }
    }    

    let activate_flag = args.contains(&"--activate".to_string());
    let mut no_color_flag = args.contains(&"--no-color".to_string());
    if args.contains(&"--color".to_string()) {
        no_color_flag = false;
    }
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
    let compile_flag = args.contains(&"--compile".to_string()) || args.contains(&"-c".to_string());
    let run_flag = args.contains(&"--run".to_string()) || args.contains(&"-r".to_string());
    let dump_dir = args.iter()
        .find(|arg| arg.starts_with("--dump-dir="))
        .map(|arg| (arg.trim_start_matches("--dump-dir="), true))
        .unwrap_or((".", false));
    let cache: (CacheFormat, bool) = match args.iter().find(|arg| arg.starts_with("--cache=")) {
        Some(arg) => {
            let val_str = arg.trim_start_matches("--cache=");
            match CacheFormat::from_str(val_str) {
                Some(val) => (val, true),
                None => {
                    eprintln!("Invalid value for --cache, defaulting to 'bin_le'.");
                    (CacheFormat::BinLe, true)
                }
            }
        },
        None => (CacheFormat::NoCache, false),
    };
    let cls_cache_flag = args.contains(&"--clean-cache".to_string()) || args.contains(&"-cc".to_string()) || args.contains(&"--clear-cache".to_string()) || args.contains(&"--cls-cache".to_string());
    let c_compiler = args.iter()
        .find(|arg| arg.starts_with("--c-compiler="))
        .map(|arg| arg.trim_start_matches("--c-compiler="))
        .unwrap_or("gcc");
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

    let commands: Vec<String> = [
        "--activate",
        "-a",
        "--no-color",
        "--color",
        "--quiet",
        "-q",
        "--debug",
        "-d",
        "--debug-mode=",
        "--exit",
        "-e",
        "--info",
        "-i",
        "--help",
        "-h",
        "--version",
        "-v",
        "--build-info",
        "--disable-preprocessor",
        "-dp",
        "--config=",
        "--dump-pp",
        "--dump-ast",
        "--dump",
        "--allow-unsafe",
        "--stack-size=",
        "--compile",
        "-c",
        "--run",
        "-r",
        "--cache=",
        "--clean-cache",
        "-cc",
        "--clear-cache",
        "--cls-cache",
        "--c-compiler=",
        "--argv=",
        "--",
        "--dump-dir=",
    ].iter().map(|s| s.to_string()).collect();

    for arg in &args {
        if arg.starts_with("--") {
            let is_valid = if commands.contains(arg) {
                true
            } else if let Some(eq_pos) = arg.find('=') {
                let prefix = &arg[..=eq_pos];
                commands.iter().any(|cmd| cmd == prefix)
            } else {
                false
            };

            if !is_valid {
                eprintln!("Unknown argument: {}", arg);
                if let Some(suggestion) = find_closest_match(arg, &commands) {
                    eprintln!("Did you mean: {}", suggestion);
                }
                exit(1);
            }
        }
    }

    if help_flag {
        println!("{}", "Usage:".bold());
        println!("  lucia [options] [files...]\n");
    
        println!("{}", "Options:".bold());
        let options = [
            ("--activate, -a", "Activate the environment"),
            ("--no-color", "Disable colored output"),
            ("--color", "Enable colored output (default)"),
            ("--quiet, -q", "Suppress debug and warning messages"),
            ("--debug, -d", "Enable debug mode"),
            ("--debug-mode=<mode>", "Set debug mode (full, normal, minimal)"),
            ("--exit, -e", "Exit if no files are provided"),
            ("--info, -i", "Show build and environment information"),
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
            ("--compile, -c", "Compile the source code to a binary"),
            ("--run, -r", "Run the source code after compiling"),
            ("--cache=<format>", "Enable or disable caching (default: 'no_cache') (check config-guide.md)"),
            ("--clean-cache, -cc", "Clear the cache directory"),
            ("--c-compiler=<compiler>", "Specify the C compiler to use for compilation (default: gcc)"),
            ("--argv=<args>", "Pass additional arguments to the interpreter as a JSON array"),
            ("--dump-dir=<path>", "Specify the directory to dump preprocessed and AST files (default: current directory)"),
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

    if args.contains(&"--info".to_string()) || args.contains(&"-i".to_string()) {
        let info = get_build_info();
        let binary_path = std_env::current_exe().unwrap();
        let binary_size = fs::metadata(&binary_path).unwrap().len();
    
        let size_str = if binary_size >= 1024 * 1024 {
            format!("{:.2} MB", binary_size as f64 / (1024.0 * 1024.0))
        } else if binary_size >= 1024 {
            format!("{:.2} KB", binary_size as f64 / 1024.0)
        } else {
            format!("{} B", binary_size)
        };
    
        println!("{}", "Binary Info".bold().bright_cyan().underline());
        println!("{:<35}{}", "Binary Path:".green(), binary_path.display());
        println!("{:<35}{}", "Binary Size:".green(), size_str);
        println!("{:<35}{}", "Target Triple:".green(), info.target);
        println!("{:<35}{}", "Build Profile:".green(), info.profile);
    
        println!();
        println!("{}", "Build Metadata".bold().bright_cyan().underline());
        println!("{:<35}{}", "Name:".green(), info.name);
        println!("{:<35}{}", "Version:".green(), info.version);
        println!("{:<35}{}", "UUID:".green(), info.uuid);
        println!("{:<35}{}", "Git Hash:".green(), info.git_hash);
        println!("{:<35}{}", "File Hash:".green(), info.file_hash);
        println!("{:<35}{}", "Build Date:".green(), info.build_date);
        println!("{:<35}{}", "Repository:".green(), info.repository);
    
        println!();
        println!("{}", "Environment".bold().bright_cyan().underline());
        println!("{:<35}{}", "Rustc Version:".green(), info.rustc_version);
        println!("{:<35}{}", "Rustc Channel:".green(), info.rustc_channel);
        println!("{:<35}{}", "CI System:".green(), info.ci);
        println!("{:<35}{}", "Dependencies:".green(), info.dependencies);

        println!();
        println!("{}", "Code Information".bold().bright_cyan().underline());
        println!("{:<35}{}", "Rust lines of code:".green(), env!("RUST_LOC"));
        println!("{:<35}{}", "Lucia lines of code:".green(), env!("LUCIA_LOC"));
        println!("{:<35}{}", "Total lines of code:".green(), env!("TOTAL_LOC"));
        println!("{:<35}{}", "Rust files:".green(), env!("RUST_FILES"));
        println!("{:<35}{}", "Total files:".green(), env!("TOTAL_FILES"));
        
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
                        if let Err(err) = create_config_file(&config_path, &enviroment_dir) {
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
                if let Err(err) = create_config_file(&config_path, &enviroment_dir) {
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

    if let Err(_) = std_env::set_current_dir(&home_dir_path) {
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
    config.debug_mode = debug_mode;
    if quiet_flag {
        config.debug = false;
        config.use_lucia_traceback = false;
        config.warnings = false;
    }
    if cache.1 {
        config.cache_format = cache.0;
    }

    if cls_cache_flag {
        config.cache_format = CacheFormat::NoCache;

        let cache_dirs = vec![
            (home_dir_path.join("cache"), true),    // optional
            (home_dir_path.join(".cache"), false),  // must exist
        ];

        for (cache_dir, optional) in cache_dirs {
            if !cache_dir.exists() {
                if optional {
                    continue;
                } else {
                    eprintln!("Required cache directory '.cache' does not exist.");
                    exit(1);
                }
            }

            if !cache_dir.is_dir() {
                eprintln!("'{}' is not a directory.", cache_dir.display());
                exit(1);
            }

            let mut error_found = false;

            match fs::read_dir(&cache_dir) {
                Ok(entries) => {
                    for entry in entries.flatten() {
                        let path = entry.path();

                        if path.is_file() {
                            if path.extension().and_then(|e| e.to_str()) == Some("bin") {
                                let _ = fs::remove_file(&path);
                            } else {
                                eprintln!("Found non-.bin file in '{}': {:?}, aborting.", cache_dir.display(), path);
                                error_found = true;
                                break;
                            }
                        } else if path.is_dir() {
                            let mut valid_subdir = true;

                            match fs::read_dir(&path) {
                                Ok(sub_entries) => {
                                    for sub_entry in sub_entries.flatten() {
                                        let sub_path = sub_entry.path();
                                        if sub_path.is_dir() {
                                            eprintln!("Found nested directory in subdir '{:?}', aborting.", sub_path);
                                            error_found = true;
                                            valid_subdir = false;
                                            break;
                                        }
                                        if sub_path.extension().and_then(|e| e.to_str()) == Some("bin") {
                                            let _ = fs::remove_file(&sub_path);
                                        } else {
                                            eprintln!("Found non-.bin file in subdir '{:?}', aborting.", sub_path);
                                            error_found = true;
                                            valid_subdir = false;
                                            break;
                                        }
                                    }

                                    if valid_subdir && !error_found {
                                        let _ = fs::remove_dir(&path);
                                    }
                                }
                                Err(e) => {
                                    eprintln!("Failed to read subdir '{}': {}", path.display(), e);
                                    error_found = true;
                                    break;
                                }
                            }
                        } else {
                            eprintln!("Unknown entry in cache: {:?}", path);
                            error_found = true;
                            break;
                        }
                    }
                }
                Err(e) => {
                    eprintln!("Failed to read cache directory '{}': {}", cache_dir.display(), e);
                    exit(1);
                }
            }

            if error_found {
                eprintln!("Aborting cache clean due to invalid files in '{}'.", cache_dir.display());
                exit(1);
            }

            if cache_dir.ends_with("cache") {
                let _ = fs::remove_dir(&cache_dir);
            } else {
                println!("Cache directory '{}' cleaned successfully.", fix_path(cache_dir.display().to_string()));
            }
        }
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
                eprintln!("Error: File '{}' does not exist or is not a valid file", fix_path(path.display().to_string()));
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
    if !supports_color() {
        config.supports_color = false;
    }
    if no_color_flag {
        config.supports_color = false;
    }

    match load_std_libs(&libs_path.display().to_string(), moded) {
        Ok((_, (f, n))) => {
            if f {
                print_colored(
                    &format!("Warning: Standard libraries mismatch for {}. Running in moded mode.", n),
                    &config.color_scheme.warning,
                    Some(config.supports_color),
                );
            }
        }
        Err((e, m)) => {
            if m {
                handle_error(
                    &Error::with_help(
                        "ModedSTDLibError",
                        &e,
                        "Set 'moded' to true in your config file to ignore this error.",
                        to_static(exe_path.display().to_string()),
                    ),
                    "",
                    &config,
                );
            } else {
                handle_error(
                    &Error::new(
                        "STDLibError",
                        &e,
                        to_static(exe_path.display().to_string()),
                    ),
                    "",
                    &config,
                );
            }
            exit(1);
        }
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
            );
            exit(1);
        } else if config.warnings {
            print_colored(
                &format!(
                    "Warning: Lucia version mismatch: expected {}, got {}. Running in moded mode.",
                    VERSION, config.version
                ),
                &config.color_scheme.warning,
                Some(config.supports_color),
            );
        }
    }

    if compile_flag {
        if non_flag_args.is_empty() {
            eprintln!("No files provided to compile. Exiting.");
            exit(1);
        }
        let result = compile(&config, non_flag_args.clone(), cwd, home_dir_path, config_path, dump_dir, dump_pp_flag, dump_ast_flag, run_flag, c_compiler);
        match result {
            Ok(message) => {
                if !quiet_flag {
                    println!("{}", message);
                }
                exit(0);
            }
            Err((code, errors)) => {
                for error in errors {
                    handle_error(&error, "", &config);
                }
                exit(code);
            }
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
            
            std_env::set_current_dir(&cwd).ok();
            execute_file(path, file_path.clone(), &config, disable_preprocessor, home_dir_path.clone(), config_path.clone(), debug_mode_some, &argv, dump_dir, &dump_pp_flag, &dump_ast_flag);
        }
    } else {
        let debug_mode_some = if config.debug {
            Some(config.debug_mode.clone())
        } else {
            None
        };

        std_env::set_current_dir(&cwd).ok();
        repl(config, disable_preprocessor, home_dir_path, config_path, debug_mode_some, cwd.clone(), &argv, dump_dir, &dump_pp_flag, &dump_ast_flag);
    }
}

fn execute_file(
    path: &Path,
    file_path: String,
    config: &Config,
    disable_preprocessor: bool,
    home_dir_path: PathBuf,
    config_path: PathBuf,
    debug_mode: Option<String>,
    argv: &Vec<String>,
    dump_dir: (&str, bool),
    dump_pp_flag: &bool,
    dump_ast_flag: &bool
) {
    if path.exists() && path.is_file() {
        debug_log(&format!("Executing file: {}", fix_path(path.display().to_string())), &config);

        let cache_dir = home_dir_path.join(".cache");
        if !cache_dir.exists() {
            if let Err(e) = std::fs::create_dir_all(&cache_dir) {
                debug_log(&format!("Failed to create cache directory: {}", e), &config);
            } else if cfg!(windows) {
                let status = Command::new("attrib")
                    .args(&["+h", cache_dir.to_str().unwrap()])
                    .status();
                if let Ok(status) = status {
                    if !status.success() {
                        debug_log("Failed to set hidden attribute on cache directory", &config);
                    }
                } else {
                    debug_log("Failed to run attrib command to hide cache directory", &config);
                }
            }
        }

        let file_content = match fs::read_to_string(path) {
            Ok(content) => content,
            Err(e) => {
                handle_error(&Error::with_help(
                    "FileReadError",
                    to_static(format!("Failed to read file '{}': {}", path.display(), e)),
                    "Check if the file exists and is readable.",
                    to_static(file_path.clone()),
                ), "", config);
                exit(1);
            }
        };

        let raw_tokens = Lexer::new(&file_content, to_static(file_path.clone()), None).tokenize();

        let processed_tokens = if !disable_preprocessor {
            if !config.cache_format.is_enabled() {
                if debug_mode.as_deref() == Some("full") || debug_mode.as_deref() == Some("minimal") {
                    debug_log("Cache disabled, reprocessing tokens", &config);
                }
                let mut preprocessor = Preprocessor::new(
                    home_dir_path.join("libs"),
                    file_path.as_str(),
                );
                match preprocessor.process(raw_tokens.clone(), path.parent().unwrap_or(Path::new(""))) {
                    Ok(tokens) => tokens,
                    Err(e) => {
                        handle_error(&e, &file_content, &config);
                        exit(1);
                    }
                }
            } else {
                let cached_processed = match load_tokens_from_cache(&cache_dir, &file_path, "processed", config.cache_format) {
                    Ok(opt) => opt,
                    Err(e) => {
                        debug_log(&format!("Failed to load processed tokens cache: {}", e), &config);
                        None
                    }
                };
                let cached_raw = match load_tokens_from_cache(&cache_dir, &file_path, "raw", config.cache_format) {
                    Ok(opt) => opt,
                    Err(e) => {
                        debug_log(&format!("Failed to load raw tokens cache: {}", e), &config);
                        None
                    }
                };

                let use_cache = match (cached_processed.as_ref(), cached_raw.as_ref()) {
                    (Some(processed), Some(raw)) if *raw == raw_tokens => {
                        if debug_mode.as_deref() == Some("full") || debug_mode.as_deref() == Some("minimal") {
                            debug_log("Loaded processed tokens from cache (raw tokens matched)", &config);
                        }
                        Some(processed.clone())
                    },
                    _ => None,
                };

                if let Some(tokens) = use_cache {
                    tokens
                } else {
                    if debug_mode.as_deref() == Some("full") || debug_mode.as_deref() == Some("minimal") {
                        debug_log("Raw tokens changed or cache missing, reprocessing and updating cache", &config);
                    }
                    let mut preprocessor = Preprocessor::new(
                        home_dir_path.join("libs"),
                        file_path.as_str(),
                    );
                    let tokens = match preprocessor.process(raw_tokens.clone(), path.parent().unwrap_or(Path::new(""))) {
                        Ok(tokens) => tokens,
                        Err(e) => {
                            handle_error(&e, &file_content, &config);
                            exit(1);
                        }
                    };
                    if let Err(e) = save_tokens_to_cache(&cache_dir, &file_path, "processed", &tokens, config.cache_format) {
                        debug_log(&format!("Failed to save processed tokens cache: {}", e), &config);
                    }
                    if let Err(e) = save_tokens_to_cache(&cache_dir, &file_path, "raw", &raw_tokens, config.cache_format) {
                        debug_log(&format!("Failed to save raw tokens cache: {}", e), &config);
                    }
                    tokens
                }
            }
        } else {
            raw_tokens
        };

        if *dump_pp_flag && !processed_tokens.is_empty() {
            let p = if dump_dir.1 {
                dump_dir.0.to_string()
            } else {
                Path::new(&file_path).parent().unwrap_or_else(|| Path::new(".")).to_str().unwrap_or("").to_string()
            };
            dump_pp(
                processed_tokens.iter().collect(),
                &p,
                &Path::new(&file_path).file_name().unwrap_or_default().to_str().unwrap_or(""),
                &config,
            );
        }

        let print_start_debug = debug_mode
            .as_ref()
            .map_or(false, |mode| mode == "full" || mode == "minimal");

        if print_start_debug {
            let filtered = processed_tokens
                .iter()
                .filter(|token| {
                    let t = &token.0;
                    t != "WHITESPACE" && !t.starts_with("COMMENT_") && t != "EOF"
                })
                .map(|token| (&token.0, &token.1))
                .collect::<Vec<_>>();

            debug_log(&format!("Tokens: {:?}", filtered), &config);
        }

        let tokens: Vec<Token> = processed_tokens;
        let mut parser = Parser::new(tokens.clone());

        let statements = match parser.parse_safe() {
            Ok(stmts) => stmts,
            Err(error) => {
                debug_log("Error while parsing:", &config);
                handle_error(&error, &file_content, &config);
                exit(1);
            }
        };

        if *dump_ast_flag && !statements.is_empty() {
            let p = if dump_dir.1 {
                dump_dir.0.to_string()
            } else {
                path.parent()
                    .map(|p| p.to_str().unwrap_or(""))
                    .unwrap_or("")
                    .to_string()
            };
            dump_ast(
                statements.iter().collect(),
                &p,
                &Path::new(&file_path).file_name().unwrap_or_default().to_str().unwrap_or(""),
                &config,
            );
        }

        if print_start_debug {
            debug_log(
                &format!(
                    "Statements: [{}]",
                    statements
                        .iter()
                        .map(|stmt| {
                            let cleaned = remove_loc_keys(&stmt.convert_to_map());
                            format_value(&cleaned)
                        })
                        .collect::<Vec<String>>()
                        .join(", ")
                ),
                &config,
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
            config.supports_color,
            file_path.as_str(),
            &parent_dir,
            (home_dir_path.join("libs"), config_path.clone(), !disable_preprocessor),
            argv,
        );

        if config.cache_format.is_enabled() {
            if let Ok(Some(cache)) = load_interpreter_cache(&cache_dir, config.cache_format) {
                interpreter.set_cache(cache);
            }
        }

        let _out: Value = match interpreter.interpret(statements, true) {
            Ok(out) => {
                if config.cache_format.is_enabled() {
                    if let Err(e) = save_interpreter_cache(&cache_dir, interpreter.get_cache(), config.cache_format) {
                        debug_log(&format!("Failed to save interpreter cache: {}", e), &config);
                    }
                }
                out
            }
            Err(error) => {
                debug_log("Error while interpreting:", &config);
                handle_error(&error, &file_content, &config);
                exit(1);
            }
        };
    } else {
        eprintln!("Error: File '{}' does not exist or is not a valid file", file_path);
    }
}

fn repl(
    config: Config,
    disable_preprocessor: bool,
    home_dir_path: PathBuf,
    config_path: PathBuf,
    debug_mode: Option<String>,
    cwd: PathBuf,
    argv: &Vec<String>,
    dump_dir: (&str, bool),
    dump_pp_flag: &bool,
    dump_ast_flag: &bool,
) {
    println!(
        "{}Lucia-{} REPL\nType 'exit()' to exit or 'help()' for help.{}",
        hex_to_ansi(&config.color_scheme.info, config.supports_color),
        config.version,
        hex_to_ansi("reset", config.supports_color)
    );

    let mut interpreter = Interpreter::new(
        config.clone(),
        config.supports_color,
        "<stdin>",
        &cwd,
        (
            home_dir_path.join("libs"),
            config_path.clone(),
            !disable_preprocessor,
        ),
        argv,
    );

    if config.cache_format.is_enabled() {
        let cache_dir = home_dir_path.join(".cache");
        if let Ok(Some(cache)) = load_interpreter_cache(&cache_dir, config.cache_format) {
            interpreter.set_cache(cache);
        }
    }

    let mut preprocessor = Preprocessor::new(
        home_dir_path.join("libs"),
        "<stdin>",
    );

    let print_start_debug = matches!(debug_mode.as_deref(), Some("full" | "minimal"));
    let print_intime_debug = matches!(debug_mode.as_deref(), Some("full" | "normal"));

    let mut line_number = 0;
    let mut syntax_rules: HashMap<String, SyntaxRule> = HashMap::new();

    let stop_flag = Arc::new(AtomicBool::new(false));

    loop {
        line_number += 1;
        print!(
            "{}{}{} ",
            hex_to_ansi(&config.color_scheme.input_arrows, config.supports_color),
            ">>>",
            hex_to_ansi("reset", config.supports_color)
        );

        let mut input = read_input("");
        if input.is_empty() {
            continue;
        }

        if input.starts_with(':') {
            let new_input = match &*input {
                ":exit" => Some("exit()".into()),
                ":clear" | ":cls" => {
                    if let Err(_) = clear_terminal() {
                        handle_error(
                            &Error::new("IOError", "Failed to clear screen", "<stdin>"),
                            &input,
                            &config,
                        );
                    }
                    println!(
                        "{}Lucia-{} REPL\nType 'exit()' to exit or 'help()' for help.{}",
                        hex_to_ansi(&config.color_scheme.info, config.supports_color),
                        config.version,
                        hex_to_ansi("reset", config.supports_color)
                    );
                    None
                }
                ":help" | ":?" => {
                    println!("{}", "Tip: Use ':macro-help' to see REPL macros.".cyan());
                    Some("help()".into())
                }
                ":macro-help" | "::" => {
                    let macros = vec![
                        (":exit", "Exit the REPL"),
                        (":clear / :cls", "Clear the terminal screen"),
                        (":help / :?", "Show general help"),
                        (":macro-help", "Show this help message for REPL macros"),
                    ];

                    let max_len = macros.iter().map(|(m, _)| m.len()).max().unwrap_or(0);

                    println!("{}", "Available REPL macros:".bold().underline().blue());

                    for (name, desc) in macros {
                        println!(
                            "  {:width$} - {}",
                            name.green().bold(),
                            desc.white(),
                            width = max_len
                        );
                    }
                    None
                }
                ":trace" | ":traceback" => {
                    println!("{}", "Traceback:".bold().underline().cyan());
                    if interpreter.get_traceback().is_empty() {
                        println!(" - No traceback available.");
                    }
                    for (key, value) in interpreter.get_traceback() {
                        println!(" - {}: {}", key.cyan(), value);
                    }
                    None
                }
                _ => {
                    println!("Unknown REPL macro '{}'", input);
                    None
                }
            };

            if let Some(new_input) = new_input {
                input = new_input;
            } else {
                continue;
            }
        }

        if input == "exit" {
            println!("Use 'exit()' to exit.");
            continue;
        }

        if input == "help" || input == "?" {
            println!("Use ':help' for help.");
            continue;
        }

        if input == "\x03" {
            exit(0);
        }

        let mut lexer = Lexer::new(&input, "<stdin>".into(), Some(syntax_rules.clone()));

        let raw_tokens = lexer.tokenize();

        syntax_rules = lexer.get_syntax_rules();

        let current_dir = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));

        let processed_tokens = if !disable_preprocessor {
            match preprocessor.process(raw_tokens.clone(), &current_dir) {
                Ok(toks) => toks,
                Err(e) => {
                    handle_error(&e.clone(), &input, &config);
                    continue;
                }
            }
        } else {
            raw_tokens
        };

        if *dump_pp_flag && !processed_tokens.is_empty() {
            let file_path = cwd.join(format!("stdin-{}", line_number));
            let path = if dump_dir.1 {
                dump_dir.0
            } else {
                file_path.parent().unwrap_or_else(|| Path::new(".")).to_str().unwrap_or("")
            };
            dump_pp(
                processed_tokens.iter().collect(),
                &path,
                &Path::new(&file_path).file_name().unwrap_or_default().to_str().unwrap_or(""),
                &config,
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
        
            debug_log(&format!("Tokens: {:?}", filtered), &config);
        }        

        let tokens = processed_tokens;

        let mut parser = Parser::new(
            tokens,
        );

        let statements = match parser.parse_safe() {
            Ok(stmts) => stmts,
            Err(error) => {
                if print_intime_debug {
                    debug_log(
                        "Error while parsing",
                        &config,
                    );
                }
                handle_error(&error.clone(), &input, &config);
                continue;
            }
        };

        if *dump_ast_flag && !statements.is_empty() {
            let file_path = cwd.join(format!("stdin-{}", line_number));
            let path = if dump_dir.1 {
                dump_dir.0
            } else {
                file_path.parent().unwrap_or_else(|| Path::new(".")).to_str().unwrap_or("")
            };
            dump_ast(
                statements.iter().collect(),
                &path,
                &Path::new(&file_path).file_name().unwrap_or_default().to_str().unwrap_or(""),
                &config,
            );
        }

        if print_start_debug {
            debug_log(
                &format!(
                    "Statements: [{}]",
                    statements
                        .iter()
                        .map(|stmt| {
                            let cleaned = remove_loc_keys(&stmt.convert_to_map());
                            format_value(&cleaned)
                        })
                        .collect::<Vec<String>>()
                        .join(", ")
                ),
                &config,
            );
        }

        stop_flag.store(false, Ordering::Relaxed);

        let stop_flag_clone = stop_flag.clone();

        interpreter.stop_flag = Some(stop_flag_clone);

        let mut interpreter_clone = interpreter.clone();
        let statements_clone = statements.clone();
        let loc = statements.iter().rev().find_map(|stmt| {
            if let Statement::Statement { keys, values, loc } = stmt {
                let found = keys.iter().zip(values.iter()).any(|(k, v)| {
                    *k == Value::String("type".into())
                        && (*v == Value::String("FOR".into())
                            || *v == Value::String("WHILE".into())
                            || *v == Value::String("CALL".into()))
                });
                if found {
                    loc.clone()
                } else {
                    None
                }
            } else {
                None
            }
        }).or_else(|| {
            statements.iter().rev().find_map(|stmt| {
                if let Statement::Statement { loc, .. } = stmt {
                    loc.clone()
                } else {
                    None
                }
            })
        });

        let builder = thread::Builder::new()
            .name(format!("Lucia-{} REPL", VERSION))
            .stack_size(config.stack_size);

        let handle = builder.spawn(move || {
            let result = interpreter_clone.interpret(statements_clone, true);
            (result, interpreter_clone)
        }).expect("Failed to spawn thread");

        loop {
            if stop_flag.load(Ordering::Relaxed) {
                break;
            }
            if ctrl_t_pressed() {
                stop_flag.store(true, Ordering::Relaxed);
                
                let err = match loc {
                    Some(loc) => Error::with_location("KeyboardInterrupt", "Execution interrupted by user (Ctrl+T)", loc),
                    None => Error::new("KeyboardInterrupt", "Execution interrupted by user (Ctrl+T)", "<stdin>"),
                };

                handle_error(&err, &input, &config);
                break;
            }
            if handle.is_finished() {
                break;
            }
            thread::sleep(Duration::from_millis(50));
        }

        let (out_raw, i) = handle.join().unwrap();
        interpreter = i.clone();

        let out = match out_raw {
            Ok(out) => {
                if interpreter.is_stopped() {
                    if config.cache_format.is_enabled() {
                        let cache_dir = home_dir_path.join(".cache");
                        if let Err(e) = save_interpreter_cache(&cache_dir, interpreter.get_cache(), config.cache_format) {
                            debug_log(&format!("Failed to save interpreter cache: {}", e), &config);
                        }
                    }
                    exit(0);
                }
                out
            }
            Err(error) => {
                if print_intime_debug {
                    debug_log("Error while interpreting:", &config);
                }
                handle_error(&error.clone(), &input, &config);
                continue;
            }
        };

        if !matches!(out, Value::Null) {
            println!("{}", format_value(&out));
        }
    }
}

fn compile(
    config: &Config,
    files: Vec<String>,
    cwd: PathBuf,
    home_dir_path: PathBuf,
    config_path: PathBuf,
    dump_dir: (&str, bool),
    dump_pp_flag: bool,
    dump_ast_flag: bool,
    run_flag: bool,
    c_compiler: &str,
) -> Result<String, (i32, Vec<Error>)> {
    std_env::set_current_dir(&cwd).ok();
    for file in &files {
        let mut errors: Vec<Error> = vec![];
        let path = Path::new(file);
        if !path.exists() || !path.is_file() {
            return Err((1, vec![Error::new("FileNotFoundError", &format!("File '{}' does not exist or is not a valid file", file), file)]));
        }
        let mut lexer = Lexer::new(&fs::read_to_string(path).unwrap(), file, None);
        let raw_tokens = lexer.tokenize();
        let processed_tokens = if !config.use_preprocessor {
            raw_tokens.clone()
        } else {
            let mut preprocessor = Preprocessor::new(
                home_dir_path.join("libs"),
                file,
            );
            match preprocessor.process(raw_tokens.clone(), path.parent().unwrap_or(Path::new(""))) {
                Ok(tokens) => tokens,
                Err(e) => {
                    return Err((1, vec![e]));
                }
            }
        };
        if dump_pp_flag && !processed_tokens.is_empty() {
            let path = if dump_dir.1 {
                dump_dir.0
            } else {
                path.parent().unwrap_or_else(|| Path::new(".")).to_str().unwrap_or("")
            };
            dump_pp(
                processed_tokens.iter().collect(),
                &path,
                &Path::new(&file).file_name().unwrap_or_default().to_str().unwrap_or(""),
                config,
            );
        }
        let mut parser = Parser::new(processed_tokens);
        let ast = match parser.parse_safe() {
            Ok(stmts) => stmts,
            Err(error) => {
                errors.push(error);
                return Err((1, errors));
            }
        };
        if dump_ast_flag && !ast.is_empty() {
            let file_path = path.with_extension("ast");
            let path = if dump_dir.1 {
                dump_dir.0
            } else {
                &file_path.parent().unwrap_or_else(|| Path::new(".")).to_str().unwrap_or("")
            };
            dump_ast(
                ast.iter().collect(),
                path,
                &Path::new(&file_path).file_name().unwrap_or_default().to_str().unwrap_or(""),
                &config,
            );
        }
        let mut transpiler = Transpiler::new(
            ast,
            config.clone(),
            cwd.clone(),
            home_dir_path.clone(),
            config_path.clone(),
        );
        let c_code = match transpiler.transpile() {
            Ok(code) => code,
            Err(t_errors) => {
                errors.extend(t_errors);
                return Err((1, errors));
            }
        };

        let c_path = unique_temp_name("c", &home_dir_path);

        let mut c_file = match File::create(&c_path) {
            Ok(f) => f,
            Err(e) => {
                eprintln!("Failed to create temp C file: {e}");
                return Err((1, errors));
            }
        };

        if let Err(e) = write!(c_file, "{}", c_code) {
            eprintln!("Failed to write to C file: {e}");
            return Err((1, errors));
        }

        let binary_path = {
            let mut path = PathBuf::from(file);
            if cfg!(windows) {
                path.set_extension("exe");
            } else {
                path.set_extension("");
            }
            path
        };

        let mut compile_cmd = match c_compiler {
            "gcc" | "clang" | "cc" => {
                let mut cmd = Command::new(c_compiler);
                cmd.arg(&c_path)
                    .arg("-o")
                    .arg(&binary_path)
                    .arg("-O2");
                cmd
            }

            "tcc" => {
                let mut cmd = Command::new("tcc");
                cmd.arg(&c_path)
                    .arg("-o")
                    .arg(&binary_path)
                    .arg("-O");
                cmd
            }

            "cl" => {
                let mut cmd = Command::new("cl");
                cmd.arg(&c_path)
                    .arg(format!("/Fe:{}", binary_path.to_string_lossy()))
                    .arg("/O2")
                    .arg("/nologo");
                cmd
            }

            other => {
                let mut cmd = Command::new(other);
                cmd.arg(&c_path)
                    .arg("-o")
                    .arg(&binary_path)
                    .arg("-O2");
                cmd
            }
        };

        let output = match compile_cmd.output() {
            Ok(out) => out,
            Err(e) => {
                eprintln!("Failed to invoke compiler '{c_compiler}': {e}");
                return Err((1, errors));
            }
        };

        if !output.status.success() {
            eprintln!("Compilation failed:\n{}", String::from_utf8_lossy(&output.stderr));
            return Err((1, errors));
        }

        if run_flag {
            let status = match Command::new(&binary_path).status() {
                Ok(s) => s,
                Err(e) => {
                    eprintln!("Failed to run binary: {e}");
                    return Err((1, errors));
                }
            };

            if !status.success() {
                eprintln!("Program exited with non-zero status: {status}");
                return Err((1, errors));
            }
        }
        
    }
    Ok(String::new())
}

fn main() {
    let args: Vec<String> = std_env::args().collect();

    panic::set_hook(Box::new(|panic_info| {
        const CUSTOM_PANIC_MARKER: u8 = 0x1B;
    
        let msg = if let Some(s) = panic_info.payload().downcast_ref::<&str>() {
            *s
        } else if let Some(s) = panic_info.payload().downcast_ref::<String>() {
            s.as_str()
        } else {
            "Unknown panic message"
        };
    
        let (is_custom, display_msg) = if msg.as_bytes().first() == Some(&CUSTOM_PANIC_MARKER) {
            (true, &msg[1..])
        } else {
            (false, msg)
        };
    
        let location = panic_info.location()
            .map(|loc| format!("at {}:{}:{}", loc.file(), loc.line(), loc.column()))
            .unwrap_or_else(|| "at unknown location".to_string());
    
        let build_info = get_build_info();
    

        eprintln!("{}", "Oops! The program panicked!".red().bold());
        eprintln!("Message: {}", display_msg.red());
        eprintln!("{}", location.red());
        eprintln!();
        eprintln!("{}", "--- Build info ---".dimmed());
        eprintln!("Version: {}", build_info.version.cyan());
        eprintln!("Rustc Version: {}", build_info.rustc_version.cyan());
        eprintln!("Rustc Channel: {}", build_info.rustc_channel.cyan());
        eprintln!("Target: {}", build_info.target.cyan());
        eprintln!("Git commit: {}", build_info.git_hash.cyan());
        eprintln!("UUID: {}", build_info.uuid.cyan());
        eprintln!("Build profile: {}", build_info.profile.cyan());
        eprintln!("CI: {}", build_info.ci.cyan());
        eprintln!("Dependencies: {}", build_info.dependencies.cyan());
        eprintln!("{}", "------------------".dimmed());
    
        if !is_custom {
            eprintln!("{}", "Please report this bug with the above information:".yellow());
            eprintln!("{}", "https://github.com/SirPigari/lucia-rust/issues/new".blue().underline());
            eprintln!("{}", "------------------".dimmed());
        }
    
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
                        128 * 1024 * 1024
                    } else if total_ram_bytes > 8 * 1024 * 1024 * 1024 {
                        64 * 1024 * 1024
                    } else {
                        32 * 1024 * 1024
                    }
                }
                Err(_) => {
                    32 * 1024 * 1024
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
