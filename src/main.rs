use std::env as std_env;
use std::fs::{self, File};
use std::io::{self, Read, Write, BufRead, BufReader};
use std::{thread, panic};
use std::path::{Path, PathBuf};
use std::process::{exit, Command};
use std::collections::{HashSet, HashMap};
use colored::*;
use std::sync::{
    atomic::{AtomicBool, Ordering},
    Arc,
};
use std::time::{Duration, Instant};

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
        pub mod modules;
        pub mod libs;
        pub mod tokens;
        pub mod internal_structs;
        pub mod structs_and_enums;
        pub mod precompile;
        pub mod cache;
        pub mod fmt;
        pub mod static_checker;
        pub mod repl;
        pub mod plugins;
    }
    pub mod libs {
        #[cfg(feature = "math")]
        pub mod math {
            pub mod main;
        }

        #[cfg(feature = "os")]
        pub mod os {
            pub mod main;
        }

        #[cfg(feature = "time")]
        pub mod time {
            pub mod main;
        }

        #[cfg(feature = "json")]
        pub mod json {
            pub mod main;
        }

        #[cfg(feature = "clib")]
        pub mod clib {
            pub mod main;
        }

        #[cfg(feature = "regex")]
        pub mod regex {
            pub mod main;
            pub mod regex_engine;
        }

        #[cfg(feature = "collections")]
        pub mod collections {
            pub mod main;
            pub mod deprecated_stuff;
        }

        #[cfg(feature = "random")]
        pub mod random {
            pub mod main;
        }

        #[cfg(feature = "lasm")]
        pub mod lasm {
            pub mod main;
        }

        #[cfg(feature = "fs")]
        pub mod fs {
            pub mod main;
        }

        #[cfg(feature = "nest")]
        pub mod nest {
            pub mod main;
        }

        #[cfg(feature = "libload")]
        pub mod libload {
            pub mod main;
            pub mod ffi;
        }

        #[cfg(feature = "elevator")]
        pub mod elevator {
            pub mod main;
            pub mod utils;
        }

        #[cfg(feature = "hash")]
        pub mod hash {
            pub mod main;
        }
    }
    pub mod bundler {
        pub mod template {
            pub mod common;
        }
        pub mod bundle;
    }
}

mod lexer;
mod parser;
mod interpreter;

use crate::env::runtime::config::{Config, ColorScheme, TypeCheckerConfig, Libs};
use crate::env::runtime::utils::{find_closest_match, supports_color, ctrl_t_pressed, fix_path, hex_to_ansi, get_line_info, format_value, check_ansi, clear_terminal, to_static, print_colored, escape_string, wait_any_key};
use crate::env::runtime::repl::read_input;
use crate::env::runtime::errors::Error;
use crate::env::runtime::value::Value;
use crate::env::runtime::preprocessor::Preprocessor;
use crate::env::runtime::statements::{Statement, Node, get_loc};
use crate::env::runtime::internal_structs::{BuildInfo, CacheFormat};
use crate::env::runtime::tokens::{Token, ConcreteToken, Location};
use crate::env::runtime::libs::{check_project_deps};
use crate::env::runtime::static_checker::Checker;
use crate::env::runtime::fmt;
use crate::env::runtime::cache::{save_tokens_to_cache, load_tokens_from_cache, save_interpreter_cache, load_interpreter_cache};
use crate::env::runtime::plugins::PluginRuntime;
use crate::parser::Parser;
use crate::lexer::Lexer;
use crate::interpreter::Interpreter;
use crate::env::bundler::bundle::bundle_to_exe;

#[cfg(feature = "single_executable")]
use crate::env::runtime::libs::load_std_libs_embedded;
#[cfg(not(feature = "single_executable"))]
use crate::env::runtime::libs::load_std_libs;

const VERSION: &str = env!("VERSION");
const BUILD_UUID: &str = env!("BUILD_UUID");
const CUSTOM_PANIC_MARKER: u8 = 0x1B;

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
    const NEXT_LINES: bool = false;
    const PREV_LINES: bool = false;

    let use_colors = config.supports_color;
    let use_lucia_traceback = config.use_lucia_traceback;
    let debug = config.debug;

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
                    lucia_source_loc: "<unknown>".into(),
                    line_string: String::new(),
                    line_number: 0,
                    range: (0, 0),
                };
                &dummy_loc
            }
        };

        let lucia_source_loc = if debug && loc.lucia_source_loc != "<unknown>" && !loc.lucia_source_loc.is_empty() {
            format!("Raised from {}\n", loc.lucia_source_loc)
        } else {
            String::new()
        };

        let file_name = fix_path(loc.file.to_string());
        let line_number = loc.line_number;
        let range = loc.range;
        let col = range.0.saturating_add(1);
        let reset = hex_to_ansi("reset", use_colors);

        let current_line = if line_number > 0 {
            get_line_info(source, line_number).unwrap_or_else(|| {
                if file_name.starts_with('<') && file_name.ends_with('>') {
                    source.lines().next().unwrap_or_default().to_string()
                } else {
                    loc.line_string.clone()
                }
            })
        } else {
            loc.line_string.clone()
        };

        let prev_line = if line_number > 1 { get_line_info(source, line_number - 1) } else { None };
        let next_line = if line_number > 0 { get_line_info(source, line_number + 1) } else { None };

        let indent = " ".repeat(line_number.to_string().len());

        let mut arrows_under = String::new();
        if line_number > 0 {
            let chars: Vec<char> = current_line.chars().collect();
            let line_len = chars.len();
            let start = col.saturating_sub(1).min(line_len);
            let end = range.1.min(line_len);

            if start >= line_len || end == 0 || start >= end {
                arrows_under = " ".repeat(col) + "^";
            } else {
                arrows_under = "~".repeat(line_len);
                arrows_under = arrows_under.chars()
                    .enumerate()
                    .map(|(i, c)| if i >= start && i < end { '^' } else { c })
                    .collect();
            }
        }

        trace.push_str(&format!(
            "{}{}{}",
            hex_to_ansi(&config.color_scheme.debug, use_colors),
            lucia_source_loc,
            reset,
        ));

        trace.push_str(&format!(
            "{}-> File '{}:{}:{}' got error:\n",
            hex_to_ansi(&config.color_scheme.exception, use_colors),
            file_name,
            line_number,
            col
        ));

        if line_number > 0 {
            if let Some(prev) = prev_line.as_ref() {
                if prev.len() > 8 && PREV_LINES {
                    trace.push_str(&format!("\t{} | {}\n", line_number - 1, prev));
                }
            }

            trace.push_str(&format!("\t{} | {}\n", line_number, current_line));
            trace.push_str(&format!("\t{} | {}\n", indent, arrows_under));

            if let Some(next) = next_line.as_ref() {
                if next.len() > 8 && NEXT_LINES {
                    trace.push_str(&format!("\t{} ...\n", indent));
                }
            }
        }

        trace.push_str(&format!("\t{} | {}: {}", indent, err.err_type, err.err_msg));

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
            current.err_type,
            current.err_msg,
            depth_note,
            help_msg,
            hex_to_ansi("reset", use_colors)
        );
        return;
    }

    let mut trace = String::new();
    let mut current_error = Some(error);

    while let Some(err) = current_error {
        if let Some(ref inner) = err.ref_err {
            let same_loc = match (&err.loc, &inner.loc) {
                (Some(a), Some(b)) => a.file == b.file
                    && a.line_number == b.line_number
                    && a.range == b.range,
                _ => false,
            };

            if same_loc {
                trace.push_str(&print_single_error(err));

                let indent_len = err.loc.as_ref().map_or(0, |l| l.line_number.to_string().len());
                let indent = " ".repeat(indent_len);

                trace.push_str(&format!(
                    "\n\t{}^-- caused by:\n\t{} | {}: {}",
                    hex_to_ansi(&config.color_scheme.exception, use_colors),
                    indent,
                    (**inner).err_type,
                    (**inner).err_msg
                ));

                if let Some(help) = &inner.help {
                    if !help.is_empty() {
                        trace.push_str(&format!("\n\t{}   (Help: {})", indent, help));
                    }
                }

                current_error = inner.ref_err.as_deref();
                continue;
            }
        }

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

    trace.push_str(&hex_to_ansi("reset", use_colors));

    eprintln!("{}", trace);
}

fn debug_log(message: &str, config: &Config) {
    let use_colors = config.supports_color;
    if config.debug {
        print_colored(message, &config.color_scheme.debug, use_colors);
    }
}

fn dump_pp<'a>(tokens: Vec<&Token>, ctks: Vec<ConcreteToken<'a>>, dump_dir: &str, filename: &str, config: &Config) {
    let use_colors = config.supports_color;

    let pp_buffer = fmt::format_tokens(&ctks);
    
    let i_buffer = tokens.iter()
        .take_while(|t| t.0 != "EOF")
        .map(|t| {
            let escaped_val = if t.0 == "STRING" {
                escape_string(&t.1).unwrap_or_else(|_| t.1.to_string()).replace("%", "%%")
            } else if t.0 == "SEPARATOR" && t.1 == "\\" {
                "\\\\".to_string()
            } else {
                t.1.to_string()
            };
            format!("  (\"{}\", \"{}\")", t.0, escaped_val)
        })
        .collect::<Vec<_>>()
        .join(",\n");

    let i_buffer = format!("[\n{}\n]", i_buffer);

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

    // env var for quick prototype
    if std_env::var_os("LUCIA_DUMP_PP_JSON").is_some() {
        let json_path = format!("{}.pp.json", base_path);
        let json_data = match serde_json::to_string_pretty(&tokens) {
            Ok(j) => j,
            Err(e) => {
                print_colored(
                    &format!(
                        "{}Failed to serialize preprocessed tokens to JSON: {}",
                        hex_to_ansi(&config.color_scheme.exception, use_colors),
                        e
                    ),
                    &config.color_scheme.exception,
                    use_colors,
                );
                return;
            }
        };

        if let Err(err) = File::create(&json_path)
            .and_then(|mut f| f.write_all(json_data.as_bytes()))
        {
            eprintln!(
                "{} Failed to write to {}: {}",
                hex_to_ansi(&config.color_scheme.exception, use_colors),
                json_path,
                err
            );
        } else {
            print_colored(
                &format!(
                    "{}Dumped preprocessed tokens to {}",
                    hex_to_ansi(&config.color_scheme.debug, use_colors),
                    json_path
                ),
                &config.color_scheme.debug,
                use_colors,
            );
        }
    }

    print_colored(
        &format!(
            "{}Preprocessed source code dumped to {}.i and {}.pp",
            hex_to_ansi(&config.color_scheme.debug, use_colors),
            base_path,
            base_path
        ),
        &config.color_scheme.debug,
        use_colors,
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

    let data: Vec<String> = tokens.iter()
        .map(|stmt| stmt.format_for_debug())
        .collect();

    let json_i = match serde_json::from_str::<serde_json::Value>(&format!("[{}]", data.join(","))) {
        Ok(val) => match serde_json::to_string_pretty(&val) {
            Ok(pretty) => pretty,
            Err(e) => format!("[\"Failed to serialize: {}\"]", e),
        },
        Err(e) => format!("[\"Failed to serialize: {}\"]", e),
    };

    let json_ast = match serde_json::to_string_pretty(&tokens) {
        Ok(j) => j,
        Err(e) => {
            print_colored(
                &format!(
                    "{}Failed to serialize cleaned AST: {}",
                    hex_to_ansi(&config.color_scheme.exception, use_colors),
                    e
                ),
                &config.color_scheme.exception,
                use_colors,
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
                    use_colors,
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
                    use_colors,
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

#[cfg(not(feature = "single_executable"))]
fn load_config_with_fallback(primary_path: &Path, fallback_path: &Path, env_path: &Path) -> Result<Config, String> {
    if primary_path.exists() {
        match load_config(primary_path) {
            Ok(primary_config) => {
                if fallback_path.exists() && (fallback_path != primary_path) {
                    match load_config(fallback_path) {
                        Ok(fallback_config) => {
                            Ok(merge_configs(primary_config, fallback_config))
                        }
                        Err(_) => {
                            Ok(primary_config)
                        }
                    }
                } else {
                    Ok(primary_config)
                }
            }
            Err(_) => {
                if fallback_path.exists() && (fallback_path != primary_path) {
                    load_config(fallback_path)
                } else {
                    let default_config = create_default_config(env_path);
                    Ok(default_config)
                }
            }
        }
    } else if fallback_path.exists() {
        load_config(fallback_path)
    } else {
        let default_config = create_default_config(env_path);
        Ok(default_config)
    }
}

#[cfg(not(feature = "single_executable"))]
fn merge_configs(primary: Config, fallback: Config) -> Config {
    Config {
        version: if primary.version.is_empty() { fallback.version } else { primary.version },
        moded: primary.moded,
        debug: primary.debug,
        debug_mode: if primary.debug_mode.is_empty() { fallback.debug_mode } else { primary.debug_mode },
        supports_color: primary.supports_color,
        use_lucia_traceback: primary.use_lucia_traceback,
        warnings: primary.warnings,
        cache_format: primary.cache_format,
        allow_fetch: primary.allow_fetch,
        allow_unsafe: primary.allow_unsafe,
        allow_inline_config: primary.allow_inline_config,
        disable_runtime_type_checking: primary.disable_runtime_type_checking,
        home_dir: if primary.home_dir.is_empty() { fallback.home_dir } else { primary.home_dir },
        libs_paths: if primary.libs_paths.len() <= 1 { fallback.libs_paths } else { primary.libs_paths },
        stack_size: if primary.stack_size == 0 { fallback.stack_size } else { primary.stack_size },
        type_checker: merge_type_checker_configs(primary.type_checker, fallback.type_checker),
        color_scheme: merge_color_schemes(primary.color_scheme, fallback.color_scheme),
    }
}

#[cfg(not(feature = "single_executable"))]
fn merge_color_schemes(primary: ColorScheme, fallback: ColorScheme) -> ColorScheme {
    ColorScheme {
        exception: if primary.exception.is_empty() { fallback.exception } else { primary.exception },
        warning: if primary.warning.is_empty() { fallback.warning } else { primary.warning },
        help: if primary.help.is_empty() { fallback.help } else { primary.help },
        debug: if primary.debug.is_empty() { fallback.debug } else { primary.debug },
        input_arrows: if primary.input_arrows.is_empty() { fallback.input_arrows } else { primary.input_arrows },
        note: if primary.note.is_empty() { fallback.note } else { primary.note },
        output_text: if primary.output_text.is_empty() { fallback.output_text } else { primary.output_text },
        info: if primary.info.is_empty() { fallback.info } else { primary.info },
    }
}

#[cfg(not(feature = "single_executable"))]
fn merge_type_checker_configs(primary: TypeCheckerConfig, fallback: TypeCheckerConfig) -> TypeCheckerConfig {
    TypeCheckerConfig {
        enabled: primary.enabled,
        strict: primary.strict,
        run_unchecked: primary.run_unchecked,
        nested_functions: primary.nested_functions,
        nested_loops: primary.nested_loops,
        warn_on_any: primary.warn_on_any,
        warnings: primary.warnings,
        treat_warnings_as_errors: primary.treat_warnings_as_errors,
        max_errors: primary.max_errors.or(fallback.max_errors),
        ignore_warnings: if primary.ignore_warnings.is_empty() { fallback.ignore_warnings } else { primary.ignore_warnings },
        ignore_errors: if primary.ignore_errors.is_empty() { fallback.ignore_errors } else { primary.ignore_errors },
        ignore_modules: if primary.ignore_modules.is_empty() { fallback.ignore_modules } else { primary.ignore_modules },
        pointer_types: primary.pointer_types,
        experimental_constant_folding: primary.experimental_constant_folding,
        check_imports: primary.check_imports,
        allow_dynamic_casts: primary.allow_dynamic_casts,
        log_level: if primary.log_level.is_empty() { fallback.log_level } else { primary.log_level },
        verbose: primary.verbose,
        track_value_origins: primary.track_value_origins,
        fail_fast: primary.fail_fast,
        max_nested_depth: primary.max_nested_depth.or(fallback.max_nested_depth),
        try_to_auto_fix: primary.try_to_auto_fix || fallback.try_to_auto_fix,
    }
}

fn create_default_config(env_path: &Path) -> Config {
    Config {
        version: VERSION.to_string(),
        moded: false,
        debug: false,
        debug_mode: "normal".to_string(),
        supports_color: true,
        use_lucia_traceback: true,
        warnings: true,
        cache_format: CacheFormat::NoCache,
        allow_fetch: true,
        allow_unsafe: false,
        allow_inline_config: true,
        home_dir: fix_path(env_path.to_str().unwrap_or(".").to_string()),
        libs_paths: vec![env_path.join("libs").to_str().unwrap_or("./libs").to_string()],
        disable_runtime_type_checking: false,
        stack_size: 16777216,
        type_checker: TypeCheckerConfig::default(),
        color_scheme: ColorScheme {
            exception: "#F44350".to_string(),
            warning: "#F5F534".to_string(),
            help: "#21B8DB".to_string(),
            debug: "#434343".to_string(),
            input_arrows: "#136163".to_string(),
            note: "#1CC58B".to_string(),
            output_text: "#BCBEC4".to_string(),
            info: "#9209B3".to_string(),
        },
    }
}

fn create_config_file(path: &Path, env_path: &Path) -> io::Result<()> {
    let default_config = create_default_config(env_path);

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

fn load_project_flags(project_env_path: &Path) -> Vec<String> {
    let flags_path = project_env_path.join("flags.json");
    
    if !flags_path.exists() {
        return vec![];
    }

    let flags_content = match fs::read_to_string(&flags_path) {
        Ok(content) => content,
        Err(_) => {
            eprintln!("Warning: Failed to read flags.json from project environment");
            return vec![];
        }
    };

    let flags_json: serde_json::Value = match serde_json::from_str(&flags_content) {
        Ok(json) => json,
        Err(e) => {
            eprintln!("Warning: Failed to parse flags.json: {}", e);
            return vec![];
        }
    };

    match flags_json {
        serde_json::Value::Array(arr) => {
            arr.into_iter()
                .filter_map(|v| v.as_str().map(|s| s.to_string()))
                .collect()
        }
        serde_json::Value::Object(obj) => {
            obj.into_iter()
                .map(|(k, v)| {
                    if let Some(s) = v.as_str() {
                        format!("--{}={}", k, s)
                    } else if v.is_boolean() && v.as_bool().unwrap_or(false) {
                        format!("--{}", k)
                    } else {
                        format!("--{}={}", k, v.to_string().trim_matches('"'))
                    }
                })
                .collect()
        }
        _ => {
            eprintln!("Warning: flags.json must be an array or object");
            vec![]
        }
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
        let config_res: Result<Config, _> = serde_json::from_str(&config_data);

        match config_res {
            Ok(mut config) => {
                if config.moded {
                    config.home_dir = env_path_str;
                    config.libs_paths = vec![env_path.join("libs").to_str().unwrap_or("./libs").to_string()];
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

fn lucia(
    config: Config,
    non_flag_args: Vec<String>,
    cwd: PathBuf,
    home_dir_path: PathBuf,
    config_path: PathBuf,
    dump_dir: (String, bool),
    dump_pp_flag: bool,
    dump_ast_flag: bool,
    disable_preprocessor: bool,
    argv: Vec<String>,
    timer_flag: bool,
    command_to_run: Option<String>,
    project_env_path: Option<PathBuf>,
    static_check_args: (bool, bool, bool),
    create_lcx: bool,
    cache_dir: PathBuf,
    plugin_runtime: PluginRuntime,
) {
    let dump_dir: (&str, bool) = (dump_dir.0.as_str(), dump_dir.1);

    if let Some(code) = command_to_run {
        let debug_mode_some = if config.debug {
            Some(config.debug_mode.clone())
        } else {
            None
        };
        
        std_env::set_current_dir(&cwd).ok();
        execute_code_string(code, &config, disable_preprocessor, home_dir_path.clone(), config_path.clone(), debug_mode_some, &argv, dump_dir, &dump_pp_flag, &dump_ast_flag, timer_flag, project_env_path.as_ref(), static_check_args, plugin_runtime);
        return;
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
            execute_file(path, file_path.clone(), &config, disable_preprocessor, home_dir_path.clone(), config_path.clone(), debug_mode_some, &argv, dump_dir, &dump_pp_flag, &dump_ast_flag, timer_flag, project_env_path.as_ref(), static_check_args, create_lcx, plugin_runtime.clone());
        }
    } else {
        let debug_mode_some = if config.debug {
            Some(config.debug_mode.clone())
        } else {
            None
        };

        std_env::set_current_dir(&cwd).ok();
        repl(config, disable_preprocessor, home_dir_path, config_path, debug_mode_some, cwd.clone(), &argv, dump_dir, &dump_pp_flag, &dump_ast_flag, timer_flag, project_env_path.as_ref(), static_check_args, cache_dir, plugin_runtime);
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
    dump_ast_flag: &bool,
    timer_flag: bool,
    project_env_path: Option<&PathBuf>,
    static_check_args: (bool, bool, bool),
    create_lcx: bool,
    plugin_runtime: PluginRuntime,
) {
    if path.exists() && path.is_file() {
        debug_log(&format!("Executing file: {}", fix_path(path.display().to_string())), &config);

        let is_lcx = path.extension().and_then(|e| e.to_str()).map(|s| s.eq_ignore_ascii_case("lcx")).unwrap_or(false);
        let effective_disable_preprocessor = disable_preprocessor || is_lcx;
        let effective_static_check_args = if is_lcx { (false, static_check_args.1, static_check_args.2) } else { static_check_args };

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

        let file_content_load_time = Instant::now();
        let mut file_content = match fs::read_to_string(path) {
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
        
        let include_content = load_include_file(project_env_path);
        if !include_content.is_empty() {
            file_content = include_content + &file_content;
        }
        
        if timer_flag {
            println!("{}", format!("{}File content load time: {:?}{}", hex_to_ansi(&config.color_scheme.debug, config.supports_color), file_content_load_time.elapsed(), hex_to_ansi("reset", config.supports_color)));
        }

        let lexering_time = Instant::now();
        let lexer = Lexer::new(&file_content, to_static(file_path.clone()));
        let raw_tokens = lexer.tokenize();
        if timer_flag {
            println!("{}", format!("{}Lexing time: {:?}{}", hex_to_ansi(&config.color_scheme.debug, config.supports_color), lexering_time.elapsed(), hex_to_ansi("reset", config.supports_color)));
        }

        let processed_tokens = if !effective_disable_preprocessor {
            if !config.cache_format.is_enabled() {
                if debug_mode.as_deref() == Some("full") || debug_mode.as_deref() == Some("minimal") {
                    debug_log("Cache disabled, reprocessing tokens", &config);
                }
                let preprocessing_time = Instant::now();
                let mut preprocessor = Preprocessor::new(
                    home_dir_path.join("libs"),
                    file_path.as_str(),
                );
                let res = match preprocessor.process(raw_tokens.clone(), path.parent().unwrap_or(Path::new(""))) {
                    Ok(tokens) => tokens,
                    Err(e) => {
                        handle_error(&e, &file_content, &config);
                        exit(1);
                    }
                };
                if timer_flag {
                    println!("{}", format!("{}Preprocessing time: {:?}{}", hex_to_ansi(&config.color_scheme.debug, config.supports_color), preprocessing_time.elapsed(), hex_to_ansi("reset", config.supports_color)));
                }
                res
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
                    let preprocessing_time = Instant::now();
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
                    if timer_flag {
                        println!("{}", format!("{}Preprocessing time: {:?}{}", hex_to_ansi(&config.color_scheme.debug, config.supports_color), preprocessing_time.elapsed(), hex_to_ansi("reset", config.supports_color)));
                    }
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
            let concrete_tokens = lexer.tokenize_concrete();
            dump_pp(
                processed_tokens.iter().collect(),
                concrete_tokens,
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

        let parsing_time = Instant::now();
        let tokens: Vec<Token> = processed_tokens.clone();
        let mut parser = Parser::new(tokens.clone());

        let statements = match parser.parse_safe() {
            Ok(stmts) => stmts,
            Err(error) => {
                debug_log("Error while parsing:", &config);
                handle_error(&error, &file_content, &config);
                exit(1);
            }
        };
        if timer_flag {
            println!("{}", format!("{}Parsing time: {:?}{}", hex_to_ansi(&config.color_scheme.debug, config.supports_color), parsing_time.elapsed(), hex_to_ansi("reset", config.supports_color)));
        }

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
                        .map(|stmt| stmt.format_for_debug())
                        .collect::<Vec<String>>()
                        .join(", ")
                ),
                &config,
            );
        }

        let (static_check_flag, run_flag, static_check_flag_force_run) = effective_static_check_args;
        let mut can_run = true;
        if static_check_flag {
            let static_check_time = Instant::now();
            let mut checker = Checker::new(config.clone(), file_path.clone());
            let errors = checker.check(statements.clone(), false);
            if !errors.is_empty() {
                for err in &errors {
                    handle_error(err, &file_content, &config);
                }
                if !static_check_flag_force_run {
                    if !run_flag {
                        can_run = false;
                    }
                    if !errors.is_empty() {
                        can_run = false;
                    }
                }
            }
            if timer_flag {
                println!("{}Static check time: {:?}{}", hex_to_ansi(&config.color_scheme.debug, config.supports_color), static_check_time.elapsed(), hex_to_ansi("reset", config.supports_color));
            }
        }
        if !can_run {
            return;
        }

        if create_lcx && !is_lcx {
            let token_refs: Vec<&Token> = processed_tokens.iter().collect();
            let pp_buffer = token_refs
                .iter()
                .map(|t| t.1.as_ref())
                .collect::<Vec<&str>>()
                .join(" ");
            let header = "// -*- lcx: true -*-\n\n";
            let out_content = format!("{}{}", header, pp_buffer.trim_end());
            let out_path = path.with_extension("lcx");
            match File::create(&out_path).and_then(|mut f| f.write_all(out_content.as_bytes())) {
                Ok(_) => {
                    print_colored(
                        &format!("{}Created .lcx file: {}", hex_to_ansi(&config.color_scheme.debug, config.supports_color), out_path.display()),
                        &config.color_scheme.debug,
                        config.supports_color,
                    );
                }
                Err(e) => {
                    print_colored(
                        &format!("{}Failed to write .lcx file {}: {}", hex_to_ansi(&config.color_scheme.exception, config.supports_color), out_path.display(), e),
                        &config.color_scheme.exception,
                        config.supports_color,
                    );
                }
            }
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

        let creating_interpreter_time = Instant::now();
        let mut interpreter = Interpreter::new(
            config.clone(),
            file_path.as_str(),
            &parent_dir,
            (home_dir_path.join("libs"), config_path.clone(), !effective_disable_preprocessor),
            argv,
        );
        interpreter.set_main_thread(true);
        interpreter.set_plugin_runtime(plugin_runtime);

        if config.cache_format.is_enabled() {
            if let Ok(Some(cache)) = load_interpreter_cache(&cache_dir, config.cache_format) {
                interpreter.set_cache(cache);
            }
        }
        if timer_flag {
            println!("{}", format!("{}Creating interpreter time: {:?}{}", hex_to_ansi(&config.color_scheme.debug, config.supports_color), creating_interpreter_time.elapsed(), hex_to_ansi("reset", config.supports_color)));
        }

        let interpreting_time = Instant::now();
        let _out: Value = match interpreter.interpret(statements, true) {
            Ok(out) => {
                if config.cache_format.is_enabled() {
                    if let Err(e) = save_interpreter_cache(&cache_dir, interpreter.get_cache(), config.cache_format) {
                        debug_log(&format!("Failed to save interpreter cache: {}", e), &config);
                    }
                }
                if timer_flag {
                    println!("{}", format!("{}Interpreting time: {:?}{}", hex_to_ansi(&config.color_scheme.debug, config.supports_color), interpreting_time.elapsed(), hex_to_ansi("reset", config.supports_color)));
                }
                out
            }
            Err(error) => {
                debug_log("Error while interpreting:", &config);
                if timer_flag {
                    println!("{}", format!("{}Interpreting time: {:?}{}", hex_to_ansi(&config.color_scheme.debug, config.supports_color), interpreting_time.elapsed(), hex_to_ansi("reset", config.supports_color)));
                }
                handle_error(&error, &file_content, &config);
                exit(1);
            }
        };
    } else {
        eprintln!("Error: File '{}' does not exist or is not a valid file", file_path);
    }
}

fn load_include_file(project_env_path: Option<&PathBuf>) -> String {
    if let Some(env_path) = project_env_path {
        let include_file = env_path.join("include.lc");
        if include_file.exists() {
            match fs::read_to_string(&include_file) {
                Ok(content) => {
                    eprintln!("{}", &format!("Project environment: Loading include.lc from {:?}", fix_path(include_file.display().to_string())).dimmed());
                    return "// include.lc\n\n".to_string() + &content + "\n";
                }
                Err(_) => {}
            }
        }
    }
    String::new()
}

fn execute_code_string(
    code: String,
    config: &Config,
    disable_preprocessor: bool,
    home_dir_path: PathBuf,
    config_path: PathBuf,
    _debug_mode: Option<String>,
    argv: &Vec<String>,
    dump_dir: (&str, bool),
    dump_pp_flag: &bool,
    dump_ast_flag: &bool,
    timer_flag: bool,
    project_env_path: Option<&PathBuf>,
    static_check_args: (bool, bool, bool),
    plugin_runtime: PluginRuntime,
) {
    let cache_dir = home_dir_path.join(".cache");
    if !cache_dir.exists() {
        if let Err(e) = std::fs::create_dir_all(&cache_dir) {
            debug_log(&format!("Failed to create cache directory: {}", e), &config);
        } else if cfg!(windows) {
            let status = Command::new("attrib")
                .args(&["+h", cache_dir.to_str().unwrap()])
                .output();
            if let Err(e) = status {
                debug_log(&format!("Failed to hide cache directory: {}", e), &config);
            }
        }
    }

    let include_content = load_include_file(project_env_path);
    let full_code = include_content.clone() + &code;

    let file_content = full_code.clone();
    
    let lexering_time = Instant::now();
    let lexer = Lexer::new(&full_code, "<-c>");
    let raw_tokens = lexer.tokenize();
    if timer_flag {
        println!("{}", format!("{}Lexing time: {:?}{}", hex_to_ansi(&config.color_scheme.debug, config.supports_color), lexering_time.elapsed(), hex_to_ansi("reset", config.supports_color)));
    }

    let processed_tokens = if !disable_preprocessor {
        let preprocessing_time = Instant::now();
        let mut preprocessor = Preprocessor::new(
            home_dir_path.join("libs"),
            "<-c>",
        );
        let current_dir = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
        let res = match preprocessor.process(raw_tokens.clone(), &current_dir) {
            Ok(tokens) => tokens,
            Err(e) => {
                handle_error(&e, &file_content, &config);
                exit(1);
            }
        };
        if timer_flag {
            println!("{}", format!("{}Preprocessing time: {:?}{}", hex_to_ansi(&config.color_scheme.debug, config.supports_color), preprocessing_time.elapsed(), hex_to_ansi("reset", config.supports_color)));
        }
        res
    } else {
        raw_tokens
    };

    let parsing_time = Instant::now();
    let tokens: Vec<Token> = processed_tokens.clone();
    let mut parser = Parser::new(tokens.clone());

    let statements = match parser.parse_safe() {
        Ok(stmts) => stmts,
        Err(error) => {
            handle_error(&error, &file_content, &config);
            exit(1);
        }
    };
    if timer_flag {
        println!("{}", format!("{}Parsing time: {:?}{}", hex_to_ansi(&config.color_scheme.debug, config.supports_color), parsing_time.elapsed(), hex_to_ansi("reset", config.supports_color)));
    }

    if *dump_pp_flag && !processed_tokens.is_empty() {
        let p = if dump_dir.1 {
            dump_dir.0.to_string()
        } else {
            ".".to_string()
        };
        let concrete_tokens = lexer.tokenize_concrete();
        dump_pp(
            processed_tokens.iter().collect(),
            concrete_tokens,
            &p,
            "<-c>",
            &config,
        );
    }

    if *dump_ast_flag && dump_dir.1 {
        let p = if dump_dir.1 {
            dump_dir.0.to_string()
        } else {
            ".".to_string()
        };
        dump_ast(
            statements.iter().collect(),
            &p,
            "<-c>",
            &config,
        );
    }

    let (static_check_flag, run_flag, static_check_flag_force_run) = static_check_args;
    let mut can_run = true;
    if static_check_flag {
        let static_check_time = Instant::now();
        let mut checker = Checker::new(config.clone(), "<-c>".to_string());
        let errors = checker.check(statements.clone(), true);
        if !errors.is_empty() {
            for err in &errors {
                handle_error(err, &file_content, &config);
            }
            if !static_check_flag_force_run {
                if !run_flag {
                    can_run = false;
                }
                if !errors.is_empty() {
                    can_run = false;
                }
            }
        }
        if timer_flag {
            println!("{}Static check time: {:?}{}", hex_to_ansi(&config.color_scheme.debug, config.supports_color), static_check_time.elapsed(), hex_to_ansi("reset", config.supports_color));
        }
    }
    if !can_run {
        return;
    }

    let current_dir = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
    let mut interpreter = Interpreter::new(
        config.clone(),
        "<-c>",
        &current_dir,
        (
            home_dir_path.join("libs"),
            config_path.clone(),
            !disable_preprocessor,
        ),
        argv,
    );
    interpreter.set_main_thread(true);
    interpreter.set_plugin_runtime(plugin_runtime);

    let interpreter_time_start = Instant::now();
    let result = interpreter.interpret(statements, false);

    if timer_flag {
        println!("{}", format!("{}Interpreter time: {:?}{}", hex_to_ansi(&config.color_scheme.debug, config.supports_color), interpreter_time_start.elapsed(), hex_to_ansi("reset", config.supports_color)));
    }

    match result {
        Ok(value) => {
            if !matches!(value, Value::Null) {
                println!("{}", format_value(&value));
            }
        }
        Err(error) => {
            handle_error(&error, &file_content, &config);
            exit(1);
        }
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
    timer_flag: bool,
    project_env_path: Option<&PathBuf>,
    static_check_args: (bool, bool, bool),
    cache_dir: PathBuf,
    plugin_runtime: PluginRuntime,
) {
    println!(
        "{}Lucia-{} REPL\nType 'exit()' to exit or 'help()' for help.{}",
        hex_to_ansi(&config.color_scheme.info, config.supports_color),
        config.version,
        hex_to_ansi("reset", config.supports_color)
    );

    let mut interpreter = Interpreter::new(
        config.clone(),
        "<stdin>",
        &cwd,
        (
            home_dir_path.join("libs"),
            config_path.clone(),
            !disable_preprocessor,
        ),
        argv,
    );
    interpreter.set_main_thread(true);
    interpreter.set_plugin_runtime(plugin_runtime);

    if config.cache_format.is_enabled() {
        if let Ok(Some(cache)) = load_interpreter_cache(&cache_dir, config.cache_format) {
            interpreter.set_cache(cache);
        }
    }

    let include_content = load_include_file(project_env_path);
    if !include_content.is_empty() {
        let raw_tokens = Lexer::new(&include_content, "<include.lc>").tokenize();

        let mut preprocessor_for_include = Preprocessor::new(
            home_dir_path.join("libs"),
            "<include.lc>",
        );

        let current_dir = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
        let processed_tokens = if !disable_preprocessor {
            match preprocessor_for_include.process(raw_tokens.clone(), &current_dir) {
                Ok(toks) => toks,
                Err(e) => {
                    handle_error(&e, &include_content, &config);
                    return;
                }
            }
        } else {
            raw_tokens
        };

        let mut parser = Parser::new(processed_tokens);
        let statements = parser.parse_safe().unwrap_or_else(|error| {
            handle_error(&error, &include_content, &config);
            vec![]
        });

        if let Err(error) = interpreter.interpret(statements, false) {
            handle_error(&error, &include_content, &config);
            return;
        }
    }

    let mut preprocessor = Preprocessor::new(
        home_dir_path.join("libs"),
        "<stdin>",
    );

    let mut static_checker = if static_check_args.0 {
        Some(Checker::new(config.clone(), "<stdin>".to_string()))
    } else {
        None
    };

    let print_start_debug = matches!(debug_mode.as_deref(), Some("full" | "minimal"));
    let print_intime_debug = matches!(debug_mode.as_deref(), Some("full" | "normal"));

    let mut line_number = 0;

    let stop_flag = Arc::new(AtomicBool::new(false));

    let prompt = format!(
        "{}{}{} ",
        hex_to_ansi(&config.color_scheme.input_arrows, config.supports_color),
        ">>>",
        hex_to_ansi("reset", config.supports_color)
    );

    let multiline_prompt = format!(
        "{}{}{} ",
        hex_to_ansi(&config.color_scheme.debug, config.supports_color),
        "...",
        hex_to_ansi("reset", config.supports_color)
    );

    let mut history: Vec<String> = Vec::new();
    let mut completions: Vec<String>;
    #[cfg(not(feature = "single_executable"))]
    let history_path = {
        let primary = cache_dir.join("repl.history");

        match std::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(&primary)
        {
            Ok(_) => primary,
            Err(e) if e.kind() == io::ErrorKind::NotFound => primary,
            Err(e) if e.kind() == io::ErrorKind::PermissionDenied => {
                #[cfg(windows)]
                {
                    let appdata = std::env::var("APPDATA")
                        .expect("APPDATA not set");

                    let fallback = PathBuf::from(appdata)
                        .join("Lucia")
                        .join("repl.history");

                    std::fs::create_dir_all(fallback.parent().unwrap())
                        .expect("failed to create Lucia dir");

                    fallback
                }

                #[cfg(not(windows))]
                {
                    let fallback = PathBuf::from("/tmp").join("lucia_repl.history");
                    fallback
                }
            }
            Err(e) => {
                eprintln!("{}Warning: Failed to open REPL history file: {}{}", hex_to_ansi(&config.color_scheme.warning, config.supports_color), e, hex_to_ansi("reset", config.supports_color));
                PathBuf::from("/dev/null")
            }
        }
    };
    #[cfg(feature = "single_executable")]
    let history_path = std::env::temp_dir().join("lucia_repl.history");
    
    loop {
        line_number += 1;

        completions = interpreter.get_repl_completions().into_iter().chain(preprocessor.get_repl_completions().into_iter()).collect::<Vec<_>>();
        completions.sort();
        completions.dedup();

        let mut input = read_input(&prompt, &multiline_prompt, &mut history, &completions, &history_path);
        if input.is_empty() {
            continue;
        }
        
        
        if input.starts_with(":load") {
            let parts: Vec<&str> = input.split_whitespace().collect();
            if parts.len() < 2 {
                println!("Usage: :load <file_path>");
                continue;
            }
            let file_path = parts[1].trim().trim_end_matches('"').trim_start_matches('"');
            let path = Path::new(file_path);
            if path.exists() && path.is_file() {
                let file_content = match fs::read_to_string(path) {
                    Ok(content) => content,
                    Err(e) => {
                        handle_error(&Error::with_help(
                            "FileReadError",
                            to_static(format!("Failed to read file '{}': {}", path.display(), e)),
                            "Check if the file exists and is readable.",
                            to_static(file_path.to_string()),
                        ), "", &config);
                        continue;
                    }
                };
                input = include_content.clone() + &file_content;
            } else {
                println!("Error: File '{}' does not exist or is not a valid file", file_path);
                continue;
            }
        }

        if input.starts_with(':') {
            let new_input = match &*input {
                ":exit" | ":quit" | ":q" => Some("exit()".into()),
                ":q!" | ":Q" => std::process::exit(0),
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
                    println!(
                        "{}Tip: Use ':macro-help' to see REPL macros.{}",
                        hex_to_ansi(&config.color_scheme.help, config.supports_color),
                        hex_to_ansi("reset", config.supports_color)
                    );
                    Some("help()".into())
                }
                ":clear-history" | ":clearhistory" | ":ch" => {
                    history.clear();
                    if let Err(e) = fs::remove_file(&history_path) {
                        handle_error(
                            &Error::new("IOError", &format!("Failed to clear history: {}", e), "<stdin>"),
                            &input,
                            &config,
                        );
                    } else {
                        println!("{}", "REPL history cleared.".bold().green());
                    }
                    None
                }
                ":macro-help" | "::" => {
                    let macros = vec![
                        (":exit", "Exit the REPL"),
                        (":clear / :cls", "Clear the terminal screen"),
                        (":load <file_path>", "Load and execute a file within the REPL"),
                        (":help / :?", "Show general help"),
                        (":macro-help / ::", "Show this help message for REPL macros"),
                        (":clear-history / :ch", "Clear the REPL command history"),
                        (":traceback / :t", "Show the last error traceback information"),
                        (":about / :a / :!", "Show information about Lucia REPL"),
                        (":warranty / :w", "Show warranty disclaimer"),
                        (":conditions / :c", "Show conditions for copying and modifying the software"),
                        (":license / :l", "Show the software license (GPL-3.0)"),
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
                ":trace" | ":traceback" | ":t" => {
                    println!("{}", "Traceback:".bold().underline().cyan());
                    if interpreter.get_traceback().is_empty() {
                        println!(" - No traceback available.");
                    }
                    for (key, value) in interpreter.get_traceback() {
                        println!(" - {}: {}", key.cyan(), value);
                    }
                    None
                }
                ":about" | ":a" | ":!" => {
                    println!(
                        "{}Lucia REPL - Version {}\nA REPL for the Lucia programming language.{}\n\nCopyright (C) 2025  Leonard Markovič (Markofwitch)\nThis program comes with {}ABSOLUTELY NO WARRANTY{}; for details type `:warranty'.\nThis is free software, and you are welcome to redistribute it\nunder certain conditions; type `:conditions' for details.",
                        hex_to_ansi(&config.color_scheme.info, config.supports_color),
                        config.version,
                        hex_to_ansi("reset", config.supports_color),
                        hex_to_ansi("italic", config.supports_color),
                        hex_to_ansi("reset", config.supports_color),
                    );
                    None
                }
                ":warranty" | ":w" => {
                    println!("  Disclaimer of Warranty.

  THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY
APPLICABLE LAW.  EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT
HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM \"AS IS\" WITHOUT WARRANTY
OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO,
THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
PURPOSE.  THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM
IS WITH YOU.  SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF
ALL NECESSARY SERVICING, REPAIR OR CORRECTION.

  Limitation of Liability.

  IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING
WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MODIFIES AND/OR CONVEYS
THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES, INCLUDING ANY
GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE
USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED TO LOSS OF
DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD
PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS),
EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF
SUCH DAMAGES.");
                    None
                }
                ":conditions" | ":c" => {
                    println!("  Conveying Verbatim Copies.

  You may convey verbatim copies of the Program's source code as you
receive it, in any medium, provided that you conspicuously and
appropriately publish on each copy an appropriate copyright notice;
keep intact all notices stating that this License and any
non-permissive terms added in accord with section 7 apply to the code;
keep intact all notices of the absence of any warranty; and give all
recipients a copy of this License along with the Program.

  You may charge any price or no price for each copy that you convey,
and you may offer support or warranty protection for a fee.

  Conveying Modified Source Versions.

  You may convey a work based on the Program, or the modifications to
produce it from the Program, in the form of source code under the
terms of section 4, provided that you also meet all of these conditions:

    a) The work must carry prominent notices stating that you modified
    it, and giving a relevant date.

    b) The work must carry prominent notices stating that it is
    released under this License and any conditions added under section
    7.  This requirement modifies the requirement in section 4 to
    \"keep intact all notices\".

    c) You must license the entire work, as a whole, under this
    License to anyone who comes into possession of a copy.  This
    License will therefore apply, along with any applicable section 7
    additional terms, to the whole of the work, and all its parts,
    regardless of how they are packaged.  This License gives no
    permission to license the work in any other way, but it does not
    invalidate such permission if you have separately received it.

    d) If the work has interactive user interfaces, each must display
    Appropriate Legal Notices; however, if the Program has interactive
    interfaces that do not display Appropriate Legal Notices, your
    work need not make them do so.

  A compilation of a covered work with other separate and independent
works, which are not by their nature extensions of the covered work,
and which are not combined with it such as to form a larger program,
in or on a volume of a storage or distribution medium, is called an
\"aggregate\" if the compilation and its resulting copyright are not
used to limit the access or legal rights of the compilation's users
beyond what the individual works permit.  Inclusion of a covered work
in an aggregate does not cause this License to apply to the other
parts of the aggregate.

  Conveying Non-Source Forms.

  You may convey a covered work in object code form under the terms
of sections 4 and 5, provided that you also convey the
machine-readable Corresponding Source under the terms of this License,
in one of these ways:

    a) Convey the object code in, or embodied in, a physical product
    (including a physical distribution medium), accompanied by the
    Corresponding Source fixed on a durable physical medium
    customarily used for software interchange.

    b) Convey the object code in, or embodied in, a physical product
    (including a physical distribution medium), accompanied by a
    written offer, valid for at least three years and valid for as
    long as you offer spare parts or customer support for that product
    model, to give anyone who possesses the object code either (1) a
    copy of the Corresponding Source for all the software in the
    product that is covered by this License, on a durable physical
    medium customarily used for software interchange, for a price no
    more than your reasonable cost of physically performing this
    conveying of source, or (2) access to copy the
    Corresponding Source from a network server at no charge.

    c) Convey individual copies of the object code with a copy of the
    written offer to provide the Corresponding Source.  This
    alternative is allowed only occasionally and noncommercially, and
    only if you received the object code with such an offer, in accord
    with subsection 6b.

    d) Convey the object code by offering access from a designated
    place (gratis or for a charge), and offer equivalent access to the
    Corresponding Source in the same way through the same place at no
    further charge.  You need not require recipients to copy the
    Corresponding Source along with the object code.  If the place to
    copy the object code is a network server, the Corresponding Source
    may be on a different server (operated by you or a third party)
    that supports equivalent copying facilities, provided you maintain
    clear directions next to the object code saying where to find the
    Corresponding Source.  Regardless of what server hosts the
    Corresponding Source, you remain obligated to ensure that it is
    available for as long as needed to satisfy these requirements.

    e) Convey the object code using peer-to-peer transmission, provided
    you inform other peers where the object code and Corresponding
    Source of the work are being offered to the general public at no
    charge under subsection 6d.

  A separable portion of the object code, whose source code is excluded
from the Corresponding Source as a System Library, need not be
included in conveying the object code work.

  A \"User Product\" is either (1) a \"consumer product\", which means any
tangible personal property which is normally used for personal, family,
or household purposes, or (2) anything designed or sold for incorporation
into a dwelling.  In determining whether a product is a consumer product,
doubtful cases shall be resolved in favor of coverage.  For a particular
product received by a particular user, \"normally used\" refers to a
typical or common use of that class of product, regardless of the status
of the particular user or of the way in which the particular user
actually uses, or expects or is expected to use, the product.  A product
is a consumer product regardless of whether the product has substantial
commercial, industrial or non-consumer uses, unless such uses represent
the only significant mode of use of the product.

  \"Installation Information\" for a User Product means any methods,
procedures, authorization keys, or other information required to install
and execute modified versions of a covered work in that User Product from
a modified version of its Corresponding Source.  The information must
suffice to ensure that the continued functioning of the modified object
code is in no case prevented or interfered with solely because
modification has been made.

  If you convey an object code work under this section in, or with, or
specifically for use in, a User Product, and the conveying occurs as
part of a transaction in which the right of possession and use of the
User Product is transferred to the recipient in perpetuity or for a
fixed term (regardless of how the transaction is characterized), the
Corresponding Source conveyed under this section must be accompanied
by the Installation Information.  But this requirement does not apply
if neither you nor any third party retains the ability to install
modified object code on the User Product (for example, the work has
been installed in ROM).

  The requirement to provide Installation Information does not include a
requirement to continue to provide support service, warranty, or updates
for a work that has been modified or installed by the recipient, or for
the User Product in which it has been modified or installed.  Access to a
network may be denied when the modification itself materially and
adversely affects the operation of the network or violates the rules and
protocols for communication across the network.

  Corresponding Source conveyed, and Installation Information provided,
in accord with this section must be in a format that is publicly
documented (and with an implementation available to the public in
source code form), and must require no special password or key for
unpacking, reading or copying.
");
                    None
                }
                ":l" | ":license" => {
                    println!("Lucia is licensed under the GNU General Public License v3.0 (GPL-3.0). For more details, see https://www.gnu.org/licenses/gpl-3.0.html");
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

        let lexering_time = Instant::now();
        let lexer = Lexer::new(&input, "<stdin>".into());

        let raw_tokens = lexer.tokenize();

        if timer_flag {
            println!("{}", format!("{}Lexing time: {:?}{}", hex_to_ansi(&config.color_scheme.debug, config.supports_color), lexering_time.elapsed(), hex_to_ansi("reset", config.supports_color)));
        }

        let current_dir = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));

        let preprocessing_time = Instant::now();
        let processed_tokens = if !disable_preprocessor {
            match preprocessor.process(raw_tokens.clone(), &current_dir) {
                Ok(toks) => toks,
                Err(e) => {
                    if print_start_debug {
                        let filtered = raw_tokens
                            .iter()
                            .filter(|token| {
                                let t = &token.0;
                                t != "WHITESPACE" && !t.starts_with("COMMENT_") && t != "EOF"
                            })
                            .map(|token| (&token.0, &token.1))
                            .collect::<Vec<_>>();
                    
                        debug_log(&format!("Tokens: {:?}", filtered), &config);
                    }
                    handle_error(&e.clone(), &input, &config);
                    continue;
                }
            }
        } else {
            raw_tokens
        };

        if timer_flag {
            println!("{}", format!("{}Preprocessing time: {:?}{}", hex_to_ansi(&config.color_scheme.debug, config.supports_color), preprocessing_time.elapsed(), hex_to_ansi("reset", config.supports_color)));
        }

        if *dump_pp_flag && !processed_tokens.is_empty() {
            let file_path = cwd.join(format!("stdin-{}", line_number));
            let path = if dump_dir.1 {
                dump_dir.0
            } else {
                file_path.parent().unwrap_or_else(|| Path::new(".")).to_str().unwrap_or("")
            };
            let concrete_tokens = lexer.tokenize_concrete();
            dump_pp(
                processed_tokens.iter().collect(),
                concrete_tokens,
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

        let parsing_time = Instant::now();
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

        if timer_flag {
            println!("{}", format!("{}Parsing time: {:?}{}", hex_to_ansi(&config.color_scheme.debug, config.supports_color), parsing_time.elapsed(), hex_to_ansi("reset", config.supports_color)));
        }

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
                        .map(|stmt| stmt.format_for_debug())
                        .collect::<Vec<String>>()
                        .join(", ")
                ),
                &config,
            );
        }

        let mut can_run = true;
        if let Some(checker) = static_checker.as_mut() {
            let static_check_time = Instant::now();
            let errors = checker.check(statements.clone(), true);
            if !errors.is_empty() {
                if print_intime_debug {
                    debug_log("Error while static checking:", &config);
                }
                for err in &errors {
                    handle_error(err, &input, &config);
                }
                let (_, run_flag, static_check_flag_force_run) = static_check_args;
                if static_check_flag_force_run {
                } else if !run_flag || !errors.is_empty() {
                    can_run = false;
                }
            }
            if timer_flag {
                println!("{}Static check time: {:?}{}", hex_to_ansi(&config.color_scheme.debug, config.supports_color), static_check_time.elapsed(), hex_to_ansi("reset", config.supports_color));
            }
        }
        if !can_run {
            continue;
        }

        let creating_interpreter_time = Instant::now();
        stop_flag.store(false, Ordering::Relaxed);

        let stop_flag_clone = stop_flag.clone();

        interpreter.stop_flag = Some(stop_flag_clone);

        let mut interpreter_clone = interpreter.clone();
        let statements_clone = statements.clone();
        let loc = statements.iter().rev().find_map(|stmt| {
            let found = matches!(stmt.node, Node::For { .. } | Node::While { .. } | Node::Call { .. });
            if found {
                stmt.loc.clone()
            } else {
                None
            }
        }).or_else(|| {
            statements.iter().rev().find_map(|stmt| {
                stmt.loc.clone()
            })
        });

        let builder = thread::Builder::new()
            .name(format!("Lucia-{} REPL", VERSION))
            .stack_size(config.stack_size);

        if timer_flag {
            println!("{}", format!("{}Creating interpreter time: {:?}{}", hex_to_ansi(&config.color_scheme.debug, config.supports_color), creating_interpreter_time.elapsed(), hex_to_ansi("reset", config.supports_color)));
        }

        let interpreting_time = Instant::now();

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
                    Some(loc) => Error::with_location("KeyboardInterrupt", "Execution interrupted by user (Ctrl+T)", get_loc(loc)),
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
                if timer_flag {
                    println!("{}", format!("{}Interpreting time: {:?}{}", hex_to_ansi(&config.color_scheme.debug, config.supports_color), interpreting_time.elapsed(), hex_to_ansi("reset", config.supports_color)));
                }
                out
            }
            Err(error) => {
                if print_intime_debug {
                    debug_log("Error while interpreting:", &config);
                }
                if timer_flag {
                    println!("{}", format!("{}Interpreting time: {:?}{}", hex_to_ansi(&config.color_scheme.debug, config.supports_color), interpreting_time.elapsed(), hex_to_ansi("reset", config.supports_color)));
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

fn bundle(
    main_file: &str,
    output_path: &str,
    files: Vec<String>,
    cwd: PathBuf,
    config_path: PathBuf,
    run_flag: bool,
    std_libs: (bool, Vec<String>),
    args: Vec<String>,
    quiet_flag: bool,
    command_to_run: Option<String>,
    default_argv: Vec<String>,
    opt_level: String,
    hide_console: bool,
    default_quiet: bool
) -> Result<String, (i32, Vec<Error>)> {
    std::env::set_current_dir(&cwd).map_err(|e| (1, vec![Error::new("IOError", &format!("Failed to set current directory: {}", e), "<bundle>")]))?;
    let mut libraries = std_libs.1;
    if std_libs.0 {
        let libs = crate::env::runtime::libs::_STD_LIBS
            .iter()
            .map(|(k, _)| k.to_string())
            .collect::<Vec<String>>();
        libraries.extend(libs);
    }
    
    let res = bundle_to_exe(
        &config_path.to_str().unwrap_or("config.json"),
        &output_path,
        main_file,
        files.iter().map(|f| f.to_string()).collect(),
        libraries,
        args.iter().map(|a| a.to_string()).collect(),
        command_to_run,
        run_flag,
        &opt_level,
        hide_console,
        quiet_flag,
        default_quiet,
        default_argv,
    );
    return res;
}

fn main() {
    let vec_args: Vec<String> = std_env::args().filter(|s| s != "lucia").collect();

    panic::set_hook(Box::new(|panic_info| {
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
        eprintln!("");
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

        eprintln!("{}", "Press any key to continue...".yellow());
        let _ = std::io::stdout().flush();
        wait_any_key();
    println!();
    
        exit(101);
    }));

    if std_env::var_os("LUCIA_DEBUG_VALUE_SIZE").is_some() {
        Value::debug_value_size();
    }

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

    #[allow(unused_mut)]
    let mut config_path = exe_path
        .parent()
        .map(|p| p.join("..").join("config.json"))
        .ok_or_else(|| {
            eprintln!("Failed to resolve parent of executable path.");
            exit(1);
        })
        .unwrap();
    
    #[cfg(not(feature = "single_executable"))]
    let mut libs_path = exe_path
        .parent()
        .map(|p| p.join("..").join("libs.json"))
        .ok_or_else(|| {
            eprintln!("Failed to resolve parent of executable path.");
            exit(1);
        })
        .unwrap();
    
    let commands = [
        ("--activate, -a", "Activate the environment"),
        ("--no-color", "Disable colored output"),
        ("--color", "Enable colored output (default)"),
        ("--quiet, -q", "Suppress debug and warning messages"),
        ("--debug, -d", "Enable debug mode"),
        ("--debug-mode=<mode>", "Set debug mode (full, normal, minimal, none)"),
        ("--exit, -e", "Exit if no files are provided"),
        ("--info, -i", "Show build and environment information"),
        ("--help, -h", "Show this help message"),
        ("--version, -v", "Show version information"),
        ("--build-info", "Show build information"),
        ("--disable-preprocessor, -dp", "Disable preprocessor"),
        ("--fmt <path>", "Format source code file"),
        ("--config <path>", "Specify a custom config file path"),
        ("--default-config, -dc", "Reset to default configuration"),
        ("--libs-dir <dir>, -L <dir>", "Specify a custom libraries directory path (can be multiple). Relative to 'home_dir'"),
        ("--dump-pp", "Dump source code after preprocessing"),
        ("--dump-ast", "Dump AST after parsing"),
        ("--dump", "Dump both source code and AST (equivalent to --dump-pp and --dump-ast)"),
        ("--allow-unsafe", "Allow unsafe operations"),
        ("--stack-size <size>", "Set the stack size for the interpreter, default: 8388608 (8MB)"),
        ("--run, -r", "Run the source code after compiling"),
        ("--cache <format>", "Enable or disable caching (default: 'no_cache') (check config-guide.md)"),
        ("--clean-cache, -cc", "Clear the cache directory"),
        ("--c-compiler <compiler>", "Specify the C compiler to use for compilation (default: gcc)"),
        ("--argv <args>", "Pass additional arguments to the interpreter as a JSON array"),
        ("--timer, -t", "Enable timing information"),
        ("--dump-dir <path>", "Specify the directory to dump preprocessed and AST files (default: current directory)"),
        ("--bundle, -b", "Bundle the script and its dependencies into a single executable (Windows only)"),
        ("--no-project-env", "Disable project environment (.lucia/) detection"),
        ("--code <code>, -c <code>", "Execute code directly from command line"),
        ("--check, -u", "Perform static code analysis without executing"),
        ("--check --run, -w", "Perform static code analysis and run if no issues are found"),
        ("-wf", "Perform static code analysis and always run, even if issues are found"),
        ("-wa", "Perform static code analysis and run, try to fix issues automatically"),
        ("-x", "Execute the source file and convert it to .lcx format"),
        ("-- <argv>", "Arguments to pass to the executed script, anything after '--' is considered as script arguments"),
        ("<file>", "The source file to execute"),
    ];

    let mut activate_flag = false;
    let mut no_color_flag = false;
    let mut quiet_flag = false;
    let mut debug_flag = false;
    let mut exit_flag = false;
    let mut help_flag = false;
    let mut version_flag = false;
    let mut disable_preprocessor = false;
    let mut fmt_files: Vec<String> = Vec::new();
    let mut config_arg: Option<String> = None;
    let mut default_config_flag = false;
    let mut libs_dirs: Vec<String> = Vec::new();
    let mut dump_pp_flag = false;
    let mut dump_ast_flag = false;
    let mut create_lcx = false;
    let mut allow_unsafe = false;
    let mut debug_mode_flag = false;
    let mut debug_mode_value: Option<String> = None;
    let mut stack_size: (usize, bool) = (16_777_216, false);
    let mut dump_dir: (String, bool) = (".".to_string(), false);
    let mut cache: (CacheFormat, bool) = (CacheFormat::NoCache, false);
    let mut cls_cache_flag = false;
    let mut argv_arg: Option<String> = None;
    let mut timer_flag = false;
    let mut _no_project_env_flag = false;
    let mut static_check_flag = false;
    let mut static_check_flag_force_run = false;
    let mut static_check_flag_auto_fix = false;
    let mut run_flag = false;
    let mut command_to_run: Option<String> = None;

    let project_env_path = cwd.join(".lucia");
    let use_project_env = !vec_args.contains(&"--no-project-env".to_string()) && project_env_path.exists() && project_env_path.is_dir();
    
    let mut additional_args = Vec::new();
    if use_project_env {
        if !vec_args.contains(&"--quiet".to_string()) && !vec_args.contains(&"-q".to_string()) {
            eprintln!("{}", &format!("Project environment: Loading environment from '{}'", fix_path(project_env_path.display().to_string())).dimmed());
        }
        additional_args = load_project_flags(&project_env_path);
    }

    let mut left_args: Vec<String> = Vec::new();
    let mut right_argv: Vec<String> = Vec::new();
    let mut seen_sep = false;
    for a in vec_args.iter() {
        if a == "--" {
            seen_sep = true;
            continue;
        }
        if seen_sep {
            right_argv.push(a.clone());
        } else {
            left_args.push(a.clone());
        }
    }

    if let Ok(current_exe) = std_env::current_exe().and_then(|p| p.canonicalize()) {
        left_args.retain(|tok| {
            if tok.starts_with('-') {
                return true;
            }

            if let Ok(tok_path) = PathBuf::from(tok).canonicalize() {
                if tok_path == current_exe {
                    return false;
                }
            }

            if let Some(fname) = current_exe.file_name().and_then(|s| s.to_str()) {
                if tok == fname {
                    return false;
                }
            }

            true
        });
    }

    let mut dash_file: Option<String> = None;
    if let Some(pos) = left_args.iter().position(|s| s == "-") {
        if pos + 1 >= left_args.len() {
            eprintln!("'-' requires a file argument after it");
            exit(1);
        }
        dash_file = Some(left_args[pos + 1].clone());
        let mut new_right: Vec<String> = Vec::new();
        for j in (pos + 2)..left_args.len() {
            new_right.push(left_args[j].clone());
        }
        new_right.extend(right_argv.into_iter());
        right_argv = new_right;
        left_args.truncate(pos);
    }

    let mut normalized_additional_args: Vec<String> = Vec::new();
    for a in additional_args.iter() {
        if let Some(eq) = a.find('=') {
            normalized_additional_args.push(a[..eq].to_string());
            normalized_additional_args.push(a[eq + 1..].to_string());
        } else {
            normalized_additional_args.push(a.clone());
        }
    }

    let mut all_args = left_args.clone();
    all_args.extend(normalized_additional_args.clone());
    let args: HashSet<String> = all_args.into_iter().collect();

    #[cfg(not(feature = "single_executable"))]
    let has_explicit_config = vec_args.iter().any(|arg| arg == "--config" || arg.starts_with("--config="));
    #[cfg(not(feature = "single_executable"))]
    let (primary_config_path, primary_libs_path) = if use_project_env && !has_explicit_config {
        (project_env_path.join("config.json"), project_env_path.join("libs.json"))
    } else {
        (config_path.clone(), libs_path.clone())
    };

    #[cfg(not(feature = "single_executable"))]
    let fallback_config_path = config_path.clone();
    #[cfg(not(feature = "single_executable"))]
    let fallback_libs_path = libs_path.clone();

    #[cfg(not(feature = "single_executable"))] {
        config_path = primary_config_path;
        libs_path = primary_libs_path;
    }

    if args.contains("--bundle") || args.contains("-b") {
        let args: Vec<String> = vec_args.clone();
        let mut main_file = String::new();
        let mut output_path = String::new();
        let mut files = Vec::new();
        let mut default_argv: Vec<String> = Vec::new();
        let mut std_libs = (true, Vec::new());
        let mut opt_level = "0".to_string();
        let mut hide_console = false;
        let mut default_quiet = true;
        let mut i = 1;
        while i < args.len() {
            match args[i].as_str() {
                "--output" | "-o" => {
                    if i + 1 < args.len() {
                        output_path = args[i + 1].clone();
                        i += 1;
                    } else {
                        eprintln!("Error: --output requires a value.");
                        exit(1);
                    }
                }
                "-f" => {
                    if i + 1 < args.len() {
                        files.push(args[i + 1].clone());
                        i += 1;
                    } else {
                        eprintln!("Error: -f requires a value.");
                        exit(1);
                    }
                }
                "--argv" => {
                    if i + 1 < args.len() {
                        let argv_str = args[i + 1].clone();
                        match serde_json::from_str::<Vec<String>>(&argv_str) {
                            Ok(vec) => {
                                default_argv = vec;
                            }
                            Err(e) => {
                                eprintln!("Error: Failed to parse --argv value as JSON array: {}", e);
                                exit(1);
                            }
                        }
                        i += 1;
                    } else {
                        eprintln!("Error: --argv requires a value.");
                        exit(1);
                    }
                }
                "--" => {
                    i += 1;
                    while i < args.len() {
                        default_argv.push(args[i].clone());
                        i += 1;
                    }
                    break;
                }
                "--bundle" | "-b" => {}
                "--quiet" | "-q" => quiet_flag = true,
                "--std-libs" => std_libs.0 = true,
                "--no-std-libs" | "-ns" | "--dont-include-std-libs" | "-di" => std_libs.0 = false,
                "--run" | "-r" => run_flag = true,
                "--hide-console" | "-hc" => hide_console = true,
                "--no-quiet" | "-nq" => default_quiet = false,
                "--lib" | "-l" => {
                    if i + 1 < args.len() {
                        let lib = args[i + 1].clone();
                        std_libs.1.push(lib);
                        i += 1;
                    } else {
                        eprintln!("Error: --lib requires a value.");
                        exit(1);
                    }
                }
                arg if arg.starts_with("--std-lib=") => {
                    let lib = arg["--std-lib=".len()..].to_string();
                    std_libs.1.push(lib);
                }
                arg if arg.starts_with("-O") => {
                    opt_level = arg[2..].to_string();
                    if !["0", "1", "2", "3", "s", "S", "z", "Z"].contains(&opt_level.as_str()) {
                        eprintln!("Error: Invalid optimization level '{}'. Use 0, 1, 2, 3, s, S, z, or Z.", opt_level);
                        exit(1);
                    }
                }
                _ => {
                    if main_file.is_empty() && !(args[i].starts_with('-')) {
                        main_file = args[i].clone();
                        if !(PathBuf::from(&main_file).exists() && PathBuf::from(&main_file).is_file()) {
                            eprintln!("Error: Main file '{}' does not exist.", main_file);
                            exit(1);
                        }
                    } else {
                        eprintln!("Error: Unexpected argument '{}'.", args[i]);
                        exit(1);
                    }
                }
            }
            i += 1;
        }
        if main_file.is_empty() {
            eprintln!("Error: Main file is required for bundling.");
            exit(1);
        }
        if output_path.is_empty() {
            output_path = format!("{}{}", PathBuf::from(&main_file).file_stem().unwrap_or_default().to_str().unwrap_or_else(|| {
                eprintln!("Error: Invalid main file name '{}'.", main_file);
                exit(1);
            }), if cfg!(target_os = "windows") { ".exe" } else { "" });
        }
        // if cfg!(not(target_os = "windows")) {
        //     eprintln!("Error: Bundling is only supported on Windows.");
        //     exit(1);
        // }
        match bundle(
            &main_file,
            &output_path,
            files,
            cwd.clone(),
            config_path.clone(),
            run_flag,
            std_libs,
            args,
            quiet_flag,
            command_to_run,
            default_argv,
            opt_level,
            hide_console,
            default_quiet,
        ) {
            Ok(msg) => {
                println!("{}", msg);
            }
            Err((code, errors)) => {
                for error in errors {
                    handle_error(&error, &main_file, &Config::default());
                }
                exit(code);
            }
        }
        exit(0);
    }

    let mut file_args: Vec<String> = Vec::new();
    if !left_args.is_empty() && !left_args[0].starts_with('-') {
        file_args.push(left_args.remove(0));
    }

    let mut i = 0;
    while i < left_args.len() {
        let arg = &left_args[i];
        match arg.as_str() {
            "--check" | "-u" => { static_check_flag = true; }
            "-wf" => { static_check_flag = true; static_check_flag_force_run = true; }
            "-wa" => { static_check_flag = true; static_check_flag_auto_fix = true; }
            "-w" => { static_check_flag = true; static_check_flag_auto_fix = true; }
            "--run" | "-r" => { run_flag = true; }
            "--activate" | "-a" => { activate_flag = true; }
            "--no-color" => { no_color_flag = true; }
            "--color" => { no_color_flag = false; }
            "--quiet" | "-q" => { quiet_flag = true; }
            "--debug" | "-d" => { debug_flag = true; }
            "--exit" | "-e" => { exit_flag = true; }
            "--help" | "-h" => { help_flag = true; }
            "--version" | "-v" => { version_flag = true; }
            "--disable-preprocessor" | "-dp" => { disable_preprocessor = true; }
            "--dump-pp" => { dump_pp_flag = true; }
            "--dump-ast" => { dump_ast_flag = true; }
            "--dump" => { dump_pp_flag = true; dump_ast_flag = true; }
            "--allow-unsafe" => { allow_unsafe = true; }
            "--timer" | "-t" => { timer_flag = true; }
            "-x" => { create_lcx = true; }
            "--fmt" => {
                if i + 1 < args.len() {
                    let mut files = Vec::new();
                    while i + 1 < left_args.len() && !left_args[i + 1].starts_with('-') {
                        files.push(left_args[i + 1].clone());
                        i += 1;
                    }
                    fmt_files.extend(files);
                } else {
                    eprintln!("Error: --fmt requires a path.");
                    exit(1);
                }
            }
            "--clean-cache" | "-cc" | "--clear-cache" | "--cls-cache" => { cls_cache_flag = true; }
            "--no-project-env" => { _no_project_env_flag = true; }
            "--stack-size" => {
                if i + 1 < left_args.len() {
                    if let Ok(size) = left_args[i + 1].parse::<usize>() {
                        stack_size = (size, true);
                        i += 1;
                    } else {
                        eprintln!("Invalid value for --stack-size: {}", left_args[i + 1]);
                        exit(1);
                    }
                } else {
                    eprintln!("--stack-size requires a value");
                    exit(1);
                }
            }
            "--build-info" => {
                let info = get_build_info();

                println!("{}", serde_json::to_string_pretty(&info).unwrap());
                exit(0);
            }
            "--info" | "-i" => {
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
                println!("{:<35}{}", "Total amount of semicolons:".green(), env!("SEMICOLONS"));
                println!("{:<35}{}", "Rust files:".green(), env!("RUST_FILES"));
                println!("{:<35}{}", "Total files:".green(), env!("TOTAL_FILES"));
                
                exit(0);
            }
            "--" => {}
            "--dump-dir" => {
                if i + 1 < left_args.len() {
                    dump_dir = (left_args[i + 1].clone(), true);
                    i += 1;
                } else {
                    eprintln!("--dump-dir requires a path");
                    exit(1);
                }
            }
            "--config" => {
                if i + 1 < left_args.len() {
                    config_arg = Some(left_args[i + 1].clone());
                    i += 1;
                } else {
                    eprintln!("--config requires a path");
                    exit(1);
                }
            }
            "--default-config" | "-dc" => {
                default_config_flag = true;
            }
            "--libs-dir" | "-L" => {
                if i + 1 < left_args.len() {
                    let p = PathBuf::from(&left_args[i + 1]);
                    let abs_path = if p.is_absolute() {
                        p
                    } else {
                        cwd.join(p)
                    };
                    if !abs_path.exists() || !abs_path.is_dir() {
                        eprintln!("Libraries directory does not exist or is not a directory: {}", abs_path.display());
                        exit(1);
                    }
                    libs_dirs.push(abs_path.to_str().unwrap_or(left_args[i + 1].as_str()).to_string());
                    i += 1;
                } else {
                    eprintln!("{} requires a path", arg);
                    exit(1);
                }
            }
            "--cache" => {
                if i + 1 < left_args.len() {
                    let val_str = &left_args[i + 1];
                    let format = CacheFormat::from_str(val_str).unwrap_or_else(|| {
                        eprintln!("Invalid value for --cache, defaulting to 'bin_le'.");
                        CacheFormat::BinLe
                    });
                    cache = (format, true);
                    i += 1;
                } else {
                    eprintln!("--cache requires a value");
                    exit(1);
                }
            }
            "--argv" => {
                if i + 1 < left_args.len() {
                    argv_arg = Some(left_args[i + 1].clone());
                    i += 1;
                } else {
                    eprintln!("--argv requires a value (JSON-like array)");
                    exit(1);
                }
            }
            "--debug-mode" => {
                debug_mode_flag = true;
                if i + 1 < left_args.len() {
                    debug_mode_value = Some(left_args[i + 1].clone());
                    i += 1;
                }
            }
            "-c" | "--code" => {
                if i + 1 < left_args.len() {
                    command_to_run = Some(left_args[i + 1].clone());
                    i += 1;
                } else {
                    eprintln!("{} requires a value", arg);
                    exit(1);
                }
            }
            _ => {
                if arg.starts_with('-') {
                    eprintln!("Unknown argument: {}", arg);
                    let flag_vec: Vec<String> = commands.iter().map(|(flag, _)| flag.to_string()).collect();
                    if let Some(suggestion) = find_closest_match(arg, &flag_vec) {
                        eprintln!("Did you mean: {}", suggestion);
                    }
                    exit(1);
                } else {
                    if file_args.is_empty() {
                        file_args.push(arg.to_string());
                    } else {
                        eprintln!("Only one source file is allowed. Found extra file argument: {}", arg);
                        exit(1);
                    }
                }
            }
        }
        i += 1;
    }
    if let Some(dfile) = dash_file {
        if file_args.is_empty() {
            file_args.push(dfile);
        } else {
            eprintln!("Only one source file is allowed. Found extra file argument: {}", dfile);
            exit(1);
        }
    }

    if !fmt_files.is_empty() {
        eprintln!("{}", "Warning: Formatting feature is experimental and may not cover all edge cases.".yellow());
        for fmt_path in &fmt_files {
            let path = PathBuf::from(&fmt_path);
            if !path.exists() {
                eprintln!("File to format does not exist: {}", fmt_path);
                exit(1);
            }
            if path.is_dir() {
                eprintln!("Expected a file path for formatting, but found a directory: {}", fmt_path);
                exit(1);
            }
            if let Err(e) = fmt::format_file(&path, None) {
                eprintln!("Failed to format file '{}': {}", fmt_path, e);
                exit(1);
            }
        }
        for fmt_path in &fmt_files {
            println!("Formatted file: {}", fmt_path);
        }
        exit(0);
    }

    let mut argv = if !right_argv.is_empty() {
        right_argv
    } else if let Some(val) = argv_arg {
        val.trim_start_matches('[')
            .trim_end_matches(']')
            .split(',')
            .map(|s| s.trim().to_string())
            .filter(|s| !s.is_empty())
            .collect::<Vec<_>>()
    } else {
        vec![]
    };

    if let Some(file0) = file_args.first() {
        argv.insert(0, file0.clone());
    }

    if version_flag {
        println!("Lucia-{}", VERSION);
        exit(0);
    }

    if help_flag {
        println!("{}", "Usage:".bold());
        println!("  lucia [options] [files...]\n");
    
        println!("{}", "Options:".bold());

    
        for (flag, desc) in commands.iter() {
            println!("  {:<32} {}", flag.cyan(), desc);
        }
    
        exit(0);
    }

    #[cfg(not(feature = "single_executable"))]
    {
        if config_arg.is_some() {
            let config_path_str = config_arg.as_ref().unwrap();
            if config_path_str.is_empty() {
                eprintln!("No config path provided. Use --config <path> to specify a config file.");
                exit(1);
            }
            config_path = PathBuf::from(config_path_str);
            libs_path = config_path.parent().unwrap().join("libs.json");
            if !config_path.exists() {
                eprintln!("Config file does not exist: {}", config_path.display());
                exit(1);
            }
        }
    }
    #[cfg(feature = "single_executable")]
    {
        if config_arg.is_some() {
            eprintln!("Note: --config ignored in single_executable build; using default config");
        }
    }

    let config_load_time_start = Instant::now();
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

    #[cfg(feature = "single_executable")]
    let mut config = {
        create_default_config(&enviroment_dir)
    };

    #[cfg(not(feature = "single_executable"))]
    let mut config = if config_arg.is_some() {
        match load_config(&config_path) {
            Ok(config) => config,
            Err(e) => {
                eprintln!("Failed to load specified config file: {}", e);
                exit(1);
            }
        }
    } else {
        match load_config_with_fallback(&config_path, &fallback_config_path, &enviroment_dir) {
            Ok(config) => config,
            Err(e) => {
                eprintln!("Failed to load config: {}", e);
                eprintln!("Attempting to activate environment...");
                if let Err(err) = activate_environment(&enviroment_dir, true) {
                    eprintln!("Failed to activate environment: {}", err);
                    exit(1);
                }
                match load_config_with_fallback(&config_path, &fallback_config_path, &enviroment_dir) {
                    Ok(config) => config,
                    Err(e) => {
                        eprintln!("Failed to load config after activation: {}", e);
                        eprintln!("Creating new config file at {}", fallback_config_path.display());
                        if let Err(err) = create_config_file(&fallback_config_path, &enviroment_dir) {
                            eprintln!("Failed to create new config file: {}", err);
                            exit(1);
                        }
                        load_config_with_fallback(&config_path, &fallback_config_path, &enviroment_dir).unwrap_or_else(|e| {
                            eprintln!("Failed to load new config file: {}", e);
                            exit(1);
                        })
                    }
                }
            }
        }
    };
    
    if !supports_color() {
        config.supports_color = false;
    }
    if no_color_flag {
        config.supports_color = false;
    }

    if timer_flag {
        println!("{}", format!("{}Config load time: {:?}{}", hex_to_ansi(&config.color_scheme.debug, config.supports_color), config_load_time_start.elapsed(), hex_to_ansi("reset", config.supports_color)));
    }

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

    let debug_mode: String = if debug_flag || debug_mode_flag {
        if let Some(mode) = debug_mode_value.clone() {
            match mode.as_str() {
                "full" | "normal" | "minimal" => mode,
                "none" => {
                    config.debug = false;
                    "none".into()
                }
                _ => {
                    eprintln!("Invalid debug mode: '{}'. Valid modes are 'full', 'normal', 'minimal' or 'none'.", mode);
                    exit(1);
                }
            }
        } else {
            "normal".into()
        }
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

    if default_config_flag {
        config = Config::default();
        config.home_dir = enviroment_dir.join("home").to_string_lossy().to_string();
        config.libs_paths = vec![enviroment_dir.join("libs").to_string_lossy().to_string()];
    }

    if debug_flag {
        config.debug = true;
        config.debug_mode = "full".to_string();
    }
    if debug_mode == "none" {
        config.debug = false;
        config.debug_mode = "normal".to_string();
    } else {
        config.debug_mode = debug_mode;
    }
    if quiet_flag {
        config.debug = false;
        config.use_lucia_traceback = false;
        config.warnings = false;
    }
    if cache.1 {
        config.cache_format = cache.0;
    }
    config.libs_paths.extend(libs_dirs);

    if static_check_flag_auto_fix {
        config.type_checker.try_to_auto_fix = true;
    }
    static_check_flag_force_run |= config.type_checker.run_unchecked;
    static_check_flag |= config.type_checker.enabled;
    let static_check_args = (static_check_flag, run_flag, static_check_flag_force_run);

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

    let non_flag_args: Vec<String> = file_args;

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

    let libs_load_time_start = Instant::now();

    #[cfg(feature = "single_executable")]
    let libs_result = load_std_libs_embedded();

    #[cfg(not(feature = "single_executable"))]
    let libs_result = if libs_path.exists() {
        load_std_libs(&libs_path.display().to_string(), moded)
    } else if fallback_libs_path.exists() && fallback_libs_path != libs_path {
        load_std_libs(&fallback_libs_path.display().to_string(), moded)
    } else {
        load_std_libs(&fallback_libs_path.display().to_string(), moded)
    };
    
    match libs_result {
        Ok((_, (f, n))) => {
            if f {
                print_colored(
                    &format!("Warning: Standard libraries mismatch for {}. Running in moded mode.", n),
                    &config.color_scheme.warning,
                    config.supports_color,
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

    let mut plugin_runtime = PluginRuntime::new();

    let plugin_errors = plugin_runtime.load_plugins(&home_dir_path.join("libs"), config.clone()); 

    if !plugin_errors.is_empty() {
        for e in plugin_errors {
            handle_error(&e, "", &config);
        }
        exit(1);
    }

    let deps_file = project_env_path.join("dependencies.json");

    if deps_file.exists() && deps_file.is_file() {
        let content = match fs::read_to_string(&deps_file) {
            Ok(c) => c,
            Err(e) => {
                handle_error(
                    &Error::new("IOError", &format!("Failed to read '{}': {}", deps_file.display(), e), to_static(deps_file.display().to_string())),
                    &deps_file.display().to_string(),
                    &config,
                );
                exit(1);
            }
        };

        let parsed: HashMap<String, String> = match serde_json::from_str(&content) {
            Ok(p) => p,
            Err(e) => {
                handle_error(
                    &Error::new("JSONError", &format!("Failed to parse '{}': {}", deps_file.display(), e), to_static(deps_file.display().to_string())),
                    &deps_file.display().to_string(),
                    &config,
                );
                exit(1);
            }
        };

        eprintln!("{}", &format!("Project environment: Loaded dependencies from '{}'", fix_path(deps_file.display().to_string())).dimmed());

        match check_project_deps(&parsed, &home_dir_path.join("libs"), &config) {
            Ok(_) => {}
            Err((err, dep)) => {
                let (line_range, line_num, line_str) = {
                    let file = File::open(&deps_file).unwrap_or_else(|_| {
                        eprintln!("Failed to open {}", deps_file.display());
                        std::process::exit(1);
                    });
                    let reader = BufReader::new(file);

                    let mut found = ((0, 0), 0, String::new());
                    for (idx, line_res) in reader.lines().enumerate() {
                        let line = line_res.unwrap_or_else(|_| "".to_string());
                        if let Some(start) = line.find(&dep) {
                            let end = start + dep.len() + 1;
                            found = ((start, end), idx + 1, line.clone());
                            break;
                        }
                    }
                    found
                };


                let caller = std::panic::Location::caller();
                let lloc = format!("{}:{}:{}", caller.file(), caller.line(), caller.column());
                let loc = Location::new(&deps_file.display().to_string(), line_str, line_num, line_range, lloc);
                handle_error(
                    &Error::with_location("DependencyError", &err, loc),
                    &deps_file.display().to_string(),
                    &config,
                );
                exit(1);
            }
        }
    }

    if timer_flag {
        println!("{}", format!("{}Libraries load time: {:?}{}", hex_to_ansi(&config.color_scheme.debug, config.supports_color), libs_load_time_start.elapsed(), hex_to_ansi("reset", config.supports_color)));
    }

    if stack_size.1 {
        if stack_size.0 < 1_048_576 {
            eprintln!("Stack size must be at least 1MB (1048576 bytes). Defaulting to 16MB.");
            config.stack_size = 16_777_216;
        } else {
            config.stack_size = stack_size.0;
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
                config.supports_color,
            );
        }
    }

    if config.disable_runtime_type_checking && !(moded || config.allow_unsafe) {
        handle_error(
            &Error::with_help(
                "RuntimeTypeCheckingDisabledError",
                "Runtime type checking is disabled in the config file. This may lead to unexpected behavior.",
                "Set 'moded' and/or 'allow_unsafe' to true in your config file to ignore this error.",
                to_static(exe_path.display().to_string()),
            ),
            "",
            &config,
        );
        exit(1);
    } else if config.debug && config.disable_runtime_type_checking && config.warnings {
        print_colored(
            "Warning: Runtime type checking is disabled. This may lead to unexpected behavior.",
            &config.color_scheme.warning,
            config.supports_color,
        );
    }

    let stack_size_lucia = config.stack_size;

    let handle_creation_time_start = Instant::now();
    let config_debug_color = config.color_scheme.debug.clone();
    let config_supports_color = config.supports_color;
    let cache_dir = home_dir_path.join(".cache");
    let handle = match thread::Builder::new()
        .stack_size(stack_size_lucia)
        .name(format!("Lucia-{}", VERSION))
        .spawn(move || {
            lucia(
                config,
                non_flag_args,
                cwd,
                home_dir_path,
                config_path,
                dump_dir,
                dump_pp_flag,
                dump_ast_flag,
                disable_preprocessor,
                argv,
                timer_flag,
                command_to_run,
                if use_project_env { Some(project_env_path) } else { None },
                static_check_args,
                create_lcx,
                cache_dir,
                plugin_runtime,
            );
        }) {
        Ok(h) => h,
        Err(e) => {
            eprintln!("Failed to create execution thread: {}", e);
            exit(1);
        }
    };

    if timer_flag {
        println!("{}", format!("{}Handle creation time: {:?}{}", hex_to_ansi(&config_debug_color, config_supports_color), handle_creation_time_start.elapsed(), hex_to_ansi("reset", config_supports_color)));
    }

    handle.join().unwrap();
}
