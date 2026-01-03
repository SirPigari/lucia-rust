use wasm_bindgen::prelude::*;
use std::path::PathBuf;

use web_sys::{console};
use once_cell::unsync::OnceCell;
use serde_wasm_bindgen::{from_value, to_value};
use std::cell::RefCell;
use js_sys::Function;

thread_local! {
    static IS_PANIC_HOOK_SET: OnceCell<bool> = OnceCell::new();
    static IS_SLEEPING: RefCell<bool> = RefCell::new(false);

    static PRINT_CALLBACK: RefCell<Option<Function>> = RefCell::new(None);
    static CLEAR_CALLBACK: RefCell<Option<Function>> = RefCell::new(None);
    static INPUT_CALLBACK: RefCell<Option<Function>> = RefCell::new(None);
}

#[wasm_bindgen]
pub fn set_print_callback(cb: Function) {
    PRINT_CALLBACK.with(|cell| {
        *cell.borrow_mut() = Some(cb);
    });
}

#[wasm_bindgen]
pub fn set_input_callback(cb: Function) {
    INPUT_CALLBACK.with(|cell| {
        *cell.borrow_mut() = Some(cb);
    });
}

#[wasm_bindgen]
pub fn set_clear_callback(cb: Function) {
    CLEAR_CALLBACK.with(|cell| {
        *cell.borrow_mut() = Some(cb);
    });
}

#[macro_export]
macro_rules! println {
    ($($arg:tt)*) => {{
        use wasm_bindgen::JsValue;
        let s = format!($($arg)*);
        $crate::PRINT_CALLBACK.with(|cb_cell| {
            if let Some(cb) = &*cb_cell.borrow() {
                let _ = cb.call1(&JsValue::NULL, &JsValue::from_str(&s));
            } else {
                web_sys::console::log_1(&s.into());
            }
        });
    }};
}

#[macro_export]
macro_rules! eprintln {
    ($($arg:tt)*) => {{
        use wasm_bindgen::JsValue;
        let s = format!($($arg)*);
        $crate::PRINT_CALLBACK.with(|cb_cell| {
            if let Some(cb) = &*cb_cell.borrow() {
                let _ = cb.call1(&JsValue::NULL, &JsValue::from_str(&s));
            } else {
                web_sys::console::error_1(&s.into());
            }
        });
    }};
}

#[macro_export]
macro_rules! print {
    ($($arg:tt)*) => {{
        use wasm_bindgen::JsValue;
        let s = format!($($arg)*);
        $crate::PRINT_CALLBACK.with(|cb_cell| {
            if let Some(cb) = &*cb_cell.borrow() {
                let _ = cb.call1(&JsValue::NULL, &JsValue::from_str(&s));
            } else {
                web_sys::console::log_1(&s.into());
            }
        });
    }};
}

#[macro_export]
macro_rules! input {
    ($prompt:expr) => {{
        use wasm_bindgen::JsValue;
        let prompt_str = $prompt;
        $crate::INPUT_CALLBACK.with(|cb_cell| {
            if let Some(cb) = &*cb_cell.borrow() {
                match cb.call1(&JsValue::NULL, &JsValue::from_str(&prompt_str)) {
                    Ok(result) => result.as_string().unwrap_or_default(),
                    Err(_) => String::new(),
                }
            } else {
                String::new()
            }
        })
    }};
}

#[macro_export]
macro_rules! clear {
    () => {{
        $crate::CLEAR_CALLBACK.with(|cb_cell| {
            if let Some(cb) = &*cb_cell.borrow() {
                let _ = cb.call0(&wasm_bindgen::JsValue::NULL);
            } else {
                web_sys::console::clear();
            }
        });
    }};
}

#[macro_export]
macro_rules! sleep {
    ($ms:expr) => {{
        use $crate::IS_SLEEPING;
        use gloo_timers::future::TimeoutFuture;
        use wasm_bindgen_futures::spawn_local;

        let ms = $ms.try_into().unwrap_or(0);

        IS_SLEEPING.with(|cell| *cell.borrow_mut() = true);
        let fut = async move {
            TimeoutFuture::new(ms).await;
            IS_SLEEPING.with(|cell| *cell.borrow_mut() = false);
        };
        spawn_local(fut);
    }};
}

#[macro_export]
macro_rules! is_sleeping {
    () => {{
        use $crate::IS_SLEEPING;
        IS_SLEEPING.with(|cell| *cell.borrow())
    }};
}

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
        pub mod static_checker;
        pub mod plugins;
    }
    pub mod libs {
        pub mod math {
            pub mod main;
        }
        pub mod os {
            pub mod main;
        }
        pub mod time {
            pub mod main;
        }
        pub mod json {
            pub mod main;
        }
        pub mod regex {
            pub mod main;
            pub mod regex_engine;
        }
        pub mod collections {
            pub mod main;
            pub mod deprecated_stuff;
        }
        pub mod random {
            pub mod main;
        }
        pub mod nest {
            pub mod main;
        }
        pub mod hash {
            pub mod main;
        }
    }
}

mod lexer;
mod parser;
mod interpreter;

use crate::env::runtime::config::Config;
use crate::env::runtime::utils::{fix_path, hex_to_ansi, format_value, check_ansi, get_line_info};
use crate::env::runtime::internal_structs::CacheFormat;
use crate::env::runtime::libs::load_std_libs;
use crate::env::runtime::errors::Error;
use crate::env::runtime::value::Value;
use crate::env::runtime::preprocessor::Preprocessor;
use crate::env::runtime::tokens::{Token, Location};
use crate::env::runtime::static_checker::Checker;
use crate::parser::Parser;
use crate::lexer::Lexer;
use crate::interpreter::Interpreter;

const VERSION: &str = env!("CARGO_PKG_VERSION");

pub fn now_ms() -> f64 {
    web_sys::window()
        .expect("no window")
        .performance()
        .expect("performance not available")
        .now()
}

pub fn unescape_string(input: &str) -> String {
    input.replace("\\n", "\n").replace("\\t", "\t")
}

pub fn get_line_info_from_source(source: &str, line_number: usize) -> Option<String> {
    let source = unescape_string(source);
    source.lines().nth(line_number.saturating_sub(1)).map(|s| s.to_string())
}

#[cold]
pub fn handle_error(
    error: &Error,
    source: &str,
    config: &Config,
) {
    const NEXT_LINES: bool = false;
    const PREV_LINES: bool = false;

    const TAB: &str = "    ";

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

        let lucia_source_loc = if debug && (loc.lucia_source_loc != "<unknown>") && !loc.lucia_source_loc.is_empty() {
            format!("Raised from {}\n", loc.lucia_source_loc.clone())
        } else {
            String::new()
        };
        let file_name = fix_path(loc.file.to_string());
        let line_number = loc.line_number;
        let range = loc.range;
        let col = range.0;
        let reset = hex_to_ansi("reset", use_colors);

        let current_line = if line_number > 0 {
            match get_line_info(source, line_number) {
                Some(line) => line,
                None => {
                    if file_name.starts_with('<') && file_name.ends_with('>') {
                        source.lines().next().unwrap_or_default().to_string()
                    } else {
                        loc.line_string.clone()
                    }
                }
            }
        } else {
            loc.line_string.clone()
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
            let start = range.0.saturating_sub(1);
            let end = range.1;

            if start >= line_len || end == 0 || start >= end {
                arrows_under = " ".repeat(col.saturating_sub(1)) + "^";
            } else {
                arrows_under = "~".repeat(line_len);
                let len = end.saturating_sub(start);
                arrows_under.replace_range(start..end, &"^".repeat(len.max(1)));
            }
        }

        trace.push_str(&format!(
            "{}{}{}",
            hex_to_ansi(&config.color_scheme.debug, use_colors),
            lucia_source_loc,
            hex_to_ansi("reset", use_colors),
        ));

        trace.push_str(&format!(
            "{}-> File '{}:{}:{}' got error:\n",
            hex_to_ansi(&config.color_scheme.exception, use_colors),
            file_name,
            line_number,
            col
        ));

        if line_number > 0 {
            if prev_line.is_some() && prev_line.as_ref().map_or(false, |l| l.len() > 8) && PREV_LINES {
                trace.push_str(&format!("{TAB}{} | {}\n", line_number - 1, prev_line.as_ref().unwrap()));
            }

            trace.push_str(&format!("{TAB}{} | {}\n", line_number, current_line));
            trace.push_str(&format!("{TAB}{} | {}\n", indent, arrows_under));

            if next_line.is_some() && next_line.as_ref().map_or(false, |l| l.len() > 8) && NEXT_LINES {
                trace.push_str(&format!("{TAB}{} ...\n", indent));
            }
        }

        trace.push_str(&format!(
            "{TAB}{} | {}: {}",
            indent, err.error_type, err.msg
        ));

        if let Some(help) = &err.help {
            if !help.is_empty() {
                trace.push_str(&format!(
                    "\n{TAB}{} ...\n{TAB}{}{} | {}Help:{} {}{}",
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
        if let Some(ref inner) = err.ref_err {
            let same_loc = match (&err.loc, &inner.loc) {
                (Some(a), Some(b)) => a.file == b.file
                    && a.line_number == b.line_number
                    && a.range == b.range,
                _ => false,
            };

            if same_loc {
                trace.push_str(&print_single_error(err));

                let indent = " ".repeat(err.loc.as_ref().map_or(0, |l| l.line_number.to_string().len()));
                trace.push_str(&format!(
                    "\n{TAB}{}^-- caused by:\n{TAB}{} | {}: {}",
                    hex_to_ansi(&config.color_scheme.exception, use_colors),
                    indent,
                    inner.error_type,
                    inner.msg
                ));

                if let Some(help) = &inner.help {
                    if !help.is_empty() {
                        trace.push_str(&format!(
                            "\n{TAB}{}   (Help: {})",
                            indent,
                            help
                        ));
                    }
                }

                current_error = inner.ref_err.as_deref();
                continue;
            }
        }

        trace.push_str(&print_single_error(err));
        if let Some(ref inner) = err.ref_err {
            trace.push_str(&format!(
                "\n{TAB}{}^-- caused by:\n",
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

pub fn execute_code_string_wasm(
    code: String,
    config: &Config,
    disable_preprocessor: bool,
    argv: &Vec<String>,
    timer_flag: bool,
    static_check_args: (bool, bool, bool),
) {
    let code = unescape_string(&code);

    // WASM-safe timing
    let lexing_start = now_ms();
    let raw_tokens = Lexer::new(&code, "<-c>").tokenize();
    if timer_flag {
        console::log_1(&format!("Lexing time: {:.2}ms", now_ms() - lexing_start).into());
    }

    let processed_tokens = if !disable_preprocessor {
        let preprocessing_start = now_ms();
        let mut preprocessor = Preprocessor::new(PathBuf::from("libs"), "<-c>");
        let res = match preprocessor.process(raw_tokens.clone(), &PathBuf::from(".")) {
            Ok(tokens) => tokens,
            Err(e) => { handle_error(&e, &code, &config); return; }
        };
        if timer_flag {
            console::log_1(&format!("Preprocessing time: {:.2}ms", now_ms() - preprocessing_start).into());
        }
        res
    } else { raw_tokens };

    let parsing_start = now_ms();
    let tokens: Vec<Token> = processed_tokens.clone();
    let mut parser = Parser::new(tokens.clone());
    let statements = match parser.parse_safe() {
        Ok(stmts) => stmts,
        Err(error) => { handle_error(&error, &code, &config); return; }
    };
    if timer_flag {
        console::log_1(&format!("Parsing time: {:.2}ms", now_ms() - parsing_start).into());
    }

    let (static_check_flag, run_flag, static_check_flag_force_run) = static_check_args;
    let mut can_run = true;
    if static_check_flag {
        let static_check_start = now_ms();
        let mut checker = Checker::new(config.clone(), "<-c>".to_string());
        let errors = checker.check(statements.clone(), false);
        if !errors.is_empty() {
            for err in &errors { handle_error(err, &code, &config); }
            if !static_check_flag_force_run && (!run_flag || !errors.is_empty()) {
                can_run = false;
            }
        }
        if timer_flag {
            console::log_1(&format!("Static check time: {:.2}ms", now_ms() - static_check_start).into());
        }
    }

    if !can_run { return; }

    let interpreter_start = now_ms();
    let mut interpreter = Interpreter::new(
        config.clone(),
        "<-c>",
        &PathBuf::from("."),
        (PathBuf::from("libs"), PathBuf::from("."), !disable_preprocessor),
        argv,
    );

    let result = interpreter.interpret(statements, false);
    if timer_flag {
        console::log_1(&format!("Interpreter time: {:.2}ms", now_ms() - interpreter_start).into());
    }

    match result {
        Ok(value) => { if !matches!(value, Value::Null) { console::log_1(&format!("{}", format_value(&value)).into()); } }
        Err(error) => { handle_error(&error, &code, &config); }
    }
}

#[wasm_bindgen]
pub fn set_panic_hook() {
    let already_set = IS_PANIC_HOOK_SET.with(|cell| cell.get().copied().unwrap_or(false));
    if already_set {
        return;
    }

    IS_PANIC_HOOK_SET.with(|cell| { let _ = cell.set(true); });

    std::panic::set_hook(Box::new(|panic_info| {
        let msg = if let Some(s) = panic_info.payload().downcast_ref::<&str>() {
            *s
        } else if let Some(s) = panic_info.payload().downcast_ref::<String>() {
            s.as_str()
        } else {
            "Unknown panic!"
        };

        let location = if let Some(loc) = panic_info.location() {
            format!("{}:{}:{}", loc.file(), loc.line(), loc.column())
        } else {
            "<unknown location>".into()
        };

        eprintln!("{}", format!("Panic at {}: {}", location, msg));
    }));
}

#[wasm_bindgen]
pub fn get_default_config() -> JsValue {
    let config = Config::default();
    to_value(&config).unwrap_or_else(|e| {
        web_sys::console::error_1(&format!("Failed to serialize default config: {}", e).into());
        JsValue::NULL
    })
}

#[wasm_bindgen]
pub fn run_code_wasm(code: &str, config_js: JsValue) {
    crate::set_panic_hook();

    let _ = load_std_libs();

    let mut config: Config = Config::default();

    if !config_js.is_null() && !config_js.is_undefined() {
        match from_value::<serde_json::Value>(config_js.clone()) {
            Ok(js_val) => {
                if let Some(obj) = js_val.as_object() {
                    for (key, value) in obj {
                        match key.as_str() {
                            "moded" => config.moded = value.as_bool().unwrap_or(config.moded),
                            "debug" => config.debug = value.as_bool().unwrap_or(config.debug),
                            "debug_mode" => {
                                config.debug_mode = value.as_str().unwrap_or(&config.debug_mode).to_string()
                            }
                            "supports_color" => config.supports_color = value.as_bool().unwrap_or(config.supports_color),
                            "use_lucia_traceback" => config.use_lucia_traceback = value.as_bool().unwrap_or(config.use_lucia_traceback),
                            "warnings" => config.warnings = value.as_bool().unwrap_or(config.warnings),
                            "allow_fetch" => config.allow_fetch = value.as_bool().unwrap_or(config.allow_fetch),
                            "allow_unsafe" => config.allow_unsafe = value.as_bool().unwrap_or(config.allow_unsafe),
                            "allow_inline_config" => config.allow_inline_config = value.as_bool().unwrap_or(config.allow_inline_config),
                            "home_dir" => config.home_dir = value.as_str().unwrap_or(&config.home_dir).to_string(),
                            "stack_size" => {
                                if let Some(n) = value.as_u64() {
                                    config.stack_size = n as usize;
                                }
                            }
                            "version" => config.version = value.as_str().unwrap_or(&config.version).to_string(),
                            "cache_format" => {
                                if let Some(s) = value.as_str() {
                                    config.cache_format = CacheFormat::from_str(s).unwrap_or(CacheFormat::NoCache);
                                }
                            }
                            _ => {}
                        }
                    }
                }
            }
            Err(e) => web_sys::console::error_1(&format!("Failed to parse JS config: {}", e).into()),
        }
    }

    crate::execute_code_string_wasm(
        code.to_string(),
        &config,
        false,
        &Vec::new(),
        false,
        (false, true, false),
    );
}

#[wasm_bindgen]
pub fn get_lucia_version() -> String {
    VERSION.to_string()
}

#[wasm_bindgen]
pub fn run_code_wasm_no_config(code: &str) {
    crate::set_panic_hook();

    let _ = load_std_libs();

    let config: Config = Config::default();

    crate::execute_code_string_wasm(
        code.to_string(),
        &config,
        false,
        &Vec::new(),
        false,
        (false, true, false),
    );
}

fn main() {}