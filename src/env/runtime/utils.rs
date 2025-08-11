use std::io::{self, Write, stdout};
use std::collections::HashMap;
use crate::env::runtime::config::{Config};
use crate::env::runtime::functions::Function;
use once_cell::sync::Lazy;
use std::sync::{Mutex, Arc};
use crate::env::runtime::functions::{Parameter, NativeMethod, FunctionMetadata, UserFunction};
use crate::env::runtime::statements::Statement;
use crate::env::runtime::types::{Int, Float, Type};
use crate::env::runtime::value::{Value};
use serde_json::Value as JsonValue;
use std::time::{SystemTime, UNIX_EPOCH};
use std::path::PathBuf;
use std::path::Path;
use std::str::FromStr;
use std::time::Duration;
use crate::env::runtime::types::VALID_TYPES;
use regex::Regex;
use crate::env::runtime::precompile::interpret;
use crossterm::{
    execute,
    terminal::{Clear, ClearType},
    cursor::MoveTo,
    event,
};
use imagnum::math::{
    ERR_UNIMPLEMENTED,
    ERR_INVALID_FORMAT,
    ERR_DIV_BY_ZERO,
    ERR_NEGATIVE_RESULT,
    ERR_NEGATIVE_SQRT,
    ERR_NUMBER_TOO_LARGE,
    ERR_INFINITE_RESULT,
};


static STATIC_STORAGE: Lazy<Mutex<HashMap<String, &'static str>>> = Lazy::new(|| Mutex::new(HashMap::new()));

#[cfg(windows)]
pub fn supports_color() -> bool {
    use std::ptr::null_mut;
    use std::ffi::c_void;

    type HANDLE = *mut c_void;
    type DWORD = u32;
    type BOOL = i32;

    const ENABLE_VIRTUAL_TERMINAL_PROCESSING: DWORD = 0x0004;
    const STD_OUTPUT_HANDLE: DWORD = -11i32 as u32;
    const INVALID_HANDLE_VALUE: HANDLE = !0 as HANDLE;

    const FILE_TYPE_CHAR: DWORD = 0x0002;

    unsafe extern "system" {
        fn GetStdHandle(nStdHandle: DWORD) -> HANDLE;
        fn GetConsoleMode(hConsoleHandle: HANDLE, lpMode: *mut DWORD) -> BOOL;
        fn GetFileType(hFile: HANDLE) -> DWORD;
    }

    unsafe {
        let handle = GetStdHandle(STD_OUTPUT_HANDLE);
        if handle == null_mut() || handle == INVALID_HANDLE_VALUE {
            return false;
        }

        let file_type = GetFileType(handle);
        if file_type != FILE_TYPE_CHAR {
            return false;
        }

        let mut mode: DWORD = 0;
        if GetConsoleMode(handle, &mut mode as *mut DWORD) == 0 {
            return false;
        }

        (mode & ENABLE_VIRTUAL_TERMINAL_PROCESSING) != 0
    }
}

#[cfg(unix)]
pub fn supports_color() -> bool {
    use std::io::IsTerminal;
    let is_tty = std::io::stdout().is_terminal();
    let term = std::env::var("TERM").unwrap_or_default();
    let not_dumb = term != "dumb";

    is_tty && not_dumb
}

#[cfg(not(any(unix, windows)))]
pub fn supports_color() -> bool {
    false
}

#[cfg(windows)]
pub fn get_remaining_stack_size() -> Option<usize> {
    #[allow(non_camel_case_types)]
    type ULONG_PTR = usize;

    #[link(name = "kernel32")]
    unsafe extern "system" {
        fn GetCurrentThreadStackLimits(
            low_limit: *mut ULONG_PTR,
            high_limit: *mut ULONG_PTR,
        );
    }

    unsafe {
        let mut low: ULONG_PTR = 0;
        let mut high: ULONG_PTR = 0;

        GetCurrentThreadStackLimits(&mut low, &mut high);

        let sp = current_stack_pointer();

        if sp >= low && sp <= high {
            Some(sp - low)
        } else {
            None
        }
    }
}

// stupid macos
// and stupid aarch64
// and stupid chatgpt for NOT KNOWING THAT THE FUCKING EXTERN IS UNSAFE
// and the fucking compiler not checking that on windows, you literally dont need it just CHECK IT
// milochov
#[cfg(unix)]
pub fn get_remaining_stack_size() -> Option<usize> {
    use libc::{pthread_self, pthread_attr_destroy};
    use std::mem::MaybeUninit;
    use std::ptr;

    #[cfg(any(not(target_os = "macos"), target_arch = "x86_64"))]
    unsafe fn pthread_getattr_np_wrapper(thread: libc::pthread_t, attr: *mut libc::pthread_attr_t) -> libc::c_int {
        unsafe extern "C" {
            fn pthread_getattr_np(thread: libc::pthread_t, attr: *mut libc::pthread_attr_t) -> libc::c_int;
        }
        unsafe { pthread_getattr_np(thread, attr) }
    }

    #[cfg(all(target_os = "macos", target_arch = "aarch64"))]
    unsafe fn pthread_getattr_np_wrapper(_thread: libc::pthread_t, _attr: *mut libc::pthread_attr_t) -> libc::c_int {
        100_000
    }

    unsafe {
        unsafe extern "C" {
            fn pthread_attr_getstack(
                attr: *const libc::pthread_attr_t,
                stackaddr: *mut *mut libc::c_void,
                stacksize: *mut usize,
            ) -> libc::c_int;
        }

        let mut attr = MaybeUninit::<libc::pthread_attr_t>::uninit();

        if pthread_getattr_np_wrapper(pthread_self(), attr.as_mut_ptr()) != 0 {
            return None;
        }

        let mut attr = attr.assume_init();

        let mut stackaddr: *mut libc::c_void = ptr::null_mut();
        let mut stacksize: usize = 0;

        if pthread_attr_getstack(&attr, &mut stackaddr as *mut _, &mut stacksize as *mut _) != 0 {
            pthread_attr_destroy(&mut attr);
            return None;
        }

        pthread_attr_destroy(&mut attr);

        let sp = current_stack_pointer();

        let stack_bottom = stackaddr as usize;
        let stack_top = stack_bottom + stacksize;

        if sp >= stack_bottom && sp <= stack_top {
            Some(sp - stack_bottom)
        } else {
            None
        }
    }
}

#[inline(always)]
fn current_stack_pointer() -> usize {
    let x = 0u8;
    &x as *const u8 as usize
}

#[cfg(not(any(unix, windows)))]
pub fn get_remaining_stack_size() -> Option<usize> {
    None
}

pub fn to_static(s: String) -> &'static str {
    let mut cache = STATIC_STORAGE.lock().unwrap();
    if let Some(&static_ref) = cache.get(&s) {
        return static_ref;
    }
    let static_ref: &'static str = Box::leak(s.clone().into_boxed_str());
    cache.insert(s, static_ref);
    static_ref
}

pub fn levenshtein_distance(a: &str, b: &str) -> usize {
    let mut costs = vec![0; b.len() + 1];
    for j in 0..=b.len() {
        costs[j] = j;
    }

    for (i, ca) in a.chars().enumerate() {
        let mut last = i;
        costs[0] = i + 1;
        for (j, cb) in b.chars().enumerate() {
            let old = costs[j + 1];
            let cost = if ca == cb {
                0
            } else if ca.eq_ignore_ascii_case(&cb) {
                1
            } else {
                2
            };
            costs[j + 1] = std::cmp::min(
                std::cmp::min(costs[j] + 1, costs[j + 1] + 1),
                last + cost,
            );
            last = old;
        }
    }
    costs[b.len()]
}

pub fn get_line_info(source: &str, line_number: usize) -> Option<String> {
    source.lines().nth(line_number.saturating_sub(1)).map(|s| s.to_string())
}

pub fn clear_terminal() -> Result<(), io::Error> {
    let mut stdout = stdout();
    execute!(
        stdout,
        Clear(ClearType::All),
        MoveTo(0, 0)
    )?;
    stdout.flush()?;
    Ok(())
}

pub fn hex_to_ansi(hex_color: &str, use_colors: bool) -> String {
    if !use_colors {
        return "".to_string();
    }

    if hex_color == "reset" {
        return "\x1b[0m".to_string();
    }

    let hex = if hex_color.starts_with('#') { &hex_color[1..] } else { hex_color };

    if hex.len() == 6 {
        let r = u8::from_str_radix(&hex[0..2], 16).unwrap();
        let g = u8::from_str_radix(&hex[2..4], 16).unwrap();
        let b = u8::from_str_radix(&hex[4..6], 16).unwrap();
        return format!("\x1b[38;2;{};{};{}m", r, g, b);
    }

    "\x1b[0m".to_string()
}

pub fn print_colored(message: &str, color: &str, use_colors: Option<bool>) {
    let use_colors = use_colors.unwrap_or(true);
    let colored_message = format!("{}{}{}", hex_to_ansi(color, use_colors), message, hex_to_ansi("reset", use_colors));
    println!("{}", colored_message);
}

pub fn print_colored_no_newline(message: &str, color: &str, use_colors: Option<bool>) {
    let use_colors = use_colors.unwrap_or(true);
    let colored_message = format!("{}{}{}", hex_to_ansi(color, use_colors), message, hex_to_ansi("reset", use_colors));
    print!("{}", colored_message);
    std::io::stdout().flush().unwrap();
}

pub fn debug_log_no_newline(message: &str, config: &Config, use_colors: Option<bool>) {
    let use_colors = use_colors.unwrap_or(true);
    if config.debug && (config.debug_mode == "full" || config.debug_mode == "normal") {
        let single_line_message = message
            .replace('\n', "\\n")
            .replace('\r', "\\r")
            .replace('\t', "\\t")
            .replace('\0', "\\0")
            .replace('\x1b', "\\e")
            .replace(r"\A", "\n");
        print_colored_no_newline(&single_line_message, &config.color_scheme.debug, Some(use_colors));
    }
}

pub fn read_input(prompt: &str) -> String {
    print!("{}", prompt);
    io::stdout().flush().unwrap();
    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap();
    input.trim().to_string()
}

pub fn format_value(value: &Value) -> String {
    match value {
        Value::Float(n) => format_float(&n),
        Value::Int(n) => format_int(&n),
        Value::String(s) => format!("\"{}\"", s),
        Value::Boolean(b) => b.to_string(),
        Value::Null => "null".to_string(),

        Value::Map { keys, values, .. } => {
            let formatted_pairs: Vec<String> = keys
                .iter()
                .zip(values.iter())
                .filter(|(key, _)| match key {
                    Value::String(s) => s != "_line" && s != "_column",
                    _ => true,
                })
                .map(|(key, value)| format!("{}: {}", format_value(key), format_value(value)))
                .collect();
            format!("{{{}}}", formatted_pairs.join(", "))
        }

        Value::List(values) => {
            let formatted_values: Vec<String> = values.iter().map(format_value).collect();
            format!("[{}]", formatted_values.join(", "))
        }

        Value::Tuple (values) => {
            let formatted_values: Vec<String> = values.iter().map(format_value).collect();
            format!("({})", formatted_values.join(", "))
        }

        Value::Bytes(bytes) => {
            let formatted_bytes: Vec<String> = bytes.iter().map(|b| format!("{:02x}", b)).collect();
            format!("b\"{}\"", formatted_bytes.join(" "))
        }

        Value::Type(t) => t.display(),

        Value::Function(func) => {
            let addr = func.ptr() as *const () as usize;
            format!("<function '{}' at 0x{:X}>", func.get_name(), addr)
        }

        Value::Generator(generator) => {
            let addr = generator.ptr() as *const () as usize;
            match generator.name() {
                Some(name) => format!("<generator '{}' at 0x{:X}>", name, addr),
                None => format!("<generator at 0x{:X}>", addr),
            }
        }

        Value::Module(obj, _) => {
            let addr = obj.ptr() as *const () as usize;
            format!("<module '{}' at 0x{:X}>", obj.name(), addr)
        }

        Value::Pointer(arc) => {
            let raw_ptr = Arc::as_ptr(arc);
            let addr = raw_ptr as usize;
            format!("<pointer to 0x{:X}>", addr)
        }

        Value::Error(err_type, err_msg, _) => format!("<{}: {}>", err_type, err_msg),
    }
}

pub fn format_float(f: &Float) -> String {
    f.to_string()
}

pub fn format_int(n: &Int) -> String {
    n.to_string()
}

pub fn find_closest_match<'a>(target: &str, options: &'a [String]) -> Option<&'a str> {
    let mut closest: Option<(&str, usize)> = None;

    for opt in options {
        if opt == "_" {
            continue;
        }
        let dist = levenshtein_distance(target, opt);
        if closest.is_none() || dist < closest.unwrap().1 {
            closest = Some((opt.as_str(), dist));
        }
    }

    match closest {
        Some((s, dist)) if dist <= 2 => Some(s),
        _ => None,
    }
}


pub fn check_ansi<'a>(ansi: &'a str, use_colors: &bool) -> &'a str {
    if !*use_colors {
        &ansi[0..0]
    } else {
        ansi
    }
}

pub fn debug_log(message: &str, config: &Config, use_colors: Option<bool>) {
    let use_colors = use_colors.unwrap_or(true);
    if config.debug && (config.debug_mode == "full" || config.debug_mode == "normal") {
        let single_line_message = message
            .replace('\n', "\\n")
            .replace('\r', "\\r")
            .replace('\t', "\\t")
            .replace('\0', "\\0")
            .replace('\x1b', "\\e")
            .replace(r"\A", "\n");
        print_colored(&single_line_message, &config.color_scheme.debug, Some(use_colors));
    }
}

pub fn capitalize(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
        None => String::new(),
    }
}

pub fn unescape_string_full(s: &str) -> Result<String, String> {
    let wrapped = format!("\"{}\"", s);
    serde_json::from_str::<String>(&wrapped)
        .map_err(|e| format!("Failed to fully unescape: {}", e))
}

pub fn escape_string(s: &str) -> Result<String, String> {
    serde_json::to_string(s)
        .map_err(|e| format!("Failed to escape string: {}", e))
        .and_then(|quoted| {
            if quoted.len() >= 2 && quoted.starts_with('"') && quoted.ends_with('"') {
                Ok(quoted[1..quoted.len() - 1].to_string())
            } else {
                Err("Invalid JSON string output".to_string())
            }
        })
}


pub fn unescape_string(s: &str) -> Result<String, String> {
    if s.len() < 2 {
        return Err("String too short to unescape".into());
    }

    let first = s.chars().next().unwrap();
    let last = s.chars().last().unwrap();

    if (first == '"' && last == '"') || (first == '\'' && last == '\'') {
        let inner = &s[1..s.len() - 1];

        let json_quoted = if first == '\'' {
            format!("\"{}\"", inner)
        } else {
            s.to_string()
        };

        match serde_json::from_str::<String>(&json_quoted) {
            Ok(unescaped) => Ok(unescaped),
            Err(e) => Err(format!("Failed to unescape string: {}", e)),
        }
    } else {
        Err("String not properly quoted".into())
    }
}

pub fn make_native_method<F>(
    name: &str,
    func: F,
    parameters: Vec<Parameter>,
    return_type: &str,
    is_public: bool,
    is_static: bool,
    is_final: bool,
    state: Option<String>,
) -> Value
where
    F: Fn(&HashMap<String, Value>) -> Value + Send + Sync + 'static,
{
    let method = NativeMethod {
        func: Arc::new(func),
        meta: FunctionMetadata {
            name: name.to_string(),
            parameters,
            return_type: Type::new_simple(return_type),
            is_public,
            is_static,
            is_final,
            is_native: true,
            state,
        },
    };

    Value::Function(Function::NativeMethod(Arc::new(method)))
}

pub fn get_type_default(type_: &str) -> Value {
    if type_.starts_with("&") {
        let inner_type = &type_[1..];
        let boxed = Box::new(get_type_default(inner_type));
        let ptr = Box::into_raw(boxed) as usize;
        return Value::Pointer( unsafe { Arc::from_raw(ptr as *const Value) });
    }
    if type_.starts_with("?") {
        let inner_type = &type_[1..];
        return get_type_default(inner_type);
    }
    match type_ {
        "float" => Value::Float(0.0.into()),
        "int" => Value::Int(0.into()),
        "string" => Value::String(String::new()),
        "bool" => Value::Boolean(false),
        "any" => Value::Null,
        "map" => Value::Map { keys: vec![], values: vec![] },
        "list" => Value::List(vec![]),
        "bytes" => Value::Bytes(vec![]),
        "tuple" => Value::Tuple(vec![]),
        "void" => Value::Null,
        _ => Value::Null,
    }
}

pub fn get_type_default_as_statement(type_: &str) -> Statement {
    if type_.starts_with("&") {
        let inner_type = &type_[1..];
        let inner_statement = get_type_default_as_statement(inner_type);
        return Statement::Statement {
            keys: vec![Value::String("type".to_string()), Value::String("value".to_string())],
            values: vec![
                Value::String("POINTER_REF".to_string()),
                inner_statement.convert_to_map()
            ],
            loc: None,
        };
    }

    if type_.starts_with("?") {
        let inner_type = &type_[1..];
        return get_type_default_as_statement(inner_type);
    }

    match type_ {
        "int" => Statement::Statement {
            keys: vec![
                Value::String("type".to_string()),
                Value::String("value".to_string())
            ],
            values: vec![
                Value::String("NUMBER".to_string()),
                Value::String("0".to_string())
            ],
            loc: None,
        },
        "float" => Statement::Statement {
            keys: vec![
                Value::String("type".to_string()),
                Value::String("value".to_string())
            ],
            values: vec![
                Value::String("NUMBER".to_string()),
                Value::String("0.0".to_string())
            ],
            loc: None,
        },
        "str" => Statement::Statement {
            keys: vec![
                Value::String("type".to_string()),
                Value::String("value".to_string()),
                Value::String("mods".to_string()),
            ],
            values: vec![
                Value::String("STRING".to_string()),
                Value::String("\"\"".to_string()),
                Value::List(vec![]),
            ],
            loc: None,
        },
        "bool" => Statement::Statement {
            keys: vec![
                Value::String("type".to_string()),
                Value::String("value".to_string())
            ],
            values: vec![
                Value::String("BOOLEAN".to_string()),
                Value::String("false".to_string()),
            ],
            loc: None,
        },
        "any" => Statement::Statement {
            keys: vec![
                Value::String("type".to_string()),
                Value::String("value".to_string())
            ],
            values: vec![
                Value::String("BOOLEAN".to_string()),
                Value::String("null".to_string()),
            ],
            loc: None,
        },
        "map" => Statement::Statement {
            keys: vec![
                Value::String("type".to_string()),
                Value::String("keys".to_string()),
                Value::String("values".to_string())
            ],
            values: vec![
                Value::String("MAP".to_string()),
                Value::List(vec![]),
                Value::List(vec![])
            ],
            loc: None,
        },
        "list" => Statement::Statement {
            keys: vec![
                Value::String("type".to_string()),
                Value::String("iterable_type".to_string()),
                Value::String("elements".to_string())
            ],
            values: vec![
                Value::String("ITERABLE".to_string()),
                Value::String("LIST".to_string()),
                Value::List(vec![])
            ],
            loc: None,
        },
        "bytes" => Statement::Statement {
            keys: vec![
                Value::String("type".to_string()),
                Value::String("value".to_string()),
                Value::String("mods".to_string()),
            ],
            values: vec![
                Value::String("STRING".to_string()),
                Value::String("\"\"".to_string()),
                Value::List(vec![Value::String("b".to_string())])
            ],
            loc: None,
        },
        "tuple" => Statement::Statement {
            keys: vec![
                Value::String("type".to_string()),
                Value::String("elements".to_string())
            ],
            values: vec![
                Value::String("TUPLE".to_string()),
                Value::List(vec![])
            ],
            loc: None,
        },
        "void" => Statement::Statement {
            keys: vec![
                Value::String("type".to_string()),
                Value::String("value".to_string())
            ],
            values: vec![
                Value::String("BOOLEAN".to_string()),
                Value::String("null".to_string()),
            ],
            loc: None,
        },
        _ => Statement::Statement {
            keys: vec![
                Value::String("type".to_string()),
                Value::String("value".to_string())
            ],
            values: vec![
                Value::String("BOOLEAN".to_string()),
                Value::String("null".to_string()),
            ],
            loc: None,
        },
    }
}

pub fn get_type_default_as_statement_from_statement(type_: &Statement) -> Statement {
    let binding = type_.convert_to_hashmap();
    let default = Value::String("any".to_string());

    let type_name = binding.get(&Value::String("value".to_string())).unwrap_or(&default);

    match type_name {
        Value::String(type_str) => get_type_default_as_statement(&type_str),
        _ => get_type_default_as_statement("any"),
    }
}

pub fn get_imagnum_error_message(err: i16) -> String {
    match err {
        ERR_DIV_BY_ZERO => "Division by zero.".to_string(),
        ERR_NEGATIVE_RESULT => "Negative result.".to_string(),
        ERR_NEGATIVE_SQRT => "Square root of a negative number.".to_string(),
        ERR_NUMBER_TOO_LARGE => "Number too large to convert.".to_string(),
        ERR_INFINITE_RESULT => "Result is infinite.".to_string(),
        ERR_UNIMPLEMENTED => "This operation is not implemented.".to_string(),
        ERR_INVALID_FORMAT => "Invalid format.".to_string(),
        _ => "Unknown error.".to_string(),
    }
}

pub fn create_function(metadata: FunctionMetadata, body: Vec<Statement>) -> Value {
    Value::Function(Function::Custom(UserFunction {
        meta: metadata,
        body,
    }.into()))
}

pub fn create_note(text: &str, use_colors: Option<bool>, note_color: &str) -> String {
    let use_colors = use_colors.unwrap_or(false);
    format!(
        "{}{}Note:{} {}{}",
        hex_to_ansi(note_color, use_colors),
        check_ansi("\x1b[1m", &use_colors),
        check_ansi("\x1b[22m", &use_colors),
        text,
        hex_to_ansi("reset", use_colors)
    )
}

pub fn get_type_from_token_name(token_name: &str) -> String {
    match token_name {
        "NUMBER" => "int".to_string(),
        "FLOAT" => "float".to_string(),
        "STRING" => "string".to_string(),
        "BOOLEAN" => "bool".to_string(),
        "MAP" => "map".to_string(),
        "LIST" => "list".to_string(),
        "TUPLE" => "tuple".to_string(),
        _ => "unknown".to_string(),
    }
}

pub fn unescape_string_literal(s: &str) -> Result<String, String> {
    let quote_start = s.find(|c| c == '"' || c == '\'')
        .ok_or_else(|| "No starting quote found".to_string())?;

    let sliced = &s[quote_start..];
    unescape_string(sliced)
}

pub fn replace_accented(c: char) -> char {
    match c {
        'á' | 'à' | 'ä' | 'â' | 'ã' | 'å' | 'ā' => 'a',
        'Á' | 'À' | 'Ä' | 'Â' | 'Ã' | 'Å' | 'Ā' => 'A',
        'é' | 'è' | 'ë' | 'ê' | 'ē' => 'e',
        'É' | 'È' | 'Ë' | 'Ê' | 'Ē' => 'E',
        'í' | 'ì' | 'ï' | 'î' | 'ī' => 'i',
        'Í' | 'Ì' | 'Ï' | 'Î' | 'Ī' => 'I',
        'ó' | 'ò' | 'ö' | 'ô' | 'õ' | 'ō' => 'o',
        'Ó' | 'Ò' | 'Ö' | 'Ô' | 'Õ' | 'Ō' => 'O',
        'ú' | 'ù' | 'ü' | 'û' | 'ū' => 'u',
        'Ú' | 'Ù' | 'Ü' | 'Û' | 'Ū' => 'U',
        'ç' => 'c',
        'Ç' => 'C',
        'ñ' => 'n',
        'Ñ' => 'N',
        _ => c,
    }
}

fn strip_extension(name: &str) -> &str {
    if name.ends_with(".lc") {
        &name[..name.len() - 3]
    } else if name.ends_with(".lucia") {
        &name[..name.len() - 6]
    } else if name.ends_with(".rs") {
        &name[..name.len() - 3]
    } else {
        name
    }
}

pub fn sanitize_alias(alias: &str) -> String {
    let alias = strip_extension(alias);

    let mut result = String::new();
    let mut chars = alias.chars();

    if let Some(first) = chars.next() {
        let first = replace_accented(first);
        if first.is_ascii_alphabetic() || first == '_' {
            result.push(first);
        } else if first.is_ascii_digit() {
            result.push('_');
            result.push(first);
        } else {
            result.push('_');
        }
    }

    for c in chars {
        let c = replace_accented(c);
        if c.is_ascii_alphanumeric() || c == '_' {
            result.push(c);
        } else {
            result.push('_');
        }
    }

    if result.is_empty() {
        "_".to_string()
    } else {
        result
    }
}

pub fn special_function_meta() -> HashMap<String, FunctionMetadata> {
    let mut map = HashMap::new();

    map.insert(
        "exit".to_string(),
        FunctionMetadata {
            name: "exit".to_string(),
            parameters: vec![Parameter::positional_optional("code", "int", Value::Int(Int::from_i64(0 as i64)))],
            return_type: Type::new_simple("void"),
            is_public: true,
            is_static: true,
            is_final: true,
            is_native: true,
            state: None,
        },
    );
    map.insert(
        "fetch".to_string(),
        FunctionMetadata {
            name: "fetch".to_string(),
            parameters: vec![
                Parameter::positional("url", "str"),
                Parameter::positional_optional("method", "str", Value::String("GET".to_string())),
                Parameter::positional_optional("headers", "map", Value::Map { keys: vec![], values: vec![] }),
                Parameter::positional_optional("params", "map", Value::Map { keys: vec![], values: vec![] }),
                Parameter::positional_optional("data", "map", Value::Map { keys: vec![], values: vec![] }),
                Parameter::positional_optional("json", "any", NULL),
            ],
            return_type: Type::new_simple("map"),
            is_public: true,
            is_static: true,
            is_final: true,
            is_native: true,
            state: None,
        }
    );
    map.insert(
        "eval".to_string(),
        FunctionMetadata {
            name: "eval".to_string(),
            parameters: vec![Parameter::positional("code", "str")],
            return_type: Type::new_simple("any"),
            is_public: true,
            is_static: true,
            is_final: true,
            is_native: true,
            state: None,
        },
    );
    map.insert(
        "exec".to_string(),
        FunctionMetadata {
            name: "exec".to_string(),
            parameters: vec![Parameter::positional("code", "str")],
            return_type: Type::new_simple("any"),
            is_public: true,
            is_static: true,
            is_final: true,
            is_native: true,
            state: None,
        },
    );
    map.insert(
        "00__set_cfg__".to_string(),
        FunctionMetadata {
            name: "00__set_cfg__".to_string(),
            parameters: vec![
                Parameter::positional("key", "str"),
                Parameter::positional("value", "any"),
            ],
            return_type: Type::new_simple("void"),
            is_public: true,
            is_static: true,
            is_final: true,
            is_native: true,
            state: None,
        },
    );
    map.insert(
        "00__set_dir__".to_string(),
        FunctionMetadata {
            name: "00__set_dir__".to_string(),
            parameters: vec![
                Parameter::positional("d", "str"),
                Parameter::positional("i", "bool"),
            ],
            return_type: Type::new_simple("void"),
            is_public: true,
            is_static: true,
            is_final: true,
            is_native: true,
            state: None,
        },
    );

    return map;
}

pub fn deep_get<'a>(root: &'a Value, path: &[Value]) -> Option<&'a Value> {
    let mut cur = root;
    for key in path {
        match cur {
            Value::Map { keys, values } => {
                let i = keys.iter().position(|k| k == key)?;
                cur = &values[i];
            }
            _ => return None,
        }
    }
    Some(cur)
}

pub fn deep_insert(root: &mut Value, path: &[Value], val: Value) -> bool {
    let mut cur = root;
    for (i, key) in path.iter().enumerate() {
        match cur {
            Value::Map { keys, values } => {
                if let Some(pos) = keys.iter().position(|k| k == key) {
                    if i + 1 == path.len() {
                        values[pos] = val.clone();
                        return true;
                    } else {
                        cur = &mut values[pos];
                    }
                } else {
                    keys.push(key.clone());
                    let new_val = if i + 1 == path.len() {
                        val.clone()
                    } else {
                        Value::Map { keys: vec![], values: vec![] }
                    };
                    values.push(new_val);
                    if i + 1 == path.len() {
                        return true;
                    } else {
                        cur = values.last_mut().unwrap();
                    }
                }
            }
            _ => return false,
        }
    }
    false
}

fn version_to_tuple(v: &str) -> Option<(u64, u64, u64)> {
    let parts: Vec<&str> = v.split('.').collect();
    if parts.len() != 3 {
        return None;
    }
    let major = parts[0].parse().ok()?;
    let minor = parts[1].parse().ok()?;
    let patch = parts[2].parse().ok()?;
    Some((major, minor, patch))
}

fn cmp_version(a: (u64,u64,u64), b: (u64,u64,u64)) -> std::cmp::Ordering {
    if a.0 != b.0 {
        return a.0.cmp(&b.0);
    }
    if a.1 != b.1 {
        return a.1.cmp(&b.1);
    }
    a.2.cmp(&b.2)
}

pub fn check_version(current: &str, required: &str) -> bool {
    if required.is_empty() {
        return true;
    }

    let cur = match version_to_tuple(current) {
        Some(v) => v,
        None => return false,
    };

    let ge = |a, b| cmp_version(a, b) != std::cmp::Ordering::Less;
    let gt = |a, b| cmp_version(a, b) == std::cmp::Ordering::Greater;
    let le = |a, b| cmp_version(a, b) != std::cmp::Ordering::Greater;
    let lt = |a, b| cmp_version(a, b) == std::cmp::Ordering::Less;
    let eq = |a, b| cmp_version(a, b) == std::cmp::Ordering::Equal;

    if required.starts_with('^') {
        let req_str = &required[1..];
        let req = match version_to_tuple(req_str) {
            Some(v) => v,
            None => return false,
        };
        let upper = (req.0 + 1, 0, 0);
        return ge(cur, req) && lt(cur, upper);
    } else if required.starts_with(">=") {
        let req_str = &required[2..];
        let req = match version_to_tuple(req_str) {
            Some(v) => v,
            None => return false,
        };
        return ge(cur, req);
    } else if required.starts_with("<=") {
        let req_str = &required[2..];
        let req = match version_to_tuple(req_str) {
            Some(v) => v,
            None => return false,
        };
        return le(cur, req);
    } else if required.starts_with('<') {
        let req_str = &required[1..];
        let req = match version_to_tuple(req_str) {
            Some(v) => v,
            None => return false,
        };
        return lt(cur, req);
    } else if required.starts_with('>') {
        let req_str = &required[1..];
        let req = match version_to_tuple(req_str) {
            Some(v) => v,
            None => return false,
        };
        return gt(cur, req);
    } else if required.starts_with('~') {
        let req_str = &required[1..];
        let req = match version_to_tuple(req_str) {
            Some(v) => v,
            None => return false,
        };
        let upper = (req.0, req.1 + 1, 0);
        return ge(cur, req) && lt(cur, upper);
    } else {
        let req = match version_to_tuple(required) {
            Some(v) => v,
            None => return false,
        };
        return eq(cur, req);
    }
}

pub fn fix_path(raw_path: String) -> String {
    let path = raw_path.trim();
    if path.is_empty() {
        return String::new();
    }
    if path.starts_with('/') {
        return path.to_string();
    }
    if path.starts_with("./") {
        return path[2..].to_string();
    }
    if path.starts_with(r"\\?\") {
        return path[4..].to_string();
    }
    path.to_string().replace("\\", "/")
        .replace("//", "/")
        .replace(":/", "://")
        .to_string()
}

fn remove_note_field(input: &str) -> String {
    let re = regex::Regex::new(r#"(?s),?\s*_note:\s*[^,}]+,?"#).unwrap();
    let result = re.replace_all(input, "");
    result.trim().trim_start_matches(',').trim().to_string()
}

pub fn fix_and_parse_json(input: &str) -> Option<JsonValue> {
    let no_note = remove_note_field(input);

    let re_unicode = regex::Regex::new(r#"\\u\{([0-9a-fA-F]{1,4})\}"#).unwrap();
    let fixed_unicode = re_unicode.replace_all(&no_note, |caps: &regex::Captures| {
        format!("\\u{:0>4}", caps[1].to_uppercase())
    });

    let re_keys = regex::Regex::new(r#"(?P<prefix>[\{\s,])(?P<key>[a-zA-Z_][\w]*)\s*:"#).unwrap();
    let fixed_keys = re_keys.replace_all(&fixed_unicode, r#"$prefix"$key":"#);

    let re_unquoted_vals = regex::Regex::new(r#":\s*([\?&]?[a-zA-Z_][\w]*)"#).unwrap();
    let mut fully_fixed = re_unquoted_vals.replace_all(&fixed_keys, |caps: &regex::Captures| {
        let val = &caps[1];
        if val == "true" || val == "false" || val == "null" {
            format!(": {}", val)
        } else {
            format!(": \"{}\"", val)
        }
    }).to_string();

    let re_unquoted_in_array = regex::Regex::new(r#"(\[|\s*,\s*)([\?&]?[a-zA-Z_][\w]*)"#).unwrap();
    loop {
        let new_fixed = re_unquoted_in_array.replace_all(&fully_fixed, |caps: &regex::Captures| {
            format!("{}\"{}\"", &caps[1], &caps[2])
        }).to_string();
        if new_fixed == fully_fixed {
            break;
        }
        fully_fixed = new_fixed;
    }

    serde_json::from_str(&fully_fixed).ok()
}

fn json_object_to_value_map(obj: &serde_json::Map<String, JsonValue>) -> (Vec<Value>, Vec<Value>) {
    let mut keys = Vec::new();
    let mut values = Vec::new();

    for (k, v) in obj {
        keys.push(Value::String(k.clone()));
        values.push(json_to_value(v));
    }

    (keys, values)
}

pub fn json_to_value(v: &JsonValue) -> Value {
    match v {
        JsonValue::String(s) => Value::String(s.clone()),
        JsonValue::Number(n) => {
            if let Some(i) = n.as_i64() {
                Value::Int(i.into())
            } else if let Some(f) = n.as_f64() {
                Value::Float(f.into())
            } else {
                Value::Null
            }
        }
        JsonValue::Bool(b) => Value::Boolean(*b),
        JsonValue::Array(arr) => {
            let list = arr.iter().map(json_to_value).collect();
            Value::List(list)
        }
        JsonValue::Object(map) => {
            let (k, v) = json_object_to_value_map(map);
            Value::Map { keys: k, values: v }
        }
        JsonValue::Null => Value::Null,
    }
}

pub fn remove_loc_keys(value: &Value) -> Value {
    match value {
        Value::Map { keys, values } => {
            let filtered: Vec<(Value, Value)> = keys.iter()
                .zip(values.iter())
                .filter_map(|(k, v)| {
                    if let Value::String(s) = k {
                        if s == "_loc" {
                            return None;
                        }
                    }
                    Some((k.clone(), remove_loc_keys(v)))
                })
                .collect();

            let (new_keys, new_values): (Vec<_>, Vec<_>) = filtered.into_iter().unzip();

            Value::Map {
                keys: new_keys,
                values: new_values,
            }
        }
        Value::List(items) => {
            Value::List(items.iter().map(remove_loc_keys).collect())
        }
        Value::Tuple(items) => {
            Value::Tuple(items.iter().map(remove_loc_keys).collect())
        }
        _ => value.clone(),
    }
}

pub fn char_to_digit(c: char) -> Option<u32> {
    match c {
        '0'..='9' => Some(c as u32 - '0' as u32),
        'a'..='z' => Some(c as u32 - 'a' as u32 + 10),
        'A'..='Z' => Some(c as u32 - 'A' as u32 + 36),
        _ => None,
    }
}

pub fn get_precedence(op: &str) -> u8 {
    match op {
        "^^^" => 10,
        "^^" => 9,
        "++" | "--" | "!" | "~" | "bnot" | "not" => 9,
        "^" => 8,
        "*" | "/" | "%" | "*=" | "/=" | "%=" => 7,
        "+" | "-" | "+=" | "-=" | "^=" => 6,
        "<<" | ">>" | "lshift" | "rshift" => 5,
        "&" | "band" => 4,
        "xor" => 3,
        "|" | "bor" => 2,
        ">" | "<" | ">=" | "<=" | "==" | "!="
        | "is" | "isnt" | "isn't" | "nein" => 1,
        "&&" | "and" => 0,
        "||" | "or" => 0,
        _ => 0,
    }
}

pub fn unique_temp_name(suffix: &str, home_dir: &Path) -> PathBuf {
    let now = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_millis();
    let temp_dir = home_dir.join("transpiler").join("temp");
    std::fs::create_dir_all(&temp_dir).ok();
    temp_dir.join(format!("lucia_temp_{now}.{suffix}"))
}

pub fn convert_value_to_type(
    target_type: &str,
    value: &Value,
) -> Result<Value, (&'static str, &'static str, &'static str)> {
    match target_type {
        "str" => Ok(match value {
            Value::String(s) => Value::String(s.clone()),
            _ => Value::String(value.to_string()),
        }),

        "int" => {
            if let Value::Int(i) = value {
                Ok(Value::Int(i.clone()))
            } else if let Value::Float(f) = value {
                if f.is_integer_like() {
                    f.to_int()
                        .map_or_else(
                            |_| Err(("ConversionError", "Failed to convert float to int", "")),
                            |i| Ok(Value::Int(i)),
                        )
                } else {
                    let rounded = f.round(0);
                    rounded
                        .to_int()
                        .map_or_else(
                            |_| Err(("ConversionError", "Failed to convert rounded float to int", "")),
                            |i| Ok(Value::Int(i)),
                        )
                }
            } else if let Value::String(s) = value {
                match u128::from_str(s) {
                    Ok(num) => Ok(Value::Int(Int::from_str(&num.to_string()).unwrap())),
                    Err(_) => Err(("ConversionError", "Failed to convert string to int", "Ensure string is a valid unsigned integer")),
                }
            } else if let Value::Boolean(b) = value {
                Ok(if *b {
                    Value::Int(Int::from_i64(1))
                } else {
                    Value::Int(Int::from_i64(0))
                })
            } else {
                Err(("TypeError", "Cannot convert value to int", ""))
            }
        }

        "float" => {
            if let Value::Float(f) = value {
                Ok(Value::Float(f.clone()))
            } else if let Value::Int(i) = value {
                i.to_float()
                    .map_or_else(
                        |_| Err(("ConversionError", "Failed to convert int to float", "")),
                        |f| Ok(Value::Float(f)),
                    )
            } else if let Value::String(s) = value {
                Float::from_str(&s)
                    .map_or_else(
                        |_| Err(("ConversionError", "Failed to convert string to float", "Ensure valid float")),
                        |f| Ok(Value::Float(f)),
                    )
            } else {
                Err(("TypeError", "Cannot convert value to float", ""))
            }
        }

        "bool" => Ok(if value.is_truthy() {
            Value::Boolean(true)
        } else {
            Value::Boolean(false)
        }),

        "void" => {
            if value.is_null() {
                Ok(NULL)
            } else {
                Err(("TypeError", "Cannot convert non-null value to void", ""))
            }
        }

        "any" => Ok(value.clone()),

        "list" => match value {
            Value::List(l) => Ok(Value::List(l.clone())),
            Value::Tuple(t) => Ok(Value::List(t.iter().cloned().collect())),
            Value::String(s) => Ok(Value::List(s.chars().map(|c| Value::String(c.to_string())).collect())),
            _ => Err(("TypeError", "Cannot convert value to list", "")),
        },

        "map" => match value {
            Value::Map { keys, values } => Ok(Value::Map {
                keys: keys.clone(),
                values: values.clone(),
            }),
            _ => Err(("TypeError", "Cannot convert value to map", "")),
        },

        "bytes" => match value {
            Value::Bytes(b) => Ok(Value::Bytes(b.clone())),
            Value::String(s) => Ok(Value::Bytes(s.clone().into_bytes())),
            _ => Err(("TypeError", "Cannot convert value to bytes", "")),
        },

        "function" => match value {
            Value::Function(func) => Ok(Value::Function(func.clone())),
            _ => Err(("TypeError", "Cannot convert value to function", "")),
        },

        "tuple" => match value {
            Value::Tuple(t) => Ok(Value::Tuple(t.clone())),
            Value::List(l) => Ok(Value::Tuple(l.iter().cloned().collect())),
            Value::String(s) => Ok(Value::Tuple(s.chars().map(|c| Value::String(c.to_string())).collect())),
            _ => Err(("TypeError", "Cannot convert value to tuple", "")),
        },

        _ => Err(("NotImplemented", "Type conversion not implemented", "")),
    }
}

pub fn ctrl_t_pressed() -> bool {
    if event::poll(Duration::from_millis(0)).unwrap_or(false) {
        if let event::Event::Key(event::KeyEvent { code, modifiers, .. }) = event::read().unwrap() {
            if code == event::KeyCode::Char('t') && modifiers.contains(event::KeyModifiers::CONTROL) {
                return true;
            }
        }
    }
    false
}

pub fn wrap_in_help(text: &str, use_colors: bool, config: &Config) -> String {
    format!(
        "{}{}{}",
        hex_to_ansi(&config.color_scheme.note, use_colors),
        text,
        hex_to_ansi(&config.color_scheme.help, use_colors)
    )
}

pub fn parse_type(type_str: &str) -> Type {
    if type_str.is_empty() {
        return Type::new_simple("any");
    }
    if VALID_TYPES.contains(&type_str) {
        return Type::new_simple(type_str);
    }
    let result = interpret(type_str);
    if result.is_err() {
        return Type::new_simple("any");
    }
    if let Value::Type(t) = result.unwrap() {
        return t;
    }
    Type::new_simple("any")
}

static NUMBER_REGEX: Lazy<Regex> = Lazy::new(|| {
    let number_pattern = r"(?x)
        -?
        (
            \d+\#[0-9a-zA-Z_]+
            | 0[bB][01]+(?:_[01]+)*
            | 0[oO][0-7]+(?:_[0-7]+)*
            | 0[xX][\da-fA-F]+(?:_[\da-fA-F]+)*
            | \.\d+(?:_\d+)*
            |
            (?:\d+(?:_\d)* 
                (?:\.\d+(?:_\d+)*)?
            )
            (?:[eE][+-]?\d+)?
        )
    ";

    Regex::new(number_pattern).unwrap()
});

pub fn is_number(n: &str) -> bool {
    NUMBER_REGEX.is_match(n)
}

pub const KEYWORDS: &[&str] = &[
    "fun", "gen", "return", "export", "throw", "end", "catch", "try", "static", "non-static",
    "public", "private", "final", "mutable", "if", "else", "then", "for",
    "while", "as", "from", "import", "in", "forget", "and", "or", "not",
    "isnt", "is", "xor", "xnor", "nein", "match", "break", "continue",
    "defer", "scope", "pass", "band", "lshift", "rshift", "bor", "bnot",
    "type", "where", "true", "false", "null", "void", "any", "int",
    "float", "bool", "str", "map", "list", "function", "bytes", "tuple",
    "auto", "generator"
];

pub const NULL: Value = Value::Null;
pub const TRUE: Value = Value::Boolean(true);
pub const FALSE: Value = Value::Boolean(false);
