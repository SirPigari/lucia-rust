use std::io;
#[cfg(not(target_arch = "wasm32"))]
use std::io::{stdout, Write};
use std::collections::HashMap;
#[cfg(not(target_arch = "wasm32"))]
use std::fs;
use crate::env::runtime::config::{Config};
use crate::env::runtime::functions::Function;
use once_cell::sync::Lazy;
use std::sync::{Mutex, Arc};
use crate::env::runtime::functions::{Parameter, NativeMethod, FunctionMetadata, UserFunction};
use crate::env::runtime::statements::Statement;
use crate::env::runtime::structs_and_enums::Struct;
use crate::env::runtime::types::{Int, Float, Type};
use crate::env::runtime::value::{Value};
use std::str::FromStr;
#[cfg(not(target_arch = "wasm32"))]
use std::time::Duration;
use crate::env::runtime::types::VALID_TYPES;
use crate::env::runtime::precompile::interpret;
use crate::env::runtime::tokens::Token;
use crate::env::runtime::native;
use crate::env::runtime::internal_structs::EffectFlags;

pub use imagnum::errors::get_error_message as get_imagnum_error_message;

#[cfg(not(target_arch = "wasm32"))]
use crossterm::{
    execute,
    terminal::{Clear, ClearType},
    cursor::MoveTo,
    event,
};

static STATIC_STORAGE: Lazy<Mutex<HashMap<String, &'static str>>> = Lazy::new(|| Mutex::new(HashMap::default()));

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
#[cfg(not(target_arch = "wasm32"))]
pub fn supports_color() -> bool {
    use std::io::IsTerminal;
    let is_tty = std::io::stdout().is_terminal();
    let term = std::env::var("TERM").unwrap_or_default();
    let not_dumb = term != "dumb";

    is_tty && not_dumb
}

#[cfg(not(any(unix, windows)))]
#[cfg(not(target_arch = "wasm32"))]
pub fn supports_color() -> bool {
    false
}

#[cfg(target_arch = "wasm32")]
pub fn supports_color() -> bool {
    true
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
#[cfg(not(target_arch = "wasm32"))]
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

#[cfg(not(target_arch = "wasm32"))]
pub fn get_line_info(file_path: &str, line_number: usize) -> Option<String> {
    let source = match fs::read_to_string(file_path) {
        Ok(content) => content,
        Err(_) => return None,
    };
    source.lines().nth(line_number.saturating_sub(1)).map(|s| s.to_string())
}

#[cfg(target_arch = "wasm32")]
pub fn get_line_info(source: &str, line_number: usize) -> Option<String> {
    source.lines().nth(line_number.saturating_sub(1)).map(|s| s.to_string())
}

#[cfg(target_arch = "wasm32")]
pub fn clear_terminal() -> Result<(), io::Error> {
    clear!();
    Ok(())
}

#[cfg(not(target_arch = "wasm32"))]
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

static COLOR_MAP: Lazy<HashMap<&'static str, &'static str>> = Lazy::new(|| {
    let mut m = HashMap::new();
    m.insert("black", "\x1b[30m");
    m.insert("red", "\x1b[31m");
    m.insert("green", "\x1b[32m");
    m.insert("yellow", "\x1b[33m");
    m.insert("blue", "\x1b[34m");
    m.insert("magenta", "\x1b[35m");
    m.insert("cyan", "\x1b[36m");
    m.insert("white", "\x1b[37m");
    m.insert("gray", "\x1b[90m");
    m.insert("bright_black", "\x1b[90m");
    m.insert("bright_red", "\x1b[91m");
    m.insert("bright_green", "\x1b[92m");
    m.insert("bright_yellow", "\x1b[93m");
    m.insert("bright_blue", "\x1b[94m");
    m.insert("bright_magenta", "\x1b[95m");
    m.insert("bright_cyan", "\x1b[96m");
    m.insert("bright_white", "\x1b[97m");
    m.insert("reset", "\x1b[0m");
    m
});

pub fn hex_to_ansi(hex_color: &str, use_colors: bool) -> String {
    if !use_colors {
        return "".to_string();
    }

    if let Some(ansi) = COLOR_MAP.get(hex_color.to_lowercase().replace(' ', "_").as_str()) {
        return ansi.to_string();
    }

    let hex = if hex_color.starts_with('#') { &hex_color[1..] } else if hex_color.starts_with("0x") { &hex_color[2..] } else { hex_color };

    if hex.len() == 6 {
        let r = u8::from_str_radix(&hex[0..2], 16).unwrap();
        let g = u8::from_str_radix(&hex[2..4], 16).unwrap();
        let b = u8::from_str_radix(&hex[4..6], 16).unwrap();
        return format!("\x1b[38;2;{};{};{}m", r, g, b);
    }

    "\x1b[0m".to_string()
}

pub fn print_colored(message: &str, color: &str, use_colors: bool) {
    let colored_message = format!("{}{}{}", hex_to_ansi(color, use_colors), message, hex_to_ansi("reset", use_colors));
    println!("{}", colored_message);
}

#[cfg(not(target_arch = "wasm32"))]
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
        Value::String(s) => format!("\"{}\"", s.replace('\\', "\\\\").replace('\"', "\\\"")),
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

        Value::Module(obj) => {
            let addr = obj.ptr() as *const () as usize;
            format!("<module '{}' from '{}' at 0x{:X}>", obj.name(), fix_path(obj.path().display().to_string()), addr)
        }

        Value::Enum(enm) => enm.display(),
        Value::Struct(strct) => strct.display(),

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

pub fn debug_log(message: &str, config: &Config) {
    if config.debug && (config.debug_mode == "full" || config.debug_mode == "normal") {
        let single_line_message = message
            .replace('\n', "\\n")
            .replace('\r', "\\r")
            .replace('\t', "\\t")
            .replace('\0', "\\0")
            .replace('\x1b', "\\e")
            .replace(r"\A", "\n");
        print_colored(&single_line_message, &config.color_scheme.debug, config.supports_color);
    }
}

pub fn capitalize(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
        None => String::new(),
    }
}

// pub fn unescape_string_full(s: &str) -> Result<String, String> {
//     let wrapped = format!("\"{}\"", s);
//     serde_json::from_str::<String>(&wrapped)
//         .map_err(|e| format!("Failed to fully unescape: {}", e))
// }

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

#[cfg(not(target_arch = "wasm32"))]
pub fn get_type_from_statement(stmt: &Statement) -> Option<String> {
    let map = stmt.convert_to_hashmap();
    match map.get(&Value::String("type".to_string())) {
        Some(Value::String(type_str)) if type_str == "TYPE" => {
            match map.get(&Value::String("value".to_string())) {
                Some(Value::String(inner_type)) => Some(inner_type.clone()),
                _ => None,
            }
        }
        _ => None,
    }
}

pub fn get_inner_string(s: &str) -> Result<String, String> {
    let s = s.trim();

    if s.is_empty() {
        return Err("empty string".into());
    }

    let mut chars = s.chars();
    while let Some(c) = chars.as_str().chars().next() {
        if ['f', 'b', 'r'].contains(&c) {
            chars.next();
        } else {
            break;
        }
    }

    let remaining = chars.as_str();

    if remaining.len() < 2 {
        return Err("not a quoted string".into());
    }

    let first_char = remaining.chars().next().unwrap();
    let last_char = remaining.chars().last().unwrap();

    if !['"', '\''].contains(&first_char) || first_char != last_char {
        return Err("not a valid quoted string".into());
    }

    Ok(remaining[1..remaining.len()-1].to_string())
}

pub fn unescape_string_premium_edition(s: &str) -> Result<String, String> {
    let mut result = String::new();
    let mut chars = s.chars().peekable();

    while let Some(c) = chars.next() {
        if c != '\\' {
            result.push(c);
            continue;
        }

        // We have a backslash
        let next_c = chars.next().ok_or("Trailing backslash in string literal")?;
        match next_c {
            '\\' => result.push('\\'),
            '\'' => result.push('\''),
            '"' => result.push('"'),
            'n' => result.push('\n'),
            'r' => result.push('\r'),
            't' => result.push('\t'),
            '0' => result.push('\0'),
            'a' => result.push('\x07'),
            'b' => result.push('\x08'),
            'f' => result.push('\x0c'),
            'v' => result.push('\x0b'),
            'x' => {
                // Hex escape \xNN
                let hex: String = chars.by_ref().take(2).collect();
                if hex.len() != 2 { return Err(format!("Invalid hex escape: \\x{}", hex)); }
                let val = u8::from_str_radix(&hex, 16)
                    .map_err(|_| format!("Invalid hex escape: \\x{}", hex))?;
                result.push(val as char);
            }
            'u' => {
                // Unicode escape \uNNNN
                let hex: String = chars.by_ref().take(4).collect();
                if hex.len() != 4 { return Err(format!("Invalid unicode escape: \\u{}", hex)); }
                let val = u32::from_str_radix(&hex, 16)
                    .map_err(|_| format!("Invalid unicode escape: \\u{}", hex))?;
                result.push(std::char::from_u32(val).ok_or("Invalid unicode codepoint")?);
            }
            'U' => {
                // Unicode escape \UNNNNNNNN
                let hex: String = chars.by_ref().take(8).collect();
                if hex.len() != 8 { return Err(format!("Invalid unicode escape: \\U{}", hex)); }
                let val = u32::from_str_radix(&hex, 16)
                    .map_err(|_| format!("Invalid unicode escape: \\U{}", hex))?;
                result.push(std::char::from_u32(val).ok_or("Invalid unicode codepoint")?);
            }
            '0'..='7' => {
                // Octal escape \NNN (up to 3 digits)
                let mut oct = next_c.to_string();
                for _ in 0..2 {
                    if let Some(&next_digit) = chars.peek() {
                        if next_digit >= '0' && next_digit <= '7' {
                            oct.push(chars.next().unwrap());
                        } else {
                            break;
                        }
                    }
                }
                let val = u8::from_str_radix(&oct, 8)
                    .map_err(|_| format!("Invalid octal escape: \\{}", oct))?;
                result.push(val as char);
            }
            other => return Err(format!("Unknown escape sequence: \\{}", other)),
        }
    }

    Ok(result)
}

pub fn make_native_method<F>(
    name: &str,
    func: F,
    parameters: Vec<Parameter>,
    return_type: &str,
    is_public: bool,
    is_static: bool,
    is_final: bool,
    state: Option<String>
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
            effects: EffectFlags::UNKNOWN,
        },
    };

    Value::Function(Function::NativeMethod(Arc::new(method)))
}

pub fn get_type_default(type_: &str) -> Value {
    if type_.starts_with("&") {
        let inner_type = &type_[1..];
        let default_val = get_type_default(inner_type);

        let arc = Arc::new(Mutex::new(default_val));

        return Value::Pointer(arc);
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

pub fn create_function(metadata: FunctionMetadata, body: Vec<Statement>) -> Value {
    Value::Function(Function::Custom(UserFunction {
        meta: metadata,
        body,
    }.into()))
}

pub fn parse_usize_radix(value: &str) -> Option<usize> {
    if let Some(stripped) = value.strip_prefix("0b") {
        usize::from_str_radix(stripped, 2).ok()
    } else if let Some(stripped) = value.strip_prefix("0o") {
        usize::from_str_radix(stripped, 8).ok()
    } else if let Some(stripped) = value.strip_prefix("0x") {
        usize::from_str_radix(stripped, 16).ok()
    } else {
        match value.parse::<f64>() {
            Ok(f) if f.fract() == 0.0 && f >= 0.0 => Some(f as usize),
            _ => None,
        }
    }
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
        "TYPE" => "type".to_string(),
        "POINTER_REF" => "pointer".to_string(),
        t => t.to_string(),
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
    let mut map = HashMap::default();

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
            effects: EffectFlags::IO,
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
            effects: EffectFlags::IO,
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
            effects: EffectFlags::UNKNOWN,
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
            effects: EffectFlags::UNKNOWN,
        },
    );
    map.insert(
        "warn".to_string(),
        FunctionMetadata {
            name: "warn".to_string(),
            parameters: vec![Parameter::positional("message", "str")],
            return_type: Type::new_simple("any"),
            is_public: true,
            is_static: true,
            is_final: true,
            is_native: true,
            state: None,
            effects: EffectFlags::IO,
        },
    );
    map.insert(
        "as_method".to_string(),
        FunctionMetadata {
            name: "as_method".to_string(),
            parameters: vec![Parameter::positional("func", "function")],
            return_type: Type::new_simple("any"),
            is_public: true,
            is_static: true,
            is_final: true,
            is_native: true,
            state: None,
            effects: EffectFlags::PURE,
        },
    );
    map.insert(
        "module".to_string(),
        FunctionMetadata {
            name: "module".to_string(),
            parameters: vec![Parameter::positional("path", "str")],
            return_type: Type::new_simple("module"),
            is_public: true,
            is_static: true,
            is_final: true,
            is_native: true,
            state: None,
            effects: EffectFlags::IO,
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
            effects: EffectFlags::STATE,
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
            effects: EffectFlags::STATE,
        },
    );

    return map;
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
        .replace("\\\\", "/")
        .replace("//", "/")
        .replace(":/", "://")
        .to_string()
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

pub fn is_valid_token(token: &Option<Token>) -> bool {
    match token {
        Some(t) => match t.0.as_str() {
            "EOF" | "EOL" => false,
            _ => true,
        },
        None => false,
    }
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

#[cfg(not(target_arch = "wasm32"))]
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

pub fn wrap_in_help(text: &str, config: &Config) -> String {
    format!(
        "{}{}{}",
        hex_to_ansi(&config.color_scheme.note, config.supports_color),
        text,
        hex_to_ansi(&config.color_scheme.help, config.supports_color)
    )
}

pub fn parse_type(type_str: &str) -> Type {
    if type_str.is_empty() {
        return Type::new_simple("any");
    }
    if VALID_TYPES.contains(&type_str) {
        return Type::new_simple(type_str);
    }
    if type_str.contains('|') {
        let parts: Vec<&str> = type_str.split('|').map(|s| s.trim()).collect();
        let types: Vec<Type> = parts.iter().map(|&part| parse_type(part)).collect();
        return Type::new_union(types);
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

pub fn is_number(n: &str) -> bool {
    let s = n;
    let len = s.len();
    if len == 0 {
        return false;
    }

    if is_number_parentheses(n) {
        return true;
    }

    let mut chars = s.char_indices().peekable();
    let mut rel_end = 0;

    if let Some(&(i, c)) = chars.peek() {
        if c == '-' {
            rel_end = i + c.len_utf8();
            chars.next();
        }
    }

    fn consume_while<F>(chars: &mut std::iter::Peekable<std::str::CharIndices>, rel_end: &mut usize, mut pred: F)
    where F: FnMut(char) -> bool {
        while let Some(&(i, c)) = chars.peek() {
            if pred(c) {
                *rel_end = i + c.len_utf8();
                chars.next();
            } else {
                break;
            }
        }
    }

    {
        let mut tmp = chars.clone();
        let mut tmp_rel_end = rel_end;
        let mut matched = false;

        if let Some(&(_, c)) = tmp.peek() {
            if c.is_ascii_digit() {
                consume_while(&mut tmp, &mut tmp_rel_end, |ch| ch.is_ascii_digit());
                if let Some(&(i2, '#')) = tmp.peek() {
                    tmp.next();
                    tmp_rel_end = i2 + 1;
                    consume_while(&mut tmp, &mut tmp_rel_end, |ch| ch.is_ascii_alphanumeric() || ch == '_');
                    matched = true;
                }
            }
        }

        if matched {
            return tmp_rel_end == len;
        }
    }

    if let Some(&(_, '0')) = chars.peek() {
        let mut tmp = chars.clone();
        tmp.next();
        if let Some(&(i2, base)) = tmp.peek() {
            let digits = match base {
                'b' | 'B' => "01_",
                'o' | 'O' => "01234567_",
                'x' | 'X' => "0123456789abcdefABCDEF_",
                _ => "",
            };
            if !digits.is_empty() {
                tmp.next();
                let mut tmp_rel_end = i2 + base.len_utf8();
                consume_while(&mut tmp, &mut tmp_rel_end, |ch| digits.contains(ch));
                return tmp_rel_end == len;
            }
        }
    }

    let mut has_digits = false;
    let mut dot_seen = false;
    let mut in_exponent = false;
    let mut last_char: Option<char> = None;

    while let Some(&(i, c)) = chars.peek() {
        let accept = match c {
            '0'..='9' => { has_digits = true; true }
            '_' => true,
            '.' => {
                if dot_seen || in_exponent {
                    false
                } else if let Some((_, next_ch)) = chars.clone().nth(1) {
                    if next_ch == '.' {
                        false
                    } else {
                        dot_seen = true;
                        true
                    }
                } else {
                    dot_seen = true;
                    true
                }
            }
            'e' | 'E' => {
                if has_digits && !in_exponent {
                    in_exponent = true;
                    true
                } else {
                    false
                }
            }
            '+' | '-' => matches!(last_char, Some('e') | Some('E')),
            _ => false,
        };
        if !accept { break; }
        rel_end = i + c.len_utf8();
        last_char = Some(c);
        chars.next();
    }

    has_digits && rel_end == len
}

pub fn is_number_parentheses(n: &str) -> bool {
    let s = n.trim();
    if s.is_empty() {
        return false;
    }

    let mut chars = s.chars().peekable();
    let mut seen_dot = false;
    let mut main_digits = false;

    while let Some(&c) = chars.peek() {
        match c {
            '0'..='9' => {
                main_digits = true;
                chars.next();
            }
            '.' => {
                if seen_dot {
                    return false;
                }
                seen_dot = true;
                chars.next();
            }
            '(' => break,
            _ => return false,
        }
    }

    if !main_digits {
        return false;
    }

    if let Some('(') = chars.next() {
        let mut inner_digits = false;
        while let Some(c) = chars.next() {
            match c {
                '0'..='9' => inner_digits = true,
                ')' => {
                    return inner_digits && chars.peek().is_none();
                }
                _ => return false,
            }
        }
        return false;
    }

    chars.peek().is_none()
}

pub fn get_inner_type(value: &Type) -> Result<(String, Type), String> {
    match value {
        Type::Simple { .. } => Ok((value.display_simple(), value.clone())),
        Type::Alias { name: _, base_type, .. } => get_inner_type(base_type),
        Type::Enum { .. } | Type::Struct { .. } => Err(format!(
            "An 'as' expression can only be used to convert between primitive types, not '{}'",
            value.display_simple()
        )),
        _ => Ok((value.display_simple(), value.clone())),
    }
}

pub fn check_pattern(
    value: &Value,
    pattern: &Value,
) -> Result<(bool, HashMap<String, Value>), (String, String)> {
    let mut variables: HashMap<String, Value> = HashMap::default();

    fn inner(
        value: &Value,
        pat: &Value,
        vars: &mut HashMap<String, Value>,
    ) -> Result<bool, (String, String)> {
        // Union pattern
        if let Value::Map { keys, values: pats } = pat {
            if keys.len() == 1 && keys[0] == Value::String("union".into()) {
                if !pats.is_empty() {
                    for subpat in pats {
                        let mut local_vars = vars.clone();
                        if inner(value, subpat, &mut local_vars)? {
                            *vars = local_vars;
                            return Ok(true);
                        }
                    }
                    return Ok(false);
                }
            }
        }

        // Handle wildcard '_'
        if let Value::Map { values, .. } = pat {
            if let Value::List(p_segments) = &values[0] {
                if p_segments.len() == 1 {
                    if let Value::String(s) = &p_segments[0] {
                        if s == "_" {
                            return Ok(true);
                        }
                    }
                }
            }
        }

        match (value, pat) {
            // Primitive match
            (Value::Int(a), Value::Int(b)) if a == b => Ok(true),
            (Value::Float(a), Value::Float(b)) if a == b => Ok(true),
            (Value::Boolean(a), Value::Boolean(b)) if a == b => Ok(true),
            (Value::String(a), Value::String(b)) if a == b => Ok(true),

            // Tuple match
            (val, Value::Tuple(p_elems)) if val.is_iterable() => {
                let iter = val.iter().collect::<Vec<_>>();
                if p_elems.len() != iter.len() { return Ok(false); }
                for (p, c) in p_elems.iter().zip(iter.iter()) {
                    if !inner(c, p, vars)? { return Ok(false); }
                }
                Ok(true)
            }

            // List match
            (Value::List(c_elems), Value::List(p_elems)) => {
                if c_elems.len() != p_elems.len() { return Ok(false); }
                for (c, p) in c_elems.iter().zip(p_elems.iter()) {
                    if !inner(c, p, vars)? { return Ok(false); }
                }
                Ok(true)
            }

            // Enum matching
            (Value::Enum(cond_enum), Value::Map { keys: p_keys, values: p_vals }) => {
                if p_keys.len() != 2 || p_vals.len() != 2 { return Ok(false); }

                let p_segments = match &p_vals[0] { Value::List(l) => l, _ => return Ok(false) };
                let p_args = match &p_vals[1] { Value::List(l) => l, _ => return Ok(false) };
                if p_segments.is_empty() { return Ok(false); }

                let pat_variant_name = match &p_segments[p_segments.len() - 1] {
                    Value::String(s) => s,
                    _ => return Ok(false),
                };

                let variant_name = match get_variant_name(&cond_enum.ty, cond_enum.variant.0) {
                    Some(name) => name,
                    None => return Ok(false),
                };
                if variant_name != *pat_variant_name { return Ok(false); }

                match &*cond_enum.variant.1 {
                    Value::Tuple(payload_elems) => {
                        if payload_elems.len() != p_args.len() { return Ok(false); }

                        for (payload, arg_pat) in payload_elems.iter().zip(p_args.iter()) {
                            if let Value::Map { values, .. } = arg_pat {
                                if let Value::List(segs) = &values[0] {
                                    if segs.len() == 1 {
                                        if let Value::String(var_name) = &segs[0] {
                                            if get_variant_name(&cond_enum.ty, cond_enum.variant.0)
                                                .as_deref() != Some(var_name)
                                            {
                                                if let Some(existing) = vars.get(var_name) {
                                                    if existing != payload {
                                                        return Ok(false);
                                                    }
                                                } else {
                                                    vars.insert(var_name.clone(), payload.clone());
                                                    continue;
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                            if !inner(payload, arg_pat, vars)? { return Ok(false); }
                        }
                        Ok(true)
                    }
                    other_val => {
                        if p_args.len() == 1 {
                            inner(other_val, &p_args[0], vars)
                        } else {
                            Ok(false)
                        }
                    }
                }
            }

            // Single-segment path pattern -> bind variable
            (val, Value::Map { keys: p_keys, values }) => {
                if p_keys.len() == 2 && values.len() == 2 {
                    let p_segments = match &values[0] { Value::List(l) => l, _ => return Ok(false) };
                    let p_args = match &values[1] { Value::List(l) => l, _ => return Ok(false) };
                    if p_segments.len() == 1 && p_args.is_empty() {
                        if let Value::String(var_name) = &p_segments[0] {
                            if var_name != "_" {
                                if let Some(existing) = vars.get(var_name) {
                                    if existing != val {
                                        return Ok(false);
                                    }
                                } else {
                                    vars.insert(var_name.clone(), val.clone());
                                }
                            }
                            return Ok(true);
                        }
                    }
                }
                Ok(false)
            }

            _ => Ok(false),
        }
    }

    let matched = inner(value, pattern, &mut variables)?;

    if matched {
        Ok((true, variables))
    } else {
        Ok((false, HashMap::default()))
    }
}

pub fn gamma_lanczos(z: f64, level: usize) -> f64 {
    // standard Lanczos approximation (Gamma(z))
    fn gamma_core(z: f64) -> f64 {
        const COEFFS: [f64; 9] = [
            0.99999999999980993,
            676.5203681218851,
           -1259.1392167224028,
            771.32342877765313,
           -176.61502916214059,
            12.507343278686905,
           -0.13857109526572012,
            9.9843695780195716e-6,
            1.5056327351493116e-7,
        ];
        let g = 7.0;

        if z < 0.5 {
            std::f64::consts::PI / ((std::f64::consts::PI * z).sin() * gamma_core(1.0 - z))
        } else {
            let z = z - 1.0;
            let mut x = COEFFS[0];
            for (i, &c) in COEFFS.iter().enumerate().skip(1) {
                x += c / (z + i as f64);
            }
            let t = z + g + 0.5;
            (2.0 * std::f64::consts::PI).sqrt() * t.powf(z + 0.5) * (-t).exp() * x
        }
    }

    if level == 1 {
        gamma_core(z) // standard gamma
    } else {
        // generalized factorial step-k
        // n!_k = k^(n/k) * Γ(n/k + 1)
        let n = z - 1.0;
        let scaled = n / level as f64;
        (level as f64).powf(scaled) * gamma_core(scaled + 1.0)
    }
}

pub fn get_enum_idx(enm: &Type, name: &str) -> Option<usize> {
    if let Type::Enum { variants, .. } = enm {
        variants.iter().find(|(v_name, _, _)| v_name == name).map(|(_, _, idx)| *idx)
    } else {
        None
    }
}

pub fn get_variant_name(enm: &Type, idx: usize) -> Option<String> {
    if let Type::Enum { variants, .. } = enm {
        variants.get(idx).map(|(name, _, _)| name.clone())
    } else {
        None
    }
}

pub fn format_with_dynamic_fill(s: &str, fill: char, width: usize, align: char) -> String {
    let len = s.chars().count();
    if width <= len {
        return s.to_string();
    }
    let pad = width - len;
    match align {
        '<' => {
            let mut result = s.to_string();
            result.extend(std::iter::repeat(fill).take(pad));
            result
        }
        '>' => {
            let mut result = String::with_capacity(width);
            result.extend(std::iter::repeat(fill).take(pad));
            result.push_str(s);
            result
        }
        '^' => {
            let left = pad / 2;
            let right = pad - left;
            let mut result = String::with_capacity(width);
            result.extend(std::iter::repeat(fill).take(left));
            result.push_str(s);
            result.extend(std::iter::repeat(fill).take(right));
            result
        }
        _ => s.to_string(),
    }
}

pub fn apply_format_spec(value: &Value, spec: &str) -> Result<String, String> {
    let mut s = escape_string(&native::format_value(&value))?;

    if spec.trim() == "?" {
        s = escape_string(&format_value(&value))?;
        return Ok(s);
    }

    let mut chars = spec.chars().peekable();
    let mut fill = ' ';
    let mut align = None;
    let mut width = None;
    let mut precision = None;
    let mut sign = None;
    let mut typ = None;

    if let Some(&c1) = chars.peek() {
        if let Some(c2) = chars.clone().nth(1) {
            if matches!(c2, '<' | '>' | '^') {
                fill = c1;
                align = Some(c2);
                chars.next();
                chars.next();
            }
        }
    }
    if align.is_none() {
        if let Some(&c) = chars.peek() {
            if matches!(c, '<' | '>' | '^') {
                align = Some(c);
                chars.next();
            }
        }
    }

    let mut width_str = String::new();
    while let Some(&c) = chars.peek() {
        if c.is_ascii_digit() {
            width_str.push(c);
            chars.next();
        } else {
            break;
        }
    }
    if !width_str.is_empty() {
        width = width_str.parse::<usize>().ok();
    }

    if let Some(&'.') = chars.peek() {
        chars.next();
        let mut prec_str = String::new();
        while let Some(&c) = chars.peek() {
            if c.is_ascii_digit() {
                prec_str.push(c);
                chars.next();
            } else {
                break;
            }
        }
        if !prec_str.is_empty() {
            precision = prec_str.parse::<usize>().ok();
        }
    }

    if let Some(&c) = chars.peek() {
        if matches!(c, '+' | '-') {
            sign = Some(c);
            chars.next();
        }
    }

    if let Some(c) = chars.next() {
        typ = Some(c);
    }

    if precision.is_some() {
        match typ {
            Some(c) if matches!(c, 'f' | 'F' | 'e' | 'E') => {}
            _ => return Err("Precision specified but type is not a float".to_string()),
        }
    }

    if let Some(t) = typ {
        match t {
            'b' => {
                let val: i128 = s.parse().map_err(|_| "Invalid integer")?;
                s = format!("{:b}", val);
            }
            'o' => {
                let val: i128 = s.parse().map_err(|_| "Invalid integer")?;
                s = format!("{:o}", val);
            }
            'x' => {
                let val: i128 = s.parse().map_err(|_| "Invalid integer")?;
                s = format!("{:x}", val);
            }
            'X' => {
                let val: i128 = s.parse().map_err(|_| "Invalid integer")?;
                s = format!("{:X}", val);
            }
            'f' | 'F' => {
                let prec = precision.unwrap_or(6);
                let val: f64 = s.parse().map_err(|_| "Invalid float")?;
                s = format!("{:.*}", prec, val);
                if t == 'F' { s = s.to_uppercase(); }
            }
            'e' | 'E' => {
                let prec = precision.unwrap_or(6);
                let val: f64 = s.parse().map_err(|_| "Invalid float")?;
                s = format!("{:.*e}", prec, val);
                if t == 'E' { s = s.to_uppercase(); }
            }
            'U' | 'u' => s = s.to_uppercase(),
            'L' | 'l' => s = s.to_lowercase(),
            _ => return Err(format!("Unknown format type: {}", t)),
        }
    }

    if let Some(sign_char) = sign {
        if sign_char == '+' && !s.starts_with('-') && !s.starts_with('+') && s.parse::<i128>().is_ok() {
            s = format!("+{}", s);
        }
    }

    if let Some(w) = width {
        s = format_with_dynamic_fill(&s, fill, w, align.unwrap_or('>'));
    }

    Ok(s)
}

pub fn type_matches(actual: &Type, expected: &Type) -> bool {
    let inner_actual = match get_inner_type(actual) {
        Ok((_, inner)) => inner,
        Err(_) => actual.clone(),
    };
    let inner_expected = match get_inner_type(expected) {
        Ok((_, inner)) => inner,
        Err(_) => expected.clone(),
    };

    use Type::*;

    // match &inner_expected {
    //     Simple { name, is_reference, .. } if name == "any" => {
    //         let actual_ref = match &inner_actual {
    //             Simple { is_reference, .. } => *is_reference,
    //             _ => false,
    //         };
    //         return *is_reference == actual_ref;
    //     }
    //     _ => {}
    // }

    if inner_actual == inner_expected {
        return true;
    }

    match (&inner_actual, &inner_expected) {
        (
            Simple { name: actual_name, is_reference: actual_ref, .. },
            Simple { name: expected_name, is_reference: expected_ref, .. },
        ) if expected_name == "any" || actual_name == "any" =>
        {
            return actual_ref == expected_ref;
        }

        (
            Simple { name: actual_name, .. },
            _
        ) if actual_name == "any" =>
        {
            return true;
        }

        (
            _, Simple { name: expected_name, .. }
        ) if expected_name == "any" =>
        {
            return true;
        }

        (
            Struct {
                name: name_a,
                generics: generics_a,
                methods: methods_a,
                wheres: wheres_a,
                ..
            },
            Struct {
                name: name_e,
                generics: generics_e,
                methods: methods_e,
                wheres: wheres_e,
                ..
            },
        ) => {
            if name_a == name_e
                && generics_a == generics_e
                && methods_a == methods_e
                && wheres_a == wheres_e
                && !generics_a.is_empty()
            {
                return true;
            }
        }

        (
            Enum {
                name: name_a,
                generics: generics_a,
                wheres: wheres_a,
                ..
            },
            Enum {
                name: name_e,
                generics: generics_e,
                wheres: wheres_e,
                ..
            },
        ) => {
            if name_a == name_e && generics_a == generics_e && wheres_a == wheres_e && !generics_a.is_empty() {
                return true;
            }
        }

        _ => {}
    }

    false
}

pub fn generate_name_variants(name: &str) -> Vec<String> {
    let mut variants = vec![name.to_string()];
    
    if name.contains('_') {
        let mut queue = vec![name.to_string()];
        while let Some(curr) = queue.pop() {
            let next = curr.replacen('_', "-", 1);
            if !variants.contains(&next) {
                variants.push(next.clone());
                if next.contains('_') {
                    queue.push(next);
                }
            }
        }
    }

    variants
}

pub fn find_struct_method(
    s1: &Struct,
    s2: Option<&Struct>,
    func_name: &str,
    expected_param_types: &[Type],
    expected_return_type: &Type
) -> Result<Function, (String, String, String)> {
    let struct_type_1 = s1.get_type();
    // let struct_type_2 = s2.map(|s| s.get_type());

    if let Type::Struct { methods, .. } = &struct_type_1 {
        if let Some(func) = methods.iter().find(|(name, _)| name == func_name).map(|(_, f)| f) {
            if let Type::Function { parameter_types, return_type } = func.get_type() {
                if parameter_types.len() != expected_param_types.len() {
                    return Err((
                        "ParameterError".to_string(),
                        format!("Expected {} parameters for '{}', got {}", expected_param_types.len(), func_name, parameter_types.len()),
                        "".to_string(),
                    ));
                }

                for (i, expected_type) in expected_param_types.iter().enumerate() {
                    if !type_matches(&parameter_types[i], expected_type) {
                        return Err((
                            "TypeError".to_string(),
                            format!(
                                "Expected parameter type '{}' at position {} for '{}', got '{}'",
                                expected_type.display_simple(),
                                i,
                                func_name,
                                parameter_types[i].display_simple()
                            ),
                            "".to_string(),
                        ));
                    }
                }

                if !type_matches(&return_type, expected_return_type) {
                    return Err((
                        "TypeError".to_string(),
                        format!(
                            "Expected return type '{}' for '{}', got '{}'",
                            expected_return_type.display_simple(),
                            func_name,
                            return_type.display_simple()
                        ),
                        "".to_string(),
                    ));
                }

                return Ok(func.clone());
            } else {
                return Err((
                    "TypeError".to_string(),
                    format!("Expected '{}' to be a function", func_name),
                    "".to_string(),
                ));
            }
        }

        if let Some(s2) = s2 {
            if let Type::Struct { methods: methods2, .. } = &s2.get_type() {
                for (method_name, func) in methods2 {
                    if !func.is_static() { continue; }

                    if let Type::Function { parameter_types, return_type } = func.get_type() {
                        if parameter_types.len() == 1 &&
                           type_matches(&parameter_types[0], &struct_type_1) &&
                           type_matches(&return_type, &s2.get_type()) {
                            return Err((
                                "TypeError".to_string(),
                                format!(
                                    "Cannot perform struct operation between different struct types '{}' and '{}'",
                                    struct_type_1.display_simple(),
                                    s2.get_type().display_simple()
                                ),
                                format!(
                                    "Try calling '{}.{}({})'",
                                    s2.display(),
                                    method_name,
                                    s1.display()
                                ),
                            ));
                        }
                    }
                }
            }
        }

        return Err((
            "TypeError".to_string(),
            format!("Struct type '{}' does not have method '{}'", struct_type_1.display_simple(), func_name),
            "".to_string(),
        ));
    } else {
        return Err((
            "TypeError".to_string(),
            format!("Expected a struct type, got '{}'", struct_type_1.display_simple()),
            "".to_string(),
        ));
    }
}

pub fn diff_fields(
    a_name: &str,
    b_name: &str,
    a: &Vec<(String, Statement, Vec<String>)>,
    b: &Vec<(String, Statement, Vec<String>)>,
) -> Result<HashMap<String, String>, String> {
    let mut map = HashMap::new();
    let mut i = 0;
    let mut j = 0;

    while i < a.len() && j < b.len() {
        let (name_a, stmt_a, mods_a) = &a[i];
        let (name_b, stmt_b, mods_b) = &b[j];

        map.insert(name_a.clone(), name_b.clone());

        let is_a_stmt_b_stmt = stmt_a.is_equal_to_statement(stmt_b);

        if is_a_stmt_b_stmt && mods_a == mods_b {
            i += 1;
            j += 1;
            continue;
        }

        if !is_a_stmt_b_stmt {
            return Err(format!("Field {} has a different type", name_a));
        }

        if mods_a != mods_b {
            return Err(format!("Field {} has different modifiers", name_a));
        }

        i += 1;
        j += 1;
    }

    while i < a.len() {
        let (name, _, _) = &a[i];
        return Err(format!("Field {} is not in {}", name, b_name));
    }

    while j < b.len() {
        let (name, _, _) = &b[j];
        return Err(format!(
            "Field {} is in {} but not in {}",
            name, b_name, a_name
        ));
    }

    Ok(map)
}

pub fn convert_json_value_to_lucia_value(json_value: &serde_json::Value) -> Value {
    match json_value {
        serde_json::Value::Null => Value::Null,
        serde_json::Value::Bool(b) => Value::Boolean(*b),
        serde_json::Value::Number(n) => {
            if let Some(i) = n.as_i64() {
                Value::Int(Int::from(i))
            } else if let Some(f) = n.as_f64() {
                Value::Float(Float::from(f))
            } else {
                Value::Null
            }
        }
        serde_json::Value::String(s) => Value::String(s.clone()),
        serde_json::Value::Array(arr) => {
            let list = arr.iter().map(convert_json_value_to_lucia_value).collect();
            Value::List(list)
        }
        serde_json::Value::Object(obj) => {
            let keys: Vec<Value> = obj.keys().map(|k| Value::String(k.clone())).collect();
            let values: Vec<Value> = obj.values().map(convert_json_value_to_lucia_value).collect();
            Value::Map { keys, values }
        }
    }
}

pub const KEYWORDS: &[&str] = &[
    "fun", "gen", "return", "export", "throw", "end", "catch", "try", "static", "non-static",
    "public", "private", "final", "mutable", "if", "else", "then", "for",
    "while", "as", "from", "import", "in", "forget", "and", "or", "not",
    "isnt", "is", "xor", "xnor", "nein", "match", "break", "continue",
    "defer", "scope", "pass", "band", "lshift", "rshift", "bor", "bnot",
    "type", "typedef", "where", "enum", "struct", "true", "false", "null", "void", "any", "int",
    "float", "bool", "str", "map", "list", "function", "bytes", "tuple", "module", 
    "auto", "generator", "impl"
];

pub const RESERVED_KEYWORDS: &[&str] = &[
    "fun", "gen", "return", "export", "throw", "try", "if", "for", "while", "import",
    "forget", "match", "defer", "true", "false", "null", "typedef", "type"
];

pub const CAN_BE_UNINITIALIZED: &[&str] = &[
    "int", "float", "bool", "str", "map", "list", "function", "bytes", "tuple", "any", "void"
];

#[cfg(target_pointer_width = "64")]
pub const MAX_PTR: usize = 0x0000_FFFF_FFFF_FFFF;

#[cfg(target_pointer_width = "32")]
pub const MAX_PTR: usize = 0xFFFF_FFFF;

pub const NULL: Value = Value::Null;
pub const TRUE: Value = Value::Boolean(true);
pub const FALSE: Value = Value::Boolean(false);
