use crate::env::runtime::utils::{to_static, format_int, fix_path, format_value as format_value_dbg, self, parse_type, get_inner_type, find_struct_method};
use crate::env::runtime::value::Value;
use crate::env::runtime::types::{Int, Float, Type};
use crate::env::runtime::functions::{Function, NativeFunction, SharedNativeFunction, Parameter};
use crate::env::runtime::generators::{GeneratorType, Generator, NativeGenerator, RangeValueIter};
use crate::env::runtime::internal_structs::{EffectFlags};
use crate::interpreter::Interpreter;
use crate::env::runtime::variables::Variable;
use serde_json::json;
use crate::env::runtime::config::{get_version};
use std::collections::HashMap;
use std::sync::Arc;
use std::io::{self, Write};
use once_cell::sync::Lazy;

// -------------------------------
// Function Implementations
fn print(args: &HashMap<String, Value>) -> Value {
    let sep = match args.get("sep") {
        Some(Value::String(s)) => s.clone(),
        Some(other) => format_value(other),
        None => " ".to_string(),
    };

    let end = match args.get("end") {
        Some(Value::String(s)) => s.clone(),
        Some(other) => format_value(other),
        None => "\n".to_string(),
    };

    let values = match args.get("args") {
        Some(Value::List(list)) => list,
        _ => &vec![],
    };

    let mut out = String::new();
    for (i, val) in values.iter().enumerate() {
        if i > 0 {
            out.push_str(&sep);
        }
        out.push_str(&format_value(val));
    }

    out.push_str(&end);

    print!("{}", out);

    Value::Null
}

fn styled_print(args: &HashMap<String, Value>) -> Value {
    let values = match args.get("args") {
        Some(Value::List(list)) => list,
        _ => &vec![],
    };

    let sep = match args.get("sep") {
        Some(Value::String(s)) => s.clone(),
        Some(other) => format_value(other),
        None => " ".to_string(),
    };

    let mut text = values
        .iter()
        .map(|v| format_value(v))
        .collect::<Vec<_>>()
        .join(sep.as_str());

    let mut style = String::new();

    if let Some(Value::String(fg)) = args.get("fg_color") {
        style += &utils::hex_to_ansi(fg, true);
    }

    if let Some(Value::String(bg)) = args.get("bg_color") {
        let hex = if bg.starts_with('#') { &bg[1..] } else { bg };
        if hex.len() == 6 {
            if let (Ok(r), Ok(g), Ok(b)) = (
                u8::from_str_radix(&hex[0..2], 16),
                u8::from_str_radix(&hex[2..4], 16),
                u8::from_str_radix(&hex[4..6], 16),
            ) {
                style.push_str(&format!("\x1b[48;2;{};{};{}m", r, g, b));
            }
        }
    }

    if matches!(args.get("bold"), Some(Value::Boolean(true))) {
        style.push_str("\x1b[1m");
    }
    if matches!(args.get("italic"), Some(Value::Boolean(true))) {
        style.push_str("\x1b[3m");
    }
    if matches!(args.get("underline"), Some(Value::Boolean(true))) {
        style.push_str("\x1b[4m");
    }
    if matches!(args.get("blink"), Some(Value::Boolean(true))) {
        style.push_str("\x1b[5m");
    }
    if matches!(args.get("reverse"), Some(Value::Boolean(true))) {
        style.push_str("\x1b[7m");
    }
    if matches!(args.get("strikethrough"), Some(Value::Boolean(true))) {
        style.push_str("\x1b[9m");
    }

    if let Some(Value::String(link)) = args.get("link") {
        text = format!("[{}]({})", text, link);
    }

    let reset = "\x1b[0m";
    let styled_text = format!("{}{}{}", style, text, reset);

    let mut print_args = HashMap::new();
    print_args.insert("args".to_string(), Value::List(vec![Value::String(styled_text)]));
    if let Some(end_val) = args.get("end") {
        print_args.insert("end".to_string(), end_val.clone());
    }

    print(&print_args);
    return Value::Null;
}

fn input(args: &HashMap<String, Value>) -> Value {
    let prompt = match args.get("prompt") {
        Some(Value::String(s)) => s,
        Some(v) => &format_value(v),
        None => "",
    };

    print!("{}", prompt);
    io::stdout().flush().unwrap();

    let mut buffer = String::new();
    match io::stdin().read_line(&mut buffer) {
        Ok(_) => Value::String(buffer.trim_end().to_string()),
        Err(_) => Value::Error("IOError", "Failed to read input", None),
    }
}

fn len(args: &HashMap<String, Value>, interpreter: &mut Interpreter) -> Value {
    match args.get("v") {
        Some(Value::List(list)) => Value::Int(list.len().into()),
        Some(Value::String(s)) => Value::Int(s.chars().count().into()),
        Some(Value::Map { keys, .. }) => Value::Int(keys.len().into()),
        Some(v @ Value::Int(_) | v @ Value::Float(_)) => {
            Value::Int(format_value(v).len().into())
        }
        Some(Value::Tuple(t)) => Value::Int(t.len().into()),
        Some(Value::Boolean(_)) => Value::Int(1.into()),
        Some(Value::Null) => Value::Int(0.into()),
        Some(Value::Bytes(b)) => Value::Int(b.len().into()),
        Some(Value::Struct(s)) => {
            let mut var = Variable::new_pt(s.name().to_string(), Value::Struct(s.clone()), s.get_type(), false, false, false);
            if let Ok(method) = find_struct_method(&s, None, "op_len", &[s.get_type()], &Type::new_simple("int")) {
                if method.is_static() {
                    return Value::Error("TypeError", "Cannot call static method 'op_len' on struct instance", None);
                }
                let result = interpreter.call_function(&method, vec![], HashMap::new(), Some((None, Some(&mut var))));
                if interpreter.err.is_some() {
                    let err = interpreter.err.clone().expect("Error should be present");
                    return Value::Error(to_static(err.error_type), to_static(err.msg), None);
                }
                return result;
            } else {
                return Value::Error("TypeError", "Struct not indexable", None);
            }
        }
        Some(v) => {
            let msg = format!(
                "Function 'len()' doesn't support type '{}'",
                v.get_type().display_simple()
            );
            Value::Error("TypeError", to_static(msg), None)
        }
        None => Value::Null,
    }
}

// i need that rn 
fn help(args: &HashMap<String, Value>) -> Value {
    let info = json!({
        "print": {
            "type": "function",
            "description": "Prints the given values to the standard output.",
            "args": {
                "args": {
                    "type": "*any",
                    "description": "Values to print."
                },
                "end": {
                    "type": "str",
                    "description": "String to append after printing (default: '\\n')."
                },
                "sep": {
                    "type": "str",
                    "description": "String to separate values (default: ' ')."
                }
            }
        },
        "exit": {
            "type": "function",
            "description": "Exits the program.",
            "example": "exit()"
        }
    });

    if let Some(Value::String(obj)) = args.get("object") {
        let obj_name = obj.trim_end_matches("()");
        if let Some(obj_info) = info.get(obj_name) {
            if let Some(obj_type) = obj_info.get("type").and_then(|v| v.as_str()) {
                println!("Help for '{}':", obj_name);
                println!("  Type: {}", obj_type);
            }

            if let Some(desc) = obj_info.get("description").and_then(|v| v.as_str()) {
                println!("  Description: {}", desc);
            }

            if let Some(example) = obj_info.get("example").and_then(|v| v.as_str()) {
                println!("  Example: {}", example);
            }

            if let Some(args) = obj_info.get("args").and_then(|v| v.as_object()) {
                println!("  Arguments:");
                for (arg_name, arg_info) in args {
                    if let Some(arg_map) = arg_info.as_object() {
                        let ty = arg_map.get("type").and_then(|v| v.as_str()).unwrap_or("unknown");
                        let desc = arg_map.get("description").and_then(|v| v.as_str()).unwrap_or("");
                        println!("    - {}: {} ({})", arg_name, ty, desc);
                    }
                }
            }
        } else {
            println!("No help available for '{}'", obj_name);
            println!("This object may not exist or is not documented.");
            return Value::Null;
        }

        return Value::Null;
    }

    let version = get_version();

    let content = format!(r#"Welcome to Lucia-{}!

If you're new to Lucia, start with the tutorial:
https://github.com/SirPigari/lucia/tree/main/env/Docs/introduction.md

- Need help? Enter help(object="object name") to get information about a function or object.
- Want to see available modules or keywords? Use modules() or keywords().
- Ready to exit? Type exit() to leave the Lucia REPL.

Happy coding!"#, version);

    println!("{}", content);
    Value::Null
}

fn type_name(args: &HashMap<String, Value>) -> Value {
    if let Some(value) = args.get("obj") {
        return Value::Type(value.get_type());
    }
    Value::Error("TypeError", "No value provided for type_name()", None)
}

fn size_of(args: &HashMap<String, Value>) -> Value {
    if let Some(value) = args.get("obj") {
        return Value::Int(Int::from_i64(value.get_size() as i64));
    }
    Value::Error("TypeError", "No value provided for size_of()", None)
}

fn sum(args: &HashMap<String, Value>) -> Value {
    if let Some(Value::List(values)) = args.get("args") {
        let mut total = Float::from_f64(0.0);
        let mut stack = values.iter().collect::<Vec<_>>();

        while let Some(value) = stack.pop() {
            match value {
                Value::Int(i) => {
                    match i.to_i64() {
                        Ok(n) => {
                            total += Float::from_f64(n as f64);
                        }
                        Err(_) => {
                            return Value::Error("TypeError", "Failed to convert BigInt to f64", None);
                        }
                    }
                }
                Value::Float(f) => {
                    total += f.clone();
                }
                Value::List(ls) | Value::Tuple(ls) => {
                    for v in ls {
                        stack.push(v);
                    }
                }
                _ => return Value::Error("TypeError", "Value is not summable", None),
            }
        }

        Value::Float(total)
    } else {
        Value::Error("TypeError", "Expected a list of numeric values", None)
    }
}

fn ord(args: &HashMap<String, Value>) -> Value {
    if let Some(Value::String(s)) = args.get("s") {
        if let Some(c) = s.chars().next() {
            return Value::Int(Int::from_i64(c as i64));
        }
    }
    Value::Error("TypeError", "Expected a string with at least one character", None)
}

fn char(args: &HashMap<String, Value>) -> Value {
    if let Some(Value::Int(i)) = args.get("i") {
        if let Some(c) = std::char::from_u32(i.to_i64().unwrap_or(0) as u32) {
            return Value::String(c.to_string());
        }
    }
    Value::Error("TypeError", "Expected an integer representing a Unicode code point", None)
}

fn styledstr(args: &HashMap<String, Value>) -> Value {
    let values = match args.get("args") {
        Some(Value::List(list)) => list,
        _ => &vec![],
    };

    let sep = match args.get("sep") {
        Some(Value::String(s)) => s.clone(),
        Some(other) => format_value(other),
        None => " ".to_string(),
    };

    let mut text = values
        .iter()
        .map(|v| format_value(v))
        .collect::<Vec<_>>()
        .join(sep.as_str());

    let mut style = String::new();

    if let Some(Value::String(fg)) = args.get("fg_color") {
        style += &utils::hex_to_ansi(fg, true);
    }

    if let Some(Value::String(bg)) = args.get("bg_color") {
        let hex = if bg.starts_with('#') { &bg[1..] } else { bg };
        if hex.len() == 6 {
            if let (Ok(r), Ok(g), Ok(b)) = (
                u8::from_str_radix(&hex[0..2], 16),
                u8::from_str_radix(&hex[2..4], 16),
                u8::from_str_radix(&hex[4..6], 16),
            ) {
                style.push_str(&format!("\x1b[48;2;{};{};{}m", r, g, b));
            }
        }
    }

    if matches!(args.get("bold"), Some(Value::Boolean(true))) {
        style.push_str("\x1b[1m");
    }
    if matches!(args.get("italic"), Some(Value::Boolean(true))) {
        style.push_str("\x1b[3m");
    }
    if matches!(args.get("underline"), Some(Value::Boolean(true))) {
        style.push_str("\x1b[4m");
    }
    if matches!(args.get("blink"), Some(Value::Boolean(true))) {
        style.push_str("\x1b[5m");
    }
    if matches!(args.get("reverse"), Some(Value::Boolean(true))) {
        style.push_str("\x1b[7m");
    }
    if matches!(args.get("strikethrough"), Some(Value::Boolean(true))) {
        style.push_str("\x1b[9m");
    }

    if let Some(Value::String(link)) = args.get("link") {
        text = format!("[{}]({})", text, link);
    }

    let reset = "\x1b[0m";
    let styled_text = format!("{}{}{}", style, text, reset);
    let end = match args.get("end") {
        Some(Value::String(s)) => s.clone(),
        Some(other) => format_value(other),
        None => "".to_string(),
    };

    let mut output = String::new();
    output.push_str(&styled_text);
    output.push_str(&end);

    Value::String(output)
}

fn array(args: &HashMap<String, Value>) -> Value {
    if let Some(Value::Int(size)) = args.get("size") {
        if let Ok(size) = size.to_usize() {
            let initial_value = args.get("initial_value").cloned().unwrap_or(Value::Null);

            let mut list = Vec::with_capacity(size);
            for _ in 0..size {
                list.push(initial_value.clone());
            }
            return Value::List(list);
        } else {
            return Value::Error("TypeError", "Size must be a positive integer", None);
        }
    }
    Value::Error("TypeError", "Expected 'size' parameter of type int", None)
}

fn range(args: &HashMap<String, Value>) -> Value {
    let (as_gen, use_tuple) = if let Some(Value::Type(as_type)) = args.get("as") {
        let (_, inner) = match get_inner_type(as_type) {
            Ok(t) => t,
            Err(e) => return Value::Error("TypeError", to_static(e), None),
        };
        match inner {
            Type::Simple { name, .. } => match name.as_str() {
                "generator" => (true, false),
                "tuple" => (false, true),
                "list" => (false, false),
                _ => {
                    return Value::Error(
                        "TypeError",
                        to_static(format!("unsupported type '{}' for 'as'", name)),
                        None,
                    );
                }
            },
            _ => {
                return Value::Error(
                    "TypeError",
                    "unsupported complex type for 'as'",
                    None,
                )
            }
        }
    } else {
        (false, false)
    };

    let (start, end) = match (args.get("a"), args.get("b")) {
        (Some(a), Some(Value::Null)) | (Some(a), None) => (Value::Int(Int::from(0)), a.clone()),
        (Some(a), Some(b)) => (a.clone(), b.clone()),
        _ => return Value::Error("TypeError", "expected at least 'a'", None),
    };

    let step_val = match args.get("step") {
        Some(Value::Null) | None => Value::Int(Int::from(1)),
        Some(v) => v.clone(),
    };

    let zero_step = match &step_val {
        Value::Int(i) => *i == Int::from(0),
        _ => true,
    };
    if zero_step {
        return Value::Error("ValueError", "step cannot be zero", None);
    }

    if as_gen {
        let start_val = match start {
            Value::Int(i) => i.clone(),
            _ => return Value::Error("TypeError", "generator only supports integers", None),
        };
        let end_val = match end {
            Value::Int(i) => i.clone(),
            _ => return Value::Error("TypeError", "generator only supports integers", None),
        };
        let step = match step_val {
            Value::Int(s) if s > Int::from(0) => match s.to_usize() {
                Ok(v) => v,
                Err(_) => return Value::Error("ValueError", "step too large", None),
            },
            _ => return Value::Error("TypeError", "generator step must be positive integer", None),
        };

        let start_val = Value::Int(start_val);
        let step_val = Value::Int(Int::from(step));
        let range_iter = RangeValueIter::new(&start_val, &Value::Int((&end_val - &Int::from(1)).unwrap_or(end_val)), &step_val);

        let generator = Generator::new_anonymous(
            GeneratorType::Native(NativeGenerator {
                iter: Box::new(range_iter),
                iteration: 0,
            }),
            false,
        );

        Value::Generator(generator)
    } else {
        let zero_step = match &step_val {
            Value::Int(i) => *i == Int::from(0),
            Value::Float(f) => *f == Float::from(0.0),
            _ => false,
        };
        if zero_step {
            return Value::Error("ValueError", "step cannot be zero", None);
        }

        let mut elements = Vec::new();
        let mut current = start.clone();

        let continue_loop = |c: &Value, e: &Value, s: &Value| -> bool {
            match (c, e, s) {
                (Value::Int(c), Value::Int(e), Value::Int(s)) => if *s > Int::from(0) { c < e } else { c > e },
                (Value::Float(c), Value::Float(e), Value::Float(s)) => if *s > Float::from(0.0) { c < e } else { c > e },
                (Value::Int(c), Value::Int(e), Value::Float(s)) => {
                    let c_f = Float::from(c.to_i64().unwrap_or(0) as f64);
                    let e_f = Float::from(e.to_i64().unwrap_or(0) as f64);
                    if *s > Float::from(0.0) { c_f < e_f } else { c_f > e_f }
                }
                (Value::Float(c), Value::Int(e), Value::Float(s)) => {
                    let e_f = Float::from(e.to_i64().unwrap_or(0) as f64);
                    if *s > Float::from(0.0) { c < &e_f } else { c > &e_f }
                }
                (Value::Float(c), Value::Int(e), Value::Int(s)) => {
                    let e_f = Float::from(e.to_i64().unwrap_or(0) as f64);
                    let s_f = Float::from(s.to_i64().unwrap_or(0) as f64);
                    if s_f > Float::from(0.0) { c < &e_f } else { c > &e_f }
                }
                _ => false,
            }
        };

        while continue_loop(&current, &end, &step_val) {
            elements.push(current.clone());

            current = match (&current, &step_val) {
                (Value::Int(c), Value::Int(s)) => match c + s {
                    Ok(r) => Value::Int(r),
                    Err(_) => return Value::Error("OverflowError", "integer overflow", None),
                },
                (Value::Float(c), Value::Float(s)) => match c + s {
                    Ok(r) => Value::Float(r),
                    Err(_) => return Value::Error("OverflowError", "float overflow", None),
                },
                (Value::Int(c), Value::Float(s)) => {
                    let c_f = Float::from(c.to_i64().unwrap_or(0) as f64);
                    match &c_f + s {
                        Ok(r) => Value::Float(r),
                        Err(_) => return Value::Error("OverflowError", "float overflow", None),
                    }
                }
                (Value::Float(c), Value::Int(s)) => {
                    let s_f = Float::from(s.to_i64().unwrap_or(0) as f64);
                    match c + &s_f {
                        Ok(r) => Value::Float(r),
                        Err(_) => return Value::Error("OverflowError", "float overflow", None),
                    }
                }
                _ => return Value::Error("TypeError", "unsupported numeric types", None),
            };
        }

        if use_tuple {
            Value::Tuple(elements)
        } else {
            Value::List(elements)
        }
    }
}

fn complex(args: &HashMap<String, Value>) -> Value {
    let real = match args.get("real") {
        Some(Value::Int(i)) => match i.to_float() {
            Ok(f) => f,
            Err(_) => return Value::Error("TypeError", "failed to convert int to float for real part", None),
        },
        Some(Value::Float(f)) => f.clone(),
        Some(_) => return Value::Error("TypeError", "real part must be int or float", None),
        None => return Value::Error("TypeError", "real part is required", None),
    };
    let imag = match args.get("imaginary") {
        Some(Value::Int(i)) => match i.to_float() {
            Ok(f) => f,
            Err(_) => return Value::Error("TypeError", "failed to convert int to float for imaginary part", None),
        },
        Some(Value::Float(f)) => f.clone(),
        Some(_) => return Value::Error("TypeError", "imaginary part must be int or float", None),
        None => return Value::Error("TypeError", "imaginary part is required", None),
    };
    Value::Float(Float::complex(real, imag))
}

fn __placeholder__(_args: &HashMap<String, Value>) -> Value {
    Value::Error("PlaceholderError", "This is a placeholder function and should not be called.", None)
}

// -------------------------------
// Utility Functions
pub fn format_value(value: &Value) -> String {
    match value {
        Value::Float(n) => n.to_str(),
        Value::Int(n) => format_int(&n.clone()),
        Value::String(s) => s.clone(),
        Value::Boolean(b) => b.to_string(),
        Value::Null => "null".to_string(),
        Value::Map { keys, values, .. } => {
            let formatted_pairs: Vec<String> = keys
                .iter()
                .zip(values.iter())
                .filter(|(key, _)| {
                    if let Value::String(s) = key {
                        s != "_line" && s != "_column"
                    } else {
                        true
                    }
                })
                .map(|(key, value)| format!("{}: {}", utils::format_value(key), utils::format_value(value)))
                .collect();
            format!("{{{}}}", formatted_pairs.join(", "))
        }
        Value::List(values) => {
            if values.is_empty() {
                "[]".to_string()
            } else {
                let formatted_values: Vec<String> = values.iter().map(utils::format_value).collect();
                format!("[{}]", formatted_values.join(", "))
            }
        }
        Value::Tuple(values) => {
            if values.is_empty() {
                "()".to_string()
            } else {
                let formatted_values: Vec<String> = values.iter().map(utils::format_value).collect();
                format!("({})", formatted_values.join(", "))
            }
        }
        Value::Bytes(bytes) => {
            match String::from_utf8(bytes.clone()) {
                Ok(decoded) => decoded,
                Err(_) => format!("<invalid utf-8: {:?}>", bytes),
            }
        }
        Value::Type(ty) => {
            ty.display()
        }
        Value::Function(func) => {
            format!("<function '{}' at {:p}>", func.get_name(), func)
        }
        Value::Error(err_type, err_msg, _) => {
            format!("<{}: {}>", err_type, err_msg)
        }
        Value::Module(obj) => {
            format!("<module '{}' from '{}' at {:p}>", obj.name(), fix_path(obj.path().display().to_string()), obj.ptr())
        }
        _ => format_value_dbg(value),
    }
}

// -------------------------------
// Function Definitions
pub fn print_fn() -> Function {
    Function::Native(Arc::new(NativeFunction::new(
        "print",
        print,
        vec![  
            Parameter::variadic_optional("args", "any", Value::String("".to_string())), 
            Parameter::positional_optional("end", "str", Value::String("\n".to_string())),
            Parameter::positional_optional("sep", "str", Value::String(" ".to_string())),
        ],
        "void",
        true, true, true,
        None,
        EffectFlags::IO,
    )))
}

pub fn styled_print_fn() -> Function {
    Function::Native(Arc::new(NativeFunction::new(
        "styled_print",
        styled_print,
        vec![
            Parameter::variadic_optional("args", "any", Value::String("".to_string())),
            Parameter::keyword_optional("sep", "str", Value::String(" ".to_string())),
            Parameter::keyword_optional("end", "str", Value::String("\n".to_string())),
            Parameter::keyword_optional("fg_color", "str", Value::String("reset".to_string())),
            Parameter::keyword_optional("bg_color", "str", Value::String("reset".to_string())),
            Parameter::keyword_optional("bold", "bool", Value::Boolean(false)),
            Parameter::keyword_optional("italic", "bool", Value::Boolean(false)),
            Parameter::keyword_optional("underline", "bool", Value::Boolean(false)),
            Parameter::keyword_optional("blink", "bool", Value::Boolean(false)),
            Parameter::keyword_optional("reverse", "bool", Value::Boolean(false)),
            Parameter::keyword_optional("strikethrough", "bool", Value::Boolean(false)),
            Parameter::keyword_optional("link", "any", Value::Null),
        ],
        "void",
        true, true, true,
        None,
        EffectFlags::IO,
    )))
}

pub fn input_fn() -> Function {
    Function::Native(Arc::new(NativeFunction::new(
        "input",
        input,
        vec![
            Parameter::positional_optional("prompt", "str", Value::String("".to_string())),
        ],
        "str",
        true, true, true,
        None,
        EffectFlags::IO,
    )))
}

pub fn len_fn() -> Function {
    Function::SharedNative(Arc::new(SharedNativeFunction::new(
        "len",
        len,
        vec![
            Parameter::positional("v", "any"),
        ],
        "int",
        true, true, true,
        None,
        EffectFlags::PURE,
    )))
}

pub fn help_fn() -> Function {
    Function::Native(Arc::new(NativeFunction::new(
        "help",
        help,
        vec![
            Parameter::positional_optional("object", "any", Value::Null),
        ],
        "void",
        true, true, true,
        None,
        EffectFlags::IO,
    )))
}

pub fn type_fn() -> Function {
    Function::Native(Arc::new(NativeFunction::new(
        "type_of",
        type_name,
        vec![
            Parameter::positional("obj", "any"),
        ],
        "type",
        true, true, true,
        None,
        EffectFlags::PURE,
    )))
}

pub fn size_of_fn() -> Function {
    Function::Native(Arc::new(NativeFunction::new(
        "size_of",
        size_of,
        vec![
            Parameter::positional("obj", "any"),
        ],
        "int",
        true, true, true,
        None,
        EffectFlags::PURE,
    )))
}

pub fn sum_fn() -> Function {
    Function::Native(Arc::new(NativeFunction::new(
        "sum",
        sum,
        vec![
            Parameter::variadic_optional("args", "any", Value::Int(0.into())), 
            Parameter::keyword_optional("start", "any", Value::Int(0.into())),
        ],
        "float",
        true, true, true,
        None,
        EffectFlags::PURE,
    )))
}

pub fn ord_fn() -> Function {
    Function::Native(Arc::new(NativeFunction::new(
        "ord",
        ord,
        vec![
            Parameter::positional("s", "str"),
        ],
        "int",
        true, true, true,
        None,
        EffectFlags::PURE,
    )))
}

pub fn char_fn() -> Function {
    Function::Native(Arc::new(NativeFunction::new(
        "char",
        char,
        vec![
            Parameter::positional("i", "int"),
        ],
        "str",
        true, true, true,
        None,
        EffectFlags::PURE,
    )))
}

pub fn styledstr_fn() -> Function {
    Function::Native(Arc::new(NativeFunction::new(
        "styledstr",
        styledstr,
        vec![
            Parameter::variadic_optional("args", "any", Value::String("".to_string())),
            Parameter::keyword_optional("sep", "str", Value::String(" ".to_string())),
            Parameter::keyword_optional("end", "str", Value::String("\n".to_string())),
            Parameter::keyword_optional("fg_color", "str", Value::String("reset".to_string())),
            Parameter::keyword_optional("bg_color", "str", Value::String("reset".to_string())),
            Parameter::keyword_optional("bold", "bool", Value::Boolean(false)),
            Parameter::keyword_optional("italic", "bool", Value::Boolean(false)),
            Parameter::keyword_optional("underline", "bool", Value::Boolean(false)),
            Parameter::keyword_optional("blink", "bool", Value::Boolean(false)),
            Parameter::keyword_optional("reverse", "bool", Value::Boolean(false)),
            Parameter::keyword_optional("strikethrough", "bool", Value::Boolean(false)),
            Parameter::keyword_optional("link", "any", Value::Null),
        ],
        "str",
        true, true, true,
        None,
        EffectFlags::PURE,
    )))
}

pub fn array_fn() -> Function {
    Function::Native(Arc::new(NativeFunction::new(
        "array",
        array,
        vec![
            Parameter::positional("size", "int"),
            Parameter::positional_optional("initial_value", "any", Value::Null),
        ],
        "list",
        true, true, true,
        None,
        EffectFlags::PURE,
    )))
}

pub static INT_OR_FLOAT: Lazy<Type> = Lazy::new(|| parse_type("int | float"));
pub static LIST_OR_GEN: Lazy<Type> = Lazy::new(|| parse_type("list | tuple | generator"));

pub fn range_fn() -> Function {
    Function::Native(Arc::new(NativeFunction::new_pt(
        "range",
        range,
        vec![
            Parameter::positional_optional_pt("a", &INT_OR_FLOAT, Value::Null),
            Parameter::positional_optional_pt("b", &INT_OR_FLOAT, Value::Null),
            Parameter::positional_optional_pt("step", &INT_OR_FLOAT, Value::Int(1.into())),
            Parameter::keyword_optional("as", "type", Value::Type(Type::new_simple("generator"))),
        ],
        &LIST_OR_GEN,
        true, true, true,
        None,
        EffectFlags::PURE,
    )))
}

pub fn complex_fn() -> Function {
    Function::Native(Arc::new(NativeFunction::new(
        "complex",
        complex,
        vec![
            Parameter::positional_pt("real", &INT_OR_FLOAT),
            Parameter::positional_pt("imaginary", &INT_OR_FLOAT),
        ],
        "float",
        true, true, true,
        None,
        EffectFlags::PURE,
    )))
}

pub fn placeholder_fn() -> Function {
    Function::Native(Arc::new(NativeFunction::new(
        "__placeholder__",
        __placeholder__,
        vec![
            Parameter::variadic_optional("args", "any", Value::Null),
            Parameter::keyword_optional("kwargs", "any", Value::Null),
        ],
        "void",
        true, true, true,
        None,
        EffectFlags::PURE,
    )))
}