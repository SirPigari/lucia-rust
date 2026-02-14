use crate::env::runtime::utils::{to_static, format_int, fix_path, format_value as format_value_dbg, self, parse_type, get_inner_type, find_struct_method, make_native_method_pt, make_native_method, make_native_shared_fn, convert_value_to_type, check_type_ident};
use crate::env::runtime::value::Value;
use crate::env::runtime::types::{Int, Float, Type, SimpleType};
use crate::env::runtime::functions::{Function, NativeFunction, SharedNativeFunction, Parameter};
use crate::env::runtime::generators::{GeneratorType, Generator, NativeGenerator, RangeValueIter, VecIter, EnumerateIter, FilterIter, MapIter, RepeatingIter};
#[cfg(not(target_arch = "wasm32"))]
use crate::env::runtime::repl::read_input_no_repl;
use crate::env::runtime::internal_structs::{EffectFlags};
use crate::interpreter::Interpreter;
use crate::env::runtime::variables::Variable;
use crate::env::runtime::config::{get_version};
use imagnum::{create_int};
use std::collections::HashMap;
use std::sync::Arc;
use once_cell::sync::Lazy;
use rustc_hash::{FxHashSet, FxHashMap};
use parking_lot::Mutex;
use std::io::{stdout, Write};

// -------------------------------
// Function Implementations
fn print(args: &HashMap<String, Value>, interpreter: &mut Interpreter) -> Value {
    let sep = match args.get("sep") {
        Some(Value::String(s)) => s.clone(),
        Some(other) => format_value(other, interpreter),
        None => " ".to_string(),
    };

    let end = match args.get("end") {
        Some(Value::String(s)) => s.clone(),
        Some(other) => format_value(other, interpreter),
        None => "".to_string(),
    };

    let values = match args.get("args") {
        Some(Value::List(list)) => list,
        _ => &vec![],
    };

    let flush = match args.get("flush") {
        Some(Value::Boolean(b)) => *b,
        _ => false,
    };

    let mut out = String::new();
    for (i, val) in values.iter().enumerate() {
        if i > 0 {
            out.push_str(&sep);
        }
        out.push_str(&format_value(val, interpreter));
    }

    out.push_str(&end);

    print!("{}", out);
    if flush {
        stdout().flush().unwrap_or(());
    }

    Value::Null
}

fn println(args: &HashMap<String, Value>, interpreter: &mut Interpreter) -> Value {
    let mut args = args.clone();
    args.insert("end".to_string(), Value::String("\n".to_string()));
    print(&args, interpreter)
}

fn styledstr(args: &HashMap<String, Value>, interpreter: &mut Interpreter) -> Value {
    let values = match args.get("args") {
        Some(Value::List(list)) => list,
        _ => &vec![],
    };

    let sep = match args.get("sep") {
        Some(Value::String(s)) => s.clone(),
        Some(other) => format_value(other, interpreter),
        None => " ".to_string(),
    };

    let mut text = values
        .iter()
        .map(|v| format_value(v, interpreter))
        .collect::<Vec<_>>()
        .join(sep.as_str());

    let mut style = String::new();

    let fg = args.get("fg_color").and_then(|v| match v {
        Value::String(s) => Some(s.as_str()),
        _ => None,
    }).unwrap_or("reset");

    let bg = args.get("bg_color").and_then(|v| match v {
        Value::String(s) => Some(s.as_str()),
        _ => None,
    }).unwrap_or("reset");

    if bg != "reset" {
        style += &utils::hex_to_ansi_bg(bg, true);
    }

    if fg != "reset" || bg == "reset" {
        style += &utils::hex_to_ansi(fg, true);
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
        // OSC 8: \x1b]8;;url\x1b\text\x1b]8;;\x1b\
        text = format!("\x1b]8;;{}\x1b\\{}\x1b]8;;\x1b\\", link, text);
    }

    let styled_text = format!("{}{}", style, text);
    let end = match args.get("end") {
        Some(Value::String(s)) => s.clone(),
        Some(other) => format_value(other, interpreter),
        None => "".to_string(),
    };

    let mut output = String::new();
    output.push_str(&styled_text);
    output.push_str(&end);

    Value::String(output)
}

fn styled_print(args: &HashMap<String, Value>, interpreter: &mut Interpreter) -> Value {
    let mut args = args.clone();
    if args.get("end") == Some(&Value::String("".to_owned())) {
        args.insert("end".to_string(), Value::String("\x1b[0m".to_string()));
    }
    let str_to_print = styledstr(&args, interpreter);

    print!("{}", match str_to_print {
        Value::String(s) => s,
        _ => return Value::new_error("RuntimeError", "styledstr did not return a string", None),
    });
    return Value::Null;
}

#[cfg(not(target_arch = "wasm32"))]
fn input(args: &HashMap<String, Value>, interpreter: &mut Interpreter) -> Value {
    let prompt = match args.get("prompt") {
        Some(Value::String(s)) => s,
        Some(v) => &format_value(v, interpreter),
        None => "",
    };

    let multiline: Option<String> = match args.get("multiline") {
        Some(Value::String(b)) => Some(b.clone()),
        _ => None,
    };

    let err: bool = match args.get("err") {
        Some(Value::Boolean(b)) => *b,
        _ => false,
    };

    match read_input_no_repl(prompt, multiline.as_deref()) {
        Ok(input) => Value::String(input),
        Err((err_type, err_msg)) => if err { Value::new_error(&err_type, &err_msg, None) } else { Value::String("".to_string()) },
    }
}

#[cfg(target_arch = "wasm32")]
fn input(args: &HashMap<String, Value>, _interpreter: &mut Interpreter) -> Value {
    let prompt = match args.get("prompt") {
        Some(Value::String(s)) => s,
        Some(v) => &format_value_dbg(v),
        None => "",
    };

    Value::String(input!(prompt))
}

fn len(args: &HashMap<String, Value>, interpreter: &mut Interpreter) -> Value {
    match args.get("v") {
        Some(Value::List(list)) => Value::Int(list.len().into()),
        Some(Value::String(s)) => Value::Int(s.chars().count().into()),
        Some(Value::Map(map)) => Value::Int(map.len().into()),
        Some(v @ Value::Int(_) | v @ Value::Float(_)) => {
            Value::Int(format_value(v, interpreter).len().into())
        }
        Some(Value::Tuple(t)) => Value::Int(t.len().into()),
        Some(Value::Boolean(_)) => Value::Int(1.into()),
        Some(Value::Null) => Value::Int(0.into()),
        Some(Value::Bytes(b)) => Value::Int(b.len().into()),
        Some(Value::Struct(s)) => {
            let mut var = Variable::new_pt(s.name().to_string(), Value::Struct(s.clone()), s.get_type(), false, false, false);
            if let Ok(method) = find_struct_method(&s, None, "op_len", &[s.get_type()], &Type::new_simple("int")) {
                if method.is_static() {
                    return Value::new_error("TypeError", "Cannot call static method 'op_len' on struct instance", None);
                }
                let result = interpreter.call_function(&method, vec![], HashMap::new(), Some((None, None, Some(&mut var))));
                if interpreter.err.is_some() {
                    let err = interpreter.err.clone().expect("Error should be present");
                    return Value::Error(Arc::new(err));
                }
                return result;
            } else {
                return Value::new_error("TypeError", "Struct not indexable", None);
            }
        }
        Some(v) => {
            let msg = format!(
                "Function 'len()' doesn't support type '{}'",
                v.get_type().display_simple()
            );
            Value::new_error("TypeError", to_static(msg), None)
        }
        None => Value::Null,
    }
}

// i need that rn 
fn help(args: &HashMap<String, Value>) -> Value {
    if let Some(val) = args.get("value") {
        if !matches!(val, Value::Null) {
            println!("{}", val.help_string());
            return Value::Null;
        }
    }

    let version = get_version();

    let content = format!(r#"Welcome to Lucia-{}!

If you're new to Lucia, start with the tutorial:
https://github.com/SirPigari/lucia/tree/main/env/Docs/introduction.md

- Need help? Enter help(value) to get information about a function or object.
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
    Value::new_error("TypeError", "No value provided for type_name()", None)
}

fn size_of(args: &HashMap<String, Value>) -> Value {
    if let Some(value) = args.get("obj") {
        return Value::Int(Int::from_i64(value.get_size() as i64));
    }
    Value::new_error("TypeError", "No value provided for size_of()", None)
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
                            return Value::new_error("TypeError", "Failed to convert BigInt to f64", None);
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
                _ => return Value::new_error("TypeError", "Value is not summable", None),
            }
        }

        Value::Float(total)
    } else {
        Value::new_error("TypeError", "Expected a list of numeric values", None)
    }
}

fn ord(args: &HashMap<String, Value>) -> Value {
    if let Some(Value::String(s)) = args.get("s") {
        if let Some(c) = s.chars().next() {
            return Value::Int(Int::from_i64(c as i64));
        }
    }
    Value::new_error("TypeError", "Expected a string with at least one character", None)
}

fn char(args: &HashMap<String, Value>) -> Value {
    if let Some(Value::Int(i)) = args.get("i") {
        if let Some(c) = std::char::from_u32(i.to_i64().unwrap_or(0) as u32) {
            return Value::String(c.to_string());
        }
    }
    Value::new_error("TypeError", "Expected an integer representing a Unicode code point", None)
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
            return Value::new_error("TypeError", "Size must be a positive integer", None);
        }
    }
    Value::new_error("TypeError", "Expected 'size' parameter of type int", None)
}

fn range(args: &HashMap<String, Value>) -> Value {
    let (as_gen, use_tuple) = if let Some(Value::Type(as_type)) = args.get("as") {
        let (_, inner) = match get_inner_type(as_type) {
            Ok(t) => t,
            Err(e) => return Value::new_error("TypeError", to_static(e), None),
        };
        match inner {
            Type::Simple { ty } => match ty {
                SimpleType::Generator => (true, false),
                SimpleType::Tuple => (false, true),
                SimpleType::List => (false, false),
                _ => {
                    return Value::new_error(
                        "TypeError",
                        to_static(format!("unsupported type '{}' for 'as'", ty.to_string())),
                        None,
                    );
                }
            },
            _ => {
                return Value::new_error(
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
        _ => return Value::new_error("TypeError", "expected at least 'a'", None),
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
        return Value::new_error("ValueError", "step cannot be zero", None);
    }

    if as_gen {
        let start_val = match start {
            Value::Int(i) => i.clone(),
            _ => return Value::new_error("TypeError", "generator only supports integers", None),
        };
        let end_val = match end {
            Value::Int(i) => i.clone(),
            _ => return Value::new_error("TypeError", "generator only supports integers", None),
        };
        let step = match step_val {
            Value::Int(s) if s > Int::from(0) => match s.to_usize() {
                Ok(v) => v,
                Err(_) => return Value::new_error("ValueError", "step too large", None),
            },
            _ => return Value::new_error("TypeError", "generator step must be positive integer", None),
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
            return Value::new_error("ValueError", "step cannot be zero", None);
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
                    Err(_) => return Value::new_error("OverflowError", "integer overflow", None),
                },
                (Value::Float(c), Value::Float(s)) => match c + s {
                    Ok(r) => Value::Float(r),
                    Err(_) => return Value::new_error("OverflowError", "float overflow", None),
                },
                (Value::Int(c), Value::Float(s)) => {
                    let c_f = Float::from(c.to_i64().unwrap_or(0) as f64);
                    match &c_f + s {
                        Ok(r) => Value::Float(r),
                        Err(_) => return Value::new_error("OverflowError", "float overflow", None),
                    }
                }
                (Value::Float(c), Value::Int(s)) => {
                    let s_f = Float::from(s.to_i64().unwrap_or(0) as f64);
                    match c + &s_f {
                        Ok(r) => Value::Float(r),
                        Err(_) => return Value::new_error("OverflowError", "float overflow", None),
                    }
                }
                _ => return Value::new_error("TypeError", "unsupported numeric types", None),
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
            Err(_) => return Value::new_error("TypeError", "failed to convert int to float for real part", None),
        },
        Some(Value::Float(f)) => f.clone(),
        Some(_) => return Value::new_error("TypeError", "real part must be int or float", None),
        None => return Value::new_error("TypeError", "real part is required", None),
    };
    let imag = match args.get("imaginary") {
        Some(Value::Int(i)) => match i.to_float() {
            Ok(f) => f,
            Err(_) => return Value::new_error("TypeError", "failed to convert int to float for imaginary part", None),
        },
        Some(Value::Float(f)) => f.clone(),
        Some(_) => return Value::new_error("TypeError", "imaginary part must be int or float", None),
        None => return Value::new_error("TypeError", "imaginary part is required", None),
    };
    Value::Float(Float::complex(real, imag))
}

fn __placeholder__(_args: &HashMap<String, Value>) -> Value {
    Value::new_error("PlaceholderError", "This is a placeholder function and should not be called.", None)
}

// -------------------------------
// Utility Functions
pub fn format_value(value: &Value, interpreter: &mut Interpreter) -> String {
    match value {
        Value::Float(n) => n.to_str(),
        Value::Int(n) => format_int(&n.clone()),
        Value::String(s) => s.clone(),
        Value::Boolean(b) => b.to_string(),
        Value::Null => "null".to_string(),
        Value::Map(map) => {
            let formatted_pairs: Vec<String> = map
                .iter()
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
        Value::Generator(gener) => {
            format!("<generator '{}' at {:p}>", gener.name().unwrap_or("<unnamed>"), gener)
        }
        Value::Error(e) => {
            format!("<{}: {}>", e.err_type, e.err_msg)
        }
        Value::Module(obj) => {
            format!("<module '{}' from '{}' at {:p}>", obj.name(), fix_path(obj.path().display().to_string()), obj.ptr())
        }
        Value::Struct(s) => {
            let mut var = Variable::new_pt(s.name().to_string(), Value::Struct(s.clone()), s.get_type(), false, false, false);
            if let Ok(method) = find_struct_method(&s, None, "op_display", &[s.get_type()], &Type::new_simple("str")) {
                if method.is_static() {
                    return format_value_dbg(value);
                }
                let result = interpreter.call_function(&method, vec![], HashMap::new(), Some((None, None, Some(&mut var))));
                if interpreter.err.is_some() {
                    return format_value_dbg(value);
                }
                return result.to_string();
            } else {
                return format_value_dbg(value);
            }
        }
        _ => format_value_dbg(value),
    }
}

// -------------------------------
// Function Definitions
pub fn print_fn() -> Function {
    Function::SharedNative(Arc::new(SharedNativeFunction::new(
        "print",
        print,
        vec![  
            Parameter::variadic_optional("args", "any", Value::String("".to_string())), 
            Parameter::positional_optional("end", "str", Value::String("".to_string())),
            Parameter::positional_optional("sep", "str", Value::String(" ".to_string())),
        ],
        "void",
        true, true, true,
        None,
        EffectFlags::IO,
        "Prints the given arguments to the console. Accepts multiple values and an optional separator and end string.\nExample:\n    print(1, 2, 3, sep='-') // '1-2-3'\nBy default, the separator is a space and the end string is empty. You can also specify end='\\n' to add a newline at the end of the output.",
    )))
}

pub fn println_fn() -> Function {
    Function::SharedNative(Arc::new(SharedNativeFunction::new(
        "println",
        println,
        vec![
            Parameter::variadic_optional("args", "any", Value::String("".to_string())), 
            Parameter::positional_optional("sep", "str", Value::String(" ".to_string())),
        ],
        "void",
        true, true, true,
        None,
        EffectFlags::IO,
        "Prints the given arguments to the console, followed by a newline. Accepts multiple values and an optional separator.\nExample:\n    println(1, 2, 3, sep='-') // '1-2-3'",
    )))
}

pub fn styled_print_fn() -> Function {
    Function::SharedNative(Arc::new(SharedNativeFunction::new(
        "styled_print",
        styled_print,
        vec![
            Parameter::variadic_optional("args", "any", Value::String("".to_string())),
            Parameter::keyword_optional("sep", "str", Value::String(" ".to_string())),
            Parameter::keyword_optional("end", "str", Value::String("\n\x1b[0m".to_string())),
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
        "Prints the given arguments to the console with ANSI styling.\n\nOptions:\n- sep: Separator between values (default: ' ')\n- end: String appended after the last value (default: '\\n\\x1b[0m')\n- fg_color: Foreground color (color name or hex code, default: 'reset')\n- bg_color: Background color (color name or hex code, default: 'reset')\n- bold, italic, underline, blink, reverse, strikethrough: Boolean flags to enable text styles (default: false)\n- link: If provided, makes the text a clickable hyperlink with the given URL.",
    )))
}

pub fn input_fn() -> Function {
    Function::SharedNative(Arc::new(SharedNativeFunction::new(
        "input",
        input,
        vec![
            Parameter::positional_optional("prompt", "str", Value::String("".to_string())),
            Parameter::positional_optional_pt("multiline", &Type::new_simple("?str"), Value::Null),
            Parameter::keyword_optional("err", "bool", Value::Boolean(false)),
        ],
        "str",
        true, true, true,
        None,
        EffectFlags::IO,
        "Reads a line of input from the user. If 'multiline' string is provided it uses it as a multiline prompt, allowing multiple lines of input. If 'err' is true, on error it throws the error instead of returning an empty string.",
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
        "Returns the length of the given value.\n\nSupports strings, lists, tuples, maps, bytes, and structs with an 'op_len' method. For strings, it returns the number of characters. For lists and tuples, it returns the number of elements. For maps, it returns the number of key-value pairs. For bytes, it returns the number of bytes. For structs, it looks for an 'op_len' method and calls it if found; otherwise, it raises a TypeError.",
    )))
}

pub fn help_fn() -> Function {
    Function::Native(Arc::new(NativeFunction::new(
        "help",
        help,
        vec![
            Parameter::positional_optional("value", "any", Value::Null),
        ],
        "void",
        true, true, true,
        None,
        EffectFlags::IO,
        "Provides help information about a specific value or general usage instructions if no value is provided. If a value is given, it will display detailed information about that value, including its type, methods, and properties. If no value is provided, it will show a welcome message and basic instructions for using the REPL.",
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
        "Returns the type of the given object as a string.\nExamples:\n    type_of(123) // 'int'\n    type_of([1, 2, 3]) // 'list'\nThis function is useful for debugging and introspection purposes.",
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
        "Returns the size of the given object in bytes. The size is an approximation and may not reflect the actual memory usage, especially for complex objects. For simple types like int, float, bool, and null, it returns a fixed size. For strings, lists, tuples, maps, and bytes, it returns the size of the contained data plus some overhead. For structs and modules, it includes the size of their fields and methods. Note that this function is primarily intended for informational purposes and should not be used for precise memory management.",
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
        "Sums up all the numeric values in the provided list, including nested lists and tuples. Non-numeric values will cause a TypeError.",
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
        "Returns the Unicode code point of the first character in the given string. If the string is empty, it returns a TypeError.",
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
        "Converts an integer Unicode code point to its corresponding character. The input integer should represent a valid Unicode code point (0 to 0x10FFFF). If the input is valid, the function returns a string containing the corresponding character. If the input is invalid (e.g., out of range), it returns a TypeError.",
    )))
}

pub fn styledstr_fn() -> Function {
    Function::SharedNative(Arc::new(SharedNativeFunction::new(
        "styledstr",
        styledstr,
        vec![
            Parameter::variadic_optional("args", "any", Value::String("".to_string())),
            Parameter::keyword_optional("sep", "str", Value::String(" ".to_string())),
            Parameter::keyword_optional("end", "str", Value::String("\x1b[0m".to_string())),
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
        "Returns the styled string from the arguments using ANSI styling.\n\nOptions:\n- sep: Separator between values (default: ' ')\n- end: String appended after the last value (default: '\\n\\x1b[0m')\n- fg_color: Foreground color (color name or hex code, default: 'reset')\n- bg_color: Background color (color name or hex code, default: 'reset')\n- bold, italic, underline, blink, reverse, strikethrough: Boolean flags to enable text styles (default: false)\n- link: If provided, makes the text a clickable hyperlink with the given URL.",
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
        "Creates a list of the specified size, filled with the given initial value (or null if not provided).",
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
        "Similar to python's range() but with more flexibility. It can generate a sequence of numbers from 'a' to 'b' with a specified 'step'. If only 'a' is provided, it generates numbers from 0 to 'a'. The 'step' can be positive or negative but cannot be zero. By default, it returns a list, but if the 'as' parameter is set to 'generator', it returns a generator instead. The function supports both integers and floats for the range parameters.",
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
        "Makes a complex number from the given real and imaginary parts. Both parts can be integers or floats. The result is a complex number represented as a float with special handling for the imaginary part.",
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
        "This is a placeholder function and should not be called.",
    )))
}

fn generate_methods_for_default_types() -> FxHashMap<(String, String), Function> {
    let mut methods: FxHashMap<(String, String), Function> = FxHashMap::with_capacity_and_hasher(64, Default::default());

    let to_string = {
        make_native_method(
            "to_string",
            "Convert the value to its string representation. This method is available on all types and provides a human-readable representation of the value. For complex types like lists, maps, and structs, this will produce a string that shows the structure and content of the value. For primitive types like integers and booleans, it will return their standard string representation.",
            move |value, _args| {
                Value::String(value.to_string())
            },
            vec![],
            "str",
            true, false, true,
            None,
        )
    };
    let clone = {
        make_native_method(
            "clone",
            "Create a deep copy of the value. For primitive types, this will simply return the same value. For complex types like lists, maps, and structs, this will create a new instance with the same content. Note that for structs, this will only clone the struct itself and not any internal references it may contain.",
            move |value, _args| {
                match &value {
                    Value::Pointer(inner_arc) => {
                        let inner_guard = inner_arc.lock();
                        let (inner_value, counter) = &*inner_guard;
                        let cloned_value = inner_value.clone();
                        Value::Pointer(Arc::new(Mutex::new((cloned_value, *counter))))
                    },
                    Value::Null => Value::Null,
                    _ => value.clone(),
                }
            },
            vec![],
            "any",
            true, false, true,
            None,
        )
    };

    let is_null = {
        make_native_method(
            "is_null",
            "Check if the value is specifically null.",
            move |value, _args| {
                Value::Boolean(value.is_null())
            },
            vec![],
            "bool",
            true, false, true,
            None,
        )
    };

    let is_truthy = {
        make_native_method(
            "is_truthy",
            "Check if the value is truthy (not null, not false, not empty).",
            move |value, _args| {
                Value::Boolean(value.is_truthy())
            },
            vec![],
            "bool",
            true, false, true,
            None,
        )
    };

    let is_some = {
        make_native_method(
            "is_some",
            "Same as is_truthy().",
            move |value, _args| {
                Value::Boolean(value.is_truthy())
            },
            vec![],
            "bool",
            true, false, true,
            None,
        )
    };

    methods.extend([
        (("to_string".to_owned(), "any".to_owned()), to_string),
        (("clone".to_owned(), "any".to_owned()), clone),
        (("is_null".to_owned(), "any".to_owned()), is_null),
        (("is_truthy".to_owned(), "any".to_owned()), is_truthy),
        (("is_some".to_owned(), "any".to_owned()), is_some),
    ]);

    // STRING METHODS

    let to_bytes = {
        make_native_method(
            "to_bytes",
            "Convert the string to a byte array using UTF-8 encoding. This method is only applicable to string values. If the value is not a string, it will return null.",
            move |value, _args| {
                match value.to_bytes() {
                    Some(bytes) => Value::Bytes(bytes),
                    None => Value::Null,
                }
            },
            vec![],
            "bytes",
            true, false, true,
            None,
        )
    };

    let endswith = {
        make_native_method(
            "endswith",
            "Check if the string ends with the specified suffix.",
            move |value, args| {
                if let Some(suffix_value) = args.get("suffix") {
                    match (value, suffix_value) {
                        (Value::String(s), Value::String(suffix)) => {
                            Value::Boolean(s.ends_with(&*suffix))
                        }
                        _ => Value::new_error("TypeError", "endswith() expects a string suffix", None),
                    }
                } else {
                    Value::new_error("TypeError", "endswith() missing required argument 'suffix'", None)
                }
            },
            vec![
                Parameter::positional("suffix", "str"),
            ],
            "bool",
            true, false, true,
            None,
        )
    };

    let startswith = {
        make_native_method(
            "startswith",
            "Check if the string starts with the specified prefix.",
            move |value, args| {
                if let Some(prefix_value) = args.get("prefix") {
                    dbg!(&prefix_value, &value);
                    let res = match (value, prefix_value) {
                        (Value::String(s), Value::String(prefix)) => {
                            Value::Boolean(s.starts_with(&*prefix))
                        }
                        _ => Value::new_error("TypeError", "startswith() expects a string prefix", None),
                    };
                    dbg!(&res);
                    res
                } else {
                    Value::new_error("TypeError", "startswith() missing required argument 'prefix'", None)
                }
            },
            vec![
                Parameter::positional("prefix", "str"),
            ],
            "bool",
            true, false, true,
            None,
        )
    };

    let split = {
        make_native_method(
            "split",
            "Split the string into a list of substrings based on a specified delimiter. The delimiter is provided as an argument to the method. If the delimiter is not found in the string, the result will be a list containing the original string as its only element.",
            move |value, args| {
                if let Some(delim_value) = args.get("delimiter") {
                    match (value, delim_value) {
                        (Value::String(s), Value::String(delim)) => {
                            let parts: Vec<Value> = s.split(delim.as_str())
                                .map(|part| Value::String(part.to_string()))
                                .collect();
                            Value::List(parts)
                        }
                        _ => Value::new_error("TypeError", "split() expects a string delimiter", None),
                    }
                } else {
                    Value::new_error("TypeError", "split() missing required argument 'delimiter'", None)
                }
            },
            vec![
                Parameter::positional("delimiter", "str"),
            ],
            "list",
            true, false, true,
            None,
        )
    };

    let split_lines = {
        make_native_method(
            "split_lines",
            "Split the string into lines based on newline characters. Different types of newline characters (\\n, \\r\\n, \\r) are all recognized as line separators.",
            move |value, _args| {
                match value {
                    Value::String(s) => {
                        let parts: Vec<Value> = s.lines()
                            .map(|part| Value::String(part.to_string()))
                            .collect();
                        Value::List(parts)
                    }
                    _ => Value::new_error("TypeError", "split_lines() can only be called on strings", None),
                }
            },
            vec![],
            "list",
            true, false, true,
            None,
        )
    };

    let split_words = {
        make_native_method(
            "split_words",
            "Split the string into words based on whitespace. Consecutive whitespace characters are treated as a single separator.",
            move |value, _args| {
                match value {
                    Value::String(s) => {
                        let parts: Vec<Value> = s.split_whitespace()
                            .map(|part| Value::String(part.to_string()))
                            .collect();
                        Value::List(parts)
                    }
                    _ => Value::new_error("TypeError", "split_words() can only be called on strings", None),
                }
            },
            vec![],
            "list",
            true, false, true,
            None,
        )
    };

    let join = {
        make_native_method(
            "join",
            "Join a list of strings into a single string, using the current string as the separator.\nExample:\n    ', '.join(['apple', 'banana', 'cherry']) // 'apple, banana, cherry'",
            move |value, args| {
                if let Some(parts_value) = args.get("parts") {
                    match (value, parts_value) {
                        (Value::String(s), Value::List(parts)) => {
                            let joined: String = parts.iter()
                                .filter_map(|v| v.to_string().into())
                                .collect::<Vec<String>>()
                                .join(&s);
                            Value::String(joined)
                        }
                        _ => Value::new_error("TypeError", "join() expects a list of strings", None),
                    }
                } else {
                    Value::new_error("TypeError", "join() missing required argument 'parts'", None)
                }
            },
            vec![
                Parameter::positional("parts", "list"),
            ],
            "str",
            true, false, true,
            None,
        )
    };

    let trim = {
        make_native_method(
            "trim",
            "Return a copy of the string with leading and trailing whitespace removed.",
            move |value, _args| {
                match value {
                    Value::String(s) => Value::String(s.trim().to_string()),
                    _ => Value::new_error("TypeError", "trim() can only be called on strings", None),
                }
            },
            vec![],
            "str",
            true, false, true,
            None,
        )
    };

    let chars = {
        make_native_method_pt(
            "chars",
            "Return a list of characters in the string. Each character is returned as a string of length 1.\nNote: It is planned to add a 'char' type in the future, and when that happens, this method will return a list of 'char' instead of 'str[1]'.",
            move |value, _args| {
                match value {
                    Value::String(s) => {
                        let char_list: Vec<Value> = s.chars().map(|c| Value::String(c.to_string())).collect();
                        Value::List(char_list)
                    }
                    _ => Value::new_error("TypeError", "chars() can only be called on strings", None),
                }
            },
            vec![],
            &Type::new_list(Type::new_simple("str")),
            true, false, true,
            None,
        )
    };

    let isalpha = {
        make_native_method(
            "isalpha",
            "Check if all characters in the string are alphabetic (a-z, A-Z).",
            move |value, _args| {
                match value {
                    Value::String(s) => Value::Boolean(s.chars().all(|c| c.is_alphabetic())),
                    _ => Value::new_error("TypeError", "isalpha() can only be called on strings", None),
                }
            },
            vec![],
            "bool",
            true, false, true,
            None,
        )
    };

    let isdigit = {
        make_native_method(
            "isdigit",
            "Check if all characters in the string are digits (0-9).",
            move |value, _args| {
                match value {
                    Value::String(s) => Value::Boolean(s.chars().all(|c| c.is_digit(10))),
                    _ => Value::new_error("TypeError", "isdigit() can only be called on strings", None),
                }
            },
            vec![],
            "bool",
            true, false, true,
            None,
        )
    };

    let islower = {
        make_native_method(
            "islower",
            "Check if all characters in the string are lowercase letters.",
            move |value, _args| {
                match value {
                    Value::String(s) => Value::Boolean(s.chars().any(|c| c.is_lowercase()) && s.chars().all(|c| !c.is_uppercase())),
                    _ => Value::new_error("TypeError", "islower() can only be called on strings", None),
                }
            },
            vec![],
            "bool",
            true, false, true,
            None,
        )
    };

    let isupper = {
        make_native_method(
            "isupper",
            "Check if all characters in the string are uppercase (and there is at least one uppercase character).",
            move |value, _args| {
                match value {
                    Value::String(s) => Value::Boolean(s.chars().any(|c| c.is_uppercase()) && s.chars().all(|c| !c.is_lowercase())),
                    _ => Value::new_error("TypeError", "isupper() can only be called on strings", None),
                }
            },
            vec![],
            "bool",
            true, false, true,
            None,
        )
    };

    let isalnum = {
        make_native_method(
            "isalnum",
            "Check if all characters in the string are alphanumeric (letters or digits).",
            move |value, _args| {
                match value {
                    Value::String(s) => Value::Boolean(s.chars().all(|c| c.is_alphanumeric())),
                    _ => Value::new_error("TypeError", "isalnum() can only be called on strings", None),
                }
            },
            vec![],
            "bool",
            true, false, true,
            None,
        )
    };

    let isspace = {
        make_native_method(
            "isspace",
            "Check if all characters in the string are whitespace (spaces, tabs, newlines).",
            move |value, _args| {
                match value {
                    Value::String(s) => Value::Boolean(s.chars().all(|c| c.is_whitespace())),
                    _ => Value::new_error("TypeError", "isspace() can only be called on strings", None),
                }
            },
            vec![],
            "bool",
            true, false, true,
            None,
        )
    };

    let isnumeric = {
        make_native_method(
            "isnumeric",
            "Check if all characters in the string are numeric (0-9).",
            move |value, _args| {
                match value {
                    Value::String(s) => Value::Boolean(s.chars().all(|c| c.is_numeric())),
                    _ => Value::new_error("TypeError", "isnumeric() can only be called on strings", None),
                }
            },
            vec![],
            "bool",
            true, false, true,
            None,
        )
    };

    let replace = {
        make_native_method(
            "replace",
            "Replace occurrences of a substring in a string with a new substring. Takes two required arguments: 'old' (the substring to replace) and 'new' (the substring to replace with). Optional keyword arguments: 'count' (the maximum number of replacements to perform, default is -1 for all occurrences), 'case_sensitive' (whether the replacement should be case-sensitive, default is true), 'from_left' (whether to replace from the left side, default is true), and 'from_right' (whether to replace from the right side, default is false). If both 'from_left' and 'from_right' are false, replacements will be made from left to right.",
            move |value, args| {
                if let (Some(old_value), Some(new_value)) = (args.get("old"), args.get("new")) {
                    match (value, old_value, new_value) {
                        (Value::String(s), Value::String(old), Value::String(new)) => {
                            let count = match args.get("count") {
                                Some(Value::Int(i)) => {
                                    if *i < 0.into() { usize::MAX } else { i.to_usize().unwrap_or(usize::MAX) }
                                }
                                _ => usize::MAX,
                            };

                            let case_sensitive = match args.get("case_sensitive") {
                                Some(Value::Boolean(b)) => *b,
                                _ => true,
                            };

                            let from_left = match args.get("from_left") {
                                Some(Value::Boolean(b)) => *b,
                                _ => true,
                            };

                            let from_right = match args.get("from_right") {
                                Some(Value::Boolean(b)) => *b,
                                _ => false,
                            };

                            let result = if case_sensitive {
                                if from_right {
                                    let parts = s.rsplitn(count.saturating_add(1), old).collect::<Vec<_>>();
                                    parts.join(new)
                                } else if from_left {
                                    let parts = s.splitn(count.saturating_add(1), old).collect::<Vec<_>>();
                                    parts.join(new)
                                } else {
                                    s.replacen(old, new, count)
                                }
                            } else {
                                let mut result = s.clone();
                                let old_lower = old.to_lowercase();

                                if from_left || from_right {
                                    let mut indices = vec![];
                                    let mut pos = 0;

                                    let lower = result.to_lowercase();
                                    while let Some(found) = lower[pos..].find(&old_lower) {
                                        indices.push(pos + found);
                                        pos = pos + found + old.len();
                                    }

                                    if from_right {
                                        indices.reverse();
                                    }

                                    let mut replaced = String::with_capacity(result.len());
                                    let mut last = 0;
                                    let mut performed = 0;

                                    for idx in indices {
                                        if performed >= count {
                                            break;
                                        }
                                        replaced.push_str(&result[last..idx]);
                                        replaced.push_str(new);
                                        last = idx + old.len();
                                        performed += 1;
                                    }

                                    replaced.push_str(&result[last..]);
                                    result = replaced;
                                } else {
                                    let mut lower = result.to_lowercase();
                                    let mut performed = 0;
                                    let mut start = 0;

                                    while let Some(pos) = lower[start..].find(&old_lower) {
                                        if performed >= count {
                                            break;
                                        }
                                        let real = start + pos;
                                        result.replace_range(real..real + old.len(), new);

                                        lower = result.to_lowercase();
                                        start = real + new.len();
                                        performed += 1;
                                    }
                                }

                                result
                            };

                            Value::String(result)
                        }
                        _ => Value::new_error("TypeError", "replace() expects string arguments", None),
                    }
                } else {
                    Value::new_error("TypeError", "replace() missing required arguments 'old' and 'new'", None)
                }
            },
            vec![
                Parameter::positional("old", "str"),
                Parameter::positional("new", "str"),
                Parameter::positional_optional("count", "int", Value::Int((-1).into())),
                Parameter::keyword_optional("case_sensitive", "bool", Value::Boolean(true)),
                Parameter::keyword_optional("from_left", "bool", Value::Boolean(true)),
                Parameter::keyword_optional("from_right", "bool", Value::Boolean(false)),
            ],
            "str",
            true, false, true,
            None,
        )
    };

    let replace_more = {
        make_native_method(
            "replace_more",
            "Replace multiple substrings in a string based on a map of replacements. The keys of the map are the substrings to be replaced, and the values are the substrings to replace with.",
            move |value, args| {
                if let Some(replacements_value) = args.get("replacements") {
                    match (value, replacements_value) {
                        (Value::String(s), Value::Map(replacements)) => {
                            let mut result = s.clone();
                            for (old_value, new_value) in replacements {
                                if let (Value::String(old), Value::String(new)) = (old_value, new_value) {
                                    result = result.replace(old, new);
                                } else {
                                    return Value::new_error("TypeError", "replace_more() expects a map of string to string replacements", None);
                                }
                            }
                            Value::String(result)
                        }
                        _ => Value::new_error("TypeError", "replace_more() expects a map of replacements", None),
                    }
                } else {
                    Value::new_error("TypeError", "replace_more() missing required argument 'replacements'", None)
                }
            },
            vec![
                Parameter::positional("replacements", "map"),
            ],
            "str",
            true, false, true,
            None,
        )
    };

    let to_upper = {
        make_native_method(
            "to_upper",
            "Convert a string to uppercase.",
            move |value, _args| {
                match value {
                    Value::String(s) => Value::String(s.to_uppercase()),
                    _ => Value::new_error("TypeError", "to_upper() can only be called on strings", None),
                }
            },
            vec![],
            "str",
            true, false, true,
            None,
        )
    };

    let to_lower = {
        make_native_method(
            "to_lower",
            "Convert a string to lowercase.",
            move |value, _args| {
                match value {
                    Value::String(s) => Value::String(s.to_lowercase()),
                    _ => Value::new_error("TypeError", "to_lower() can only be called on strings", None),
                }
            },
            vec![],
            "str",
            true, false, true,
            None,
        )
    };

    let ord = {
        make_native_method(
            "ord",
            "Return the Unicode code point of a single character string. If the string is empty or has more than one character, throws a ValueError.",
            move |value, _args| {
                match value {
                    Value::String(s) => {
                        let mut chars = s.chars();
                        if let Some(c) = chars.next() {
                            if chars.next().is_none() {
                                Value::Int((c as u32).into())
                            } else {
                                Value::new_error("ValueError", "ord() expected a single character string", None)
                            }
                        } else {
                            Value::new_error("ValueError", "ord() expected a non-empty string", None)
                        }
                    }
                    _ => Value::new_error("TypeError", "ord() can only be called on strings", None),
                }
            },
            vec![],
            "int",
            true, false, true,
            None,
        )
    };

    let to_int = {
        make_native_method(
            "to_int",
            "Convert a string to an int. If the string is not a valid integer, throws a ValueError.",
            move |value, _args| {
                match value {
                    Value::String(s) => {
                        match Int::from_str(s) {
                            Ok(i) => Value::Int(i),
                            Err(_) => Value::new_error("ValueError", "Failed to convert string to int", None),
                        }
                    }
                    _ => Value::new_error("TypeError", "to_int() can only be called on strings", None),
                }
            },
            vec![],
            "int",
            true, false, true,
            None,
        )
    };

    let to_float = {
        make_native_method(
            "to_float",
            "Convert a string to a float. If the string is not a valid float, throws a ValueError.",
            move |value, _args| {
                match value {
                    Value::String(s) => {
                        match Float::from_str(s) {
                            Ok(f) => Value::Float(f),
                            Err(_) => Value::new_error("ValueError", "Failed to convert string to float", None),
                        }
                    }
                    _ => Value::new_error("TypeError", "to_float() can only be called on strings", None),
                }
            },
            vec![],
            "float",
            true, false, true,
            None,
        )
    };

    methods.extend([
        (("to_bytes".to_owned(), "str".to_owned()), to_bytes),
        (("endswith".to_owned(), "str".to_owned()), endswith),
        (("startswith".to_owned(), "str".to_owned()), startswith),
        (("split".to_owned(), "str".to_owned()), split),
        (("split_lines".to_owned(), "str".to_owned()), split_lines),
        (("split_words".to_owned(), "str".to_owned()), split_words.clone()),
        (("split_whitespace".to_owned(), "str".to_owned()), split_words),
        (("join".to_owned(), "str".to_owned()), join),
        (("trim".to_owned(), "str".to_owned()), trim),
        (("chars".to_owned(), "str".to_owned()), chars),
        (("isalpha".to_owned(), "str".to_owned()), isalpha),
        (("isdigit".to_owned(), "str".to_owned()), isdigit),
        (("islower".to_owned(), "str".to_owned()), islower),
        (("isupper".to_owned(), "str".to_owned()), isupper),
        (("isalnum".to_owned(), "str".to_owned()), isalnum),
        (("isspace".to_owned(), "str".to_owned()), isspace),
        (("isnumeric".to_owned(), "str".to_owned()), isnumeric),
        (("replace".to_owned(), "str".to_owned()), replace),
        (("replace_more".to_owned(), "str".to_owned()), replace_more),
        (("to_upper".to_owned(), "str".to_owned()), to_upper),
        (("to_lower".to_owned(), "str".to_owned()), to_lower),
        (("ord".to_owned(), "str".to_owned()), ord),
        (("to_int".to_owned(), "str".to_owned()), to_int),
        (("to_float".to_owned(), "str".to_owned()), to_float),
    ]);

    // INT METHODS

    let to_float = {
        make_native_method(
            "to_float",
            "Convert an int to a float. If the int is too large to fit in a float, an error is returned.",
            move |value, _args| {
                match value {
                    Value::Int(i) => match i.to_float() {
                        Ok(f) => Value::Float(f),
                        Err(_) => Value::new_error("TypeError", "Failed to convert int to float", None),
                    },
                    _ => Value::new_error("TypeError", "to_float() can only be called on int", None),
                }
            },
            vec![],
            "float",
            true, false, true,
            None,
        )
    };

    let format = {
        make_native_method(
            "format",
            "Return a string representation of the int with formatting options.\n\n\nSupported keyword arguments:\n- sep (str): Character to use as a separator for thousands (default: '_')\n- width (int): Minimum width of the resulting string, padded with the pad character if necessary (default: 0)\n- pad (str): Character to use for padding if width is specified (default: ' ')\n- show_plus (bool): Whether to show a plus sign for positive numbers (default: false)\n- base (int): Numerical base for formatting (default: 10, other options: 2, 8, 16)\n- abbreviate (bool): Whether to abbreviate large numbers with K, M, B, etc. (default: false)\n- paren_neg (bool): Whether to enclose negative numbers in parentheses instead of using a minus sign (default: false)",
            move |value, args| {
                if let Value::Int(val) = value {
                    let sep = if let Some(Value::String(s)) = args.get("sep") {
                        s.as_str()
                    } else {
                        "_"
                    };
            
                    let width = if let Some(Value::Int(i)) = args.get("width") {
                        i.to_i64().unwrap_or(0)
                    } else {
                        0
                    };
            
                    let pad = if let Some(Value::String(p)) = args.get("pad") {
                        p.chars().next().unwrap_or(' ')
                    } else {
                        ' '
                    };
            
                    let show_plus = if let Some(Value::Boolean(b)) = args.get("show_plus") {
                        *b
                    } else {
                        false
                    };
            
                    let base = if let Some(Value::Int(i)) = args.get("base") {
                        i.to_i64().unwrap_or(10)
                    } else {
                        10
                    };
            
                    let abbreviate = if let Some(Value::Boolean(b)) = args.get("abbreviate") {
                        *b
                    } else {
                        false
                    };
            
                    let paren_neg = if let Some(Value::Boolean(b)) = args.get("paren_neg") {
                        *b
                    } else {
                        false
                    };
            
                    let mut s = match base {
                        2 => format!("0b{:b}", val),
                        8 => format!("0o{:o}", val),
                        16 => format!("0x{:x}", val),
                        _ => val.to_string(),
                    };
            
                    if abbreviate {
                        let mut f = val.to_i64().unwrap_or(0) as f64;
                        let units = ["", "K", "M", "B", "T"];
                        let mut idx = 0;
                        while f.abs() >= 1000.0 && idx < units.len() as i64 - 1 {
                            f /= 1000.0;
                            idx += 1;
                        }
                        s = format!("{:.2}{}", f, units[idx as usize]);
                    } else if base == 10 {
                        let val_str = s.clone();
                        let chars: Vec<char> = val_str.chars().collect();
                        let (start, negative) = if chars.get(0) == Some(&'-') {
                            (1, true)
                        } else {
                            (0, false)
                        };
                        let mut result = String::with_capacity(s.len() + (s.len() / 3));
                        let mut count = 0;
                        for i in (start..chars.len() as i64).rev() {
                            if count > 0 && count % 3 == 0 {
                                result = format!("{}{}", sep, result);
                            }
                            result.insert(0, chars[i as usize]);
                            count += 1;
                        }
                        if negative {
                            result.insert(0, '-');
                        }
                        s = result;
                    }
                    if paren_neg && s.starts_with('-') {
                        s = format!("({})", &s[1..]);
                    } else if show_plus && !s.starts_with('-') {
                        s = format!("+{}", s);
                    }
                    if width > s.len() as i64 {
                        let padding = std::iter::repeat(pad).take((width - s.len() as i64).try_into().unwrap()).collect::<String>();
                        s = format!("{}{}", padding, s);
                    }
                    Value::String(s)
                } else {
                    Value::new_error("TypeError", "format() can only be called on int", None)
                }
            },
            vec![
                Parameter::positional_optional("sep", "str", Value::String("_".to_string())),
                Parameter::positional_optional("width", "int", Value::Int(0.into())),
                Parameter::positional_optional("pad", "str", Value::String(" ".to_string())),
                Parameter::positional_optional("show_plus", "bool", Value::Boolean(false)),
                Parameter::positional_optional("base", "int", Value::Int(10.into())),
                Parameter::positional_optional("abbreviate", "bool", Value::Boolean(false)),
                Parameter::positional_optional("paren_neg", "bool", Value::Boolean(false)),
            ],
            "str",
            true, false, true,
            None,
        )
    };

    let display = {
        make_native_method(
            "display",
            "Return a string representation of the int without any formatting, just the digits.",
            move |value, _args| {
                Value::String(value.to_string())
            },
            vec![],
            "str",
            true, false, true,
            None,
        )
    };

    let is_even = {
        make_native_method(
            "is_even",
            "Return true if the int is even, false if odd.",
            move |value, _args| {
                match value {
                    Value::Int(i) => Value::Boolean((&*i % Int::from(2)).map_or(false, |r| r.is_zero())),
                    _ => Value::new_error("TypeError", "is_even() can only be called on int", None),
                }
            },
            vec![],
            "bool",
            true, false, true,
            None,
        )
    };

    let is_odd = {
        make_native_method(
            "is_odd",
            "Return true if the int is odd, false if even.",
            move |value, _args| {
                match value {
                    Value::Int(i) => Value::Boolean(!(&*i % Int::from(2)).map_or(false, |r| r.is_zero())),
                    _ => Value::new_error("TypeError", "is_odd() can only be called on int", None),
                }
            },
            vec![],
            "bool",
            true, false, true,
            None,
        )
    };

    methods.extend([
        (("to_float".to_owned(), "int".to_owned()), to_float),
        (("format".to_owned(), "int".to_owned()), format),
        (("display".to_owned(), "int".to_owned()), display),
        (("is_even".to_owned(), "int".to_owned()), is_even),
        (("is_odd".to_owned(), "int".to_owned()), is_odd),
    ]);

    // FLOAT METHODS

    let to_int = {
        make_native_method(
            "to_int",
            "Convert the float to an int, truncating towards zero. Different than .round(), same as 'as int'.",
            move |value, _args| {
                match value {
                    Value::Float(f) => match f.to_int() {
                        Ok(i) => Value::Int(i),
                        Err(_) => Value::new_error("TypeError", "Failed to convert Float to Int", None),
                    },
                    _ => Value::new_error("TypeError", "to_int() can only be called on float", None),
                }
            },
            vec![],
            "int",
            true, false, true,
            None,
        )
    };

    let format = {
        make_native_method(
            "format",
            "Return a string representation of the float with formatting options.\n\nOptions:\n- sep: character to use as a thousands separator (default '_')\n- precision: number of decimal places (default 6)\n- width: minimum width of the resulting string, padded with 'pad' (default 0)\n- pad: character to use for padding if width is specified (default ' ')\n- show_plus: whether to show a '+' sign for positive numbers (default false)\n- scientific: whether to use scientific notation (default false)\n- abbreviate: whether to abbreviate large numbers with K/M/B/T suffixes (default false)\n- paren_neg: whether to enclose negative numbers in parentheses instead of using a '-' sign (default false)",
            move |value, args| {
                if let Value::Float(val) = value {
                    let sep = if let Some(Value::String(s)) = args.get("sep") {
                        s.as_str()
                    } else {
                        "_"
                    };
            
                    let precision = if let Some(Value::Int(i)) = args.get("precision") {
                        i.to_i64().unwrap_or(6)
                    } else {
                        6
                    };
            
                    let width = if let Some(Value::Int(i)) = args.get("width") {
                        i.to_i64().unwrap_or(0)
                    } else {
                        0
                    };
            
                    let pad = if let Some(Value::String(p)) = args.get("pad") {
                        p.chars().next().unwrap_or(' ')
                    } else {
                        ' '
                    };
            
                    let show_plus = if let Some(Value::Boolean(b)) = args.get("show_plus") {
                        *b
                    } else {
                        false
                    };
            
                    let scientific = if let Some(Value::Boolean(b)) = args.get("scientific") {
                        *b
                    } else {
                        false
                    };
            
                    let abbreviate = if let Some(Value::Boolean(b)) = args.get("abbreviate") {
                        *b
                    } else {
                        false
                    };
            
                    let paren_neg = if let Some(Value::Boolean(b)) = args.get("paren_neg") {
                        *b
                    } else {
                        false
                    };
            
                    let mut f = val.to_f64().unwrap_or(0.0);
                    let mut s = if scientific {
                        format!("{:.*e}", precision as usize, f)
                    } else if abbreviate {
                        let units = ["", "K", "M", "B", "T"];
                        let mut idx = 0;
                        while f.abs() >= 1000.0 && idx < units.len() as i64 - 1 {
                            f /= 1000.0;
                            idx += 1;
                        }
                        format!("{:.2}{}", f, units[idx as usize])
                    } else {
                        format!("{:.*}", precision as usize, f)
                    };
            
                    if s.contains('.') {
                        s = s.trim_end_matches('0').trim_end_matches('.').to_string();
                    }
                    if !scientific && !abbreviate && s.contains('.') {
                        let parts: Vec<&str> = s.split('.').collect();
                        let int_part = parts[0];
                        let frac_part = parts.get(1).copied().unwrap_or("");
                        let chars: Vec<char> = int_part.chars().collect();
            
                        let (start, negative) = if chars.get(0) == Some(&'-') {
                            (1, true)
                        } else {
                            (0, false)
                        };
            
                        let mut result = String::new();
                        let mut count = 0;
                        for i in (start..chars.len() as usize).rev() {
                            if count > 0 && count % 3 == 0 {
                                result = format!("{}{}", sep, result);
                            }
                            result.insert(0, chars[i]);
                            count += 1;
                        }
                        if negative {
                            result.insert(0, '-');
                        }
            
                        result.push('.');
                        result.push_str(frac_part);
                        s = result;
                    }
                    if paren_neg && s.starts_with('-') {
                        s = format!("({})", &s[1..]);
                    } else if show_plus && !s.starts_with('-') {
                        s = format!("+{}", s);
                    }
                    if width > s.len() as i64 {
                        let padding = std::iter::repeat(pad).take((width - s.len() as i64).try_into().unwrap()).collect::<String>();
                        s = format!("{}{}", padding, s);
                    }
                    Value::String(s)
                } else {
                    Value::new_error("TypeError", "format() can only be called on float", None)
                }
            },
            vec![
                Parameter::positional_optional("sep", "str", Value::String("_".to_string())),
                Parameter::positional_optional("precision", "int", Value::Int(6.into())),
                Parameter::positional_optional("width", "int", Value::Int(0.into())),
                Parameter::positional_optional("pad", "str", Value::String(" ".to_string())),
                Parameter::positional_optional("show_plus", "bool", Value::Boolean(false)),
                Parameter::positional_optional("scientific", "bool", Value::Boolean(false)),
                Parameter::positional_optional("abbreviate", "bool", Value::Boolean(false)),
                Parameter::positional_optional("paren_neg", "bool", Value::Boolean(false)),
            ],
            "str",
            true, false, true,
            None,
        )
    };

    let display = {
        make_native_method(
            "display",
            "Return a string representation of the float for display purposes. Similar to format() but with some differences in how it handles precision and formatting. Mainly intended for user-facing output rather than precise control over formatting.",
            move |value, _args| {
                Value::String(value.to_string())
            },
            vec![],
            "str",
            true, false, true,
            None,
        )
    };

    let round = {
        make_native_method(
            "round",
            "Round the float to a given precision in decimal digits (default 0 digits). Returns a new float and does not modify original.",
            move |value, args| {
                let precision = if let Some(Value::Int(i)) = args.get("precision") {
                    i.to_i64().unwrap_or(0)
                } else {
                    0
                };
        
                if let Value::Float(val) = value {
                    return Value::Float(val.round(precision as usize));
                }
        
                Value::new_error("TypeError", "round() can only be called on float", None)
            },
            vec![Parameter::positional_optional("precision", "int", Value::Int(0.into()))],
            "float",
            true, false, true,
            None,
        )
    };

    methods.extend([
        (("to_int".to_owned(), "float".to_owned()), to_int),
        (("format".to_owned(), "float".to_owned()), format),
        (("display".to_owned(), "float".to_owned()), display),
        (("round".to_owned(), "float".to_owned()), round),
    ]);

    // LIST METHODS

    let append = {
        make_native_method(
            "append",
            "Return a new list with the item appended to the end. Does not modify original list.",
            move |value, args| {
                if let Some(item) = args.get("item") {
                    match value {
                        Value::List(list) => {
                            let mut new_list = list.clone();
                            new_list.push(item.clone());
                            Value::List(new_list)
                        }
                        _ => Value::new_error("TypeError", "append() can only be called on lists", None),
                    }
                } else {
                    Value::new_error("TypeError", "append() missing required argument 'item'", None)
                }
            },
            vec![Parameter::positional("item", "any")],
            "list",
            true, false, true,
            None,
        )
    };

    let push = {
        make_native_method(
            "push",
            "Add an item to the end of the list. Modifies list in place.",
            |value, args| {
                if let Some(item) = args.get("item") {
                    match value {
                        Value::List(list) => {
                            list.push(item.clone());
                            Value::Null
                        }
                        _ => Value::new_error("TypeError", "push() can only be called on lists", None),
                    }
                } else {
                    Value::new_error("TypeError", "push() missing required argument 'item'", None)
                }
            },
            vec![Parameter::positional("item", "any")],
            "void",
            true, false, true,
            None,
        )
    };

    let pop = {
        make_native_method(
            "pop",
            "Remove and return the last item from the list. Modifies list in place.",
            |value, _args| {
                match value {
                    Value::List(list) => {
                        if let Some(item) = list.pop() {
                            item
                        } else {
                            Value::new_error("IndexError", "pop() from empty list", None)
                        }
                    }
                    _ => Value::new_error("TypeError", "pop() can only be called on lists", None),
                }
            },
            vec![],
            "any",
            true, false, true,
            None,
        )
    };

    let extend_list = {
        make_native_method(
            "extend",
            "Extend the list by appending all the items from another list. Modifies list in place.",
            |value, args| {
                if let Some(Value::List(to_extend)) = args.get("item") {
                    match value {
                        Value::List(list) => {
                            list.extend(to_extend.clone());
                            Value::Null
                        }
                        _ => Value::new_error("TypeError", "extend() can only be called on lists", None),
                    }
                } else {
                    Value::new_error("TypeError", "extend() missing required argument 'item'", None)
                }
            },
            vec![Parameter::positional("item", "list")],
            "void",
            true, false, true,
            None,
        )
    };

    let into_list = {
        make_native_method(
            "into",
            "Convert the items in the list into a the type specified. Only works with primitive types and uses same rules as 'as'.",
            move |value, args| {
                let type_ = args
                    .get("ty")
                    .and_then(|v| if let Value::Type(t) = v { Some(t.display_simple()) } else { None })
                    .unwrap_or_else(|| "any".to_string());

                let item_vec = match value {
                    Value::List(list) => list.clone(),
                    _ => return Value::new_error("TypeError", "into() can only be called on lists", None),
                };

                let mut list = Vec::with_capacity(item_vec.len());
                for item in item_vec {
                    match convert_value_to_type(&type_, &item) {
                        Ok(converted) => list.push(converted),
                        Err((err_type, err_msg, _)) => return Value::new_error(err_type, err_msg, None),
                    }
                }

                Value::List(list)
            },
            vec![Parameter::positional("ty", "type")],
            "list",
            true, false, true,
            None,
        )
    };

    let into_gen = {
        make_native_method(
            "into_gen",
            "Convert the list into a generator that yields the items in the list one at a time.",
            move |value, _args| {
                let vec = match value {
                    Value::List(list) => list.clone(),
                    _ => return Value::new_error("TypeError", "into_gen() can only be called on lists", None),
                };
                
                let vec_iter = VecIter::new(&vec);

                let generator = Generator::new_anonymous(
                    GeneratorType::Native(NativeGenerator {
                        iter: Box::new(vec_iter),
                        iteration: 0,
                    }),
                    false,
                );

                Value::Generator(generator)
            },
            vec![],
            "generator",
            true, false, true,
            None,
        )
    };

    let into_repeating_gen = {
        make_native_method(
            "into_repeating_gen",
            "Create a new generator that repeats the items of the original generator indefinitely",
            move |value, _args| {
                let vec = match value {
                    Value::List(list) => list.clone(),
                    _ => return Value::new_error("TypeError", "into_repeating_gen() can only be called on lists", None),
                };
                
                let vec_iter = VecIter::new(&vec);
                let generator = Generator::new_anonymous(
                    GeneratorType::Native(NativeGenerator {
                        iter: Box::new(vec_iter),
                        iteration: 0,
                    }),
                    false,
                );
                let repeating_iter = RepeatingIter::new(&generator);
                let repeating_generator = Generator::new_anonymous(
                    GeneratorType::Native(NativeGenerator {
                        iter: Box::new(repeating_iter),
                        iteration: 0,
                    }),
                    false,
                );

                Value::Generator(repeating_generator)
            },
            vec![],
            "generator",
            true, false, true,
            None,
        )
    };

    let enumerate_list = {
        make_native_method(
            "enumerate",
            "Return a generator that yields pairs of (index, item) for each item in the list, where index is the position of the item in the list starting from 0.",
            move |value, _args| {
                let vec = match value {
                    Value::List(list) => list.clone(),
                    _ => return Value::new_error("TypeError", "enumerate() can only be called on lists", None),
                };
                let vec_enumerated = vec.iter().enumerate().map(|(i, v)| {
                    Value::Tuple(vec![Value::Int(create_int(&(i as i64).to_string())), v.clone()])
                }).collect::<Vec<Value>>();
                let vec_iter = VecIter::new(&vec_enumerated);

                let generator = Generator::new_anonymous(
                    GeneratorType::Native(NativeGenerator {
                        iter: Box::new(vec_iter),
                        iteration: 0,
                    }),
                    false,
                );

                Value::Generator(generator)
            },
            vec![],
            "generator",
            true, false, true,
            None,
        )
    };

    let map_list = {
        make_native_shared_fn(
            "map",
            "Apply the provided function to each item in the list, returning a generator that yields the results. The function should take a single argument, which will be each item from the list in turn.",
            move |args, interpreter| {
                let value = args.get("self").cloned().unwrap_or(Value::Null);
                if let Some(Value::Function(func)) = args.get("f") {
                    let vec = match value {
                        Value::List(list) => list.clone(),
                        _ => return Value::new_error("TypeError", "map() can only be called on lists", None),
                    };
                    let vec_iter = VecIter::new(&vec);
                    let vec_gen = Generator::new_anonymous(
                        GeneratorType::Native(NativeGenerator {
                            iter: Box::new(vec_iter),
                            iteration: 0,
                        }),
                        false,
                    );
                    let map_iter = MapIter::new(&vec_gen, func.clone(), interpreter);
                    let generator = Generator::new_anonymous(
                        GeneratorType::Native(NativeGenerator {
                            iter: Box::new(map_iter),
                            iteration: 0,
                        }),
                        false,
                    );
                    return Value::Generator(generator);
                } else {
                    return Value::new_error("TypeError", "map() missing required argument 'f'", None);
                }
            },
            vec![Parameter::positional("f", "function")],
            "generator",
            true, false, true,
            None,
        )
    };

    let filter_list = {
        make_native_shared_fn(
            "filter",
            "Filter the items in the list using the provided function, which should return true for items that should be included in the output generator.",
            move |args, interpreter| {
                let value = args.get("self").cloned().unwrap_or(Value::Null);
                if let Some(Value::Function(func)) = args.get("f") {
                    let vec = match value {
                        Value::List(list) => list.clone(),
                        _ => return Value::new_error("TypeError", "filter() can only be called on lists", None),
                    };
                    let vec_iter = VecIter::new(&vec);
                    let vec_gen = Generator::new_anonymous(
                        GeneratorType::Native(NativeGenerator {
                            iter: Box::new(vec_iter),
                            iteration: 0,
                        }),
                        false,
                    );
                    let filter_iter = FilterIter::new(&vec_gen, func.clone(), interpreter);
                    let generator = Generator::new_anonymous(
                        GeneratorType::Native(NativeGenerator {
                            iter: Box::new(filter_iter),
                            iteration: 0,
                        }),
                        false,
                    );
                    return Value::Generator(generator);
                } else {
                    return Value::new_error("TypeError", "filter() missing required argument 'f'", None);
                }
            },
            vec![Parameter::positional("f", "function")],
            "generator",
            true, false, true,
            None,
        )
    };

    let sort = {
        let function_signature = parse_type("function[any] -> any");
        make_native_shared_fn(
            "sort",
            "Sort the items in the list using the provided key function. If 'reverse' is true, the list will be sorted in reverse order.",
            move |args, interpreter| {
                let value = args.get("self").cloned().unwrap_or(Value::Null);
                let reverse = matches!(args.get("reverse"), Some(Value::Boolean(true)));

                let function: Option<_> = match args.get("f") {
                    Some(Value::Function(func)) => Some(func.clone()),
                    Some(Value::Null) | None => None,
                    _ => return Value::new_error("RuntimeError", "Expected 'f' to be a function or null", None),
                };

                let items = match value {
                    Value::List(list) => list.clone(),
                    _ => return Value::new_error("TypeError", "sort() can only be called on lists", None),
                };
                let mut keys = Vec::with_capacity(items.len());
                if let Some(function) = function {
                    for item in &items {
                        let key_res = interpreter.call_function(
                            &function,
                            vec![item.clone()],
                            std::collections::HashMap::default(),
                            None,
                        );

                        if interpreter.err.is_some() {
                            return interpreter.err.take().unwrap().to_value();
                        }

                        keys.push(key_res);
                    }
                } else {
                    keys = items.clone();
                }
                let mut indices: Vec<usize> = (0..items.len()).collect();

                indices.sort_by(|&i, &j| {
                    let ord = keys[i]
                        .partial_cmp(&keys[j])
                        .unwrap_or(std::cmp::Ordering::Equal);
                    if reverse { ord.reverse() } else { ord }
                });
                let sorted_items: Vec<Value> = indices.into_iter().map(|i| items[i].clone()).collect();
                Value::List(sorted_items)
            },
            vec![
                Parameter::positional_optional_pt("f", &function_signature, Value::Null),
                Parameter::keyword_optional("reverse", "bool", Value::Boolean(false)),
            ],
            "list",
            true, false, true,
            None,
        )
    };
    let sort_by = {
        let function_signature = parse_type("function[any, any] -> bool");
        make_native_shared_fn(
            "sort_by",
            "Sort the items in the list using the provided comparison function, which should return true if the first argument is less than the second. If 'reverse' is true, the function should return true if the first argument is greater than the second.",
            move |args, interpreter| {
                let value = args.get("self").cloned().unwrap_or(Value::Null);
                let reverse = matches!(args.get("reverse"), Some(Value::Boolean(true)));

                let function = match args.get("f") {
                    Some(Value::Function(func)) => func.clone(),
                    _ => {
                        return Value::new_error(
                            "RuntimeError",
                            "Expected 'f' to be a function",
                            None,
                        )
                    }
                };

                let items: Vec<Value> = match &value {
                    Value::List(list) => list.clone(),
                    _ => return Value::new_error("TypeError", "Expected a list", None),
                };

                let mut indices: Vec<usize> = (0..items.len()).collect();

                let cache: Arc<Mutex<FxHashMap<(usize, usize), std::cmp::Ordering>>> =
                    Arc::new(Mutex::new(FxHashMap::default()));

                let mut had_error: Option<crate::Error> = None;

                let functione = |&i: &usize, &j: &usize| {
                    if had_error.is_some() {
                        return std::cmp::Ordering::Equal;
                    }

                    {
                        let cache_guard = cache.lock();
                        if let Some(&ord) = cache_guard.get(&(i, j)) {
                            return ord;
                        }
                    }

                    let res = {
                        let result = interpreter.call_function(
                            &function,
                            vec![items[i].clone(), items[j].clone()],
                            std::collections::HashMap::default(),
                            None,
                        );

                        if let Some(err) = interpreter.err.take() {
                            had_error = Some(err);
                        };
                        
                        result
                    };

                    let mut ord = match res {
                        Value::Boolean(true) => std::cmp::Ordering::Less,
                        Value::Boolean(false) => std::cmp::Ordering::Greater,
                        _ => std::cmp::Ordering::Equal,
                    };

                    if reverse {
                        ord = ord.reverse();
                    };

                    {
                        let mut cache_guard = cache.lock();
                        cache_guard.insert((i, j), ord);
                        cache_guard.insert((j, i), ord.reverse());
                    }

                    ord
                };

                if args.get("unstable") == Some(&Value::Boolean(true)) {
                    indices.sort_unstable_by(functione);
                } else {
                    indices.sort_by(functione);
                }

                if had_error.is_some() {
                    return had_error.unwrap().to_value();
                }

                let sorted_items: Vec<Value> =
                    indices.into_iter().map(|i| items[i].clone()).collect();

                Value::List(sorted_items)
            },
            vec![
                Parameter::positional_pt("f", &function_signature),
                Parameter::positional_optional("reverse", "bool", Value::Boolean(false)),
                Parameter::positional_optional("unstable", "bool", Value::Boolean(false)),
            ],
            "list",
            false,
            true,
            true,
            None,
        )
    };
    let all = {
        make_native_shared_fn(
            "all",
            "Return true if all items in the list are truthy (or if the optional function returns true for all items), false otherwise.",
            move |args, interpreter| {
                let value = args.get("self").cloned().unwrap_or(Value::Null);
                let function: Option<_> = match args.get("f") {
                    Some(Value::Function(func)) => Some(func.clone()),
                    Some(Value::Null) | None => None,
                    _ => return Value::new_error("RuntimeError", "Expected 'f' to be a function or null", None),
                };

                let items = match value {
                    Value::List(list) => list.clone(),
                    _ => return Value::new_error("TypeError", "all() can only be called on lists", None),
                };

                for item in &items {
                    let result = if let Some(function) = &function {
                        let res = interpreter.call_function(
                            function,
                            vec![item.clone()],
                            std::collections::HashMap::default(),
                            None,
                        );

                        if interpreter.err.is_some() {
                            return interpreter.err.take().unwrap().to_value();
                        }

                        res
                    } else {
                        item.clone()
                    };

                    match result {
                        Value::Boolean(b) => {
                            if !b {
                                return Value::Boolean(false);
                            }
                        }
                        _ => return Value::new_error("TypeError", "Function must return a boolean", None),
                    }
                }

                Value::Boolean(true)
            },
            vec![Parameter::positional_optional("f", "function", Value::Null)],
            "bool",
            true, false, true,
            None,
        )
    };
    let clear = {
        make_native_method(
            "clear",
            "Remove all items from the list, leaving it empty.",
            |value, _args| {
                match value {
                    Value::List(list) => {
                        list.clear();
                        Value::Null
                    }
                    _ => Value::new_error("TypeError", "clear() can only be called on lists", None),
                }
            },
            vec![],
            "void",
            true, false, true,
            None,
        )
    };
    let contains = {
        make_native_method(
            "contains",
            "Return true if the specified item is in the list, false otherwise.",
            move |value, args| {
                if let Some(item) = args.get("item") {
                    match value {
                        Value::List(list) => {
                            for elem in list {
                                if elem == item {
                                    return Value::Boolean(true);
                                }
                            }
                            Value::Boolean(false)
                        }
                        _ => Value::new_error("TypeError", "contains() can only be called on lists", None),
                    }
                } else {
                    Value::new_error("TypeError", "contains() missing required argument 'item'", None)
                }
            },
            vec![Parameter::positional("item", "any")],
            "bool",
            true, false, true,
            None,
        )
    };
    let index_of = {
        make_native_method(
            "index_of",
            "Return the index of the first occurrence of the specified item in the list, or -1 if the item is not found. An optional start argument specifies the index to start the search from.",
            move |value, args| {
                if let Some(item) = args.get("item") {
                    match value {
                        Value::List(list) => {
                            let start = if let Some(Value::Int(i)) = args.get("start") {
                                i.to_usize().unwrap_or(0)
                            } else {
                                0
                            };
                            for (i, elem) in list.iter().enumerate().skip(start) {
                                if elem == item {
                                    return Value::Int(create_int(&(i as i64).to_string()));
                                }
                            }
                            Value::new_error("ValueError", "Item not found in list", None)
                        }
                        _ => Value::new_error("TypeError", "index_of() can only be called on lists", None),
                    }
                } else {
                    Value::new_error("TypeError", "index_of() missing required argument 'item'", None)
                }
            },
            vec![
                Parameter::positional("item", "any"),
                Parameter::positional_optional("start", "int", Value::Int(0.into())),
            ],
            "int",
            true, false, true,
            None,
        )
    };
    let undup = {
        make_native_method(
            "undup",
            "Return a new list with duplicate items removed, preserving the original order of items.",
            move |value, _args| {
                match value {
                    Value::List(list) => {
                        let mut seen = FxHashSet::with_capacity_and_hasher(list.len(), Default::default());
                        let mut unduped = Vec::with_capacity(list.len());
                        for item in list {
                            if seen.insert(item.clone()) {
                                unduped.push(item.clone());
                            }
                        }
                        Value::List(unduped)
                    }
                    _ => Value::new_error("TypeError", "undup() can only be called on lists", None),
                }
            },
            vec![],
            "list",
            true, false, true,
            None,
        )
    };
    let count = {
        make_native_method(
            "count",
            "Return the number of occurrences of the specified item in the list.",
            move |value, args| {
                if let Some(item) = args.get("item") {
                    match value {
                        Value::List(list) => {
                            let mut count = 0;
                            for elem in list {
                                if elem == item {
                                    count += 1;
                                }
                            }
                            Value::Int(create_int(&count.to_string()))
                        }
                        _ => Value::new_error("TypeError", "count() can only be called on lists", None),
                    }
                } else {
                    Value::new_error("TypeError", "count() missing required argument 'item'", None)
                }
            },
            vec![Parameter::positional("item", "any")],
            "int",
            true, false, true,
            None,
        )
    };
    let reverse = {
        make_native_method(
            "reverse",
            "Reverse the items of the list in place.",
            |value, _args| {
                match value {
                    Value::List(list) => {
                        list.reverse();
                        Value::Null
                    }
                    _ => Value::new_error("TypeError", "reverse() can only be called on lists", None),
                }
            },
            vec![],
            "list",
            true, false, true,
            None,
        )
    };
    let reversed = {
        make_native_method(
            "reversed",
            "Return a new list containing the items of the original list in reverse order.",
            |value, _args| {
                match value {
                    Value::List(list) => {
                        let reversed_list: Vec<Value> = list.iter().rev().cloned().collect();
                        Value::List(reversed_list)
                    }
                    _ => Value::new_error("TypeError", "reversed() can only be called on lists", None),
                }
            },
            vec![],
            "list",
            true, false, true,
            None,
        )
    };
    let reduce = {
        make_native_shared_fn(
            "reduce",
            "Apply a function of two arguments cumulatively to the items of a list, from left to right, reducing the list to a single value. If an initial value is provided, it is placed before the items of the list in the calculation, and serves as a default when the list is empty.",
            move |args, interpreter| {
                let value = args.get("self").cloned().unwrap_or(Value::Null);
                if let Some(Value::Function(func)) = args.get("f") {
                    let list = match value {
                        Value::List(list) => list.clone(),
                        _ => return Value::new_error("TypeError", "reduce() can only be called on lists", None),
                    };
                    if list.is_empty() {
                        return Value::new_error("ValueError", "Cannot reduce an empty list", None);
                    }
                    let mut iter = list.iter();
                    let mut accumulator = match args.get("initial") {
                        Some(initial) if *initial != Value::Null => initial.clone(),
                        _ => iter.next().unwrap().clone(),
                    };
                    for item in iter {
                        accumulator = interpreter.call_function(
                            func,
                            vec![accumulator, item.clone()],
                            std::collections::HashMap::default(),
                            None,
                        );
                        if interpreter.err.is_some() {
                            return interpreter.err.take().unwrap().to_value();
                        }
                    }
                    return accumulator;
                } else {
                    return Value::new_error("TypeError", "reduce() missing required argument 'f'", None);
                }
            },
            vec![
                Parameter::positional("f", "function"),
                Parameter::positional_optional("initial", "any", Value::Null)
            ],
            "any",
            true, false, true,
            None,
        )
    };
    let is_sorted = {
        make_native_method(
            "is_sorted",
            "Return true if the list is sorted in non-decreasing order, and false otherwise.",
            |value, _args| {
                match value {
                    Value::List(list) => {
                        for i in 1..list.len() {
                            if list[i - 1] > list[i] {
                                return Value::Boolean(false);
                            }
                        }
                        Value::Boolean(true)
                    }
                    _ => Value::new_error("TypeError", "is_sorted() can only be called on lists", None),
                }
            },
            vec![],
            "bool",
            true, false, true,
            None,
        )
    };
    let get = {
        make_native_method(
            "get",
            "Return the item at the specified index in the list, or a default value if the index is out of bounds. Supports negative indexing, where -1 refers to the last item, -2 to the second-to-last, and so on.",
            move |value, args| {
                let index = if let Some(Value::Int(i)) = args.get("index") {
                    i.to_isize().unwrap_or(0)
                } else {
                    return Value::new_error("TypeError", "get() missing required argument 'index'", None);
                };
        
                let default = args.get("default").cloned().unwrap_or(Value::Null);
        
                match value {
                    Value::List(list) => {
                        let idx = if index < 0 {
                            list.len() as isize + index
                        } else {
                            index
                        };
                        if idx >= 0 && (idx as usize) < list.len() {
                            list[idx as usize].clone()
                        } else {
                            default
                        }
                    }
                    _ => Value::new_error("TypeError", "get() can only be called on lists", None),
                }
            },
            vec![
                Parameter::positional("index", "int"),
                Parameter::positional_optional("default", "any", Value::Null),
            ],
            "any",
            true, false, true,
            None,
        )
    };

    methods.extend([
        (("append".to_owned(), "list".to_owned()), append),
        (("push".to_owned(), "list".to_owned()), push),
        (("pop".to_owned(), "list".to_owned()), pop),
        (("extend".to_owned(), "list".to_owned()), extend_list),
        (("into".to_owned(), "list".to_owned()), into_list),
        (("into_gen".to_owned(), "list".to_owned()), into_gen.clone()),
        (("iter".to_owned(), "list".to_owned()), into_gen),
        (("into_repeating_gen".to_owned(), "list".to_owned()), into_repeating_gen.clone()),
        (("repeating_iter".to_owned(), "list".to_owned()), into_repeating_gen),
        (("enumerate".to_owned(), "list".to_owned()), enumerate_list),
        (("map".to_owned(), "list".to_owned()), map_list),
        (("filter".to_owned(), "list".to_owned()), filter_list),
        (("sort".to_owned(), "list".to_owned()), sort),
        (("sort_by".to_owned(), "list".to_owned()), sort_by),
        (("all".to_owned(), "list".to_owned()), all),
        (("clear".to_owned(), "list".to_owned()), clear),
        (("contains".to_owned(), "list".to_owned()), contains),
        (("index_of".to_owned(), "list".to_owned()), index_of),
        (("undup".to_owned(), "list".to_owned()), undup.clone()),
        (("dedup".to_owned(), "list".to_owned()), undup),
        (("count".to_owned(), "list".to_owned()), count),
        (("reverse".to_owned(), "list".to_owned()), reverse),
        (("reversed".to_owned(), "list".to_owned()), reversed),
        (("reduce".to_owned(), "list".to_owned()), reduce),
        (("is_sorted".to_owned(), "list".to_owned()), is_sorted),
        (("get".to_owned(), "list".to_owned()), get),
    ]);

    // TUPLE METHODS

    let to_list = {
        make_native_method(
            "to_list",
            "Convert the tuple into a list. This is a shallow conversion, so if the tuple contains mutable objects, they will be shared between the tuple and the resulting list.",
            move |value, _args| {
                if let Value::Tuple(tuple) = value {
                    return Value::List(tuple.to_vec());
                }
                Value::Null
            },
            vec![],
            "list",
            true, false, true,
            None,
        )
    };

    let enumerate_tuple = {
        make_native_method(
            "enumerate",
            "Return a generator that yields pairs of (index, value) for each item in the tuple.",
            move |value, _args| {
                let vec = match value {
                    Value::Tuple(v) => v.clone(),
                    _ => return Value::new_error("TypeError", "Expected a tuple", None),
                };
                let vec_enumerated = vec.iter().enumerate().map(|(i, v)| {
                    Value::Tuple(vec![Value::Int(create_int(&(i as i64).to_string())), v.clone()])
                }).collect::<Vec<Value>>();
                let vec_iter = VecIter::new(&vec_enumerated);

                let generator = Generator::new_anonymous(
                    GeneratorType::Native(NativeGenerator {
                        iter: Box::new(vec_iter),
                        iteration: 0,
                    }),
                    false,
                );

                Value::Generator(generator)
            },
            vec![],
            "generator",
            true, false, true,
            None,
        )
    };

    methods.extend([
        (("to_list".to_owned(), "tuple".to_owned()), to_list),
        (("enumerate".to_owned(), "tuple".to_owned()), enumerate_tuple),
    ]);

    // POINTER METHODS

    let extract_ptr = {
        make_native_method(
            "extract_ptr",
            "Extract the raw pointer value as an integer. This is unsafe and should be used with caution, as it can lead to undefined behavior if misused. The pointer is valid as long as there are references to it in the program, but once all references are dropped, the pointer may become invalid. Use this method only if you know what you're doing and understand the risks involved.",
            move |value, _args| {
                match value {
                    Value::Pointer(ptr_arc) => {
                        let raw_ptr: *const Mutex<(Value, usize)> = Arc::as_ptr(ptr_arc);
                        Value::Int((raw_ptr as usize).into())
                    }
                    _ => Value::Null,
                }
            },
            vec![],
            "int",
            true, false, true,
            None,
        )
    };

    methods.extend([
        (("extract_ptr".to_owned(), "&any".to_owned()), extract_ptr),
    ]);

    // GENERATOR METHODS

    let collect = {
        make_native_method(
            "collect",
            "Convert the generator into a list. If the generator is infinite, return an error.",
            move |value, _args| {
                if let Value::Generator(generator) = value {
                    if !generator.is_infinite() {
                        let v = generator.to_vec();
                        if let Some(Value::Error(e)) = v.iter().find(|item| matches!(item, Value::Error(..))) {
                            return Value::Error(e.clone());
                        }
                        return Value::List(v);
                    } else {
                        return Value::new_error("TypeError", "Cannot convert infinite generator to list", None);
                    }
                }
                Value::Null
            },
            vec![],
            "list",
            true, false, true,
            None,
        )
    };

    let collect_into = {
        make_native_method(
            "collect_into",
            "Convert the generator into a list of the specified type. If any item cannot be converted to the specified type, return an error.",
            move |value, args| {
                let type_ = args
                    .get("ty")
                    .and_then(|v| if let Value::Type(t) = v { Some(t.display_simple()) } else { None })
                    .unwrap_or_else(|| "any".to_string());

                let item_vec = match value {
                    Value::Generator(generator) if !generator.is_infinite() => {
                        let v = generator.to_vec();
                        if let Some(Value::Error(e)) = v.iter().find(|item| matches!(item, Value::Error(..))) {
                            return Value::Error(e.clone());
                        }
                        v
                    }
                    Value::Generator(_) => return Value::new_error("TypeError", "Cannot convert infinite generator to list", None),
                    _ => return Value::new_error("TypeError", "Expected a generator", None),
                };

                let mut list = Vec::with_capacity(item_vec.len());
                for item in item_vec {
                    match convert_value_to_type(&type_, &item) {
                        Ok(converted) => list.push(converted),
                        Err((err_type, err_msg, _)) => return Value::new_error(err_type, err_msg, None),
                    }
                }

                Value::List(list)
            },
            vec![Parameter::positional("ty", "type")],
            "list",
            true, false, true,
            None,
        )
    };

    let next = {
        make_native_method(
            "next",
            "Return the next value from the generator. If the generator is done, return an error.",
            move |value, _args| {
                if let Value::Generator(generator) = value {
                    if let Some(next_value) = generator.next() {
                        return next_value;
                    } else {
                        return Value::new_error("StopIteration", "No more items in generator", None);
                    }
                }
                Value::Null
            },
            vec![],
            "any",
            true, false, true,
            None,
        )
    };

    let is_done = {
        make_native_method(
            "is_done",
            "Return true if the generator is done (i.e., has no more items to yield), false otherwise.",
            move |value, _args| {
                if let Value::Generator(generator) = value {
                    return Value::Boolean(generator.is_done());
                }
                Value::Null
            },
            vec![],
            "bool",
            true, false, true,
            None,
        )
    };

    let peek = {
        make_native_method(
            "peek",
            "Return the next value from the generator without advancing it. If the generator is done, return an error.",
            move |value, _args| {
                if let Value::Generator(generator) = value {
                    if let Some(peeked_value) = generator.peek() {
                        return peeked_value;
                    } else {
                        return Value::new_error("StopIteration", "No more items in generator", None);
                    }
                }
                Value::Null
            },
            vec![],
            "any",
            true, false, true,
            None,
        )
    };

    let enumerate_gen = {
        make_native_method(
            "enumerate",
            "Create a new generator that yields pairs of (index, value) from the original generator",
            move |value, _args| {
                let generator = match value {
                    Value::Generator(generator) => generator,
                    _ => return Value::new_error("TypeError", "Expected a generator", None),
                };
                let enumerate_iter = EnumerateIter::new(generator);

                let generator = Generator::new_anonymous(
                    GeneratorType::Native(NativeGenerator {
                        iter: Box::new(enumerate_iter),
                        iteration: 0,
                    }),
                    false,
                );

                Value::Generator(generator)
            },
            vec![],
            "generator",
            true, false, true,
            None,
        )
    };

    let map_gen = {
        make_native_shared_fn(
            "map",
            "Create a new generator that applies a given function to each item from the original generator. The function should take one argument (the item) and return the transformed value.",
            move |args, interpreter| {
                let value = args.get("self").cloned().unwrap_or(Value::Null);
                if let Some(Value::Function(func)) = args.get("f") {
                    let generator = match value {
                        Value::Generator(generator) => generator,
                        _ => return Value::new_error("TypeError", "map() can only be called on generators", None),
                    };
                    let map_iter = MapIter::new(&generator, func.clone(), interpreter);
                    let generator = Generator::new_anonymous(
                        GeneratorType::Native(NativeGenerator {
                            iter: Box::new(map_iter),
                            iteration: 0,
                        }),
                        false,
                    );
                    return Value::Generator(generator);
                } else {
                    return Value::new_error("TypeError", "map() missing required argument 'f'", None);
                }
            },
            vec![Parameter::positional("f", "function")],
            "generator",
            true, false, true,
            None,
        )
    };

    let filter_gen = {
        make_native_shared_fn(
            "filter",
            "Create a new generator that yields only the items from the original generator for which a given function returns true. The function should take one argument (the item) and return a boolean.",
            move |args, interpreter| {
                let value = args.get("self").cloned().unwrap_or(Value::Null);
                if let Some(Value::Function(func)) = args.get("f") {
                    let generator = match value {
                        Value::Generator(generator) => generator,
                        _ => return Value::new_error("TypeError", "filter() can only be called on generators", None),
                    };
                    let filter_iter = FilterIter::new(&generator, func.clone(), interpreter);
                    let generator = Generator::new_anonymous(
                        GeneratorType::Native(NativeGenerator {
                            iter: Box::new(filter_iter),
                            iteration: 0,
                        }),
                        false,
                    );
                    return Value::Generator(generator);
                } else {
                    return Value::new_error("TypeError", "filter() missing required argument 'f'", None);
                }
            },
            vec![Parameter::positional("f", "function")],
            "generator",
            true, false, true,
            None,
        )
    };

    let repeating = {
        make_native_method(
            "repeating",
            "Create a new generator that repeats the items of the original generator indefinitely",
            move |value, _args| {
                let generator = match value {
                    Value::Generator(generator) => generator,
                    _ => return Value::new_error("TypeError", "repeating() can only be called on generators", None),
                };
                let repeating_iter = RepeatingIter::new(generator);
                let repeating_generator = Generator::new_anonymous(
                    GeneratorType::Native(NativeGenerator {
                        iter: Box::new(repeating_iter),
                        iteration: 0,
                    }),
                    false,
                );

                Value::Generator(repeating_generator)
            },
            vec![],
            "generator",
            true, false, true,
            None,
        )
    };

    let take = {
        make_native_method(
            "take",
            "Create a new generator that yields only the first n items from the original generator",
            move |value, args| {
                if let Value::Generator(generator) = value {
                    if let Some(Value::Int(n)) = args.get("n") {
                        let n = n.to_usize().unwrap_or(0);
                        let taken_values: Vec<Value> = generator.take(n as usize);
                        let vec_iter = VecIter::new(&taken_values);
                        let generator = Generator::new_anonymous(
                            GeneratorType::Native(NativeGenerator {
                                iter: Box::new(vec_iter),
                                iteration: 0,
                            }),
                            false,
                        );
                        return Value::Generator(generator);
                    } else {
                        return Value::new_error("TypeError", "Expected 'n' to be an integer", None);
                    }
                }
                Value::Null
            },
            vec![Parameter::positional("n", "int")],
            "generator",
            true, false, true,
            None,
        )
    };

    methods.extend([
        (("collect".to_owned(), "generator".to_owned()), collect),
        (("collect_into".to_owned(), "generator".to_owned()), collect_into),
        (("next".to_owned(), "generator".to_owned()), next),
        (("is_done".to_owned(), "generator".to_owned()), is_done),
        (("peek".to_owned(), "generator".to_owned()), peek),
        (("enumerate".to_owned(), "generator".to_owned()), enumerate_gen),
        (("map".to_owned(), "generator".to_owned()), map_gen),
        (("filter".to_owned(), "generator".to_owned()), filter_gen),
        (("repeating".to_owned(), "generator".to_owned()), repeating),
        (("take".to_owned(), "generator".to_owned()), take),
    ]);

    // MAP METHODS

    let get = {
        make_native_method(
            "get",
            "Get the value associated with a key in a map, or return a default value if the key is not present",
            move |value, args| {
                if let Some(key) = args.get("key") {
                    if let Value::Map(map) = value {
                        if let Some(val) = map.get(key) {
                            return val.clone();
                        } else if let Some(default) = args.get("default") {
                            return default.clone();
                        }
                        return Value::Null;
                    }
                }
                Value::Null
            },
            vec![Parameter::positional("key", "any"), Parameter::positional_optional("default", "any", Value::Null)],
            "any",
            true, false, true,
            None,
        )
    };

    let filter_map = {
        make_native_shared_fn(
            "filter",
            "Create a new map containing only the key-value pairs from the original map for which a given function returns true. The function should take two arguments (key and value) and return a boolean.",
            move |args, interpreter| {
                let value = args.get("self").cloned().unwrap_or(Value::Null);
                if let Some(Value::Function(func)) = args.get("f") {
                    let map = match value {
                        Value::Map(map) => map,
                        _ => return Value::new_error("TypeError", "Expected a map", None),
                    };
                    let new_map: FxHashMap<Value, Value> = map.iter().filter_map(|(key, val)| {
                        let result = interpreter.call_function(
                            func,
                            vec![key.clone(), val.clone()],
                            HashMap::default(),
                            None
                        );
                        if result.is_truthy() {
                            Some((key.clone(), val.clone()))
                        } else {
                            None
                        }
                    }).collect();
                    return Value::Map(new_map);
                } else {
                    return Value::new_error("TypeError", "Expected 'f' to be a function", None);
                }
            },
            vec![Parameter::positional("f", "function")],
            "map",
            true, false, true,
            None,
        )
    };

    let map_map = {
        make_native_shared_fn(
            "map",
            "Create a new map by applying a function to each key-value pair in the original map. The function should take two arguments (key and value) and return the new value for that key.",
            move |args, interpreter| {
                let value = args.get("self").cloned().unwrap_or(Value::Null);
                if let Some(Value::Function(func)) = args.get("f") {
                    let map = match value {
                        Value::Map(map) => map,
                        _ => return Value::new_error("TypeError", "Expected a map", None),
                    };
                    let new_map: FxHashMap<Value, Value> = map.iter().map(|(key, val)| {
                        let new_val = interpreter.call_function(
                            func,
                            vec![key.clone(), val.clone()],
                            HashMap::default(),
                            None
                        );
                        (key.clone(), new_val)
                    }).collect();
                    return Value::Map(new_map);
                } else {
                    return Value::new_error("TypeError", "Expected 'f' to be a function", None);
                }
            },
            vec![Parameter::positional("f", "function")],
            "map",
            true, false, true,
            None,
        )
    };

    let keys = {
        make_native_method(
            "keys",
            "Get the keys of a map as a list",
            move |value, _args| {
                if let Value::Map(map) = value {
                    return Value::List(map.keys().cloned().collect());
                }
                Value::Null
            },
            vec![],
            "list",
            true, false, true,
            None,
        )
    };

    let values = {
        make_native_method(
            "values",
            "Get a list of all values in a map",
            move |value, _args| {
                if let Value::Map(map) = value {
                    return Value::List(map.values().cloned().collect());
                }
                Value::Null
            },
            vec![],
            "list",
            true, false, true,
            None,
        )
    };

    let zip = {
        make_native_method(
            "zip",
            "Zip a map into a list of key-value tuple pairs",
            move |value, _args| {
                if let Value::Map(map) = value {
                    let zipped: Vec<Value> = map.iter().map(|(k, v)| {
                        Value::Tuple(vec![k.clone(), v.clone()])
                    }).collect();
                    return Value::List(zipped);
                }
                Value::Null
            },
            vec![],
            "list",
            true, false, true,
            None,
        )
    };

    let contains_key = {
        make_native_method(
            "contains_key",
            "Check if a map contains a given key\n\nTime complexity: O(1) on average since maps are implemented as hash maps",
            move |value, args| {
                if let Some(key) = args.get("key") {
                    if let Value::Map(map) = value {
                        return Value::Boolean(map.contains_key(key));
                    }
                }
                Value::Boolean(false)
            },
            vec![Parameter::positional("key", "any")],
            "bool",
            true, false, true,
            None,
        )
    };

    let contains_value = {
        make_native_method(
            "contains_value",
            "Check if a map contains a given value\n\nTime complexity: O(n) since it has to potentially check every value in the map",
            move |value, args| {
                if let Some(val) = args.get("value") {
                    if let Value::Map(map) = value {
                        for v in map.values() {
                            if v == val {
                                return Value::Boolean(true);
                            }
                        }
                    }
                }
                Value::Boolean(false)
            },
            vec![Parameter::positional("value", "any")],
            "bool",
            true, false, true,
            None,
        )
    };

    let insert = {
        make_native_method(
            "insert",
            "Insert a key-value pair into a map",
            move |value, args| {
                if let (Some(key), Some(val)) = (args.get("key"), args.get("value")) {
                    if let Value::Map(map) = value {
                        map.insert(key.clone(), val.clone());
                        return Value::Null;
                    }
                }
                Value::new_error("TypeError", "Expected a map and both 'key' and 'value' arguments", None)
            },
            vec![
                Parameter::positional("key", "any"),
                Parameter::positional("value", "any"),
            ],
            "void",
            true, false, true,
            None,
        )
    };

    let clear = {
        make_native_method(
            "clear",
            "Clear all key-value pairs from a map (basically resetting it to an empty map)",
            move |value, _args| {
                if let Value::Map(map) = value {
                    map.clear();
                    return Value::Null;
                }
                Value::new_error("TypeError", "Expected a map", None)
            },
            vec![],
            "void",
            true, false, true,
            None,
        )
    };

    let extend_map = {
        make_native_method(
            "extend",
            "Extend a map with another map's key-value pairs",
            move |value, args| {
                if let Some(Value::Map(omap)) = args.get("other") {
                    if let Value::Map(smap) = value {
                        smap.extend(omap.clone());
                        return Value::Null;
                    }
                }
                Value::new_error("TypeError", "Expected a map and 'other' argument to be a map", None)
            },
            vec![Parameter::positional("other", "map")],
            "void",
            true, false, true,
            None,
        )
    };

    methods.extend([
        (("get".to_owned(), "map".to_owned()), get),
        (("filter".to_owned(), "map".to_owned()), filter_map),
        (("map".to_owned(), "map".to_owned()), map_map),
        (("keys".to_owned(), "map".to_owned()), keys),
        (("values".to_owned(), "map".to_owned()), values),
        (("zip".to_owned(), "map".to_owned()), zip),
        (("contains_key".to_owned(), "map".to_owned()), contains_key),
        (("contains_value".to_owned(), "map".to_owned()), contains_value),
        (("insert".to_owned(), "map".to_owned()), insert),
        (("clear".to_owned(), "map".to_owned()), clear),
        (("extend".to_owned(), "map".to_owned()), extend_map),
    ]);

    // ENUM METHODS

    let unwrap = {
        make_native_method(
            "unwrap",
            "Unwrap an enum variant, or raise an error if it's null",
            move |value, _args| {
                match value {
                    Value::Enum(enm) => {
                        *enm.variant.1.clone()
                    }
                    _ => Value::new_error("TypeError", "Expected enum variant", None),
                }
            },
            vec![],
            "any",
            false, true, true,
            None,
        )
    };

    let unwrap_or = {
        make_native_method(
            "unwrap_or",
            "Unwrap an enum variant, or return a default value if it's null",
            move |value, args| {
                match value {
                    Value::Enum(enm) => {
                        if *enm.variant.1 != Value::Null {
                            *enm.variant.1.clone()
                        } else {
                            args.get("default").cloned().unwrap_or(Value::Null)
                        }
                    }
                    _ => Value::new_error("TypeError", "Expected enum variant", None),
                }
            },
            vec![
                Parameter::positional("default", "any")
            ],
            "any",
            false, true, true,
            None,
        )
    };

    let discriminant = {
        make_native_method(
            "discriminant",
            "Get the discriminant (integer index) of an enum variant",
            move |value, _args| {
                match value {
                    Value::Enum(enm) => {
                        Value::Int(Int::from_i64(enm.variant.0 as i64))
                    }
                    _ => Value::new_error("TypeError", "Expected enum variant", None),
                }
            },
            vec![],
            "int",
            false, true, true,
            None,
        )
    };

    methods.extend([
        (("unwrap".to_owned(), "enum".to_owned()), unwrap),
        (("unwrap_or".to_owned(), "enum".to_owned()), unwrap_or),
        (("discriminant".to_owned(), "enum".to_owned()), discriminant),
    ]);
    
    methods
}

pub static DEFAULT_TYPE_METHODS: Lazy<FxHashMap<(String, String), Function>> = Lazy::new(|| {
    generate_methods_for_default_types()
});

pub fn get_default_type_method(name: &str, ty: &str) -> Option<&'static Function> {
    DEFAULT_TYPE_METHODS
        .get(&(name.to_owned(), ty.to_owned()))
        .or_else(|| DEFAULT_TYPE_METHODS.get(&(name.to_owned(), "any".to_string())))
}

pub fn get_type_method_names(val: &Value) -> Vec<String> {
    DEFAULT_TYPE_METHODS.iter()
        .filter_map(|((method_name, ty_name), _)| {
            if check_type_ident(val, ty_name) {
                Some(method_name.clone())
            } else {
                None
            }
        })
        .collect()
}
