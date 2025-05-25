use crate::env::helpers::utils::{Value, self};
use crate::env::helpers::functions::{Function, FunctionMetadata, NativeFunction, Parameter};
use std::collections::HashMap;
use std::sync::Arc;


fn print(pos_args: Vec<Value>, named_args: &HashMap<String, Value>) -> Value {
    let sep = match named_args.get("sep") {
        Some(Value::String(s)) => s.clone(),
        Some(other) => format_value(other),
        None => " ".to_string(),
    };

    let end = match named_args.get("end") {
        Some(Value::String(s)) => s.clone(),
        Some(other) => format_value(other),
        None => "\n".to_string(),
    };

    let mut out = "".to_string();
    for (i, arg) in pos_args.iter().enumerate() {
        if i > 0 {
            out.push_str(&sep);
        }
        out.push_str(&format_value(arg));
    }

    out.push_str(&end);
    print!("{}", out);

    Value::Null
}


fn format_value(value: &Value) -> String {
    match value {
        Value::Float(n) => {
            let n_str = n.to_string();
            if let Some(dot_pos) = n_str.find('.') {
                let mut trimmed = n_str.trim_start_matches('0');
                if trimmed.starts_with('.') {
                    trimmed = &trimmed[1..];
                }
                let trimmed = trimmed.trim_end_matches('0');
                if trimmed == "" {
                    "0".to_string()
                } else {
                    trimmed.to_string()
                }
            } else {
                n_str.trim_start_matches('0').to_string()
            }
        }
        Value::Int(n) => {
            let trimmed = n.to_string().trim_start_matches('0').to_string();
            if trimmed == "" {
                "0".to_string()
            } else {
                trimmed
            }
        }
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
        Value::Function(func) => {
            format!("<function '{}' at {:p}>", func.get_name(), func)
        }
        Value::Error(err_type, err_msg) => {
            format!("<{}: {}>", err_type, err_msg)
        }
        _ => "<unsupported value type>".to_string(),
    }
}

pub fn print_fn() -> Function {
    Function::Native(Arc::new(NativeFunction::new(
        "print",
        print,
        vec![  
            Parameter::variadic_optional("args", "any", Value::String("".to_string())), 
            Parameter::keyword_variadic_optional("end", "str", Value::String("\n".to_string())),
            Parameter::keyword_variadic_optional("sep", "str", Value::String(" ".to_string())),
        ],
        "void",
        true, true, true,
        None,
    )))
}