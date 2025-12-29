use crate::env::runtime::value::Value;
use crate::env::runtime::utils::{make_native_method_val, make_native_method_val_pt, convert_value_to_type, to_static, parse_type};
use crate::env::runtime::functions::Parameter;
use crate::env::runtime::generators::{Generator, GeneratorType, NativeGenerator, VecIter, EnumerateIter, FilterIter, MapIter, RepeatingIter};
use std::collections::HashMap;
use crate::env::runtime::types::{Float, Int, Type};
use imagnum::{create_int, create_float};
use serde::{Serialize, Deserialize};
use crate::interpreter::Interpreter;
use std::sync::Arc;
use parking_lot::Mutex;
use bincode::{Encode, Decode};
use rustc_hash::{FxHashSet, FxHashMap};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Encode, Decode)]
pub struct Variable {
    name: String,
    pub value: Value,
    pub type_: Type,
    is_static: bool,
    is_public: bool,
    is_final: bool,
    pub properties: HashMap<String, Variable>,
}

#[allow(dead_code)]
impl Variable {
    pub fn new(name: String, value: Value, type_: String, is_static: bool, is_public: bool, is_final: bool) -> Self {
        Self {
            name,
            value,
            type_: Type::new_simple(&type_),
            is_static,
            is_public,
            is_final,
            properties: HashMap::default(),
        }
    }

    pub fn new_pt(name: String, value: Value, type_: Type, is_static: bool, is_public: bool, is_final: bool) -> Self {
        Self {
            name,
            value,
            type_,
            is_static,
            is_public,
            is_final,
            properties: HashMap::default(),
        }
    }

    pub fn init_properties(&mut self, interpreter: &mut Interpreter) {
        let to_string = {
            let val_clone = self.value.clone();
            make_native_method_val(
                "to_string",
                move |_args| {
                    match val_clone.to_string() {
                        s if !s.is_empty() => Value::String(s),
                        _ => Value::Null,
                    }
                },
                vec![],
                "str",
                false, true, true,
                None,
            )
        };

        let clone = {
            let val_clone = self.value.clone();
            make_native_method_val_pt(
                "clone",
                move |_args| {
                    match &val_clone {
                        Value::Pointer(inner_arc) => {
                            let inner_guard = inner_arc.lock();
                            let (inner_value, counter) = &*inner_guard;
                            let cloned_value = inner_value.clone();
                            Value::Pointer(Arc::new(Mutex::new((cloned_value, *counter))))
                        },
                        Value::Null => Value::Null,
                        _ => val_clone.clone(),
                    }
                },
                vec![],
                &self.value.get_type(),
                false, true, true,
                None,
            )
        };
        
        let is_null = {
            let val_clone = self.value.clone();
            make_native_method_val(
                "is_null",
                move |_args| {
                    Value::Boolean(val_clone.is_null())
                },
                vec![],
                "bool",
                false, true, true,
                None,
            )
        };
        
        let is_truthy = {
            let val_clone = self.value.clone();
            make_native_method_val(
                "is_truthy",
                move |_args| {
                    Value::Boolean(val_clone.is_truthy())
                },
                vec![],
                "bool",
                false, true, true,
                None,
            )
        };
        let is_some = {
            let val_clone = self.value.clone();
            make_native_method_val(
                "is_some",
                move |_args| {
                    Value::Boolean(val_clone.is_truthy())
                },
                vec![],
                "bool",
                false, true, true,
                None,
            )
        };

        self.properties.insert(
            "to_string".to_string(),
            Variable::new(
                "to_string".to_string(),
                to_string,
                "function".to_string(),
                false,
                true,
                true,
            ),
        );
        self.properties.insert(
            "clone".to_string(),
            Variable::new(
                "clone".to_string(),
                clone,
                "function".to_string(),
                false,
                true,
                true,
            ),
        );
        self.properties.insert(
            "is_null".to_string(),
            Variable::new(
                "is_null".to_string(),
                is_null,
                "function".to_string(),
                false,
                true,
                true,
            ),
        );
        self.properties.insert(
            "is_truthy".to_string(),
            Variable::new(
                "is_truthy".to_string(),
                is_truthy,
                "function".to_string(),
                false,
                true,
                true,
            ),
        );
        self.properties.insert(
            "is_some".to_string(),
            Variable::new(
                "is_some".to_string(),
                is_some,
                "function".to_string(),
                false,
                true,
                true,
            ),
        );
    
        match &self.value {
            Value::String(_) => {
                let to_bytes = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "to_bytes",
                        move |_args| {
                            match val_clone.to_bytes() {
                                Some(bytes) => Value::Bytes(bytes),
                                None => Value::Null,
                            }
                        },
                        vec![],
                        "bytes",
                        false, true, true,
                        None,
                    )
                };  
                let endswith = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "endswith",
                        move |args| {
                            if let Some(s) = args.get("suffix").and_then(|v| match v {
                                Value::String(s) => Some(s),
                                _ => None,
                            }) {
                                if let Value::String(val) = &val_clone {
                                    return Value::Boolean(val.ends_with(s));
                                }
                            }
                            Value::Null
                        },
                        vec![Parameter::positional("suffix", "str")],
                        "bool",
                        false, true, true,
                        None,
                    )
                };        
                let startswith = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "startswith",
                        move |args| {
                            if let Some(s) = args.get("prefix").and_then(|v| match v {
                                Value::String(s) => Some(s),
                                _ => None,
                            }) {
                                if let Value::String(val) = &val_clone {
                                    return Value::Boolean(val.starts_with(s));
                                }
                            }
                            Value::Null
                        },
                        vec![Parameter::positional("prefix", "str")],
                        "bool",
                        false, true, true,
                        None,
                    )
                };
                let to_int = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "to_int",
                        move |_args| {
                            match &val_clone {
                                Value::String(s) => Value::Int(create_int(s)),
                                _ => Value::Null,
                            }
                        },
                        vec![],
                        "int",
                        false, true, true,
                        None,
                    )
                };
                let to_float = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "to_float",
                        move |_args| {
                            match &val_clone {
                                Value::String(s) => Value::Float(create_float(s)),
                                _ => Value::Null,
                            }
                        },
                        vec![],
                        "float",
                        false, true, true,
                        None,
                    )
                };
                let split = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "split",
                        move |args| {
                            if let Some(Value::String(delim)) = args.get("delimiter") {
                                if let Value::String(val) = &val_clone {
                                    let parts: Vec<Value> = val.split(delim.as_str())
                                        .map(|s| Value::String(s.to_string()))
                                        .collect();
                                    return Value::List(parts);
                                }
                            }
                            Value::Null
                        },
                        vec![Parameter::positional("delimiter", "str")],
                        "list",
                        false, true, true,
                        None,
                    )
                };
                let split_lines = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "split_lines",
                        move |_args| {
                            if let Value::String(val) = &val_clone {
                                let parts: Vec<Value> = val.lines()
                                    .map(|s| Value::String(s.to_string()))
                                    .collect();
                                return Value::List(parts);
                            }
                            Value::Null
                        },
                        vec![],
                        "list",
                        false, true, true,
                        None,
                    )
                };
                let join = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "join",
                        move |args| {
                            if let Some(Value::List(parts)) = args.get("parts") {
                                if let Value::String(val) = &val_clone {
                                    let joined: String = parts.iter()
                                        .filter_map(|v| v.to_string().into())
                                        .collect::<Vec<String>>()
                                        .join(val);
                                    return Value::String(joined);
                                }
                            }
                            Value::Null
                        },
                        vec![Parameter::positional("parts", "list")],
                        "str",
                        false, true, true,
                        None,
                    )
                };
                let trim = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "trim",
                        move |_args| {
                            match &val_clone {
                                Value::String(s) => Value::String(s.trim().to_string()),
                                _ => Value::Null,
                            }
                        },
                        vec![],
                        "str",
                        false, true, true,
                        None,
                    )
                };
                let chars = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "chars",
                        move |_args| {
                            if let Value::String(s) = &val_clone {
                                let char_list: Vec<Value> = s.chars().map(|c| Value::String(c.to_string())).collect();
                                return Value::List(char_list);
                            }
                            Value::Null
                        },
                        vec![],
                        "list",
                        false, true, true,
                        None,
                    )
                };
                let isalpha = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "isalpha",
                        move |_args| {
                            if let Value::String(s) = &val_clone {
                                return Value::Boolean(s.chars().all(|c| c.is_alphabetic()));
                            }
                            Value::Null
                        },
                        vec![],
                        "bool",
                        false, true, true,
                        None,
                    )
                };
                let isdigit = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "isdigit",
                        move |_args| {
                            if let Value::String(s) = &val_clone {
                                return Value::Boolean(s.chars().all(|c| c.is_digit(10)));
                            }
                            Value::Null
                        },
                        vec![],
                        "bool",
                        false, true, true,
                        None,
                    )
                };
                let islower = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "islower",
                        move |_args| {
                            if let Value::String(s) = &val_clone {
                                return Value::Boolean(s.chars().all(|c| c.is_lowercase()));
                            }
                            Value::Null
                        },
                        vec![],
                        "bool",
                        false, true, true,
                        None,
                    )
                };
                let isupper = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "isupper",
                        move |_args| {
                            if let Value::String(s) = &val_clone {
                                return Value::Boolean(s.chars().all(|c| c.is_uppercase()));
                            }
                            Value::Null
                        },
                        vec![],
                        "bool",
                        false, true, true,
                        None,
                    )
                };
                let isalnum = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "isalnum",
                        move |_args| {
                            if let Value::String(s) = &val_clone {
                                return Value::Boolean(s.chars().all(|c| c.is_alphanumeric()));
                            }
                            Value::Null
                        },
                        vec![],
                        "bool",
                        false, true, true,
                        None,
                    )
                };
                let isspace = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "isspace",
                        move |_args| {
                            if let Value::String(s) = &val_clone {
                                return Value::Boolean(s.chars().all(|c| c.is_whitespace()));
                            }
                            Value::Null
                        },
                        vec![],
                        "bool",
                        false, true, true,
                        None,
                    )
                };
                let replace = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "replace",
                        move |args| {
                            if let (Some(Value::String(from)), Some(Value::String(to))) = (args.get("from"), args.get("to")) {
                                if let Value::String(val) = &val_clone {
                                    let replaced = val.replace(from, to);
                                    return Value::String(replaced);
                                }
                            }
                            Value::Null
                        },
                        vec![
                            Parameter::positional("from", "str"),
                            Parameter::positional("to", "str"),
                        ],
                        "str",
                        false, true, true,
                        None,
                    )
                };
                let to_lower = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "to_lower",
                        move |_args| {
                            match &val_clone {
                                Value::String(s) => Value::String(s.to_lowercase()),
                                _ => Value::Null,
                            }
                        },
                        vec![],
                        "str",
                        false, true, true,
                        None,
                    )
                };
                let to_upper = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "to_upper",
                        move |_args| {
                            match &val_clone {
                                Value::String(s) => Value::String(s.to_uppercase()),
                                _ => Value::Null,
                            }
                        },
                        vec![],
                        "str",
                        false, true, true,
                        None,
                    )
                };
                let iter = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "iter",
                        move |_args| {
                            if let Value::String(s) = &val_clone {
                                let char_list: Vec<Value> = s.chars().map(|c| Value::String(c.to_string())).collect();
                                let vec_iter = VecIter::new(&char_list);
                                let generator = Generator::new_anonymous(
                                    GeneratorType::Native(NativeGenerator {
                                        iter: Box::new(vec_iter),
                                        iteration: 0,
                                    }),
                                    false,
                                );
                                return Value::Generator(generator);
                            }
                            Value::Null
                        },
                        vec![],
                        "generator",
                        false, true, true,
                        None,
                    )
                };
                let to_lf = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "to_lf",
                        move |_args| {
                            match &val_clone {
                                Value::String(s) => {
                                    let converted = s.replace("\r\n", "\n").replace("\r", "\n");
                                    Value::String(converted)
                                },
                                _ => Value::Null,
                            }
                        },
                        vec![],
                        "str",
                        false, true, true,
                        None,
                    )
                };
                let to_crlf = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "to_crlf",
                        move |_args| {
                            match &val_clone {
                                Value::String(s) => {
                                    let converted = s.replace("\r\n", "\n").replace("\r", "\n").replace("\n", "\r\n");
                                    Value::String(converted)
                                },
                                _ => Value::Null,
                            }
                        },
                        vec![],
                        "str",
                        false, true, true,
                        None,
                    )
                };
                let split_whitespace = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "split_whitespace",
                        move |_args| {
                            match &val_clone {
                                Value::String(s) => {
                                    let parts: Vec<Value> = s.split_whitespace()
                                        .map(|s| Value::String(s.to_string()))
                                        .collect();
                                    return Value::List(parts);
                                },
                                _ => Value::Null,
                            }
                        },
                        vec![],
                        "list",
                        false, true, true,
                        None,
                    )
                };

                self.properties.insert(
                    "to_bytes".to_string(),
                    Variable::new(
                        "to_bytes".to_string(),
                        to_bytes,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "endswith".to_string(),
                    Variable::new(
                        "endswith".to_string(),
                        endswith,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "startswith".to_string(),
                    Variable::new(
                        "startswith".to_string(),
                        startswith,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "to_int".to_string(),
                    Variable::new(
                        "to_int".to_string(),
                        to_int,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "to_float".to_string(),
                    Variable::new(
                        "to_float".to_string(),
                        to_float,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "split".to_string(),
                    Variable::new(
                        "split".to_string(),
                        split,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "split_lines".to_string(),
                    Variable::new(
                        "split_lines".to_string(),
                        split_lines,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "join".to_string(),
                    Variable::new(
                        "join".to_string(),
                        join,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "trim".to_string(),
                    Variable::new(
                        "trim".to_string(),
                        trim,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "chars".to_string(),
                    Variable::new(
                        "chars".to_string(),
                        chars,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "isalpha".to_string(),
                    Variable::new(
                        "isalpha".to_string(),
                        isalpha,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "isdigit".to_string(),
                    Variable::new(
                        "isdigit".to_string(),
                        isdigit,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "islower".to_string(),
                    Variable::new(
                        "islower".to_string(),
                        islower,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "isupper".to_string(),
                    Variable::new(
                        "isupper".to_string(),
                        isupper,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "isalnum".to_string(),
                    Variable::new(
                        "isalnum".to_string(),
                        isalnum,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "isspace".to_string(),
                    Variable::new(
                        "isspace".to_string(),
                        isspace,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "replace".to_string(),
                    Variable::new(
                        "replace".to_string(),
                        replace,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "to_lower".to_string(),
                    Variable::new(
                        "to_lower".to_string(),
                        to_lower,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "to_upper".to_string(),
                    Variable::new(
                        "to_upper".to_string(),
                        to_upper,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "iter".to_string(),
                    Variable::new(
                        "iter".to_string(),
                        iter.clone(),
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "into_gen".to_string(),
                    Variable::new(
                        "into_gen".to_string(),
                        iter,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "to_lf".to_string(),
                    Variable::new(
                        "to_lf".to_string(),
                        to_lf,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "to_crlf".to_string(),
                    Variable::new(
                        "to_crlf".to_string(),
                        to_crlf,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "split_whitespace".to_string(),
                    Variable::new(
                        "split_whitespace".to_string(),
                        split_whitespace,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
            }
            Value::Int(_) => {
                let to_float = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "to_float",
                        move |_args| {
                            match &val_clone {
                                Value::Int(i) => match i.to_float() {
                                    Ok(f) => Value::Float(f),
                                    Err(_) => Value::Error("TypeError", "Failed to convert Int to Float", None),
                                },
                                Value::Float(f) => Value::Float(f.clone()),
                                _ => Value::Float(Float::from_f64(0.0)),
                            }
                        },
                        vec![],
                        "float",
                        false, true, true,
                        None,
                    )
                };                
            
                let format = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "format",
                        move |args| {
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
                
                            if let Value::Int(val) = &val_clone {
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
                                    let mut result = String::new();
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
                
                                return Value::String(s);
                            }
                
                            Value::Null
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
                        false, true, true,
                        None,
                    )
                };
                
                self.properties.insert(
                    "to_float".to_string(),
                    Variable::new(
                        "to_float".to_string(),
                        to_float,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "format".to_string(),
                    Variable::new(
                        "format".to_string(),
                        format,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
            }
            Value::Float(_) => {
                let to_int = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "to_int",
                        move |_args| {
                            match &val_clone {
                                Value::Float(f) => match f.to_int() {
                                    Ok(i) => Value::Int(i),
                                    Err(_) => Value::Error("TypeError", "Failed to convert Float to Int", None),
                                },
                                Value::Int(i) => Value::Int(i.clone()),
                                _ => Value::Int(0i64.into()),
                            }
                        },
                        vec![],
                        "int",
                        false, true, true,
                        None,
                    )
                };                
            
                let format = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "format",
                        move |args| {
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
                
                            if let Value::Float(val) = &val_clone {
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
                
                                return Value::String(s);
                            }
                
                            Value::Null
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
                        false, true, true,
                        None,
                    )
                };

                let round = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "round",
                        move |args| {
                            let precision = if let Some(Value::Int(i)) = args.get("precision") {
                                i.to_i64().unwrap_or(0)
                            } else {
                                0
                            };
                
                            if let Value::Float(val) = &val_clone {
                                return Value::Float(val.round(precision as usize));
                            }
                
                            Value::Null
                        },
                        vec![Parameter::positional_optional("precision", "int", Value::Int(0.into()))],
                        "float",
                        false, true, true,
                        None,
                    )
                };
            
                self.properties.insert(
                    "to_int".to_string(),
                    Variable::new(
                        "to_int".to_string(),
                        to_int,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "format".to_string(),
                    Variable::new(
                        "format".to_string(),
                        format,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "round".to_string(),
                    Variable::new(
                        "round".to_string(),
                        round,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
            }
            Value::List(_) => {
                let self_arc = Arc::new(Mutex::new(self.clone()));

                let append = {
                    let self_arc = Arc::clone(&self_arc);
                    make_native_method_val(
                        "append",
                        move |args| {
                            if let Some(item) = args.get("item") {
                                let mut locked = self_arc.lock();
                                if let Value::List(list) = &mut locked.value {
                                    list.push(item.clone());
                                    return Value::List(list.clone()); // return self for chaining
                                }
                            }
                            Value::Null
                        },
                        vec![Parameter::positional("item", "any")],
                        "list",
                        false, true, true,
                        None,
                    )
                };

                let extend = {
                    let self_arc = Arc::clone(&self_arc);
                    make_native_method_val(
                        "extend",
                        move |args| {
                            if let Some(Value::List(to_extend)) = args.get("item") {
                                let mut locked = self_arc.lock();
                                if let Value::List(list) = &mut locked.value {
                                    list.extend(to_extend.clone());
                                    return Value::List(list.clone());
                                }
                            }
                            Value::Null
                        },
                        vec![Parameter::positional("item", "list")],
                        "list",
                        false, true, true,
                        None,
                    )
                };

                let into = {
                    let self_arc = Arc::clone(&self_arc);
                    make_native_method_val(
                        "into",
                        move |args| {
                            let locked = self_arc.lock();
                            let type_ = args
                                .get("ty")
                                .and_then(|v| if let Value::Type(t) = v { Some(t.display_simple()) } else { None })
                                .unwrap_or_else(|| "any".to_string());

                            let item_vec = match &locked.value {
                                Value::List(list) => list.clone(),
                                _ => return Value::Error("TypeError", "Expected a list", None),
                            };

                            let mut list = Vec::with_capacity(item_vec.len());
                            for item in item_vec {
                                match convert_value_to_type(&type_, &item) {
                                    Ok(converted) => list.push(converted),
                                    Err((err_type, err_msg, _)) => return Value::Error(err_type, err_msg, None),
                                }
                            }

                            Value::List(list)
                        },
                        vec![Parameter::positional("ty", "type")],
                        "list",
                        false, true, true,
                        None,
                    )
                };

                let into_gen = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "into_gen",
                        move |_args| {
                            let vec = match &val_clone {
                                Value::List(list) => list.clone(),
                                _ => return Value::Error("TypeError", "Expected a list", None),
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
                        false, true, true,
                        None,
                    )
                };

                let into_repeating_gen = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "into_repeating_gen",
                        move |_args| {
                            let vec = match &val_clone {
                                Value::List(list) => list.clone(),
                                _ => return Value::Error("TypeError", "Expected a list", None),
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
                        false, true, true,
                        None,
                    )
                };

                let enumerate = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "enumerate",
                        move |_args| {
                            let vec = match &val_clone {
                                Value::List(list) => list.clone(),
                                _ => return Value::Error("TypeError", "Expected a list", None),
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
                        false, true, true,
                        None,
                    )
                };

                let map = {
                    let val_clone = self.value.clone();
                    let interpreter_clone = interpreter.clone();
                    make_native_method_val(
                        "map",
                        move |args| {
                            if let Value::List(list) = &val_clone {
                                if let Some(Value::Function(func)) = args.get("f") {
                                    let vec_iter = VecIter::new(list);
                                    let vec_gen = Generator::new_anonymous(
                                        GeneratorType::Native(NativeGenerator {
                                            iter: Box::new(vec_iter),
                                            iteration: 0,
                                        }),
                                        false,
                                    );
                                    let map_iter = MapIter::new(&vec_gen, func.clone(), &interpreter_clone);
                                    let generator = Generator::new_anonymous(
                                        GeneratorType::Native(NativeGenerator {
                                            iter: Box::new(map_iter),
                                            iteration: 0,
                                        }),
                                        false,
                                    );
                                    return Value::Generator(generator);
                                } else {
                                    return Value::Error("TypeError", "Expected 'f' to be a function", None);
                                }
                            }
                            Value::Null
                        },
                        vec![Parameter::positional("f", "function")],
                        "generator",
                        false, true, true,
                        None,
                    )
                };

                let filter = {
                    let val_clone = self.value.clone();
                    let interpreter_clone = interpreter.clone();
                    make_native_method_val(
                        "filter",
                        move |args| {
                            if let Value::List(list) = &val_clone {
                                if let Some(Value::Function(func)) = args.get("f") {
                                    let vec_iter = VecIter::new(list);
                                    let vec_gen = Generator::new_anonymous(
                                        GeneratorType::Native(NativeGenerator {
                                            iter: Box::new(vec_iter),
                                            iteration: 0,
                                        }),
                                        false,
                                    );
                                    let filter_iter = FilterIter::new(&vec_gen, func.clone(), &interpreter_clone);
                                    let generator = Generator::new_anonymous(
                                        GeneratorType::Native(NativeGenerator {
                                            iter: Box::new(filter_iter),
                                            iteration: 0,
                                        }),
                                        false,
                                    );
                                    return Value::Generator(generator);
                                } else {
                                    return Value::Error("TypeError", "Expected 'f' to be a function", None);
                                }
                            }
                            Value::Null
                        },
                        vec![Parameter::positional("f", "function")],
                        "generator",
                        false, true, true,
                        None,
                    )
                };
                let sort = {
                    let function_signature = parse_type("function[any] -> any | null");
                    let value_clone = self.value.clone();
                    let interpreter_arc = Arc::new(Mutex::new(interpreter.clone()));

                    make_native_method_val(
                        "sort",
                        {
                            let interpreter_arc = interpreter_arc.clone();

                            move |args| {
                                let reverse = matches!(args.get("reverse"), Some(Value::Boolean(true)));

                                let function: Option<_> = match args.get("f") {
                                    Some(Value::Function(func)) => Some(func.clone()),
                                    Some(Value::Null) | None => None,
                                    _ => return Value::Error("RuntimeError", "Expected 'f' to be a function or null", None),
                                };

                                let items = match &value_clone {
                                    Value::List(list) => list.clone(),
                                    _ => return Value::Error("TypeError", "Expected a list", None),
                                };

                                let mut keys = Vec::with_capacity(items.len());

                                if let Some(function) = function {
                                    let mut interp_guard = interpreter_arc.lock();
                                    for item in &items {
                                        let key_res = interp_guard.call_function(
                                            &function,
                                            vec![item.clone()],
                                            std::collections::HashMap::default(),
                                            None,
                                        );

                                        if interp_guard.err.is_some() {
                                            return interp_guard.err.take().unwrap().to_value();
                                        }

                                        keys.push(key_res);
                                    }
                                    drop(interp_guard);
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
                            }
                        },
                        vec![
                            Parameter::positional_optional_pt("f", &function_signature, Value::Null),
                            Parameter::positional_optional("reverse", "bool", Value::Boolean(false)),
                        ],
                        "list",
                        false, true, true,
                        None,
                    )
                };
                let sort_by = {
                    let function_signature = parse_type("function[any, any] -> bool");
                    let value_clone = self.value.clone();
                    let interpreter_arc = Arc::new(Mutex::new(interpreter.clone()));

                    make_native_method_val(
                        "sort_by",
                        {
                            let interpreter_arc = interpreter_arc.clone();

                            move |args| {
                                let reverse = matches!(args.get("reverse"), Some(Value::Boolean(true)));

                                let function = match args.get("f") {
                                    Some(Value::Function(func)) => func.clone(),
                                    _ => {
                                        return Value::Error(
                                            "RuntimeError",
                                            "Expected 'f' to be a function",
                                            None,
                                        )
                                    }
                                };

                                let items = match &value_clone {
                                    Value::List(list) => list.clone(),
                                    _ => return Value::Error("TypeError", "Expected a list", None),
                                };

                                let mut indices: Vec<usize> = (0..items.len()).collect();
                                let mut interp_guard = interpreter_arc.lock();

                                let cache: Arc<Mutex<FxHashMap<(usize, usize), std::cmp::Ordering>>> = Arc::new(Mutex::new(FxHashMap::default()));

                                indices.sort_by(|&i, &j| {
                                    {
                                        let cache_guard = cache.lock();
                                        if let Some(&ord) = cache_guard.get(&(i, j)) {
                                            return ord;
                                        }
                                    }

                                    let res = interp_guard.call_function(
                                        &function,
                                        vec![items[i].clone(), items[j].clone()],
                                        std::collections::HashMap::default(),
                                        None,
                                    );

                                    if interp_guard.err.is_some() {
                                        return std::cmp::Ordering::Equal;
                                    }

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
                                });

                                if interp_guard.err.is_some() {
                                    return interp_guard.err.take().unwrap().to_value();
                                }

                                let sorted_items: Vec<Value> =
                                    indices.into_iter().map(|i| items[i].clone()).collect();

                                Value::List(sorted_items)
                            }
                        },
                        vec![
                            Parameter::positional_pt("f", &function_signature),
                            Parameter::positional_optional("reverse", "bool", Value::Boolean(false)),
                        ],
                        "list",
                        false,
                        true,
                        true,
                        None,
                    )
                };
                let sort_by_unstable = {
                    let function_signature = parse_type("function[any, any] -> bool");
                    let value_clone = self.value.clone();
                    let interpreter_arc = Arc::new(Mutex::new(interpreter.clone()));

                    make_native_method_val(
                        "sort_by_unstable",
                        {
                            let interpreter_arc = interpreter_arc.clone();

                            move |args| {
                                let reverse = matches!(args.get("reverse"), Some(Value::Boolean(true)));

                                let function = match args.get("f") {
                                    Some(Value::Function(func)) => func.clone(),
                                    _ => {
                                        return Value::Error(
                                            "RuntimeError",
                                            "Expected 'f' to be a function",
                                            None,
                                        )
                                    }
                                };

                                let items: Vec<Value> = match &value_clone {
                                    Value::List(list) => list.clone(),
                                    _ => return Value::Error("TypeError", "Expected a list", None),
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
                                        let mut interp_guard = interpreter_arc.lock();
                                        let result = interp_guard.call_function(
                                            &function,
                                            vec![items[i].clone(), items[j].clone()],
                                            std::collections::HashMap::default(),
                                            None,
                                        );

                                        if let Some(err) = interp_guard.err.take() {
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

                                if args.get("unstabler") == Some(&Value::Boolean(true)) {
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
                            }
                        },
                        vec![
                            Parameter::positional_pt("f", &function_signature),
                            Parameter::positional_optional("reverse", "bool", Value::Boolean(false)),
                            Parameter::positional_optional("unstabler", "bool", Value::Boolean(false)),
                        ],
                        "list",
                        false,
                        true,
                        true,
                        None,
                    )
                };
                let all = {
                    let function_signature = parse_type("function[any] -> any | null");
                    let interpreter_clone = interpreter.clone();
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "all",
                        move |args| {
                            let function: Option<_> = match args.get("f") {
                                Some(Value::Function(func)) => Some(func.clone()),
                                Some(Value::Null) | None => None,
                                _ => return Value::Error("RuntimeError", "Expected 'f' to be a function or null", None),
                            };

                            let items = match &val_clone {
                                Value::List(list) => list.clone(),
                                _ => return Value::Error("TypeError", "Expected a list", None),
                            };

                            for item in &items {
                                let result = if let Some(function) = &function {
                                    let mut interp = interpreter_clone.clone();
                                    let res = interp.call_function(
                                        function,
                                        vec![item.clone()],
                                        std::collections::HashMap::default(),
                                        None,
                                    );

                                    if interp.err.is_some() {
                                        return interp.err.take().unwrap().to_value();
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
                                    _ => return Value::Error("TypeError", "Function must return a boolean", None),
                                }
                            }

                            Value::Boolean(true)
                        },
                        vec![Parameter::positional_optional_pt("f", &function_signature, Value::Null)],
                        "bool",
                        false, true, true,
                        None,
                    )
                };
                let contains = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "contains",
                        move |args| {
                            if let Some(item) = args.get("item") {
                                if let Value::List(list) = &val_clone {
                                    return Value::Boolean(list.contains(item));
                                }
                            }
                            Value::Null
                        },
                        vec![Parameter::positional("item", "any")],
                        "bool",
                        false, true, true,
                        None,
                    )
                };
                let index_of = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "index_of",
                        move |args| {
                            if let Some(item) = args.get("item") {
                                if let Value::List(list) = &val_clone {
                                    let start = if let Some(Value::Int(i)) = args.get("start") {
                                        i.to_usize().unwrap_or(0)
                                    } else {
                                        0
                                    };
                                    for (index, list_item) in list.iter().enumerate().skip(start) {
                                        if list_item == item {
                                            return Value::Int(create_int(&(index as i64).to_string()));
                                        }
                                    }
                                    return Value::Error("ValueError", "Item not found in list", None);
                                }
                            }
                            Value::Null
                        },
                        vec![
                            Parameter::positional("item", "any"),
                            Parameter::positional_optional("start", "int", Value::Int(0.into())),
                        ],
                        "int",
                        false, true, true,
                        None,
                    )
                };
                let undup = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "undup",
                        move |_args| {
                            if let Value::List(list) = &val_clone {
                                let mut seen = FxHashSet::with_capacity_and_hasher(list.len(), Default::default());
                                let mut unduped = Vec::with_capacity(list.len());
                                for item in list {
                                    if seen.insert(item.clone()) {
                                        unduped.push(item.clone());
                                    }
                                }
                                return Value::List(unduped);
                            }
                            Value::Null
                        },
                        vec![],
                        "list",
                        false, true, true,
                        None,
                    )
                };
                let reverse = {
                    let value_clone = self.value.clone();
                    make_native_method_val(
                        "reverse",
                        move |_args| {
                            let items = match &value_clone {
                                Value::List(list) => list.clone(),
                                _ => return Value::Error("TypeError", "Expected a list", None),
                            };

                            let mut reversed_items = items;
                            reversed_items.reverse();

                            Value::List(reversed_items)
                        },
                        vec![],
                        "list",
                        false, true, true,
                        None,
                    )
                };
                let reduce = {
                    let value_clone = self.value.clone();
                    let interpreter_clone = interpreter.clone();
                    make_native_method_val(
                        "reduce",
                        move |args| {
                            if let Value::List(list) = &value_clone {
                                if let Some(Value::Function(func)) = args.get("f") {
                                    let mut interpreter_clone = interpreter_clone.clone();
                                    if list.is_empty() {
                                        return Value::Error("ValueError", "Cannot reduce an empty list", None);
                                    }
                                    let mut result = match args.get("initial") {
                                        Some(initial) if *initial != Value::Null => initial.clone(),
                                        _ => list[0].clone(),
                                    };
                                    for item in &list[1..] {
                                        let res = interpreter_clone.call_function(
                                            func,
                                            vec![result.clone(), item.clone()],
                                            std::collections::HashMap::default(),
                                            None,
                                        );
                                        if interpreter_clone.err.is_some() {
                                            return interpreter_clone.err.take().unwrap().to_value();
                                        }
                                        result = res;
                                    }
                                    return result;
                                } else {
                                    return Value::Error("TypeError", "Expected 'f' to be a function", None);
                                }
                            }
                            Value::Null
                        },
                        vec![
                            Parameter::positional("f", "function"),
                            Parameter::positional_optional("initial", "any", Value::Null)
                        ],
                        "any",
                        false, true, true,
                        None,
                    )
                };
                let pop = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "pop",
                        move |args| {
                            let index = if let Some(Value::Int(i)) = args.get("index") {
                                match i.to_isize() {
                                    Ok(idx) => idx,
                                    Err(_) => return Value::Error("IndexError", "Index out of bounds", None),
                                }
                            } else {
                                -1
                            };
                            if let Value::List(list) = &val_clone {
                                if list.is_empty() {
                                    return Value::Error("IndexError", "Pop from empty list", None);
                                }
                                let mut new_list = list.clone();
                                let item = new_list.remove(if index < 0 {
                                    let idx = new_list.len() as isize + index;
                                    if idx < 0 {
                                        return Value::Error("IndexError", "Index out of bounds", None);
                                    }
                                    idx as usize
                                } else {
                                    let idx = index as usize;
                                    if idx >= new_list.len() {
                                        return Value::Error("IndexError", "Index out of bounds", None);
                                    }
                                    idx
                                });
                                return Value::Tuple(vec![Value::List(new_list), item]);
                            }
                            Value::Null
                        },
                        vec![
                            Parameter::positional("index", "int"),
                        ],
                        "tuple",
                        false, true, true,
                        None,
                    )
                };
                let dedup = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "dedup",
                        move |_args| {
                            if let Value::List(list) = &val_clone {
                                let mut seen = FxHashSet::with_capacity_and_hasher(list.len(), Default::default());
                                let mut deduped = Vec::with_capacity(list.len());
                                for item in list {
                                    if !seen.contains(item) {
                                        seen.insert(item.clone());
                                        deduped.push(item.clone());
                                    }
                                }
                                return Value::List(deduped);
                            }
                            Value::Null
                        },
                        vec![],
                        "list",
                        false, true, true,
                        None,
                    )
                };
                let max = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "max",
                        move |_args| {
                            if let Value::List(list) = &val_clone {
                                if list.is_empty() {
                                    return Value::Null;
                                }
                                let mut max_value = list[0].clone();
                                for item in list.iter().skip(1) {
                                    if let Some(ordering) = item.partial_cmp(&max_value) {
                                        if ordering == std::cmp::Ordering::Greater {
                                            max_value = item.clone();
                                        }
                                    } else {
                                        return Value::Error("TypeError", "Cannot compare values", None);
                                    }
                                }
                                return max_value;
                            }
                            Value::Null
                        },
                        vec![],
                        "any",
                        false, true, true,
                        None,
                    )
                };
                let min = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "min",
                        move |_args| {
                            if let Value::List(list) = &val_clone {
                                if list.is_empty() {
                                    return Value::Null;
                                }
                                let mut min_value = list[0].clone();
                                for item in list.iter().skip(1) {
                                    if let Some(ordering) = item.partial_cmp(&min_value) {
                                        if ordering == std::cmp::Ordering::Less {
                                            min_value = item.clone();
                                        }
                                    } else {
                                        return Value::Error("TypeError", "Cannot compare values", None);
                                    }
                                }
                                return min_value;
                            }
                            Value::Null
                        },
                        vec![],
                        "any",
                        false, true, true,
                        None,
                    )
                };
                let is_sorted = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "is_sorted",
                        move |_args| {
                            if let Value::List(list) = &val_clone {
                                if list.len() <= 1 {
                                    return Value::Boolean(true);
                                }
                                for i in 0..list.len() - 1 {
                                    if let Some(ordering) = list[i].partial_cmp(&list[i + 1]) {
                                        if ordering == std::cmp::Ordering::Greater {
                                            return Value::Boolean(false);
                                        }
                                    } else {
                                        return Value::Error("TypeError", "Cannot compare values", None);
                                    }
                                }
                                return Value::Boolean(true);
                            }
                            Value::Null
                        },
                        vec![],
                        "bool",
                        false, true, true,
                        None,
                    )
                };

                self.properties.insert(
                    "max".to_string(),
                    Variable::new(
                        "max".to_string(),
                        max,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "min".to_string(),
                    Variable::new(
                        "min".to_string(),
                        min,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "append".to_string(),
                    Variable::new(
                        "append".to_string(),
                        append,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "extend".to_string(),
                    Variable::new(
                        "extend".to_string(),
                        extend,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "into".to_string(),
                    Variable::new(
                        "into".to_string(),
                        into,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "into_gen".to_string(),
                    Variable::new(
                        "into_gen".to_string(),
                        into_gen.clone(),
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "iter".to_string(),
                    Variable::new(
                        "iter".to_string(),
                        into_gen,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "into_repeating_gen".to_string(),
                    Variable::new(
                        "into_repeating_gen".to_string(),
                        into_repeating_gen.clone(),
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "repeating_iter".to_string(),
                    Variable::new(
                        "repeating_iter".to_string(),
                        into_repeating_gen,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "enumerate".to_string(),
                    Variable::new(
                        "enumerate".to_string(),
                        enumerate,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "map".to_string(),
                    Variable::new(
                        "map".to_string(),
                        map,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "filter".to_string(),
                    Variable::new(
                        "filter".to_string(),
                        filter,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "sort".to_string(),
                    Variable::new(
                        "sort".to_string(),
                        sort,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "sort_by".to_string(),
                    Variable::new(
                        "sort_by".to_string(),
                        sort_by,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "sort_by_unstable".to_string(),
                    Variable::new(
                        "sort_by_unstable".to_string(),
                        sort_by_unstable,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "all".to_string(),
                    Variable::new(
                        "all".to_string(),
                        all,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "contains".to_string(),
                    Variable::new(
                        "contains".to_string(),
                        contains,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "index_of".to_string(),
                    Variable::new(
                        "index_of".to_string(),
                        index_of,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "undup".to_string(),
                    Variable::new(
                        "undup".to_string(),
                        undup,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "reverse".to_string(),
                    Variable::new(
                        "reverse".to_string(),
                        reverse,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "reduce".to_string(),
                    Variable::new(
                        "reduce".to_string(),
                        reduce,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "pop".to_string(),
                    Variable::new(
                        "pop".to_string(),
                        pop,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "dedup".to_string(),
                    Variable::new(
                        "dedup".to_string(),
                        dedup,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "is_sorted".to_string(),
                    Variable::new(
                        "is_sorted".to_string(),
                        is_sorted,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
            }
            Value::Tuple(_) => {
                let to_list = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "to_list",
                        move |_args| {
                            if let Value::Tuple(tuple) = &val_clone {
                                return Value::List(tuple.to_vec());
                            }
                            Value::Null
                        },
                        vec![],
                        "list",
                        false, true, true,
                        None,
                    )
                };

                let enumerate = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "enumerate",
                        move |_args| {
                            let vec = match &val_clone {
                                Value::Tuple(v) => v.clone(),
                                _ => return Value::Error("TypeError", "Expected a tuple", None),
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
                        false, true, true,
                        None,
                    )
                };
                
                self.properties.insert(
                    "to_list".to_string(),
                    Variable::new(
                        "to_list".to_string(),
                        to_list,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "enumerate".to_string(),
                    Variable::new(
                        "enumerate".to_string(),
                        enumerate,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
            }     
            Value::Pointer(_) => {
                let extract_ptr = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "extract_ptr",
                        move |_args| {
                            match &val_clone {
                                Value::Pointer(ptr_arc) => {
                                    let raw_ptr: *const Mutex<(Value, usize)> = Arc::as_ptr(ptr_arc);
                                    Value::Int((raw_ptr as usize).into())
                                }
                                _ => Value::Null,
                            }
                        },
                        vec![],
                        "int",
                        false, true, true,
                        None,
                    )
                };
                
                self.properties.insert(
                    "extract_ptr".to_string(),
                    Variable::new(
                        "extract_ptr".to_string(),
                        extract_ptr,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
            }
            Value::Generator(_) => {
                let collect = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "collect",
                        move |_args| {
                            if let Value::Generator(generator) = &val_clone {
                                if !generator.is_infinite() {
                                    let v = generator.to_vec();
                                    if let Some(Value::Error(err_type, err_msg, ref_err)) = v.iter().find(|item| matches!(item, Value::Error(..))) {
                                        return Value::Error(err_type, err_msg, ref_err.clone());
                                    }
                                    return Value::List(v);
                                } else {
                                    return Value::Error("TypeError", "Cannot convert infinite generator to list", None);
                                }
                            }
                            Value::Null
                        },
                        vec![],
                        "list",
                        false, true, true,
                        None,
                    )
                };

                let collect_into = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "collect_into",
                        move |args| {
                            let type_ = args
                                .get("ty")
                                .and_then(|v| if let Value::Type(t) = v { Some(t.display_simple()) } else { None })
                                .unwrap_or_else(|| "any".to_string());

                            let item_vec = match &val_clone {
                                Value::Generator(generator) if !generator.is_infinite() => {
                                    let v = generator.to_vec();
                                    if let Some(Value::Error(err_type, err_msg, ref_err)) = v.iter().find(|item| matches!(item, Value::Error(..))) {
                                        return Value::Error(err_type, err_msg, ref_err.clone());
                                    }
                                    v
                                }
                                Value::Generator(_) => return Value::Error("TypeError", "Cannot convert infinite generator to list", None),
                                _ => return Value::Error("TypeError", "Expected a generator", None),
                            };

                            let mut list = Vec::with_capacity(item_vec.len());
                            for item in item_vec {
                                match convert_value_to_type(&type_, &item) {
                                    Ok(converted) => list.push(converted),
                                    Err((err_type, err_msg, _)) => return Value::Error(err_type, err_msg, None),
                                }
                            }

                            Value::List(list)
                        },
                        vec![Parameter::positional("ty", "type")],
                        "list",
                        false, true, true,
                        None,
                    )
                };

                let next = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "next",
                        move |_args| {
                            if let Value::Generator(generator) = &val_clone {
                                if let Some(next_value) = generator.next() {
                                    return next_value;
                                } else {
                                    return Value::Error("StopIteration", "No more items in generator", None);
                                }
                            }
                            Value::Null
                        },
                        vec![],
                        "any",
                        false, true, true,
                        None,
                    )
                };

                let is_done = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "is_done",
                        move |_args| {
                            if let Value::Generator(generator) = &val_clone {
                                return Value::Boolean(generator.is_done());
                            }
                            Value::Null
                        },
                        vec![],
                        "bool",
                        false, true, true,
                        None,
                    )
                };

                let peek = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "peek",
                        move |_args| {
                            if let Value::Generator(generator) = &val_clone {
                                if let Some(peeked_value) = generator.peek() {
                                    return peeked_value;
                                } else {
                                    return Value::Error("StopIteration", "No more items in generator", None);
                                }
                            }
                            Value::Null
                        },
                        vec![],
                        "any",
                        false, true, true,
                        None,
                    )
                };

                let enumerate = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "enumerate",
                        move |_args| {
                            let generator = match &val_clone {
                                Value::Generator(generator) => generator,
                                _ => return Value::Error("TypeError", "Expected a generator", None),
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
                        false, true, true,
                        None,
                    )
                };

                let filter = {
                    let val_clone = self.value.clone();
                    let interpreter_clone = interpreter.clone();
                    make_native_method_val(
                        "filter",
                        move |args| {
                            if let Value::Generator(generator) = &val_clone {
                                if let Some(Value::Function(func)) = args.get("f") {
                                    let filter_iter = FilterIter::new(generator, func.clone(), &interpreter_clone);
                                    let generator = Generator::new_anonymous(
                                        GeneratorType::Native(NativeGenerator {
                                            iter: Box::new(filter_iter),
                                            iteration: 0,
                                        }),
                                        false,
                                    );
                                    return Value::Generator(generator);
                                } else {
                                    return Value::Error("TypeError", "Expected 'f' to be a function", None);
                                }
                            }
                            Value::Null
                        },
                        vec![Parameter::positional("f", "function")],
                        "generator",
                        false, true, true,
                        None,
                    )
                };

                let map = {
                    let val_clone = self.value.clone();
                    let interpreter_clone = interpreter.clone();
                    make_native_method_val(
                        "map",
                        move |args| {
                            if let Value::Generator(generator) = &val_clone {
                                if let Some(Value::Function(func)) = args.get("f") {
                                    let map_iter = MapIter::new(generator, func.clone(), &interpreter_clone);
                                    let generator = Generator::new_anonymous(
                                        GeneratorType::Native(NativeGenerator {
                                            iter: Box::new(map_iter),
                                            iteration: 0,
                                        }),
                                        false,
                                    );
                                    return Value::Generator(generator);
                                } else {
                                    return Value::Error("TypeError", "Expected 'f' to be a function", None);
                                }
                            }
                            Value::Null
                        },
                        vec![Parameter::positional("f", "function")],
                        "generator",
                        false, true, true,
                        None,
                    )
                };
                
                let take = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "take",
                        move |args| {
                            if let Value::Generator(generator) = &val_clone {
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
                                    return Value::Error("TypeError", "Expected 'n' to be an integer", None);
                                }
                            }
                            Value::Null
                        },
                        vec![Parameter::positional("n", "int")],
                        "generator",
                        false, true, true,
                        None,
                    )
                };

                self.properties.insert(
                    "collect".to_string(),
                    Variable::new(
                        "collect".to_string(),
                        collect,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "collect_into".to_string(),
                    Variable::new(
                        "collect_into".to_string(),
                        collect_into,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "next".to_string(),
                    Variable::new(
                        "next".to_string(),
                        next,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "is_done".to_string(),
                    Variable::new(
                        "is_done".to_string(),
                        is_done,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "peek".to_string(),
                    Variable::new(
                        "peek".to_string(),
                        peek,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "enumerate".to_string(),
                    Variable::new(
                        "enumerate".to_string(),
                        enumerate,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "filter".to_string(),
                    Variable::new(
                        "filter".to_string(),
                        filter,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "map".to_string(),
                    Variable::new(
                        "map".to_string(),
                        map,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "take".to_string(),
                    Variable::new(
                        "take".to_string(),
                        take,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
            }
            Value::Map(_) => {
                let get = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "get",
                        move |args| {
                            if let Some(key) = args.get("key") {
                                if let Value::Map(map) = &val_clone {
                                    if let Some(value) = map.get(key) {
                                        return value.clone();
                                    }
                                }
                            }
                            Value::Null
                        },
                        vec![Parameter::positional("key", "any")],
                        "any",
                        false, true, true,
                        None,
                    )
                };
                let filter = {
                    let val_clone = self.value.clone();
                    let interpreter_clone = interpreter.clone();
                    make_native_method_val(
                        "filter",
                        move |args| {
                            if let Some(Value::Function(func)) = args.get("f") {
                                if let Value::Map(map) = &val_clone {
                                    let mut interpreter = interpreter_clone.clone();

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
                                    return Value::Error("TypeError", "Expected a map", None);
                                }
                            } else {
                                return Value::Error("TypeError", "Expected 'f' to be a function", None);
                            }
                        },
                        vec![Parameter::positional("f", "function")],
                        "map",
                        false, true, true,
                        None,
                    )
                };
                let map = {
                    let val_clone = self.value.clone();
                    let interpreter_clone = interpreter.clone();
                    make_native_method_val(
                        "map",
                        move |args| {
                            if let Some(Value::Function(func)) = args.get("f") {
                                if let Value::Map(map) = &val_clone {
                                    let mut interpreter = interpreter_clone.clone();

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
                                    return Value::Error("TypeError", "Expected a map", None);
                                }
                            } else {
                                return Value::Error("TypeError", "Expected 'f' to be a function", None);
                            }
                        },
                        vec![Parameter::positional("f", "function")],
                        "map",
                        false, true, true,
                        None,
                    )
                };
                let keys = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "keys",
                        move |_args| {
                            if let Value::Map(map) = &val_clone {
                                return Value::List(map.keys().cloned().collect());
                            }
                            Value::Null
                        },
                        vec![],
                        "list",
                        false, true, true,
                        None,
                    )
                };
                let values = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "values",
                        move |_args| {
                            if let Value::Map(map) = &val_clone {
                                return Value::List(map.values().cloned().collect());
                            }
                            Value::Null
                        },
                        vec![],
                        "list",
                        false, true, true,
                        None,
                    )
                };
                let zip = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "zip",
                        move |_args| {
                            let (keys, values) = if let Value::Map(map) = &val_clone {
                                (map.keys().cloned().collect::<Vec<_>>(), map.values().cloned().collect::<Vec<_>>())
                            } else {
                                return Value::Error("TypeError", "Expected a map", None);
                            };
                            Value::List(keys.into_iter().zip(values).map(|(k, v)| Value::Tuple(vec![k, v])).collect())
                        },
                        vec![],
                        "list",
                        false, true, true,
                        None,
                    )
                };
                let contains_key = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "contains_key",
                        move |args| {
                            if let Some(key) = args.get("key") {
                                if let Value::Map(map) = &val_clone {
                                    return Value::Boolean(map.contains_key(key));
                                }
                            }
                            Value::Boolean(false)
                        },
                        vec![Parameter::positional("key", "any")],
                        "bool",
                        false, true, true,
                        None,
                    )
                };
                let contains_value = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "contains_value",
                        move |args| {
                            if let Some(value) = args.get("value") {
                                if let Value::Map(map) = &val_clone {
                                    return Value::Boolean(map.values().any(|v| v == value));
                                }
                            }
                            Value::Boolean(false)
                        },
                        vec![Parameter::positional("value", "any")],
                        "bool",
                        false, true, true,
                        None,
                    )
                };
                let remove = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "remove",
                        move |args| {
                            if let Some(key) = args.get("key") {
                                if let Value::Map(map) = &val_clone {
                                    let mut new_map = map.clone();
                                    new_map.remove(key);
                                    return Value::Map(new_map);
                                }
                            }
                            val_clone.clone()
                        },
                        vec![Parameter::positional("key", "any")],
                        "map",
                        false, true, true,
                        None,
                    )
                };
                let insert = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "insert",
                        move |args| {
                            if let (Some(key), Some(value)) = (args.get("key"), args.get("value")) {
                                if let Value::Map(map) = &val_clone {
                                    let mut new_map = map.clone();
                                    new_map.insert(key.clone(), value.clone());
                                    return Value::Map(new_map);
                                }
                            }
                            val_clone.clone()
                        },
                        vec![
                            Parameter::positional("key", "any"),
                            Parameter::positional("value", "any"),
                        ],
                        "map",
                        false, true, true,
                        None,
                    )
                };

                self.properties.insert(
                    "get".to_string(),
                    Variable::new(
                        "get".to_string(),
                        get,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "filter".to_string(),
                    Variable::new(
                        "filter".to_string(),
                        filter,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "map".to_string(),
                    Variable::new(
                        "map".to_string(),
                        map,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "keys".to_string(),
                    Variable::new(
                        "keys".to_string(),
                        keys,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "values".to_string(),
                    Variable::new(
                        "values".to_string(),
                        values,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "zip".to_string(),
                    Variable::new(
                        "zip".to_string(),
                        zip,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "contains_key".to_string(),
                    Variable::new(
                        "contains_key".to_string(),
                        contains_key,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "contains_value".to_string(),
                    Variable::new(
                        "contains_value".to_string(),
                        contains_value,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "remove".to_string(),
                    Variable::new(
                        "remove".to_string(),
                        remove,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "insert".to_string(),
                    Variable::new(
                        "insert".to_string(),
                        insert,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
            }
            Value::Enum(_) => {
                let unwrap = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "unwrap",
                        move |_| {
                            match &val_clone {
                                Value::Enum(enm) => {
                                    *enm.variant.1.clone()
                                }
                                _ => Value::Error("TypeError", "Expected enum variant", None),
                            }
                        },
                        vec![],
                        "any",
                        false, true, true,
                        None,
                    )
                };
                let unwrap_or = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "unwrap_or",
                        move |args| {
                            match &val_clone {
                                Value::Enum(enm) => {
                                    if *enm.variant.1 != Value::Null {
                                        *enm.variant.1.clone()
                                    } else {
                                        args.get("default").cloned().unwrap_or(Value::Null)
                                    }
                                }
                                _ => Value::Error("TypeError", "Expected enum variant", None),
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
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "discriminant",
                        move |_| {
                            match &val_clone {
                                Value::Enum(enm) => {
                                    Value::Int(Int::from_i64(enm.variant.0 as i64))
                                }
                                _ => Value::Error("TypeError", "Expected enum variant", None),
                            }
                        },
                        vec![],
                        "int",
                        false, true, true,
                        None,
                    )
                };

                self.properties.insert(
                    "unwrap".to_string(),
                    Variable::new(
                        "unwrap".to_string(),
                        unwrap,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "unwrap_or".to_string(),
                    Variable::new(
                        "unwrap_or".to_string(),
                        unwrap_or,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "discriminant".to_string(),
                    Variable::new(
                        "discriminant".to_string(),
                        discriminant,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
            }
            Value::Type(_) => {
                let base_type = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "base_type",
                        move |_| {
                            if let Value::Type(t) = &val_clone {
                                return Value::Type(t.base_type());
                            }
                            Value::Error("TypeError", "Expected a type", None)
                        },
                        vec![],
                        "type",
                        false, true, true,
                        None,
                    )
                };
                let name = {
                    let val_clone = self.value.clone();
                    make_native_method_val(
                        "name",
                        move |_| {
                            if let Value::Type(t) = &val_clone {
                                return Value::String(t.display_simple());
                            }
                            Value::Error("TypeError", "Expected a type", None)
                        },
                        vec![],
                        "str",
                        false, true, true,
                        None,
                    )
                };

                self.properties.insert(
                    "base_type".to_string(),
                    Variable::new(
                        "base_type".to_string(),
                        base_type,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "name".to_string(),
                    Variable::new(
                        "name".to_string(),
                        name,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
            }
            _ => {}
        };
    }

    pub fn is_init(&self) -> bool {
        !self.properties.is_empty() && matches!(self.value, Value::Struct(_) | Value::Module(_))
    }

    pub fn get_value(&self) -> &Value {
        &self.value
    }

    pub fn get_value_mut(&mut self) -> &mut Value {
        &mut self.value
    }

    pub fn set_value(&mut self, value: Value) {
        self.value = value;
    }

    pub fn get_type(&self) -> Type {
        self.type_.clone()
    }

    pub fn type_name(&self) -> &str {
        to_static(self.type_.display())
    }

    pub fn get_name(&self) -> &str {
        &self.name
    }
    
    pub fn is_static(&self) -> bool {
        self.is_static
    }

    pub fn is_public(&self) -> bool {
        self.is_public
    }

    pub fn is_final(&self) -> bool {
        self.is_final
    }

    pub fn set_final(&mut self, is_final: bool) {
        self.is_final = is_final;
    }

    pub fn set_static(&mut self, is_static: bool) {
        self.is_static = is_static;
    }

    pub fn set_public(&mut self, is_public: bool) {
        self.is_public = is_public;
    }

    pub fn set_type(&mut self, type_: Type) {
        self.type_ = type_;
    }

    pub fn set_name(&mut self, name: String) {
        self.name = name;
    }

    pub fn is_native(&self) -> bool {
        match &self.value {
            Value::Function(f) => f.is_native(),
            _ => false,
        }
    }
}
