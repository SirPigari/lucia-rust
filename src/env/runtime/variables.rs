use crate::env::runtime::value::Value;
use crate::env::runtime::utils::make_native_method;
use crate::env::runtime::functions::Parameter;
use std::collections::HashMap;
use std::fmt::{self, Display};
use std::str::FromStr;
use crate::env::runtime::types::{Float, Int};
use imagnum::{create_int, create_float};

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    name: String,
    pub value: Value,
    type_: String,
    is_static: bool,
    is_public: bool,
    is_final: bool,
    pub properties: HashMap<String, Variable>,
}

impl Variable {
    pub fn new(name: String, value: Value, type_: String, is_static: bool, is_public: bool, is_final: bool) -> Self {
        Self {
            name,
            value,
            type_,
            is_static,
            is_public,
            is_final,
            properties: HashMap::new(),
        }
    }

    pub fn init_properties(&mut self) {
        let val_clone = self.value.clone();
        let to_string = {
            let val_clone = self.value.clone();
            make_native_method(
                "toString",
                move |_args| {
                    match val_clone.to_string() {
                        s if !s.is_empty() => Value::String(s),
                        _ => Value::Null,
                    }
                },
                vec![],
                "str",
                true, true, true,
                None,
            )
        };
    
        self.properties.insert(
            "toString".to_string(),
            Variable::new(
                "toString".to_string(),
                to_string,
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
                    make_native_method(
                        "toBytes",
                        move |_args| {
                            match val_clone.to_bytes() {
                                Some(bytes) => Value::Bytes(bytes),
                                None => Value::Null,
                            }
                        },
                        vec![],
                        "bytes",
                        true, true, true,
                        None,
                    )
                };
                
                let endswith = {
                    let val_clone = self.value.clone();
                    make_native_method(
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
                        true, true, true,
                        None,
                    )
                };
                
                let startswith = {
                    let val_clone = self.value.clone();
                    make_native_method(
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
                        true, true, true,
                        None,
                    )
                };
                let to_int = {
                    let val_clone = self.value.clone();
                    make_native_method(
                        "toInt",
                        move |_args| {
                            match &val_clone {
                                Value::String(s) => Value::Int(create_int(s)),
                                _ => Value::Null,
                            }
                        },
                        vec![],
                        "int",
                        true, true, true,
                        None,
                    )
                };
                let to_float = {
                    let val_clone = self.value.clone();
                    make_native_method(
                        "toFloat",
                        move |_args| {
                            match &val_clone {
                                Value::String(s) => Value::Float(create_float(s)),
                                _ => Value::Null,
                            }
                        },
                        vec![],
                        "float",
                        true, true, true,
                        None,
                    )
                };
                let split = {
                    let val_clone = self.value.clone();
                    make_native_method(
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
                        true, true, true,
                        None,
                    )
                };
                let join = {
                    let val_clone = self.value.clone();
                    make_native_method(
                        "join",
                        move |args| {
                            if let Some(Value::List(parts)) = args.get("parts") {
                                if let Value::String(val) = &val_clone {
                                    let joined: String = parts.iter()
                                        .filter_map(|v| match v {
                                            Value::String(s) => Some(s.clone()),
                                            _ => None,
                                        })
                                        .collect::<Vec<String>>()
                                        .join(val);
                                    return Value::String(joined);
                                }
                            }
                            Value::Null
                        },
                        vec![Parameter::positional("parts", "list")],
                        "str",
                        true, true, true,
                        None,
                    )
                };

                self.properties.insert(
                    "toBytes".to_string(),
                    Variable::new(
                        "toBytes".to_string(),
                        to_bytes,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "endsWith".to_string(),
                    Variable::new(
                        "endsWith".to_string(),
                        endswith,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "startsWith".to_string(),
                    Variable::new(
                        "startsWith".to_string(),
                        startswith,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "toInt".to_string(),
                    Variable::new(
                        "toInt".to_string(),
                        to_int,
                        "function".to_string(),
                        false,
                        true,
                        true,
                    ),
                );
                self.properties.insert(
                    "toFloat".to_string(),
                    Variable::new(
                        "toFloat".to_string(),
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
            }
            Value::Int(_) => {
                let to_float = {
                    let val_clone = self.value.clone();
                    make_native_method(
                        "toFloat",
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
                        true, true, true,
                        None,
                    )
                };                
            
                let format = {
                    let val_clone = self.value.clone();
                    make_native_method(
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
                        true, true, true,
                        None,
                    )
                };
                
                self.properties.insert(
                    "toFloat".to_string(),
                    Variable::new(
                        "toFloat".to_string(),
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
                    make_native_method(
                        "toInt",
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
                        true, true, true,
                        None,
                    )
                };                
            
                let format = {
                    let val_clone = self.value.clone();
                    make_native_method(
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
                        true, true, true,
                        None,
                    )
                };

                let round = {
                    let val_clone = self.value.clone();
                    make_native_method(
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
                        true, true, true,
                        None,
                    )
                };
            
                self.properties.insert(
                    "toInt".to_string(),
                    Variable::new(
                        "toInt".to_string(),
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
                let append = {
                    let val_clone = self.value.clone();
                    make_native_method(
                        "append",
                        move |args| {
                            if let Some(item) = args.get("item") {
                                if let Value::List(list) = &val_clone {
                                    let mut new_list = list.clone();
            
                                    new_list.push(item.clone());
            
                                    return Value::List(new_list);
                                }
                            }
                            Value::Null
                        },
                        vec![Parameter::positional("item", "any")],
                        "list",
                        true, true, true,
                        None,
                    )
                };
            
                let extend = {
                    let val_clone = self.value.clone();
                    make_native_method(
                        "extend",
                        move |args| {
                            if let Some(item) = args.get("item") {
                                if let (Value::List(list), Value::List(to_extend)) = (&val_clone, item) {
                                    let mut new_list = list.clone();
                                    new_list.extend(to_extend.clone());
                                    return Value::List(new_list);
                                }
                            }
                            Value::Null
                        },
                        vec![Parameter::positional("item", "list")],
                        "list",
                        true, true, true,
                        None,
                    )
                };
            
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
            }            
            Value::Pointer(_) => {
                let extract_ptr = {
                    let val_clone = self.value.clone();
                    make_native_method(
                        "extractPtr",
                        move |_args| {
                            match &val_clone {
                                Value::Pointer(ptr) => Value::Int(create_int(&ptr.to_string())),
                                _ => Value::Null,
                            }
                        },
                        vec![],
                        "int",
                        true, true, true,
                        None,
                    )
                };
                
                self.properties.insert(
                    "extractPtr".to_string(),
                    Variable::new(
                        "extractPtr".to_string(),
                        extract_ptr,
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
        !self.properties.is_empty()
    }

    pub fn get_value(&self) -> &Value {
        &self.value
    }

    pub fn set_value(&mut self, value: Value) {
        self.value = value;
    }

    pub fn type_name(&self) -> &str {
        &self.type_
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

    pub fn set_type(&mut self, type_: String) {
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