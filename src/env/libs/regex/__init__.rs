use std::collections::HashMap;
use std::sync::Mutex;
use crate::env::runtime::functions::{Function, Parameter};
use crate::env::runtime::value::Value;
use crate::env::runtime::utils::to_static;
use crate::env::runtime::variables::Variable;
use super::regex_engine::{RegexEngine, normal, fancy};
use std::sync::Arc;
use once_cell::sync::Lazy;
use crate::insert_native_fn;

// This module provides regular expression matching capabilities.
// It includes functions for compiling, matching, and replacing patterns in strings.
// Lucia version 2.0.0, module: regex@0.9.0

static REGEX_CACHE: Lazy<Mutex<HashMap<String, RegexEngine>>> = Lazy::new(|| {
    Mutex::new(HashMap::new())
});

fn is_fancy_pattern(pattern: &str) -> bool {
    // lookahead, lookbehind, backref, atomic groups
    let fancy_features = ["(?=", "(?!", "(?<=", "(?<!", "\\1", "\\2", "(?>"];
    fancy_features.iter().any(|f| pattern.contains(f))
}


fn get_cached_regex(pattern: &str) -> Result<RegexEngine, String> {
    let mut cache = REGEX_CACHE.lock().unwrap();
    if let Some(re) = cache.get(pattern) {
        return Ok(re.clone());
    }

    if is_fancy_pattern(pattern) {
        match fancy::Regex::new(pattern) {
            Ok(re) => {
                cache.insert(pattern.to_string(), RegexEngine::Fancy(re.clone()));
                Ok(RegexEngine::Fancy(re))
            }
            Err(e) => Err(format!("Invalid fancy regex pattern: {}", e)),
        }
    } else {
        match normal::Regex::new(pattern) {
            Ok(re) => {
                cache.insert(pattern.to_string(), RegexEngine::Normal(re.clone()));
                Ok(RegexEngine::Normal(re))
            }
            Err(e) => Err(format!("Invalid regex pattern: {}", e)),
        }
    }
}

fn regex_match(args: &HashMap<String, Value>) -> Value {
    let pattern = match args.get("pattern") {
        Some(Value::String(s)) => s,
        _ => return Value::Error("TypeError".into(), "expected a string for 'pattern'".into(), None),
    };

    let value = match args.get("value") {
        Some(Value::String(s)) => s,
        _ => return Value::Error("TypeError".into(), "expected a string for 'value'".into(), None),
    };

    match get_cached_regex(pattern) {
        Ok(re) => match re.find(value) {
            Some(mat) => Value::String(mat.as_str().to_string()),
            None => Value::Null,
        },
        Err(e) => Value::Error("RegexError".into(), to_static(e), None),
    }
}

fn regex_replace(args: &HashMap<String, Value>) -> Value {
    let pattern = match args.get("pattern") {
        Some(Value::String(s)) => s,
        _ => return Value::Error("TypeError".into(), "expected a string for 'pattern'".into(), None),
    };

    let value = match args.get("value") {
        Some(Value::String(s)) => s,
        _ => return Value::Error("TypeError".into(), "expected a string for 'value'".into(), None),
    };

    let replacement = match args.get("replacement") {
        Some(Value::String(s)) => s.as_str(),
        _ => return Value::Error("TypeError".into(), "expected a string for 'replacement'".into(), None),
    };

    match get_cached_regex(pattern) {
        Ok(re) => {
            let replaced = re.replace_all(value, replacement);
            Value::String(replaced.to_owned())
        }
        Err(e) => Value::Error("RegexError".into(), to_static(e), None),
    }
}

fn regex_is_match(args: &HashMap<String, Value>) -> Value {
    let pattern = match args.get("pattern") {
        Some(Value::String(s)) => s,
        _ => return Value::Error("TypeError".into(), "expected a string for 'pattern'".into(), None),
    };

    let value = match args.get("value") {
        Some(Value::String(s)) => s,
        _ => return Value::Error("TypeError".into(), "expected a string for 'value'".into(), None),
    };

    match get_cached_regex(pattern) {
        Ok(re) => Value::Boolean(re.is_match(value)),
        Err(_) => Value::Boolean(false),
    }
}

fn regex_find_all(args: &HashMap<String, Value>) -> Value {
    let pattern = match args.get("pattern") {
        Some(Value::String(s)) => s,
        _ => return Value::Error("TypeError".into(), "expected a string for 'pattern'".into(), None),
    };
    let value = match args.get("value") {
        Some(Value::String(s)) => s,
        _ => return Value::Error("TypeError".into(), "expected a string for 'value'".into(), None),
    };

    match get_cached_regex(pattern) {
        Ok(re) => {
            let matches: Vec<Value> = re.find_iter(value)
                .into_iter()
                .map(|m| Value::String(m.as_str().to_string()))
                .collect();
            Value::List(matches)
        }
        Err(e) => Value::Error("RegexError".into(), to_static(e), None),
    }
}

fn regex_split(args: &HashMap<String, Value>) -> Value {
    let pattern = match args.get("pattern") {
        Some(Value::String(s)) => s,
        _ => return Value::Error("TypeError".into(), "expected a string for 'pattern'".into(), None),
    };
    let value = match args.get("value") {
        Some(Value::String(s)) => s,
        _ => return Value::Error("TypeError".into(), "expected a string for 'value'".into(), None),
    };

    match get_cached_regex(pattern) {
        Ok(re) => {
            let parts: Vec<Value> = re.split(value)
                .into_iter()
                .map(|s| Value::String(s.to_string()))
                .collect();
            Value::List(parts)
        }
        Err(e) => Value::Error("RegexError".into(), to_static(e), None),
    }
}

fn regex_count(args: &HashMap<String, Value>) -> Value {
    let pattern = match args.get("pattern") {
        Some(Value::String(s)) => s,
        _ => return Value::Error("TypeError".into(), "expected a string for 'pattern'".into(), None),
    };
    let value = match args.get("value") {
        Some(Value::String(s)) => s,
        _ => return Value::Error("TypeError".into(), "expected a string for 'value'".into(), None),
    };

    match get_cached_regex(pattern) {
        Ok(re) => Value::Int((re.find_iter(value).len() as i64).into()),
        Err(_) => Value::Int(0.into()),
    }
}

fn regex_capture(args: &HashMap<String, Value>) -> Value {
    let pattern = match args.get("pattern") {
        Some(Value::String(s)) => s,
        _ => return Value::Error("TypeError".into(), "expected a string for 'pattern'".into(), None),
    };
    let value = match args.get("value") {
        Some(Value::String(s)) => s,
        _ => return Value::Error("TypeError".into(), "expected a string for 'value'".into(), None),
    };

    match get_cached_regex(pattern) {
        Ok(re) => {
            match re.captures(value) {
                Some(c) => {
                    let mut keys = Vec::new();
                    let mut values = Vec::new();
                    for (i, mat) in c.groups.iter().enumerate() {
                        keys.push(Value::String(i.to_string()));
                        values.push(match mat {
                            Some(m) => Value::String(m.clone()),
                            None => Value::Null,
                        });
                    }
                    Value::Map { keys, values }
                },
                None => Value::Null,
            }
        }
        Err(e) => Value::Error("RegexError".into(), to_static(e), None),
    }
}

fn regex_match_all(args: &HashMap<String, Value>) -> Value {
    let pattern = match args.get("pattern") {
        Some(Value::String(s)) => s,
        _ => return Value::Error("TypeError".into(), "expected a string for 'pattern'".into(), None),
    };
    let value = match args.get("value") {
        Some(Value::String(s)) => s,
        _ => return Value::Error("TypeError".into(), "expected a string for 'value'".into(), None),
    };

    match get_cached_regex(pattern) {
        Ok(re) => {
            let matches: Vec<Value> = re.find_iter(value)
                .into_iter()
                .map(|m| {
                    let keys = vec!["match".into(), "start".into(), "end".into()];
                    let values = vec![
                        Value::String(m.as_str().to_string()),
                        Value::Int((m.start as i64).into()),
                        Value::Int((m.end as i64).into()),
                    ];
                    Value::Map { keys, values }
                })
                .collect();
            Value::List(matches)
        }
        Err(e) => Value::Error("RegexError".into(), to_static(e), None),
    }
}

fn regex_named_groups(args: &HashMap<String, Value>) -> Value {
    let pattern = match args.get("pattern") {
        Some(Value::String(s)) => s,
        _ => return Value::Error("TypeError".into(), "expected a string for 'pattern'".into(), None),
    };
    let value = match args.get("value") {
        Some(Value::String(s)) => s,
        _ => return Value::Error("TypeError".into(), "expected a string for 'value'".into(), None),
    };

    match get_cached_regex(pattern) {
        Ok(re) => {
            match re.captures(value) {
                Some(caps) => {
                    let mut keys = Vec::new();
                    let mut values = Vec::new();
                    for name in re.capture_names().into_iter().flatten() {
                        keys.push(Value::String(name.clone()));
                        values.push(match caps.name(&name) {
                            Some(m) => Value::String(m.to_string()),
                            None => Value::Null,
                        });
                    }
                    Value::Map { keys, values }
                },
                None => Value::Null,
            }
        }
        Err(e) => Value::Error("RegexError".into(), to_static(e), None),
    }
}

fn regex_escape(args: &HashMap<String, Value>) -> Value {
    let value = match args.get("value") {
        Some(Value::String(s)) => s,
        _ => return Value::Error("TypeError".into(), "expected a string for 'value'".into(), None),
    };

    Value::String(regex::escape(value))
}

fn regex_is_fancy(args: &HashMap<String, Value>) -> Value {
    let pattern = match args.get("pattern") {
        Some(Value::String(s)) => s,
        _ => return Value::Error("TypeError".into(), "expected a string for 'pattern'".into(), None),
    };

    Value::Boolean(is_fancy_pattern(pattern))
}

pub fn register() -> HashMap<String, Variable> {
    let mut map = HashMap::new();

    insert_native_fn!(
        map,
        "match",
        regex_match,
        vec![Parameter::positional("pattern", "str"), Parameter::positional("value", "str")],
        "any"
    );
    insert_native_fn!(
        map,
        "is_match",
        regex_is_match,
        vec![Parameter::positional("pattern", "str"), Parameter::positional("value", "str")],
        "bool"
    );
    insert_native_fn!(
        map,
        "replace",
        regex_replace,
        vec![
            Parameter::positional("pattern", "str"),
            Parameter::positional("value", "str"),
            Parameter::positional_optional("replacement", "str", Value::String("".into()))
        ],
        "str"
    );
        insert_native_fn!(
        map,
        "find_all",
        regex_find_all,
        vec![Parameter::positional("pattern", "str"), Parameter::positional("value", "str")],
        "list"
    );
    insert_native_fn!(
        map,
        "split",
        regex_split,
        vec![Parameter::positional("pattern", "str"), Parameter::positional("value", "str")],
        "list"
    );
    insert_native_fn!(
        map,
        "count",
        regex_count,
        vec![Parameter::positional("pattern", "str"), Parameter::positional("value", "str")],
        "int"
    );
    insert_native_fn!(
        map,
        "capture",
        regex_capture,
        vec![Parameter::positional("pattern", "str"), Parameter::positional("value", "str")],
        "map"
    );
    insert_native_fn!(
        map,
        "match_all",
        regex_match_all,
        vec![Parameter::positional("pattern", "str"), Parameter::positional("value", "str")],
        "list"
    );
    insert_native_fn!(
        map,
        "named_groups",
        regex_named_groups,
        vec![Parameter::positional("pattern", "str"), Parameter::positional("value", "str")],
        "map"
    );
    insert_native_fn!(
        map,
        "escape",
        regex_escape,
        vec![Parameter::positional("value", "str")],
        "str"
    );
    insert_native_fn!(
        map,
        "is_fancy",
        regex_is_fancy,
        vec![Parameter::positional("pattern", "str")],
        "bool"
    );

    map
}
