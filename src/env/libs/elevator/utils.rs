use std::collections::HashMap;
use crate::env::runtime::functions::{Function, Parameter};
use crate::env::runtime::types::{Int, Float};
use crate::env::runtime::value::Value;
use crate::env::runtime::utils::{
    parse_type,
    levenshtein_distance,
    hex_to_ansi,
    format_value,
    unescape_string,
    replace_accented,
    sanitize_alias,
};
use crate::env::runtime::variables::Variable;
use crate::env::runtime::internal_structs::EffectFlags;

use crate::insert_native_fn;

// This module provides various utilities.
// Lucia version 2.0.0, module: elevator@42.0.0

pub fn create_utils_map() -> HashMap<String, Variable> {
    let mut map = HashMap::with_capacity(10);

    let str_or_list = parse_type("str | list");

    insert_native_fn!(
        map,
        "levenshtein_distance",
        levenshtein_distance_handler,
        vec![
            Parameter::positional("s1", "str"),
            Parameter::positional("s2", "str")
        ],
        "int",
        EffectFlags::PURE
    );
    insert_native_fn!(
        map,
        "hex_to_ansi",
        hex_to_ansi_handler,
        vec![Parameter::positional("hex", "str")],
        "str",
        EffectFlags::PURE
    );
    insert_native_fn!(
        map,
        "format_value",
        format_value_handler,
        vec![Parameter::positional("value", "any")],
        "str",
        EffectFlags::PURE
    );
    insert_native_fn!(
        map,
        "unescape_string",
        unescape_string_handler,
        vec![Parameter::positional("s", "str")],
        "str",
        EffectFlags::PURE
    );
    insert_native_fn!(
        map,
        "replace_accented",
        replace_accented_handler,
        vec![Parameter::positional("s", "str")],
        "str",
        EffectFlags::PURE
    );
    insert_native_fn!(
        map,
        "valid_alias",
        sanitize_alias_handler,
        vec![Parameter::positional("s", "str")],
        "str",
        EffectFlags::PURE
    );
    insert_native_fn!(
        map,
        "decode_rle",
        decode_rle,
        vec![
            Parameter::positional_pt("rle", &str_or_list),
        ],
        "any",
        EffectFlags::PURE
    );
    insert_native_fn!(
        map,
        "nth_prime",
        nth_prime,
        vec![
            Parameter::positional("n", "int"),
        ],
        "int",
        EffectFlags::PURE
    );

    map
}

fn nth_prime_native(n: u64) -> u64 {
    if n < 6 {
        return [0, 2, 3, 5, 7, 11][n as usize];
    }
    let limit = (n as f64 * (n as f64).ln() * 1.2) as usize;
    let mut sieve = vec![true; limit];
    sieve[0] = false;
    sieve[1] = false;

    for i in 2..((limit as f64).sqrt() as usize + 1) {
        if sieve[i] {
            for j in ((i * i)..limit).step_by(i) {
                sieve[j] = false;
            }
        }
    }

    let mut count = 0;
    for (i, &is_prime) in sieve.iter().enumerate() {
        if is_prime {
            count += 1;
            if count == n {
                return i as u64;
            }
        }
    }
    0
}

fn nth_prime_f64(n: f64) -> u64 {
    if n < 6.0 {
        return [0.0, 2.0, 3.0, 5.0, 7.0, 11.0][n as usize] as u64;
    }
    let est = n * (n.ln() + n.ln().ln() - 1.0 + ((n.ln().ln() - 2.0) / n.ln()));
    est.round() as u64
}

fn nth_prime_big(n: &Int) -> Int {
    if *n < Int::from(6) {
        match n {
            x if *x == Int::from(1) => Int::from(2),
            x if *x == Int::from(2) => Int::from(3),
            x if *x == Int::from(3) => Int::from(5),
            x if *x == Int::from(4) => Int::from(7),
            x if *x == Int::from(5) => Int::from(11),
            _ => unreachable!(),
        }
    } else {
        let n_f = Float::from(n.clone());
        let ln_n = n_f.ln().expect("Failed to compute ln(n)");
        let ln_ln_n = ln_n.ln().expect("Failed to compute ln(ln(n))");
        let correction = ((&ln_ln_n - Float::from(2.0)).expect("Failed to compute correction") / &ln_n).expect("Failed to compute correction term");
        let est =(&n_f * ((((&ln_n + &ln_ln_n).expect("Failed to compute ln(n) + ln(ln(n))") - &*Float::ONE).expect("Failed to compute ln(n) + ln(ln(n)) - 1") + correction).expect("Failed to compute final estimate"))).expect("Failed to compute estimate");
        est.round(0).to_int().unwrap()
    }
}

fn nth_prime(args: &HashMap<String, Value>) -> Value {
    let n = match args.get("n") {
        Some(Value::Int(i)) => i.clone(),
        _ => return Value::new_error("TypeError", "n must be an integer.", None),
    };
    if let Ok(n_u64) = n.clone().to_u64() {
        if n_u64 <= 1_000_000 {
            return Value::Int(Int::from(nth_prime_native(n_u64)));
        } else if n_u64 <= 1_000_000_000 {
            return Value::Int(Int::from(nth_prime_f64(n_u64 as f64)));
        }
    }
    Value::Int(nth_prime_big(&n))
}

fn decode_rle(args: &HashMap<String, Value>) -> Value {
    let rle = match args.get("rle") {
        Some(v) => v,
        _ => return Value::new_error("TypeError", "rle must be a string.", None),
    };

    match rle {
        Value::String(s) => {
            let mut decoded = String::new();
            let mut count_str = String::new();

            for c in s.chars() {
                if c.is_digit(10) {
                    count_str.push(c);
                } else {
                    let count: usize = if count_str.is_empty() {
                        1
                    } else {
                        count_str.parse().unwrap_or(1)
                    };
                    decoded.push_str(&c.to_string().repeat(count));
                    count_str.clear();
                }
            }

            Value::String(decoded)
        }
        Value::List(lst) => {
            let mut decoded = Vec::with_capacity(lst.len() * 2);

            for item in lst {
                match item {
                    Value::Int(_) => {
                        decoded.push(item.clone());
                    },
                    Value::List(inner_lst) => {
                        if inner_lst.len() != 2 {
                            return Value::new_error("ValueError", "Each RLE pair must have exactly two elements.", None);
                        }
                        let count = match &inner_lst[0] {
                            Value::Int(c) => c.to_usize().unwrap_or(0),
                            _ => return Value::new_error("TypeError", "RLE count must be an integer.", None),
                        };
                        let value = inner_lst[1].clone();
                        for _ in 0..count {
                            decoded.push(value.clone());
                        }
                    },
                    _ => return Value::new_error("TypeError", "All items in rle list must be int.", None),
                }
            }         
            Value::List(decoded)
        }
        _ => Value::new_error("TypeError", "rle must be a string or list.", None),
    }
}

fn levenshtein_distance_handler(args: &HashMap<String, Value>) -> Value {
    if let (Some(Value::String(s1)), Some(Value::String(s2))) = (args.get("s1"), args.get("s2")) {
        Value::Int(Int::from_i64(levenshtein_distance(s1, s2) as i64))
    } else {
        Value::new_error("TypeError", "expected two strings", None)
    }
}

fn hex_to_ansi_handler(args: &HashMap<String, Value>) -> Value {
    if let Some(Value::String(hex)) = args.get("hex") {
        Value::String(hex_to_ansi(hex, true))
    } else {
        Value::new_error("TypeError", "expected a string", None)
    }
}

fn format_value_handler(args: &HashMap<String, Value>) -> Value {
    if let Some(value) = args.get("value") {
        Value::String(format_value(value))
    } else {
        Value::new_error("TypeError", "expected a value", None)
    }
}

fn unescape_string_handler(args: &HashMap<String, Value>) -> Value {
    if let Some(Value::String(s)) = args.get("s") {
        match unescape_string(s) {
            Ok(unescaped) => Value::String(unescaped),
            Err(e) => Value::new_error("UnescapeError", e, None),
        }
    } else {
        Value::new_error("TypeError", "expected a string", None)
    }
}

fn replace_accented_handler(args: &HashMap<String, Value>) -> Value {
    if let Some(Value::String(s)) = args.get("s") {
        let replaced: String = s.chars()
            .map(|c| replace_accented(c))
            .collect();
        Value::String(replaced)
    } else {
        Value::new_error("TypeError", "expected a string", None)
    }
}

fn sanitize_alias_handler(args: &HashMap<String, Value>) -> Value {
    if let Some(Value::String(s)) = args.get("s") {
        Value::String(sanitize_alias(s))
    } else {
        Value::new_error("TypeError", "expected a string", None)
    }
}