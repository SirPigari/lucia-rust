use std::collections::HashMap;
use rand::{Rng, thread_rng};
use rand::seq::SliceRandom;
use std::sync::Arc;
use crate::env::runtime::functions::{Function, NativeFunction, Parameter};
use crate::env::runtime::value::Value;
use crate::env::runtime::variables::Variable;

use crate::{insert_native_fn, insert_native_var};

// This module provides random number and selection utilities.
// It includes functions for generating random integers, floats, making random choices,
// Lucia version 2.0.0, module: random@0.7.42

fn random_int_handler(args: &HashMap<String, Value>) -> Value {
    let mut rng = thread_rng();

    let min = match args.get("min") {
        Some(Value::Int(i)) => match i.to_i64() {
            Ok(v) => v,
            Err(_) => return Value::Error("ValueError".into(), "Invalid min value".into()),
        },
        _ => 0,
    };

    let max = match args.get("max") {
        Some(Value::Int(i)) => match i.to_i64() {
            Ok(v) => v,
            Err(_) => return Value::Error("ValueError".into(), "Invalid max value".into()),
        },
        _ => 100,
    };

    if max < min {
        return Value::Error("ValueError".into(), "max must be >= min".into());
    }

    let val = rng.gen_range(min..=max);
    Value::Int(crate::env::runtime::types::Int::from_i64(val))
}

fn random_float_handler(args: &HashMap<String, Value>) -> Value {
    let mut rng = thread_rng();

    let min = match args.get("min") {
        Some(Value::Float(f)) => match f.to_f64() {
            Ok(v) => v,
            Err(_) => return Value::Error("ValueError".into(), "Invalid min float".into()),
        },
        _ => 0.0,
    };

    let max = match args.get("max") {
        Some(Value::Float(f)) => match f.to_f64() {
            Ok(v) => v,
            Err(_) => return Value::Error("ValueError".into(), "Invalid max float".into()),
        },
        _ => 100.0,
    };

    if max < min {
        return Value::Error("ValueError".into(), "max must be >= min".into());
    }

    let val = rng.gen_range(min..=max);

    Value::Float(crate::env::runtime::types::Float::from_f64(val))
}

fn random_choice_handler(args: &HashMap<String, Value>) -> Value {
    match args.get("list") {
        Some(Value::List(list)) if !list.is_empty() => {
            let mut rng = thread_rng();
            let idx = rng.gen_range(0..list.len());
            list[idx].clone()
        }
        Some(_) => Value::Error("ValueError".into(), "list must not be empty".into()),
        None => Value::Error("TypeError".into(), "expected a list".into()),
    }
}

fn shuffle_handler(args: &HashMap<String, Value>) -> Value {
    match args.get("list") {
        Some(Value::List(list)) => {
            let mut cloned = list.clone();
            let mut rng = thread_rng();
            cloned.shuffle(&mut rng);
            Value::List(cloned)
        }
        Some(_) => Value::Error("TypeError".into(), "expected a list".into()),
        None => Value::Error("TypeError".into(), "expected a list".into()),
    }
}

pub fn register() -> HashMap<String, Variable> {
    let mut map = HashMap::new();

    insert_native_fn!(
        map,
        "randint",
        random_int_handler,
        vec![
            Parameter::positional_optional("min", "int", Value::Int(crate::env::runtime::types::Int::from_i64(0))),
            Parameter::positional_optional("max", "int", Value::Int(crate::env::runtime::types::Int::from_i64(100)))
        ],
        "int"
    );

    insert_native_fn!(
        map,
        "randfloat",
        random_float_handler,
        vec![
            Parameter::positional_optional("min", "float", Value::Float(crate::env::runtime::types::Float::from_f64(0.0))),
            Parameter::positional_optional("max", "float", Value::Float(crate::env::runtime::types::Float::from_f64(1.0)))
        ],
        "float"
    );

    insert_native_fn!(
        map,
        "randchoice",
        random_choice_handler,
        vec![
            Parameter::positional("list", "list")
        ],
        "any"
    );

    insert_native_fn!(
        map,
        "shuffle",
        shuffle_handler,
        vec![
            Parameter::positional("list", "list")
        ],
        "list"
    );

    map
}

pub fn init() -> Value {
    Value::Null
}
