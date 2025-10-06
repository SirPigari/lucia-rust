use std::collections::HashMap;
use std::sync::Arc;
use rand::{Rng, SeedableRng};
use rand::rngs::SmallRng;
use rand::prelude::{SliceRandom, IndexedRandom};

use crate::env::runtime::functions::{Function, Parameter};
use crate::env::runtime::types::{Int, Float};
use crate::env::runtime::internal_structs::EffectFlags;
use crate::env::runtime::value::Value;
use crate::env::runtime::variables::Variable;
use crate::insert_native_fn;


#[cfg(not(target_arch = "wasm32"))]
use std::time::{SystemTime, UNIX_EPOCH};

#[cfg(target_arch = "wasm32")]
use web_sys::{window, Crypto};

fn make_rng() -> SmallRng {
    #[cfg(not(target_arch = "wasm32"))]
    {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("time went backwards")
            .subsec_nanos();

        let mut seed = [0u8; 32];
        for (i, b) in seed.iter_mut().enumerate() {
            *b = ((nanos >> (i % 4 * 8)) & 0xFF) as u8;
        }

        SmallRng::from_seed(seed)
    }

    #[cfg(target_arch = "wasm32")]
    {
        let mut seed = [0u8; 32];
        let crypto: Crypto = window()
            .unwrap()
            .crypto()
            .expect("crypto not available");
        crypto
            .get_random_values_with_u8_array(&mut seed)
            .expect("failed to get random values");
        SmallRng::from_seed(seed)
    }
}


fn random_int_handler(args: &HashMap<String, Value>) -> Value {
    let mut rng = make_rng();

    let min = match args.get("min") {
        Some(Value::Int(i)) => i.to_i64().unwrap_or(0),
        _ => 0,
    };
    let max = match args.get("max") {
        Some(Value::Int(i)) => i.to_i64().unwrap_or(100),
        _ => 100,
    };

    if max < min {
        return Value::Error("ValueError".into(), "max must be >= min".into(), None);
    }

    Value::Int(Int::from_i64(rng.random_range(min..=max)))
}

fn random_float_handler(args: &HashMap<String, Value>) -> Value {
    let mut rng = make_rng();

    let min = match args.get("min") {
        Some(Value::Float(f)) => f.to_f64().unwrap_or(0.0),
        _ => 0.0,
    };
    let max = match args.get("max") {
        Some(Value::Float(f)) => f.to_f64().unwrap_or(1.0),
        _ => 1.0,
    };

    if max < min {
        return Value::Error("ValueError".into(), "max must be >= min".into(), None);
    }

    Value::Float(Float::from_f64(rng.random_range(min..=max)))
}

fn random_choice_handler(args: &HashMap<String, Value>) -> Value {
    match args.get("list") {
        Some(Value::List(list)) if !list.is_empty() => {
            let mut rng = make_rng();
            list.choose(&mut rng).unwrap().clone()
        }
        Some(Value::List(_)) => Value::Error("ValueError".into(), "list must not be empty".into(), None),
        _ => Value::Error("TypeError".into(), "expected a list".into(), None),
    }
}

fn shuffle_handler(args: &HashMap<String, Value>) -> Value {
    match args.get("list") {
        Some(Value::List(list)) => {
            let mut cloned = list.clone();
            let mut rng = make_rng();
            cloned.shuffle(&mut rng);
            Value::List(cloned)
        }
        _ => Value::Error("TypeError".into(), "expected a list".into(), None),
    }
}

// ==== Register Functions ====
pub fn register() -> HashMap<String, Variable> {
    let mut map = HashMap::new();

    insert_native_fn!(
        map,
        "randint",
        random_int_handler,
        vec![
            Parameter::positional_optional("min", "int", Value::Int(Int::from_i64(0))),
            Parameter::positional_optional("max", "int", Value::Int(Int::from_i64(100))),
        ],
        "int",
        EffectFlags::PURE
    );

    insert_native_fn!(
        map,
        "randfloat",
        random_float_handler,
        vec![
            Parameter::positional_optional("min", "float", Value::Float(Float::from_f64(0.0))),
            Parameter::positional_optional("max", "float", Value::Float(Float::from_f64(1.0))),
        ],
        "float",
        EffectFlags::PURE
    );

    insert_native_fn!(
        map,
        "randchoice",
        random_choice_handler,
        vec![Parameter::positional("list", "list")],
        "any",
        EffectFlags::PURE
    );

    insert_native_fn!(
        map,
        "shuffle",
        shuffle_handler,
        vec![Parameter::positional("list", "list")],
        "list",
        EffectFlags::PURE
    );

    map
}
