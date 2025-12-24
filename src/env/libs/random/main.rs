use std::collections::HashMap;
use crate::env::runtime::functions::{Parameter};
use crate::env::runtime::types::{Int, Float};
use crate::env::runtime::internal_structs::EffectFlags;
use crate::env::runtime::value::Value;
use crate::env::runtime::variables::Variable;
use crate::insert_native_fn;
use crate::env::runtime::functions::Function;

#[cfg(not(target_arch = "wasm32"))]
use imagnum::random::{randint as imagnum_randint, randfloat as imagnum_randfloat, rand as imagnum_rand};

#[cfg(target_arch = "wasm32")]
use web_sys::{window, Crypto};

#[cfg(target_arch = "wasm32")]
fn wasm_rand_f64() -> f64 {
    let crypto: Crypto = window().unwrap().crypto().expect("crypto not available");
    let mut buf = [0u8; 8];
    crypto.get_random_values_with_u8_array(&mut buf).unwrap();
    let val = u64::from_le_bytes(buf);
    (val as f64) / (u64::MAX as f64)
}

#[cfg(target_arch = "wasm32")]
fn wasm_rand_i64(min: i64, max: i64) -> i64 {
    if max < min {
        return min;
    }
    let r = wasm_rand_f64();
    min + ((max - min) as f64 * r) as i64
}

#[cfg(target_arch = "wasm32")]
fn random_int_handler(args: &HashMap<String, Value>) -> Value {
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

    Value::Int(Int::from_i64(wasm_rand_i64(min, max)))
}

#[cfg(not(target_arch = "wasm32"))]
fn random_int_handler(args: &HashMap<String, Value>) -> Value {
    let min = match args.get("min") {
        Some(Value::Int(i)) => i.clone(),
        _ => Int::from_i64(0),
    };
    let max = match args.get("max") {
        Some(Value::Int(i)) => i.clone(),
        _ => Int::from_i64(100),
    };

    Value::Int(imagnum_randint(&min, &max))
}

#[cfg(target_arch = "wasm32")]
fn random_float_handler(args: &HashMap<String, Value>) -> Value {
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

    let val = min + (max - min) * wasm_rand_f64();
    Value::Float(Float::from_f64(val))
}

#[cfg(not(target_arch = "wasm32"))]
fn random_float_handler(args: &HashMap<String, Value>) -> Value {
    let min = match args.get("min") {
        Some(Value::Float(f)) => f.clone(),
        _ => Float::from_f64(0.0),
    };
    let max = match args.get("max") {
        Some(Value::Float(f)) => f.clone(),
        _ => Float::from_f64(1.0),
    };

    Value::Float(imagnum_randfloat(&min, &max))
}

#[cfg(target_arch = "wasm32")]
fn random_choice_handler(args: &HashMap<String, Value>) -> Value {
    match args.get("list") {
        Some(Value::List(list)) if !list.is_empty() => {
            let idx = wasm_rand_i64(0, (list.len() - 1) as i64) as usize;
            list[idx].clone()
        }
        Some(Value::List(_)) => Value::Error("ValueError".into(), "list must not be empty".into(), None),
        _ => Value::Error("TypeError".into(), "expected a list".into(), None),
    }
}

#[cfg(not(target_arch = "wasm32"))]
fn random_choice_handler(args: &HashMap<String, Value>) -> Value {
    use rand::prelude::IndexedRandom;
    use rand::rngs::SmallRng;
    use rand::SeedableRng;
    use std::time::{SystemTime, UNIX_EPOCH};

    match args.get("list") {
        Some(Value::List(list)) if !list.is_empty() => {
            let nanos = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .subsec_nanos();
            let mut seed = [0u8; 32];
            for (i, b) in seed.iter_mut().enumerate() {
                *b = ((nanos >> (i % 4 * 8)) & 0xFF) as u8;
            }
            let mut rng = SmallRng::from_seed(seed);
            list.choose(&mut rng).unwrap().clone()
        }
        Some(Value::List(_)) => Value::Error("ValueError".into(), "list must not be empty".into(), None),
        _ => Value::Error("TypeError".into(), "expected a list".into(), None),
    }
}

#[cfg(target_arch = "wasm32")]
fn shuffle_handler(args: &HashMap<String, Value>) -> Value {
    match args.get("list") {
        Some(Value::List(list)) => {
            let mut cloned = list.clone();
            let len = cloned.len();
            for i in (1..len).rev() {
                let j = wasm_rand_i64(0, i as i64) as usize;
                cloned.swap(i, j);
            }
            Value::List(cloned)
        }
        _ => Value::Error("TypeError".into(), "expected a list".into(), None),
    }
}

#[cfg(not(target_arch = "wasm32"))]
fn shuffle_handler(args: &HashMap<String, Value>) -> Value {
    use rand::prelude::SliceRandom;
    use rand::rngs::SmallRng;
    use rand::SeedableRng;
    use std::time::{SystemTime, UNIX_EPOCH};

    match args.get("list") {
        Some(Value::List(list)) => {
            let mut cloned = list.clone();
            let nanos = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .subsec_nanos();
            let mut seed = [0u8; 32];
            for (i, b) in seed.iter_mut().enumerate() {
                *b = ((nanos >> (i % 4 * 8)) & 0xFF) as u8;
            }
            let mut rng = SmallRng::from_seed(seed);
            cloned.shuffle(&mut rng);
            Value::List(cloned)
        }
        _ => Value::Error("TypeError".into(), "expected a list".into(), None),
    }
}

#[cfg(target_arch = "wasm32")]
fn rand_handler(_: &HashMap<String, Value>) -> Value {
    Value::Float(Float::from_f64(wasm_rand_f64()))
}

#[cfg(not(target_arch = "wasm32"))]
fn rand_handler(_: &HashMap<String, Value>) -> Value {
    Value::Float(imagnum_rand())
}

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
        "rand",
        rand_handler,
        vec![],
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
