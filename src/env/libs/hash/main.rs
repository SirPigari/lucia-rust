use std::collections::HashMap;
use std::hash::Hasher;
use crate::env::runtime::variables::Variable;
use crate::env::runtime::functions::{Function, Parameter};
use crate::env::runtime::internal_structs::EffectFlags;
use crate::env::runtime::utils::parse_type;
use crate::env::runtime::value::Value;
use crate::env::runtime::types::Int;
use crate::{insert_native_fn, insert_native_var};

use sha2::{Sha224, Sha256, Sha384, Sha512, Digest};
use sha3::{Sha3_224, Sha3_256, Sha3_384, Sha3_512};
use blake2::{Blake2b512, Blake2s256};
use blake3;
use md5;
use sha1::Sha1;
use ripemd::Ripemd160;
use argon2::{Argon2, PasswordHasher};
use argon2::password_hash::SaltString;
use pbkdf2::pbkdf2_hmac;
use bcrypt::hash as bcrypt_hash;
use scrypt::{scrypt, Params as ScryptParams};
use crc32fast;
use fnv::FnvHasher;

pub fn register() -> HashMap<String, Variable> {
    let mut map = HashMap::new();
    let input_type = parse_type("list[int] | str");

    macro_rules! hash_fn {
        ($name:literal, $algo:expr) => {
            insert_native_fn!(map, $name, |args: &HashMap<String, Value>| {
                let b = match args.get("input") {
                    Some(Value::String(s)) => s.as_bytes().to_vec(),
                    Some(Value::List(l)) => {
                        let mut out = Vec::with_capacity(l.len());
                        for v in l {
                            match v {
                                Value::Int(i) => match i.to_u8() {
                                    Ok(b) => out.push(b),
                                    Err(_) => return Value::Error("ValueError", "invalid byte", None),
                                },
                                _ => return Value::Error("TypeError", "list[int] required", None),
                            }
                        }
                        out
                    }
                    _ => return Value::Error("TypeError", "input must be list[int] or str", None),
                };
                let hash = $algo(&b);
                Value::String(hex::encode(&hash))
            }, vec![Parameter::positional_pt("input", &input_type)], "str", EffectFlags::PURE);
        };
    }

    hash_fn!("md5", |b: &[u8]| md5::compute(b).0.to_vec());
    hash_fn!("sha1", |b: &[u8]| Sha1::digest(b).as_slice().to_vec());
    hash_fn!("sha224", |b: &[u8]| Sha224::digest(b).as_slice().to_vec());
    hash_fn!("sha256", |b: &[u8]| Sha256::digest(b).as_slice().to_vec());
    hash_fn!("sha384", |b: &[u8]| Sha384::digest(b).as_slice().to_vec());
    hash_fn!("sha512", |b: &[u8]| Sha512::digest(b).as_slice().to_vec());
    hash_fn!("sha3_224", |b: &[u8]| Sha3_224::digest(b).as_slice().to_vec());
    hash_fn!("sha3_256", |b: &[u8]| Sha3_256::digest(b).as_slice().to_vec());
    hash_fn!("sha3_384", |b: &[u8]| Sha3_384::digest(b).as_slice().to_vec());
    hash_fn!("sha3_512", |b: &[u8]| Sha3_512::digest(b).as_slice().to_vec());
    hash_fn!("blake2b", |b: &[u8]| Blake2b512::digest(b).as_slice().to_vec());
    hash_fn!("blake2s", |b: &[u8]| Blake2s256::digest(b).as_slice().to_vec());
    hash_fn!("blake3", |b: &[u8]| blake3::hash(b).as_bytes().to_vec());
    hash_fn!("ripemd160", |b: &[u8]| Ripemd160::digest(b).as_slice().to_vec());

    insert_native_fn!(map, "argon2", |args: &HashMap<String, Value>| {
        let password = match args.get("password") {
            Some(Value::String(s)) => s.as_bytes().to_vec(),
            Some(Value::List(l)) => {
                let mut out = Vec::with_capacity(l.len());
                for v in l {
                    match v {
                        Value::Int(i) => match i.to_u8() {
                            Ok(b) => out.push(b),
                            Err(_) => return Value::Error("ValueError", "invalid byte", None),
                        },
                        _ => return Value::Error("TypeError", "list[int] required", None),
                    }
                }
                out
            }
            _ => return Value::Error("TypeError", "input must be list[int] or str", None),
        };
        let salt = match args.get("salt") {
            Some(Value::String(s)) => s.as_bytes(),
            _ => return Value::Error("TypeError", "salt required", None),
        };
        let memory = args.get("memory_kib").and_then(|v| if let Value::Int(i) = v { Some(i) } else { None }).and_then(|i| i.to_u32().ok()).unwrap_or(19456);
        let iterations = args.get("iterations").and_then(|v| if let Value::Int(i) = v { Some(i) } else { None }).and_then(|i| i.to_u32().ok()).unwrap_or(2);
        let parallelism = args.get("parallelism").and_then(|v| if let Value::Int(i) = v { Some(i) } else { None }).and_then(|i| i.to_u32().ok()).unwrap_or(1);

        let params = match argon2::Params::new(memory, iterations, parallelism, None) {
            Ok(p) => p,
            Err(_) => return Value::Error("ValueError", "invalid argon2 params", None),
        };

        let argon = Argon2::new(argon2::Algorithm::Argon2id, argon2::Version::V0x13, params);
        let salt = SaltString::encode_b64(salt).unwrap();
        match argon.hash_password(&password, &salt) {
            Ok(h) => Value::String(h.to_string()),
            Err(_) => Value::Error("HashError", "argon2 failed", None),
        }
    }, vec![
        Parameter::positional_pt("password", &input_type),
        Parameter::positional("salt", "str"),
        Parameter::positional_optional("memory_kib", "int", Value::Int(Int::from_i64(19456))),
        Parameter::positional_optional("iterations", "int", Value::Int(Int::from_i64(2))),
        Parameter::positional_optional("parallelism", "int", Value::Int(Int::from_i64(1))),
    ], "str", EffectFlags::PURE);

    insert_native_fn!(map, "bcrypt", |args: &HashMap<String, Value>| {
        let password = match args.get("password") {
            Some(Value::String(s)) => s.as_bytes().to_vec(),
            Some(Value::List(l)) => {
                let mut out = Vec::with_capacity(l.len());
                for v in l {
                    match v {
                        Value::Int(i) => match i.to_u8() {
                            Ok(b) => out.push(b),
                            Err(_) => return Value::Error("ValueError", "invalid byte", None),
                        },
                        _ => return Value::Error("TypeError", "list[int] required", None),
                    }
                }
                out
            }
            _ => return Value::Error("TypeError", "input must be list[int] or str", None),
        };
        let cost = args.get("cost").and_then(|v| if let Value::Int(i) = v { Some(i) } else { None }).and_then(|i| i.to_u32().ok()).unwrap_or(12);
        match bcrypt_hash(password, cost) {
            Ok(h) => Value::String(h),
            Err(_) => Value::Error("HashError", "bcrypt failed", None),
        }
    }, vec![
        Parameter::positional_pt("password", &input_type),
        Parameter::positional_optional("cost", "int", Value::Int(Int::from_i64(12))),
    ], "str", EffectFlags::PURE);

    insert_native_fn!(map, "pbkdf2", |args: &HashMap<String, Value>| {
        let password = match args.get("password") {
            Some(Value::String(s)) => s.as_bytes().to_vec(),
            Some(Value::List(l)) => {
                let mut out = Vec::with_capacity(l.len());
                for v in l {
                    match v {
                        Value::Int(i) => match i.to_u8() {
                            Ok(b) => out.push(b),
                            Err(_) => return Value::Error("ValueError", "invalid byte", None),
                        },
                        _ => return Value::Error("TypeError", "list[int] required", None),
                    }
                }
                out
            }
            _ => return Value::Error("TypeError", "input must be list[int] or str", None),
        };
        let salt = match args.get("salt") {
            Some(Value::String(s)) => s.as_bytes().to_vec(),
            Some(Value::List(l)) => {
                let mut out = Vec::with_capacity(l.len());
                for v in l {
                    match v {
                        Value::Int(i) => match i.to_u8() {
                            Ok(b) => out.push(b),
                            Err(_) => return Value::Error("ValueError", "invalid byte", None),
                        },
                        _ => return Value::Error("TypeError", "list[int] required", None),
                    }
                }
                out
            }
            _ => return Value::Error("TypeError", "salt required", None),
        };
        let iterations = args.get("iterations").and_then(|v| if let Value::Int(i) = v { Some(i) } else { None }).and_then(|i| i.to_u32().ok()).unwrap_or(100_000);
        let out_len = args.get("output_len").and_then(|v| if let Value::Int(i) = v { Some(i) } else { None }).and_then(|i| i.to_usize().ok()).unwrap_or(32);
        let mut out = vec![0u8; out_len];
        pbkdf2_hmac::<Sha256>(&password, &salt, iterations, &mut out);
        Value::String(hex::encode(out))
    }, vec![
        Parameter::positional_pt("password", &input_type),
        Parameter::positional_pt("salt", &input_type),
        Parameter::positional_optional("iterations", "int", Value::Int(Int::from_i64(100_000))),
        Parameter::positional_optional("output_len", "int", Value::Int(Int::from_i64(32))),
    ], "str", EffectFlags::PURE);

    insert_native_fn!(map, "scrypt", |args: &HashMap<String, Value>| {
        let password = match args.get("password") {
            Some(Value::String(s)) => s.as_bytes().to_vec(),
            Some(Value::List(l)) => {
                let mut out = Vec::with_capacity(l.len());
                for v in l {
                    match v {
                        Value::Int(i) => match i.to_u8() {
                            Ok(b) => out.push(b),
                            Err(_) => return Value::Error("ValueError", "invalid byte", None),
                        },
                        _ => return Value::Error("TypeError", "list[int] required", None),
                    }
                }
                out
            }
            _ => return Value::Error("TypeError", "password required", None),
        };
        let salt = match args.get("salt") {
            Some(Value::String(s)) => s.as_bytes().to_vec(),
            Some(Value::List(l)) => {
                let mut out = Vec::with_capacity(l.len());
                for v in l {
                    match v {
                        Value::Int(i) => match i.to_u8() {
                            Ok(b) => out.push(b),
                            Err(_) => return Value::Error("ValueError", "invalid byte", None),
                        },
                        _ => return Value::Error("TypeError", "list[int] required", None),
                    }
                }
                out
            }
            _ => return Value::Error("TypeError", "salt required", None),
        };
        let n = args.get("n").and_then(|v| if let Value::Int(i) = v { Some(i) } else { None }).and_then(|i| i.to_u32().ok()).unwrap_or(16384);
        let r = args.get("r").and_then(|v| if let Value::Int(i) = v { Some(i) } else { None }).and_then(|i| i.to_u32().ok()).unwrap_or(8);
        let p = args.get("p").and_then(|v| if let Value::Int(i) = v { Some(i) } else { None }).and_then(|i| i.to_u32().ok()).unwrap_or(1);
        let out_len = args.get("output_len").and_then(|v| if let Value::Int(i) = v { Some(i) } else { None }).and_then(|i| i.to_usize().ok()).unwrap_or(32);
        let mut out = vec![0u8; out_len];
        let log_n = n.ilog2() as u8;
        match ScryptParams::new(log_n, r, p, out_len) {
            Ok(params) => {
                if let Err(_) = scrypt(&password, &salt, &params, &mut out) {
                    return Value::Error("HashError", "scrypt failed", None);
                }
                Value::String(hex::encode(out))
            }
            Err(_) => Value::Error("ValueError", "invalid scrypt params", None),
        }
    }, vec![
        Parameter::positional_pt("password", &input_type),
        Parameter::positional_pt("salt", &input_type),
        Parameter::positional_optional("n", "int", Value::Int(Int::from_i64(16384))),
        Parameter::positional_optional("r", "int", Value::Int(Int::from_i64(8))),
        Parameter::positional_optional("p", "int", Value::Int(Int::from_i64(1))),
        Parameter::positional_optional("output_len", "int", Value::Int(Int::from_i64(32))),
    ], "str", EffectFlags::PURE);

    insert_native_fn!(map, "crc32", |args: &HashMap<String, Value>| {
        let b = match args.get("input") {
            Some(Value::String(s)) => s.as_bytes().to_vec(),
            Some(Value::List(l)) => {
                let mut out = Vec::with_capacity(l.len());
                for v in l {
                    match v {
                        Value::Int(i) => match i.to_u8() {
                            Ok(b) => out.push(b),
                            Err(_) => return Value::Error("ValueError", "invalid byte", None),
                        },
                        _ => return Value::Error("TypeError", "list[int] required", None),
                    }
                }
                out
            }
            _ => return Value::Error("TypeError", "input must be list[int] or str", None),
        };
        Value::Int(Int::from_i64(crc32fast::hash(&b) as i64))
    }, vec![Parameter::positional_pt("input", &input_type)], "int", EffectFlags::PURE);

    insert_native_fn!(map, "fnv1a32", |args: &HashMap<String, Value>| {
        let b = match args.get("input") {
            Some(Value::String(s)) => s.as_bytes().to_vec(),
            Some(Value::List(l)) => {
                let mut out = Vec::with_capacity(l.len());
                for v in l {
                    match v {
                        Value::Int(i) => match i.to_u8() {
                            Ok(b) => out.push(b),
                            Err(_) => return Value::Error("ValueError", "invalid byte", None),
                        },
                        _ => return Value::Error("TypeError", "list[int] required", None),
                    }
                }
                out
            }
            _ => return Value::Error("TypeError", "input must be list[int] or str", None),
        };
        let mut h = FnvHasher::default();
        h.write(&b);
        Value::Int(Int::from_i64(h.finish() as i64))
    }, vec![Parameter::positional_pt("input", &input_type)], "int", EffectFlags::PURE);

    insert_native_fn!(map, "fnv1a64", |args: &HashMap<String, Value>| {
        let b = match args.get("input") {
            Some(Value::String(s)) => s.as_bytes().to_vec(),
            Some(Value::List(l)) => {
                let mut out = Vec::with_capacity(l.len());
                for v in l {
                    match v {
                        Value::Int(i) => match i.to_u8() {
                            Ok(b) => out.push(b),
                            Err(_) => return Value::Error("ValueError", "invalid byte", None),
                        },
                        _ => return Value::Error("TypeError", "list[int] required", None),
                    }
                }
                out
            }
            _ => return Value::Error("TypeError", "input must be list[int] or str", None),
        };
        let mut h = FnvHasher::default();
        h.write(&b);
        Value::Int(Int::from_i64(h.finish() as i64))
    }, vec![Parameter::positional_pt("input", &input_type)], "int", EffectFlags::PURE);

    insert_native_var!(map, "MD5_OUTPUT_SIZE", Value::Int(Int::from_i64(16)), "int");
    insert_native_var!(map, "SHA256_OUTPUT_SIZE", Value::Int(Int::from_i64(32)), "int");
    insert_native_var!(map, "SHA512_OUTPUT_SIZE", Value::Int(Int::from_i64(64)), "int");
    insert_native_var!(map, "BLAKE2B_OUTPUT_SIZE", Value::Int(Int::from_i64(64)), "int");
    insert_native_var!(map, "BLAKE3_OUTPUT_SIZE", Value::Int(Int::from_i64(32)), "int");
    insert_native_var!(map, "RIPEMD160_OUTPUT_SIZE", Value::Int(Int::from_i64(20)), "int");

    map
}
