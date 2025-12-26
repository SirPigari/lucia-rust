use std::collections::HashMap;
use std::hash::Hasher;
use crate::env::runtime::variables::Variable;
use crate::env::runtime::functions::{Function, Parameter};
use crate::env::runtime::internal_structs::EffectFlags;
use crate::env::runtime::utils::{parse_type, to_hex};
use crate::env::runtime::value::Value;
use crate::env::runtime::types::Int;
use crate::{insert_native_fn, insert_native_var, insert_native_fn_pt, insert_native_shared_fn_pt};
use crate::interpreter::Interpreter;

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
    let output_type = parse_type("list[int] | str");

    macro_rules! hash_fn {
        ($name:literal, $algo:expr) => {
            insert_native_fn_pt!(map, $name, |args: &HashMap<String, Value>| {
                let mut is_list_input = false;
                let b = match args.get("input") {
                    Some(Value::String(s)) => s.as_bytes().to_vec(),
                    Some(Value::List(l)) => {
                        is_list_input = true;
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
                if is_list_input {
                    let mut out = Vec::with_capacity(hash.len());
                    for byte in hash {
                        out.push(Value::Int(Int::from_i64(byte as i64)));
                    }
                    Value::List(out)
                } else {
                    Value::String(to_hex(&hash))
                }
            }, vec![Parameter::positional_pt("input", &input_type)], &output_type, EffectFlags::PURE);
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

    insert_native_fn_pt!(map, "argon2", |args: &HashMap<String, Value>| {
        let mut is_list_input = false;
        let password = match args.get("password") {
            Some(Value::String(s)) => s.as_bytes().to_vec(),
            Some(Value::List(l)) => {
                is_list_input = true;
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
            Ok(h) => if is_list_input {
                let hash_str = h.to_string();
                let mut out = Vec::with_capacity(hash_str.len());
                for byte in hash_str.as_bytes() {
                    out.push(Value::Int(Int::from_i64(*byte as i64)));
                }
                Value::List(out)
            } else {
                Value::String(h.to_string())
            },
            Err(_) => Value::Error("HashError", "argon2 failed", None),
        }
    }, vec![
        Parameter::positional_pt("password", &input_type),
        Parameter::positional("salt", "str"),
        Parameter::positional_optional("memory_kib", "int", Value::Int(Int::from_i64(19456))),
        Parameter::positional_optional("iterations", "int", Value::Int(Int::from_i64(2))),
        Parameter::positional_optional("parallelism", "int", Value::Int(Int::from_i64(1))),
    ], &output_type, EffectFlags::PURE);

    insert_native_fn_pt!(map, "bcrypt", |args: &HashMap<String, Value>| {
        let mut is_list_input = false;
        let password = match args.get("password") {
            Some(Value::String(s)) => s.as_bytes().to_vec(),
            Some(Value::List(l)) => {
                is_list_input = true;
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
            Ok(h) => if is_list_input {
                let hash_str = h;
                let mut out = Vec::with_capacity(hash_str.len());
                for byte in hash_str.as_bytes() {
                    out.push(Value::Int(Int::from_i64(*byte as i64)));
                }
                Value::List(out)
            } else {
                Value::String(h)
            },
            Err(_) => Value::Error("HashError", "bcrypt failed", None),
        }
    }, vec![
        Parameter::positional_pt("password", &input_type),
        Parameter::positional_optional("cost", "int", Value::Int(Int::from_i64(12))),
    ], &output_type, EffectFlags::PURE);

    insert_native_fn_pt!(map, "pbkdf2", |args: &HashMap<String, Value>| {
        let mut is_list_input = false;
        let password = match args.get("password") {
            Some(Value::String(s)) => s.as_bytes().to_vec(),
            Some(Value::List(l)) => {
                is_list_input = true;
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
        if is_list_input {
            let mut list_out = Vec::with_capacity(out.len());
            for byte in out {
                list_out.push(Value::Int(Int::from_i64(byte as i64)));
            }
            Value::List(list_out)
        } else {
            Value::String(to_hex(&out))
        }
    }, vec![
        Parameter::positional_pt("password", &input_type),
        Parameter::positional_pt("salt", &input_type),
        Parameter::positional_optional("iterations", "int", Value::Int(Int::from_i64(100_000))),
        Parameter::positional_optional("output_len", "int", Value::Int(Int::from_i64(32))),
    ], &output_type, EffectFlags::PURE);

    insert_native_fn_pt!(map, "scrypt", |args: &HashMap<String, Value>| {
        let mut is_list_input = false;
        let password = match args.get("password") {
            Some(Value::String(s)) => s.as_bytes().to_vec(),
            Some(Value::List(l)) => {
                is_list_input = true;
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
                if is_list_input {
                    let mut list_out = Vec::with_capacity(out.len());
                    for byte in out {
                        list_out.push(Value::Int(Int::from_i64(byte as i64)));
                    }
                    Value::List(list_out)
                } else {
                    Value::String(to_hex(&out))
                }
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
    ], &output_type, EffectFlags::PURE);

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
    let algorythm_type = parse_type("function[list[int] | str] -> list[int] | str");
    insert_native_shared_fn_pt!(map, "hmac", |args: &HashMap<String, Value>, interpreter: &mut Interpreter| {
        let mut is_list_input = false;
        let key = match args.get("key") {
            Some(Value::String(s)) => s.as_bytes().to_vec(),
            Some(Value::List(l)) => {
                is_list_input = true;
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
            _ => return Value::Error("TypeError", "key must be list[int] or str", None),
        };
        let message = match args.get("message") {
            Some(Value::String(s)) => s.as_bytes().to_vec(),
            Some(Value::List(l)) => {
                is_list_input = true;
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
            _ => return Value::Error("TypeError", "message must be list[int] or str", None),
        };
        let algorithm_fn = match args.get("algorithm") {
            Some(Value::Function(f)) => f,
            _ => return Value::Error("TypeError", "algorithm must be a function", None),
        };

        let mut algorithm = |input: &Value| {
            let result = interpreter.call_function(&algorithm_fn, vec![input.clone()], HashMap::new(), None);
            if interpreter.err.is_some() {
                Value::Error("RuntimeError", "error while calling algorithm function", interpreter.err.take())
            } else {
                result
            }
        };

        let block_size = match args.get("block_size") {
            Some(Value::Int(i)) => i.to_usize().unwrap_or(64),
            Some(Value::Null) => match algorithm_fn.get_name() {
                "md5" => 64,
                "sha1" => 64,
                "sha224" => 64,
                "sha256" => 64,
                "sha384" => 128,
                "sha512" => 128,
                "sha3_224" => 144,
                "sha3_256" => 136,
                "sha3_384" => 104,
                "sha3_512" => 72,
                "blake2b" => 128,
                "blake2s" => 64,
                "blake3" => 64,
                "ripemd160" => 64,
                _ => 64,
            },
            _ => 64,
        };

        // pad or hash key to block_size
        let mut key_block = vec![0u8; block_size];
        if key.len() > block_size {
            let hashed = algorithm(&Value::List(
                key.iter().map(|b| Value::Int(Int::from_i64(*b as i64))).collect()
            ));
            if let Value::List(h) = hashed {
                for i in 0..h.len().min(block_size) {
                    key_block[i] = if let Value::Int(i) = &h[i] { i.to_u8().unwrap_or(0) } else { 0 };
                }
            } else if let Value::String(s) = hashed {
                let bytes = s.as_bytes();
                for i in 0..bytes.len().min(block_size) {
                    key_block[i] = bytes[i];
                }
            } else {
                return Value::Error("TypeError", "algorithm returned invalid type", None);
            }
        } else {
            for i in 0..key.len() {
                key_block[i] = key[i];
            }
        }

        // prepare ipad and opad
        let mut ipad = vec![0x36; block_size];
        let mut opad = vec![0x5c; block_size];
        for i in 0..block_size {
            ipad[i] ^= key_block[i];
            opad[i] ^= key_block[i];
        }

        // inner hash
        let inner_input = Value::List(
            ipad.iter().map(|b| Value::Int(Int::from_i64(*b as i64)))
                .chain(message.iter().map(|b| Value::Int(Int::from_i64(*b as i64))))
                .collect()
        );
        let inner_hash = algorithm(&inner_input);

        // outer hash input
        let outer_input = match inner_hash {
            Value::List(l) => {
                let mut v = opad.clone();
                for b in l {
                    v.push(if let Value::Int(i) = b { i.to_u8().unwrap_or(0) } else { 0 });
                }
                Value::List(v.iter().map(|b| Value::Int(Int::from_i64(*b as i64))).collect())
            },
            Value::String(s) => {
                let mut v = opad.clone();
                v.extend(s.as_bytes());
                Value::List(v.iter().map(|b| Value::Int(Int::from_i64(*b as i64))).collect())
            },
            _ => return Value::Error("TypeError", "algorithm returned invalid type", None),
        };

        let hmac_result = match algorithm(&outer_input) {
            Value::List(l) => l.iter().map(|b| if let Value::Int(i) = b { i.to_u8().unwrap_or(0) } else { 0 }).collect::<Vec<u8>>(),
            Value::String(s) => s.as_bytes().to_vec(),
            _ => return Value::Error("TypeError", "algorithm returned invalid type", None),
        };


        if is_list_input {
            let mut out = Vec::with_capacity(hmac_result.len());
            for byte in hmac_result {
                out.push(Value::Int(Int::from_i64(byte as i64)));
            }
            Value::List(out)
        } else {
            Value::String(to_hex(&hmac_result))
        }
    }, vec![
        Parameter::positional_pt("key", &input_type),
        Parameter::positional_pt("message", &input_type),
        Parameter::positional_pt("algorithm", &algorythm_type),
    ], &output_type, EffectFlags::PURE);

    insert_native_var!(map, "MD5_OUTPUT_SIZE", Value::Int(Int::from_i64(16)), "int");
    insert_native_var!(map, "SHA256_OUTPUT_SIZE", Value::Int(Int::from_i64(32)), "int");
    insert_native_var!(map, "SHA512_OUTPUT_SIZE", Value::Int(Int::from_i64(64)), "int");
    insert_native_var!(map, "BLAKE2B_OUTPUT_SIZE", Value::Int(Int::from_i64(64)), "int");
    insert_native_var!(map, "BLAKE3_OUTPUT_SIZE", Value::Int(Int::from_i64(32)), "int");
    insert_native_var!(map, "RIPEMD160_OUTPUT_SIZE", Value::Int(Int::from_i64(20)), "int");

    map
}
