use std::collections::HashMap;
use std::sync::Arc;
use crate::env::runtime::functions::{Function, NativeFunction, Parameter};
use crate::env::runtime::types::{Int};
use crate::env::runtime::errors::Error;
use crate::env::runtime::value::Value;
use crate::env::runtime::variables::Variable;

use crate::env::libs::lasm::cpu::CPU;

use crate::{insert_native_fn};

// This module offers cross-platform, assembly-inspired functions for low-level operations.
// It covers bitwise operations, memory manipulation, and other essential system-level tasks.
// Lucia version 2.0.0, module: lasm@1.0.3

fn asm(args: &HashMap<String, Value>) -> Value {
    let asm = match args.get("asm") {
        Some(Value::String(s)) => s,
        _ => return Value::Error("TypeError", "expected a string for 'asm'", None),
    };

    let mut cpu = CPU::new();

    if let Err(e) = cpu.load_program(asm.to_string()) {
        let (err_type, err_msg) = e;
        let referr = Error::new(err_type.as_str(), err_msg.as_str(), "<lasm::cpu::CPU::load_program>");
        return Value::Error("LASMRuntimeError", "failed to load lasm program", Some(referr));
    }

    match cpu.run() {
        Ok(Some(result)) => Value::Int(Int::from_i64(result as i64)),
        Ok(None) => Value::Error("LASMRuntimeError", "program terminated without return", None),
        Err(e) => {
            let (err_type, err_msg) = e;
            let referr = Error::new(err_type.as_str(), err_msg.as_str(), "<lasm::cpu::CPU::run>");
            Value::Error("LASMRuntimeError", "failed to run lasm", Some(referr))
        }
    }
}

pub fn register() -> HashMap<String, Variable> {
    let mut map = HashMap::new();
    
    insert_native_fn!(
        map,
        "asm",
        asm,
        vec![Parameter::positional("asm", "str")],
        "int"
    );
    
    map
}