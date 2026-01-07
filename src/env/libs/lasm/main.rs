use std::collections::HashMap;
use crate::env::runtime::functions::{Function, Parameter};
use crate::env::runtime::types::{Int, Type};
use crate::env::runtime::errors::Error;
use crate::env::runtime::value::Value;
use crate::env::runtime::internal_structs::EffectFlags;
use crate::env::runtime::variables::Variable;
use crate::env::runtime::utils::to_static;
use crate::env::runtime::statements::Statement;
use crate::env::runtime::tokens::Location;
use crate::env::runtime::structs_and_enums::Struct;

use once_cell::sync::Lazy;
use lasm::{compile, call, LasmFunction, Target, LasmError};
use std::sync::Arc;
use parking_lot::Mutex;
use sha2::{Sha256, Digest};

use crate::{insert_native_fn, insert_native_fn_pt, insert_native_var};

// This module offers cross-platform, low-level assembly capabilities through the LASM (Lucia Assembly) language.
// It compiles using cranelift jit compiler and allows execution of assembly code within the Lucia environment.
// Lucia version 2.0.0, module: lasm@2.0.0

type LasmId = [u8; 32];
static FUNCTION_IDS: Lazy<Arc<Mutex<HashMap<LasmId, LasmFunction>>>> = Lazy::new(|| Arc::new(Mutex::new(HashMap::new())));

static LASM_FUNCTION_STRUCT_TYPE: Lazy<Type> = Lazy::new(|| Type::Struct {
    name: "LasmFunction".to_string(),
    fields: vec![
        (
            "id".to_string(),
            Statement::make_value(Value::Type(Type::new_simple("bytes"))),
            vec!["private".to_owned()]
        ),
        (
            "freed".to_string(),
            Statement::make_value(Value::Type(Type::new_simple("bool"))),
            vec!["private".to_owned()]
        )
    ],
    methods: vec![],
    generics: Vec::new(),
    wheres: Vec::new(),
});


fn handle_lasm_error(err: LasmError) -> Value {
    match err {
        LasmError::Compile { message, location } => Value::new_error("LasmError", "Error while compiling LASM program", Some(Error::with_some_location("CompileError", &message.as_str(), location.map(|loc| Location::from_lasm_loc(&loc, "<lasm>"))))),
        LasmError::Parse { message, location } => Value::new_error("LasmParseError", "Error while parsing LASM program", Some(Error::with_some_location("ParseError", &message.as_str(), Some(Location::from_lasm_loc(&location, "<lasm>"))))),
        LasmError::Runtime(msg) => Value::new_error("LasmRuntimeError", to_static(msg), None),
        e => Value::new_error("LasmErrorError", to_static(format!("{:?}", e)), None),
    }
}

fn expect_lasm_ptr(value: &Value) -> Result<Arc<Mutex<(Value, usize)>>, Value> {
    match value {
        Value::Pointer(p) => Ok(p.clone()),
        _ => Err(Value::new_error(
            "TypeError",
            "Expected a pointer to LasmFunction",
            None,
        )),
    }
}

fn le_compile(args: &HashMap<String, Value>) -> Value {
    let asm = match args.get("asm") {
        Some(Value::String(s)) => s,
        _ => return Value::new_error("TypeError", "Expected 'asm' to be a string", None),
    };

    let function = match compile(asm, Target::Native) {
        Ok(func) => func,
        Err(e) => return handle_lasm_error(e),
    };

    let mut hasher = Sha256::new();
    hasher.update(asm.as_bytes());
    let hash: LasmId = hasher.finalize().into();

    let mut ids = FUNCTION_IDS.lock();
    ids.entry(hash).or_insert(function);
    drop(ids);

    let mut fields = HashMap::new();
    fields.insert(
        "id".to_string(),
        (Box::new(Value::Bytes(hash.to_vec())), Type::new_simple("bytes")),
    );
    fields.insert(
        "freed".to_string(),
        (Box::new(Value::Boolean(false)), Type::new_simple("bool")),
    );

    let s = Value::Struct(Box::new(Struct::new_with_fields(
        LASM_FUNCTION_STRUCT_TYPE.clone(),
        fields,
    )));

    Value::Pointer(Arc::new(Mutex::new((s, 1))))
}


fn le_call(args: &HashMap<String, Value>) -> Value {
    let ptr = match args.get("function") {
        Some(v) => match expect_lasm_ptr(v) {
            Ok(p) => p,
            Err(e) => return e,
        },
        None => return Value::new_error("TypeError", "Missing 'function'", None),
    };

    let guard = ptr.lock();
    let func_struct = match &guard.0 {
        Value::Struct(s) => s,
        _ => return Value::new_error("TypeError", "Pointer does not reference a LasmFunction", None),
    };

    if let Value::Boolean(true) = &*func_struct.get_field("freed").unwrap() {
        return Value::new_error("UseAfterFreeError", "LasmFunction was freed", None);
    }

    let id_value = func_struct.get_field("id").unwrap();
    let id: LasmId = match &*id_value {
        Value::Bytes(b) if b.len() == 32 => {
            let mut arr = [0u8; 32];
            arr.copy_from_slice(b);
            arr
        }
        _ => return Value::new_error("TypeError", "Invalid LasmFunction id", None),
    };

    let ids = FUNCTION_IDS.lock();
    let function = match ids.get(&id) {
        Some(f) => f,
        None => return Value::new_error("LasmFunctionNotFound", "Function not found", None),
    };

    let variables_value = match args.get("vars") {
        Some(Value::Map(m)) => m,
        _ => return Value::new_error("TypeError", "Expected 'vars' to be a map", None),
    };

    let mut vars = HashMap::new();
    for (k, v) in variables_value.iter() {
        let ks = match k {
            Value::String(s) => s.clone(),
            _ => return Value::new_error("TypeError", "Map keys must be strings", None),
        };

        let lv = match v {
            Value::Int(i) => lasm::Value::Int(i.to_i64().unwrap()),
            Value::String(s) => lasm::Value::String(s.clone()),
            Value::Boolean(b) => lasm::Value::Int(if *b { 1 } else { 0 }),
            Value::Float(f) => lasm::Value::Float(f.to_f64().unwrap()),
            Value::Null => lasm::Value::Null,
            _ => return Value::new_error("TypeError", "Unsupported LASM variable type", None),
        };

        vars.insert(ks, lv);
    }

    match call(function, &vars) {
        Ok((exit, map)) => Value::Tuple(vec![
            Value::Int(Int::from(exit as i64)),
            Value::Map(map.into_iter().map(|(k, v)| (Value::String(k), v.into())).collect()),
        ]),
        Err(e) => handle_lasm_error(e),
    }
}


fn le_free(args: &HashMap<String, Value>) -> Value {
    let ptr = match args.get("function") {
        Some(v) => match expect_lasm_ptr(v) {
            Ok(p) => p,
            Err(e) => return e,
        },
        None => return Value::new_error("TypeError", "Missing 'function'", None),
    };

    let mut guard = ptr.lock();
    let func_struct = match &mut guard.0 {
        Value::Struct(s) => s,
        _ => return Value::new_error("TypeError", "Pointer does not reference a LasmFunction", None),
    };

    let id_value = func_struct.get_field("id").unwrap();
    let id: LasmId = match &*id_value {
        Value::Bytes(b) if b.len() == 32 => {
            let mut arr = [0u8; 32];
            arr.copy_from_slice(b);
            arr
        }
        _ => return Value::new_error("TypeError", "Invalid LasmFunction id", None),
    };

    let mut ids = FUNCTION_IDS.lock();
    let existed = ids.remove(&id).is_some();

    func_struct.set_field("freed".to_owned(), Value::Boolean(true));

    Value::Boolean(existed)
}


pub fn register() -> HashMap<String, Variable> {
    let mut map = HashMap::new();

    let lasm_fst_ptr = Type::Reference {
        base_type: Box::new(LASM_FUNCTION_STRUCT_TYPE.clone()),
        ref_level: 1,
    };
    
    insert_native_fn_pt!(
        map,
        "compile",
        le_compile,
        vec![Parameter::positional("asm", "str")],
        &lasm_fst_ptr,
        EffectFlags::PURE
    );
    insert_native_fn!(
        map,
        "call",
        le_call,
        vec![
            Parameter::positional_pt("function", &lasm_fst_ptr),
            Parameter::positional("vars", "map")
        ],
        "any",
        EffectFlags::UNSAFE | EffectFlags::UNKNOWN
    );
    insert_native_fn!(
        map,
        "free",
        le_free,
        vec![
            Parameter::positional_pt("function", &lasm_fst_ptr)
        ],
        "bool",
        EffectFlags::PURE
    );
    insert_native_var!(
        map,
        "LasmFunction",
        Value::Type(LASM_FUNCTION_STRUCT_TYPE.clone()),
        "type"
    );
    
    map
}