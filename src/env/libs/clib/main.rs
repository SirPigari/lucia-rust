use std::collections::HashMap;
use std::ffi::{CString, CStr};
use std::ptr;
use std::sync::{Arc, Mutex};

use libc::{c_char, c_void, size_t};

unsafe extern "C" {
    pub fn malloc(size: size_t) -> *mut c_void;
    pub fn free(ptr: *mut c_void);
    pub fn calloc(nmemb: size_t, size: size_t) -> *mut c_void;
    pub fn realloc(ptr: *mut c_void, size: size_t) -> *mut c_void;

    pub fn strlen(s: *const c_char) -> size_t;
    pub fn strcpy(dest: *mut c_char, src: *const c_char) -> *mut c_char;
    pub fn strcmp(s1: *const c_char, s2: *const c_char) -> i32;
    pub fn memcpy(dest: *mut c_void, src: *const c_void, n: size_t) -> *mut c_void;

    pub fn printf(format: *const c_char, ...) -> i32;
    pub fn time(tloc: *mut i64) -> i64;
    pub fn getenv(name: *const c_char) -> *mut c_char;
    pub fn exit(status: i32) -> !;
}

use crate::env::runtime::functions::Parameter;
use crate::env::runtime::value::Value;
use crate::env::runtime::variables::Variable;
use crate::env::runtime::config::{Config, get_from_config};
use crate::env::runtime::errors::Error;
use crate::env::runtime::utils::to_static;
use crate::env::runtime::functions::Function;
use crate::env::runtime::utils::parse_type;
use crate::env::runtime::internal_structs::EffectFlags;
use crate::{insert_native_fn, insert_native_fn_pt};

const MAX_ALLOC_SIZE: usize = 1 << 30; // 1 GB

pub fn init_clib(config: Arc<Config>, file_path: String) -> Result<(), Error> {
    if !get_from_config(&config, "allow_unsafe").is_truthy() {
        return Err(Error::with_help(
            "UnsafeError",
            "Clib operations are unsafe and cannot be performed without explicit permission.",
            "To enable unsafe operations, set 'allow_unsafe' to true in the configuration.",
            to_static(file_path),
        ));
    }
    Ok(())
}

pub fn register() -> HashMap<String, Variable> {
    let mut map = HashMap::new();

    let any_ptr_type = parse_type("&any");

    insert_native_fn!(map, "printf", printf_fn, vec![Parameter::positional("format", "str"), Parameter::variadic("args", "any")], "int", EffectFlags::IO | EffectFlags::UNSAFE);

    insert_native_fn_pt!(map, "malloc", malloc_fn, vec![Parameter::positional("size", "int")], &any_ptr_type, EffectFlags::UNSAFE);
    insert_native_fn_pt!(map, "calloc", calloc_fn, vec![
        Parameter::positional("count", "int"),
        Parameter::positional("size", "int")
    ], &any_ptr_type, EffectFlags::UNSAFE);
    insert_native_fn_pt!(map, "realloc", realloc_fn, vec![
        Parameter::positional_pt("ptr", &any_ptr_type),
        Parameter::positional("new_size", "int")
    ], &any_ptr_type, EffectFlags::UNSAFE);
    insert_native_fn!(map, "free", free_fn, vec![Parameter::positional("ptr", "&any")], "void", EffectFlags::UNSAFE);

    insert_native_fn!(map, "strlen", strlen_fn, vec![Parameter::positional_pt("ptr", &parse_type("&int"))], "int", EffectFlags::UNSAFE);
    insert_native_fn_pt!(map, "strcpy", strcpy_fn, vec![
        Parameter::positional_pt("dst", &any_ptr_type),
        Parameter::positional_pt("src", &any_ptr_type)
    ], &any_ptr_type, EffectFlags::UNSAFE);

    insert_native_fn!(map, "strcmp", strcmp_fn, vec![
        Parameter::positional_pt("a", &any_ptr_type),
        Parameter::positional_pt("b", &any_ptr_type)
    ], "int", EffectFlags::UNSAFE);

    insert_native_fn_pt!(map, "memcpy", memcpy_fn, vec![
        Parameter::positional_pt("dst", &any_ptr_type),
        Parameter::positional_pt("src", &any_ptr_type),
        Parameter::positional("size", "int")
    ], &any_ptr_type, EffectFlags::UNSAFE);

    insert_native_fn_pt!(map, "alloc_string", alloc_string_fn, vec![Parameter::positional("text", "str")], &any_ptr_type, EffectFlags::UNSAFE);

    insert_native_fn!(map, "time", time_fn, vec![], "int", EffectFlags::IO);
    insert_native_fn!(map, "getenv", getenv_fn, vec![Parameter::positional("key", "str")], "str", EffectFlags::IO);

    insert_native_fn!(map, "exit", exit_fn, vec![Parameter::positional("code", "int")], "void", EffectFlags::IO);

    map
}

fn ptr_from_value_pointer(v: &Value) -> Result<*mut c_void, Value> {
    match v {
        Value::Pointer(arc_val) => match &*arc_val.lock().unwrap() {
            (Value::Int(i), _) => Ok(i.to_i64().unwrap() as usize as *mut c_void),
            _ => Err(Value::Error("TypeError", "Expected inner pointer as Int", None)),
        },
        _ => Err(Value::Error("TypeError", "Expected pointer type", None)),
    }
}

fn value_pointer_from_raw(ptr: *mut c_void) -> Value {
    Value::Pointer(Arc::new(Mutex::new((Value::Int((ptr as usize as i64).into()), 1))))
}

fn printf_fn(args: &HashMap<String, Value>) -> Value {
    if args.get("args").is_some() {
        return Value::Error("NotImplementedError", "Variadic arguments are not supported in this FFI binding", None);
    }

    match args.get("format") {
        Some(Value::String(s)) => match CString::new(s.as_str()) {
            Ok(cstr) => unsafe {
                let written = printf(cstr.as_ptr());
                Value::Int((written as i64).into())
            },
            Err(_) => Value::Error("FFIError", "CString conversion failed", None),
        },
        _ => Value::Error("TypeError", "Expected 'text' to be a string", None),
    }
}

fn malloc_fn(args: &HashMap<String, Value>) -> Value {
    match args.get("size") {
        Some(Value::Int(n)) => {
            let Ok(v) = n.to_i64() else {
                return Value::Error("TypeError", "Expected integer for 'size'", None);
            };
            if v <= 0 || v as u64 > MAX_ALLOC_SIZE as u64 {
                return Value::Error("ValueError", "Requested malloc size is invalid or too large", None);
            }
            let size = v as usize;
            unsafe {
                let ptr = malloc(size);
                if ptr.is_null() {
                    Value::Error("AllocError", "malloc failed", None)
                } else {
                    value_pointer_from_raw(ptr)
                }
            }
        }
        _ => Value::Error("TypeError", "Expected 'size' to be an integer", None),
    }
}

fn calloc_fn(args: &HashMap<String, Value>) -> Value {
    match (args.get("count"), args.get("size")) {
        (Some(Value::Int(count)), Some(Value::Int(size))) => {
            let Ok(c) = count.to_i64() else {
                return Value::Error("TypeError", "Expected integer for 'count'", None);
            };
            let Ok(s) = size.to_i64() else {
                return Value::Error("TypeError", "Expected integer for 'size'", None);
            };
            if c <= 0 || s <= 0 || (c as u64).saturating_mul(s as u64) > MAX_ALLOC_SIZE as u64 {
                return Value::Error("ValueError", "Requested calloc size is too large or invalid", None);
            }
            unsafe {
                let ptr = calloc(c as size_t, s as size_t);
                if ptr.is_null() {
                    Value::Error("AllocError", "calloc failed", None)
                } else {
                    value_pointer_from_raw(ptr)
                }
            }
        }
        _ => Value::Error("TypeError", "Expected 'count' and 'size' to be integers", None),
    }
}

fn realloc_fn(args: &HashMap<String, Value>) -> Value {
    match (args.get("ptr"), args.get("new_size")) {
        (Some(ptr_val), Some(Value::Int(new_size))) => {
            let Ok(ns) = new_size.to_i64() else {
                return Value::Error("TypeError", "Expected integer for 'new_size'", None);
            };
            if ns <= 0 || ns as u64 > MAX_ALLOC_SIZE as u64 {
                return Value::Error("ValueError", "Invalid or too large size for realloc", None);
            }
            let raw_ptr = match ptr_from_value_pointer(ptr_val) {
                Ok(p) => p,
                Err(e) => return e,
            };
            unsafe {
                let new_ptr = realloc(raw_ptr, ns as size_t);
                if new_ptr.is_null() {
                    Value::Error("AllocError", "realloc failed", None)
                } else {
                    value_pointer_from_raw(new_ptr)
                }
            }
        }
        _ => Value::Error("TypeError", "Expected 'ptr' (&any) and 'new_size' (int)", None),
    }
}

fn free_fn(args: &HashMap<String, Value>) -> Value {
    match args.get("ptr") {
        Some(ptr_val) => match ptr_from_value_pointer(ptr_val) {
            Ok(raw_ptr) => unsafe {
                free(raw_ptr);
                Value::Null
            },
            Err(e) => e,
        },
        _ => Value::Error("TypeError", "Expected 'ptr' to be a pointer", None),
    }
}

fn strlen_fn(args: &HashMap<String, Value>) -> Value {
    match args.get("ptr") {
        Some(ptr_val) => match ptr_from_value_pointer(ptr_val) {
            Ok(raw_ptr) => unsafe {
                let len = strlen(raw_ptr as *const c_char);
                Value::Int((len as i64).into())
            },
            Err(e) => e,
        },
        _ => Value::Error("TypeError", "Expected 'ptr' to be a pointer", None),
    }
}

fn strcpy_fn(args: &HashMap<String, Value>) -> Value {
    match (args.get("dst"), args.get("src")) {
        (Some(dst_val), Some(src_val)) => {
            let dst_ptr = match ptr_from_value_pointer(dst_val) {
                Ok(p) => p as *mut c_char,
                Err(e) => return e,
            };
            let src_ptr = match ptr_from_value_pointer(src_val) {
                Ok(p) => p as *const c_char,
                Err(e) => return e,
            };
            unsafe {
                let result = strcpy(dst_ptr, src_ptr);
                value_pointer_from_raw(result as *mut c_void)
            }
        }
        _ => Value::Error("TypeError", "Expected 'dst' and 'src' to be pointers", None),
    }
}

fn strcmp_fn(args: &HashMap<String, Value>) -> Value {
    match (args.get("a"), args.get("b")) {
        (Some(a_val), Some(b_val)) => {
            let a_ptr = match ptr_from_value_pointer(a_val) {
                Ok(p) => p as *const c_char,
                Err(e) => return e,
            };
            let b_ptr = match ptr_from_value_pointer(b_val) {
                Ok(p) => p as *const c_char,
                Err(e) => return e,
            };
            unsafe {
                let res = strcmp(a_ptr, b_ptr);
                Value::Int((res as i64).into())
            }
        }
        _ => Value::Error("TypeError", "Expected 'a' and 'b' to be pointers", None),
    }
}

fn memcpy_fn(args: &HashMap<String, Value>) -> Value {
    match (args.get("dst"), args.get("src"), args.get("size")) {
        (Some(dst_val), Some(src_val), Some(Value::Int(size))) => {
            let Ok(sz) = size.to_i64() else {
                return Value::Error("TypeError", "Expected integer for 'size'", None);
            };
            if sz < 0 || sz as u64 > MAX_ALLOC_SIZE as u64 {
                return Value::Error("ValueError", "memcpy size too large or invalid", None);
            }
            let dst_ptr = match ptr_from_value_pointer(dst_val) {
                Ok(p) => p,
                Err(e) => return e,
            };
            let src_ptr = match ptr_from_value_pointer(src_val) {
                Ok(p) => p,
                Err(e) => return e,
            };
            unsafe {
                let res = memcpy(dst_ptr, src_ptr, sz as size_t);
                value_pointer_from_raw(res)
            }
        }
        _ => Value::Error("TypeError", "Expected 'dst', 'src' as pointers and 'size' as int", None),
    }
}

fn alloc_string_fn(args: &HashMap<String, Value>) -> Value {
    match args.get("text") {
        Some(Value::String(s)) => {
            match CString::new(s.as_str()) {
                Ok(cstring) => {
                    let bytes = cstring.to_bytes_with_nul();
                    unsafe {
                        let ptr = malloc(bytes.len());
                        if ptr.is_null() {
                            Value::Error("AllocError", "malloc failed in alloc_string", None)
                        } else {
                            ptr::copy_nonoverlapping(bytes.as_ptr(), ptr as *mut u8, bytes.len());
                            value_pointer_from_raw(ptr)
                        }
                    }
                }
                Err(_) => Value::Error("FFIError", "CString conversion failed", None),
            }
        }
        _ => Value::Error("TypeError", "Expected 'text' to be string", None),
    }
}

fn time_fn(_: &HashMap<String, Value>) -> Value {
    unsafe {
        let t = time(ptr::null_mut());
        Value::Int((t as i64).into())
    }
}

fn getenv_fn(args: &HashMap<String, Value>) -> Value {
    match args.get("key") {
        Some(Value::String(key)) => {
            match CString::new(key.as_str()) {
                Ok(cstr) => unsafe {
                    let ptr = getenv(cstr.as_ptr());
                    if ptr.is_null() {
                        Value::Null
                    } else {
                        let c_str = CStr::from_ptr(ptr);
                        match c_str.to_str() {
                            Ok(s) => Value::String(s.to_string()),
                            Err(_) => Value::Error("FFIError", "Failed to convert getenv result", None),
                        }
                    }
                },
                Err(_) => Value::Error("FFIError", "CString conversion failed", None),
            }
        }
        _ => Value::Error("TypeError", "Expected 'key' to be string", None),
    }
}

fn exit_fn(args: &HashMap<String, Value>) -> Value {
    match args.get("code") {
        Some(Value::Int(code)) => {
            let c = code.to_i64().unwrap_or(0) as i32;
            unsafe {
                exit(c);
            }
        }
        _ => {
            unsafe { exit(0); }
        }
    }
}
