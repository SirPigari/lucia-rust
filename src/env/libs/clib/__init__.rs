// VERY UNSAFE
// Pointers are experimental and unsafe in Lucia. DO NOT USE THEM UNLESS YOU KNOW WHAT YOU ARE DOING.

use std::collections::HashMap;
use std::ffi::{CString, CStr};
use std::ptr;
use std::sync::Arc;

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
use crate::env::runtime::functions::{Function, NativeFunction};
use crate::{insert_native_fn};

const MAX_ALLOC_SIZE: usize = 1 << 30; // 1 GB

// This module provides native FFI bindings to the C standard library (libc).
// It exposes core libc functionality such as memory allocation, string manipulation,
// and system-level utilities like time and environment access.
// Lucia version 2.0.0, module: clib@0.2.0

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

    insert_native_fn!(map, "printf", printf_fn, vec![Parameter::positional("text", "str")], "int");

    insert_native_fn!(map, "malloc", malloc_fn, vec![Parameter::positional("size", "int")], "&any");
    insert_native_fn!(map, "calloc", calloc_fn, vec![
        Parameter::positional("count", "int"),
        Parameter::positional("size", "int")
    ], "&any");
    insert_native_fn!(map, "realloc", realloc_fn, vec![
        Parameter::positional("ptr", "&any"),
        Parameter::positional("new_size", "int")
    ], "&any");
    insert_native_fn!(map, "free", free_fn, vec![Parameter::positional("ptr", "&any")], "void");

    insert_native_fn!(map, "strlen", strlen_fn, vec![Parameter::positional("ptr", "&int")], "int");
    insert_native_fn!(map, "strcpy", strcpy_fn, vec![
        Parameter::positional("dst", "&int"),
        Parameter::positional("src", "&int")
    ], "&int");

    insert_native_fn!(map, "strcmp", strcmp_fn, vec![
        Parameter::positional("a", "&int"),
        Parameter::positional("b", "&int")
    ], "int");

    insert_native_fn!(map, "memcpy", memcpy_fn, vec![
        Parameter::positional("dst", "&any"),
        Parameter::positional("src", "&any"),
        Parameter::positional("size", "int")
    ], "&any");

    insert_native_fn!(map, "alloc_string", alloc_string_fn, vec![Parameter::positional("text", "str")], "&any");

    insert_native_fn!(map, "time", time_fn, vec![], "int");
    insert_native_fn!(map, "getenv", getenv_fn, vec![Parameter::positional("key", "str")], "str");

    insert_native_fn!(map, "exit", exit_fn, vec![Parameter::positional("code", "int")], "void");

    map
}

fn printf_fn(args: &HashMap<String, Value>) -> Value {
    match args.get("text") {
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
                    Value::Pointer(ptr as usize)
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
                    Value::Pointer(ptr as usize)
                }
            }
        }
        _ => Value::Error("TypeError", "Expected 'count' and 'size' to be integers", None),
    }
}

fn realloc_fn(args: &HashMap<String, Value>) -> Value {
    match (args.get("ptr"), args.get("new_size")) {
        (Some(Value::Pointer(ptr)), Some(Value::Int(new_size))) => {
            let Ok(ns) = new_size.to_i64() else {
                return Value::Error("TypeError", "Expected integer for 'new_size'", None);
            };
            if ns <= 0 || ns as u64 > MAX_ALLOC_SIZE as u64 {
                return Value::Error("ValueError", "Invalid or too large size for realloc", None);
            }
            unsafe {
                let new_ptr = realloc(*ptr as *mut c_void, ns as size_t);
                if new_ptr.is_null() {
                    Value::Error("AllocError", "realloc failed", None)
                } else {
                    Value::Pointer(new_ptr as usize)
                }
            }
        }
        _ => Value::Error("TypeError", "Expected 'ptr' (&any) and 'new_size' (int)", None),
    }
}

fn free_fn(args: &HashMap<String, Value>) -> Value {
    match args.get("ptr") {
        Some(Value::Pointer(p)) => unsafe {
            free(*p as *mut c_void);
            Value::Null
        },
        _ => Value::Error("TypeError", "Expected 'ptr' to be a pointer", None),
    }
}

fn strlen_fn(args: &HashMap<String, Value>) -> Value {
    match args.get("ptr") {
        Some(Value::Pointer(p)) => unsafe {
            let len = strlen(*p as *const c_char);
            Value::Int((len as i64).into())
        },
        _ => Value::Error("TypeError", "Expected 'ptr' to be a pointer", None),
    }
}

fn strcpy_fn(args: &HashMap<String, Value>) -> Value {
    match (args.get("dst"), args.get("src")) {
        (Some(Value::Pointer(dst)), Some(Value::Pointer(src))) => unsafe {
            let result = strcpy(*dst as *mut c_char, *src as *const c_char);
            Value::Pointer(result as usize)
        },
        _ => Value::Error("TypeError", "Expected 'dst' and 'src' to be pointers", None),
    }
}

fn strcmp_fn(args: &HashMap<String, Value>) -> Value {
    match (args.get("a"), args.get("b")) {
        (Some(Value::Pointer(a)), Some(Value::Pointer(b))) => unsafe {
            let res = strcmp(*a as *const c_char, *b as *const c_char);
            Value::Int((res as i64).into())
        },
        _ => Value::Error("TypeError", "Expected 'a' and 'b' to be pointers", None),
    }
}

fn memcpy_fn(args: &HashMap<String, Value>) -> Value {
    match (args.get("dst"), args.get("src"), args.get("size")) {
        (Some(Value::Pointer(dst)), Some(Value::Pointer(src)), Some(Value::Int(size))) => {
            let Ok(sz) = size.to_i64() else {
                return Value::Error("TypeError", "Expected integer for 'size'", None);
            };
            if sz < 0 || sz as u64 > MAX_ALLOC_SIZE as u64 {
                return Value::Error("ValueError", "memcpy size too large or invalid", None);
            }
            unsafe {
                let out = memcpy(*dst as *mut c_void, *src as *const c_void, sz as usize);
                Value::Pointer(out as usize)
            }
        }
        _ => Value::Error("TypeError", "Expected 'dst', 'src' (&any) and 'size' (int)", None),
    }
}

fn alloc_string_fn(args: &HashMap<String, Value>) -> Value {
    match args.get("text") {
        Some(Value::String(s)) => {
            let cstr = match CString::new(s.as_str()) {
                Ok(cstr) => cstr,
                Err(_) => return Value::Error("FFIError", "CString conversion failed", None),
            };
            let len = cstr.as_bytes_with_nul().len();
            if len > MAX_ALLOC_SIZE {
                return Value::Error("ValueError", "String too large to allocate", None);
            }
            unsafe {
                let mem = malloc(len);
                if mem.is_null() {
                    return Value::Error("AllocError", "malloc failed", None);
                }
                ptr::copy_nonoverlapping(cstr.as_ptr(), mem as *mut i8, len);
                Value::Pointer(mem as usize)
            }
        }
        _ => Value::Error("TypeError", "Expected 'text' to be a string", None),
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
            let key_cstr = match CString::new(key.as_str()) {
                Ok(k) => k,
                Err(_) => return Value::Error("FFIError", "CString conversion failed", None),
            };
            unsafe {
                let val = getenv(key_cstr.as_ptr());
                if val.is_null() {
                    Value::Null
                } else {
                    let c_str = CStr::from_ptr(val);
                    match c_str.to_str() {
                        Ok(s) => Value::String(s.to_string()),
                        Err(_) => Value::Error("FFIError", "Invalid UTF-8 from getenv", None),
                    }
                }
            }
        }
        _ => Value::Error("TypeError", "Expected 'key' to be a string", None),
    }
}

fn exit_fn(args: &HashMap<String, Value>) -> Value {
    match args.get("code") {
        Some(Value::Int(code)) => {
            let c = code.to_i64().unwrap_or(0) as i32;
            unsafe { exit(c); }
        }
        _ => {}
    }
    Value::Null
}
