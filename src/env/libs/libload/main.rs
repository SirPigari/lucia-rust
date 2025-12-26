use std::collections::HashMap;
use std::sync::Arc;
use std::path::Path;
use std::ffi::{c_void, CStr, CString};
use parking_lot::Mutex;
use once_cell::sync::OnceCell;
use crate::env::runtime::utils::escape_string;
use rustc_hash::FxHashMap;

#[cfg(unix)]
use libc::{dlsym, RTLD_DEFAULT};

#[cfg(windows)]
#[link(name = "kernel32")]
unsafe extern "system" {
    fn GetModuleHandleA(lpModuleName: *const i8) -> *mut u8;
    fn GetProcAddress(hModule: *mut u8, lpProcName: *const i8) -> *mut u8;
}

use crate::env::runtime::functions::Parameter;
use crate::env::runtime::utils::to_static;
use crate::env::runtime::value::Value;
use crate::env::runtime::variables::Variable;
use crate::env::runtime::config::{Config, get_from_config};
use crate::env::runtime::errors::Error;
use crate::env::runtime::functions::Function;
use crate::env::runtime::types::{Type, Int, Float};
use crate::env::runtime::statements::Statement;
use crate::env::runtime::internal_structs::EffectFlags;
use crate::interpreter::Interpreter;

use crate::env::libs::libload::ffi::{LuciaLib, LuciaFfiFn, ValueType, get_list};

use crate::{insert_native_fn, insert_native_shared_fn};
#[cfg(unix)]
use crate::insert_native_var;

static STRINGS: OnceCell<Mutex<Vec<CString>>> = OnceCell::new();

fn libload_error(msg: &str) -> Value {
    Value::Error("LibloadError", to_static(msg.to_owned()), None)
}

pub fn create_str_ptr(args: &HashMap<String, Value>) -> Value {
    let s = match args.get("string") {
        Some(Value::String(s)) => s,
        _ => return libload_error("Expected string argument"),
    };

    let strings = STRINGS.get_or_init(|| Mutex::new(Vec::new()));
    let mut locked = strings.lock();

    for cstr in locked.iter() {
        if let Ok(existing_str) = cstr.to_str() {
            if existing_str == s {
                let ptr = cstr.as_ptr() as usize as i64;
                return Value::Pointer(Arc::new(Mutex::new((Value::Int(ptr.into()), 1))));
            }
        }
    }

    let cstring = match CString::new(s.as_str()) {
        Ok(c) => c,
        Err(_) => return libload_error("Failed to create CString"),
    };
    let ptr = cstring.as_ptr();

    locked.push(cstring);

    Value::Pointer(Arc::new(Mutex::new((Value::Int((ptr as usize as i64).into()), 1))))
}

fn manual_escape_string(s: &str) -> String {
    let mut escaped = String::new();
    for c in s.chars() {
        match c {
            '\0' => escaped.push_str("\\0"),
            '\n' => escaped.push_str("\\n"),
            '\r' => escaped.push_str("\\r"),
            '\t' => escaped.push_str("\\t"),
            '\\' => escaped.push_str("\\\\"),
            '"'  => escaped.push_str("\\\""),
            '\'' => escaped.push_str("\\\'"),
            c if (c as u32) < 0x20 => escaped.push_str(&format!("\\u{:04X}", c as u32)),
            _ => escaped.push(c),
        }
    }
    escaped
}

pub fn parse_str_ptr(args: &HashMap<String, Value>) -> Value {
    let ptr_val = args.get("ptr");
    let ptr = match ptr_val {
        Some(Value::Pointer(arc)) => {
            if let (Value::Int(i), _) = &*arc.lock() {
                i.to_i64().unwrap_or(0) as *const i8
            } else {
                return Value::Error("TypeError", "Pointer must wrap Int", None);
            }
        }
        _ => return Value::Error("TypeError", "Expected pointer argument", None),
    };

    if ptr.is_null() {
        return Value::Error("ValueError", "Null pointer provided", None);
    }

    unsafe {
        let cstr = CStr::from_ptr(ptr);
        let mut bytes: Vec<u8> = cstr.to_bytes().to_vec();

        bytes.retain(|&b| ![b'\r'].contains(&b));

        if bytes.is_empty() {
            return Value::String(String::new());
        }

        let s = String::from_utf8_lossy(&bytes);

        let escaped = match escape_string(&s) {
            Ok(e) => e,
            Err(_) => manual_escape_string(&s),
        };

        Value::String(escaped)
    }
}

fn get_list_native(args: &HashMap<String, Value>) -> Value {
    let ptr_val = args.get("ptr");
    let type_val = args.get("type");
    let len_val = args.get("len");
    let ptr = match ptr_val {
        Some(Value::Pointer(arc)) => {
            if let (Value::Int(i), _) = &*arc.lock() {
                let raw = i.to_i64().unwrap_or(0) as usize;
                raw as *const std::ffi::c_void
            } else {
                return Value::Error("TypeError", "Pointer must wrap Int", None);
            }
        }
        _ => return Value::Error("TypeError", "Expected pointer argument", None),
    };
    let len = match len_val {
        Some(Value::Int(i)) => i.to_i64().unwrap_or(0) as usize,
        _ => return Value::Error("TypeError", "Expected length int argument", None),
    };
    match type_val {
        Some(Value::String(s)) => {
            let elem_type = match s.as_str() {
                "int" => ValueType::Int,
                "float64" => ValueType::Float64,
                "float" | "float32" => ValueType::Float32,
                "bool" => ValueType::Boolean,
                "byte" => ValueType::Byte,
                "ptr" => ValueType::Ptr,
                "void" => ValueType::Void,
                _ => return Value::Error("TypeError", "Unknown type string", None),
            };
            unsafe { get_list(ptr, elem_type, len) }
        }
        Some(Value::Type(t)) => {
            match t {
                Type::Struct { name: _, fields, .. } => {
                    let mut result = Vec::new();
                    use crate::env::runtime::utils::get_type_from_statement;
                    let struct_size: usize = fields.iter().map(|(_, field_type, _)| {
                        let type_name = get_type_from_statement(field_type).unwrap_or_else(|| "any".to_string());
                        match type_name.as_str() {
                            "int" => std::mem::size_of::<i64>(),
                            "float" => std::mem::size_of::<f64>(),
                            "bool" => std::mem::size_of::<u8>(),
                            "ptr" => std::mem::size_of::<*const std::ffi::c_void>(),
                            _ => 0,
                        }
                    }).sum();
                    for i in 0..len {
                        let struct_ptr = (ptr as usize + i * struct_size) as *const u8;
                        let mut field_map = FxHashMap::default();
                        let mut offset = 0;
                        for (field_name, field_type, _) in fields {
                            let type_name = get_type_from_statement(field_type).unwrap_or_else(|| "any".to_string());
                            let val = match type_name.as_str() {
                                "int" => {
                                    let p = unsafe { struct_ptr.add(offset) as *const i64 };
                                    offset += std::mem::size_of::<i64>();
                                    Value::Int(unsafe { (*p).into() })
                                }
                                "float" => {
                                    let p = unsafe { struct_ptr.add(offset) as *const f64 };
                                    offset += std::mem::size_of::<f64>();
                                    Value::Float(unsafe { (*p).into() })
                                }
                                "bool" => {
                                    let p = unsafe { struct_ptr.add(offset) as *const u8 };
                                    offset += std::mem::size_of::<u8>();
                                    Value::Boolean(unsafe { *p != 0 })
                                }
                                "ptr" => {
                                    let p = unsafe { struct_ptr.add(offset) as *const *const std::ffi::c_void };
                                    offset += std::mem::size_of::<*const std::ffi::c_void>();
                                    Value::Pointer(Arc::new(Mutex::new((Value::Int(unsafe { *p as usize as i64 }.into()), 1))))
                                }
                                _ => Value::Null,
                            };
                            field_map.insert(Value::String(field_name.clone()), val);
                        }
                        result.push(Value::Map(field_map));
                    }
                    Value::List(result)
                }
                _ => Value::Error("TypeError", "Type must be struct for C struct list", None),
            }
        }
        _ => Value::Error("TypeError", "Expected type string or struct type", None),
    }
}

#[cfg(windows)]
unsafe extern "system" {
    fn SetDllDirectoryA(lpPathName: *const i8) -> i32;
}

#[cfg(windows)]
fn set_dll_directory(args: &HashMap<String, Value>) -> Value {
    let path = match args.get("path") {
        Some(Value::String(s)) => s,
        _ => return libload_error("Expected path: str"),
    };

    let c_path = match CString::new(path.as_str()) {
        Ok(c) => c,
        Err(_) => return libload_error("Failed to create CString from path"),
    };

    unsafe {
        if SetDllDirectoryA(c_path.as_ptr()) == 0 {
            return libload_error("Failed to set DLL directory");
        }
    }

    Value::Null
}

struct CallbackData {
    body: Box<Vec<Statement>>,
    interp_ptr: *mut Interpreter,
}

unsafe impl Sync for CallbackData {}

static CALLBACKS: Mutex<Vec<&'static CallbackData>> = Mutex::new(Vec::new());

fn create_callback(args: &HashMap<String, Value>, interp_ptr: &mut Interpreter) -> Value {
    let func = match args.get("f") {
        Some(Value::Function(f)) => f.clone(),
        _ => return Value::Error("TypeError", "Expected function argument", None),
    };

    let cb: &'static CallbackData = Box::leak(Box::new(CallbackData {
        body: Box::new(func.get_body()),
        interp_ptr,
    }));

    let mut lock = CALLBACKS.lock();
    lock.push(cb);

    extern "C" fn wrapper() {
        unsafe {
            let lock = CALLBACKS.lock();
            let cb = lock.last().unwrap();
            let interp: &mut Interpreter = &mut *cb.interp_ptr;

            let args: HashMap<String, Value> = HashMap::new();
            for (k, v) in args.iter() {
                interp.variables.insert(
                    k.clone(),
                    Variable::new(k.to_string(), v.clone(), "any".to_string(), true, false, false),
                );
            }

            for stmt in cb.body.iter() {
                let _res = interp.evaluate(&stmt);
                if interp.err.is_some() {
                    return;
                }
            }
        }
    }

    let func_ptr_int: Int = (wrapper as *const () as usize as i64).into();
    Value::Pointer(Arc::new(Mutex::new((Value::Int(func_ptr_int), 1))))
}


fn cast(args: &HashMap<String, Value>) -> Value {
    let val = match args.get("value") {
        Some(v) => v.clone(),
        None => return Value::Error("TypeError", "Expected value argument", None),
    };

    let ptr_arc = match args.get("to") {
        Some(Value::Pointer(p)) => p.clone(),
        _ => return Value::Error("TypeError", "Expected a pointer", None),
    };
    {
        let mut inner = ptr_arc.lock();
        std::mem::drop(std::mem::replace(&mut *inner, (val, 1)));
    }

    Value::Pointer(ptr_arc)
}

fn create_array_ptr(args: &HashMap<String, Value>) -> Value {
    let size = match args.get("size") {
        Some(Value::Int(i)) => match i.to_i64() {
            Ok(v) if v >= 0 => v as usize,
            Ok(_) | Err(_) => return libload_error("Invalid Int value for size"),
        },
        _ => return libload_error("Expected size: Int"),
    };

    let elem_type = match args.get("type") {
        Some(Value::String(s)) => match s.as_str() {
            "int" => ValueType::Int,
            "float64" => ValueType::Float64,
            "float" | "float32" => ValueType::Float32,
            "bool" => ValueType::Boolean,
            "byte" => ValueType::Byte,
            "ptr" => ValueType::Ptr,
            "void" => ValueType::Void,
            _ => return libload_error("Unknown type string"),
        },
        _ => return libload_error("Expected type: str"),
    };

    let elements_lucia = match args.get("elements") {
        Some(Value::List(l)) => l.clone(),
        _ => return libload_error("Expected elements: list"),
    };

    let mut bytes_vec: Vec<u8> = Vec::with_capacity(size * 8);

    for val in elements_lucia.iter().take(size) {
        match elem_type {
            ValueType::Byte => match val {
                Value::Int(i) => match i.to_i64() {
                    Ok(v) if v >= 0 && v <= 255 => bytes_vec.push(v as u8),
                    _ => return libload_error("Invalid Int for Byte"),
                },
                _ => return libload_error("Expected Int for Byte array"),
            },
            ValueType::Boolean => match val {
                Value::Boolean(b) => bytes_vec.push(if *b { 1 } else { 0 }),
                _ => return libload_error("Expected Boolean for Boolean array"),
            },
            ValueType::Int => match val {
                Value::Int(i) => match i.to_i32() {
                    Ok(v) => bytes_vec.extend(&v.to_le_bytes()),
                    Err(_) => return libload_error("Invalid Int value"),
                },
                _ => return libload_error("Expected Int for Int array"),
            },
            ValueType::Float64 => match val {
                Value::Float(f) => match f.to_f64() {
                    Ok(v) => bytes_vec.extend(&v.to_le_bytes()),
                    Err(_) => return libload_error("Invalid Float value"),
                },
                _ => return libload_error("Expected Float for Float64 array"),
            },
            ValueType::Float32 => match val {
                Value::Float(f) => match f.to_f32() {
                    Ok(v) => bytes_vec.extend(&v.to_le_bytes()),
                    Err(_) => return libload_error("Invalid Float value"),
                },
                _ => return libload_error("Expected Float for Float32 array"),
            },
            ValueType::Ptr => match val {
                Value::Pointer(p) => {
                    let addr = match &*p.lock() {
                        (Value::Int(i), _) => match i.to_i64() {
                            Ok(v) => v as usize,
                            Err(_) => return libload_error("Invalid Int inside Pointer"),
                        },
                        _ => return libload_error("Expected Int inside Pointer"),
                    };
                    bytes_vec.extend(&addr.to_le_bytes());
                }
                _ => return libload_error("Expected Pointer for Ptr array"),
            },
            _ => return libload_error("Unsupported type for array allocation"),
        }
    }

    let boxed_slice = bytes_vec.into_boxed_slice();
    let ptr_val = (boxed_slice.as_ptr() as usize).into();

    std::mem::forget(boxed_slice);

    let arc_ptr = Arc::new(Mutex::new((Value::Int(ptr_val), 1)));
    Value::Pointer(arc_ptr)
}

fn load_lib(args: &HashMap<String, Value>) -> Value {
    let flags: Option<i32> = match args.get("flags") {
        Some(Value::Int(i)) => match i.to_i32() {
            Ok(f) => Some(f),
            Err(_) => return libload_error("Invalid flags value"),
        },
        _ => None,
    };
    #[cfg(not(unix))]
    if flags.is_some() {
        return libload_error("RTLD flags are only supported on Unix systems");
    }
    match args.get("path") {
        Some(Value::String(path)) => {
            let lib = {
                #[cfg(unix)]
                {
                    if let Some(f) = flags {
                        unsafe { LuciaLib::load_flags(Path::new(path), f) }
                    } else {
                        unsafe { LuciaLib::load(Path::new(path)) }
                    }
                }
                #[cfg(not(unix))]
                {
                    unsafe { LuciaLib::load(Path::new(path)) }
                }
            };
            match lib {
                Ok(lib) => {
                    let ptr_val = Box::into_raw(Box::new(lib)) as usize;
                    Value::Pointer(Arc::new(Mutex::new((Value::Int((ptr_val as i64).into()), 1))))
                }
                Err(e) => libload_error(&format!("Failed to load library: {}", e)),
            }
        }
        _ => libload_error("Expected path: str"),
    }
}

fn get_fn(args: &HashMap<String, Value>) -> Value {
    let lib_val = args.get("lib");
    let name_val = args.get("name");
    let args_val = args.get("args");
    let ret_val = args.get("ret");

    match (lib_val, name_val, args_val, ret_val) {
        (
            Some(Value::Pointer(lib_ptr_arc)),
            Some(Value::String(name)),
            Some(Value::List(arg_tys)),
            Some(Value::String(ret_ty)),
        ) => {
            let raw_ptr = if let (Value::Int(ptr_int), _) = &*lib_ptr_arc.lock() {
                match ptr_int.to_i64() {
                    Ok(i) => i as usize as *mut LuciaLib,
                    Err(_) => return libload_error("Invalid library pointer conversion"),
                }
            } else {
                return libload_error("Invalid library pointer type");
            };

            let lib = unsafe { &*raw_ptr };

            let parse_ty = |s: &str| match s {
                "int" => Some(ValueType::Int),
                "float64" => Some(ValueType::Float64),
                "float" | "float32" => Some(ValueType::Float32),
                "bool" => Some(ValueType::Boolean),
                "void" => Some(ValueType::Void),
                "byte" => Some(ValueType::Byte),
                "any" | "str" | "ptr" => Some(ValueType::Ptr),
                "array" => Some(ValueType::Ptr),
                "" => Some(ValueType::Ptr),
                _ => None,
            };

            let arg_types: Option<Vec<_>> = arg_tys
                .iter()
                .map(|v| {
                    if let Value::String(s) = v {
                        parse_ty(s)
                    } else if let Value::Type(t) = v {
                        parse_ty(&t.display_simple())
                    } else {
                        None
                    }
                })
                .collect();

            let ret_ty = parse_ty(ret_ty.as_str());

            match (arg_types, ret_ty) {
                (Some(arg_types), Some(ret_ty)) => {
                    match unsafe { lib.get_function(name, arg_types, ret_ty.clone()) } {
                        Ok(f) => {
                            let fn_ptr = Box::into_raw(Box::new(f)) as usize as i64;
                            Value::Pointer(Arc::new(Mutex::new((Value::Int(fn_ptr.into()), 1))))
                        }
                        Err(e) => libload_error(&format!("Failed to get function: {}", e)),
                    }
                }
                _ => libload_error("Invalid argument or return types"),
            }
        }
        _ => libload_error("Expected (lib: ptr, name: str, args: [str], ret: str)"),
    }
}

pub fn get_fn_std(args: &HashMap<String, Value>) -> Value {
    let name_val = args.get("name");
    let args_val = args.get("args");
    let ret_val = args.get("ret");

    match (name_val, args_val, ret_val) {
        (Some(Value::String(name)), Some(Value::List(arg_tys)), Some(Value::String(ret_ty))) => {
            let parse_basic_ty = |s: &str| match s {
                "int" => Some(ValueType::Int),
                "float64" => Some(ValueType::Float64),
                "float" | "float32" => Some(ValueType::Float32),
                "bool" => Some(ValueType::Boolean),
                "void" => Some(ValueType::Void),
                "any" | "str" | "ptr" => Some(ValueType::Ptr),
                "" => Some(ValueType::Ptr),
                _ => None,
            };

            let parse_ty = |s: &str| {
                if let Some(stripped) = s.strip_prefix('*') {
                    Some((parse_basic_ty(stripped)?, true))
                } else {
                    Some((parse_basic_ty(s)?, false))
                }
            };

            let arg_types: Option<Vec<_>> = arg_tys.iter()
                .map(|v| if let Value::String(s) = v { parse_ty(s) } else { None })
                .collect();

            let ret_type = parse_basic_ty(ret_ty);

            match (arg_types, ret_type) {
                (Some(arg_types), Some(_)) => {
                    let is_variadic = arg_types.iter().any(|(_, var)| *var);

                    let c_name = match CString::new(name.as_str()) {
                        Ok(c) => c,
                        Err(_) => return libload_error("Invalid function name"),
                    };

                    #[cfg(unix)]
                    let f_ptr = unsafe { dlsym(RTLD_DEFAULT, c_name.as_ptr()) };

                    #[cfg(windows)]
                    let f_ptr = unsafe {
                        let mut h_mod = GetModuleHandleA(b"ucrtbase.dll\0".as_ptr() as *const i8);
                        if h_mod.is_null() {
                            h_mod = GetModuleHandleA(b"msvcrt.dll\0".as_ptr() as *const i8);
                        }
                        if h_mod.is_null() {
                            return libload_error("Failed to find CRT DLL");
                        }
                        GetProcAddress(h_mod, c_name.as_ptr())
                    };

                    if f_ptr.is_null() {
                        return libload_error(&format!("Function not found: {}", name));
                    }

                    let fn_ptr = Box::into_raw(Box::new((f_ptr, is_variadic))) as usize as i64;
                    Value::Pointer(Arc::new(Mutex::new((Value::Int(fn_ptr.into()), 1))))
                }
                _ => libload_error("Invalid argument or return types"),
            }
        }
        _ => libload_error("Expected (name: str, args: [str], ret: str)"),
    }
}

fn call_fn(args: &HashMap<String, Value>) -> Value {
    match (args.get("fn"), args.get("args"), args.get("stdcall")) {
        (Some(Value::Pointer(fn_ptr_arc)), Some(Value::List(call_args)), Some(Value::Boolean(stdcall))) => {
            let fn_ptr = if let (Value::Int(ptr_int), _) = &*fn_ptr_arc.lock() {
                match ptr_int.to_i64() {
                    Ok(i) => i as usize as *mut LuciaFfiFn,
                    Err(_) => return libload_error("Invalid function pointer (failed to convert to i64)"),
                }
            } else {
                return libload_error("Invalid function pointer type");
            };

            let fun = unsafe { &*fn_ptr };

            match unsafe { fun.call(call_args, *stdcall) } {
                Ok(v) => v,
                Err(e) => libload_error(&e),
            }
        }
        _ => libload_error("Expected (fn: ptr, args: [any], stdcall: bool)"),
    }
}

fn create_struct(args: &HashMap<String, Value>) -> Value {
    let values: Vec<&Value> = match args.get("fields") {
        Some(Value::List(l)) => l.iter().collect(),
        _ => return Value::Error("TypeError", "Expected fields: list", None),
    };

    let types: Vec<&str> = match args.get("types") {
        Some(Value::List(l)) => l.iter().filter_map(|v| {
            if let Value::String(s) = v { Some(s.as_str()) } else { None }
        }).collect(),
        None => vec!["int"; values.len()],
        _ => return Value::Error("TypeError", "Expected types: list of str", None),
    };

    if types.len() != values.len() {
        return Value::Error("ValueError", "Types length must match values length", None);
    }

    let is_ptr = match args.get("is_ptr") {
        Some(Value::Boolean(b)) => *b,
        _ => false,
    };

    let mut bytes_vec: Vec<u8> = Vec::new();

    for (val, ty) in values.iter().zip(types.iter()) {
        match *ty {
            "byte" => if let Value::Int(i) = val {
                bytes_vec.push(i.to_i64().unwrap_or(0) as u8);
            },
            "bool" => if let Value::Boolean(b) = val {
                bytes_vec.push(if *b { 1 } else { 0 });
            },
            "int" => if let Value::Int(i) = val {
                bytes_vec.extend(&i.to_i64().unwrap_or(0).to_le_bytes());
            },
            "float" => if let Value::Float(f) = val {
                bytes_vec.extend(&f.to_f64().unwrap_or(0.0).to_le_bytes());
            },
            "ptr" => if let Value::Pointer(p) = val {
                let addr = match &*p.lock() {
                    (Value::Int(i), _) => i.to_i64().unwrap_or(0) as usize,
                    _ => 0usize,
                };
                bytes_vec.extend(&addr.to_le_bytes());
            },
            "str" => if let Value::String(s) = val {
                bytes_vec.extend(&(s.as_ptr() as usize).to_le_bytes());
            },
            _ => {},
        }
    }

    if is_ptr {
        let boxed = bytes_vec.into_boxed_slice();
        let raw_ptr = Box::into_raw(boxed) as *mut u8;
        return Value::Pointer(Arc::new(Mutex::new((Value::Int(Int::from(raw_ptr as usize)), 1))));
    }

    let hex_string: String = bytes_vec.iter().rev().map(|b| format!("{:02X}", b)).collect();

    match Int::from_hex(&hex_string) {
        Ok(num) => Value::Int(num),
        Err(_) => Value::Error("ValueError", "Failed to convert hex to Int", None),
    }
}

fn parse_struct(args: &HashMap<String, Value>) -> Value {
    let struct_val = match args.get("struct") {
        Some(v) => v,
        None => return Value::Error("TypeError", "Expected struct", None),
    };

    let types: Vec<&str> = match args.get("types_of_the_fields") {
        Some(Value::List(l)) => l.iter().filter_map(|v| {
            if let Value::String(s) = v { Some(s.as_str()) } else { None }
        }).collect(),
        _ => return Value::Error("TypeError", "Expected types_of_the_fields: list of str", None),
    };

    let is_ptr = match args.get("is_ptr") {
        Some(Value::Boolean(b)) => *b,
        _ => false,
    };

    let packed = match args.get("packed") {
        Some(Value::Boolean(b)) => *b,
        _ => true,
    };

    let bytes: Vec<u8> = if is_ptr {
        if let Value::Pointer(p) = struct_val {
            let addr = match &*p.lock() {
                (Value::Int(i), _) => i.to_i64().unwrap_or(0) as *const u8,
                _ => return Value::Error("TypeError", "Expected pointer to struct bytes", None),
            };
            let total_len: usize = types.iter().map(|ty| match *ty {
                "byte" | "bool" => 1,
                "int" => std::mem::size_of::<i64>(),
                "float" => std::mem::size_of::<f64>(),
                "ptr" | "str" => std::mem::size_of::<usize>(),
                _ => 0,
            }).sum();

            unsafe { std::slice::from_raw_parts(addr, total_len).to_vec() }
        } else {
            return Value::Error("TypeError", "Expected struct as pointer", None);
        }
    } else if let Value::Int(i) = struct_val {
        let hex = match i.to_str_radix(16) {
            Ok(s) => {
                if s.len() % 2 == 0 { s } else { format!("0{}", s) }
            }
            Err(_) => return Value::Error("ValueError", "Failed to convert Int to hex string", None),
        };

        (0..hex.len())
            .step_by(2)
            .filter_map(|j| u8::from_str_radix(&hex[j..j+2], 16).ok())
            .rev()
            .collect()
    } else {
        return Value::Error("TypeError", "Expected struct as int or ptr", None);
    };

    let mut offset = 0;
    let mut fields: Vec<Value> = Vec::new();

    for ty in types {
        let (size, align) = match ty {
            "byte" | "bool" => (1, 1),
            "int" => (8, 8),
            "float" => (8, 8),
            "ptr" | "str" => (std::mem::size_of::<usize>(), std::mem::size_of::<usize>()),
            _ => (0, 1),
        };

        if !packed && align > 1 {
            let padding = (align - (offset % align)) % align;
            offset += padding;
        }

        match ty {
            "byte" => {
                fields.push(Value::Int(Int::from(bytes[offset] as i64)));
            }
            "bool" => {
                fields.push(Value::Boolean(bytes[offset] != 0));
            }
            "int" => {
                let mut buf = [0u8; 8];
                buf.copy_from_slice(&bytes[offset..offset+8]);
                fields.push(Value::Int(Int::from(i64::from_le_bytes(buf))));
            }
            "float" => {
                let mut buf = [0u8; 8];
                buf.copy_from_slice(&bytes[offset..offset+8]);
                fields.push(Value::Float(Float::from(f64::from_le_bytes(buf))));
            }
            "ptr" | "str" => {
                let mut buf = [0u8; std::mem::size_of::<usize>()];
                buf.copy_from_slice(&bytes[offset..offset+std::mem::size_of::<usize>()]);
                fields.push(Value::Pointer(
                    Arc::new(Mutex::new((Value::Int(Int::from(usize::from_le_bytes(buf))), 1)))
                ));
            }
            _ => {}
        }

        offset += size;
    }

    Value::List(fields)
}

// libload.parse_struct(libload.create_struct([240, 240, 240, 255], ["byte", "byte", "byte", "byte"]), ["byte", "byte", "byte", "byte"])

pub fn init_libload(config: Arc<Config>, file_path: String) -> Result<(), Error> {
    if !get_from_config(&config, "allow_unsafe").is_truthy() {
        return Err(Error::with_help(
            "UnsafeError",
            "Libload operations are unsafe and cannot be performed without explicit permission.",
            "To enable unsafe operations, set 'allow_unsafe' to true in the configuration.",
            to_static(file_path),
        ));
    }
    Ok(())
}

fn malloc_fn(args: &HashMap<String, Value>) -> Value {
    let size = match args.get("size") {
        Some(Value::Int(i)) => i.to_i64().unwrap_or(0) as usize,
        _ => return Value::Error("TypeError", "Expected 'size': Int", None),
    };

    let ptr = unsafe { libc::malloc(size) as *mut u8 };
    if ptr.is_null() {
        return Value::Error("MemoryError", "Failed to allocate memory", None);
    }

    Value::Pointer(Arc::new(Mutex::new((Value::Int((ptr as usize as i64).into()), 1))))
}

fn write_byte_fn(args: &HashMap<String, Value>) -> Value {
    let base = match args.get("base") {
        Some(Value::Pointer(p)) => {
            if let (Value::Int(i), _) = &*p.lock() { i.to_i64().unwrap_or(0) as *mut u8 } else { return Value::Int(0.into()); }
        }
        _ => return Value::Error("TypeError", "Expected 'base': Pointer", None),
    };

    let offset = match args.get("offset") {
        Some(Value::Int(i)) => i.to_i64().unwrap_or(0) as usize,
        _ => return Value::Error("TypeError", "Expected 'offset': Int", None),
    };

    let value = match args.get("value") {
        Some(Value::Int(i)) => i.to_i64().unwrap_or(0) as u8,
        _ => return Value::Error("TypeError", "Expected 'value': Int", None),
    };

    unsafe { base.add(offset).write(value) };

    Value::Int(0.into())
}

fn write_i64_fn(args: &HashMap<String, Value>) -> Value {
    let base = match args.get("base") {
        Some(Value::Pointer(p)) => {
            if let (Value::Int(i), _) = &*p.lock() { i.to_i64().unwrap_or(0) as *mut i64 } else { return Value::Int(0.into()); }
        }
        _ => return Value::Error("TypeError", "Expected 'base': Pointer", None),
    };

    let offset = match args.get("offset") {
        Some(Value::Int(i)) => i.to_i64().unwrap_or(0) as usize,
        _ => return Value::Error("TypeError", "Expected 'offset': Int", None),
    };

    let value = match args.get("value") {
        Some(Value::Int(i)) => i.to_i64().unwrap_or(0),
        _ => return Value::Error("TypeError", "Expected 'value': Int", None),
    };

    unsafe { (base as *mut u8).add(offset).cast::<i64>().write(value) };

    Value::Int(0.into())
}

fn write_f64_fn(args: &HashMap<String, Value>) -> Value {
    let base = match args.get("base") {
        Some(Value::Pointer(p)) => {
            if let (Value::Int(i), _) = &*p.lock() {
                i.to_i64().unwrap_or(0) as *mut f64
            } else { return Value::Int(0.into()); }
        }
        _ => return Value::Error("TypeError", "Expected 'base': Pointer", None),
    };

    let offset = match args.get("offset") {
        Some(Value::Int(i)) => i.to_i64().unwrap_or(0) as usize,
        _ => return Value::Error("TypeError", "Expected 'offset': Int", None),
    };

    let value = match args.get("value") {
        Some(Value::Float(f)) => f.to_f64().unwrap_or(0.0),
        _ => return Value::Error("TypeError", "Expected 'value': Float", None),
    };

    unsafe { (base as *mut u8).add(offset).cast::<f64>().write(value) };

    Value::Int(0.into())
}

pub fn write_ptr_fn(args: &HashMap<String, Value>) -> Value {
    let base = match args.get("base") {
        Some(Value::Pointer(p)) => match &p.lock().0 {
            Value::Int(addr) => addr.to_i64().unwrap_or(0) as *mut u8,
            _ => return Value::Error("TypeError", "'base' Pointer must contain Int", None),
        },
        _ => return Value::Error("TypeError", "Expected 'base' as Pointer", None),
    };

    let offset = match args.get("offset") {
        Some(Value::Int(i)) => i.to_i64().unwrap_or(0) as usize,
        _ => return Value::Error("TypeError", "Expected 'offset' as Int", None),
    };

    let value = match args.get("ptr") {
        Some(Value::Pointer(p)) => match &p.lock().0 {
            Value::Int(addr) => addr.to_i64().unwrap_or(0) as *const c_void,
            _ => std::ptr::null(),
        },
        _ => std::ptr::null(),
    };

    unsafe { (base.add(offset) as *mut *const c_void).write(value) };

    Value::Int(0.into())
}

pub fn unload_lib(args: &HashMap<String, Value>) -> Value {
    let lib_val = args.get("lib");

    match lib_val {
        Some(Value::Pointer(lib_ptr_arc)) => {
            let raw_ptr = if let Value::Int(ptr_int) = &lib_ptr_arc.lock().0 {
                match ptr_int.to_i64() {
                    Ok(i) => i as usize as *mut LuciaLib,
                    Err(_) => return libload_error("Invalid library pointer conversion"),
                }
            } else {
                return libload_error("Invalid library pointer type");
            };

            unsafe {
                drop(Box::from_raw(raw_ptr));
            }

            Value::Null
        }
        _ => libload_error("Expected (lib: ptr)"),
    }
}

#[cfg(unix)]
fn insert_rtld_constants(map: &mut HashMap<String, Variable>) {
    insert_native_var!(map, "RTLD_LAZY", Value::Int(libc::RTLD_LAZY.into()), "int");
    insert_native_var!(map, "RTLD_NOW", Value::Int(libc::RTLD_NOW.into()), "int");
    insert_native_var!(map, "RTLD_GLOBAL", Value::Int(libc::RTLD_GLOBAL.into()), "int");
    insert_native_var!(map, "RTLD_LOCAL", Value::Int(libc::RTLD_LOCAL.into()), "int");
    insert_native_var!(map, "RTLD_NODELETE", Value::Int(libc::RTLD_NODELETE.into()), "int");
    insert_native_var!(map, "RTLD_NOLOAD", Value::Int(libc::RTLD_NOLOAD.into()), "int");
    #[cfg(target_os = "linux")]
    insert_native_var!(map, "RTLD_DEEPBIND", Value::Int(libc::RTLD_DEEPBIND.into()), "int");
}

pub fn register() -> HashMap<String, Variable> {
    let mut map = HashMap::new();

    insert_native_fn!(
        map,
        "load_lib",
        load_lib,
        vec![Parameter::positional("path", "str"), Parameter::positional_optional("flags", "int", Value::Null)],
        "any",
        EffectFlags::UNSAFE
    );

    insert_native_fn!(
        map,
        "get_fn",
        get_fn,
        vec![
            Parameter::positional("lib", "any"),
            Parameter::positional("name", "str"),
            Parameter::positional("args", "list"),
            Parameter::positional("ret", "str"),
        ],
        "any",
        EffectFlags::UNSAFE
    );

    insert_native_fn!(
        map,
        "get_fn_std",
        get_fn_std,
        vec![
            Parameter::positional("name", "str"),
            Parameter::positional("args", "list"),
            Parameter::positional("ret", "str"),
        ],
        "any",
        EffectFlags::UNSAFE
    );

    insert_native_fn!(
        map,
        "call_fn",
        call_fn,
        vec![
            Parameter::positional("fn", "any"),
            Parameter::positional("args", "list"),
            Parameter::positional_optional("stdcall", "bool", Value::Boolean(false)),
        ],
        "any",
        EffectFlags::UNSAFE
    );

    insert_native_fn!(
        map,
        "create_str_ptr",
        create_str_ptr,
        vec![Parameter::positional("string", "str")],
        "any",
        EffectFlags::UNSAFE
    );

    insert_native_fn!(
        map,
        "parse_str_ptr",
        parse_str_ptr,
        vec![Parameter::positional("ptr", "any")],
        "str",
        EffectFlags::UNSAFE
    );

    insert_native_fn!(
        map,
        "get_list",
        get_list_native,
        vec![
            Parameter::positional("ptr", "any"),
            Parameter::positional("type", "str"),
            Parameter::positional("len", "int")
        ],
        "list",
        EffectFlags::UNSAFE
    );
    insert_native_fn!(
        map,
        "cast",
        cast,
        vec![
            Parameter::positional("value", "any"),
            Parameter::positional("to", "any")
        ],
        "any",
        EffectFlags::UNSAFE
    );
    insert_native_shared_fn!(
        map,
        "create_callback",
        create_callback,
        vec![Parameter::positional("f", "function")],
        "any",
        EffectFlags::UNSAFE
    );
    insert_native_fn!(
        map,
        "create_struct",
        create_struct,
        vec![
            Parameter::positional("fields", "list"),
            Parameter::positional_optional("types", "list", Value::Null),
            Parameter::positional_optional("packed", "bool", Value::Boolean(false)),
            Parameter::positional_optional("is_ptr", "bool", Value::Boolean(false)),
        ],
        "any",
        EffectFlags::UNSAFE
    );
    insert_native_fn!(
        map,
        "parse_struct",
        parse_struct,
        vec![
            Parameter::positional("struct", "any"),
            Parameter::positional("types_of_the_fields", "list"),
            Parameter::positional_optional("packed", "bool", Value::Boolean(false)),
            Parameter::positional_optional("is_ptr", "bool", Value::Boolean(false)),
        ],
        "list",
        EffectFlags::UNSAFE
    );

    insert_native_fn!(
        map,
        "malloc",
        malloc_fn,
        vec![Parameter::positional("size", "int")],
        "any",
        EffectFlags::UNSAFE
    );
    insert_native_fn!(
        map,
        "write_byte",
        write_byte_fn,
        vec![
            Parameter::positional("base", "any"),
            Parameter::positional("offset", "int"),
            Parameter::positional("value", "int"),
        ],
        "int",
        EffectFlags::UNSAFE
    );
    insert_native_fn!(
        map,
        "write_i64",
        write_i64_fn,
        vec![
            Parameter::positional("base", "any"),
            Parameter::positional("offset", "int"),
            Parameter::positional("value", "int"),
        ],
        "int",
        EffectFlags::UNSAFE
    );
    insert_native_fn!(
        map,
        "write_f64",
        write_f64_fn,
        vec![
            Parameter::positional("base", "any"),
            Parameter::positional("offset", "int"),
            Parameter::positional("value", "float"),
        ],
        "int",
        EffectFlags::UNSAFE
    );
    insert_native_fn!(
        map,
        "write_ptr",
        write_ptr_fn,
        vec![
            Parameter::positional("base", "any"),
            Parameter::positional("offset", "int"),
            Parameter::positional("ptr", "any"),
        ],
        "int",
        EffectFlags::UNSAFE
    );
    insert_native_fn!(
        map,
        "unload_lib",
        unload_lib,
        vec![Parameter::positional("lib", "any")],
        "void",
        EffectFlags::UNSAFE
    );

    #[cfg(windows)]
    insert_native_fn!(
        map,
        "set_dll_directory",
        set_dll_directory,
        vec![Parameter::positional("path", "str")],
        "void",
        EffectFlags::UNSAFE
    );

    insert_native_fn!(
        map,
        "create_array_ptr",
        create_array_ptr,
        vec![
            Parameter::positional("elements", "list"),
            Parameter::positional("size", "int"),
            Parameter::positional("type", "str"),
        ],
        "any",
        EffectFlags::UNSAFE
    );

    #[cfg(unix)]
    insert_rtld_constants(&mut map);

    map
}
