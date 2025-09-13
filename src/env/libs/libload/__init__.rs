use std::collections::HashMap;
use std::sync::Arc;
use std::path::Path;
use std::ffi::CString;
use std::sync::Mutex;
use once_cell::sync::OnceCell;

#[cfg(unix)]
use libc::{dlsym, RTLD_DEFAULT};

#[cfg(windows)]
#[link(name = "kernel32")]
unsafe extern "system" {
    fn GetModuleHandleA(lpModuleName: *const i8) -> *mut u8;
    fn GetProcAddress(hModule: *mut u8, lpProcName: *const i8) -> *mut u8;
}

use crate::env::runtime::functions::{NativeFunction, Parameter};
use crate::env::runtime::utils::to_static;
use crate::env::runtime::value::Value;
use crate::env::runtime::variables::Variable;
use crate::env::runtime::config::{Config, get_from_config};
use crate::env::runtime::errors::Error;
use crate::env::runtime::functions::Function;
use crate::env::runtime::types::{Type, Int};
use crate::env::runtime::statements::Statement;
use crate::interpreter::Interpreter;

use crate::env::libs::libload::ffi::{LuciaLib, LuciaFfiFn, ValueType, get_list};

use crate::{insert_native_fn};

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
    let mut locked = strings.lock().unwrap();

    for cstr in locked.iter() {
        if let Ok(existing_str) = cstr.to_str() {
            if existing_str == s {
                let ptr = cstr.as_ptr() as usize as i64;
                return Value::Pointer(Arc::new(Value::Int(ptr.into())));
            }
        }
    }

    let cstring = match CString::new(s.as_str()) {
        Ok(c) => c,
        Err(_) => return libload_error("Failed to create CString"),
    };
    let ptr = cstring.as_ptr();

    locked.push(cstring);

    Value::Pointer(Arc::new(Value::Int((ptr as usize as i64).into())))
}


fn get_list_native(args: &HashMap<String, Value>) -> Value {
    let ptr_val = args.get("ptr");
    let type_val = args.get("type");
    let len_val = args.get("len");
    let ptr = match ptr_val {
        Some(Value::Pointer(arc)) => {
            if let Value::Int(i) = arc.as_ref() {
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
                "float" => ValueType::Float,
                "bool" => ValueType::Boolean,
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
                        let mut field_map = HashMap::new();
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
                                    Value::Pointer(Arc::new(Value::Int(unsafe { *p as usize as i64 }.into())))
                                }
                                _ => Value::Null,
                            };
                            field_map.insert(field_name.clone(), val);
                        }
                        result.push(Value::Map {
                            keys: field_map.keys().cloned().map(Value::String).collect(),
                            values: field_map.values().cloned().collect(),
                        });
                    }
                    Value::List(result)
                }
                _ => Value::Error("TypeError", "Type must be struct for C struct list", None),
            }
        }
        _ => Value::Error("TypeError", "Expected type string or struct type", None),
    }
}

struct CallbackData {
    body: Box<Vec<Statement>>,
    interp_ptr: *mut Interpreter,
}

unsafe impl Sync for CallbackData {}

static CALLBACKS: Mutex<Vec<&'static CallbackData>> = Mutex::new(Vec::new());

fn create_callback(args: &HashMap<String, Value>) -> Value {
    let func;
    let interp_ptr = match args.get("self") {
        Some(Value::Function(Function::CustomMethod(m))) => {
            func = m.get_function();
            &m.interpreter as *const _ as *mut Interpreter
        }
        _ => return Value::Error("TypeError", "Expected interpreter pointer argument", None),
    };

    let cb: &'static CallbackData = Box::leak(Box::new(CallbackData {
        body: Box::new(func.body.clone()),
        interp_ptr,
    }));

    let mut lock = CALLBACKS.lock().unwrap();
    lock.push(cb);

    extern "C" fn wrapper() {
        unsafe {
            let lock = CALLBACKS.lock().unwrap();
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
                dbg!(&stmt, &interp);
                let res = interp.evaluate(stmt.clone());
                dbg!(&res);
                if interp.err.is_some() {
                    return;
                }
            }
        }
    }

    let func_ptr_int: Int = (wrapper as usize as i64).into();
    Value::Pointer(Arc::new(Value::Int(func_ptr_int)))
}


fn cast(args: &HashMap<String, Value>) -> Value {
    let val = match args.get("value") {
        Some(v) => v.clone(),
        None => return Value::Error("TypeError", "Expected value argument", None),
    };

    let ptr_as_int = match args.get("to") {
        Some(Value::Pointer(ptr)) => match &**ptr {
            Value::Int(i) => match i.to_i64() {
                Ok(i) => i as usize,
                Err(_) => return Value::Error("TypeError", "Invalid pointer conversion", None),
            },
            _ => return Value::Error("TypeError", "Expected Int pointer", None),
        },
        _ => return Value::Error("TypeError", "Expected a pointer", None),
    };

    let raw_ptr = ptr_as_int as *mut Value;

    unsafe {
        raw_ptr.write(val.clone());
        Value::Pointer(Arc::from_raw(raw_ptr))
    }
}

fn load_lib(args: &HashMap<String, Value>) -> Value {
    match args.get("path") {
        Some(Value::String(path)) => {
            match unsafe { LuciaLib::load(Path::new(path)) } {
                Ok(lib) => {
                    let ptr_val = Box::into_raw(Box::new(lib)) as usize;
                    Value::Pointer(Arc::new(Value::Int((ptr_val as i64).into())))
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
            let raw_ptr = if let Value::Int(ptr_int) = &**lib_ptr_arc {
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
                "float" => Some(ValueType::Float),
                "bool" => Some(ValueType::Boolean),
                "void" => Some(ValueType::Void),
                "any" | "str" | "ptr" => Some(ValueType::Ptr),
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
                            Value::Pointer(Arc::new(Value::Int(fn_ptr.into())))
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
                "float" => Some(ValueType::Float),
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
                    Value::Pointer(Arc::new(Value::Int(fn_ptr.into())))
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
            let fn_ptr = if let Value::Int(ptr_int) = &**fn_ptr_arc {
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

pub fn register() -> HashMap<String, Variable> {
    let mut map = HashMap::new();

    insert_native_fn!(
        map,
        "load_lib",
        load_lib,
        vec![Parameter::positional("path", "str")],
        "any"
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
        "any"
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
        "any"
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
        "any"
    );

    insert_native_fn!(
        map,
        "create_str_ptr",
        create_str_ptr,
        vec![Parameter::positional("string", "str")],
        "any"
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
        "list"
    );
    insert_native_fn!(
        map,
        "cast",
        cast,
        vec![
            Parameter::positional("value", "any"),
            Parameter::positional("to", "any")
        ],
        "any"
    );
    insert_native_fn!(
        map,
        "create_callback",
        create_callback,
        vec![Parameter::positional("self", "function")],
        "any"
    );

    map
}
