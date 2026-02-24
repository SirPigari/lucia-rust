mod env {
    pub mod runtime {
        #[allow(dead_code)]
        pub mod utils;
        pub mod config;
        pub mod types;
        pub mod errors;
        pub mod value;
        pub mod functions;
        pub mod generators;
        pub mod variables;
        pub mod native;
        pub mod statements;
        pub mod pattern_reg;
        #[allow(dead_code)]
        pub mod preprocessor;
        pub mod modules;
        #[allow(dead_code)]
        pub mod libs;
        pub mod tokens;
        pub mod internal_structs;
        pub mod structs_and_enums;
        pub mod precompile;
        pub mod static_checker;
        #[allow(dead_code)]
        pub mod repl;
        pub mod plugins;
    }
    pub mod libs {
        #[cfg(feature = "math")] pub mod math { pub mod main; }
        #[cfg(feature = "os")] pub mod os { pub mod main; }
        #[cfg(feature = "time")] pub mod time { pub mod main; }
        #[cfg(feature = "json")] pub mod json { pub mod main; }
        #[cfg(feature = "clib")] pub mod clib { pub mod main; }
        #[cfg(feature = "regex")] pub mod regex { pub mod main; pub mod regex_engine; }
        #[cfg(feature = "collections")] pub mod collections { pub mod main; pub mod deprecated_stuff; }
        #[cfg(feature = "random")] pub mod random { pub mod main; }
        #[cfg(feature = "lasm")] pub mod lasm { pub mod main; }
        #[cfg(feature = "fs")] pub mod fs { pub mod main; }
        #[cfg(feature = "nest")] pub mod nest { pub mod main; }
        #[cfg(feature = "libload")] pub mod libload { pub mod main; pub mod ffi; }
        #[cfg(feature = "elevator")] pub mod elevator { pub mod main; pub mod utils; }
        #[cfg(feature = "hash")] pub mod hash { pub mod main; }
    }
}

mod lexer;
mod parser;
mod interpreter;

use crate::env::runtime::config::Config;
use crate::env::runtime::functions::FunctionMetadata;
use crate::env::runtime::internal_structs::EffectFlags;
use crate::env::runtime::types::{Type, SimpleType};
use crate::env::runtime::value::Value;
use crate::env::runtime::preprocessor::Preprocessor;
use crate::env::runtime::libs::load_std_libs_embedded;
use crate::env::runtime::utils::format_value;
use crate::env::runtime::variables::Variable;
use crate::env::runtime::errors::Error;
use crate::env::runtime::functions::{Function, NativeFunction, Parameter};
use parking_lot::Mutex;
use rustc_hash::FxHashMap;
use std::collections::HashMap;
use core::ffi::{c_char, c_void};
use std::mem::ManuallyDrop;
use std::ffi::CString;
use std::ffi::CStr;
use std::sync::Arc;
use std::path::PathBuf;
use std::cell::RefCell;
use once_cell::sync::Lazy;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};

use interpreter::Interpreter;
use parser::Parser;
use lexer::Lexer;

const VERSION: &str = env!("VERSION");
const CUSTOM_PANIC_MARKER: u8 = 0x1B;

thread_local! {
    static DEBUG_BUFFER: RefCell<String> = RefCell::new(String::with_capacity(2048));
    static DISPLAY_BUFFER: RefCell<String> = RefCell::new(String::with_capacity(2048));
    static LOCATION_BUFFER: RefCell<String> = RefCell::new(String::with_capacity(256));
}

#[repr(C)]
pub struct BuildInfo {
    name:          *const c_char,
    version:       *const c_char,
    uuid:          *const c_char,
    rustc_version: *const c_char,
    rustc_channel: *const c_char,
    target:        *const c_char,
    repository:    *const c_char,
    git_hash:      *const c_char,
    file_hash:     *const c_char,
    profile:       *const c_char,
    build_date:    *const c_char,
}

unsafe impl Send for BuildInfo {}
unsafe impl Sync for BuildInfo {}

pub type CBool = u8;

#[repr(C)]
pub struct LuciaConfig {
    pub moded: CBool,
    pub debug: CBool,
    pub debug_mode: *const c_char,
    pub supports_color: CBool,
    pub use_lucia_traceback: CBool,
    pub warnings: CBool,
    pub allow_fetch: CBool,
    pub allow_unsafe: CBool,
    pub allow_inline_config: CBool,
    pub disable_runtime_type_checking: CBool,
    pub home_dir: *const c_char,
    pub libs_paths: [*const c_char; 16], // only allow up to 16 paths for simplicity
    pub stack_size: usize,
    pub version: *const c_char,
}

unsafe fn config_from_abi(cfg: &LuciaConfig) -> Config {
    Config {
        moded: cfg.moded != 0,
        debug: cfg.debug != 0,
        debug_mode: if cfg.debug_mode.is_null() { "".into() } else { unsafe { CStr::from_ptr(cfg.debug_mode).to_string_lossy().into_owned() } },
        supports_color: cfg.supports_color != 0,
        use_lucia_traceback: cfg.use_lucia_traceback != 0,
        warnings: cfg.warnings != 0,
        allow_fetch: cfg.allow_fetch != 0,
        allow_unsafe: cfg.allow_unsafe != 0,
        allow_inline_config: cfg.allow_inline_config != 0,
        disable_runtime_type_checking: cfg.disable_runtime_type_checking != 0,
        home_dir: if cfg.home_dir.is_null() { "".into() } else { unsafe { CStr::from_ptr(cfg.home_dir).to_string_lossy().into_owned() } },
        libs_paths: {
            let mut paths = Vec::with_capacity(16);
            for i in 0..16 {
                let path_ptr = cfg.libs_paths[i];
                if !path_ptr.is_null() {
                    let path_str = unsafe { CStr::from_ptr(path_ptr).to_string_lossy().into_owned() };
                    paths.push(path_str);
                }
            }
            paths
        },
        stack_size: cfg.stack_size,
        version: if cfg.version.is_null() { "".into() } else { unsafe { CStr::from_ptr(cfg.version).to_string_lossy().into_owned() } },
        color_scheme: Default::default(),
        cache_format: Default::default(),
        type_checker: Default::default(),
    }
}

#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq)]
#[allow(non_camel_case_types)]
pub enum LuciaValueType {
    VALUE_NULL = 0,
    VALUE_INT = 1,
    VALUE_FLOAT = 2,
    VALUE_STRING = 3,
    VALUE_BOOLEAN = 4,
    VALUE_LIST = 5,
    VALUE_MAP = 6,
    VALUE_BYTES = 7,
    VALUE_NATIVE = 8, // for native C functions
    VALUE_POINTER = 9,
    VALUE_UNSUPPORTED = 255,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct LuciaValue {
    pub tag: LuciaValueType,
    pub data: ValueData,
    pub length: usize,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub union ValueData {
    pub int_v: i64,
    pub float_v: f64,
    pub bool_v: CBool,
    pub string_v: *const c_char,
    pub bytes_ptr: *const u8,
    pub list_ptr: *const LuciaValue,
    pub map_ptr: *const LuciaValue, // flattened as key,value,key,value
    pub native_func: *mut c_void,
    pub pointer: *mut c_void,
}

pub type LuciaNativeFunc = extern "C" fn(args: *const LuciaArgs) -> LuciaResult;

struct LuciaVariablesInner {
    keys: Vec<CString>,
    values: Vec<LuciaValue>,
    include_default: CBool,
}

#[repr(C)]
pub struct LuciaVariables {
    keys: *const *const c_char,
    values: *const LuciaValue,
    len: usize,
    capacity: usize,
    include_default: CBool,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct LuciaArgs {
    values: *const LuciaValue,
    len: usize,
}

fn value_to_abi(v: &Value) -> LuciaValue {
    match v {
        Value::Int(i) => LuciaValue { tag: LuciaValueType::VALUE_INT, data: ValueData { int_v: match i.to_i64() { Ok(val) => val, Err(_) => i64::MAX } }, length: 0 },
        Value::Float(f) => LuciaValue { tag: LuciaValueType::VALUE_FLOAT, data: ValueData { float_v: match f.to_f64() { Ok(val) => val, Err(_) => f64::MAX } }, length: 0 },
        Value::Boolean(b) => LuciaValue { tag: LuciaValueType::VALUE_BOOLEAN, data: ValueData { bool_v: *b as u8 }, length: 0 },
        Value::Null => LuciaValue { tag: LuciaValueType::VALUE_NULL, data: ValueData { int_v: 0 }, length: 0 },
        Value::String(s) => {
            match CString::new(s.as_str()) {
                Ok(cstr) => {
                    let ptr = cstr.into_raw();
                    LuciaValue {
                        tag: LuciaValueType::VALUE_STRING,
                        data: ValueData { string_v: ptr },
                        length: s.len(),
                    }
                }
                Err(_) => LuciaValue {
                    tag: LuciaValueType::VALUE_STRING,
                    data: ValueData { string_v: std::ptr::null() },
                    length: 0,
                },
            }
        },
        Value::Tuple(l) | Value::List(l) => {
            let abi_list: Vec<LuciaValue> =
                l.iter().map(value_to_abi).collect();

            let boxed = abi_list.into_boxed_slice();
            let ptr = boxed.as_ptr();
            let len = boxed.len();
            std::mem::forget(boxed);

            LuciaValue {
                tag: LuciaValueType::VALUE_LIST,
                data: ValueData { list_ptr: ptr },
                length: len,
            }
        },
        Value::Bytes(b) => {
            let boxed = b.clone().into_boxed_slice();
            let ptr = boxed.as_ptr();
            let len = boxed.len();
            std::mem::forget(boxed);

            LuciaValue {
                tag: LuciaValueType::VALUE_BYTES,
                data: ValueData { bytes_ptr: ptr },
                length: len,
            }
        },
        Value::Map(m) => {
            let flat_map: Vec<LuciaValue> = m.iter()
                .flat_map(|(k, v)| vec![value_to_abi(k), value_to_abi(v)])
                .collect();

            let boxed_map = flat_map.into_boxed_slice();
            let map_ptr = boxed_map.as_ptr();
            let len = boxed_map.len();
            std::mem::forget(boxed_map);

            LuciaValue {
                tag: LuciaValueType::VALUE_MAP,
                data: ValueData { map_ptr },
                length: len,
            }
        },
        Value::Pointer(p) => {
            let arc_ptr = Arc::into_raw(Arc::clone(p)) as *mut std::ffi::c_void;
            LuciaValue {
                tag: LuciaValueType::VALUE_POINTER,
                data: ValueData {
                    pointer: arc_ptr as *mut std::ffi::c_void,
                },
                length: 0,
            }
        },
        _ => LuciaValue { tag: LuciaValueType::VALUE_UNSUPPORTED, data: ValueData { int_v: 0 }, length: 0 },
    }
}

pub fn native_call_hashmap_to_lucia_args(map: &HashMap<String, Value>) -> *const LuciaArgs {
    let binding = Value::List(vec![]);
    let args = map.get("args").unwrap_or(&binding);

    let l = match args {
        Value::List(lst) => lst,
        _ => &vec![],
    };

    let values: Vec<LuciaValue> = l.iter().map(|v| value_to_abi(&v)).collect();
    
    let boxed = values.into_boxed_slice();
    let len = boxed.len();
    let values_ptr = boxed.as_ptr();
    std::mem::forget(boxed);

    let args = Box::new(LuciaArgs {
        values: values_ptr,
        len,
    });

    Box::into_raw(args) as *const LuciaArgs
}

pub fn lucia_args_free_from_map(args: *const LuciaArgs) {
    if args.is_null() {
        return;
    }
    unsafe {
        let args_ref = &*args;
        if !args_ref.values.is_null() && args_ref.len > 0 {
            let slice = std::slice::from_raw_parts_mut(
                args_ref.values as *mut LuciaValue,
                args_ref.len,
            );
            for val in slice.iter() {
                lucia_free_value(*val);
            }
            let _ = Vec::from_raw_parts(
                args_ref.values as *mut LuciaValue,
                args_ref.len,
                args_ref.len,
            );
        }
        let _ = Box::from_raw(args as *mut LuciaArgs);
    }
}

static NATIVE_FUNC_CACHE: Lazy<Mutex<HashMap<usize, Arc<NativeFunction>>>> =
    Lazy::new(|| Mutex::new(HashMap::new()));

fn abi_to_value(v: &LuciaValue) -> Value {
    match v.tag {
        LuciaValueType::VALUE_INT => Value::Int(imagnum::Int::from(unsafe { v.data.int_v })),
        LuciaValueType::VALUE_FLOAT => Value::Float(imagnum::Float::from(unsafe { v.data.float_v })),
        LuciaValueType::VALUE_BOOLEAN => Value::Boolean(unsafe { v.data.bool_v } != 0),
        LuciaValueType::VALUE_STRING => {
            if unsafe { v.data.string_v }.is_null() {
                Value::String(String::new())
            } else {
                let c_str = unsafe { CStr::from_ptr(v.data.string_v) };
                match c_str.to_str() {
                    Ok(s) => Value::String(s.to_string()),
                    Err(_) => Value::String(String::new()),
                }
            }
        },
        LuciaValueType::VALUE_LIST => {
            let len = v.length;
            if unsafe { v.data.list_ptr }.is_null() || len == 0 {
                Value::List(vec![])
            } else {
                let slice = unsafe { std::slice::from_raw_parts(v.data.list_ptr, len) };
                let list: Vec<Value> = slice.iter().map(abi_to_value).collect();
                Value::List(list)
            }
        },
        LuciaValueType::VALUE_BYTES => {
            let len = v.length;
            if unsafe { v.data.bytes_ptr }.is_null() || len == 0 {
                Value::Bytes(vec![])
            } else {
                let slice = unsafe { std::slice::from_raw_parts(v.data.bytes_ptr, len) };
                Value::Bytes(slice.to_vec())
            }
        },
        LuciaValueType::VALUE_MAP => {
            let len = v.length;
            if unsafe { v.data.map_ptr }.is_null() || len == 0 {
                Value::Map(rustc_hash::FxHashMap::default())
            } else {
                let slice = unsafe { std::slice::from_raw_parts(v.data.map_ptr, len) };
                let mut map = rustc_hash::FxHashMap::default();
                let mut i = 0;
                while i + 1 < len {
                    let key = abi_to_value(&slice[i]);
                    let value = abi_to_value(&slice[i + 1]);
                    map.insert(key, value);
                    i += 2;
                }
                Value::Map(map)
            }
        },
        LuciaValueType::VALUE_NATIVE => {
            let func_addr = unsafe { v.data.native_func as usize };

            let func = {
                let mut cache = NATIVE_FUNC_CACHE.lock();
                cache.entry(func_addr).or_insert_with(|| {
                    let func_ptr: LuciaNativeFunc = unsafe { std::mem::transmute(v.data.native_func) };

                    let handler = move |args: &HashMap<String, Value>| -> Value {
                        let abi_args = native_call_hashmap_to_lucia_args(args);
                        let res = func_ptr(abi_args);
                        lucia_args_free_from_map(abi_args);

                        match res.tag {
                            LuciaResultTag::LUCIA_RESULT_OK => abi_to_value(&(unsafe { res.data.value })),
                            LuciaResultTag::LUCIA_RESULT_ERROR => {
                                let err_type_str = if unsafe { res.data.error.err_type.is_null() } {
                                    "Unknown".to_string()
                                } else {
                                    unsafe { CStr::from_ptr(res.data.error.err_type) }
                                        .to_string_lossy()
                                        .to_string()
                                };

                                let err_msg_str = if unsafe { res.data.error.err_msg.is_null() } {
                                    "Unknown error".to_string()
                                } else {
                                    unsafe { CStr::from_ptr(res.data.error.err_msg) }
                                        .to_string_lossy()
                                        .to_string()
                                };

                                Value::new_error(&err_type_str, &err_msg_str, None)
                            },
                            _ => Value::new_error(
                                "NativeError",
                                "Native function returned invalid result tag",
                                None,
                            ),
                        }
                    };

                    Arc::new(NativeFunction {
                        func: Arc::new(handler),
                        meta: FunctionMetadata {
                            name: "native".to_owned(),
                            parameters: vec![Parameter::variadic("args", "any")],
                            return_type: Type::Simple { ty: SimpleType::Any },
                            is_public: true,
                            is_final: false,
                            is_static: false,
                            is_native: true,
                            state: None,
                            effects: EffectFlags::UNSAFE,
                            doc: None,
                        }
                    })
                })
                .clone()
            };

            Value::Function(Function::Native(func))
        }
        _ => Value::Null,
    }
}

fn error_to_abi(e: &Error) -> LuciaError {
    let location = e.loc.clone().unwrap_or_default();
    LuciaError {
        err_type: CString::new(e.err_type.clone()).unwrap().into_raw(),
        err_msg: CString::new(e.err_msg.clone()).unwrap().into_raw(),
        help_msg: CString::new(e.help.clone().unwrap_or_default()).unwrap().into_raw(),
        line_num: location.line_number as u32,
        line_text: CString::new(location.line_string.clone()).unwrap().into_raw(),
        column: location.range.0,
    }
}

fn vars_from_abi(v: *const LuciaVariables) -> FxHashMap<String, Variable> {
    let mut vars = FxHashMap::default();
    if v.is_null() {
        return vars;
    }
    let vars_ref = unsafe { &*v };
    if vars_ref.len == 0 || vars_ref.keys.is_null() || vars_ref.values.is_null() {
        return vars;
    }
    let keys_slice = unsafe { std::slice::from_raw_parts(vars_ref.keys, vars_ref.len) };
    let values_slice = unsafe { std::slice::from_raw_parts(vars_ref.values, vars_ref.len) };
    for i in 0..vars_ref.len {
        let key_ptr = keys_slice[i];
        if key_ptr.is_null() {
            continue;
        }
        let key_cstr = unsafe { CStr::from_ptr(key_ptr) };
        let key_str = match key_cstr.to_str() {
            Ok(s) => s.to_string(),
            Err(_) => continue,
        };
        let value = abi_to_value(&values_slice[i]);
        vars.insert(key_str.clone(), Variable::new(key_str, value, "any".to_string(), false, true, false));
    }
    vars
}

fn vars_to_abi(v: &FxHashMap<String, Variable>, vv: *mut LuciaVariables) {
    if vv.is_null() {
        return;
    }

    unsafe {
        let vars_ref = &*vv;
        if !vars_ref.values.is_null() && vars_ref.len > 0 {
            let values_slice = std::slice::from_raw_parts(vars_ref.values, vars_ref.len);
            for val in values_slice {
                lucia_free_value(*val);
            }
        }
    }

    let inner_ptr = unsafe { (vv as *mut u8).add(std::mem::size_of::<LuciaVariables>()) as *mut LuciaVariablesInner };

    let mut keys_cstrings: Vec<CString> = Vec::with_capacity(v.len());
    let mut values_abi: Vec<LuciaValue> = Vec::with_capacity(v.len());

    for (s, val) in v.iter() {
        match CString::new(s.as_str()) {
            Ok(cstr) => {
                keys_cstrings.push(cstr);
                values_abi.push(value_to_abi(val.get_value()));
            }
            Err(_) => {}
        }
    }

    let key_ptrs: Vec<*const c_char> = keys_cstrings.iter().map(|s| s.as_ptr()).collect();

    unsafe {
        std::ptr::write(inner_ptr, LuciaVariablesInner {
            keys: keys_cstrings,
            values: values_abi,
            include_default: (*vv).include_default,
        });

        let inner = &*inner_ptr;
        let key_ptrs_boxed = key_ptrs.into_boxed_slice();
        let keys_ptr = key_ptrs_boxed.as_ptr();
        let len = inner.values.len();
        std::mem::forget(key_ptrs_boxed);

        (*vv).keys = keys_ptr;
        (*vv).values = inner.values.as_ptr();
        (*vv).len = len;
        (*vv).capacity = len;
    }
}

#[unsafe(no_mangle)] pub extern "C" fn lucia_value_null() -> LuciaValue { LuciaValue { tag: LuciaValueType::VALUE_NULL, data: ValueData { int_v: 0 }, length: 0 } }
#[unsafe(no_mangle)] pub extern "C" fn lucia_value_int(v: i64) -> LuciaValue { LuciaValue { tag: LuciaValueType::VALUE_INT, data: ValueData { int_v: v }, length: 0 } }
#[unsafe(no_mangle)] pub extern "C" fn lucia_value_float(v: f64) -> LuciaValue { LuciaValue { tag: LuciaValueType::VALUE_FLOAT, data: ValueData { float_v: v }, length: 0 } }
#[unsafe(no_mangle)] pub extern "C" fn lucia_value_bool(v: CBool) -> LuciaValue { LuciaValue { tag: LuciaValueType::VALUE_BOOLEAN, data: ValueData { bool_v: v }, length: 0 } }
#[unsafe(no_mangle)] pub extern "C" fn lucia_value_string(utf8: *const c_char) -> LuciaValue {
    if utf8.is_null() {
        return LuciaValue { tag: LuciaValueType::VALUE_STRING, data: ValueData { string_v: std::ptr::null() }, length: 0 };
    }
    let c_str = unsafe { CStr::from_ptr(utf8) };
    match c_str.to_str() {
        Ok(s) => {
            match CString::new(s) {
                Ok(cstring) => {
                    let len = s.len();
                    let ptr = cstring.into_raw();
                    LuciaValue {
                        tag: LuciaValueType::VALUE_STRING,
                        data: ValueData { string_v: ptr },
                        length: len,
                    }
                }
                Err(_) => LuciaValue {
                    tag: LuciaValueType::VALUE_STRING,
                    data: ValueData { string_v: std::ptr::null() },
                    length: 0,
                },
            }
        }
        Err(_) => LuciaValue {
            tag: LuciaValueType::VALUE_STRING,
            data: ValueData { string_v: std::ptr::null() },
            length: 0,
        },
    }
}
#[unsafe(no_mangle)] pub extern "C" fn lucia_value_bytes(data: *const u8, len: usize) -> LuciaValue {
    if data.is_null() || len == 0 {
        return LuciaValue { tag: LuciaValueType::VALUE_BYTES, data: ValueData { bytes_ptr: std::ptr::null() }, length: 0 };
    }
    let slice = unsafe { std::slice::from_raw_parts(data, len) };
    let boxed = slice.to_vec().into_boxed_slice();
    let ptr = boxed.as_ptr();
    std::mem::forget(boxed);
    LuciaValue {
        tag: LuciaValueType::VALUE_BYTES,
        data: ValueData { bytes_ptr: ptr },
        length: len,
    }
}
#[unsafe(no_mangle)] pub extern "C" fn lucia_value_list(items: *const LuciaValue, len: usize) -> LuciaValue {
    if items.is_null() || len == 0 {
        return LuciaValue { tag: LuciaValueType::VALUE_LIST, data: ValueData { list_ptr: std::ptr::null() }, length: 0 };
    }
    let slice = unsafe { std::slice::from_raw_parts(items, len) };
    let boxed = slice.to_vec().into_boxed_slice();
    let ptr = boxed.as_ptr();
    std::mem::forget(boxed);
    LuciaValue {
        tag: LuciaValueType::VALUE_LIST,
        data: ValueData { list_ptr: ptr },
        length: len,
    }
}
#[unsafe(no_mangle)] pub extern "C" fn lucia_value_map(entries: *const LuciaValue, len: usize) -> LuciaValue {
    if entries.is_null() || len == 0 {
        return LuciaValue { tag: LuciaValueType::VALUE_MAP, data: ValueData { map_ptr: std::ptr::null() }, length: 0 };
    }
    let flat_len = len * 2;
    let slice = unsafe { std::slice::from_raw_parts(entries, flat_len) };
    let boxed = slice.to_vec().into_boxed_slice();
    let ptr = boxed.as_ptr();
    std::mem::forget(boxed);
    LuciaValue {
        tag: LuciaValueType::VALUE_MAP,
        data: ValueData { map_ptr: ptr },
        length: flat_len,
    }
}

#[unsafe(no_mangle)] pub extern "C" fn lucia_value_clone(v: LuciaValue) -> LuciaValue {
    v.clone()
}

#[unsafe(no_mangle)] pub extern "C" fn lucia_value_deep_clone(v: LuciaValue) -> LuciaValue {
    v.clone() // for now, there is no need for a deep clone since everything is cloned by the derived clone but could change in future
}

#[unsafe(no_mangle)] pub extern "C" fn lucia_value_is_valid(v: LuciaValue) -> CBool {
    match v.tag {
        LuciaValueType::VALUE_NULL | LuciaValueType::VALUE_INT | LuciaValueType::VALUE_FLOAT |
        LuciaValueType::VALUE_STRING | LuciaValueType::VALUE_BOOLEAN | LuciaValueType::VALUE_LIST |
        LuciaValueType::VALUE_MAP | LuciaValueType::VALUE_BYTES | LuciaValueType::VALUE_POINTER => 1,
        _ => 0,
    }
}

#[unsafe(no_mangle)] pub extern "C" fn lucia_value_is_valid_ptr(v: *const LuciaValue) -> CBool {
    if v.is_null() {
        return 0;
    }

    match unsafe { &*v }.tag {
        LuciaValueType::VALUE_NULL | LuciaValueType::VALUE_INT | LuciaValueType::VALUE_FLOAT |
        LuciaValueType::VALUE_STRING | LuciaValueType::VALUE_BOOLEAN | LuciaValueType::VALUE_LIST |
        LuciaValueType::VALUE_MAP | LuciaValueType::VALUE_BYTES | LuciaValueType::VALUE_POINTER => 1,
        _ => 0,
    }
}

#[unsafe(no_mangle)] pub extern "C" fn lucia_value_is_truthy(v: LuciaValue) -> CBool {
    let val = abi_to_value(&v);
    if val.is_truthy() { 1 } else { 0 }
}

#[unsafe(no_mangle)] pub extern "C" fn lucia_value_cmp(a: LuciaValue, b: LuciaValue) -> i32 {
    let val_a = abi_to_value(&a);
    let val_b = abi_to_value(&b);
    match val_a.partial_cmp(&val_b) {
        Some(std::cmp::Ordering::Less) => -1,
        Some(std::cmp::Ordering::Greater) => 1,
        Some(std::cmp::Ordering::Equal) => 0,
        None => 0,
    }
}

#[unsafe(no_mangle)] pub extern "C" fn lucia_value_hash(v: LuciaValue) -> u64 {
    use std::hash::{Hash, Hasher};
    let val = abi_to_value(&v);
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    val.hash(&mut hasher);
    hasher.finish()
}

#[unsafe(no_mangle)] pub extern "C" fn lucia_value_get_type(v: LuciaValue) -> LuciaValueType {
    v.tag
}

#[unsafe(no_mangle)] pub extern "C" fn lucia_value_type_name(t: LuciaValueType) -> *const c_char {
    match t {
        LuciaValueType::VALUE_NULL        => b"void\0".as_ptr() as *const c_char,
        LuciaValueType::VALUE_INT         => b"int\0".as_ptr() as *const c_char,
        LuciaValueType::VALUE_FLOAT       => b"float\0".as_ptr() as *const c_char,
        LuciaValueType::VALUE_STRING      => b"str\0".as_ptr() as *const c_char,
        LuciaValueType::VALUE_BOOLEAN     => b"bool\0".as_ptr() as *const c_char,
        LuciaValueType::VALUE_LIST        => b"list\0".as_ptr() as *const c_char,
        LuciaValueType::VALUE_MAP         => b"map\0".as_ptr() as *const c_char,
        LuciaValueType::VALUE_BYTES       => b"bytes\0".as_ptr() as *const c_char,
        LuciaValueType::VALUE_NATIVE      => b"native public mutable function[*any] -> any\0".as_ptr() as *const c_char,
        LuciaValueType::VALUE_POINTER     => b"<ptr>\0".as_ptr() as *const c_char,
        LuciaValueType::VALUE_UNSUPPORTED => b"<unsupported>\0".as_ptr() as *const c_char,
    }
}

#[unsafe(no_mangle)] pub extern "C" fn lucia_value_string_ptr(v: LuciaValue) -> *const c_char {
    unsafe { v.data.string_v }
}

#[unsafe(no_mangle)] pub extern "C" fn lucia_value_string_clone(v: LuciaValue, out: *mut *const c_char, out_len: *mut usize) -> CBool {
    if v.tag != LuciaValueType::VALUE_STRING {
        return 0;
    }
    let str_ptr = unsafe { v.data.string_v };
    if str_ptr.is_null() {
        unsafe {
            *out = std::ptr::null();
            *out_len = 0;
        }
        return 1;
    }
    let c_str = unsafe { CStr::from_ptr(str_ptr) };
    match c_str.to_str() {
        Ok(s) => {
            match CString::new(s) {
                Ok(cstring) => {
                    let len = s.len();
                    let ptr = cstring.into_raw();
                    unsafe {
                        *out = ptr;
                        *out_len = len;
                    }
                    1
                }
                Err(_) => 0,
            }
        }
        Err(_) => 0,
    }
}

#[unsafe(no_mangle)] pub extern "C" fn lucia_value_is_null(v: LuciaValue) -> CBool {
    if v.tag == LuciaValueType::VALUE_NULL {
        1
    } else {
        0
    }
}

#[unsafe(no_mangle)] pub extern "C" fn lucia_value_debug(v: LuciaValue) -> *const c_char {
    let val = abi_to_value(&v);
    let debug_str = format_value(&val);
    DEBUG_BUFFER.with(|buf_cell| {
        let mut buf = buf_cell.borrow_mut();
        buf.clear();
        buf.push_str(&debug_str);
        buf.push('\0');
        buf.as_ptr() as *const c_char
    })
}

#[unsafe(no_mangle)] pub extern "C" fn lucia_value_display(v: LuciaValue) -> *const c_char {
    let val = abi_to_value(&v);
    let display_str = val.to_string();
    DISPLAY_BUFFER.with(|buf_cell| {
        let mut buf = buf_cell.borrow_mut();
        buf.clear();
        buf.push_str(&display_str);
        buf.push('\0');
        buf.as_ptr() as *const c_char
    })
}

#[unsafe(no_mangle)] pub extern "C" fn lucia_list_len(list: LuciaValue) -> usize {
    if list.tag != LuciaValueType::VALUE_LIST {
        return 0;
    }
    list.length as usize
}
#[unsafe(no_mangle)] pub extern "C" fn lucia_list_get(list: LuciaValue, index: usize) -> *const LuciaValue {
    if list.tag != LuciaValueType::VALUE_LIST {
        return std::ptr::null();
    }
    if index >= list.length {
        return std::ptr::null();
    }
    unsafe { &*list.data.list_ptr.add(index) as *const LuciaValue }
}
#[unsafe(no_mangle)] pub extern "C" fn lucia_list_try_get(list: LuciaValue, index: usize, out: *mut *const LuciaValue) -> CBool {
    if list.tag != LuciaValueType::VALUE_LIST {
        return 0;
    }
    if index >= list.length {
        return 0;
    }
    unsafe { *out = &*list.data.list_ptr.add(index) as *const LuciaValue; }
    1
}
#[unsafe(no_mangle)] pub extern "C" fn lucia_map_len(map: LuciaValue) -> usize {
    if map.tag != LuciaValueType::VALUE_MAP {
        return 0;
    }
    (map.length / 2) as usize // length is flattened [k,v,k,v,...], so divide by 2 for pair count
}
#[unsafe(no_mangle)] pub extern "C" fn lucia_map_get(map: LuciaValue, key: *const LuciaValue) -> *const LuciaValue {
    if map.tag != LuciaValueType::VALUE_MAP || key.is_null() {
        return std::ptr::null();
    }
    let len = map.length;
    let slice = unsafe { std::slice::from_raw_parts(map.data.map_ptr, len) };
    let key_val = unsafe { *key };
    let mut i = 0;
    while i + 1 < len {
        if lucia_value_cmp(slice[i], key_val) == 0 {
            return &slice[i + 1] as *const LuciaValue;
        }
        i += 2;
    }
    std::ptr::null()
}
#[unsafe(no_mangle)] pub extern "C" fn lucia_map_try_get(map: LuciaValue, key: *const LuciaValue, out: *mut *const LuciaValue) -> CBool {
    if map.tag != LuciaValueType::VALUE_MAP || key.is_null() {
        return 0;
    }
    let len = map.length;
    let slice = unsafe { std::slice::from_raw_parts(map.data.map_ptr, len) };
    let key_val = unsafe { *key };
    let mut i = 0;
    while i + 1 < len {
        if lucia_value_cmp(slice[i], key_val) == 0 {
            unsafe { *out = &slice[i + 1] as *const LuciaValue; }
            return 1;
        }
        i += 2;
    }
    0
}
#[unsafe(no_mangle)] pub extern "C" fn lucia_map_get_cstr(map: LuciaValue, key: *const c_char) -> *const LuciaValue {
    if map.tag != LuciaValueType::VALUE_MAP {
        return std::ptr::null();
    }
    if key.is_null() {
        return std::ptr::null();
    }
    let key_value = lucia_value_string(key);
    let result = lucia_map_get(map, &key_value as *const LuciaValue);
    lucia_free_value(key_value);
    result
}
#[unsafe(no_mangle)] pub extern "C" fn lucia_map_try_get_cstr(map: LuciaValue, key: *const c_char, out: *mut *const LuciaValue) -> CBool {
    if map.tag != LuciaValueType::VALUE_MAP {
        return 0;
    }
    if key.is_null() {
        return 0;
    }
    let key_value = lucia_value_string(key);
    let result = lucia_map_try_get(map, &key_value as *const LuciaValue, out);
    lucia_free_value(key_value);
    result
}


#[unsafe(no_mangle)]
pub extern "C" fn lucia_error_display(err: *const LuciaError) -> *const c_char {
    if err.is_null() {
        return std::ptr::null();
    }

    unsafe {
        let line_text = if !(*err).line_text.is_null() {
            CStr::from_ptr((*err).line_text)
        } else {
            CStr::from_bytes_with_nul_unchecked(b"<null>\0")
        };

        let err_type = if !(*err).err_type.is_null() {
            CStr::from_ptr((*err).err_type)
        } else {
            CStr::from_bytes_with_nul_unchecked(b"<null>\0")
        };

        let err_msg = if !(*err).err_msg.is_null() {
            CStr::from_ptr((*err).err_msg)
        } else {
            CStr::from_bytes_with_nul_unchecked(b"<null>\0")
        };

        let help_msg = if !(*err).help_msg.is_null() {
            Some(CStr::from_ptr((*err).help_msg))
        } else {
            None
        };

        let mut display_str = format!("{}:{}: {}\n - {}: {}\n",
            (*err).line_num,
            (*err).column,
            line_text.to_string_lossy(),
            err_type.to_string_lossy(),
            err_msg.to_string_lossy()
        );

        if let Some(help) = help_msg {
            display_str.push_str(&format!("Help: {}\n", help.to_string_lossy()));
        }
        DISPLAY_BUFFER.with(|buf_cell| {
            let mut buf = buf_cell.borrow_mut();
            buf.clear();
            buf.push_str(&display_str);
            buf.push('\0');
            buf.as_ptr() as *const c_char
        })
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn lucia_error_print(err: *const LuciaError, out: *mut libc::FILE) {
    if err.is_null() || out.is_null() {
        return;
    }

    unsafe {
        libc::fprintf(out, b"%s\0".as_ptr() as *const c_char, lucia_error_display(err));
    }
}

#[unsafe(no_mangle)] pub extern "C" fn lucia_error_type(err: *const LuciaError) -> *const c_char {
    if err.is_null() { return std::ptr::null(); }
    unsafe { (*err).err_type }
}
#[unsafe(no_mangle)] pub extern "C" fn lucia_error_message(err: *const LuciaError) -> *const c_char {
    if err.is_null() { return std::ptr::null(); }
    unsafe { (*err).err_msg }
}
#[unsafe(no_mangle)] pub extern "C" fn lucia_error_help(err: *const LuciaError) -> *const c_char {
    if err.is_null() { return std::ptr::null(); }
    unsafe { (*err).help_msg }
}
#[unsafe(no_mangle)] pub extern "C" fn lucia_error_location(err: *const LuciaError) -> *const c_char {
    if err.is_null() { return std::ptr::null(); }
    unsafe {
        let location_str = format!("{}:{}", (*err).line_num, (*err).column);
        LOCATION_BUFFER.with(|buf_cell| {
            let mut buf = buf_cell.borrow_mut();
            buf.clear();
            buf.push_str(&location_str);
            buf.push('\0');
            buf.as_ptr() as *const c_char
        })
    }
}

#[unsafe(no_mangle)] pub extern "C" fn value_as_int(v: LuciaValue) -> i64 { unsafe { v.data.int_v } }
#[unsafe(no_mangle)] pub extern "C" fn value_as_float(v: LuciaValue) -> f64 { unsafe { v.data.float_v } }
#[unsafe(no_mangle)] pub extern "C" fn value_as_bool(v: LuciaValue) -> u8 { unsafe { v.data.bool_v } }
#[unsafe(no_mangle)] pub extern "C" fn value_as_string(v: LuciaValue) -> *const c_char { unsafe { v.data.string_v } }
#[unsafe(no_mangle)] pub extern "C" fn value_as_bytes_ptr(v: LuciaValue) -> *const u8 { unsafe { v.data.bytes_ptr } }
#[unsafe(no_mangle)] pub extern "C" fn value_as_bytes_len(v: LuciaValue) -> usize { v.length }
#[unsafe(no_mangle)] pub extern "C" fn value_as_list_ptr(v: LuciaValue) -> *const LuciaValue { unsafe { v.data.list_ptr } }
#[unsafe(no_mangle)] pub extern "C" fn value_as_list_len(v: LuciaValue) -> usize { v.length }
#[unsafe(no_mangle)] pub extern "C" fn value_as_map_ptr(v: LuciaValue) -> *const LuciaValue { unsafe { v.data.map_ptr } }
#[unsafe(no_mangle)] pub extern "C" fn value_as_map_len(v: LuciaValue) -> usize {  v.length }
#[unsafe(no_mangle)] pub extern "C" fn value_as_pointer(v: LuciaValue) -> *mut c_void { unsafe { v.data.pointer } }

#[unsafe(no_mangle)] pub extern "C" fn try_value_as_int(v: LuciaValue, out: *mut i64) -> CBool {
    if v.tag == LuciaValueType::VALUE_INT {
        unsafe { *out = v.data.int_v; }
        1
    } else {
        0
    }
}
#[unsafe(no_mangle)] pub extern "C" fn try_value_as_float(v: LuciaValue, out: *mut f64) -> CBool {
    if v.tag == LuciaValueType::VALUE_FLOAT {
        unsafe { *out = v.data.float_v; }
        1
    } else {
        0
    }
}
#[unsafe(no_mangle)] pub extern "C" fn try_value_as_bool(v: LuciaValue, out: *mut CBool) -> CBool {
    if v.tag == LuciaValueType::VALUE_BOOLEAN {
        unsafe { *out = v.data.bool_v; }
        1
    } else {
        0
    }
}
#[unsafe(no_mangle)] pub extern "C" fn try_value_as_string(v: LuciaValue, out: *mut *const c_char, out_len: *mut usize) -> CBool {
    if v.tag == LuciaValueType::VALUE_STRING {
        unsafe {
            *out = v.data.string_v;
            *out_len = v.length;
        }
        1
    } else {
        0
    }
}
#[unsafe(no_mangle)] pub extern "C" fn try_value_as_bytes(v: LuciaValue, out_ptr: *mut *const u8, out_len: *mut usize) -> CBool {
    if v.tag == LuciaValueType::VALUE_BYTES {
        unsafe {
            *out_ptr = v.data.bytes_ptr;
            *out_len = v.length;
        }
        1
    } else {
        0
    }
}
#[unsafe(no_mangle)] pub extern "C" fn try_value_as_list(v: LuciaValue, out_ptr: *mut *const LuciaValue, out_len: *mut usize) -> CBool {
    if v.tag == LuciaValueType::VALUE_LIST {
        unsafe {
            *out_ptr = v.data.list_ptr;
            *out_len = v.length;
        }
        1
    } else {
        0
    }
}
#[unsafe(no_mangle)] pub extern "C" fn try_value_as_map(v: LuciaValue, out_ptr: *mut *const LuciaValue, out_len: *mut usize) -> CBool {
    if v.tag == LuciaValueType::VALUE_MAP {
        unsafe {
            *out_ptr = v.data.map_ptr;
            *out_len = v.length;
        }
        1
    } else {
        0
    }

}
#[unsafe(no_mangle)] pub extern "C" fn try_value_as_pointer(v: LuciaValue, out: *mut *mut c_void) -> CBool {
    if v.tag == LuciaValueType::VALUE_POINTER {
        unsafe { *out = v.data.pointer; }
        1
    } else {
        0
    }
}

#[repr(C)]
pub struct ValueAsArgs {
    pub force: CBool,
    pub cast: CBool,
}

// #define value_as(v, t, out, ...) lucia__value_as_args(v, t, out, ValueAsArgs { __VA_ARGS__ })
#[unsafe(no_mangle)] pub extern "C" fn lucia__value_as_args(
    v: LuciaValue,
    t: LuciaValueType,
    out: *mut c_void,
    args: ValueAsArgs,
) -> CBool {
    if out.is_null() {
        return 0;
    }

    unsafe {
        if v.tag == t {
            match t {
                LuciaValueType::VALUE_INT => {
                    *(out as *mut i64) = v.data.int_v;
                }
                LuciaValueType::VALUE_FLOAT => {
                    *(out as *mut f64) = v.data.float_v;
                }
                LuciaValueType::VALUE_BOOLEAN => {
                    *(out as *mut CBool) = v.data.bool_v;
                }
                LuciaValueType::VALUE_STRING => {
                    *(out as *mut *const c_char) = v.data.string_v;
                }
                LuciaValueType::VALUE_BYTES => {
                    *(out as *mut *const u8) = v.data.bytes_ptr;
                }
                LuciaValueType::VALUE_LIST => {
                    *(out as *mut *const LuciaValue) = v.data.list_ptr;
                }
                LuciaValueType::VALUE_MAP => {
                    *(out as *mut *const LuciaValue) = v.data.map_ptr;
                }
                LuciaValueType::VALUE_POINTER => {
                    *(out as *mut *mut c_void) = v.data.pointer;
                }
                _ => return 0,
            }
            return 1;
        }

        if args.cast != 0 {
            match (v.tag, t) {
                (LuciaValueType::VALUE_INT, LuciaValueType::VALUE_FLOAT) => {
                    *(out as *mut f64) = v.data.int_v as f64;
                    return 1;
                }
                (LuciaValueType::VALUE_FLOAT, LuciaValueType::VALUE_INT) => {
                    *(out as *mut i64) = v.data.float_v as i64;
                    return 1;
                }
                (LuciaValueType::VALUE_BOOLEAN, LuciaValueType::VALUE_INT) => {
                    *(out as *mut i64) = v.data.bool_v as i64;
                    return 1;
                }
                (LuciaValueType::VALUE_INT, LuciaValueType::VALUE_BOOLEAN) => {
                    *(out as *mut CBool) = (v.data.int_v != 0) as CBool;
                    return 1;
                }
                _ => {}
            }
        }

        if args.force != 0 {
            match t {
                LuciaValueType::VALUE_INT => {
                    *(out as *mut i64) = 0;
                }
                LuciaValueType::VALUE_FLOAT => {
                    *(out as *mut f64) = 0.0;
                }
                LuciaValueType::VALUE_BOOLEAN => {
                    *(out as *mut CBool) = 0;
                }
                LuciaValueType::VALUE_STRING => {
                    *(out as *mut *const c_char) = std::ptr::null();
                }
                LuciaValueType::VALUE_BYTES => {
                    *(out as *mut *const u8) = std::ptr::null();
                }
                LuciaValueType::VALUE_LIST | LuciaValueType::VALUE_MAP => {
                    *(out as *mut *const LuciaValue) = std::ptr::null();
                }
                LuciaValueType::VALUE_POINTER => {
                    *(out as *mut *mut c_void) = std::ptr::null_mut();
                }
                _ => return 0,
            }
            return 1;
        }
    }

    0
}

#[repr(C)]
pub struct LuciaError {
    pub err_type: *const c_char,
    pub err_msg: *const c_char,
    pub help_msg: *const c_char,
    pub line_num: u32,
    pub line_text: *const c_char,
    pub column: usize,
}

#[repr(u8)]
#[derive(Copy, Clone)]
#[allow(non_camel_case_types)]
pub enum LuciaResultTag {
    LUCIA_RESULT_OK = 1,
    LUCIA_RESULT_ERROR = 2,
    LUCIA_RESULT_CONFIG_ERR = 3,
    LUCIA_RESULT_PANIC = 4,
    LUCIA_RESULT_INTERRUPT = 5,
}

#[repr(C)]
pub struct LuciaResult {
    pub tag: LuciaResultTag,
    pub data: LuciaResultData,
}

#[repr(C)]
pub union LuciaResultData {
    pub value: LuciaValue,
    pub error: ManuallyDrop<LuciaError>,
    pub panic_msg: *const c_char,
    pub interrupt_msg: *const c_char,
}

#[unsafe(no_mangle)]
pub extern "C" fn lucia_result_is_ok(res: *const LuciaResult) -> CBool {
    if res.is_null() {
        return 0;
    }
    unsafe {
        match (*res).tag {
            LuciaResultTag::LUCIA_RESULT_OK => 1,
            _ => 0,
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn lucia_result_is_error(res: *const LuciaResult) -> CBool {
    if res.is_null() {
        return 0;
    }
    unsafe {
        match (*res).tag {
            LuciaResultTag::LUCIA_RESULT_ERROR => 1,
            _ => 0,
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn lucia_result_value(res: *const LuciaResult) -> *const LuciaValue {
    if res.is_null() {
        return std::ptr::null();
    }

    unsafe {
        match (*res).tag {
            LuciaResultTag::LUCIA_RESULT_OK => {
                &(*res).data.value as *const LuciaValue
            }
            _ => std::ptr::null(),
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn lucia_result_error(res: *const LuciaResult) -> *const LuciaError {
    if res.is_null() {
        return std::ptr::null();
    }

    unsafe {
        match (*res).tag {
            LuciaResultTag::LUCIA_RESULT_ERROR => {
                &(*res).data.error as *const ManuallyDrop<LuciaError> as *const LuciaError
            }
            _ => std::ptr::null(),
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn lucia_result_try_as_value(res: *const LuciaResult, value: *mut *const LuciaValue) -> CBool {
    if res.is_null() {
        return 0;
    }

    unsafe {
        match (*res).tag {
            LuciaResultTag::LUCIA_RESULT_OK => {
                *value = &(*res).data.value as *const LuciaValue;
                1
            }
            _ => 0,
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn lucia_result_try_as_error(res: *const LuciaResult, error: *mut *const LuciaError) -> CBool {
    if res.is_null() {
        return 0;
    }

    unsafe {
        match (*res).tag {
            LuciaResultTag::LUCIA_RESULT_ERROR => {
                *error = &(*res).data.error as *const ManuallyDrop<LuciaError> as *const LuciaError;
                1
            }
            _ => 0,
        }
    }
}

#[unsafe(no_mangle)] pub extern "C" fn lucia_result_display(res: LuciaResult) -> *const c_char {
    match res.tag {
        LuciaResultTag::LUCIA_RESULT_OK => {
            lucia_value_display(unsafe { res.data.value })
        },
        LuciaResultTag::LUCIA_RESULT_ERROR => {
            lucia_error_display(unsafe { &*res.data.error as *const _ })
        },
        LuciaResultTag::LUCIA_RESULT_CONFIG_ERR => {
            b"Invalid config data (Config Error)\0".as_ptr() as *const c_char
        },
        LuciaResultTag::LUCIA_RESULT_PANIC => {
            unsafe { res.data.panic_msg }
        },
        LuciaResultTag::LUCIA_RESULT_INTERRUPT => {
            unsafe { res.data.interrupt_msg }
        },
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn lucia_new_result_value(v: LuciaValue) -> LuciaResult {
    LuciaResult {
        tag: LuciaResultTag::LUCIA_RESULT_OK,
        data: LuciaResultData { value: v },
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn lucia_new_result_error(err_type: *const c_char, err_msg: *const c_char) -> LuciaResult {
    let err = LuciaError {
        err_type,
        err_msg,
        help_msg: std::ptr::null(),
        line_num: 0,
        line_text: std::ptr::null(),
        column: 0,
    };
    LuciaResult {
        tag: LuciaResultTag::LUCIA_RESULT_ERROR,
        data: LuciaResultData { error: ManuallyDrop::new(err) },
    }
}

#[unsafe(no_mangle)]
pub static BUILD_INFO: BuildInfo = BuildInfo {
    name:          concat!(env!("CARGO_PKG_NAME"), "\0").as_ptr() as *const c_char,
    version:       concat!(env!("VERSION"), "\0").as_ptr() as *const c_char,
    uuid:          concat!(env!("BUILD_UUID"), "\0").as_ptr() as *const c_char,
    rustc_version: concat!(env!("RUSTC_VERSION"), "\0").as_ptr() as *const c_char,
    rustc_channel: concat!(env!("RUSTC_CHANNEL"), "\0").as_ptr() as *const c_char,
    target:        concat!(env!("TARGET_TRIPLE"), "\0").as_ptr() as *const c_char,
    repository:    concat!(env!("REPO"), "\0").as_ptr() as *const c_char,
    git_hash:      concat!(env!("GIT_HASH"), "\0").as_ptr() as *const c_char,
    file_hash:     concat!(env!("FILE_HASH"), "\0").as_ptr() as *const c_char,
    profile:       concat!(env!("PROFILE"), "\0").as_ptr() as *const c_char,
    build_date:    concat!(env!("BUILD_DATE_ISO"), "\0").as_ptr() as *const c_char,
};

#[unsafe(no_mangle)]
pub extern "C" fn lucia_get_build_info() -> *const BuildInfo {
    &BUILD_INFO
}

#[unsafe(no_mangle)]
pub extern "C" fn lucia_free_value(v: LuciaValue) {
    unsafe {
        match v.tag {
            LuciaValueType::VALUE_STRING => {
                if !v.data.string_v.is_null() {
                    let _ = CString::from_raw(v.data.string_v as *mut c_char);
                }
            }

            LuciaValueType::VALUE_LIST | LuciaValueType::VALUE_MAP => {
                let ptr = v.data.list_ptr as *mut LuciaValue;
                let len = v.length;
                if !ptr.is_null() {
                    let slice = std::slice::from_raw_parts_mut(ptr, len);
                    for item in slice {
                        lucia_free_value(*item);
                    }
                    Vec::from_raw_parts(ptr, len, len);
                }
            }

            LuciaValueType::VALUE_BYTES => {
                let ptr = v.data.bytes_ptr as *mut u8;
                let len = v.length;
                if !ptr.is_null() {
                    Vec::from_raw_parts(ptr, len, len);
                }
            }

            LuciaValueType::VALUE_POINTER => {
                if !v.data.pointer.is_null() {
                    let _ = Arc::from_raw(v.data.pointer as *const Mutex<(Value, usize)>);
                }
            }

            _ => {}
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn lucia_free_error(err: LuciaError) {
    let err_type_ptr = err.err_type as *mut c_char;
    if !err_type_ptr.is_null() {
        unsafe { let _ = CString::from_raw(err_type_ptr); }
    }
    let err_msg_ptr = err.err_msg as *mut c_char;
    if !err_msg_ptr.is_null() {
        unsafe { let _ = CString::from_raw(err_msg_ptr); }
    }
    let help_msg_ptr = err.help_msg as *mut c_char;
    if !help_msg_ptr.is_null() {
        unsafe { let _ = CString::from_raw(help_msg_ptr); }
    }
    let line_text_ptr = err.line_text as *mut c_char;
    if !line_text_ptr.is_null() {
        unsafe { let _ = CString::from_raw(line_text_ptr); }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn lucia_free_config(cfg: LuciaConfig) {
    let debug_mode_ptr = cfg.debug_mode as *mut c_char;
    if !debug_mode_ptr.is_null() {
        unsafe { let _ = CString::from_raw(debug_mode_ptr); }
    }
    let home_dir_ptr = cfg.home_dir as *mut c_char;
    if !home_dir_ptr.is_null() {
        unsafe { let _ = CString::from_raw(home_dir_ptr); }
    }
    let libs_paths = cfg.libs_paths;
    for path_ptr in libs_paths.iter() {
        let p = *path_ptr as *mut c_char;
        if !p.is_null() {
            unsafe { let _ = CString::from_raw(p); }
        }
    }
    let version_ptr = cfg.version as *mut c_char;
    if !version_ptr.is_null() {
        unsafe { let _ = CString::from_raw(version_ptr); }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn lucia_free_result(mut res: LuciaResult) {
    match res.tag {
        LuciaResultTag::LUCIA_RESULT_OK => {
            let value = unsafe { res.data.value };
            lucia_free_value(value);
        }
        LuciaResultTag::LUCIA_RESULT_ERROR => {
            let error = unsafe { ManuallyDrop::take(&mut res.data.error) };
            lucia_free_error(error);
        }
        LuciaResultTag::LUCIA_RESULT_PANIC => {
            let panic_msg_ptr = unsafe { res.data.panic_msg as *mut c_char };
            if !panic_msg_ptr.is_null() {
                unsafe { let _ = CString::from_raw(panic_msg_ptr); }
            }
        }
        LuciaResultTag::LUCIA_RESULT_INTERRUPT => {
            let interrupt_msg_ptr = unsafe { res.data.interrupt_msg as *mut c_char };
            if !interrupt_msg_ptr.is_null() {
                unsafe { let _ = CString::from_raw(interrupt_msg_ptr); }
            }
        }
        _ => {}
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn lucia_variables_new(capacity: usize) -> *mut LuciaVariables {
    let layout = std::alloc::Layout::new::<LuciaVariables>()
        .extend(std::alloc::Layout::new::<LuciaVariablesInner>())
        .expect("layout extend failed")
        .0
        .pad_to_align();

    let ptr = unsafe { std::alloc::alloc(layout) };
    if ptr.is_null() {
        std::alloc::handle_alloc_error(layout);
    }

    let vars_ptr = ptr as *mut LuciaVariables;
    let inner_ptr = unsafe { ptr.add(std::mem::size_of::<LuciaVariables>()) as *mut LuciaVariablesInner };

    unsafe {
        std::ptr::write(inner_ptr, LuciaVariablesInner {
            keys: Vec::with_capacity(capacity),
            values: Vec::with_capacity(capacity),
            include_default: 0,
        });

        std::ptr::write(vars_ptr, LuciaVariables {
            keys: std::ptr::null(),
            values: std::ptr::null(),
            len: 0,
            capacity,
            include_default: 0,
        });
    }

    vars_ptr
}

#[unsafe(no_mangle)]
pub extern "C" fn lucia_variables_new_default() -> *mut LuciaVariables {
    let v = lucia_variables_new(0);
    unsafe { (*v).include_default = 1; }
    let inner_ptr = unsafe { (v as *mut u8).add(std::mem::size_of::<LuciaVariables>()) as *mut LuciaVariablesInner };
    unsafe { (*inner_ptr).include_default = 1; }
    v
}

unsafe fn lucia_variables_rebuild_ptrs(vars: *mut LuciaVariables) {
    let inner_ptr = unsafe { (vars as *mut u8)
        .add(std::mem::size_of::<LuciaVariables>()) 
        as *mut LuciaVariablesInner };

    let inner = unsafe { &*inner_ptr };

    let key_ptrs: Vec<*const c_char> = inner.keys.iter().map(|s| s.as_ptr()).collect();
    let len = key_ptrs.len();
    let boxed = key_ptrs.into_boxed_slice();
    let keys_ptr = boxed.as_ptr();
    std::mem::forget(boxed);

    let vars_ref: &mut LuciaVariables = unsafe { &mut *vars };

    vars_ref.keys = keys_ptr;
    vars_ref.values = if inner.values.is_empty() { std::ptr::null() } else { inner.values.as_ptr() };
    vars_ref.len = len;
    vars_ref.capacity = inner.keys.capacity();
}

#[unsafe(no_mangle)]
pub extern "C" fn lucia_variables_insert(
    vars: *mut LuciaVariables,
    key: *const c_char,
    value: LuciaValue,
) {
    if vars.is_null() || key.is_null() {
        return;
    }

    let key_str = unsafe { CStr::from_ptr(key) };
    let key_owned = match key_str.to_str() {
        Ok(s) => s.to_string(),
        Err(_) => return,
    };

    unsafe {
        let inner_ptr = (vars as *mut u8)
            .add(std::mem::size_of::<LuciaVariables>())
            as *mut LuciaVariablesInner;

        for (i, existing_key) in (&(*inner_ptr).keys).iter().enumerate() {
            if existing_key.to_str().unwrap_or("") == key_owned.as_str() {
                let old = (&(*inner_ptr).values)[i];
                lucia_free_value(old);

                (&mut (*inner_ptr).values)[i] = value;

                lucia_variables_rebuild_ptrs(vars);
                return;
            }
        }

        if let Ok(cstr) = CString::new(key_owned) {
            (&mut (*inner_ptr).keys).push(cstr);
            (&mut (*inner_ptr).values).push(value);
        }

        lucia_variables_rebuild_ptrs(vars);
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn lucia_variables_insert_function(vars: *mut LuciaVariables, key: *const c_char, func: LuciaNativeFunc) {
    let value = LuciaValue {
        tag: LuciaValueType::VALUE_NATIVE,
        data: ValueData { native_func: func as *mut c_void },
        length: 0,
    };
    lucia_variables_insert(vars, key, value);
}


#[unsafe(no_mangle)]
pub extern "C" fn lucia_variables_remove(vars: *mut LuciaVariables, key: *const c_char) {
    if vars.is_null() || key.is_null() {
        return;
    }

    let key_str = unsafe { CStr::from_ptr(key) };
    let key_s = match key_str.to_str() {
        Ok(s) => s,
        Err(_) => return,
    };

    unsafe {
        let inner_ptr = (vars as *mut u8)
            .add(std::mem::size_of::<LuciaVariables>())
            as *mut LuciaVariablesInner;

        if let Some(pos) = (&(*inner_ptr).keys).iter().position(|k| k.to_str().unwrap_or("") == key_s) {
            let old_value = (&(*inner_ptr).values)[pos];
            lucia_free_value(old_value);

            (&mut (*inner_ptr).keys).remove(pos);
            (&mut (*inner_ptr).values).remove(pos);

            lucia_variables_rebuild_ptrs(vars);
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn lucia_variables_clear(vars: *mut LuciaVariables) {
    if vars.is_null() {
        return;
    }

    unsafe {
        let inner_ptr = (vars as *mut u8)
            .add(std::mem::size_of::<LuciaVariables>())
            as *mut LuciaVariablesInner;

        let inner = &mut *inner_ptr;

        for val in &inner.values {
            lucia_free_value(*val);
        }

        inner.keys.clear();
        inner.values.clear();

        lucia_variables_rebuild_ptrs(vars);
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn lucia_variables_get(
    vars: *const LuciaVariables,
    key: *const c_char,
    out: *mut *const LuciaValue,
) -> CBool {
    if vars.is_null() || key.is_null() || out.is_null() {
        return 0;
    }

    let key_str = unsafe { CStr::from_ptr(key) };
    let key_s = match key_str.to_str() {
        Ok(s) => s,
        Err(_) => return 0,
    };

    unsafe {
        let inner_ptr = (vars as *const u8)
            .add(std::mem::size_of::<LuciaVariables>())
            as *const LuciaVariablesInner;

        for (i, existing_key) in (&(*inner_ptr).keys).iter().enumerate() {
            if existing_key.to_str().unwrap_or("") == key_s {
                *out = &(&(*inner_ptr).values)[i] as *const LuciaValue;
                return 1;
            }
        }
    }

    0
}

#[unsafe(no_mangle)]
pub extern "C" fn lucia_variables_get_or_default(vars: *const LuciaVariables, key: *const c_char, default_value: LuciaValue) -> LuciaValue {
    let mut out: *const LuciaValue = std::ptr::null();
    if lucia_variables_get(vars, key, &mut out) != 0 && !out.is_null() {
        unsafe { *out }
    } else {
        default_value
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn lucia_variables_free(vars: *mut LuciaVariables) {
    if vars.is_null() {
        return;
    }

    unsafe {
        let vars_ref = &*vars;
        if !vars_ref.keys.is_null() && vars_ref.len > 0 {
            let _ = Box::from_raw(std::slice::from_raw_parts_mut(
                vars_ref.keys as *mut *const c_char,
                vars_ref.len,
            ));
        }

        let inner_ptr = (vars as *mut u8).add(std::mem::size_of::<LuciaVariables>()) as *mut LuciaVariablesInner;
        let inner = std::ptr::read(inner_ptr);
        for val in inner.values {
            lucia_free_value(val);
        }

        let layout = std::alloc::Layout::new::<LuciaVariables>()
            .extend(std::alloc::Layout::new::<LuciaVariablesInner>())
            .expect("layout extend failed")
            .0
            .pad_to_align();
        std::alloc::dealloc(vars as *mut u8, layout);
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn lucia_args_get(args: *const LuciaArgs, out: *mut *const LuciaValue, index: usize) -> CBool {
    if args.is_null() || out.is_null() {
        return 0;
    }
    unsafe {
        if index >= (*args).len || (*args).values.is_null() {
            return 0;
        }
        *out = (*args).values.add(index);
        1
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn lucia_args_get_or_default(args: *const LuciaArgs, index: usize, default_value: LuciaValue) -> LuciaValue {
    let mut out: *const LuciaValue = std::ptr::null();
    if lucia_args_get(args, &mut out, index) != 0 && !out.is_null() {
        unsafe { *out }
    } else {
        default_value
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn lucia_args_len(args: *const LuciaArgs) -> usize {
    if args.is_null() {
        return 0;
    }
    unsafe { (*args).len }
}

// very useful
#[unsafe(no_mangle)]
pub extern "C" fn lucia_print_size_checks(out: *mut libc::FILE) {
    use std::mem::{size_of, align_of};

    macro_rules! output {
        ($($arg:tt)*) => {{
            unsafe {
                use std::ffi::CString;
                let s = format!($($arg)*);
                let cstr = CString::new(s).expect("CString::new failed");
                libc::fputs(cstr.as_ptr(), out);
                libc::fputs("\n\0".as_ptr() as *const c_char, out);
            }
        }};
    }

    macro_rules! static_assert_type {
        ($t:ty) => {
            if stringify!($t) == "usize" || stringify!($t).starts_with("*") {
                output!("#if UINTPTR_MAX == 0xffffffffffffffff"); // 64-bit
                output!("LUCIA_STATIC_ASSERT(sizeof({t}) == 8, \"{t} expected to be 8 bytes on 64-bit\");", t=stringify!($t));
                output!("LUCIA_STATIC_ASSERT(alignof({t}) == 8, \"{t} alignment expected to be 8 on 64-bit\");", t=stringify!($t));
                output!("#else"); // 32-bit
                output!("LUCIA_STATIC_ASSERT(sizeof({t}) == 4, \"{t} expected to be 4 bytes on 32-bit\");", t=stringify!($t));
                output!("LUCIA_STATIC_ASSERT(alignof({t}) == 4, \"{t} alignment expected to be 4 on 32-bit\");", t=stringify!($t));
                output!("#endif");
            } else {
                output!("LUCIA_STATIC_ASSERT(sizeof({t}) == {s}, \"Size of {t} was expected to be {s} byte{plural}\");", t=stringify!($t), s=size_of::<$t>(), plural=if size_of::<$t>() == 1 { "" } else { "s" });
                output!("LUCIA_STATIC_ASSERT(alignof({t}) == {a}, \"Alignment of {t} was expected to be {a}\");", t=stringify!($t), a=align_of::<$t>());
            }
        };
    }

    macro_rules! static_assert_field {
        ($struct:ty, $field:ident, $ty:ty) => {
            if stringify!($ty) == "usize" || stringify!($ty).starts_with("*") {
                output!("#if UINTPTR_MAX == 0xffffffffffffffff"); // 64-bit
                output!("LUCIA_STATIC_ASSERT(sizeof((({s}*)0)->{f}) == 8, \"{s}.{f} expected to be 8 bytes on 64-bit\");", s=stringify!($struct), f=stringify!($field));
                output!("#ifndef _MSC_VER");
                output!("LUCIA_STATIC_ASSERT(alignof((({s}*)0)->{f}) == 8, \"{s}.{f} alignment expected to be 8 on 64-bit\");", s=stringify!($struct), f=stringify!($field));
                output!("#endif");
                output!("#else"); // 32-bit
                output!("LUCIA_STATIC_ASSERT(sizeof((({s}*)0)->{f}) == 4, \"{s}.{f} expected to be 4 bytes on 32-bit\");", s=stringify!($struct), f=stringify!($field));
                output!("#ifndef _MSC_VER");
                output!("LUCIA_STATIC_ASSERT(alignof((({s}*)0)->{f}) == 4, \"{s}.{f} alignment expected to be 4 on 32-bit\");", s=stringify!($struct), f=stringify!($field));
                output!("#endif");
                output!("#endif");
            } else {
                output!("LUCIA_STATIC_ASSERT(sizeof((({s}*)0)->{f}) == {sz}, \"{s}.{f} was expected to be {sz} byte{plural}\");", s=stringify!($struct), f=stringify!($field), sz=size_of::<$ty>(), plural=if size_of::<$ty>() == 1 { "" } else { "s" });
                output!("#ifndef _MSC_VER");
                output!("LUCIA_STATIC_ASSERT(alignof((({s}*)0)->{f}) == {a}, \"{s}.{f} alignment was expected to be {a}\");", s=stringify!($struct), f=stringify!($field), a=align_of::<$ty>());
                output!("#endif");
            }
        };
    }

    // top-level types
    static_assert_type!(CBool);
    static_assert_type!(LuciaValueType);
    static_assert_type!(LuciaResultTag);
    static_assert_type!(ValueData);
    static_assert_type!(LuciaResultData);
    static_assert_type!(BuildInfo);
    static_assert_type!(LuciaConfig);
    static_assert_type!(LuciaValue);
    static_assert_type!(LuciaError);
    static_assert_type!(LuciaResult);
    static_assert_type!(LuciaVariables);
    static_assert_type!(LuciaArgs);
    static_assert_type!(ValueAsArgs);
    static_assert_type!(InterruptArgs);

    // ValueData fields
    static_assert_field!(ValueData, int_v, i64);
    static_assert_field!(ValueData, float_v, f64);
    static_assert_field!(ValueData, bool_v, CBool);
    static_assert_field!(ValueData, string_v, *const c_char);
    static_assert_field!(ValueData, bytes_ptr, *const u8);
    static_assert_field!(ValueData, list_ptr, *const LuciaValue);
    static_assert_field!(ValueData, map_ptr, *const LuciaValue);
    static_assert_field!(ValueData, pointer, *mut c_void);

    // LuciaResultData fields
    static_assert_field!(LuciaResultData, value, LuciaValue);
    static_assert_field!(LuciaResultData, error, ManuallyDrop<LuciaError>);
    static_assert_field!(LuciaResultData, panic_msg, *const c_char);

    // BuildInfo fields
    static_assert_field!(BuildInfo, name, *const c_char);
    static_assert_field!(BuildInfo, version, *const c_char);
    static_assert_field!(BuildInfo, uuid, *const c_char);
    static_assert_field!(BuildInfo, rustc_version, *const c_char);
    static_assert_field!(BuildInfo, rustc_channel, *const c_char);
    static_assert_field!(BuildInfo, target, *const c_char);
    static_assert_field!(BuildInfo, repository, *const c_char);
    static_assert_field!(BuildInfo, git_hash, *const c_char);
    static_assert_field!(BuildInfo, file_hash, *const c_char);
    static_assert_field!(BuildInfo, profile, *const c_char);
    static_assert_field!(BuildInfo, build_date, *const c_char);

    // LuciaConfig fields
    static_assert_field!(LuciaConfig, moded, CBool);
    static_assert_field!(LuciaConfig, debug, CBool);
    static_assert_field!(LuciaConfig, debug_mode, *const c_char);
    static_assert_field!(LuciaConfig, supports_color, CBool);
    static_assert_field!(LuciaConfig, use_lucia_traceback, CBool);
    static_assert_field!(LuciaConfig, warnings, CBool);
    static_assert_field!(LuciaConfig, allow_fetch, CBool);
    static_assert_field!(LuciaConfig, allow_unsafe, CBool);
    static_assert_field!(LuciaConfig, allow_inline_config, CBool);
    static_assert_field!(LuciaConfig, disable_runtime_type_checking, CBool);
    static_assert_field!(LuciaConfig, home_dir, *const c_char);
    static_assert_field!(LuciaConfig, libs_paths, [*const c_char; 16]);
    static_assert_field!(LuciaConfig, stack_size, usize);
    static_assert_field!(LuciaConfig, version, *const c_char);

    // LuciaValue fields
    static_assert_field!(LuciaValue, tag, LuciaValueType);
    static_assert_field!(LuciaValue, data, ValueData);
    static_assert_field!(LuciaValue, length, usize);

    // LuciaError fields
    static_assert_field!(LuciaError, err_type, *const c_char);
    static_assert_field!(LuciaError, err_msg, *const c_char);
    static_assert_field!(LuciaError, help_msg, *const c_char);
    static_assert_field!(LuciaError, line_num, u32);
    static_assert_field!(LuciaError, line_text, *const c_char);
    static_assert_field!(LuciaError, column, usize);

    // LuciaResult fields
    static_assert_field!(LuciaResult, tag, LuciaResultTag);
    static_assert_field!(LuciaResult, data, LuciaResultData);

    // LuciaVariables fields
    static_assert_field!(LuciaVariables, keys, *const *const c_char);
    static_assert_field!(LuciaVariables, values, *const LuciaValue);
    static_assert_field!(LuciaVariables, len, usize);
    static_assert_field!(LuciaVariables, capacity, usize);
    static_assert_field!(LuciaVariables, include_default, CBool);

    // LuciaArgs fields
    static_assert_field!(LuciaArgs, values, *const LuciaValue);
    static_assert_field!(LuciaArgs, len, usize);

    // ValueAsArgs fields
    static_assert_field!(ValueAsArgs, force, CBool);
    static_assert_field!(ValueAsArgs, cast, CBool);

    // InterruptArgs fields
    static_assert_field!(InterruptArgs, msg, *const c_char);
    static_assert_field!(InterruptArgs, all, CBool);
    static_assert_field!(InterruptArgs, last_thread, CBool);
    static_assert_field!(InterruptArgs, thread, u64);
    static_assert_field!(InterruptArgs, cancel, CBool);
}

#[unsafe(no_mangle)]
pub extern "C" fn lucia_default_config() -> LuciaConfig {
    let default_cfg = Config::default();

    LuciaConfig {
        moded: default_cfg.moded as CBool,
        debug: default_cfg.debug as CBool,
        debug_mode: CString::new(default_cfg.debug_mode).unwrap().into_raw(),
        supports_color: default_cfg.supports_color as CBool,
        use_lucia_traceback: default_cfg.use_lucia_traceback as CBool,
        warnings: default_cfg.warnings as CBool,
        allow_fetch: default_cfg.allow_fetch as CBool,
        allow_unsafe: default_cfg.allow_unsafe as CBool,
        allow_inline_config: default_cfg.allow_inline_config as CBool,
        disable_runtime_type_checking: default_cfg.disable_runtime_type_checking as CBool,
        home_dir: CString::new(default_cfg.home_dir).unwrap().into_raw(),
        libs_paths: {
            let mut arr = [std::ptr::null(); 16];
            for (i, path) in default_cfg.libs_paths.into_iter().enumerate().take(16) {
                arr[i] = CString::new(path).unwrap().into_raw();
            }
            arr
        },
        stack_size: default_cfg.stack_size,
        version: CString::new(default_cfg.version).unwrap().into_raw()
    }
}

#[cfg(windows)]
#[link(name = "kernel32")]
unsafe extern "system" {
    unsafe fn GetCurrentThreadId() -> u32;
}

fn current_thread_id() -> u64 {
    #[cfg(windows)]
    unsafe {
        GetCurrentThreadId() as u64
    }

    #[cfg(unix)]
    unsafe {
        libc::pthread_self() as u64
    }
}

static LAST_THREAD_ID: AtomicU64 = AtomicU64::new(0);
static INTERRUPTS: Lazy<Mutex<HashMap<u64, (Option<String>, Arc<AtomicBool>)>>> = Lazy::new(|| Mutex::new(HashMap::new()));

#[repr(C)]
pub struct InterruptArgs {
    pub msg: *const c_char,
    pub all: CBool,
    pub last_thread: CBool,
    pub thread: u64,
    pub cancel: CBool,
}

#[unsafe(no_mangle)]
pub extern "C" fn lucia__lucia_interrupt(args: InterruptArgs) {
    let msg = if args.msg.is_null() {
        None
    } else {
        unsafe { CStr::from_ptr(args.msg).to_str().ok().map(|s| s.to_owned()) }
    };

    let mut map = INTERRUPTS.lock();

    let apply = |entry: &mut (Option<String>, Arc<AtomicBool>)| {
        if args.cancel != 0 {
            entry.1.store(false, Ordering::Release);
            entry.0 = None;
        } else {
            entry.1.store(true, Ordering::Release);
            if msg.is_some() {
                entry.0 = msg.clone();
            }
        }
    };

    if args.all != 0 {
        for entry in map.values_mut() {
            apply(entry);
        }
        return;
    }

    if args.thread != 0 {
        if let Some(entry) = map.get_mut(&args.thread) {
            apply(entry);
        }
        return;
    }

    if args.last_thread != 0 {
        let id = LAST_THREAD_ID.load(Ordering::Acquire);
        if id != 0 {
            let entry = map.entry(id).or_insert_with(|| (None, Arc::new(AtomicBool::new(false))));
            apply(entry);
        }
        return;
    }

    let id = current_thread_id();
    let entry = map.entry(id).or_insert_with(|| (None, Arc::new(AtomicBool::new(false))));
    apply(entry);
}

#[unsafe(no_mangle)]
pub extern "C" fn lucia_interpret(
    code: *const c_char,
    config: *const LuciaConfig,
) -> LuciaResult {
    lucia_interpret_with_argv(code, std::ptr::null(), 0, config)
}

#[unsafe(no_mangle)]
pub extern "C" fn lucia_interpret_with_argv(
    code: *const c_char,
    argv: *const *const c_char,
    argc: usize,
    config: *const LuciaConfig,
) -> LuciaResult {
    lucia_interpret_with_vars_and_argv(code, config, std::ptr::null_mut(), argv, argc)
}

#[unsafe(no_mangle)]
pub extern "C" fn lucia_interpret_with_vars(
    code: *const c_char,
    config: *const LuciaConfig,
    vars: *mut LuciaVariables,
) -> LuciaResult {
    lucia_interpret_with_vars_and_argv(code, config, vars, std::ptr::null(), 0)
}

#[unsafe(no_mangle)]
pub extern "C" fn lucia_interpret_with_vars_and_argv(
    code: *const c_char,
    config: *const LuciaConfig,
    vars: *mut LuciaVariables,
    argv: *const *const c_char,
    argc: usize,
) -> LuciaResult {
    let result = std::panic::catch_unwind(|| {
        if code.is_null() || config.is_null() {
            return LuciaResult {
                tag: LuciaResultTag::LUCIA_RESULT_CONFIG_ERR,
                data: unsafe { std::mem::zeroed() },
            };
        }

        let code_str = unsafe { CStr::from_ptr(code).to_string_lossy().into_owned() };
        let cfg = unsafe { config_from_abi(&*config) };
        let mut argv_vec: Vec<String> = Vec::new();
        for i in 0..argc {
            let arg_ptr = unsafe { *argv.add(i) };
            if !arg_ptr.is_null() {
                let arg_str = unsafe { CStr::from_ptr(arg_ptr).to_string_lossy().into_owned() };
                argv_vec.push(arg_str);
            }
        }

        let lexer = Lexer::new(&code_str, "<foreign>");
        let tokens = lexer.tokenize();

        let cwd = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));

        let mut preprocessor = Preprocessor::new(&cwd, "<foreign>");
        let processed_tokens = match preprocessor.process(tokens, &cwd) {
            Ok(toks) => toks,
            Err(e) => {
                let err_abi = error_to_abi(&e);
                return LuciaResult {
                    tag: LuciaResultTag::LUCIA_RESULT_ERROR,
                    data: LuciaResultData { error: ManuallyDrop::new(err_abi) },
                };
            }
        };

        let mut parser = Parser::new(processed_tokens);
        let statements = match parser.parse_safe() {
            Ok(stmts) => stmts,
            Err(e) => {
                let err_abi = error_to_abi(&e);
                return LuciaResult {
                    tag: LuciaResultTag::LUCIA_RESULT_ERROR,
                    data: LuciaResultData { error: ManuallyDrop::new(err_abi) },
                };
            }
        };

        let _ = load_std_libs_embedded();

        let include_default = vars.is_null() || unsafe { (*vars).include_default } != 0;

        let mut interpreter = Interpreter::new(cfg, "<foreign>", &cwd, (cwd.clone(), cwd.clone(), false), &argv_vec);
        let thread_id = current_thread_id();
        LAST_THREAD_ID.store(thread_id, Ordering::Release);
        let flag = {
            let mut map = INTERRUPTS.lock();

            let entry = map.entry(thread_id).or_insert_with(|| {
                (None, Arc::new(AtomicBool::new(false)))
            });

            entry.1.store(false, Ordering::Release);

            entry.1.clone()
        };
        interpreter.stop_flag = Some(flag.clone());
        interpreter.set_main_thread(true);
        let vv = vars_from_abi(vars);
        if include_default {
            interpreter.variables.extend(vv);
        } else {
            interpreter.variables = vv;
        }

        let result = interpreter.interpret(statements, false);
        
        if flag.load(Ordering::Acquire) {
            let msg = {
                let map = INTERRUPTS.lock();
                map.get(&thread_id).and_then(|v| v.0.clone())
            };

            let msg = msg.unwrap_or_else(|| "interrupt".to_string());

            return LuciaResult {
                tag: LuciaResultTag::LUCIA_RESULT_INTERRUPT,
                data: LuciaResultData {
                    interrupt_msg: CString::new(msg).unwrap().into_raw()
                }
            };
        }

        match result {
            Ok(val) => {
                let val_abi = value_to_abi(&val);
                vars_to_abi(&interpreter.variables, vars);
                LuciaResult {
                    tag: LuciaResultTag::LUCIA_RESULT_OK,
                    data: LuciaResultData { value: val_abi },
                }
            }
            Err(e) => {
                let err_abi = error_to_abi(&e);
                LuciaResult {
                    tag: LuciaResultTag::LUCIA_RESULT_ERROR,
                    data: LuciaResultData { error: ManuallyDrop::new(err_abi) },
                }
            }
        }
    });

    match result {
        Ok(r) => r,
        Err(panic_payload) => {
            let msg: String = if let Some(s) = panic_payload.downcast_ref::<&str>() {
                s.to_string()
            } else if let Some(s) = panic_payload.downcast_ref::<String>() {
                s.clone()
            } else {
                "unknown panic".into()
            };

            let display_msg: String = if msg.as_bytes().first() == Some(&CUSTOM_PANIC_MARKER) {
                msg[1..].to_string()
            } else {
                msg
            };

            let c_msg = CString::new(display_msg).unwrap_or_else(|_| CString::new("panic").unwrap());

            LuciaResult {
                tag: LuciaResultTag::LUCIA_RESULT_PANIC,
                data: LuciaResultData { panic_msg: c_msg.into_raw() },
            }
        }
    }
}
