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
use crate::env::runtime::value::Value;
use crate::env::runtime::preprocessor::Preprocessor;
use crate::env::runtime::libs::load_std_libs_embedded;
use crate::env::runtime::utils::format_value;
pub use crate::env::runtime::errors::Error;
use parking_lot::Mutex;
use core::ffi::{c_char, c_void};
use std::mem::ManuallyDrop;
use std::ffi::CString;
use std::ffi::CStr;
use std::sync::Arc;
use std::path::PathBuf;
use std::cell::RefCell;

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
    VALUE_POINTER = 8,
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
    pub pointer: *mut core::ffi::c_void,
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

#[unsafe(no_mangle)] pub extern "C" fn lucia_list_len(list: LuciaValue) -> u32 {
    if list.tag != LuciaValueType::VALUE_LIST {
        return 0;
    }
    list.length as u32
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
#[unsafe(no_mangle)] pub extern "C" fn lucia_map_len(map: LuciaValue) -> u32 {
    if map.tag != LuciaValueType::VALUE_MAP {
        return 0;
    }
    (map.length / 2) as u32 // length is flattened [k,v,k,v,...], so divide by 2 for pair count
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

#[unsafe(no_mangle)] pub extern "C" fn lucia_error_print(err: *const LuciaError, out: *mut libc::FILE) {
    if err.is_null() || out.is_null() {
        return;
    }
    unsafe {
        libc::fprintf(
            out,
            b"%d:%zu: %s\n - %s: %s".as_ptr() as *const c_char,
            (*err).line_num,
            (*err).column,
            (*err).line_text,
            (*err).err_type,
            (*err).err_msg,
        );
        if !(*err).help_msg.is_null() {
            libc::fprintf(
                out,
                b"\nHelp: %s".as_ptr() as *const c_char,
                (*err).help_msg,
            );
        }
        libc::fprintf(out, b"\n".as_ptr() as *const c_char);
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
        _ => {}
    }
}

// very useful
#[unsafe(no_mangle)]
pub extern "C" fn lucia_print_size_checks() {
    use std::mem::{size_of, align_of};

    macro_rules! static_assert_type {
        ($t:ty) => {
            if stringify!($t) == "usize" || stringify!($t).starts_with("*") {
                println!("#if UINTPTR_MAX == 0xffffffffffffffff"); // 64-bit
                println!("LUCIA_STATIC_ASSERT(sizeof({t}) == 8, \"{t} expected to be 8 bytes on 64-bit\");", t=stringify!($t));
                println!("LUCIA_STATIC_ASSERT(alignof({t}) == 8, \"{t} alignment expected to be 8 on 64-bit\");", t=stringify!($t));
                println!("#else"); // 32-bit
                println!("LUCIA_STATIC_ASSERT(sizeof({t}) == 4, \"{t} expected to be 4 bytes on 32-bit\");", t=stringify!($t));
                println!("LUCIA_STATIC_ASSERT(alignof({t}) == 4, \"{t} alignment expected to be 4 on 32-bit\");", t=stringify!($t));
                println!("#endif");
            } else {
                println!("LUCIA_STATIC_ASSERT(sizeof({t}) == {s}, \"Size of {t} was expected to be {s}\");", t=stringify!($t), s=size_of::<$t>());
                println!("LUCIA_STATIC_ASSERT(alignof({t}) == {a}, \"Alignment of {t} was expected to be {a}\");", t=stringify!($t), a=align_of::<$t>());
            }
        };
    }

    macro_rules! static_assert_field {
        ($struct:ty, $field:ident, $ty:ty) => {
            if stringify!($ty) == "usize" || stringify!($ty).starts_with("*") {
                println!("#if UINTPTR_MAX == 0xffffffffffffffff");
                println!("LUCIA_STATIC_ASSERT(sizeof((({s}*)0)->{f}) == 8, \"{s}.{f} expected to be 8 bytes on 64-bit\");", s=stringify!($struct), f=stringify!($field));
                println!("LUCIA_STATIC_ASSERT(alignof((({s}*)0)->{f}) == 8, \"{s}.{f} alignment expected to be 8 on 64-bit\");", s=stringify!($struct), f=stringify!($field));
                println!("#else");
                println!("LUCIA_STATIC_ASSERT(sizeof((({s}*)0)->{f}) == 4, \"{s}.{f} expected to be 4 bytes on 32-bit\");", s=stringify!($struct), f=stringify!($field));
                println!("LUCIA_STATIC_ASSERT(alignof((({s}*)0)->{f}) == 4, \"{s}.{f} alignment expected to be 4 on 32-bit\");", s=stringify!($struct), f=stringify!($field));
                println!("#endif");
            } else {
                println!("LUCIA_STATIC_ASSERT(sizeof((({s}*)0)->{f}) == {sz}, \"{s}.{f} was expected to be {sz}\");", s=stringify!($struct), f=stringify!($field), sz=size_of::<$ty>());
                println!("LUCIA_STATIC_ASSERT(alignof((({s}*)0)->{f}) == {a}, \"{s}.{f} alignment was expected to be {a}\");", s=stringify!($struct), f=stringify!($field), a=align_of::<$ty>());
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
    static_assert_type!(ValueAsArgs);

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

    // ValueAsArgs fields
    static_assert_field!(ValueAsArgs, force, CBool);
    static_assert_field!(ValueAsArgs, cast, CBool);
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

#[unsafe(no_mangle)]
pub extern "C" fn lucia_interpret_with_argv(
    code: *const c_char,
    argv: *const *const c_char,
    argc: usize,
    config: *const LuciaConfig,
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

        let mut interpreter = Interpreter::new(cfg, "<foreign>", &cwd, (cwd.clone(), cwd.clone(), false), &argv_vec);
        interpreter.set_main_thread(true);
        
        match interpreter.interpret(statements, false) {
            Ok(val) => {
                let val_abi = value_to_abi(&val);
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

#[unsafe(no_mangle)]
pub extern "C" fn lucia_interpret(
    code: *const c_char,
    config: *const LuciaConfig,
) -> LuciaResult {
    lucia_interpret_with_argv(code, std::ptr::null(), 0, config)
}
