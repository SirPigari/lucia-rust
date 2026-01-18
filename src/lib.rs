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
pub use crate::env::runtime::errors::Error;
use core::ffi::{c_char, c_void};
use std::mem::ManuallyDrop;
use std::ffi::CString;
use std::ffi::CStr;
use std::sync::Arc;
use std::path::PathBuf;

use interpreter::Interpreter;
use parser::Parser;
use lexer::Lexer;

const NAME: &str = env!("CARGO_PKG_NAME");
const VERSION: &str = env!("VERSION");
const UUID: &str = env!("BUILD_UUID");
const RUSTC_VERSION: &str = env!("RUSTC_VERSION");
const RUSTC_CHANNEL: &str = env!("RUSTC_CHANNEL");
const TARGET: &str = env!("TARGET_TRIPLE");
const REPOSITORY: &str = env!("REPO");
const GIT_HASH: &str = env!("GIT_HASH");
const FILE_HASH: &str = env!("FILE_HASH");
const PROFILE: &str = env!("PROFILE");
const BUILD_DATE: &str = env!("BUILD_DATE");

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
        stack_size: cfg.stack_size,
        version: if cfg.version.is_null() { "".into() } else { unsafe { CStr::from_ptr(cfg.version).to_string_lossy().into_owned() } },
        color_scheme: Default::default(),
        cache_format: Default::default(),
        type_checker: Default::default(),
    }
}

#[repr(C)]
#[derive(Copy, Clone, PartialEq, Eq)]
#[allow(non_camel_case_types)]
pub enum LuciaValueType {
    VALUE_INT = 1,
    VALUE_FLOAT = 2,
    VALUE_STRING = 3,
    VALUE_BOOLEAN = 4,
    VALUE_NULL = 5,
    VALUE_LIST = 6,
    VALUE_MAP = 7,
    VALUE_BYTES = 8,
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
    pub pointer: *mut core::ffi::c_void,
}

fn value_to_abi(v: &Value) -> LuciaValue {
    match v {
        Value::Int(i) => LuciaValue { tag: LuciaValueType::VALUE_INT, data: ValueData { int_v: match i.to_i64() { Ok(val) => val, Err(_) => i64::MAX } }, length: 0 },
        Value::Float(f) => LuciaValue { tag: LuciaValueType::VALUE_FLOAT, data: ValueData { float_v: match f.to_f64() { Ok(val) => val, Err(_) => f64::MAX } }, length: 0 },
        Value::Boolean(b) => LuciaValue { tag: LuciaValueType::VALUE_BOOLEAN, data: ValueData { bool_v: *b as u8 }, length: 0 },
        Value::Null => LuciaValue { tag: LuciaValueType::VALUE_NULL, data: ValueData { int_v: 0 }, length: 0 },
        Value::String(s) => LuciaValue { tag: LuciaValueType::VALUE_STRING, data: ValueData { string_v: s.as_ptr() as *const c_char }, length: s.len() },
        Value::List(l) => LuciaValue { tag: LuciaValueType::VALUE_LIST, data: ValueData { list_ptr: l.as_ptr() as *const LuciaValue }, length: l.len() },
        Value::Bytes(b) => LuciaValue { tag: LuciaValueType::VALUE_BYTES, data: ValueData { bytes_ptr: b.as_ptr() }, length: b.len() },
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
        }
        Value::Pointer(p) => LuciaValue { tag: LuciaValueType::VALUE_POINTER, data: ValueData { pointer: Arc::as_ptr(p) as *mut _ }, length: 0 },
        _ => LuciaValue { tag: LuciaValueType::VALUE_UNSUPPORTED, data: ValueData { int_v: 0 }, length: 0 },
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

#[repr(C)]
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
    pub value: ManuallyDrop<LuciaValue>,        // LUCIA_RESULT_OK
    pub error: ManuallyDrop<LuciaError>,        // LUCIA_RESULT_ERROR
    pub panic_msg: *const c_char, // LUCIA_RESULT_PANIC
}

#[unsafe(no_mangle)]
pub extern "C" fn lucia_get_build_info() -> BuildInfo {
    BuildInfo {
        name:          NAME.as_ptr() as *const c_char,
        version:       VERSION.as_ptr() as *const c_char,
        uuid:          UUID.as_ptr() as *const c_char,
        rustc_version: RUSTC_VERSION.as_ptr() as *const c_char,
        rustc_channel: RUSTC_CHANNEL.as_ptr() as *const c_char,
        target:        TARGET.as_ptr() as *const c_char,
        repository:    REPOSITORY.as_ptr() as *const c_char,
        git_hash:      GIT_HASH.as_ptr() as *const c_char,
        file_hash:     FILE_HASH.as_ptr() as *const c_char,
        profile:       PROFILE.as_ptr() as *const c_char,
        build_date:    BUILD_DATE.as_ptr() as *const c_char,
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn lucia_free_value(v: LuciaValue) {
    match v.tag {
        LuciaValueType::VALUE_STRING => {
            let s_ptr = unsafe { v.data.string_v as *mut c_char };
            if !s_ptr.is_null() {
                unsafe { let _ = CString::from_raw(s_ptr); }
            }
        }
        LuciaValueType::VALUE_LIST => {
            let list_ptr = unsafe { v.data.list_ptr as *mut LuciaValue };
            let len = v.length;
            if !list_ptr.is_null() {
                let list_slice = unsafe { std::slice::from_raw_parts_mut(list_ptr, len) };
                for item in list_slice.iter_mut() {
                    lucia_free_value(*item);
                }
                unsafe { Vec::from_raw_parts(list_ptr, len, len); }
            }
        }
        LuciaValueType::VALUE_BYTES => {
            let bytes_ptr = unsafe { v.data.bytes_ptr as *mut u8 };
            let len = v.length;
            if !bytes_ptr.is_null() {
                unsafe { Vec::from_raw_parts(bytes_ptr, len, len); }
            }
        }
        _ => {}
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
    let version_ptr = cfg.version as *mut c_char;
    if !version_ptr.is_null() {
        unsafe { let _ = CString::from_raw(version_ptr); }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn lucia_free_result(res: LuciaResult) {
    match res.tag {
        LuciaResultTag::LUCIA_RESULT_OK => {
            let value = unsafe { ManuallyDrop::into_inner(res.data.value) };
            lucia_free_value(value);
        }
        LuciaResultTag::LUCIA_RESULT_ERROR => {
            let error = unsafe { ManuallyDrop::into_inner(res.data.error) };
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

#[unsafe(no_mangle)]
pub extern "C" fn lucia_free_build_info(info: BuildInfo) {
    unsafe {
        if !info.name.is_null() { let _ = CString::from_raw(info.name as *mut c_char); }
        if !info.version.is_null() { let _ = CString::from_raw(info.version as *mut c_char); }
        if !info.uuid.is_null() { let _ = CString::from_raw(info.uuid as *mut c_char); }
        if !info.rustc_version.is_null() { let _ = CString::from_raw(info.rustc_version as *mut c_char); }
        if !info.rustc_channel.is_null() { let _ = CString::from_raw(info.rustc_channel as *mut c_char); }
        if !info.target.is_null() { let _ = CString::from_raw(info.target as *mut c_char); }
        if !info.repository.is_null() { let _ = CString::from_raw(info.repository as *mut c_char); }
        if !info.git_hash.is_null() { let _ = CString::from_raw(info.git_hash as *mut c_char); }
        if !info.file_hash.is_null() { let _ = CString::from_raw(info.file_hash as *mut c_char); }
        if !info.profile.is_null() { let _ = CString::from_raw(info.profile as *mut c_char); }
        if !info.build_date.is_null() { let _ = CString::from_raw(info.build_date as *mut c_char); }
    }
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
                    data: LuciaResultData { value: ManuallyDrop::new(val_abi) },
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
            let c_msg = CString::new(msg).unwrap_or_else(|_| CString::new("panic").unwrap());

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
