use std::collections::HashMap;
use std::sync::Arc;

use crate::env::runtime::functions::{Function, NativeFunction, Parameter};
use crate::env::runtime::value::Value;
use crate::env::runtime::variables::Variable;
use crate::env::runtime::errors::Error;
use crate::env::runtime::utils::to_static;
use crate::env::runtime::config::{Config, get_from_config};
use std::env as std_env;
use std::path::{PathBuf, Path};
use once_cell::sync::Lazy;

use crate::{insert_native_fn, insert_native_var};

use crate::env::libs::clib::_tcc;

// This module provides a way to compile and run C code within the Lucia environment using the Tiny C Compiler (TCC).
// It includes functions for compiling and executing C code, as well as basic C library functions.
// Lucia version 2.0.0, module: clib@0.1.69

static TCC_PATH: Lazy<String> = Lazy::new(|| {
    let exe_path = std_env::current_exe().expect("Failed to get current exe path");
    let exe_dir = exe_path.parent().expect("Failed to get exe directory");

    let os_subdir = if cfg!(target_os = "windows") {
        "windows"
    } else if cfg!(target_os = "macos") {
        "macos"
    } else {
        "linux"
    };

    let exe_filename = if cfg!(target_os = "windows") {
        let arch_dir = if cfg!(target_arch = "x86_64") {
            "win64"
        } else {
            "win32"
        };
        format!("{}/tcc.exe", arch_dir)
    } else if cfg!(target_os = "macos") {
        "tcc".to_string()
    } else {
        "tcc".to_string()
    };

    let full_path: PathBuf = exe_dir.join("tcc").join(os_subdir).join(exe_filename);

    full_path.canonicalize()
        .unwrap_or(full_path)
        .to_string_lossy()
        .into_owned()
});

pub fn register() -> HashMap<String, Variable> {
    let mut map = HashMap::new();

    insert_native_fn!(
        map,
        "run",
        run,
        vec![Parameter::positional("code", "str")],
        "any"
    );

    insert_native_fn!(
        map,
        "compile",
        compile,
        vec![
            Parameter::positional("code", "str"),
            Parameter::positional("file_path", "str")
        ],
        "void"
    );

    insert_native_fn!(
        map,
        "compile_shared",
        compile_shared,
        vec![
            Parameter::positional("code", "str"),
            Parameter::positional("file_path", "str")
        ],
        "void"
    );

    insert_native_fn!(
        map,
        "check",
        check,
        vec![Parameter::positional("code", "str")],
        "bool"
    );

    insert_native_fn!(
        map,
        "export",
        export,
        vec![
            Parameter::positional("file_path", "str"),
            Parameter::positional("value", "str"),
            Parameter::positional("expected_type", "str")
        ],
        "any"
    );

    map
}

pub fn init_clib(config: Arc<Config>, file_path: String) -> Result<(), Error> {
    if !get_from_config(&config, "allow_unsafe").is_truthy() {
        return Err(Error::with_help(
            "UnsafeError",
            "Clib operations are unsafe and cannot be performed without explicit permission.",
            "To enable unsafe operations, set 'allow_unsafe' to true in the configuration.",
            to_static(file_path),
        ));
    }

    let lib_path = &*TCC_PATH;

    if !Path::new(lib_path).exists() {
        return Err(Error::with_help(
            "RuntimeError",
            "TCC_PATH is not set or the path does not exist.",
            &format!("Please ensure TCC_PATH points to the TCC shared library. Current path: {}", lib_path),
            to_static(file_path),
        ));
    }

    Ok(())
}

// ------ Native functions ------

fn tcc_error(msg: &str) -> Value {
    Value::Error("TccError", to_static(msg.to_string()), None)
}

fn run(args: &HashMap<String, Value>) -> Value {
    match args.get("code") {
        Some(Value::String(code)) => {
            match _tcc::run(&TCC_PATH, code) {
                Ok(val) => val,
                Err(e) => tcc_error(&format!("Failed to run code: {}", e)),
            }
        }
        _ => Value::Error("TypeError", "Argument 'code' must be a string", None),
    }
}

fn compile(args: &HashMap<String, Value>) -> Value {
    let code = match args.get("code") {
        Some(Value::String(s)) => s,
        _ => return Value::Error("TypeError", "Argument 'code' must be a string", None),
    };

    let file_path = match args.get("file_path") {
        Some(Value::String(s)) => s,
        _ => return Value::Error("TypeError", "Argument 'file_path' must be a string", None),
    };

    match _tcc::compile(&TCC_PATH, code, file_path) {
        Ok(()) => Value::Null,
        Err(e) => tcc_error(&format!("Failed to compile: {}", e)),
    }
}

fn compile_shared(args: &HashMap<String, Value>) -> Value {
    let code = match args.get("code") {
        Some(Value::String(s)) => s,
        _ => return Value::Error("TypeError", "Argument 'code' must be a string", None),
    };

    let file_path = match args.get("file_path") {
        Some(Value::String(s)) => s,
        _ => return Value::Error("TypeError", "Argument 'file_path' must be a string", None),
    };

    match _tcc::compile_shared(&TCC_PATH, code, file_path) {
        Ok(()) => Value::Null,
        Err(e) => tcc_error(&format!("Failed to compile shared library: {}", e)),
    }
}

fn check(args: &HashMap<String, Value>) -> Value {
    match args.get("code") {
        Some(Value::String(code)) => {
            match _tcc::check(&TCC_PATH, code) {
                Ok(valid) => Value::Boolean(valid),
                Err(e) => tcc_error(&format!("Check failed: {}", e)),
            }
        }
        _ => Value::Error("TypeError", "Argument 'code' must be a string", None),
    }
}

fn export(args: &HashMap<String, Value>) -> Value {
    let file_path = match args.get("file_path") {
        Some(Value::String(s)) => s,
        _ => return Value::Error("TypeError", "Argument 'file_path' must be a string", None),
    };

    let value = match args.get("value") {
        Some(Value::String(s)) => s,
        _ => return Value::Error("TypeError", "Argument 'value' must be a string", None),
    };

    let expected_type = match args.get("expected_type") {
        Some(Value::String(s)) => s,
        _ => return Value::Error("TypeError", "Argument 'expected_type' must be a string", None),
    };

    match _tcc::export(&TCC_PATH, file_path, value, expected_type) {
        Ok(val) => val,
        Err(e) => tcc_error(&format!("Export failed: {}", e)),
    }
}
