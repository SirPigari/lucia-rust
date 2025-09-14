use std::collections::HashMap;
use std::sync::Arc;
use crate::env::runtime::functions::{Function, NativeFunction, Parameter};
use crate::env::runtime::types::Int;
use crate::env::runtime::value::Value;
use crate::env::runtime::variables::Variable;
use crate::env::runtime::config::{get_from_config, Config};
use crate::{insert_native_fn, insert_native_var};

#[cfg(not(target_arch = "wasm32"))]
use std::env::consts;
#[cfg(not(target_arch = "wasm32"))]
use sys_info;

#[cfg(target_pointer_width = "64")]
const MAX_PTR: usize = 0x0000_FFFF_FFFF_FFFF;

#[cfg(target_pointer_width = "32")]
const MAX_PTR: usize = 0xFFFF_FFFF; // max for 32-bit usize

// ==== Helpers ====

#[cfg(not(target_arch = "wasm32"))]
fn wrap_string_fn<F>(func: F) -> impl Fn(&HashMap<String, Value>) -> Value
where
    F: Fn() -> Result<String, sys_info::Error> + Send + Sync + 'static,
{
    move |_| match func() {
        Ok(s) => Value::String(s),
        Err(e) => Value::Error("OSError", Box::leak(Box::new(format!("{}", e))), None),
    }
}

#[cfg(not(target_arch = "wasm32"))]
fn wrap_u64_fn<F>(func: F) -> impl Fn(&HashMap<String, Value>) -> Value
where
    F: Fn() -> Result<u64, sys_info::Error> + Send + Sync + 'static,
{
    move |_| match func() {
        Ok(v) => Value::Int(Int::from_i64(v as i64)),
        Err(e) => Value::Error("OSError", Box::leak(Box::new(format!("{}", e))), None),
    }
}

#[cfg(not(target_arch = "wasm32"))]
fn wrap_usize_fn<F>(func: F) -> impl Fn(&HashMap<String, Value>) -> Value
where
    F: Fn() -> Result<usize, sys_info::Error> + Send + Sync + 'static,
{
    move |_| match func() {
        Ok(v) => Value::Int(Int::from_i64(v as i64)),
        Err(e) => Value::Error("OSError", Box::leak(Box::new(format!("{}", e))), None),
    }
}

// Unsafe pointer functions are the same for all platforms
fn to_ptr(ptr: usize, allow_unsafe: bool) -> Value {
    if !allow_unsafe {
        return Value::Error(
            "TypeError",
            "This function is unsafe and can result in segmentation fault. Set 'allow_unsafe' to true to run this function.",
            None,
        );
    }

    if ptr < 0x1000 || ptr > MAX_PTR {
        return Value::Error("ValueError", "Pointer value is out of valid range", None);
    }

    unsafe {
        let arc = Arc::from_raw(ptr as *const Value);
        Value::Pointer(arc)
    }
}

fn from_ptr(ptr: usize, allow_unsafe: bool) -> Value {
    if !allow_unsafe {
        return Value::Error(
            "TypeError",
            "This function is unsafe and can result in segmentation fault. Set 'allow_unsafe' to true to run this function.",
            None,
        );
    }

    if ptr < 0x1000 || ptr > MAX_PTR {
        return Value::Error("ValueError", "Pointer value is out of valid range", None);
    }

    unsafe {
        let raw = ptr as *const Value;
        (*raw).clone()
    }
}

// Panic helper
fn panic_handler(args: &HashMap<String, Value>) -> Value {
    const CUSTOM_PANIC_MARKER: u8 = 0x1B;

    if let Some(Value::String(message)) = args.get("message") {
        let mut marked_message = String::new();
        marked_message.push(CUSTOM_PANIC_MARKER as char);
        marked_message.push_str(message);
        panic!("{}", marked_message);
    }
    Value::Error("PanicError", "Panic called".into(), None)
}

// ==== Registration ====

pub fn register(config: &Config) -> HashMap<String, Variable> {
    let mut map = HashMap::new();

    #[cfg(not(target_arch = "wasm32"))]
    {
        // Native OS functions
        let string_fns: Vec<(&str, Box<dyn Fn() -> Result<String, sys_info::Error> + Send + Sync>)> = vec![
            ("os_name", Box::new(sys_info::os_type)),
            ("os_version", Box::new(sys_info::os_release)),
            ("hostname", Box::new(sys_info::hostname)),
            ("os_arch", Box::new(|| Ok(consts::ARCH.to_string()))),
        ];

        for (name, func) in string_fns {
            insert_native_fn!(map, name, wrap_string_fn(func), vec![], "str");
        }

        let u64_fns: Vec<(&str, fn() -> Result<u64, sys_info::Error>)> =
            vec![("cpu_speed", sys_info::cpu_speed), ("mem_total", || Ok(sys_info::mem_info()?.total)), ("mem_free", || Ok(sys_info::mem_info()?.free))];

        for (name, func) in u64_fns {
            insert_native_fn!(map, name, wrap_u64_fn(func), vec![], "int");
        }

        let usize_fns: Vec<(&str, fn() -> Result<usize, sys_info::Error>)> = vec![
            ("cpu_num", || Ok(sys_info::cpu_num()? as usize)),
            ("processes", || Ok(sys_info::proc_total()? as usize)),
        ];

        for (name, func) in usize_fns {
            insert_native_fn!(map, name, wrap_usize_fn(func), vec![], "int");
        }

        insert_native_fn!(map, "loadavg", |_: &HashMap<String, Value>| -> Value {
            match sys_info::loadavg() {
                Ok(l) => {
                    let mut inner_map = HashMap::new();
                    inner_map.insert("one".to_string(), Value::Float(l.one.into()));
                    inner_map.insert("five".to_string(), Value::Float(l.five.into()));
                    inner_map.insert("fifteen".to_string(), Value::Float(l.fifteen.into()));
                    Value::Map { 
                        keys: inner_map.keys().cloned().map(Value::String).collect(),
                        values: inner_map.values().cloned().collect(),
                    }
                }
                Err(e) => Value::Error("OSError", Box::leak(Box::new(format!("{}", e))), None),
            }
        }, vec![], "map");

        insert_native_fn!(map, "disk_info", |_: &HashMap<String, Value>| -> Value {
            match sys_info::disk_info() {
                Ok(d) => {
                    let mut inner_map = HashMap::new();
                    inner_map.insert("total".to_string(), Value::Int(Int::from_i64(d.total as i64)));
                    inner_map.insert("free".to_string(), Value::Int(Int::from_i64(d.free as i64)));
                    Value::Map { 
                        keys: inner_map.keys().cloned().map(Value::String).collect(),
                        values: inner_map.values().cloned().collect(),
                    }
                }
                Err(e) => Value::Error("OSError", Box::leak(Box::new(format!("{}", e))), None),
            }
        }, vec![], "map");
    }

    #[cfg(target_arch = "wasm32")]
    {
        use js_sys::Date;

        insert_native_fn!(map, "os_name", |_: &HashMap<String, Value>| -> Value {
            Value::String("WebAssembly".into())
        }, vec![], "str");

        insert_native_fn!(map, "os_version", |_: &HashMap<String, Value>| -> Value {
            Value::String("N/A".into())
        }, vec![], "str");

        insert_native_fn!(map, "hostname", |_: &HashMap<String, Value>| -> Value {
            Value::String("Browser".into())
        }, vec![], "str");

        insert_native_fn!(map, "cpu_num", |_: &HashMap<String, Value>| -> Value {
            Value::Int(Int::from_i64(1)) // No real CPU info in WASM
        }, vec![], "int");

        insert_native_fn!(map, "mem_total", |_: &HashMap<String, Value>| -> Value {
            Value::Int(Int::from_i64(0)) // Can't access memory info in browser
        }, vec![], "int");

        insert_native_fn!(map, "time_now", |_: &HashMap<String, Value>| -> Value {
            let d = Date::new_0();
            Value::Float(d.get_time().into())
        }, vec![], "float");
    }

    // Unsafe pointer handling
    let allow_unsafe = matches!(get_from_config(config, "allow_unsafe"), Value::Boolean(true));
    
    insert_native_fn!(map, "from_ptr", move |args: &HashMap<String, Value>| -> Value {
        if let Some(Value::Int(int)) = args.get("ptr") {
            if let Ok(ptr) = int.to_i64() {
                from_ptr(ptr as usize, allow_unsafe)
            } else {
                Value::Error("TypeError", "Invalid 'ptr' integer value", None)
            }
        } else {
            Value::Error("TypeError", "Expected 'ptr' to be an integer", None)
        }
    }, vec![Parameter::positional("ptr", "int")], "any");

    insert_native_fn!(map, "to_ptr", move |args: &HashMap<String, Value>| -> Value {
        if let Some(Value::Int(int)) = args.get("ptr") {
            if let Ok(ptr) = int.to_i64() {
                to_ptr(ptr as usize, allow_unsafe)
            } else {
                Value::Error("TypeError", "Invalid 'ptr' integer value", None)
            }
        } else {
            Value::Error("TypeError", "Expected 'ptr' to be an integer", None)
        }
    }, vec![Parameter::positional("ptr", "int")], "any");

    insert_native_fn!(map, "panic", panic_handler, vec![Parameter::positional_optional("message", "str", "Panic called without a message".into())], "void");

    // Common vars
    insert_native_var!(map, "word_size", Value::Int(Int::from_i64(std::mem::size_of::<usize>() as i64)), "int");
    insert_native_var!(map, "pointer_size", Value::Int(Int::from_i64(std::mem::size_of::<*const ()>() as i64)), "int");
    #[cfg(not(target_arch = "wasm32"))]
    insert_native_var!(map, "arch", Value::String(consts::ARCH.to_string()), "str");
    #[cfg(target_arch = "wasm32")]
    insert_native_var!(map, "arch", Value::String("wasm32".into()), "str");

    map
}
