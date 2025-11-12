use std::collections::HashMap;
use std::sync::Arc;
use crate::env::runtime::functions::{Function, Parameter};
use crate::env::runtime::types::Int;
use crate::env::runtime::value::Value;
use crate::env::runtime::variables::Variable;
use crate::env::runtime::config::{get_from_config, Config};
use crate::env::runtime::internal_structs::EffectFlags;
use crate::env::runtime::utils::MAX_PTR;
use crate::env::runtime::modules::Module;
use crate::{insert_native_fn, make_native_fn_pt, insert_native_var};
use crate::env::runtime::structs_and_enums::Struct;
use std::sync::Mutex;
use std::path::PathBuf;

use crate::env::runtime::statements::Statement;
use crate::env::runtime::types::Type;

#[cfg(not(target_arch = "wasm32"))]
use std::env::consts;
#[cfg(not(target_arch = "wasm32"))]
use crate::env::runtime::utils::to_static;
#[cfg(not(target_arch = "wasm32"))]
use sys_info;

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
            "This function is unsafe. Enable allow_unsafe to use it.",
            None,
        );
    }

    if ptr < 0x1000 || ptr > MAX_PTR {
        return Value::Error("ValueError", "Pointer value is out of valid range", None);
    }

    unsafe {
        let arc_ptr = ptr as *const Mutex<Value>;
        let arc_ref: Arc<Mutex<Value>> = Arc::from_raw(arc_ptr);
        let cloned_arc = Arc::clone(&arc_ref);
        std::mem::forget(arc_ref);
        Value::Pointer(cloned_arc)
    }
}

fn from_ptr(ptr: usize, allow_unsafe: bool) -> Value {
    if !allow_unsafe {
        return Value::Error(
            "TypeError",
            "This function is unsafe. Enable allow_unsafe to use it.",
            None,
        );
    }

    if ptr < 0x1000 || ptr > MAX_PTR {
        return Value::Error("ValueError", "Pointer value is out of valid range", None);
    }

    unsafe {
        let arc_ptr = ptr as *const Mutex<Value>;
        let arc_ref: Arc<Mutex<Value>> = Arc::from_raw(arc_ptr);
        let val = arc_ref.lock().unwrap().clone();
        std::mem::forget(arc_ref);
        val
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

#[cfg(unix)]
fn create_signal_map() -> HashMap<String, Variable> {
    let mut map = HashMap::new();
    insert_native_var!(map, "SIGHUP",    Value::Int(Int::from_i64(1)),  "int");
    insert_native_var!(map, "SIGINT",    Value::Int(Int::from_i64(2)),  "int");
    insert_native_var!(map, "SIGQUIT",   Value::Int(Int::from_i64(3)),  "int");
    insert_native_var!(map, "SIGILL",    Value::Int(Int::from_i64(4)),  "int");
    insert_native_var!(map, "SIGTRAP",   Value::Int(Int::from_i64(5)),  "int");
    insert_native_var!(map, "SIGABRT",   Value::Int(Int::from_i64(6)),  "int");
    insert_native_var!(map, "SIGBUS",    Value::Int(Int::from_i64(7)),  "int");
    insert_native_var!(map, "SIGFPE",    Value::Int(Int::from_i64(8)),  "int");
    insert_native_var!(map, "SIGKILL",   Value::Int(Int::from_i64(9)),  "int");
    insert_native_var!(map, "SIGUSR1",   Value::Int(Int::from_i64(10)), "int");
    insert_native_var!(map, "SIGSEGV",   Value::Int(Int::from_i64(11)), "int");
    insert_native_var!(map, "SIGUSR2",   Value::Int(Int::from_i64(12)), "int");
    insert_native_var!(map, "SIGPIPE",   Value::Int(Int::from_i64(13)), "int");
    insert_native_var!(map, "SIGALRM",   Value::Int(Int::from_i64(14)), "int");
    insert_native_var!(map, "SIGTERM",   Value::Int(Int::from_i64(15)), "int");
    insert_native_var!(map, "SIGCHLD",   Value::Int(Int::from_i64(17)), "int");
    insert_native_var!(map, "SIGCONT",   Value::Int(Int::from_i64(18)), "int");
    insert_native_var!(map, "SIGSTOP",   Value::Int(Int::from_i64(19)), "int");
    insert_native_var!(map, "SIGTSTP",   Value::Int(Int::from_i64(20)), "int");
    insert_native_var!(map, "SIGTTIN",   Value::Int(Int::from_i64(21)), "int");
    insert_native_var!(map, "SIGTTOU",   Value::Int(Int::from_i64(22)), "int");

    insert_native_fn!(map, "send", |args: &HashMap<String, Value>| -> Value {
        let signum = match args.get("sig") {
            Some(Value::Int(int)) => match int.to_i64() {
                Ok(v) => v,
                Err(_) => return Value::Error("TypeError", "Invalid 'sig' integer value", None),
            },
            _ => return Value::Error("TypeError", "Expected 'sig' to be an integer", None),
        };
        let pid = match args.get("pid") {
            Some(Value::Int(int)) => match int.to_i64() {
                Ok(v) => v,
                Err(_) => return Value::Error("TypeError", "Invalid 'pid' integer value", None),
            },
            _ => return Value::Error("TypeError", "Expected 'pid' to be an integer", None),
        };

        let res = unsafe { libc::kill(pid as libc::pid_t, signum as libc::c_int) };
        if res == 0 {
            Value::Boolean(true)
        } else {
            Value::Error("OSError", to_static(format!("Failed to send signal {} to pid {}", signum, pid)), None)
        }
    }, vec![Parameter::positional("pid", "int"), Parameter::positional("sig", "int")], "bool", EffectFlags::IO);
    map
}

#[cfg(windows)]
fn create_signal_map() -> HashMap<String, Variable> {
    let mut map = HashMap::new();
    insert_native_var!(map, "SIGABRT", Value::Int(Int::from_i64(6)),  "int");
    insert_native_var!(map, "SIGFPE",  Value::Int(Int::from_i64(8)),  "int");
    insert_native_var!(map, "SIGILL",  Value::Int(Int::from_i64(4)),  "int");
    insert_native_var!(map, "SIGINT",  Value::Int(Int::from_i64(2)),  "int");
    insert_native_var!(map, "SIGSEGV", Value::Int(Int::from_i64(11)), "int");
    insert_native_var!(map, "SIGTERM", Value::Int(Int::from_i64(15)), "int");

    insert_native_fn!(map, "send", |args: &HashMap<String, Value>| -> Value {
        use std::process;
        use libc::raise;

        let signum = match args.get("sig") {
            Some(Value::Int(int)) => match int.to_i64() {
                Ok(v) => v,
                Err(_) => return Value::Error("TypeError", "Invalid 'sig' integer value", None),
            },
            _ => return Value::Error("TypeError", "Expected 'sig' to be an integer", None),
        };

        let pid = match args.get("pid") {
            Some(Value::Int(int)) => match int.to_i64() {
                Ok(v) => v,
                Err(_) => return Value::Error("TypeError", "Invalid 'pid' integer value", None),
            },
            _ => return Value::Error("TypeError", "Expected 'pid' to be an integer", None),
        };

        let current_pid = process::id() as i64;

        if pid == current_pid {
            let res = unsafe { raise(signum as i32) };
            if res == 0 {
                Value::Boolean(true)
            } else {
                Value::Error("OSError", to_static(format!("Failed to raise signal {} on self", signum)), None)
            }
        } else {
            if signum != 15 {
                return Value::Error("OSError", "Only SIGTERM (15) can be sent to other processes", None);
            }

            match process::Command::new("taskkill")
                .args(&["/PID", &pid.to_string(), "/F"])
                .output()
            {
                Ok(output) if output.status.success() => Value::Boolean(true),
                Ok(output) => {
                    let err_msg = String::from_utf8_lossy(&output.stderr).trim().trim_start_matches("ERROR: ").to_string();
                    Value::Error(
                        "OSError",
                        to_static(format!("Failed to terminate process {}: {}", pid, err_msg)),
                        None,
                    )
                }
                Err(e) => Value::Error(
                    "OSError",
                    to_static(format!("Failed to run taskkill for {}: {}", pid, e)),
                    None,
                ),
            }
        }
    }, vec![Parameter::positional("pid", "int"), Parameter::positional("sig", "int")], "bool", EffectFlags::IO);
    map
}

#[cfg(target_arch = "wasm32")]
fn create_signal_map() -> HashMap<String, Variable> {
    let mut map = HashMap::new();
    insert_native_var!(map, "SIGABRT", Value::Int(Int::from_i64(6)),  "int");
    insert_native_var!(map, "SIGFPE",  Value::Int(Int::from_i64(8)),  "int");
    insert_native_var!(map, "SIGILL",  Value::Int(Int::from_i64(4)),  "int");
    insert_native_var!(map, "SIGINT",  Value::Int(Int::from_i64(2)),  "int");
    insert_native_var!(map, "SIGSEGV", Value::Int(Int::from_i64(11)), "int");
    insert_native_var!(map, "SIGTERM", Value::Int(Int::from_i64(15)), "int");

    insert_native_fn!(map, "send", |_args: &HashMap<String, Value>| -> Value {
        Value::Error("OSError", "Signal handling is not supported in WebAssembly environment", None)
    }, 
    vec![Parameter::positional("pid", "int"), Parameter::positional("sig", "int")], 
    "bool", EffectFlags::IO);
    map
}

#[cfg(not(target_arch = "wasm32"))]
fn create_subprocess_map() -> HashMap<String, Variable> {
    let mut map = HashMap::new();

    let mut process_output_struct = Type::Struct {
        name: "ProcessOutput".to_string(),
        fields: vec![
            ("pid".to_string(), Statement::make_value(Value::Type(Type::new_simple("int"))), vec![]),
            ("stdout".to_string(), Statement::make_value(Value::Type(Type::new_simple("str"))), vec![]),
            ("stderr".to_string(), Statement::make_value(Value::Type(Type::new_simple("str"))), vec![]),
            ("returncode".to_string(), Statement::make_value(Value::Type(Type::new_simple("int"))), vec![]),
            ("shellerr".to_string(), Statement::make_value(Value::Type(Type::new_simple("str"))), vec![]),
        ],
        methods: vec![],
        generics: Vec::new(),
        wheres: Vec::new(),
    };

    let mut methods = Vec::new();

    let exitcode_method = ("exitcode".to_string(),
        make_native_fn_pt!("exitcode", |args: &HashMap<String, Value>| -> Value {
            if let Some(Value::Struct(instance)) = args.get("self") {
                if let Some(val) = instance.get_field("returncode") {
                    return val.clone();
                }
            }
            Value::Error("AttributeError", "Failed to get exitcode", None)
        }, vec![Parameter::instance("self", &process_output_struct, vec![])], &Type::new_simple("int"), EffectFlags::PURE)
    );

    let runned_method = ("runned".to_string(),
        make_native_fn_pt!("runned", |args: &HashMap<String, Value>| -> Value {
            if let Some(Value::Struct(instance)) = args.get("self") {
                match instance.get_field("shellerr") {
                    Some(Value::String(s)) => {
                        return Value::Boolean(s.is_empty());
                    }
                    _ => {}
                }
            }
            Value::Error("AttributeError", "Failed to get runned status", None)
        }, vec![Parameter::instance("self", &process_output_struct, vec![])], &Type::new_simple("bool"), EffectFlags::PURE)
    );

    methods.push(exitcode_method);
    methods.push(runned_method);

    if let Type::Struct { methods: old_methods, .. } = &mut process_output_struct {
        *old_methods = methods;
    }

    let closure_struct = process_output_struct.clone();
    insert_native_fn!(map, "popen", move |args: &HashMap<String, Value>| -> Value {
        use std::process::{Command, Stdio};
        use std::io::Read;
        use std::path::Path;

        let args_val = match args.get("args") {
            Some(Value::List(list)) => list.iter().map(|v| {
                if let Value::String(s) = v {
                    s.clone()
                } else {
                    "".to_string()
                }
            }).collect::<Vec<String>>(),
            _ => return Value::Error("TypeError", "Expected 'args' to be a list of strings", None),
        };

        let shell = match args.get("shell") {
            Some(Value::Boolean(b)) => *b,
            _ => false,
        };

        let cwd = match args.get("cwd") {
            Some(Value::String(s)) => Some(s.as_str()),
            _ => None,
        };

        let capture_output = match args.get("capture_output") {
            Some(Value::Boolean(b)) => *b,
            _ => false,
        };

        let mut command = if shell {
            let (shell_cmd, flag) = if cfg!(target_os = "windows") {
                ("cmd", "/C")
            } else {
                ("sh", "-c")
            };
            let mut cmd = Command::new(shell_cmd);
            cmd.arg(flag).arg(args_val.join(" "));
            cmd
        } else {
            let mut cmd = Command::new(&args_val[0]);
            if args_val.len() > 1 {
                cmd.args(&args_val[1..]);
            }
            cmd
        };

        if let Some(dir) = cwd {
            command.current_dir(Path::new(dir));
        }

        if capture_output {
            command.stdout(Stdio::piped()).stderr(Stdio::piped());
        } else {
            command.stdout(Stdio::inherit()).stderr(Stdio::inherit());
        }

        match command.spawn() {
            Ok(mut child) => {
                let mut stdout = String::new();
                let mut stderr = String::new();

                if let Some(ref mut out) = child.stdout {
                    let mut reader = std::io::BufReader::new(out);
                    reader.read_to_string(&mut stdout).ok();
                }

                if let Some(ref mut err) = child.stderr {
                    let mut reader = std::io::BufReader::new(err);
                    reader.read_to_string(&mut stderr).ok();
                }

                let status = child.wait().unwrap();
                let returncode = status.code().unwrap_or(-1);

                let mut output_map: HashMap<String, (Box<Value>, Type)> = HashMap::new();
                output_map.insert("pid".to_string(), (Box::new(Value::Int(Int::from_i64(child.id() as i64))), Type::new_simple("int")));
                output_map.insert("stdout".to_string(), (Box::new(Value::String(stdout)), Type::new_simple("str")));
                output_map.insert("stderr".to_string(), (Box::new(Value::String(stderr)), Type::new_simple("str")));
                output_map.insert("returncode".to_string(), (Box::new(Value::Int(Int::from_i64(returncode as i64))), Type::new_simple("int")));
                output_map.insert("shellerr".to_string(), (Box::new(Value::String("".to_string())), Type::new_simple("str")));
                Value::Struct(Struct::new_with_fields(
                    closure_struct.clone(),
                    output_map,
                ))
            }
            Err(e) => {
                let mut output_map: HashMap<String, (Box<Value>, Type)> = HashMap::new();
                output_map.insert("pid".to_string(), (Box::new(Value::Int(Int::from_i64(-1))), Type::new_simple("int")));
                output_map.insert("stdout".to_string(), (Box::new(Value::String("".to_string())), Type::new_simple("str")));
                output_map.insert("stderr".to_string(), (Box::new(Value::String("".to_string())), Type::new_simple("str")));
                output_map.insert("returncode".to_string(), (Box::new(Value::Int(Int::from_i64(-1))), Type::new_simple("int")));
                output_map.insert("shellerr".to_string(), (Box::new(Value::String(e.to_string())), Type::new_simple("str")));
                Value::Struct(Struct::new_with_fields(
                    closure_struct.clone(),
                    output_map,
                ))
            }
        }
    }, vec![Parameter::positional("args", "list"), Parameter::positional_optional("shell", "bool", Value::Boolean(false)), Parameter::positional_optional_pt("cwd", &Type::new_simple("str").set_maybe_type(true), Value::Null), Parameter::positional_optional("capture_output", "bool", Value::Boolean(false))], "ProcessOutput", EffectFlags::IO);

    insert_native_fn!(map, "run", |args: &HashMap<String, Value>| -> Value {
        use std::process::Command;

        let command = match args.get("command") {
            Some(Value::String(s)) => s,
            _ => return Value::Error("TypeError", "Expected 'command' to be a string", None),
        };

        let (shell, flag) = if cfg!(target_os = "windows") {
            ("cmd", "/C")
        } else {
            ("sh", "-c")
        };

        match Command::new(shell)
            .arg(flag)
            .arg(command)
            .status()
        {
            Ok(status) => {
                let code = status.code().unwrap_or(-1);
                Value::Int(code.into())
            }
            Err(e) => Value::Error(
                "RuntimeError",
                to_static(format!("Failed to execute command: {}", e)),
                None,
            ),
        }
    }, vec![Parameter::positional("command", "str")], "int", EffectFlags::IO);

    insert_native_var!(map, "ProcessOutput", Value::Type(process_output_struct), "type");

    map
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
            insert_native_fn!(map, name, wrap_string_fn(func), vec![], "str", EffectFlags::IO);
        }

        let u64_fns: Vec<(&str, fn() -> Result<u64, sys_info::Error>)> =
            vec![("cpu_speed", sys_info::cpu_speed), ("mem_total", || Ok(sys_info::mem_info()?.total)), ("mem_free", || Ok(sys_info::mem_info()?.free))];

        for (name, func) in u64_fns {
            insert_native_fn!(map, name, wrap_u64_fn(func), vec![], "int", EffectFlags::IO);
        }

        let usize_fns: Vec<(&str, fn() -> Result<usize, sys_info::Error>)> = vec![
            ("cpu_num", || Ok(sys_info::cpu_num()? as usize)),
            ("processes", || Ok(sys_info::proc_total()? as usize)),
        ];

        for (name, func) in usize_fns {
            insert_native_fn!(map, name, wrap_usize_fn(func), vec![], "int", EffectFlags::IO);
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
        }, vec![], "map", EffectFlags::IO);

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
        }, vec![], "map", EffectFlags::IO);
    }

    #[cfg(target_arch = "wasm32")]
    {
        use js_sys::Date;

        insert_native_fn!(map, "os_name", |_: &HashMap<String, Value>| -> Value {
            Value::String("WebAssembly".into())
        }, vec![], "str", EffectFlags::IO);

        insert_native_fn!(map, "os_version", |_: &HashMap<String, Value>| -> Value {
            Value::String("N/A".into())
        }, vec![], "str", EffectFlags::IO);

        insert_native_fn!(map, "hostname", |_: &HashMap<String, Value>| -> Value {
            Value::String("Browser".into())
        }, vec![], "str", EffectFlags::IO);

        insert_native_fn!(map, "cpu_num", |_: &HashMap<String, Value>| -> Value {
            Value::Int(Int::from_i64(1)) // No real CPU info in WASM
        }, vec![], "int", EffectFlags::IO);

        insert_native_fn!(map, "mem_total", |_: &HashMap<String, Value>| -> Value {
            Value::Int(Int::from_i64(0)) // Can't access memory info in browser
        }, vec![], "int", EffectFlags::IO);

        insert_native_fn!(map, "time_now", |_: &HashMap<String, Value>| -> Value {
            let d = Date::new_0();
            Value::Float(d.get_time().into())
        }, vec![], "float", EffectFlags::IO);
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
    }, vec![Parameter::positional("ptr", "int")], "any", EffectFlags::UNSAFE);

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
    }, vec![Parameter::positional("ptr", "int")], "any", EffectFlags::UNSAFE);

    insert_native_fn!(map, "panic", panic_handler, vec![Parameter::positional_optional("message", "str", "Panic called without a message".into())], "void", EffectFlags::IO);

    insert_native_fn!(map, "is_unsafe_allowed", move |_: &HashMap<String, Value>| -> Value {
        Value::Boolean(allow_unsafe)
    }, vec![], "bool", EffectFlags::PURE);

    insert_native_fn!(map, "get_max_ptr", |_: &HashMap<String, Value>| -> Value {
        Value::Int(Int::from_i64(MAX_PTR as i64))
    }, vec![], "int", EffectFlags::PURE);

    // signals
    let signal_module = Module {
        name: "signal".to_string(),
        properties: create_signal_map(),
        parameters: Vec::new(),
        is_public: true,
        is_static: true,
        is_final: true,
        state: None,
        path: PathBuf::from("os/signal"),
    };

    insert_native_var!(map, "signal", Value::Module(signal_module), "module");

    // subprocess

    #[cfg(not(target_arch = "wasm32"))]
    let subprocess_module = Module {
        name: "subprocess".to_string(),
        properties: create_subprocess_map(),
        parameters: Vec::new(),
        is_public: true,
        is_static: true,
        is_final: true,
        state: None,
        path: PathBuf::from("os/subprocess"),
    };

    #[cfg(not(target_arch = "wasm32"))]
    insert_native_var!(map, "subprocess", Value::Module(subprocess_module), "module");

    insert_native_fn!(map, "pid", |_: &HashMap<String, Value>| -> Value {
        #[cfg(not(target_arch = "wasm32"))]
        {
            use std::process;
            Value::Int(Int::from_i64(process::id() as i64))
        }
        #[cfg(target_arch = "wasm32")]
        {
            Value::Int(Int::from_i64(0))
        }
    }, vec![], "int", EffectFlags::PURE);
    
    insert_native_var!(map, "word_size", Value::Int(Int::from_i64(std::mem::size_of::<usize>() as i64)), "int");
    insert_native_var!(map, "pointer_size", Value::Int(Int::from_i64(std::mem::size_of::<*const ()>() as i64)), "int");
    #[cfg(not(target_arch = "wasm32"))]
    insert_native_var!(map, "arch", Value::String(consts::ARCH.to_string()), "str");
    #[cfg(target_arch = "wasm32")]
    insert_native_var!(map, "arch", Value::String("wasm32".into()), "str");

    map
}
