use std::collections::HashMap;
use std::sync::Arc;
use crate::env::runtime::functions::{Function, Parameter};
use crate::env::runtime::types::Int;
use crate::env::runtime::value::Value;
use crate::env::runtime::variables::Variable;
use crate::env::runtime::config::{get_from_config, Config};
use crate::env::runtime::internal_structs::EffectFlags;
use crate::env::runtime::utils::{MAX_PTR};
use crate::env::runtime::modules::Module;
#[cfg(not(target_arch = "wasm32"))]
use rustc_hash::FxHashMap;
use crate::{insert_native_fn, insert_native_var, CUSTOM_PANIC_MARKER};
#[cfg(not(target_arch = "wasm32"))]
use std::io::Write;
#[cfg(not(target_arch = "wasm32"))]
use crossterm::event::{poll, read, Event, KeyCode};
#[cfg(not(target_arch = "wasm32"))]
use crate::{make_native_fn_pt, make_native_static_fn_pt, insert_native_fn_pt};
#[cfg(not(target_arch = "wasm32"))]
use crate::env::runtime::structs_and_enums::{Struct, Enum};
use parking_lot::Mutex;
#[cfg(not(target_arch = "wasm32"))]
use std::process::{Command, Stdio};
use std::path::PathBuf;
#[cfg(not(target_arch = "wasm32"))]
use std::{path::Path, io::Read, time::{Instant, Duration}};
#[cfg(not(target_arch = "wasm32"))]
use once_cell::sync::Lazy;

#[cfg(not(target_arch = "wasm32"))]
use crate::env::runtime::statements::Statement;
#[cfg(not(target_arch = "wasm32"))]
use crate::env::runtime::types::Type;

#[cfg(not(target_arch = "wasm32"))]
use std::env::consts;
#[cfg(not(target_arch = "wasm32"))]
use crate::env::runtime::utils::{to_static, supports_color};
#[cfg(not(target_arch = "wasm32"))]
use sys_info;

#[cfg(not(target_arch = "wasm32"))]
static LAST_KEY_TIME_AND_CODE: Lazy<Mutex<(Instant, Option<KeyCode>)>> = Lazy::new(|| Mutex::new((Instant::now(), None)));

#[cfg(not(target_arch = "wasm32"))]
fn wrap_string_fn<F>(func: F) -> impl Fn(&HashMap<String, Value>) -> Value
where
    F: Fn() -> Result<String, sys_info::Error> + Send + Sync + 'static,
{
    move |_| match func() {
        Ok(s) => Value::String(s),
        Err(e) => Value::new_error("OSError", e.to_string(), None),
    }
}

#[cfg(not(target_arch = "wasm32"))]
fn wrap_u64_fn<F>(func: F) -> impl Fn(&HashMap<String, Value>) -> Value
where
    F: Fn() -> Result<u64, sys_info::Error> + Send + Sync + 'static,
{
    move |_| match func() {
        Ok(v) => Value::Int(Int::from_i64(v as i64)),
        Err(e) => Value::new_error("OSError", e.to_string(), None),
    }
}

#[cfg(not(target_arch = "wasm32"))]
fn wrap_usize_fn<F>(func: F) -> impl Fn(&HashMap<String, Value>) -> Value
where
    F: Fn() -> Result<usize, sys_info::Error> + Send + Sync + 'static,
{
    move |_| match func() {
        Ok(v) => Value::Int(Int::from_i64(v as i64)),
        Err(e) => Value::new_error("OSError", e.to_string(), None),
    }
}

#[cfg(not(target_arch = "wasm32"))]
fn get_logged_user(_: &HashMap<String, Value>) -> Value {
    if let Ok(name) = std::env::var("USERNAME") {
        return Value::String(name);
    }
    if let Ok(name) = std::env::var("USER") {
        return Value::String(name);
    }

    Value::String("unknown".to_string())
}

fn to_ptr(ptr: usize, allow_unsafe: bool) -> Value {
    if !allow_unsafe {
        return Value::new_error(
            "TypeError",
            "This function is unsafe. Enable allow_unsafe to use it.",
            None,
        );
    }

    if ptr < 0x1000 || ptr > MAX_PTR {
        return Value::new_error("ValueError", "Pointer value is out of valid range", None);
    }

    unsafe {
        let arc_ptr = ptr as *const Mutex<(Value, usize)>;
        let arc_ref: Arc<Mutex<(Value, usize)>> = Arc::from_raw(arc_ptr);
        let cloned_arc = Arc::clone(&arc_ref);
        std::mem::forget(arc_ref);
        Value::Pointer(cloned_arc)
    }
}

fn from_ptr(ptr: usize, allow_unsafe: bool) -> Value {
    if !allow_unsafe {
        return Value::new_error(
            "TypeError",
            "This function is unsafe. Enable allow_unsafe to use it.",
            None,
        );
    }

    if ptr < 0x1000 || ptr > MAX_PTR {
        return Value::new_error("ValueError", "Pointer value is out of valid range", None);
    }

    unsafe {
        let arc_ptr = ptr as *const Mutex<Value>;
        let arc_ref: Arc<Mutex<Value>> = Arc::from_raw(arc_ptr);
        let val = arc_ref.lock().clone();
        std::mem::forget(arc_ref);
        val
    }
}

fn panic_handler(args: &HashMap<String, Value>) -> Value {
    if let Some(Value::String(message)) = args.get("message") {
        let mut marked_message = String::new();
        marked_message.push(CUSTOM_PANIC_MARKER as char);
        marked_message.push_str(message);
        panic!("{}", marked_message);
    }
    Value::new_error("PanicError", "Panic called", None)
}

#[cfg(unix)]
fn create_signal_map() -> HashMap<String, Variable> {
    let mut map = HashMap::with_capacity(23);
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
                Err(_) => return Value::new_error("TypeError", "Invalid 'sig' integer value", None),
            },
            _ => return Value::new_error("TypeError", "Expected 'sig' to be an integer", None),
        };
        let pid = match args.get("pid") {
            Some(Value::Int(int)) => match int.to_i64() {
                Ok(v) => v,
                Err(_) => return Value::new_error("TypeError", "Invalid 'pid' integer value", None),
            },
            _ => return Value::new_error("TypeError", "Expected 'pid' to be an integer", None),
        };

        let res = unsafe { libc::kill(pid as libc::pid_t, signum as libc::c_int) };
        if res == 0 {
            Value::Boolean(true)
        } else {
            Value::new_error("OSError", to_static(format!("Failed to send signal {} to pid {}", signum, pid)), None)
        }
    }, vec![Parameter::positional("pid", "int"), Parameter::positional("sig", "int")], "bool", EffectFlags::IO);
    map
}

#[cfg(windows)]
fn create_signal_map() -> HashMap<String, Variable> {
    let mut map = HashMap::with_capacity(7);
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
                Err(_) => return Value::new_error("TypeError", "Invalid 'sig' integer value", None),
            },
            _ => return Value::new_error("TypeError", "Expected 'sig' to be an integer", None),
        };

        let pid = match args.get("pid") {
            Some(Value::Int(int)) => match int.to_i64() {
                Ok(v) => v,
                Err(_) => return Value::new_error("TypeError", "Invalid 'pid' integer value", None),
            },
            _ => return Value::new_error("TypeError", "Expected 'pid' to be an integer", None),
        };

        let current_pid = process::id() as i64;

        if pid == current_pid {
            let res = unsafe { raise(signum as i32) };
            if res == 0 {
                Value::Boolean(true)
            } else {
                Value::new_error("OSError", to_static(format!("Failed to raise signal {} on self", signum)), None)
            }
        } else {
            if signum != 15 {
                return Value::new_error("OSError", "Only SIGTERM (15) can be sent to other processes", None);
            }

            match process::Command::new("taskkill")
                .args(&["/PID", &pid.to_string(), "/F"])
                .output()
            {
                Ok(output) if output.status.success() => Value::Boolean(true),
                Ok(output) => {
                    let err_msg = String::from_utf8_lossy(&output.stderr).trim().trim_start_matches("ERROR: ").to_string();
                    Value::new_error(
                        "OSError",
                        format!("Failed to terminate process {}: {}", pid, err_msg),
                        None,
                    )
                }
                Err(e) => Value::new_error(
                    "OSError",
                    format!("Failed to run taskkill for {}: {}", pid, e),
                    None,
                ),
            }
        }
    }, vec![Parameter::positional("pid", "int"), Parameter::positional("sig", "int")], "bool", EffectFlags::IO);
    map
}

#[cfg(target_arch = "wasm32")]
fn create_signal_map() -> HashMap<String, Variable> {
    let mut map = HashMap::with_capacity(7);
    insert_native_var!(map, "SIGABRT", Value::Int(Int::from_i64(6)),  "int");
    insert_native_var!(map, "SIGFPE",  Value::Int(Int::from_i64(8)),  "int");
    insert_native_var!(map, "SIGILL",  Value::Int(Int::from_i64(4)),  "int");
    insert_native_var!(map, "SIGINT",  Value::Int(Int::from_i64(2)),  "int");
    insert_native_var!(map, "SIGSEGV", Value::Int(Int::from_i64(11)), "int");
    insert_native_var!(map, "SIGTERM", Value::Int(Int::from_i64(15)), "int");

    insert_native_fn!(map, "send", |_args: &HashMap<String, Value>| -> Value {
        Value::new_error("OSError", "Signal handling is not supported in WebAssembly environment", None)
    }, 
    vec![Parameter::positional("pid", "int"), Parameter::positional("sig", "int")], 
    "bool", EffectFlags::IO);
    map
}

#[cfg(not(target_arch = "wasm32"))]
fn create_subprocess_map() -> HashMap<String, Variable> {
    let mut map = HashMap::with_capacity(7);

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

    let exitcode_method = ("exitcode".to_string(),
        make_native_fn_pt!("exitcode", |args: &HashMap<String, Value>| -> Value {
            if let Some(Value::Struct(instance)) = args.get("self") {
                if let Some(val) = instance.get_field("returncode") {
                    return val.clone();
                }
            }
            Value::new_error("AttributeError", "Failed to get exitcode", None)
        }, vec![Parameter::instance("self", &process_output_struct, vec![])], &Type::new_simple("int"), EffectFlags::PURE)
    );

    let runned_method = ("runned".to_string(),
        make_native_fn_pt!("runned", |args: &HashMap<String, Value>| -> Value {
            if let Some(Value::Struct(instance)) = args.get("self") {
                match instance.get_field("shellerr") {
                    Some(Value::String(s)) => return Value::Boolean(s.is_empty()),
                    _ => {}
                }
            }
            Value::new_error("AttributeError", "Failed to get runned status", None)
        }, vec![Parameter::instance("self", &process_output_struct, vec![])], &Type::new_simple("bool"), EffectFlags::PURE)
    );

    if let Type::Struct { methods, .. } = &mut process_output_struct {
        *methods = vec![exitcode_method, runned_method];
    }

    let closure_struct = process_output_struct.clone();

    insert_native_fn_pt!(map, "popen", move |args: &HashMap<String, Value>| -> Value {
        let args_val = match args.get("args") {
            Some(Value::List(list)) => list.iter().map(|v| {
                if let Value::String(s) = v {
                    s.clone()
                } else {
                    "".to_string()
                }
            }).collect::<Vec<String>>(),
            _ => return Value::new_error("TypeError", "Expected 'args' to be a list of strings", None),
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
                Value::Struct(Box::new(Struct::new_with_fields(
                    closure_struct.clone(),
                    output_map,
                )))
            }
            Err(e) => {
                let mut output_map: HashMap<String, (Box<Value>, Type)> = HashMap::new();
                output_map.insert("pid".to_string(), (Box::new(Value::Int(Int::from_i64(-1))), Type::new_simple("int")));
                output_map.insert("stdout".to_string(), (Box::new(Value::String("".to_string())), Type::new_simple("str")));
                output_map.insert("stderr".to_string(), (Box::new(Value::String("".to_string())), Type::new_simple("str")));
                output_map.insert("returncode".to_string(), (Box::new(Value::Int(Int::from_i64(-1))), Type::new_simple("int")));
                output_map.insert("shellerr".to_string(), (Box::new(Value::String(e.to_string())), Type::new_simple("str")));
                Value::Struct(Box::new(Struct::new_with_fields(
                    closure_struct.clone(),
                    output_map,
                )))
            }
        }
    }, vec![
        Parameter::positional("args", "list"),
        Parameter::positional_optional("shell", "bool", Value::Boolean(false)),
        Parameter::positional_optional_pt("cwd", &Type::new_simple("?str"), Value::Null),
        Parameter::positional_optional("capture_output", "bool", Value::Boolean(false))
    ], &process_output_struct, EffectFlags::IO);

    insert_native_fn!(map, "run", |args: &HashMap<String, Value>| -> Value {
        let command = match args.get("command") {
            Some(Value::String(s)) => s,
            _ => return Value::new_error("TypeError", "Expected 'command' to be a string", None),
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
            Err(e) => Value::new_error(
                "RuntimeError",
                to_static(format!("Failed to execute command: {}", e)),
                None,
            ),
        }
    }, vec![Parameter::positional("command", "str")], "int", EffectFlags::IO);

    let redirect_enum = Type::Enum {
        name: "Redirect".to_string(),
        variants: vec![
            ("Pipe".to_string(), Statement::Null, 0),
            ("Inherit".to_string(), Statement::Null, 1),
            ("Null".to_string(), Statement::Null, 2),
            ("File".to_string(), Statement::make_value(Value::Type(Type::new_simple("str"))), 3),
        ],
        generics: Vec::new(),
        wheres: Vec::new(),
    };

    let mut cmd_struct = Type::Struct {
        name: "Cmd".to_string(),
        fields: vec![
            ("args".to_string(), Statement::make_value(Value::Type(Type::new_simple("list"))), vec![]),
            ("stdin".to_string(), Statement::make_value(Value::Type(redirect_enum.clone())), vec![]),
            ("stdout".to_string(), Statement::make_value(Value::Type(redirect_enum.clone())), vec![]),
            ("stderr".to_string(), Statement::make_value(Value::Type(redirect_enum.clone())), vec![]),
        ],
        methods: vec![],
        generics: Vec::new(),
        wheres: Vec::new(),
    };

    let process_output_struct_clone = process_output_struct.clone();

    let run_inside = move |args: &HashMap<String, Value>| -> Value {
        let self_struct = match args.get("self") {
            Some(Value::Struct(s)) => s,
            _ => return Value::new_error("TypeError", "Expected 'self' to be a struct", None),
        };

        let args_val = match self_struct.get_field("args") {
            Some(Value::List(list)) => list.iter().filter_map(|v| {
                if let Value::String(s) = v { Some(s.clone()) } else { None }
            }).collect::<Vec<String>>(),
            _ => return Value::new_error("TypeError", "Expected 'args' to be a list", None),
        };

        if args_val.is_empty() {
            return Value::new_error("ValueError", "Command args list is empty", None);
        }

        let stdin_val = self_struct.get_field("stdin");
        let stdout_val = self_struct.get_field("stdout");
        let stderr_val = self_struct.get_field("stderr");

        let mut shellerr = String::new();

        let map_redirect = |v: Option<&Value>, shellerr: &mut String| -> Stdio {
            match v {
                Some(Value::Enum(e)) => match e.variant.0 {
                    0 => Stdio::piped(),
                    1 => Stdio::inherit(),
                    2 => Stdio::null(),
                    3 => { // File
                        match *e.variant.1 {
                            Value::String(ref path) => match std::fs::File::create(path) {
                                Ok(f) => Stdio::from(f),
                                Err(err) => {
                                    shellerr.push_str(&format!("Failed to create file '{}': {}\n", path, err));
                                    Stdio::null()
                                }
                            },
                            _ => {
                                shellerr.push_str("File variant payload must be a string\n");
                                Stdio::null()
                            }
                        }
                    },
                    _ => {
                        shellerr.push_str(&format!("Unknown Redirect variant index: {}\n", e.variant.0));
                        Stdio::null()
                    }
                },
                _ => Stdio::null(),
            }
        };

        let mut command = Command::new(&args_val[0]);
        if args_val.len() > 1 {
            command.args(&args_val[1..]);
        }

        command.stdin(map_redirect(stdin_val, &mut shellerr));
        command.stdout(map_redirect(stdout_val, &mut shellerr));
        command.stderr(map_redirect(stderr_val, &mut shellerr));

        let mut output_map: HashMap<String, (Box<Value>, Type)> = HashMap::new();

        match command.spawn() {
            Ok(mut child) => {
                let mut stdout = String::new();
                let mut stderr = String::new();

                if let Some(ref mut out) = child.stdout {
                    std::io::BufReader::new(out).read_to_string(&mut stdout).ok();
                }
                if let Some(ref mut err) = child.stderr {
                    std::io::BufReader::new(err).read_to_string(&mut stderr).ok();
                }

                let status = child.wait().unwrap();
                let returncode = status.code().unwrap_or(-1);

                output_map.insert("pid".to_string(), (Box::new(Value::Int(Int::from_i64(child.id() as i64))), Type::new_simple("int")));
                output_map.insert("stdout".to_string(), (Box::new(Value::String(stdout)), Type::new_simple("str")));
                output_map.insert("stderr".to_string(), (Box::new(Value::String(stderr)), Type::new_simple("str")));
                output_map.insert("returncode".to_string(), (Box::new(Value::Int(Int::from_i64(returncode as i64))), Type::new_simple("int")));
                output_map.insert("shellerr".to_string(), (Box::new(Value::String(shellerr)), Type::new_simple("str")));

                Value::Struct(Box::new(Struct::new_with_fields(process_output_struct_clone.clone(), output_map)))
            }
            Err(e) => {
                output_map.insert("pid".to_string(), (Box::new(Value::Int(Int::from_i64(-1))), Type::new_simple("int")));
                output_map.insert("stdout".to_string(), (Box::new(Value::String("".to_string())), Type::new_simple("str")));
                output_map.insert("stderr".to_string(), (Box::new(Value::String("".to_string())), Type::new_simple("str")));
                output_map.insert("returncode".to_string(), (Box::new(Value::Int(Int::from_i64(-1))), Type::new_simple("int")));
                output_map.insert("shellerr".to_string(), (Box::new(Value::String(format!("Failed to spawn command: {}\n{}", e, shellerr))), Type::new_simple("str")));

                Value::Struct(Box::new(Struct::new_with_fields(process_output_struct_clone.clone(), output_map)))
            }
        }
    };


    let run_method = ("run".to_string(), 
        make_native_fn_pt!("run", run_inside.clone(), vec![Parameter::instance("self", &cmd_struct, vec![])], &process_output_struct, EffectFlags::IO)
    );

    let output_method = ("output".to_string(),
        make_native_fn_pt!("output", run_inside.clone(), vec![Parameter::instance("self", &cmd_struct, vec![])], &process_output_struct, EffectFlags::IO)
    );

    let stdout_get = ("stdout".to_string(),
        make_native_fn_pt!("stdout", |args: &HashMap<String, Value>| {
            if let Some(Value::Struct(s)) = args.get("self") {
                s.get_field("stdout").cloned().unwrap_or(Value::Null)
            } else { Value::Null }
        }, vec![Parameter::instance("self", &cmd_struct, vec![])], &redirect_enum, EffectFlags::PURE)
    );

    let stdout_set = ("set_stdout".to_string(),
        make_native_fn_pt!("set_stdout", |args: &HashMap<String, Value>| {
            if let Some(Value::Struct(s)) = args.get("self") {
                if let Some(val) = args.get("value") {
                    let mut s = s.clone();
                    s.set("stdout".to_string(), val.clone());
                    return Value::Struct(s);
                }
            }
            Value::new_error("TypeError", "Failed to set stdout", None)
        }, vec![Parameter::instance("self", &cmd_struct, vec![]), Parameter::positional_pt("value", &redirect_enum)], &cmd_struct, EffectFlags::PURE)
    );

    let stdin_get = ("stdin".to_string(),
        make_native_fn_pt!("stdin", |args: &HashMap<String, Value>| {
            if let Some(Value::Struct(s)) = args.get("self") {
                s.get_field("stdin").cloned().unwrap_or(Value::Null)
            } else { Value::Null }
        }, vec![Parameter::instance("self", &cmd_struct, vec![])], &redirect_enum, EffectFlags::PURE)
    );

    let stdin_set = ("set_stdin".to_string(),
        make_native_fn_pt!("set_stdin", |args: &HashMap<String, Value>| {
            if let Some(Value::Struct(s)) = args.get("self") {
                if let Some(val) = args.get("value") {
                    let mut s = s.clone();
                    s.set("stdin".to_string(), val.clone());
                    return Value::Struct(s);
                }
            }
            Value::new_error("TypeError", "Failed to set stdin", None)
        }, vec![Parameter::instance("self", &cmd_struct, vec![]), Parameter::positional_pt("value", &redirect_enum)], &cmd_struct, EffectFlags::PURE)
    );

    let stderr_get = ("stderr".to_string(),
        make_native_fn_pt!("stderr", |args: &HashMap<String, Value>| {
            if let Some(Value::Struct(s)) = args.get("self") {
                s.get_field("stderr").cloned().unwrap_or(Value::Null)
            } else { Value::Null }
        }, vec![Parameter::instance("self", &cmd_struct, vec![])], &redirect_enum, EffectFlags::PURE)
    );

    let stderr_set = ("set_stderr".to_string(),
        make_native_fn_pt!("set_stderr", |args: &HashMap<String, Value>| {
            if let Some(Value::Struct(s)) = args.get("self") {
                if let Some(val) = args.get("value") {
                    let mut s = s.clone();
                    s.set("stderr".to_string(), val.clone());
                    return Value::Struct(s);
                }
            }
            Value::new_error("TypeError", "Failed to set stderr", None)
        }, vec![Parameter::instance("self", &cmd_struct, vec![]), Parameter::positional_pt("value", &redirect_enum)], &cmd_struct, EffectFlags::PURE)
    );

    let args_set = ("args".to_string(),
        make_native_fn_pt!("args", |args_map: &HashMap<String, Value>| {
            if let Some(Value::Struct(s)) = args_map.get("self") {
                if let Some(Value::List(list)) = args_map.get("value") {
                    let mut s = s.clone();
                    s.set("args".to_string(), Value::List(list.clone()));
                    return Value::Struct(s);
                }
            }
            Value::new_error("TypeError", "Failed to set args", None)
        }, vec![
            Parameter::instance("self", &cmd_struct, vec![]),
            Parameter::positional_pt("value", &Type::Indexed {
                base_type: Box::new(Type::new_simple("list")),
                elements: vec![Type::new_simple("str")],
            })
        ], &cmd_struct, EffectFlags::PURE)
    );

    let arg_append = ("arg".to_string(),
        make_native_fn_pt!("arg", |args_map: &HashMap<String, Value>| {
            if let Some(Value::Struct(s)) = args_map.get("self") {
                if let Some(Value::String(val)) = args_map.get("value") {
                    let mut current_args = match s.get_field("args") {
                        Some(Value::List(list)) => list.clone(),
                        _ => vec![],
                    };
                    current_args.push(Value::String(val.clone()));
                    let mut s = s.clone();
                    s.set("args".to_string(), Value::List(current_args));
                    return Value::Struct(s);
                }
            }
            Value::new_error("TypeError", "Failed to append arg", None)
        }, vec![
            Parameter::instance("self", &cmd_struct, vec![]),
            Parameter::positional("value", "str")
        ], &cmd_struct, EffectFlags::PURE)
    );

    let redirect_enum_clone = redirect_enum.clone();
    let clear = ("clear".to_string(),
        make_native_fn_pt!("clear", move |args: &HashMap<String, Value>| {
            if let Some(Value::Struct(s)) = args.get("self") {
                let mut s = s.clone();
                s.set("args".to_string(), Value::List(vec![]));
                s.set("stdin".to_string(), Value::Enum(Box::new(Enum::new(redirect_enum_clone.clone(), (1, Value::Null)))));
                s.set("stdout".to_string(), Value::Enum(Box::new(Enum::new(redirect_enum_clone.clone(), (1, Value::Null)))));
                s.set("stderr".to_string(), Value::Enum(Box::new(Enum::new(redirect_enum_clone.clone(), (1, Value::Null)))));
                return Value::Struct(s);
            }
            Value::new_error("TypeError", "Failed to clear args", None)
        }, vec![Parameter::instance("self", &cmd_struct, vec![])], &cmd_struct, EffectFlags::PURE)
    );

    let args_clear = ("clear_args".to_string(), 
        make_native_fn_pt!("clear_args", move |args: &HashMap<String, Value>| {
            if let Some(Value::Struct(s)) = args.get("self") {
                let mut s = s.clone();
                s.set("args".to_string(), Value::List(vec![]));
                return Value::Struct(s);
            }
            Value::new_error("TypeError", "Failed to clear args", None)
        }, vec![Parameter::instance("self", &cmd_struct, vec![])], &cmd_struct, EffectFlags::PURE)
    );

    
    if let Type::Struct { methods, .. } = &mut cmd_struct {
        *methods = vec![
            run_method,
            output_method,
            stdout_get, stdout_set,
            stdin_get, stdin_set,
            stderr_get, stderr_set,
            args_set,
            arg_append,
            clear,
            args_clear
        ];
    }

    let default_cmd_instance = Arc::new(Struct::new_with_fields(
        cmd_struct.clone(),
        HashMap::from_iter(vec![
            ("args".to_string(), (Box::new(Value::List(vec![])), Type::new_simple("list"))),
            ("stdin".to_string(), (Box::new(Value::Enum(Box::new(Enum::new(redirect_enum.clone(), (1, Value::Null))))), redirect_enum.clone())),
            ("stdout".to_string(), (Box::new(Value::Enum(Box::new(Enum::new(redirect_enum.clone(), (1, Value::Null))))), redirect_enum.clone())),
            ("stderr".to_string(), (Box::new(Value::Enum(Box::new(Enum::new(redirect_enum.clone(), (1, Value::Null))))), redirect_enum.clone())),
        ])
    ));
    
    // I HATE FUCKING RUSTS CLOSURE SHIT AND THSI FUCKING CODEBASE WHY IN THE FUCK DID I HAVE TO SPEND A HOUR TRYING TO CREATE A NEW STATIC METHOD
    let new_cmd_closure: Box<dyn Fn(&HashMap<String, Value>) -> Value + Send + Sync + 'static> =
        Box::new({
            let default_cmd_instance = Arc::clone(&default_cmd_instance);
            move |_| -> Value {
                Value::Struct(Box::new((*default_cmd_instance).clone()))
            }
        });
        
        let new_cmd_method = ("new".to_string(),
        make_native_static_fn_pt!("new", new_cmd_closure, vec![], &cmd_struct, EffectFlags::PURE)
    );

    if let Type::Struct { methods, .. } = &mut cmd_struct {
        methods.push(new_cmd_method);
    }
    
    insert_native_var!(map, "Redirect", Value::Type(redirect_enum.clone()), "type");
    insert_native_var!(map, "Cmd", Value::Type(cmd_struct.clone()), "type");
    insert_native_var!(map, "ProcessOutput", Value::Type(process_output_struct.clone()), "type");
    
    map
}

#[cfg(not(target_arch = "wasm32"))]
fn create_terminal_map() -> HashMap<String, Variable> {
    let mut map = HashMap::new();

    insert_native_fn!(map, "isatty", |args: &HashMap<String, Value>| -> Value {
        let fd = match args.get("fd") {
            Some(Value::Int(int)) => match int.to_i64() {
                Ok(v) => v,
                Err(_) => return Value::new_error("TypeError", "Invalid 'fd' integer value", None),
            },
            _ => return Value::new_error("TypeError", "Expected 'fd' to be an integer", None),
        };

        let res = unsafe { libc::isatty(fd as libc::c_int) };
        Value::Boolean(res != 0)
    }, vec![Parameter::positional("fd", "int")], "bool", EffectFlags::IO);
    insert_native_fn!(map, "supports_color", |_: &HashMap<String, Value>| -> Value {
        Value::Boolean(supports_color())
    }, vec![], "bool", EffectFlags::IO);

    insert_native_fn!(map, "get_terminal_size", |_: &HashMap<String, Value>| -> Value {
        match crossterm::terminal::size() {
            Ok((cols, rows)) => {
                Value::Tuple(vec![
                    Value::Int(Int::from_i64(cols as i64)),
                    Value::Int(Int::from_i64(rows as i64)),
                ])
            }
            Err(e) => Value::new_error("OSError", e.to_string(), None),
        }
    }, vec![], "tuple", EffectFlags::IO);
    insert_native_fn!(map, "clear_screen", |_: &HashMap<String, Value>| -> Value {
        match crossterm::execute!(
            std::io::stdout(),
            crossterm::terminal::Clear(crossterm::terminal::ClearType::All)
        ) {
            Ok(_) => Value::Boolean(true),
            Err(e) => Value::new_error("OSError", e.to_string(), None),
        }
    }, vec![], "bool", EffectFlags::IO);

    insert_native_fn!(map, "reset_screen", |_: &HashMap<String, Value>| -> Value {
        match crossterm::execute!(
            std::io::stdout(),
            crossterm::terminal::Clear(crossterm::terminal::ClearType::Purge),
            crossterm::cursor::MoveTo(0,0)
        ) {
            Ok(_) => Value::Boolean(true),
            Err(e) => Value::new_error("OSError", e.to_string(), None),
        }
    }, vec![], "bool", EffectFlags::IO);
    insert_native_fn!(map, "beep", |_: &HashMap<String, Value>| -> Value {
        let mut out = std::io::stdout();
        match crossterm::execute!(out, crossterm::style::Print("\x07")) {
            Ok(_) => {
                let _ = out.flush();
                Value::Boolean(true)
            },
            Err(e) => Value::new_error("OSError", e.to_string(), None),
        }
    }, vec![], "bool", EffectFlags::IO);
    insert_native_fn!(map, "hide_cursor", |_: &HashMap<String, Value>| -> Value {
        match crossterm::execute!(
            std::io::stdout(),
            crossterm::cursor::Hide
        ) {
            Ok(_) => Value::Boolean(true),
            Err(e) => Value::new_error("OSError", e.to_string(), None),
        }
    }, vec![], "bool", EffectFlags::IO);
    insert_native_fn!(map, "show_cursor", |_: &HashMap<String, Value>| -> Value {
        match crossterm::execute!(
            std::io::stdout(),
            crossterm::cursor::Show
        ) {
            Ok(_) => Value::Boolean(true),
            Err(e) => Value::new_error("OSError", e.to_string(), None),
        }
    }, vec![], "bool", EffectFlags::IO);
    insert_native_fn!(map, "flush", |args: &HashMap<String, Value>| -> Value {
        let fd = match args.get("fd") {
            Some(Value::Int(i)) => match i.to_i64() {
                Ok(v) => v,
                Err(_) => return Value::new_error("TypeError", "Invalid fd integer", None),
            },
            _ => 1,
        };

        match fd {
            1 => match std::io::stdout().flush() {
                Ok(_) => Value::Boolean(true),
                Err(e) => Value::new_error("OSError", e.to_string(), None),
            },
            2 => match std::io::stderr().flush() {
                Ok(_) => Value::Boolean(true),
                Err(e) => Value::new_error("OSError", e.to_string(), None),
            },
            _ => Value::new_error("OSError", "Only fd 0, 1, 2 are supported", None),
        }
    }, vec![Parameter::positional_optional("fd", "int", Value::Int(Int::from(1)))], "bool", EffectFlags::IO);
    
    let key_enum = Type::Enum {
        name: "KeyCode".to_string(),
        variants: vec![
            ("Up".to_string(), Statement::Null, 0),
            ("Down".to_string(), Statement::Null, 1),
            ("Left".to_string(), Statement::Null, 2),
            ("Right".to_string(), Statement::Null, 3),
            ("Enter".to_string(), Statement::Null, 4),
            ("Esc".to_string(), Statement::Null, 5),
            ("Char".to_string(), Statement::make_value(Value::Type(Type::new_simple("str"))), 6),
            ("Space".to_string(), Statement::Null, 7),
            ("Backspace".to_string(), Statement::Null, 8),
            ("Tab".to_string(), Statement::Null, 9),
            ("Delete".to_string(), Statement::Null, 10),
            ("Insert".to_string(), Statement::Null, 11),
            ("Home".to_string(), Statement::Null, 12),
            ("End".to_string(), Statement::Null, 13),
            ("PageUp".to_string(), Statement::Null, 14),
            ("PageDown".to_string(), Statement::Null, 15),
            ("F1".to_string(), Statement::Null, 16),
            ("F2".to_string(), Statement::Null, 17),
            ("F3".to_string(), Statement::Null, 18),
            ("F4".to_string(), Statement::Null, 19),
            ("F5".to_string(), Statement::Null, 20),
            ("F6".to_string(), Statement::Null, 21),
            ("F7".to_string(), Statement::Null, 22),
            ("F8".to_string(), Statement::Null, 23),
            ("F9".to_string(), Statement::Null, 24),
            ("F10".to_string(), Statement::Null, 25),
            ("F11".to_string(), Statement::Null, 26),
            ("F12".to_string(), Statement::Null, 27),
            ("None".to_string(), Statement::Null, 28),
            ("Other".to_string(), Statement::make_value(Value::Type(Type::new_simple("str"))), 29),
        ],
        generics: Vec::new(),
        wheres: Vec::new(),
    };
    insert_native_var!(map, "KeyCode", Value::Type(key_enum.clone()), "type");

    let key_enum_clone = key_enum.clone();
    insert_native_fn_pt!(map, "read_key", move |args: &HashMap<String, Value>| -> Value {
        let timeout_ms = match args.get("timeout") {
            Some(Value::Int(int)) => match int.to_i64() { Ok(v) => v, Err(_) => return Value::new_error("TypeError", "Invalid 'timeout' integer value", None) },
            _ => 0,
        };
        let blocking = match args.get("blocking") {
            Some(Value::Boolean(b)) => *b,
            _ => true,
        };

        let poll_timeout = if blocking { Duration::from_secs(1000) } else { Duration::from_millis(timeout_ms as u64) };

        let key_event_opt = match poll(poll_timeout) {
            Ok(true) => match read() {
                Ok(Event::Key(k)) => Some(k),
                _ => None,
            },
            Ok(false) => None,
            Err(_) => None,
        };

        if let Some(key_event) = key_event_opt {
            let now = Instant::now();
            let mut last = LAST_KEY_TIME_AND_CODE.lock();
            if let Some(last_code) = last.1 {
                if last_code == key_event.code && now.duration_since(last.0) < Duration::from_millis(timeout_ms as u64) {
                    return Value::Enum(Box::new(Enum::new(key_enum_clone.clone(), (27, Value::Null))));
                }
            }
            *last = (now, Some(key_event.code));

            let key_enum = match key_event.code {
                KeyCode::Up => Enum::new(key_enum_clone.clone(), (0, Value::Null)),
                KeyCode::Down => Enum::new(key_enum_clone.clone(), (1, Value::Null)),
                KeyCode::Left => Enum::new(key_enum_clone.clone(), (2, Value::Null)),
                KeyCode::Right => Enum::new(key_enum_clone.clone(), (3, Value::Null)),
                KeyCode::Enter => Enum::new(key_enum_clone.clone(), (4, Value::Null)),
                KeyCode::Esc => Enum::new(key_enum_clone.clone(), (5, Value::Null)),
                KeyCode::Char(c) if c == ' ' => Enum::new(key_enum_clone.clone(), (7, Value::Null)), // Space
                KeyCode::Char(c) => Enum::new(key_enum_clone.clone(), (6, Value::String(c.to_string()))),
                KeyCode::Backspace => Enum::new(key_enum_clone.clone(), (8, Value::Null)),
                KeyCode::Tab => Enum::new(key_enum_clone.clone(), (9, Value::Null)),
                KeyCode::Delete => Enum::new(key_enum_clone.clone(), (10, Value::Null)),
                KeyCode::Insert => Enum::new(key_enum_clone.clone(), (11, Value::Null)),
                KeyCode::Home => Enum::new(key_enum_clone.clone(), (12, Value::Null)),
                KeyCode::End => Enum::new(key_enum_clone.clone(), (13, Value::Null)),
                KeyCode::PageUp => Enum::new(key_enum_clone.clone(), (14, Value::Null)),
                KeyCode::PageDown => Enum::new(key_enum_clone.clone(), (15, Value::Null)),
                KeyCode::F(n) if (1..=12).contains(&n) => Enum::new(key_enum_clone.clone(), (15 + n as usize, Value::Null)),
                _ => Enum::new(key_enum_clone.clone(), (29, Value::String(format!("{:?}", key_event.code)))),
            };
            return Value::Enum(Box::new(key_enum));
        }

        Value::Enum(Box::new(Enum::new(key_enum_clone.clone(), (27, Value::Null))))
    }, vec![
        Parameter::positional_optional("timeout", "int", Value::Int(Int::from(0))),
        Parameter::positional_optional("blocking", "bool", Value::Boolean(true))
    ], &key_enum, EffectFlags::IO);
    insert_native_fn!(map, "move_cursor", |args: &HashMap<String, Value>| -> Value {
        let x = match args.get("x") {
            Some(Value::Int(int)) => match int.to_i64() {
                Ok(v) => v as u16,
                Err(_) => return Value::new_error("TypeError", "Invalid 'x' integer value", None),
            },
            _ => return Value::new_error("TypeError", "Expected 'x' to be an integer", None),
        };
        let y = match args.get("y") {
            Some(Value::Int(int)) => match int.to_i64() {
                Ok(v) => v as u16,
                Err(_) => return Value::new_error("TypeError", "Invalid 'y' integer value", None),
            },
            _ => return Value::new_error("TypeError", "Expected 'y' to be an integer", None),
        };

        match crossterm::execute!(
            std::io::stdout(),
            crossterm::cursor::MoveTo(x, y)
        ) {
            Ok(_) => Value::Boolean(true),
            Err(e) => Value::new_error("OSError", to_static(format!("{}", e)), None),
        }
    }, vec![
        Parameter::positional("x", "int"),
        Parameter::positional("y", "int")
    ], "bool", EffectFlags::IO);
    insert_native_fn!(map, "get_cursor_position", |_: &HashMap<String, Value>| -> Value {
        match crossterm::cursor::position() {
            Ok((x, y)) => {
                Value::Tuple(vec![
                    Value::Int(Int::from_i64(x as i64)),
                    Value::Int(Int::from_i64(y as i64)),
                ])
            }
            Err(e) => Value::new_error("OSError", to_static(format!("{}", e)), None),
        }
    }, vec![], "tuple", EffectFlags::IO);
    insert_native_fn!(map, "enable_raw_mode", |_: &HashMap<String, Value>| -> Value {
        match crossterm::terminal::enable_raw_mode() {
            Ok(_) => Value::Boolean(true),
            Err(e) => Value::new_error("OSError", to_static(format!("{}", e)), None),
        }
    }, vec![], "bool", EffectFlags::IO);
    insert_native_fn!(map, "disable_raw_mode", |_: &HashMap<String, Value>| -> Value {
        match crossterm::terminal::disable_raw_mode() {
            Ok(_) => Value::Boolean(true),
            Err(e) => Value::new_error("OSError", to_static(format!("{}", e)), None),
        }
    }, vec![], "bool", EffectFlags::IO);
    insert_native_fn!(map, "clear_line", |_: &HashMap<String, Value>| -> Value {
        match crossterm::execute!(
            std::io::stdout(),
            crossterm::terminal::Clear(crossterm::terminal::ClearType::CurrentLine)
        ) {
            Ok(_) => Value::Boolean(true),
            Err(e) => Value::new_error("OSError", to_static(format!("{}", e)), None),
        }
    }, vec![], "bool", EffectFlags::IO);
    insert_native_fn!(map, "flush_input_buffer", |_: &HashMap<String, Value>| -> Value {
        match crossterm::event::poll(std::time::Duration::from_millis(0)) {
            Ok(_) => {
                while crossterm::event::poll(std::time::Duration::from_millis(0)).unwrap_or(false) {
                    let _ = crossterm::event::read();
                }
                Value::Boolean(true)
            }
            Err(e) => Value::new_error("OSError", to_static(format!("{}", e)), None),
        }
    }, vec![], "bool", EffectFlags::IO);
    insert_native_fn!(map, "scroll", |args: &HashMap<String, Value>| -> Value {
        let lines = match args.get("lines") {
            Some(Value::Int(int)) => match int.to_i64() {
                Ok(v) => v as i16,
                Err(_) => return Value::new_error("TypeError", "Invalid 'lines' integer value", None),
            },
            _ => return Value::new_error("TypeError", "Expected 'lines' to be an integer", None),
        };

        if lines > 0 {
            match crossterm::execute!(
                std::io::stdout(),
                crossterm::terminal::ScrollDown(lines as u16)
            ) {
                Ok(_) => Value::Boolean(true),
                Err(e) => Value::new_error("OSError", to_static(format!("{}", e)), None),
            }
        } else {
            match crossterm::execute!(
                std::io::stdout(),
                crossterm::terminal::ScrollUp(lines.abs() as u16)
            ) {
                Ok(_) => Value::Boolean(true),
                Err(e) => Value::new_error("OSError", to_static(format!("{}", e)), None),
            }
        }
    }, vec![
        Parameter::positional("lines", "int")
    ], "bool", EffectFlags::IO);
    insert_native_fn!(map, "set_title", |args: &HashMap<String, Value>| -> Value {
        let title = match args.get("title") {
            Some(Value::String(s)) => s,
            _ => return Value::new_error("TypeError", "Expected 'title' to be a string", None),
        };
        
        use crossterm::ExecutableCommand;

        let mut out = std::io::stdout();
        match out.execute(crossterm::terminal::SetTitle(title)) {
            Ok(_) => Value::Boolean(true),
            Err(e) => Value::new_error("OSError", to_static(format!("{}", e)), None),
        }
    }, vec![
        Parameter::positional("title", "str")
    ], "bool", EffectFlags::IO);
    insert_native_fn!(map, "print_to_fd", |args: &HashMap<String, Value>| -> Value {
        let fd = match args.get("fd") {
            Some(Value::Int(int)) => match int.to_i64() {
                Ok(v) => v,
                Err(_) => return Value::new_error("TypeError", "Invalid 'fd' integer value", None),
            },
            _ => return Value::new_error("TypeError", "Expected 'fd' to be an integer", None),
        };
        let message = match args.get("message") {
            Some(Value::String(s)) => s,
            _ => return Value::new_error("TypeError", "Expected 'message' to be a string", None),
        };

        #[cfg(unix)]
        {
            use std::os::unix::io::FromRawFd;
            use std::io::Write;

            unsafe {
                let mut file = std::fs::File::from_raw_fd(fd as libc::c_int);
                match file.write_all(message.as_bytes()) {
                    Ok(_) => Value::Boolean(true),
                    Err(e) => Value::new_error("OSError", to_static(format!("{}", e)), None),
                }
            }
        }
        #[cfg(windows)]
        {
            use std::os::windows::io::FromRawHandle;
            use std::io::Write;

            unsafe {
                let handle = fd as std::os::windows::io::RawHandle;
                if handle.is_null() {
                    return Value::new_error("OSError", "Invalid file descriptor", None);
                }

                let mut file = std::fs::File::from_raw_handle(handle);
                let result = file.write_all(message.as_bytes());
                std::mem::forget(file);

                match result {
                    Ok(_) => Value::Boolean(true),
                    Err(e) => Value::new_error("OSError", format!("{}", e), None),
                }
            }
        }
    }, vec![
        Parameter::positional("fd", "int"),
        Parameter::positional("message", "str")
    ], "bool", EffectFlags::IO);

    map
}

pub fn register(config: &Config) -> HashMap<String, Variable> {
    let mut map = HashMap::new();

    #[cfg(not(target_arch = "wasm32"))]
    {
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
                    let mut inner_map = FxHashMap::with_capacity_and_hasher(3, Default::default());
                    inner_map.insert(Value::String("one".to_string()), Value::Float(l.one.into()));
                    inner_map.insert(Value::String("five".to_string()), Value::Float(l.five.into()));
                    inner_map.insert(Value::String("fifteen".to_string()), Value::Float(l.fifteen.into()));
                    Value::Map(inner_map)
                }
                Err(e) => Value::new_error("OSError", e.to_string(), None),
            }
        }, vec![], "map", EffectFlags::IO);

        insert_native_fn!(map, "disk_info", |_: &HashMap<String, Value>| -> Value {
            match sys_info::disk_info() {
                Ok(d) => {
                    let mut inner_map = FxHashMap::with_capacity_and_hasher(2, Default::default());
                    inner_map.insert(Value::String("total".to_string()), Value::Int(Int::from_i64(d.total as i64)));
                    inner_map.insert(Value::String("free".to_string()), Value::Int(Int::from_i64(d.free as i64)));
                    Value::Map(inner_map)
                }
                Err(e) => Value::new_error("OSError", e.to_string(), None),
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
            Value::Int(Int::from_i64(1))
        }, vec![], "int", EffectFlags::IO);

        insert_native_fn!(map, "mem_total", |_: &HashMap<String, Value>| -> Value {
            Value::Int(Int::from_i64(0))
        }, vec![], "int", EffectFlags::IO);

        insert_native_fn!(map, "time_now", |_: &HashMap<String, Value>| -> Value {
            let d = Date::new_0();
            Value::Float(d.get_time().into())
        }, vec![], "float", EffectFlags::IO);
    }

    let allow_unsafe = matches!(get_from_config(config, "allow_unsafe"), Value::Boolean(true));
    
    insert_native_fn!(map, "from_ptr", move |args: &HashMap<String, Value>| -> Value {
        if let Some(Value::Int(int)) = args.get("ptr") {
            if let Ok(ptr) = int.to_i64() {
                from_ptr(ptr as usize, allow_unsafe)
            } else {
                Value::new_error("TypeError", "Invalid 'ptr' integer value", None)
            }
        } else {
            Value::new_error("TypeError", "Expected 'ptr' to be an integer", None)
        }
    }, vec![Parameter::positional("ptr", "int")], "any", EffectFlags::UNSAFE);

    insert_native_fn!(map, "to_ptr", move |args: &HashMap<String, Value>| -> Value {
        if let Some(Value::Int(int)) = args.get("ptr") {
            if let Ok(ptr) = int.to_i64() {
                to_ptr(ptr as usize, allow_unsafe)
            } else {
                Value::new_error("TypeError", "Invalid 'ptr' integer value", None)
            }
        } else {
            Value::new_error("TypeError", "Expected 'ptr' to be an integer", None)
        }
    }, vec![Parameter::positional("ptr", "int")], "any", EffectFlags::UNSAFE);

    insert_native_fn!(map, "panic", panic_handler, vec![Parameter::positional_optional("message", "str", "Panic called without a message".into())], "void", EffectFlags::IO);
    #[cfg(not(target_arch = "wasm32"))]
    insert_native_fn!(map, "get_logged_user", get_logged_user, vec![], "str", EffectFlags::IO);

    insert_native_fn!(map, "is_unsafe_allowed", move |_: &HashMap<String, Value>| -> Value {
        Value::Boolean(allow_unsafe)
    }, vec![], "bool", EffectFlags::PURE);

    insert_native_fn!(map, "get_max_ptr", |_: &HashMap<String, Value>| -> Value {
        Value::Int(Int::from_i64(MAX_PTR as i64))
    }, vec![], "int", EffectFlags::PURE);

    insert_native_fn!(map, "exit", |args: &HashMap<String, Value>| -> Value {
        let code = match args.get("code") {
            Some(Value::Int(int)) => match int.to_i64() {
                Ok(v) => v as i32,
                Err(_) => return Value::new_error("TypeError", "Invalid 'code' integer value", None),
            },
            _ => 0,
        };
        std::process::exit(code);
        #[allow(unreachable_code)]
        Value::Null
    }, vec![Parameter::positional_optional("code", "int", Value::Int(Int::from_i64(0)))], "void", EffectFlags::PURE);

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

    // terminal
    #[cfg(not(target_arch = "wasm32"))]
    let terminal_module = Module {
        name: "terminal".to_string(),
        properties: create_terminal_map(),
        parameters: Vec::new(),
        is_public: true,
        is_static: true,
        is_final: true,
        state: None,
        path: PathBuf::from("os/terminal"),
    };

    #[cfg(not(target_arch = "wasm32"))]
    insert_native_var!(map, "terminal", Value::Module(terminal_module), "module");

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
