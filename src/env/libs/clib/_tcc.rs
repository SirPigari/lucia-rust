use std::process::{Command, Stdio};
use std::io::Write;
use std::path::{PathBuf};
use std::env::temp_dir;
use crate::env::runtime::value::Value;
use libloading::{Library, Symbol};
use libc::c_void;
use std::fs;

// TCC (Tiny C Compiler) bindings for Lucia.
// Part of Clib module for Lucia.
// Lucia version 2.0.0, module: clib@0.1.69

fn check_code(tcc_path: &str, code: &str) -> Result<(), String> {
    let discard_output = if cfg!(target_os = "windows") {
        "nul"
    } else {
        "/dev/null"
    };

    let mut child = Command::new(tcc_path)
        .arg("-c")
        .arg("-o")
        .arg(discard_output)
        .stdin(Stdio::piped())
        .spawn()
        .map_err(|e| format!("Failed to spawn process: {}", e))?;

    if let Some(mut stdin) = child.stdin.take() {
        stdin.write_all(code.as_bytes())
            .map_err(|e| format!("Failed to write to stdin: {}", e))?;
    }

    let output = child.wait()
        .map_err(|e| format!("Failed to wait on child process: {}", e))?;

    if output.success() {
        Ok(())
    } else {
        Err(format!("tcc command failed with status: {}", output))
    }
}

pub fn compile(tcc_path: &str, code: &str, file_path: &str) -> Result<(), String> {
    let mut child = Command::new(tcc_path)
        .arg("-o")
        .arg(file_path)
        .arg("-")
        .stdin(Stdio::piped())
        .spawn()
        .map_err(|e| format!("Failed to spawn process: {}", e))?;

    if let Some(mut stdin) = child.stdin.take() {
        stdin.write_all(code.as_bytes())
            .map_err(|e| format!("Failed to write to stdin: {}", e))?;
    }

    let output = child.wait()
        .map_err(|e| format!("Failed to wait on child process: {}", e))?;

    if output.success() {
        Ok(())
    } else {
        Err(format!("tcc compile failed with status: {}", output))
    }
}

pub fn compile_shared(tcc_path: &str, code: &str, file_path: &str) -> Result<(), String> {
    let mut cmd = Command::new(tcc_path);

    if cfg!(target_os = "windows") {
        cmd.arg("-shared");
    } else {
        cmd.arg("-shared").arg("-fPIC");
    }

    cmd.arg("-o")
        .arg(file_path)
        .arg("-")
        .stdin(Stdio::piped());

    let mut child = cmd.spawn()
        .map_err(|e| format!("Failed to spawn process: {}", e))?;

    if let Some(mut stdin) = child.stdin.take() {
        stdin.write_all(code.as_bytes())
            .map_err(|e| format!("Failed to write to stdin: {}", e))?;
    }

    let output = child.wait()
        .map_err(|e| format!("Failed to wait on child process: {}", e))?;

    if output.success() {
        Ok(())
    } else {
        Err(format!("tcc shared lib compile failed with status: {}", output))
    }
}


pub fn run(tcc_path: &str, code: &str) -> Result<Value, String> {
    let mut temp_exe_path = temp_dir();

    if cfg!(target_os = "windows") {
        temp_exe_path.push("temp_tcc_exec.exe");
    } else {
        temp_exe_path.push("temp_tcc_exec");
    }

    compile(tcc_path, code, temp_exe_path.to_str().unwrap())?;

    let output = Command::new(&temp_exe_path)
        .output()
        .map_err(|e| format!("Failed to execute compiled code: {}", e))?;

    if let Err(e) = fs::remove_file(&temp_exe_path) {
        eprintln!("Warning: failed to delete temp executable: {}", e);
    }

    if output.status.success() {
        let stdout_str = String::from_utf8_lossy(&output.stdout).to_string();
        Ok(Value::String(stdout_str))
    } else {
        Err(format!("Running compiled code failed: {}", output.status))
    }
}

pub fn export(tcc_path: &str, file_path: &str, symbol_name: &str, _expected_type: &str) -> Result<Value, String> {
    let lib = unsafe { Library::new(file_path) }
        .map_err(|e| format!("Failed to load library: {}", e))?;

    unsafe {
        let symbol: Symbol<*mut c_void> = lib.get(symbol_name.as_bytes())
            .map_err(|e| format!("Failed to find symbol: {}", e))?;

        let ptr_val = *symbol as *const c_void as i64;
        Ok(Value::Int(ptr_val.into()))
    }
}


pub fn check(tcc_path: &str, code: &str) -> Result<bool, String> {
    match check_code(tcc_path, code) {
        Ok(_) => Ok(true),
        Err(_) => Ok(false),
    }
}
