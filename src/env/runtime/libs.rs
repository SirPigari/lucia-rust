use std::collections::HashMap;
use once_cell::sync::Lazy;
use crate::env::runtime::internal_structs::{LibInfo, LibRegistry};
#[cfg(not(target_arch = "wasm32"))]
use crate::env::runtime::utils::{check_version, fix_path};
use crate::env::runtime::config::Libs;
#[cfg(not(target_arch = "wasm32"))]
use crate::env::runtime::config::Config;
#[cfg(not(target_arch = "wasm32"))]
use std::path::Path;
#[cfg(not(target_arch = "wasm32"))]
use std::fs;
use std::sync::RwLock;

// This module defines the standard libraries available in the runtime environment.
// Each library is represented by a name and a description, along with its version.
// The libraries are stored in a static HashMap for easy access.

// Lucia Version 2.0.0
// This file is part of the Lucia programming language runtime.

pub static _STD_LIBS: Lazy<HashMap<&'static str, LibInfo>> = Lazy::new(|| {
    let mut m = HashMap::new();

    m.insert("math", LibInfo::new(
        "Provides mathematical functions and constants.",
        "1.0.0",
        "^2.0.0",
    ));

    m.insert("os", LibInfo::new(
        "Interfaces with the operating system.",
        "1.0.0",
        "^2.0.0",
    ));

    m.insert("time", LibInfo::new(
        "Handles time and date functionality.",
        "0.3.0",
        "^2.0.0",
    ));

    m.insert("json", LibInfo::new(
        "JSON parsing and serialization.",
        "1.0.82",
        "^2.0.0",
    ));

    m.insert("config", LibInfo::new(
        "Lucia configuration management.",
        "0.2.6",
        "^2.0.0",
    ));

    m.insert("regex", LibInfo::new(
        "Regular expressions for pattern matching.",
        "0.9.0",
        "^2.0.0",
    ));

    m.insert("collections", LibInfo::new(
        "Collection of utilities.",
        "1.0.0",
        "^2.0.0",
    ));

    m.insert("random", LibInfo::new(
        "Random number generation utilities.",
        "0.7.42",
        "^2.0.0",
    ));

    m.insert("fs", LibInfo::new(
        "File system operations and utilities.",
        "0.4.0",
        "^2.0.0",
    ));

    m.insert("clib", LibInfo::new(
        "C standard library bindings for Lucia.",
        "0.1.69",
        "^2.0.0",
    ));

    m.insert("lasm", LibInfo::new(
        "Cross-platform, lightweight assembly-inspired utilities for low-level programming and direct hardware control.",
        "1.0.3",
        "^2.0.0",
    ));

    m.insert("nest", LibInfo::new(
        "HTTP client and server utilities.",
        "1.1.0",
        "^2.0.0",
    ));

    m.insert("libload", LibInfo::new(
        "Dynamic library loading and function invocation.",
        "1.0.0",
        "^2.0.0",
    ));

    m
});

pub static STD_LIBS: Lazy<LibRegistry> = Lazy::new(|| {
    LibRegistry::new()
});

#[allow(dead_code)]
pub static LIBS_JSON: Lazy<RwLock<Libs>> = Lazy::new(|| {
    RwLock::new(Libs::new())
});

#[cfg(target_arch = "wasm32")]
pub fn load_std_libs() -> Result<HashMap<String, LibInfo>, String> {
    let libs: HashMap<String, LibInfo> = _STD_LIBS.iter()
        .map(|(k, v)| (k.to_string(), v.clone()))
        .collect();

    STD_LIBS.set_all(libs.clone());

    Ok(libs)
}

#[cfg(not(target_arch = "wasm32"))]
pub fn load_std_libs(file: &str, moded: bool) -> Result<(HashMap<String, LibInfo>, (bool, String)), (String, bool)> {
    let content = fs::read_to_string(Path::new(file))
        .map_err(|e| (format!("Failed to read libs file: {}", e), false))?;

    let json: serde_json::Value = serde_json::from_str(&content)
        .map_err(|e| (format!("Failed to parse JSON: {}", e), false))?;

    let std_libs = json.get("std_libs")
        .ok_or(("Missing 'std_libs' field in JSON".to_string(), false))?;

    let parsed: HashMap<String, LibInfo> = serde_json::from_value(std_libs.clone())
        .map_err(|e| (format!("Failed to parse 'std_libs': {}", e), false))?;

    let mut dirty_libs = (false, String::new());

    for (name, lib) in &parsed {
        match _STD_LIBS.get(name.as_str()) {
            Some(orig) => {
                if lib != orig {
                    if !moded {
                        return Err((format!("Library '{}' does not match standard definition", name), true));
                    } else {
                        dirty_libs = (true, name.to_string());
                    }
                }
            }
            None => return Err((format!("Unknown standard library '{}'", name), true)),
        }
    }

    let libs = serde_json::from_str::<Libs>(&content)
        .map_err(|e| (format!("Failed to parse libs JSON: {}", e), false))?;

    STD_LIBS.set_all(parsed.clone());
    LIBS_JSON.write().unwrap().set_all(libs);

    Ok((parsed, dirty_libs))
}

#[cfg(not(target_arch = "wasm32"))]
pub fn check_project_deps(
    project_deps: &HashMap<String, String>, // dep_name -> expected_version
    libs_dir: &Path,
    config: &Config,
) -> Result<(), (String, String)> {
    for (dep_name, expected_version) in project_deps {
        if let Some(std_lib) = STD_LIBS.get(dep_name.as_str()) {
            if !check_version(&config.version, &std_lib.expected_lucia_version) {
                return Err((format!(
                    "Standard library '{}' requires Lucia version '{}', but current is '{}'.",
                    dep_name, std_lib.expected_lucia_version, config.version
                ), dep_name.clone()));
            }

            if !check_version(&std_lib.version, expected_version) {
                return Err((format!(
                    "Standard library '{}' version '{}' does not satisfy required '{}'.",
                    dep_name, std_lib.version, expected_version
                ), dep_name.clone()));
            }

            continue;
        }

        let dep_path = libs_dir.join(dep_name);
        if !dep_path.exists() {
            return Err((format!(
                "Dependency '{}' listed in manifest not found in '{}'.",
                dep_name,
                &fix_path(libs_dir.display().to_string())
            ), dep_name.clone()));
        }

        let dep_manifest_path = dep_path.join("manifest.json");
        let dep_manifest_json = match std::fs::read_to_string(&dep_manifest_path)
            .ok()
            .and_then(|content| serde_json::from_str::<serde_json::Value>(&content).ok())
        {
            Some(json) => json,
            None => {
                return Err((format!(
                    "Failed to read manifest for dependency '{}'.",
                    dep_name
                ), dep_name.clone()));
            }
        };

        let current_version = match dep_manifest_json
            .get("version")
            .and_then(|v| v.as_str())
        {
            Some(v) => v,
            None => {
                return Err((format!(
                    "No version field found in manifest for dependency '{}'.",
                    dep_name
                ), dep_name.clone()));
            }
        };

        if !check_version(current_version, expected_version) {
            return Err((format!(
                "Dependency '{}' version '{}' does not satisfy required '{}'.",
                dep_name, current_version, expected_version
            ), dep_name.clone()));
        }
    }

    Ok(())
}

// ------- Macros -------

#[macro_export]
macro_rules! insert_native_fn {
    ($map:expr, $name:expr, $handler:expr, $params:expr, $ret_type:expr) => {{
        let native_fn = NativeFunction::new(
            $name,
            $handler,
            $params,
            $ret_type,
            true,
            true,
            true,
            None,
        );

        let func = Function::Native(Arc::new(native_fn));

        $map.insert(
            $name.to_string(),
            Variable::new(
                $name.to_string(),
                Value::Function(func),
                "function".to_string(),
                true,
                true,
                true,
            ),
        );
    }};
}

#[macro_export]
macro_rules! insert_native_fn_state {
    ($map:expr, $name:expr, $handler:expr, $params:expr, $ret_type:expr, $state:expr) => {{
        let native_fn = NativeFunction::new(
            $name,
            $handler,
            $params,
            $ret_type,
            true,
            true,
            true,
            Some($state.to_string()),
        );

        let func = Function::Native(Arc::new(native_fn));

        $map.insert(
            $name.to_string(),
            Variable::new(
                $name.to_string(),
                Value::Function(func),
                "function".to_string(),
                true,
                true,
                true,
            ),
        );
    }};
}

#[macro_export]
macro_rules! insert_native_var {
    ($map:expr, $name:expr, $value:expr, $typ:expr) => {{
        $map.insert(
            $name.to_string(),
            Variable::new(
                $name.to_string(),
                $value,
                $typ.to_string(),
                true,
                true,
                true,
            ),
        );
    }};
}
