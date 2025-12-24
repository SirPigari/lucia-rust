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
use parking_lot::RwLock;
use std::sync::Arc;
#[cfg(not(target_arch = "wasm32"))]
use {bincode::{Encode, Decode}, serde::{Serialize, Deserialize}, sha2::{Sha256, Digest}, std::path::PathBuf};

use crate::VERSION;

// This module defines the standard libraries available in the runtime environment.
// Each library is represented by a name and a description, along with its version.
// The libraries are stored in a static HashMap for easy access.

// Lucia Version 2.0.0
// This file is part of the Lucia programming language runtime.

static EXPECTED_VERSION: Lazy<String> = Lazy::new(|| format!("^{}", VERSION));

pub static _STD_LIBS: Lazy<HashMap<&'static str, LibInfo>> = Lazy::new(|| {
    let mut m = HashMap::new();

    m.insert("std", LibInfo::new(
        "Standard library for Lucia.",
        "2.0.0",
        &EXPECTED_VERSION,
    ));
    #[cfg(feature = "request")]
    m.insert("request", LibInfo::new(
        "This module provides HTTP request functionality.",
        "0.11.14",
        &EXPECTED_VERSION,
    ));
    #[cfg(feature = "math")]
    m.insert("math", LibInfo::new(
        "Provides mathematical functions and constants.",
        "1.0.0",
        &EXPECTED_VERSION,
    ));

    #[cfg(feature = "os")]
    m.insert("os", LibInfo::new(
        "Interfaces with the operating system.",
        "1.0.0",
        &EXPECTED_VERSION,
    ));

    #[cfg(feature = "time")]
    m.insert("time", LibInfo::new(
        "Handles time and date functionality.",
        "0.3.0",
        &EXPECTED_VERSION,
    ));

    #[cfg(feature = "json")]
    m.insert("json", LibInfo::new(
        "JSON parsing and serialization.",
        "1.0.82",
        &EXPECTED_VERSION,
    ));

    #[cfg(feature = "regex")]
    m.insert("regex", LibInfo::new(
        "Regular expressions for pattern matching.",
        "0.9.0",
        &EXPECTED_VERSION,
    ));

    #[cfg(feature = "collections")]
    m.insert("collections", LibInfo::new(
        "Collection of utilities.",
        "2.0.0",
        &EXPECTED_VERSION,
    ));

    #[cfg(feature = "random")]
    m.insert("random", LibInfo::new(
        "Random number generation utilities.",
        "0.7.42",
        &EXPECTED_VERSION,
    ));

    #[cfg(feature = "fs")]
    m.insert("fs", LibInfo::new(
        "File system operations and utilities.",
        "0.4.0",
        &EXPECTED_VERSION,
    ));

    #[cfg(feature = "clib")]
    m.insert("clib", LibInfo::new(
        "C standard library bindings for Lucia.",
        "0.1.69",
        &EXPECTED_VERSION,
    ));

    #[cfg(feature = "lasm")]
    m.insert("lasm", LibInfo::new(
        "Cross-platform, lightweight assembly-inspired utilities for low-level programming and direct hardware control.",
        "1.0.3",
        &EXPECTED_VERSION,
    ));

    #[cfg(feature = "nest")]
    m.insert("nest", LibInfo::new(
        "HTTP client and server utilities.",
        "1.1.0",
        &EXPECTED_VERSION,
    ));

    #[cfg(feature = "libload")]
    m.insert("libload", LibInfo::new(
        "Dynamic library loading and function invocation.",
        "1.0.0",
        &EXPECTED_VERSION,
    ));

    #[cfg(feature = "elevator")]
    m.insert("elevator", LibInfo::new(
        "Random stuff from markofwitch.",
        "42.0.0",
        &EXPECTED_VERSION,
    ));

    #[cfg(feature = "hash")]
    m.insert("hash", LibInfo::new(
        "Hashing algorithms and utilities.",
        "1.0.0",
        &EXPECTED_VERSION,
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
    LIBS_JSON.write().set_all(libs);

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

#[cfg(not(target_arch = "wasm32"))]
#[inline(always)]
pub fn is_in_std_libs(lib_name: &str) -> bool {
    _STD_LIBS.contains_key(lib_name)
}

#[cfg(not(target_arch = "wasm32"))]
#[derive(Debug, Clone, Serialize, Deserialize, Encode, Decode, PartialEq, Eq, Hash)]
struct LibName {
    manifest_hash: String,
    manifest_path: PathBuf,
    names: Vec<String>,
    original_name: String,
}

#[cfg(not(target_arch = "wasm32"))]
static LIB_NAMES: Lazy<HashMap<Arc<str>, Arc<str>>> = Lazy::new(|| build_final_map(load_or_generate_cache()));

#[cfg(not(target_arch = "wasm32"))]
fn _get_lib_names_raw() -> Vec<LibName> {
    let exe = std::env::current_exe().expect("Failed to get exe path");
    let parent = exe.parent().expect("Exe has no parent").to_path_buf();
    let grandparent = parent.parent().expect("Exe parent has no parent").to_path_buf();
    let libs_dir = grandparent.join("libs");

    let mut out = Vec::new();

    for entry in fs::read_dir(&libs_dir).unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        if !path.is_dir() {
            continue;
        }

        let folder_name = match path.file_name().and_then(|s| s.to_str()) {
            Some(v) => v.to_string(),
            None => continue,
        };

        if is_in_std_libs(&folder_name) {
            continue;
        }

        let manifest_path = path.join("manifest.json");
        if !manifest_path.exists() {
            continue;
        }

        let manifest_data = fs::read(&manifest_path).unwrap();
        let manifest: serde_json::Value = serde_json::from_slice(&manifest_data).unwrap();

        let name = manifest.get("name")
            .and_then(|v| v.as_str())
            .unwrap_or("")
            .to_string();

        let names_extra = manifest.get("names")
            .and_then(|v| v.as_array())
            .map(|arr| {
                arr.iter()
                    .filter_map(|v| v.as_str().map(|s| s.to_string()))
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default();

        let mut hasher = Sha256::new();
        hasher.update(&manifest_data);
        let hash = format!("{:x}", hasher.finalize());

        let mut names = Vec::new();
        names.push(name.clone());
        names.extend(names_extra);

        out.push(LibName {
            manifest_hash: hash,
            manifest_path,
            names,
            original_name: folder_name,
        });
    }

    out
}

#[cfg(not(target_arch = "wasm32"))]
fn cache_file_path() -> PathBuf {
    let exe = std::env::current_exe().expect("Failed to get exe path");
    let parent = exe.parent().expect("Exe has no parent").to_path_buf();
    let grandparent = parent.parent().expect("Exe parent has no parent").to_path_buf();
    grandparent.join(".cache").join("lib_names.bin")
}

#[cfg(not(target_arch = "wasm32"))]
fn load_or_generate_cache() -> Vec<LibName> {
    let cache_path = cache_file_path();

    if !cache_path.exists() {
        fs::create_dir_all(cache_path.parent().unwrap()).ok();
        let fresh = _get_lib_names_raw();
        write_cache(&cache_path, &fresh);
        return fresh;
    }

    let cached_bin = fs::read(&cache_path).unwrap_or_else(|_| Vec::new());

    if cached_bin.len() < 8 {
        return Vec::new();
    }

    let len_bytes: [u8; 8] = cached_bin[0..8].try_into().unwrap();
    let compressed_len = u64::from_le_bytes(len_bytes) as usize;

    if cached_bin.len() < 8 + compressed_len {
        return Vec::new();
    }

    let compressed_data = &cached_bin[8..8 + compressed_len];

    let decompressed = match zstd::bulk::decompress(compressed_data, 10_000_000) {
        Ok(d) => d,
        Err(_) => return Vec::new(),
    };

    let (mut cached, _) = bincode::decode_from_slice::<Vec<LibName>, _>(
        &decompressed,
        bincode::config::standard().with_little_endian().with_no_limit(),
    ).unwrap_or((Vec::new(), 0));

    let mut changed = false;
    for entry in &mut cached {
        if !entry.manifest_path.exists() {
            changed = true;
            break;
        }

        let data = match fs::read(&entry.manifest_path) {
            Ok(v) => v,
            Err(_) => { changed = true; break; }
        };

        let mut hasher = Sha256::new();
        hasher.update(&data);
        let new_hash = format!("{:x}", hasher.finalize());

        if new_hash != entry.manifest_hash {
            changed = true;
            break;
        }
    }

    if changed {
        let fresh = _get_lib_names_raw();
        write_cache(&cache_path, &fresh);
        return fresh;
    }

    cached
}

#[cfg(not(target_arch = "wasm32"))]
fn write_cache(path: &PathBuf, data: &Vec<LibName>) {
    let encoded = bincode::encode_to_vec(data, bincode::config::standard().with_little_endian().with_no_limit()).unwrap();
    let compressed = zstd::bulk::compress(&encoded, 9).unwrap();
    let mut buffer = Vec::new();
    buffer.extend_from_slice(&compressed.len().to_le_bytes());
    buffer.extend_from_slice(&compressed);
    fs::write(path, buffer).unwrap();
}

#[cfg(not(target_arch = "wasm32"))]
fn build_final_map(v: Vec<LibName>) -> HashMap<Arc<str>, Arc<str>> {
    let mut map = HashMap::new();

    for lib in v {
        let orig: Arc<str> = Arc::from(lib.original_name.as_str());
        for name in lib.names {
            map.insert(Arc::from(name.as_str()), orig.clone());
        }
    }

    map
}

#[cfg(not(target_arch = "wasm32"))]
pub fn get_lib_names() -> HashMap<Arc<str>, Arc<str>> {
    LIB_NAMES.clone()
}

#[cfg(target_arch = "wasm32")]
pub fn get_lib_names() -> HashMap<Arc<str>, Arc<str>> {
    HashMap::default()
}

// ------- Macros -------

#[macro_export]
macro_rules! insert_native_fn {
    ($map:expr, $name:expr, $handler:expr, $params:expr, $ret_type:expr, $effects:expr) => {{
        let native_fn = crate::env::runtime::functions::NativeFunction::new(
            $name,
            $handler,
            $params,
            $ret_type,
            true,
            true,
            true,
            None,
            $effects,
        );

        let func = Function::Native(std::sync::Arc::new(native_fn));

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
macro_rules! insert_native_fn_pt {
    ($map:expr, $name:expr, $handler:expr, $params:expr, $ret_type:expr, $effects:expr) => {{
        let native_fn = crate::env::runtime::functions::NativeFunction::new_pt(
            $name,
            $handler,
            $params,
            $ret_type,
            true,
            true,
            true,
            None,
            $effects,
        );

        let func = Function::Native(std::sync::Arc::new(native_fn));

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
macro_rules! insert_native_shared_fn {
    ($map:expr, $name:expr, $handler:expr, $params:expr, $ret_type:expr, $effects:expr) => {{
        let native_fn = crate::env::runtime::functions::SharedNativeFunction::new(
            $name,
            $handler,
            $params,
            $ret_type,
            true,
            true,
            true,
            None,
            $effects,
        );

        let func = Function::SharedNative(std::sync::Arc::new(native_fn));

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
    ($map:expr, $name:expr, $handler:expr, $params:expr, $ret_type:expr, $state:expr, $effects:expr) => {{
        let native_fn = crate::env::runtime::functions::NativeFunction::new(
            $name,
            $handler,
            $params,
            $ret_type,
            true,
            true,
            true,
            Some($state.to_string()),
            $effects,
        );

        let func = Function::Native(std::sync::Arc::new(native_fn));

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

#[macro_export]
macro_rules! make_native_fn_pt {
    ($name:expr, $handler:expr, $params:expr, $ret_type:expr, $effects:expr) => {{
        let native_fn = crate::env::runtime::functions::NativeFunction::new_pt(
            $name,
            $handler,
            $params,
            $ret_type,
            true,
            false,
            true,
            None,
            $effects,
        );

        Function::Native(std::sync::Arc::new(native_fn))
    }};
}

#[macro_export]
macro_rules! make_native_static_fn_pt {
    ($name:expr, $handler:expr, $params:expr, $ret_type:expr, $effects:expr) => {{
        let native_fn = crate::env::runtime::functions::NativeFunction::new_pt(
            $name,
            $handler,
            $params,
            $ret_type,
            true,
            true,
            true,
            None,
            $effects,
        );

        Function::Native(std::sync::Arc::new(native_fn))
    }};
}