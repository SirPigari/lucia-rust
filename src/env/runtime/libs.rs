use std::collections::HashMap;
use once_cell::sync::{Lazy, OnceCell};
use crate::env::runtime::internal_structs::{LibInfo, LibRegistry};
#[cfg(not(target_arch = "wasm32"))]
use crate::env::runtime::{utils::{check_version, fix_path}, config::Config};
use crate::env::runtime::config::Libs;
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
        "Cross-platform, low-level assembly capabilities through the LASM (Lucia Assembly) language.",
        "2.0.0",
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

#[cfg(all(not(target_arch = "wasm32"), not(feature = "single_executable")))]
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

#[allow(dead_code)]
#[cfg(not(target_arch = "wasm32"))]
pub fn load_std_libs_embedded() -> Result<(HashMap<String, LibInfo>, (bool, String)), (String, bool)> {
    let libs: HashMap<String, LibInfo> = _STD_LIBS.iter()
        .map(|(k, v)| (k.to_string(), v.clone()))
        .collect();

    STD_LIBS.set_all(libs.clone());

    Ok((libs, (false, String::new())))
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
#[derive(Serialize, Deserialize)]
struct CacheRow {
    names: Vec<String>,
    original_name: String,
    manifest_path: String,
    manifest_hash: String,
    file_size: u64,
    modified_unix: u64,
}

#[cfg(not(target_arch = "wasm32"))]
static LAST_LIB_NAMES: OnceCell<HashMap<Arc<str>, Arc<str>>> = OnceCell::new();
#[cfg(not(target_arch = "wasm32"))]
static _LAST_LIBS: OnceCell<Vec<String>> = OnceCell::new();

#[cfg(not(target_arch = "wasm32"))]
fn file_meta_fast(path: &Path) -> Option<(u64, u64)> {
    let meta = std::fs::metadata(path).ok()?;

    let size = meta.len();

    let modified = meta.modified().ok()?
        .duration_since(std::time::UNIX_EPOCH).ok()?
        .as_secs();

    Some((size, modified))
}

#[cfg(not(target_arch = "wasm32"))]
fn read_cache_jsonl(path: &std::path::Path) -> Vec<CacheRow> {
    use std::io::{BufRead, BufReader};

    let file = match std::fs::File::open(path) {
        Ok(f) => f,
        Err(_) => return Vec::new(),
    };

    BufReader::new(file)
        .lines()
        .filter_map(|l| serde_json::from_str(&l.ok()?).ok())
        .collect()
}

#[cfg(not(target_arch = "wasm32"))]
fn write_cache_jsonl(path: &Path, libs: &[LibName]) -> std::io::Result<()> {
    use std::io::Write;

    let tmp = path.with_extension("tmp");
    let mut f = std::fs::File::create(&tmp)?;

    for l in libs {
        let (file_size, modified_unix) =
            file_meta_fast(&l.manifest_path).unwrap_or((0,0));

        let row = CacheRow {
            names: l.names.clone(),
            original_name: l.original_name.to_string(),
            manifest_path: l.manifest_path.display().to_string(),
            manifest_hash: l.manifest_hash.clone(),
            file_size,
            modified_unix,
        };

        serde_json::to_writer(&mut f, &row)?;
        f.write_all(b"\n")?;
    }

    std::fs::rename(tmp, path)?;
    Ok(())
}

#[cfg(not(target_arch = "wasm32"))]
fn _get_lib_names_raw(libs_paths: &[String]) -> Vec<LibName> {
    // let exe = std::env::current_exe().expect("Failed to get exe path");
    // let parent = exe.parent().expect("Exe has no parent").to_path_buf();
    // let grandparent = parent.parent().expect("Exe parent has no parent").to_path_buf();
    // let libs_dir = grandparent.join("libs");
    let mut out = Vec::new();

    for path in libs_paths {
        let libs_dir = Path::new(path);
        if !libs_dir.exists() {
            return out;
        }

        dbg!(&libs_dir);

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
    }

    out
}

#[cfg(not(target_arch = "wasm32"))]
fn cache_file_path() -> PathBuf {
    let exe = std::env::current_exe().expect("Failed to get exe path");
    let parent = exe.parent().expect("Exe has no parent").to_path_buf();
    let grandparent = parent.parent().expect("Exe parent has no parent").to_path_buf();
    grandparent.join(".cache").join("lib_names.jsonl")
}

#[cfg(not(target_arch = "wasm32"))]
fn load_or_generate_cache(libs_paths: &[String]) -> Vec<LibName> {
    use std::path::Path;
    use sha2::{Sha256, Digest};

    let cache_path = cache_file_path();

    if let Some(parent) = cache_path.parent() {
        std::fs::create_dir_all(parent).ok();
    }

    let rows: Vec<CacheRow> = read_cache_jsonl(&cache_path);

    if rows.is_empty() {
        let fresh = _get_lib_names_raw(libs_paths);
        let _ = write_cache_jsonl(&cache_path, &fresh);
        return fresh;
    }

    let mut changed = false;

    for row in &rows {

        let path = Path::new(&row.manifest_path);

        if !path.exists() {
            changed = true;
            break;
        }

        let meta = match std::fs::metadata(path) {
            Ok(m) => m,
            Err(_) => { changed = true; break; }
        };

        let size = meta.len();

        let modified = match meta.modified()
            .ok()
            .and_then(|m| m.duration_since(std::time::UNIX_EPOCH).ok())
            .map(|d| d.as_secs())
        {
            Some(v) => v,
            None => { changed = true; break; }
        };

        if size == row.file_size && modified == row.modified_unix {
            continue;
        }

        let data = match std::fs::read(path) {
            Ok(v) => v,
            Err(_) => { changed = true; break; }
        };

        let mut hasher = Sha256::new();
        hasher.update(&data);
        let new_hash = format!("{:x}", hasher.finalize());

        if new_hash != row.manifest_hash {
            changed = true;
            break;
        }
    }

    if changed {
        let fresh = _get_lib_names_raw(libs_paths);
        let _ = write_cache_jsonl(&cache_path, &fresh);
        return fresh;
    }

    rows.into_iter().map(|r| LibName {
        names: r.names,
        original_name: r.original_name.into(),
        manifest_path: r.manifest_path.into(),
        manifest_hash: r.manifest_hash,
    }).collect()
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
pub fn get_lib_names(libs_paths: &[String]) -> HashMap<Arc<str>, Arc<str>> {
    if let Some(ll) = _LAST_LIBS.get() {
        if ll.as_slice() == libs_paths {
            if let Some(ln) = LAST_LIB_NAMES.get() {
                return ln.clone();
            } else {
                let v = load_or_generate_cache(libs_paths);
                let final_map = build_final_map(v);
                LAST_LIB_NAMES.set(final_map.clone()).ok();
                return final_map;
            }
        } else {
            let v = load_or_generate_cache(libs_paths);
            let final_map = build_final_map(v);
            LAST_LIB_NAMES.set(final_map.clone()).ok();
            _LAST_LIBS.set(libs_paths.iter().cloned().collect()).ok();
            return final_map;
        }
    } else {
        let v = load_or_generate_cache(libs_paths);
        let final_map = build_final_map(v);
        LAST_LIB_NAMES.set(final_map.clone()).ok();
        _LAST_LIBS.set(libs_paths.iter().cloned().collect()).ok();
        return final_map;
    }
}

#[cfg(target_arch = "wasm32")]
pub fn get_lib_names(_libs_paths: &[String]) -> HashMap<Arc<str>, Arc<str>> {
    HashMap::default()
}

// ------- Macros -------

#[macro_export]
macro_rules! insert_native_fn {
    ($map:expr, $name:expr, $docs:expr, $handler:expr, $params:expr, $ret_type:expr, $effects:expr) => {{
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
            $docs,
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
    ($map:expr, $name:expr, $handler:expr, $params:expr, $ret_type:expr, $effects:expr) => {
        insert_native_fn!(
            $map,
            $name,
            "",
            $handler,
            $params,
            $ret_type,
            $effects
        )
    };
}

#[macro_export]
macro_rules! insert_native_fn_pt {
    ($map:expr, $name:expr, $docs:expr, $handler:expr, $params:expr, $ret_type:expr, $effects:expr) => {{
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
            $docs,
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
    ($map:expr, $name:expr, $handler:expr, $params:expr, $ret_type:expr, $effects:expr) => {
        insert_native_fn_pt!(
            $map,
            $name,
            "",
            $handler,
            $params,
            $ret_type,
            $effects
        )
    };
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
            "",
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
macro_rules! insert_native_shared_fn_pt {
    ($map:expr, $name:expr, $handler:expr, $params:expr, $ret_type:expr, $effects:expr) => {{
        let native_fn = crate::env::runtime::functions::SharedNativeFunction::new_pt(
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
    ($map:expr, $name:expr, $docs:expr, $handler:expr, $params:expr, $ret_type:expr, $state:expr, $effects:expr) => {{
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
            $docs,
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
    ($map:expr, $name:expr, $handler:expr, $params:expr, $ret_type:expr, $state:expr, $effects:expr) => {
        insert_native_fn_state!(
            $map,
            $name,
            "",
            $handler,
            $params,
            $ret_type,
            $state,
            $effects
        )
    };
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
    ($name:expr, $docs:expr, $handler:expr, $params:expr, $ret_type:expr, $effects:expr) => {{
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
            $docs,
        );

        Function::Native(std::sync::Arc::new(native_fn))
    }};
    ($name:expr, $handler:expr, $params:expr, $ret_type:expr, $effects:expr) => {{
        make_native_fn_pt!(
            $name,
            "",
            $handler,
            $params,
            $ret_type,
            $effects
        )
    }};
}

#[macro_export]
macro_rules! make_native_static_fn_pt {
    ($name:expr, $docs:expr, $handler:expr, $params:expr, $ret_type:expr, $effects:expr) => {{
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
            $docs,
        );

        Function::Native(std::sync::Arc::new(native_fn))
    }};
    ($name:expr, $handler:expr, $params:expr, $ret_type:expr, $effects:expr) => {{
        make_native_static_fn_pt!(
            $name,
            "",
            $handler,
            $params,
            $ret_type,
            $effects
        )
    }};
}