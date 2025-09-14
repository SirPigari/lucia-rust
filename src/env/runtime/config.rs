#![allow(dead_code)]

use serde::{Serialize, Deserialize};
use std::fs::File;
use std::io::Read;
use std::path::{Path, PathBuf};
use crate::env::runtime::value::Value;
use crate::env::runtime::types::Int;
use crate::env::runtime::utils::to_static;
use std::default::Default;
use std::collections::HashMap;
use crate::env::runtime::internal_structs::{LibInfo, CacheFormat};

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Config {
    pub moded: bool,
    pub debug: bool,
    pub debug_mode: String,
    pub supports_color: bool,
    pub use_lucia_traceback: bool,
    pub warnings: bool,
    pub cache_format: CacheFormat,
    pub allow_fetch: bool,
    pub allow_unsafe: bool,
    pub allow_inline_config: bool,
    pub home_dir: String,
    pub stack_size: usize,
    pub version: String,
    pub type_checker: TypeCheckerConfig,
    pub color_scheme: ColorScheme,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct ColorScheme {
    pub exception: String,
    pub warning: String,
    pub help: String,
    pub debug: String,
    pub input_arrows: String,
    pub note: String,
    pub output_text: String,
    pub info: String,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(default)]
pub struct TypeCheckerConfig {
    pub enabled: bool,
    pub strict: bool,
    pub run_unchecked: bool,
    pub nested_functions: bool,
    pub nested_loops: bool,
    pub warn_on_any: bool,
    pub warnings: bool,
    pub treat_warnings_as_errors: bool,
    pub max_errors: Option<i32>,
    pub ignore_warnings: Vec<String>,
    pub ignore_errors: Vec<String>,
    pub ignore_modules: Vec<String>,
    pub pointer_types: bool,
    pub experimental_constant_folding: bool,
    pub check_imports: bool,
    pub allow_dynamic_casts: bool,
    pub log_level: String,
    pub verbose: bool,
    pub track_value_origins: bool,
    pub fail_fast: bool,
    pub max_nested_depth: Option<i32>,
    pub try_to_auto_fix: bool,
}

impl Default for TypeCheckerConfig {
    fn default() -> Self {
        Self {
            enabled: false,
            strict: true,
            run_unchecked: false,
            nested_functions: true,
            nested_loops: true,
            warn_on_any: false,
            warnings: true,
            treat_warnings_as_errors: false,
            max_errors: None,
            ignore_warnings: vec![],
            ignore_errors: vec![],
            ignore_modules: vec![],
            pointer_types: true,
            experimental_constant_folding: false,
            check_imports: true,
            allow_dynamic_casts: false,
            log_level: "error".to_string(),
            verbose: false,
            track_value_origins: true,
            fail_fast: true,
            max_nested_depth: None,
            try_to_auto_fix: false,
        }
    }
}

impl Default for Config {
    fn default() -> Self {
        Self {
            version: env!("CARGO_PKG_VERSION").to_string(),
            moded: false,
            debug: false,
            debug_mode: "normal".to_string(),
            supports_color: true,
            use_lucia_traceback: true,
            cache_format: CacheFormat::NoCache,
            warnings: true,
            allow_fetch: true,
            allow_unsafe: false,
            allow_inline_config: false,
            home_dir: "lucia/src/env/".to_string(),
            stack_size: 16777216, // 16 MB
            type_checker: TypeCheckerConfig::default(),
            color_scheme: ColorScheme {
                exception: "#F44350".to_string(),
                warning: "#F5F534".to_string(),
                help: "#21B8DB".to_string(),
                debug: "#434343".to_string(),
                input_arrows: "#136163".to_string(),
                note: "#1CC58B".to_string(),
                output_text: "#BCBEC4".to_string(),
                info: "#9209B3".to_string(),
            },
        }
    }
}

pub fn set_in_config(config: &mut Config, key: &str, value: Value) -> Result<(), String> {
    match key {
        "moded" => {
            if let Value::Boolean(val) = value {
                config.moded = val;
                Ok(())
            } else {
                Err("Expected a boolean value for 'moded'".to_string())
            }
        }
        "debug" => {
            if let Value::Boolean(val) = value {
                config.debug = val;
                Ok(())
            } else {
                Err("Expected a boolean value for 'debug'".to_string())
            }
        }
        "debug_mode" => {
            if let Value::String(val) = value {
                config.debug_mode = val;
                Ok(())
            } else {
                Err("Expected a string value for 'debug_mode'".to_string())
            }
        }
        "supports_color" => {
            if let Value::Boolean(val) = value {
                config.supports_color = val;
                Ok(())
            } else {
                Err("Expected a boolean value for 'supports_color'".to_string())
            }
        }
        "use_lucia_traceback" => {
            if let Value::Boolean(val) = value {
                config.use_lucia_traceback = val;
                Ok(())
            } else {
                Err("Expected a boolean value for 'use_lucia_traceback'".to_string())
            }
        }
        "warnings" => {
            if let Value::Boolean(val) = value {
                config.warnings = val;
                Ok(())
            } else {
                Err("Expected a boolean value for 'warnings'".to_string())
            }
        }
        "cache_format" => {
            if let Value::String(val) = value {
                config.cache_format = CacheFormat::from_str(&val).ok_or_else(|| {
                    format!("Invalid cache format: {}", val)
                })?;
                Ok(())
            } else {
                Err("Expected a string value for 'cache_format'".to_string())
            }
        }
        "allow_fetch" => {
            if let Value::Boolean(val) = value {
                config.allow_fetch = val;
                Ok(())
            } else {
                Err("Expected a boolean value for 'allow_fetch'".to_string())
            }
        }
        "allow_unsafe" => {
            if let Value::Boolean(val) = value {
                config.allow_unsafe = val;
                Ok(())
            } else {
                Err("Expected a boolean value for 'allow_unsafe'".to_string())
            }
        }
        "allow_inline_config" => {
            if let Value::Boolean(val) = value {
                config.allow_inline_config = val;
                Ok(())
            } else {
                Err("Expected a boolean value for 'allow_inline_config'".to_string())
            }
        }
        "home_dir" => {
            if let Value::String(val) = value {
                config.home_dir = val;
                Ok(())
            } else {
                Err("Expected a string value for 'home_dir'".to_string())
            }
        }
        "stack_size" => {
            if let Value::Int(val) = value {
                if let Ok(i64_val) = val.to_i64() {
                    config.stack_size = i64_val as usize;
                    Ok(())
                } else {
                    Err("Expected an integer value for 'stack_size'".to_string())
                }
            } else {
                Err("Expected an integer value for 'stack_size'".to_string())
            }
        }
        "version" => {
            if let Value::String(val) = value {
                config.version = val;
                Ok(())
            } else {
                Err("Expected a string value for 'version'".to_string())
            }
        }
        "type_checker" => {
            if let Value::Map { keys, values } = value {
                if keys.len() == 21 && values.len() == 21 {
                    config.type_checker = parse_type_checker(&Value::Map { keys, values })?;
                    Ok(())
                } else {
                    Err("Expected a map with 21 type checker entries".to_string())
                }
            } else {
                Err("Expected a map for 'type_checker'".to_string())
            }
        }
        "color_scheme" => {
            if let Value::Map { keys, values } = value {
                if keys.len() == 8 && values.len() == 8 {
                    config.color_scheme = ColorScheme {
                        exception: values[0].to_string(),
                        warning: values[1].to_string(),
                        help: values[2].to_string(),
                        debug: values[3].to_string(),
                        input_arrows: values[4].to_string(),
                        note: values[5].to_string(),
                        output_text: values[6].to_string(),
                        info: values[7].to_string(),
                    };
                    Ok(())
                } else {
                    Err("Expected a map with 9 color scheme entries".to_string())
                }
            } else {
                Err("Expected a map for 'color_scheme'".to_string())
            }
        }
        _ => Err(format!("Key '{}' not found in config", key)),
    }
}

pub fn get_from_config(config: &Config, key: &str) -> Value {
    match key {
        "moded" => Value::Boolean(config.moded),
        "debug" => Value::Boolean(config.debug),
        "debug_mode" => Value::String(config.debug_mode.clone()),
        "supports_color" => Value::Boolean(config.supports_color),
        "use_lucia_traceback" => Value::Boolean(config.use_lucia_traceback),
        "warnings" => Value::Boolean(config.warnings),
        "cache_format" => Value::String(config.cache_format.to_string()),
        "allow_fetch" => Value::Boolean(config.allow_fetch),
        "allow_unsafe" => Value::Boolean(config.allow_unsafe),
        "allow_inline_config" => Value::Boolean(config.allow_inline_config),
        "home_dir" => Value::String(config.home_dir.clone()),
        "stack_size" => Value::Int(Int::from_i64(config.stack_size as i64)),
        "version" => Value::String(config.version.clone()),
        "type_checker" => {
            let tc = &config.type_checker;
            Value::Map {
                keys: vec![
                    Value::String("enabled".to_string()),
                    Value::String("strict".to_string()),
                    Value::String("run_unchecked".to_string()),
                    Value::String("nested_functions".to_string()),
                    Value::String("nested_loops".to_string()),
                    Value::String("warn_on_any".to_string()),
                    Value::String("warnings".to_string()),
                    Value::String("treat_warnings_as_errors".to_string()),
                    Value::String("max_errors".to_string()),
                    Value::String("ignore_warnings".to_string()),
                    Value::String("ignore_errors".to_string()),
                    Value::String("ignore_modules".to_string()),
                    Value::String("pointer_types".to_string()),
                    Value::String("experimental_constant_folding".to_string()),
                    Value::String("check_imports".to_string()),
                    Value::String("allow_dynamic_casts".to_string()),
                    Value::String("log_level".to_string()),
                    Value::String("verbose".to_string()),
                    Value::String("track_value_origins".to_string()),
                    Value::String("fail_fast".to_string()),
                    Value::String("max_nested_depth".to_string()),
                    Value::String("try_to_auto_fix".to_string()),
                ],
                values: vec![
                    Value::Boolean(tc.enabled),
                    Value::Boolean(tc.strict),
                    Value::Boolean(tc.run_unchecked),
                    Value::Boolean(tc.nested_functions),
                    Value::Boolean(tc.nested_loops),
                    Value::Boolean(tc.warn_on_any),
                    Value::Boolean(tc.warnings),
                    Value::Boolean(tc.treat_warnings_as_errors),
                    match tc.max_errors {
                        Some(v) => Value::Int(Int::from_i64(v as i64)),
                        None => Value::Null,
                    },
                    Value::List(tc.ignore_warnings.iter().map(|s| Value::String(s.clone())).collect()),
                    Value::List(tc.ignore_errors.iter().map(|s| Value::String(s.clone())).collect()),
                    Value::List(tc.ignore_modules.iter().map(|s| Value::String(s.clone())).collect()),
                    Value::Boolean(tc.pointer_types),
                    Value::Boolean(tc.experimental_constant_folding),
                    Value::Boolean(tc.check_imports),
                    Value::Boolean(tc.allow_dynamic_casts),
                    Value::String(tc.log_level.clone()),
                    Value::Boolean(tc.verbose),
                    Value::Boolean(tc.track_value_origins),
                    Value::Boolean(tc.fail_fast),
                    match tc.max_nested_depth {
                        Some(v) => Value::Int(Int::from_i64(v as i64)),
                        None => Value::Null,
                    },
                    Value::Boolean(tc.try_to_auto_fix),
                ],
            }
        }
        "color_scheme" => {
            let color_scheme = &config.color_scheme;
            Value::Map {
                keys: vec![
                    Value::String("exception".to_string()),
                    Value::String("warning".to_string()),
                    Value::String("help".to_string()),
                    Value::String("debug".to_string()),
                    Value::String("input_arrows".to_string()),
                    Value::String("note".to_string()),
                    Value::String("output_text".to_string()),
                    Value::String("info".to_string()),
                ],
                values: vec![
                    Value::String(color_scheme.exception.clone()),
                    Value::String(color_scheme.warning.clone()),
                    Value::String(color_scheme.help.clone()),
                    Value::String(color_scheme.debug.clone()),
                    Value::String(color_scheme.input_arrows.clone()),
                    Value::String(color_scheme.note.clone()),
                    Value::String(color_scheme.output_text.clone()),
                    Value::String(color_scheme.info.clone()),
                ],
            }
        }
        _ => Value::Error(
            "KeyError",
            to_static(format!("Key '{}' not found in config", key)),
            None,
        ),
    }
}

pub fn get_config_path() -> String {
    let source_file_path = Path::new(file!());

    let source_dir = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join(source_file_path)
        .parent()
        .map(Path::to_path_buf)
        .unwrap_or_else(|| PathBuf::from(env!("CARGO_MANIFEST_DIR")));

    let config_path = source_dir.join("../config.json");

    if config_path.exists() {
        config_path.to_string_lossy().into_owned()
    } else {
        eprintln!("Config file not found at {}", config_path.display());
        "config.json".to_string()
    }
}

pub fn get_config() -> Result<Config, String> {
    let path = get_config_path();
    if !Path::new(&path).exists() {
        return Err("Config file not found".to_string());
    }
    let mut file = File::open(path).map_err(|_| "Config file not found".to_string())?;
    let mut contents = String::new();
    file.read_to_string(&mut contents).map_err(|_| "Failed to read config file".to_string())?;

    serde_json::from_str::<Config>(&contents)
        .map_err(|e| format!("Failed to deserialize JSON: {}", e))
}

pub fn get_version() -> String {
    let config = get_config();
    match config {
        Ok(cfg) => cfg.version,
        Err(e) => {
            eprintln!("Error loading config: {}", e);
            "Unknown version".to_string()
        }
    }
}

fn parse_type_checker(value: &Value) -> Result<TypeCheckerConfig, String> {
    let map = if let Value::Map { keys, values } = value {
        if keys.len() != values.len() {
            return Err("Mismatched keys and values in type_checker map".to_string());
        }

        let mut temp = HashMap::new();
        for (k, v) in keys.iter().zip(values.iter()) {
            if let Value::String(s) = k {
                temp.insert(s.clone(), v.clone());
            } else {
                return Err("Expected string keys in type_checker map".to_string());
            }
        }
        temp
    } else {
        return Err("Expected a map for 'type_checker'".to_string());
    };

    let config = TypeCheckerConfig {
        enabled: match map.get("enabled") {
            Some(Value::Boolean(b)) => *b,
            _ => true,
        },
        strict: match map.get("strict") {
            Some(Value::Boolean(b)) => *b,
            _ => true,
        },
        run_unchecked: match map.get("run_unchecked") {
            Some(Value::Boolean(b)) => *b,
            _ => false,
        },
        nested_functions: match map.get("nested_functions") {
            Some(Value::Boolean(b)) => *b,
            _ => true,
        },
        nested_loops: match map.get("nested_loops") {
            Some(Value::Boolean(b)) => *b,
            _ => true,
        },
        warn_on_any: match map.get("warn_on_any") {
            Some(Value::Boolean(b)) => *b,
            _ => false,
        },
        warnings: match map.get("warnings") {
            Some(Value::Boolean(b)) => *b,
            _ => true,
        },
        treat_warnings_as_errors: match map.get("treat_warnings_as_errors") {
            Some(Value::Boolean(b)) => *b,
            _ => false,
        },
        max_errors: match map.get("max_errors") {
            Some(Value::Int(v)) => {
                let i = v.to_i64().or_else(|_| Err("max_errors out of i64 range".to_string()))?;
                if i < i32::MIN as i64 || i > i32::MAX as i64 {
                    return Err("max_errors out of i32 range".to_string());
                }
                Some(i as i32)
            }
            Some(Value::Null) | None => None,
            _ => return Err("Expected int or null for max_errors".to_string()),
        },
        ignore_warnings: parse_string_list(map.get("ignore_warnings"))?,
        ignore_errors: parse_string_list(map.get("ignore_errors"))?,
        ignore_modules: parse_string_list(map.get("ignore_modules"))?,
        pointer_types: match map.get("pointer_types") {
            Some(Value::Boolean(b)) => *b,
            _ => true,
        },
        experimental_constant_folding: match map.get("experimental_constant_folding") {
            Some(Value::Boolean(b)) => *b,
            _ => false,
        },
        check_imports: match map.get("check_imports") {
            Some(Value::Boolean(b)) => *b,
            _ => true,
        },
        allow_dynamic_casts: match map.get("allow_dynamic_casts") {
            Some(Value::Boolean(b)) => *b,
            _ => false,
        },
        log_level: match map.get("log_level") {
            Some(Value::String(s)) => s.clone(),
            _ => "error".to_string(),
        },
        verbose: match map.get("verbose") {
            Some(Value::Boolean(b)) => *b,
            _ => false,
        },
        track_value_origins: match map.get("track_value_origins") {
            Some(Value::Boolean(b)) => *b,
            _ => true,
        },
        fail_fast: match map.get("fail_fast") {
            Some(Value::Boolean(b)) => *b,
            _ => true,
        },
        max_nested_depth: match map.get("max_nested_depth") {
            Some(Value::Int(v)) => {
                let i = v.to_i64().or_else(|_| Err("max_nested_depth out of i64 range".to_string()))?;
                if i < i32::MIN as i64 || i > i32::MAX as i64 {
                    return Err("max_nested_depth out of i32 range".to_string());
                }
                Some(i as i32)
            }
            Some(Value::Null) | None => None,
            _ => return Err("Expected int or null for max_nested_depth".to_string()),
        },
        try_to_auto_fix: match map.get("try_to_auto_fix") {
            Some(Value::Boolean(b)) => *b,
            _ => false,
        },
    };

    Ok(config)
}

fn parse_string_list(value: Option<&Value>) -> Result<Vec<String>, String> {
    match value {
        Some(Value::List(vs)) => {
            vs.iter()
                .map(|v| match v {
                    Value::String(s) => Ok(s.clone()),
                    _ => Err("Expected string in list".to_string()),
                })
                .collect()
        }
        None => Ok(vec![]),
        _ => Err("Expected list of strings".to_string()),
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Libs {
    use_libs: bool,
    std_libs: HashMap<String, LibInfo>,
    global: HashMap<String, LibInfo>,
    redirect: HashMap<String, String>,
    block: Vec<String>,
    domains: HashMap<String, Vec<String>>,
}

impl Libs {
    pub fn new() -> Self {
        Self {
            use_libs: true,
            std_libs: HashMap::new(),
            global: HashMap::new(),
            redirect: HashMap::new(),
            block: Vec::new(),
            domains: HashMap::new(),
        }
    }

    pub fn set_all(&mut self, new_libs: Libs) {
        self.std_libs = new_libs.std_libs;
        self.global = new_libs.global;
        self.redirect = new_libs.redirect;
        self.block = new_libs.block;
        self.domains = new_libs.domains;
    }

    pub fn set_std_libs(&mut self, std_libs: HashMap<String, LibInfo>) {
        self.std_libs = std_libs;
    }
}
