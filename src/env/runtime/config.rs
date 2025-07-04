use serde::{Serialize, Deserialize};
use std::fs::File;
use std::io::Read;
use std::path::{Path, PathBuf};
use crate::env::runtime::value::Value;
use crate::env::runtime::types::Int;
use crate::env::runtime::utils::to_static;

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Config {
    pub moded: bool,
    pub debug: bool,
    pub debug_mode: String,
    pub supports_color: bool,
    pub use_lucia_traceback: bool,
    pub warnings: bool,
    pub use_preprocessor: bool,
    pub allow_fetch: bool,
    pub allow_unsafe: bool,
    pub home_dir: String,
    pub recursion_limit: usize,
    pub version: String,
    pub color_scheme: ColorScheme,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct ColorScheme {
    pub exception: String,
    pub warning: String,
    pub help: String,
    pub debug: String,
    pub comment: String,
    pub input_arrows: String,
    pub note: String,
    pub output_text: String,
    pub info: String,
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
        "use_preprocessor" => {
            if let Value::Boolean(val) = value {
                config.use_preprocessor = val;
                Ok(())
            } else {
                Err("Expected a boolean value for 'use_preprocessor'".to_string())
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
        "home_dir" => {
            if let Value::String(val) = value {
                config.home_dir = val;
                Ok(())
            } else {
                Err("Expected a string value for 'home_dir'".to_string())
            }
        }
        "recursion_limit" => {
            if let Value::Int(val) = value {
                if let Ok(i64_val) = val.to_i64() {
                    config.recursion_limit = i64_val as usize;
                    Ok(())
                } else {
                    Err("Expected an integer value for 'recursion_limit'".to_string())
                }
            } else {
                Err("Expected an integer value for 'recursion_limit'".to_string())
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
        "color_scheme" => {
            if let Value::Map { keys, values } = value {
                if keys.len() == 9 && values.len() == 9 {
                    config.color_scheme = ColorScheme {
                        exception: values[0].to_string(),
                        warning: values[1].to_string(),
                        help: values[2].to_string(),
                        debug: values[3].to_string(),
                        comment: values[4].to_string(),
                        input_arrows: values[5].to_string(),
                        note: values[6].to_string(),
                        output_text: values[7].to_string(),
                        info: values[8].to_string(),
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
        "use_preprocessor" => Value::Boolean(config.use_preprocessor),
        "allow_fetch" => Value::Boolean(config.allow_fetch),
        "allow_unsafe" => Value::Boolean(config.allow_unsafe),
        "home_dir" => Value::String(config.home_dir.clone()),
        "recursion_limit" => Value::Int(Int::from_i64(config.recursion_limit as i64)),
        "version" => Value::String(config.version.clone()),
        "color_scheme" => {
            let color_scheme = &config.color_scheme;
            Value::Map {
                keys: vec![
                    Value::String("exception".to_string()),
                    Value::String("warning".to_string()),
                    Value::String("help".to_string()),
                    Value::String("debug".to_string()),
                    Value::String("comment".to_string()),
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
                    Value::String(color_scheme.comment.clone()),
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