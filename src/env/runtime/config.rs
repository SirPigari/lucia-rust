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
    pub print_comments: bool,
    pub allow_fetch: bool,
    pub allow_unsafe: bool,
    pub home_dir: String,
    pub recursion_limit: usize,
    pub version: String,
    pub color_scheme: ColorScheme,
}

#[allow(non_snake_case)]
#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct CodeBlocks {
    pub C: bool,
    pub ASM: bool,
    pub PY: bool,
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

pub fn get_from_config(config: &Config, key: &str) -> Value {
    match key {
        "moded" => Value::Boolean(config.moded),
        "debug" => Value::Boolean(config.debug),
        "debug_mode" => Value::String(config.debug_mode.clone()),
        "supports_color" => Value::Boolean(config.supports_color),
        "use_lucia_traceback" => Value::Boolean(config.use_lucia_traceback),
        "warnings" => Value::Boolean(config.warnings),
        "use_preprocessor" => Value::Boolean(config.use_preprocessor),
        "print_comments" => Value::Boolean(config.print_comments),
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