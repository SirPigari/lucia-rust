use serde::{Serialize, Deserialize};
use std::fs::File;
use std::io::Read;
use std::path::{Path, PathBuf};

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
    pub execute_code_blocks: CodeBlocks,
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