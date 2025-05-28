use serde::{Serialize, Deserialize};

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Config {
    pub moded: bool,
    pub debug: bool,
    pub debug_mode: String,
    pub supports_color: bool,
    pub use_lucia_traceback: bool,
    pub warnings: bool,
    pub use_predefs: bool,
    pub print_comments: bool,
    pub allow_fetch: bool,
    pub execute_code_blocks: CodeBlocks,
    pub lucia_file_extensions: Vec<String>,
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
    pub input_text: String,
    pub output_text: String,
    pub info: String,
}
