use serde::Serialize;
use std::{env, fs, env::current_exe, path::PathBuf};
use std::process::Command;
use crate::env::core::utils::to_static;
use sha2::{Sha256, Digest};
use std::path::Path;

use rustc_version::{
    VersionMeta,
    Channel,
    Version,
    self,
};

#[derive(Serialize)]
pub struct BuildInfo {
    pub name: &'static str,
    pub version: &'static str,
    pub rustc_version: &'static str,
    pub repository: &'static str,
    pub git_hash: &'static str,
    pub file_hash: &'static str,
    pub profile: &'static str,
}

pub fn get_build_info() -> BuildInfo {
    let rustc_version = to_static(
        rustc_version::version()
            .map(|v| v.to_string())
            .unwrap_or_else(|_| "unknown".to_string())
    );

    let git_hash_string = match Command::new("git")
        .args(&["rev-parse", "HEAD"])
        .output()
    {
        Ok(output) if output.status.success() => {
            String::from_utf8(output.stdout)
                .unwrap_or_else(|_| "not-cloned".to_string())
                .trim()
                .to_string()
        }
        _ => "not-cloned".to_string(),
    };

    let git_hash = to_static(git_hash_string);

    let path: PathBuf;
    let file_hash: String;

    match env::current_exe() {
        Ok(exe_path) => {
            path = exe_path;
            let contents = match fs::read(&path) {
                Ok(data) => data,
                Err(_) => {
                    eprintln!("Failed to read the file at {:?}", path);
                    b"null".to_vec()
                }
            };

            let mut hasher = Sha256::new();
            hasher.update(&contents);
            file_hash = hasher.finalize()
                .iter()
                .map(|byte| format!("{:02x}", byte))
                .collect();
        }
        Err(e) => {
            eprintln!("Failed to get current executable path: {}", e);
            path = PathBuf::from("null");
            file_hash = "null".to_string();
        }
    }

    BuildInfo {
        name: env!("CARGO_PKG_NAME"),
        version: env!("CARGO_PKG_VERSION"),
        rustc_version,
        repository: "https://github.com/SirPigari/lucia-rust",
        git_hash: git_hash.trim(),
        file_hash: to_static(file_hash.trim().to_string()),
        profile: if cfg!(debug_assertions) { "debug" } else { "release" },
    }
}
