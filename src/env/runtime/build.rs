use serde::Serialize;
use std::{env, fs, path::PathBuf, process::Command};
use crate::env::runtime::utils::to_static;
use sha2::{Sha256, Digest};
use rustc_version::{self, Channel};

#[derive(Serialize)]
pub struct BuildInfo {
    pub name: &'static str,
    pub version: &'static str,
    pub rustc_version: &'static str,
    pub rustc_channel: &'static str,
    pub target: &'static str,
    pub repository: &'static str,
    pub git_hash: &'static str,
    pub file_hash: &'static str,
    pub profile: &'static str,
    pub ci: &'static str,
    pub dependencies: &'static str,
}

pub fn get_build_info() -> BuildInfo {
    let rustc_version = to_static(
        rustc_version::version()
            .map(|v| v.to_string())
            .unwrap_or_else(|_| "unknown".to_string())
    );

    let rustc_channel = to_static(
        rustc_version::version_meta()
            .map(|meta| match meta.channel {
                Channel::Stable => "stable",
                Channel::Beta => "beta",
                Channel::Nightly => "nightly",
                Channel::Dev => "dev",
            }.to_string())
            .unwrap_or_else(|_| "unknown".to_string())
    );

    let target = to_static(
        env::var("TARGET")
            .or_else(|_| {
                // fallback to rustc -vV to extract target
                Command::new("rustc")
                    .arg("-vV")
                    .output()
                    .ok()
                    .and_then(|o| {
                        String::from_utf8(o.stdout).ok().and_then(|s| {
                            s.lines()
                                .find(|l| l.starts_with("host: "))
                                .map(|l| l["host: ".len()..].to_string())
                        })
                    })
                    .ok_or_else(|| env::VarError::NotPresent)
            })
            .unwrap_or_else(|_| "unknown".to_string())
    );

    let ci = to_static(
        env::var("CI").unwrap_or_else(|_| "false".to_string())
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

    let (path, file_hash) = match env::current_exe() {
        Ok(exe_path) => {
            let contents = fs::read(&exe_path).unwrap_or_else(|_| b"null".to_vec());
            let mut hasher = Sha256::new();
            hasher.update(&contents);
            let hash = hasher.finalize()
                .iter()
                .map(|byte| format!("{:02x}", byte))
                .collect::<String>();
            (exe_path, to_static(hash))
        }
        Err(_) => (PathBuf::from("null"), to_static("null".to_string())),
    };

    let dependencies = to_static(match fs::read_to_string("Cargo.lock") {
        Ok(lockfile) => {
            let top_deps: Vec<&str> = lockfile
                .lines()
                .filter(|l| l.starts_with("name = "))
                .take(5)
                .collect();
            top_deps.join(", ")
        }
        Err(_) => "unavailable".to_string(),
    });

    BuildInfo {
        name: env!("CARGO_PKG_NAME"),
        version: env!("CARGO_PKG_VERSION"),
        rustc_version,
        rustc_channel,
        target,
        repository: "https://github.com/SirPigari/lucia-rust",
        git_hash: git_hash.trim(),
        file_hash,
        profile: if cfg!(debug_assertions) { "debug" } else { "release" },
        ci,
        dependencies,
    }
}
