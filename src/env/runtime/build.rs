use std::{env, fs, path::PathBuf, process::Command};
use sha2::{Sha256, Digest};
use serde::Serialize;
use crate::env::runtime::utils::to_static;

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

fn parse_rustc_version_and_channel() -> (String, String) {
    let output = Command::new("rustc")
        .arg("-vV")
        .output();

    if let Ok(output) = output {
        if output.status.success() {
            let stdout = String::from_utf8_lossy(&output.stdout);
            let mut version = "unknown".to_string();
            let mut channel = "unknown".to_string();

            for line in stdout.lines() {
                if line.starts_with("rustc ") {
                    // example: rustc 1.70.0-nightly (0e4f7e2d1 2023-05-25)
                    let parts: Vec<&str> = line.split_whitespace().collect();
                    if parts.len() > 1 {
                        version = parts[1].to_string();
                        if version.contains("nightly") {
                            channel = "nightly".to_string();
                        } else if version.contains("beta") {
                            channel = "beta".to_string();
                        } else {
                            channel = "stable".to_string();
                        }
                    }
                }
            }
            return (version, channel);
        }
    }
    ("unknown".to_string(), "unknown".to_string())
}

pub fn get_build_info() -> BuildInfo {
    let (rustc_version, rustc_channel) = parse_rustc_version_and_channel();

    let rustc_version = to_static(rustc_version);
    let rustc_channel = to_static(rustc_channel);

    let target = to_static(
        env::var("TARGET")
            .or_else(|_| {
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
