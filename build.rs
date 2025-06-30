use std::{env, fs, process::Command, path::Path, collections::HashSet};
use sha2::{Digest, Sha256};
use toml::Value;

const VERSION: &str = "2.0.0";

fn get_deps_from_cargo_toml() -> String {
    let cargo_toml_str = fs::read_to_string("Cargo.toml").unwrap_or_default();
    let cargo_toml: Value = cargo_toml_str.parse().unwrap_or(Value::Table(Default::default()));

    let mut deps = Vec::new();

    if let Some(Value::Table(dependencies)) = cargo_toml.get("dependencies") {
        for (name, _) in dependencies {
            deps.push(name.as_str());
        }
    }

    deps.join(", ")
}

fn to_hash(path: &Path) -> String {
    fs::read(path)
        .map(|content| {
            let mut hasher = Sha256::new();
            hasher.update(content);
            hasher.finalize()
                .iter()
                .map(|b| format!("{:02x}", b))
                .collect()
        })
        .unwrap_or_else(|_| "null".into())
}

fn main() {
    let output = Command::new("rustc").arg("-vV").output().ok();
    let rustc_info = output
        .as_ref()
        .and_then(|o| String::from_utf8(o.stdout.clone()).ok())
        .unwrap_or_default();

    let rustc_version = rustc_info
        .lines()
        .find(|line| line.starts_with("rustc "))
        .and_then(|line| line.split_whitespace().nth(1))
        .unwrap_or("unknown");

    let rustc_channel = if rustc_version.contains("nightly") {
        "nightly"
    } else if rustc_version.contains("beta") {
        "beta"
    } else {
        "stable"
    };

    let target = env::var("TARGET").unwrap_or_else(|_| {
        rustc_info
            .lines()
            .find(|line| line.starts_with("host: "))
            .map(|line| line["host: ".len()..].to_string())
            .unwrap_or_else(|| "unknown".into())
    });

    let git_hash = Command::new("git")
        .args(&["rev-parse", "HEAD"])
        .output()
        .ok()
        .and_then(|o| String::from_utf8(o.stdout).ok())
        .unwrap_or_else(|| "not-cloned".into())
        .trim()
        .to_string();

    let exe = env::current_exe().unwrap_or_default();
    let hash = to_hash(&exe);

    let deps = get_deps_from_cargo_toml();

    let profile = if cfg!(debug_assertions) { "debug" } else { "release" };

    println!("cargo:rustc-env=VERSION={VERSION}");
    println!("cargo:rustc-env=RUSTC_VERSION={rustc_version}");
    println!("cargo:rustc-env=RUSTC_CHANNEL={rustc_channel}");
    println!("cargo:rustc-env=TARGET_TRIPLE={target}");
    println!("cargo:rustc-env=GIT_HASH={git_hash}");
    println!("cargo:rustc-env=FILE_HASH={hash}");
    println!("cargo:rustc-env=PROFILE={profile}");
    println!("cargo:rustc-env=CI={}", env::var("CI").unwrap_or_else(|_| "false".into()));
    println!("cargo:rustc-env=DEPS={deps}");
    println!("cargo:rustc-env=REPO=https://github.com/SirPigari/lucia-rust");

    if cfg!(target_os = "windows") {
        use winres::WindowsResource;
        let mut res = WindowsResource::new();
        res.set("ProductName", "Lucia");
        res.set("FileDescription", "Lucia Programming Language");
        res.set("CompanyName", "Alfa Reklama");
        res.set("LegalCopyright", "© 2025 Alfa Reklama");
        res.set("FileVersion", &target);
        res.set("ProductVersion", VERSION);
        res.set("Comments", "Licensed under GPLv3");
        res.set("OriginalFilename", "lucia.exe");
        res.set("InternalName", "lucia");
        res.set("LegalTrademarks", "Lucia™ is a trademark of Alfa Reklama");
        res.set_icon("src/env/assets/lucia_icon.ico");
        res.compile().expect("Failed to compile Windows resources");
    }
}
