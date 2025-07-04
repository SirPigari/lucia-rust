use std::{env, fs, process::Command, path::Path, collections::HashSet};
use sha2::{Digest, Sha256};
use toml::Value;
use uuid::{Builder, Uuid};
use rand::RngCore;

const VERSION: &str = "2.0.0";

const CPU_OPT: &str = "native";
/*
CPU_OPT options for RUSTFLAGS -C target-cpu:
default CPU_OPT is "generic"

"native"       - Optimize for the CPU of the current machine (auto-detect).
"generic"      - Generic CPU architecture.
"x86-64"       - Generic x86-64 CPU.
"x86-64-v2"    - Intel Nehalem and later CPUs.
"x86-64-v3"    - Intel Haswell and later CPUs.
"x86-64-v4"    - Intel Ice Lake and later CPUs.
"athlon64"     - AMD Athlon 64.
"znver1"       - AMD Zen 1 architecture.
"znver2"       - AMD Zen 2 architecture.
"znver3"       - AMD Zen 3 architecture.
"skylake"      - Intel Skylake architecture.
"cascadelake"  - Intel Cascade Lake architecture.
"armv8-a"      - ARMv8-A architecture.
"cortex-a53"   - ARM Cortex-A53 CPU.

for other specific targets, pick the closest matching CPU architecture.

 full list: https://doc.rust-lang.org/rustc/codegen-options/index.html#target-cpu
*/


fn rerun_if_changed_recursive(dir: &Path) {
    if let Ok(entries) = fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                rerun_if_changed_recursive(&path);
            } else if path.extension().and_then(|s| s.to_str()) == Some("rs") {
                println!("cargo:rerun-if-changed={}", path.display());
            }
        }
    }
}

fn get_deps_from_cargo_toml() -> String {
    let manifest_dir = env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| ".".into());
    let cargo_toml_path = Path::new(&manifest_dir).join("Cargo.toml");
    let cargo_toml_str = fs::read_to_string(cargo_toml_path)
        .unwrap_or_else(|_| {
            println!("cargo:warning=Failed to read Cargo.toml");
            String::new()
        });
    let cargo_toml: Value = cargo_toml_str.parse().unwrap_or(Value::Table(Default::default()));

    let mut deps = HashSet::new();

    for section in ["dependencies", "dev-dependencies", "build-dependencies"] {
        if let Some(Value::Table(tbl)) = cargo_toml.get(section) {
            for (k, v) in tbl {
                let formatted = match v {
                    Value::String(version) => format!("{k} ({version})"),
                    Value::Table(t) => match t.get("version").and_then(Value::as_str) {
                        Some(version) => format!("{k} ({version})"),
                        None => k.to_string(),
                    },
                    _ => k.to_string(),
                };
                deps.insert(formatted);
            }
        }
    }

    let mut deps: Vec<_> = deps.into_iter().collect();
    deps.sort();
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

fn generate_uuid(git_hash: &str) -> Uuid {
    let mut hasher = Sha256::new();
    hasher.update(git_hash.as_bytes());

    let mut random_bytes = [0u8; 16];
    rand::thread_rng().fill_bytes(&mut random_bytes);
    hasher.update(&random_bytes);

    let hash = hasher.finalize();

    let bytes: [u8; 16] = hash[0..16].try_into().expect("slice with incorrect length");

    let mut builder = Builder::from_bytes(bytes);
    builder.set_variant(uuid::Variant::RFC4122);
    builder.set_version(uuid::Version::Random);

    builder.into_uuid()
}

fn main() {
    println!("cargo:rustc-env=RUSTFLAGS=-C target-cpu={CPU_OPT}");

    let manifest_dir = env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| ".".into());
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

    let hash = if cfg!(debug_assertions) {
        "skipped-in-dev".into()
    } else {
        let exe = env::current_exe().unwrap_or_default();
        to_hash(&exe)
    };

    let deps = get_deps_from_cargo_toml();

    let profile = if cfg!(debug_assertions) { "debug" } else { "release" };

    let build_uuid = generate_uuid(&git_hash).to_string();

    let manifest_dir = env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| ".".into());
    let cargo_toml_path = Path::new(&manifest_dir).join("Cargo.toml");
    let cargo_toml_str = fs::read_to_string(&cargo_toml_path).unwrap_or_default();
    let cargo_toml: Value = cargo_toml_str.parse().unwrap_or(Value::Table(Default::default()));
    let edition = cargo_toml
        .get("package")
        .and_then(|pkg| pkg.get("edition"))
        .and_then(Value::as_str)
        .unwrap_or("unknown");

    println!("cargo:rustc-env=VERSION={VERSION}");
    println!("cargo:rustc-env=BUILD_UUID={build_uuid}");
    println!("cargo:rustc-env=RUSTC_VERSION={rustc_version}");
    println!("cargo:rustc-env=RUSTC_CHANNEL={rustc_channel}");
    println!("cargo:rustc-env=TARGET_TRIPLE={target}");
    println!("cargo:rustc-env=GIT_HASH={git_hash}");
    println!("cargo:rustc-env=FILE_HASH={hash}");
    println!("cargo:rustc-env=PROFILE={profile}");
    println!("cargo:rustc-env=CI={}", env::var("CI").unwrap_or_else(|_| "false".into()));
    println!("cargo:rustc-env=DEPS={deps}");
    println!("cargo:rustc-env=REPO=https://github.com/SirPigari/lucia-rust");
    println!("cargo:rustc-env=BUILD_DATE={}", chrono::Local::now().format("%Y-%m-%d %H:%M:%S"));
    println!("cargo:rustc-env=RUST_EDITION={edition}");

    const MESSAGES: &[&str] = &[
        "is this easter egg?",
        "i hate my parser",
        "btw lucia was my crush",
        "30.6.2025",
        "151211",
    ];
    let hash_byte = git_hash.bytes().last().unwrap_or(b'0');
    let index = (hash_byte as usize) % MESSAGES.len();
    let msg = MESSAGES[index];
    println!("cargo:rustc-env=BUILD_MSG={msg}");

    let src_path = Path::new(&manifest_dir).join("src");
    let build_path = Path::new(&manifest_dir).join("build.rs");
    rerun_if_changed_recursive(&src_path);
    println!("cargo:rerun-if-changed={}", build_path.display());

    if cfg!(target_os = "windows") {
        use winres::WindowsResource;
        let mut res = WindowsResource::new();
        let icon_path = Path::new(&manifest_dir).join("src/env/assets/lucia_icon.ico");

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
        res.set_icon(icon_path.to_str().unwrap());
        res.compile().expect("Failed to compile Windows resources");
    }
}
