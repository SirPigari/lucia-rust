use std::{env, fs, process::Command, path::Path, collections::HashSet, io::BufRead, io::Write, io::BufReader};
use sha2::{Digest, Sha256};
use toml::Value;
use uuid::{Builder, Uuid};
use rand::{rng, RngCore};
use std::cmp::Ordering;

const VERSION: &str = env!("CARGO_PKG_VERSION");

const CPU_OPT: &str = "native";
/*
CPU_OPT options for RUSTFLAGS -C target-cpu:
default CPU_OPT is "generic"

"native"       - Optimize for the CPU of the current machine (auto-detect).
"generic"      - Generic CPU architecture.

Use native for custom builds, generic for release builds.

 https://doc.rust-lang.org/rustc/codegen-options/index.html#target-cpu
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

fn scan_todos_and_generate_report(src_dir: &Path, manifest_dir: &Path) {
    let mut todos = Vec::new();

    fn collect_todos_from_file(
        path: &Path,
        rel_path: &str,
        todos: &mut Vec<(String, Vec<String>)>,
    ) {
        let file = match fs::File::open(path) {
            Ok(f) => f,
            Err(_) => return,
        };
        let reader = BufReader::new(file);
        let mut lines_iter = reader.lines().enumerate().peekable();

        while let Some((line_num, Ok(line))) = lines_iter.next() {
            let trimmed = line.trim_start();

            if trimmed.starts_with("// TODO") {
                let content = trimmed
                    .strip_prefix("// TODO")
                    .unwrap_or("")
                    .trim_start_matches(':')
                    .trim();

                let mut collected = vec![content.to_string()];
                let mut peek = lines_iter.peek();

                while let Some((_, Ok(next_line))) = peek {
                    let t = next_line.trim_start();
                    if t.starts_with("//") && !t.contains("TODO") {
                        collected.push(t.trim_start_matches("//").trim().to_string());
                        lines_iter.next();
                        peek = lines_iter.peek();
                    } else {
                        break;
                    }
                }

                todos.push((format!("{}:{}", rel_path, line_num + 1), collected));
            }
        }
    }

    fn walk_dir_recursively(
        dir: &Path,
        manifest_dir: &Path,
        todos: &mut Vec<(String, Vec<String>)>,
    ) {
        if let Ok(entries) = fs::read_dir(dir) {
            for entry in entries.flatten() {
                let path = entry.path();
                if path.is_dir() {
                    walk_dir_recursively(&path, manifest_dir, todos);
                } else if path.extension().and_then(|s| s.to_str()) == Some("rs") || path.extension().and_then(|s| s.to_str()) == Some("lc") {
                    let rel_path = path
                        .strip_prefix(manifest_dir)
                        .unwrap_or(&path)
                        .to_string_lossy()
                        .replace('\\', "/");
                    collect_todos_from_file(&path, &rel_path, todos);
                }
            }
        }
    }

    fn get_git_blame_author_date(file_path: &Path, line: usize) -> Option<(String, String)> {
        let output = Command::new("git")
            .args([
                "blame",
                "-L",
                &format!("{line},{line}"),
                "--date=short",
                "--",
                file_path.to_str()?,
            ])
            .output()
            .ok()?;

        let stdout = String::from_utf8_lossy(&output.stdout);
        let line = stdout.lines().next()?;
        if line.contains("Not Committed Yet") {
            return Some(("uncommitted".to_string(), String::new()));
        }

        let parts: Vec<&str> = line.split('(').nth(1)?.split_whitespace().collect();
        if parts.len() >= 2 {
            let author = parts[0].to_string();
            let date = parts[1].to_string();
            Some((author, date))
        } else {
            None
        }
    }

    walk_dir_recursively(src_dir, manifest_dir, &mut todos);

    let mut todos_with_meta: Vec<(String, Vec<String>, Option<String>)> = todos
        .into_iter()
        .map(|(location, lines)| {
            if let Some((file_path, line_str)) = location.rsplit_once(':') {
                let line_num: usize = line_str.parse().unwrap_or(1);
                let full_path = manifest_dir.join(file_path);
                let blame_info = get_git_blame_author_date(&full_path, line_num);

                let date_opt = match &blame_info {
                    Some((author, _)) if author == "uncommitted" => None,
                    Some((_, date)) if !date.is_empty() => Some(date.clone()),
                    _ => None,
                };

                (location, lines, date_opt)
            } else {
                (location, lines, None)
            }
        })
        .collect();

    todos_with_meta.sort_by(|a, b| {
        match (&a.2, &b.2) {
            (None, Some(_)) => Ordering::Greater,
            (Some(_), None) => Ordering::Less,
            (Some(d1), Some(d2)) => d2.cmp(d1),
            (None, None) => Ordering::Equal,
        }
    });

    let report_path = manifest_dir.join("src/env/Docs/todos.md");
    if let Some(parent) = report_path.parent() {
        let _ = fs::create_dir_all(parent);
    }

    let mut file = match fs::File::create(&report_path) {
        Ok(f) => f,
        Err(e) => {
            eprintln!("Failed to write TODO report: {}", e);
            return;
        }
    };

    let _ = writeln!(file, "# TODO Report\n\nDO NOT MODIFY THIS FILE. IT'S GENERATED AUTOMATICALLY.  \n\nList of all TODOs in Lucia source code.\nBe free to fix them or add new ones.  \n");

    let total_todos = todos_with_meta.len();
    let _ = writeln!(file, "*Total TODOs found: {}*\n\n---\n", total_todos);

    for (location, lines, _) in todos_with_meta {
        if let Some((file_path, line_str)) = location.rsplit_once(':') {
            let line_num: usize = line_str.parse().unwrap_or(1);

            let relative_path = Path::new(file_path)
                .strip_prefix("src/env/Docs")
                .unwrap_or(Path::new(file_path));
            let adjusted_path = Path::new("../../../").join(relative_path);

            let full_path = manifest_dir.join(file_path);
            let blame_info = get_git_blame_author_date(&full_path, line_num);
            let meta_line = match blame_info {
                Some((author, _)) if author == "uncommitted" => "**(TODO not committed yet)**".to_string(),
                Some((author, date)) => format!("*(added by **{}** on **{}**)*", author, date),
                None => "*(added by unknown)*".to_string(),
            };

            let col = lines
                .get(0)
                .and_then(|l| l.find('/'))
                .map(|idx| idx + 1)
                .unwrap_or(1);

            let markdown_link = format!(
                "[`{}:{col}`]({}#L{})",
                location,
                adjusted_path.to_string_lossy().replace('\\', "/"),
                line_num
            );

            let _ = writeln!(file, "### {} {}", markdown_link, meta_line);

        } else {
            let _ = writeln!(file, "### `{}`", location);
        }

        let _ = writeln!(file);

        for line in lines {
            if !line.is_empty() {
                let _ = writeln!(file, "- {}", line);
            }
        }
        let _ = writeln!(file);
    }

    let _ = file.flush();
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
    rng().fill_bytes(&mut random_bytes);
    hasher.update(&random_bytes);

    let hash = hasher.finalize();

    let bytes: [u8; 16] = hash[0..16].try_into().expect("slice with incorrect length");

    let mut builder = Builder::from_bytes(bytes);
    builder.set_variant(uuid::Variant::RFC4122);
    builder.set_version(uuid::Version::Random);

    builder.into_uuid()
}

fn load_gitignore(manifest_dir: &Path) -> Vec<String> {
    let gitignore_path = manifest_dir.join(".gitignore");
    let mut ignore_patterns = Vec::new();
    if let Ok(contents) = fs::read_to_string(gitignore_path) {
        for line in contents.lines() {
            let line = line.trim();
            if line.is_empty() || line.starts_with('#') {
                continue;
            }
            ignore_patterns.push(line.to_string());
        }
    }
    ignore_patterns
}

fn simple_match(pattern: &str, text: &str) -> bool {
    let (mut p_idx, mut t_idx) = (0, 0);
    let (mut star_idx, mut match_idx) = (None, 0);

    while t_idx < text.len() {
        if p_idx < pattern.len() && (pattern.as_bytes()[p_idx] == b'*' || pattern.as_bytes()[p_idx] == text.as_bytes()[t_idx]) {
            if pattern.as_bytes()[p_idx] == b'*' {
                star_idx = Some(p_idx);
                match_idx = t_idx;
                p_idx += 1;
            } else {
                p_idx += 1;
                t_idx += 1;
            }
        } else if let Some(si) = star_idx {
            p_idx = si + 1;
            match_idx += 1;
            t_idx = match_idx;
        } else {
            return false;
        }
    }

    while p_idx < pattern.len() && pattern.as_bytes()[p_idx] == b'*' {
        p_idx += 1;
    }

    p_idx == pattern.len()
}

fn is_ignored(path: &Path, manifest_dir: &Path, ignore_patterns: &[String]) -> bool {
    if let Ok(rel_path) = path.strip_prefix(manifest_dir) {
        let rel_path_str = rel_path.to_string_lossy().replace('\\', "/");

        for pattern in ignore_patterns {
            let pat = pattern.trim_start_matches('/');

            if pat.ends_with('/') {
                if rel_path_str.starts_with(pat) {
                    return true;
                }
            } else {
                if simple_match(pat, &rel_path_str) {
                    return true;
                }
            }
        }
    }
    false
}

fn count_lines(dir: &Path, exts: &[&str], manifest_dir: &Path, ignore_patterns: &[String]) -> (usize, usize, usize) {
    let mut lines = 0;
    let mut files = 0;
    let mut semicolons = 0;

    if let Ok(entries) = fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if is_ignored(&path, manifest_dir, ignore_patterns) {
                continue;
            }
            if path.is_dir() {
                let (sub_lines, sub_files, sub_semicolons) = count_lines(&path, exts, manifest_dir, ignore_patterns);
                lines += sub_lines;
                files += sub_files;
                semicolons += sub_semicolons;
            } else {
                let should_count = if exts.is_empty() {
                    false
                } else if let Some(ext) = path.extension().and_then(|s| s.to_str()) {
                    exts.contains(&ext)
                } else {
                    false
                };

                if should_count {
                    if let Ok(file) = fs::File::open(&path) {
                        let reader = std::io::BufReader::new(file);
                        let mut in_block_comment = false;

                        for line_result in reader.lines() {
                            if let Ok(line) = line_result {
                                let mut chars = line.chars().peekable();

                                let mut in_string = false;
                                let mut in_char = false;
                                let mut escape = false;

                                let trimmed = line.trim();

                                if in_block_comment {
                                    if trimmed.contains("*/") {
                                        in_block_comment = false;
                                    }
                                    continue;
                                }

                                if trimmed.is_empty() || trimmed.starts_with("//") {
                                    continue;
                                }

                                if trimmed.starts_with("/*") {
                                    in_block_comment = true;
                                    if trimmed.contains("*/") {
                                        in_block_comment = false;
                                    }
                                    continue;
                                }

                                lines += 1;

                                while let Some(c) = chars.next() {
                                    if escape {
                                        escape = false;
                                        continue;
                                    }

                                    if c == '\\' && (in_string || in_char) {
                                        escape = true;
                                        continue;
                                    }

                                    if !in_char && c == '"' {
                                        in_string = !in_string;
                                        continue;
                                    }

                                    if !in_string && c == '\'' {
                                        in_char = !in_char;
                                        continue;
                                    }

                                    if !in_string && !in_char {
                                        if c == '/' && chars.peek() == Some(&'/') {
                                            break;
                                        }

                                        if c == ';' {
                                            semicolons += 1;
                                        }
                                    }
                                }
                            }
                        }

                        files += 1;
                    }
                }
            }
        }
    }

    (lines, files, semicolons)
}

fn join_lc_tests() -> std::io::Result<()> {
    let manifest_dir = env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| ".".into());
    let tests_dir = Path::new(&manifest_dir).join("tests");
    let out_file_path = Path::new(&manifest_dir).join("src/env/assets/tests.lucia");

    let mut out_file = fs::File::create(&out_file_path)?;

    if let Ok(entries) = fs::read_dir(&tests_dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.extension().and_then(|s| s.to_str()) == Some("lc") {
                let file = fs::File::open(&path)?;
                let reader = BufReader::new(file);

                let lines: Vec<_> = reader.lines().collect::<Result<_, _>>()?;
                let line_count = lines.len();

                let file_name = path.file_name().and_then(|s| s.to_str()).unwrap_or("unknown");
                
                writeln!(out_file, "/*\n FILE {}, LINES 1:{}\n*/\n", file_name, line_count)?;
                for line in lines {
                    writeln!(out_file, "{}", line)?;
                }

                writeln!(out_file)?;
            }
        }
    }

    Ok(())
}

fn main() {
    println!("cargo:rustc-env=RUSTFLAGS=-C target-cpu={CPU_OPT}");

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

    join_lc_tests().unwrap_or_else(|e| {
        eprintln!("Failed to join LC tests: {}", e);
    });

    let deps = get_deps_from_cargo_toml();

    let profile = if cfg!(debug_assertions) { "debug" } else { "release" };

    let build_uuid = generate_uuid(&git_hash).to_string();

    let manifest_dir = env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| ".".into());
    let cargo_toml_path = Path::new(&manifest_dir).join("Cargo.toml");
    let cargo_toml_str = fs::read_to_string(&cargo_toml_path).unwrap_or_default();
    let cargo_toml: Value = toml::from_str(&cargo_toml_str).unwrap_or(Value::Table(Default::default()));
    let edition = cargo_toml
        .get("package")
        .and_then(|pkg| pkg.get("edition"))
        .and_then(Value::as_str)
        .unwrap_or("unknown");

    let lib_static = std::env::var("CARGO_FEATURE_LIB_LUCIA_STATIC").is_ok();
    let lib_rlib   = std::env::var("CARGO_FEATURE_LIB_LUCIA_RLIB").is_ok();
    let lib_dynamic = std::env::var("CARGO_FEATURE_LIB_LUCIA").is_ok();

    if lib_static {
        println!("cargo:rustc-cdylib-link-arg=-Wl,--whole-archive");
        println!("cargo:rustc-cfg=lib_lucia_static");
        println!("cargo:rustc-link-lib=static=liblucia2.0.0");
    } else if lib_rlib {
        println!("cargo:rustc-cfg=lib_lucia_rlib");
    } else if lib_dynamic {
        println!("cargo:rustc-cfg=lib_lucia_dynamic");
        println!("cargo:rustc-link-lib=dylib=liblucia2.0.0");
    }

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
    println!("cargo:rustc-env=BUILD_DATE_ISO={}", chrono::Utc::now().to_rfc3339());
    println!("cargo:rustc-env=RUST_EDITION={edition}");

    let path = Path::new(&manifest_dir);
    let ignore_patterns = load_gitignore(path);
    let (rust_lines, rust_files, n_semicolons) = count_lines(path, &["rs"], path, &ignore_patterns);
    let (lucia_lines, _, _) = count_lines(path, &["lc", "lucia"], path, &ignore_patterns);
    let (total_lines, total_files, _) = count_lines(path, &["rs", "lc", "lucia", "yml"], path, &ignore_patterns);

    println!("cargo:rustc-env=RUST_LOC={}", rust_lines);
    println!("cargo:rustc-env=SEMICOLONS={}", n_semicolons);
    println!("cargo:rustc-env=LUCIA_LOC={}", lucia_lines);
    println!("cargo:rustc-env=TOTAL_LOC={}", total_lines);
    println!("cargo:rustc-env=RUST_FILES={}", rust_files);
    println!("cargo:rustc-env=TOTAL_FILES={}", total_files);

    const MESSAGES: &[&str] = &[
        "is this easter egg?",
        "i hate my parser",
        "btw lucia was my crush",
        "30.6.2025",
        "151211",
        "42",
        "137",
        "sowing the seeds of love",
        "west end girls",
        "always on my mind",
        "open your mind",
        "tchaikovsky piano concerto no. 1",
    ];
    let hash_byte = git_hash.bytes().last().unwrap_or(b'0');
    let index = (hash_byte as usize) % MESSAGES.len();
    let msg = MESSAGES[index];
    println!("cargo:rustc-env=BUILD_MSG={msg}");

    let src_path = Path::new(&manifest_dir).join("src");
    let build_path = Path::new(&manifest_dir).join("build.rs");
    rerun_if_changed_recursive(&src_path);
    println!("cargo:rerun-if-changed={}", build_path.display());

    scan_todos_and_generate_report(&src_path, &path);

    if std::env::var("CARGO_CFG_TARGET_OS").as_deref() == Ok("macos") {
        println!("cargo:rustc-link-arg=-mmacosx-version-min=14.5");
    }

    #[cfg(target_os = "windows")]
    {
        use winres::WindowsResource;
        let mut res = WindowsResource::new();
        let icon_path = Path::new(&manifest_dir).join("src/env/assets/lucia_icon.ico");

        res.set("ProductName", "Lucia");
        res.set("FileDescription", "Lucia Programming Language");
        res.set("CompanyName", "Lucia Programming Language Project");
        res.set("LegalCopyright", "© 2025 Lucia Programming Language Project");
        res.set("FileVersion", &target);
        res.set("ProductVersion", VERSION);
        res.set("Comments", "Licensed under GPLv3");
        res.set("OriginalFilename", "lucia.exe");
        res.set("InternalName", "lucia");
        res.set("LegalTrademarks", "Lucia™ is a trademark of Lucia Programming Language Project");
        res.set_icon(icon_path.to_str().unwrap());
        res.compile().expect("Failed to compile Windows resources");
    }
}
