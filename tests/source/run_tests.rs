use std::{
    fs,
    env,
    io::Write,
    process::Command,
    path::{Path, PathBuf},
    collections::HashSet,
};

use colored::*;
use regex::Regex;

fn remove_ansi_codes(s: &str) -> String {
    let re = Regex::new(r"\x1b\[[0-9;]*m").unwrap();
    re.replace_all(s, "").to_string()
}


fn main() {
    let args: Vec<String> = env::args().collect();

    let mut filter_prefixes = vec![];
    let mut stdout_dir: Option<PathBuf> = None;

    for arg in &args[1..] {
        if let Some(stripped) = arg.strip_prefix("--stdout=") {
            stdout_dir = Some(PathBuf::from(stripped));
        } else {
            filter_prefixes.push(arg.to_string());
        }
    }

    stdout_dir = stdout_dir.map(|p| {
        if p.is_relative() {
            env::current_dir().unwrap().join(p)
        } else {
            p
        }
    });

    if let Some(ref dir) = stdout_dir {
        if let Err(e) = fs::create_dir_all(dir) {
            eprintln!("{}", format!("Failed to create stdout directory '{}': {}", dir.display(), e).red());
            std::process::exit(1);
        }
    }

    if let Ok(exe_path) = env::current_exe() {
        if let Some(parent_dir) = exe_path.parent() {
            if let Err(e) = env::set_current_dir(parent_dir) {
                eprintln!("{}", format!("Failed to change directory: {}", e).red());
                std::process::exit(1);
            }
        }
    }

    let is_windows = cfg!(target_os = "windows");
    let lucia_exe = if is_windows { "lucia.exe" } else { "lucia" };
    let target_path: PathBuf = Path::new("../src").join("env").join("bin").join(lucia_exe);

    if !target_path.exists() {
        eprintln!("{}", format!("Target executable not found: {}", target_path.display()).red());
        std::process::exit(1);
    }

    let test_dir = Path::new(".");
    let entries = match fs::read_dir(&test_dir) {
        Ok(entries) => entries,
        Err(err) => {
            eprintln!("{}", format!("Failed to read test directory: {}", err).red());
            std::process::exit(1);
        }
    };

    let mut test_entries: Vec<_> = entries
    .filter_map(Result::ok)
    .filter(|entry| {
        entry.path().extension().map_or(false, |ext| ext == "lc")
    })
    .collect();

    test_entries.sort_by_key(|entry| {
        let binding = entry.file_name();
        let file_name = binding.to_string_lossy();
        file_name
            .split('_')
            .next()
            .and_then(|num_str| num_str.parse::<usize>().ok())
            .unwrap_or(0)
    });


    let mut passed = vec![];
    let mut failed = vec![];
    let mut found_prefixes = HashSet::new();

    println!("{}", "Lucia Test Runner".bold().underline().cyan());

    for entry in test_entries {
        let path = entry.path();
        if path.extension().map_or(false, |ext| ext == "lc") {
            let file_name = path.file_name().unwrap().to_string_lossy();

            if !filter_prefixes.is_empty() {
                let matched_prefix = filter_prefixes.iter().find(|prefix| file_name.starts_with(&format!("{}_", prefix)));
                if let Some(prefix) = matched_prefix {
                    found_prefixes.insert(prefix.to_string());
                } else {
                    continue;
                }
            }

            print!("Running: {:<40} ... ", file_name);

            let output = Command::new(&target_path)
                .arg(&path)
                .arg("-q")
                .arg("--no-color")
                .arg("--stack-size")
                .arg("4194304") // 4MB stack size
                .output();

            let mut test_stdout_file = stdout_dir.as_ref().and_then(|dir| {
                let mut file_path = dir.clone();
                file_path.push(format!("{}.stdout", path.file_stem().unwrap().to_string_lossy()));
                match fs::File::create(&file_path) {
                    Ok(f) => Some(f),
                    Err(e) => {
                        eprintln!("{}", format!("Failed to create stdout file '{}': {}", file_path.display(), e).red());
                        None
                    }
                }
            });

            match output {
                Ok(out) if out.status.success() => {
                    println!("{}", "PASSED".green());
                    passed.push(file_name.to_string());

                    if let Some(file) = test_stdout_file.as_mut() {
                        writeln!(file, "--- {} stdout ---", file_name).ok();
                        let clean_stdout = remove_ansi_codes(&String::from_utf8_lossy(&out.stdout));
                        writeln!(file, "{}", clean_stdout).ok();
                        writeln!(file).ok();
                        file.flush().unwrap_or_else(|e| {
                            eprintln!("{}", format!("Failed to flush stdout file: {}", e).red());
                        });
                    }
                }
                Ok(out) => {
                    println!("{}", "FAILED".red());
                    let stderr = String::from_utf8_lossy(&out.stderr).to_string();
                    failed.push((file_name.to_string(), stderr));

                    if let Some(file) = test_stdout_file.as_mut() {
                        writeln!(file, "--- {} stdout ---", file_name).ok();
                        let clean_stdout = remove_ansi_codes(&String::from_utf8_lossy(&out.stdout));
                        writeln!(file, "{}", clean_stdout).ok();
                        writeln!(file).ok();
                        file.flush().unwrap_or_else(|e| {
                            eprintln!("{}", format!("Failed to flush stdout file: {}", e).red());
                        });
                    }
                }
                Err(err) => {
                    println!("{}", format!("ERROR ({})", err).yellow());
                    failed.push((file_name.to_string(), format!("Failed to execute: {}", err)));

                    if let Some(file) = test_stdout_file.as_mut() {
                        writeln!(file, "--- {} stdout ---", file_name).ok();
                        writeln!(file, "<Failed to execute command>").ok();
                        writeln!(file).ok();
                        file.flush().unwrap_or_else(|e| {
                            eprintln!("{}", format!("Failed to flush stdout file: {}", e).red());
                        });
                    }
                }
            }
        }
    }

    if let Some(dir) = stdout_dir {
        println!("\n{}", "──────────────────────────────────────".dimmed());
        println!("\n{}", format!("Outputs written to directory: {}", dir.display()).dimmed());
    }

    if !filter_prefixes.is_empty() {
        let not_found: Vec<_> = filter_prefixes.iter()
            .filter(|p| !found_prefixes.contains(*p))
            .collect();

        for nf in &not_found {
            eprintln!("{}", format!("No tests found starting with prefix: {}", nf).yellow());
        }

        if passed.is_empty() && failed.is_empty() && !not_found.is_empty() {
            std::process::exit(1);
        }
    }

    println!("\n{}", "──────────────────────────────────────".dimmed());
    println!("{}", "Test Summary".bold().purple());

    println!("{} {}", "Passed:".green(), passed.len());
    for test in &passed {
        println!("  {}", test.green());
    }

    println!("\n{} {}", "Failed:".red(), failed.len());

    for (test, _) in &failed {
        println!("{}", test.red());
    }
    for (test, stderr) in &failed {
        println!("\n{}", format!("--- {} stderr ---", test).dimmed());
        println!("{}", stderr.yellow());
    }

    println!("{}", "──────────────────────────────────────".dimmed());

    if failed.is_empty() {
        println!("{}", "ALL TESTS PASSED".bold().on_green().black());
    } else {
        println!("{}", format!("{} test(s) failed.", failed.len()).bold().on_red().white());
        std::process::exit(1);
    }
}
