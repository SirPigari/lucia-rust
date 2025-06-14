use std::{
    fs,
    env,
    process::Command,
    path::{Path, PathBuf}
};

use colored::*;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filter_prefixes: Vec<String> = if args.len() > 1 {
        args[1..].to_vec()
    } else {
        vec![]
    };

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

    let mut passed = vec![];
    let mut failed = vec![];
    let mut found_prefixes = std::collections::HashSet::new();

    println!("{}", "Lucia Test Runner".bold().underline().cyan());

    for entry in entries.filter_map(Result::ok) {
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
                .output();

            match output {
                Ok(out) if out.status.success() => {
                    println!("{}", "PASSED".green());
                    passed.push(file_name.to_string());
                }
                Ok(out) => {
                    println!("{}", "FAILED".red());
                    let stderr = String::from_utf8_lossy(&out.stderr).to_string();
                    failed.push((file_name.to_string(), stderr));
                }
                Err(err) => {
                    println!("{}", format!("ERROR ({})", err).yellow());
                    failed.push((file_name.to_string(), format!("Failed to execute: {}", err)));
                }
            }
        }
    }

    if !filter_prefixes.is_empty() {
        let not_found: Vec<_> = filter_prefixes.iter()
            .filter(|p| !found_prefixes.contains(*p))
            .collect();

        for nf in &not_found {
            eprintln!("{}", format!("No tests found starting with prefix: {}", nf).yellow());
        }

        if passed.is_empty() && failed.is_empty() && !&not_found.is_empty() {
            std::process::exit(1);
        }
    }

    println!("\n{}", "──────────────────────────────────────".dimmed());
    println!("{}", "Test Summary".bold().purple());

    println!("{} {}", "Passed:".green(), passed.len());
    for test in &passed {
        println!("  {}", test.green());
    }

    println!("\n{}", "Failed:".red());
    for (test, stderr) in &failed {
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
