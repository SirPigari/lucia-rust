use std::{
    fs,
    env,
    process::Command,
    path::{Path, PathBuf},
    time::Instant,
};
use colored::*;
use serde::Deserialize;

#[derive(serde::Deserialize)]
struct BuildInfo {
    name: String,
    version: String,
    rustc_version: String,
    repository: String,
    git_hash: String,
    file_hash: String,
    profile: String,
}

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
            eprintln!("{}", format!("Failed to read benchmark directory: {}", err).red());
            std::process::exit(1);
        }
    };

    let output = Command::new(&target_path)
        .arg("--build-info")
        .output()
        .expect("Failed to run lucia --build-info");

    let info: BuildInfo = serde_json::from_slice(&output.stdout)
        .expect("Failed to parse build info");

    let mut failed = vec![];
    let mut passed = vec![];
    let mut total_time = 0u128;
    let mut found_prefixes = std::collections::HashSet::new();
    let mut times = vec![];

    let mut fastest_time = u128::MAX;
    let mut slowest_time = 0u128;
    let mut fastest_test = String::new();
    let mut slowest_test = String::new();

    println!("{}", "Lucia Benchmark Runner".bold().underline().cyan());
    println!("Using lucia version {}", info.version.bold().green());
    println!("Rustc version: {}", info.rustc_version.bold().green());
    if info.git_hash != "not-cloned" || info.git_hash != "unknown" {
        println!("Git hash: {}", info.git_hash.bold().green());
    }

    println!("{}\n", "──────────────────────────────────────".dimmed());

    let is_debug = info.profile == "debug";

    let all_entries: Vec<_> = entries.filter_map(Result::ok).collect();

    for entry in all_entries.iter() {
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

            print!("Benchmarking: {:<40} ... ", file_name);

            let start = Instant::now();
            let output = Command::new(&target_path)
                .arg(&path)
                .arg("-q")
                .output();
            let duration = start.elapsed();
            let duration_ms = duration.as_millis();
            total_time += duration_ms;
            times.push(duration_ms);

            if duration_ms < fastest_time {
                fastest_time = duration_ms;
                fastest_test = file_name.to_string();
            }
            if duration_ms > slowest_time {
                slowest_time = duration_ms;
                slowest_test = file_name.to_string();
            }

            match output {
                Ok(out) if out.status.success() => {
                    println!("{}", format!("{} ms", duration_ms).green());
                    passed.push(file_name.to_string());
                }
                _ => {
                    println!("{}", "FAILED".red());
                    failed.push(file_name.to_string());
                }
            }
        }
    }

    if !filter_prefixes.is_empty() {
        let not_found: Vec<_> = filter_prefixes.iter()
            .filter(|p| !found_prefixes.contains(*p))
            .collect();

        for nf in &not_found {
            eprintln!("{}", format!("No benchmarks found starting with prefix: {}", nf).yellow());
        }

        if failed.is_empty() && !not_found.is_empty() {
            std::process::exit(1);
        }
    }

    let total_benchmarks = passed.len() + failed.len();

    let average_time = if total_benchmarks > 0 {
        total_time as f64 / total_benchmarks as f64
    } else {
        0.0
    };

    let median_time = if !times.is_empty() {
        times.sort_unstable();
        let mid = times.len() / 2;
        if times.len() % 2 == 0 {
            (times[mid - 1] + times[mid]) / 2
        } else {
            times[mid]
        }
    } else {
        0
    };

    let success_rate = if total_benchmarks > 0 {
        (passed.len() as f64 / total_benchmarks as f64) * 100.0
    } else {
        0.0
    };

    println!("\n{}", "──────────────────────────────────────".dimmed());
    println!("{}", "Benchmark Summary".bold().purple());

    println!("{} {}", "Failed:".red(), failed.len());
    for test in &failed {
        println!("  {}", test.red());
    }
    println!("{} {} {}", "Total Time:".blue(), total_time.to_string().green(), "ms".green());
    println!("{} {}", "Total Benchmarks:".blue(), total_benchmarks);
    println!("{} {} ms", "Median Time:".blue(), median_time);
    println!("{} {:.2} ms", "Average Time:".blue(), average_time);
    println!("{} {} ({} ms)", "Fastest Benchmark:".green(), fastest_test, fastest_time);
    println!("{} {} ({} ms)", "Slowest Benchmark:".red(), slowest_test, slowest_time);
    println!("{} {:.2}%", "Success Rate:".green(), success_rate);

    if is_debug {
        println!("{}", "──────────────────────────────────────".dimmed());
        println!("{} {}", "Note:".yellow(), "Benchmarks are slower in debug profile, consider using release profile for accurate results.");
    }
    println!("{}", "──────────────────────────────────────".dimmed());

    if failed.is_empty() {
        println!("{}", "ALL BENCHMARKS PASSED".bold().on_green().black());
    } else {
        println!("{}", format!("{} benchmark(s) failed.", failed.len()).bold().on_red().white());
        std::process::exit(1);
    }
}
