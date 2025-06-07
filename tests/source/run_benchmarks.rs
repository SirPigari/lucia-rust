use std::{
    fs,
    env,
    process::Command,
    path::{Path, PathBuf},
    time::Instant,
    fs::File,
    io::Write,
    collections::{HashSet, HashMap},
};
use colored::*;
use serde::{Deserialize, Serialize};
use chrono::Utc;

#[derive(Deserialize, Serialize)]
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
    let filter_prefixes: Vec<String> = args.iter()
        .skip(1)
        .filter(|a| !a.starts_with("--"))
        .cloned()
        .collect();

    let save_results = args.iter().any(|a| a == "--save-results");

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
    let mut found_prefixes: HashSet<String> = HashSet::new();
    let mut times = vec![];

    let mut fastest_time = u128::MAX;
    let mut slowest_time = 0u128;
    let mut fastest_test = String::new();
    let mut slowest_test = String::new();

    // map file_name -> list of times for --save-results
    let mut times_map: HashMap<String, Vec<u128>> = HashMap::new();

    println!("{}", "Lucia Benchmark Runner".bold().underline().cyan());
    println!("Using lucia version {}", info.version.bold().green());
    println!("Rustc version: {}", info.rustc_version.bold().green());
    if info.git_hash != "not-cloned" && info.git_hash != "unknown" {
        println!("Git hash: {}", info.git_hash.bold().green());
    }

    println!("{}\n", "─────────────────────────────────────────".dimmed());

    let is_debug = info.profile == "debug";

    let all_entries: Vec<_> = entries.filter_map(Result::ok).collect();
    if all_entries.is_empty() {
        eprintln!("{}", "No benchmark files found.".red());
        std::process::exit(1);
    }

    let mut all_entries_sorted_filtered: Vec<_> = all_entries
        .iter()
        .filter(|entry| {
            let path = entry.path();
            path.is_file() && path.extension().map_or(false, |ext| ext == "lc")
        })
        .collect();

    all_entries_sorted_filtered.sort_by_key(|entry| {
        let binding = entry.path();
        let file_name = binding.file_name().and_then(|n| n.to_str()).unwrap_or("");
        file_name.split('_').next()
            .and_then(|num_str| num_str.parse::<u32>().ok())
            .unwrap_or(0)
    });

    for entry in all_entries_sorted_filtered.iter() {
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
            let mut durations = vec![];
            let mut success = true;

            let times_to_run = if is_debug { 5 } else { 10 };

            for _ in 0..times_to_run {
                let start = Instant::now();
                let output = Command::new(&target_path)
                    .arg(&path)
                    .arg("-q")
                    .output();
                let duration = start.elapsed().as_millis();
                match &output {
                    Ok(out) if out.status.success() => durations.push(duration),
                    _ => {
                        success = false;
                        break;
                    }
                }
            }

            if success {
                let avg_duration = durations.iter().sum::<u128>() / durations.len() as u128;
                println!("{}", format!("{} ms", avg_duration).green());
                total_time += avg_duration;
                times.push(avg_duration);
                times_map.insert(file_name.to_string(), durations);

                if avg_duration < fastest_time {
                    fastest_time = avg_duration;
                    fastest_test = file_name.to_string();
                }
                if avg_duration > slowest_time {
                    slowest_time = avg_duration;
                    slowest_test = file_name.to_string();
                }
                passed.push(file_name.to_string());
            } else {
                println!("{}", "FAILED".red());
                failed.push(file_name.to_string());
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

    println!("\n{}", "─────────────────────────────────────────".dimmed());
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
        println!("{}", "─────────────────────────────────────────".dimmed());
        println!("{} {}", "Note:".yellow(), "Benchmarks are slower in debug profile, consider using release profile for accurate results.");
    }
    println!("{}", "─────────────────────────────────────────".dimmed());

    if save_results {
        if let Err(e) = fs::create_dir_all("./benchmark-results") {
            eprintln!("{}", format!("Failed to create results directory: {}", e).red());
            std::process::exit(1);
        }

        let timestamp = Utc::now().format("%Y%m%d%H%M%S").to_string();
        let result_path = format!("./benchmark-results/{}.json", timestamp);

        let mut output_json = serde_json::Map::new();

        for (file_name, times_list) in &times_map {
            let times_json: Vec<serde_json::Value> = times_list.iter().map(|t| serde_json::json!(t)).collect();
            output_json.insert(file_name.clone(), serde_json::Value::Array(times_json));
        }

        output_json.insert("build-info".to_string(), serde_json::to_value(&info).unwrap());

        let json_value = serde_json::Value::Object(output_json);
        match File::create(&result_path) {
            Ok(mut file) => {
                if let Err(e) = file.write_all(serde_json::to_string_pretty(&json_value).unwrap().as_bytes()) {
                    eprintln!("{}", format!("Failed to write results file: {}", e).red());
                } else {
                    println!("{}", format!("Saved benchmark results to {}", result_path).bold().green());
                }
            }
            Err(e) => {
                eprintln!("{}", format!("Failed to create results file: {}", e).red());
            }
        }
    }

    if failed.is_empty() {
        println!("{}", "ALL BENCHMARKS PASSED".bold().on_green().black());
    } else {
        println!("{}", format!("{} benchmark(s) failed.", failed.len()).bold().on_red().white());
        std::process::exit(1);
    }
}
