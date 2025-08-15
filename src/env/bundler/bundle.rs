use std::{
    env,
    fs,
    path::PathBuf,
};
use tar::Builder;
use flate2::write::GzEncoder;
use flate2::Compression;
use crate::env::runtime::config::Config;
use crate::env::runtime::errors::Error;

const VERSION: &str = env!("VERSION");

pub fn bundle_to_exe(
    config_path: &str,
    output_path: &str,
    main_file: &str,
    includes: Vec<String>,
    libraries: Vec<String>,
    _args: Vec<String>,
    command_to_run: Option<String>,
    run_flag: bool,
    opt_level: &str,
    quiet_flag: bool,
) -> Result<String, (i32, Vec<Error>)> {
    let log = |msg: &str| { if !quiet_flag { println!("[DEBUG] {}", msg); } };

    log("Loading configuration...");
    let mut config = serde_json::from_str::<Config>(&fs::read_to_string(config_path).unwrap())
        .map_err(|e| (1, vec![Error::new_anonymous("ConfigError", &e.to_string())]))?;
    let home_dir = PathBuf::from(&config.home_dir);
    config.home_dir = ".".to_string();
    config.version = VERSION.to_string();

    log("Collecting library paths...");
    let libs_paths: Vec<PathBuf> = libraries.into_iter().map(|lib| {
        let p = home_dir.join("libs").join(lib);
        if p.is_absolute() { p } else { home_dir.join(p) }
    }).collect();

    log("Collecting include files...");
    let files_to_include: Vec<PathBuf> = includes.into_iter().map(|inc| {
        let p = PathBuf::from(inc);
        if p.is_absolute() { p } else { env::current_dir().unwrap().join(p) }
    }).collect();

    let lucia_exe_path = home_dir.join("bin").join("lucia.exe");

    // Main file tar-relative name
    let main_file_name = PathBuf::from(main_file)
        .file_name()
        .unwrap()
        .to_string_lossy()
        .to_string();
    let main_tar_path = format!("src/{}", main_file_name);

    // Build the command relative to the tar contents
    let command = if let Some(cmd) = command_to_run {
        cmd
    } else {
        format!(".\\lucia.exe {} --config=.\\config.json", main_tar_path)
    };

    log("Copying runner template...");
    let runner_template_path = home_dir.join("bundler").join("template").join("runner_template.exe");
    fs::copy(&runner_template_path, output_path)
        .map_err(|e| (1, vec![Error::new_anonymous("IOError", &e.to_string())]))?;

    log("Building TAR in memory...");
    let mut tar_data: Vec<u8> = Vec::new();
    {
        let level = match opt_level {
            "0" => 0,
            "1" => 1,
            "2" => 2,
            "3" => 3,
            "s" | "S" | "z" | "Z" => 9,
            _ => 6,
        };
        let encoder = GzEncoder::new(&mut tar_data, Compression::new(level));
        let mut builder = Builder::new(encoder);

        log("Adding lucia.exe...");
        builder.append_path_with_name(&lucia_exe_path, "lucia.exe")
            .map_err(|e| (1, vec![Error::new_anonymous("TarError", &e.to_string())]))?;

        log("Adding config.json...");
        let temp_config_path = env::temp_dir().join("config.json");
        fs::write(&temp_config_path, serde_json::to_string_pretty(&config).unwrap())
            .map_err(|e| (1, vec![Error::new_anonymous("IOError", &e.to_string())]))?;
        builder.append_path_with_name(&temp_config_path, "config.json")
            .map_err(|e| (1, vec![Error::new_anonymous("TarError", &e.to_string())]))?;

        log("Adding libs.json...");
        let libs_json_path = PathBuf::from(config_path).parent().unwrap().join("libs.json");
        if libs_json_path.exists() {
            builder.append_path_with_name(&libs_json_path, "libs.json")
                .map_err(|e| (1, vec![Error::new_anonymous("TarError", &e.to_string())]))?;
        }

        log("Adding run_command.txt...");
        let temp_cmd_path = env::temp_dir().join("run_command.txt");
        fs::write(&temp_cmd_path, &command)
            .map_err(|e| (1, vec![Error::new_anonymous("IOError", &e.to_string())]))?;
        builder.append_path_with_name(&temp_cmd_path, "run_command.txt")
            .map_err(|e| (1, vec![Error::new_anonymous("TarError", &e.to_string())]))?;

        log("Adding main file and includes...");
        builder.append_path_with_name(main_file, &main_tar_path)
            .map_err(|e| (1, vec![Error::new_anonymous("TarError", &e.to_string())]))?;
        for f in &files_to_include {
            let file_name = f.file_name().unwrap().to_string_lossy();
            builder.append_path_with_name(f, format!("src/{}", file_name))
                .map_err(|e| (1, vec![Error::new_anonymous("TarError", &e.to_string())]))?;
        }

        log("Adding libraries...");
        for lib in &libs_paths {
            let lib_name = lib.file_name().unwrap().to_string_lossy();
            builder.append_path_with_name(lib, format!("libs/{}", lib_name))
                .map_err(|e| (1, vec![Error::new_anonymous("TarError", &e.to_string())]))?;
        }

        let encoder = builder.into_inner()
            .map_err(|e| (1, vec![Error::new_anonymous("TarError", &e.to_string())]))?;
        encoder.finish()
            .map_err(|e| (1, vec![Error::new_anonymous("TarError", &e.to_string())]))?;
    }

    log("Appending TAR and offset to output executable...");
    let mut exe_bytes = fs::read(output_path)
        .map_err(|e| (1, vec![Error::new_anonymous("IOError", &e.to_string())]))?;
    let data_offset = exe_bytes.len() as u64;
    exe_bytes.extend_from_slice(&tar_data);
    exe_bytes.extend_from_slice(&data_offset.to_le_bytes());
    fs::write(output_path, exe_bytes)
        .map_err(|e| (1, vec![Error::new_anonymous("IOError", &e.to_string())]))?;

    log("Bundling complete!");

    if run_flag {
        log("Running the bundled executable...");
        std::process::Command::new(output_path)
            .spawn()
            .map_err(|e| (1, vec![Error::new_anonymous("RunError", &e.to_string())]))?;
    }

    Ok(format!("Bundled executable created at {}", output_path))
}
