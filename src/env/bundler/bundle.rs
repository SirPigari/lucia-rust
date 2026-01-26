use std::{
    env,
    fs,
    path::PathBuf,
};

use tar::{Builder, Header};
use zstd::stream::Encoder;

use crate::env::runtime::config::Config;
use crate::env::runtime::errors::Error;
use crate::env::bundler::template::common::*;
use crate::{BUILD_UUID, VERSION};

// TODO
// When adding a non-native lib it cannot find it.
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
    default_argv: Vec<String>,
) -> Result<String, (i32, Vec<Error>)> {
    let log = |msg: &str| { if !quiet_flag { println!("{}", msg); } };
    let extension = if cfg!(windows) { ".exe" } else { "" };

    log("Loading configuration...");
    let mut config = serde_json::from_str::<Config>(
        &fs::read_to_string(config_path).unwrap()
    ).map_err(|e| (1, vec![Error::new_anonymous("ConfigError", &e.to_string())]))?;

    let home_dir = PathBuf::from(&config.home_dir);
    config.home_dir = ".".to_string();
    config.version = VERSION.to_string();

    log("Collecting library paths...");
    let libs_paths: Vec<PathBuf> = libraries.clone().into_iter().map(|lib| {
        let p = home_dir.join("libs").join(lib);
        if p.is_absolute() { p } else { home_dir.join(p) }
    }).collect();

    print!("Collected {} libraries: \n", libs_paths.len());
    for lib_name in &libraries {
        print!("{}, ", lib_name);
    }
    println!();

    log("Collecting include files...");
    let files_to_include: Vec<PathBuf> = includes.into_iter().map(|inc| {
        let p = PathBuf::from(inc);
        if p.is_absolute() { p } else { env::current_dir().unwrap().join(p) }
    }).collect();

    let lucia_exe_path = home_dir.join("bin").join(format!("lucia{}", extension));

    let main_file_name = PathBuf::from(main_file)
        .file_name().unwrap().to_string_lossy().to_string();
    let main_tar_path = format!("src/{}", main_file_name);

    let command = command_to_run.unwrap_or_else(|| {
        if cfg!(windows) {
            format!("lucia.exe {} --config config.json -q {} -- ", main_tar_path, default_argv.join(" "))
        } else {
            format!("./lucia {} --config config.json -q {} -- ", main_tar_path, default_argv.join(" "))
        }
    });

    log("Copying runner template...");
    let runner_template_path = home_dir
        .join("bundler")
        .join("template")
        .join(format!("runner_template{}", extension));

    fs::copy(&runner_template_path, output_path)
        .map_err(|e| (1, vec![Error::new_anonymous("IOError", &e.to_string())]))?;

    log("Building TAR in memory...");
    let mut tar_data = Vec::new();

    {
        let level = match opt_level {
            "0" => 1,
            "1" => 3,
            "2" => 5,
            "3" => 7,
            "s" | "S" | "z" | "Z" => 9,
            _ => 5,
        };

        let encoder = Encoder::new(&mut tar_data, level)
            .map_err(|e| (1, vec![Error::new_anonymous("ZstdError", &e.to_string())]))?;
        let mut builder = Builder::new(encoder);

        log(&format!("Adding lucia{}...", extension));
        builder.append_path_with_name(&lucia_exe_path, format!("lucia{}", extension))
            .map_err(|e| (1, vec![Error::new_anonymous("TarError", &e.to_string())]))?;

        log("Adding config.json...");
        let config_bytes = serde_json::to_vec_pretty(&config).unwrap();
        let mut h = Header::new_gnu();
        h.set_size(config_bytes.len() as u64);
        h.set_mode(0o644);
        h.set_cksum();
        builder.append_data(&mut h, "config.json", &config_bytes[..]).unwrap();

        log("Adding libs.json...");
        let libs_json_path = PathBuf::from(config_path)
            .parent()
            .unwrap()
            .join("libs.json");

        if libs_json_path.exists() {
            builder.append_path_with_name(&libs_json_path, "libs.json")
                .map_err(|e| (1, vec![Error::new_anonymous("TarError", &e.to_string())]))?;
        }

        log("Adding run_command.txt...");
        let cmd_bytes = command.as_bytes();
        let mut h = Header::new_gnu();
        h.set_size(cmd_bytes.len() as u64);
        h.set_mode(0o644);
        h.set_cksum();
        builder.append_data(&mut h, "run_command.txt", cmd_bytes).unwrap();

        builder.append_path_with_name(main_file, &main_tar_path).unwrap();

        for f in &files_to_include {
            let name = f.file_name().unwrap().to_string_lossy();
            if f.exists() == false {
                eprintln!("Warning: Included file {:?} does not exist, skipping.", f);
                continue;
            }
            builder.append_path_with_name(f, format!("src/{}", name)).or_else(|e| {
                eprintln!("Error while including file {:?}: {:?}", f, e);
                Ok(())
            })?;
        }

        log("Adding libraries...");
        for lib in &libs_paths {
            let name = lib.file_name().unwrap().to_string_lossy();
            builder.append_path_with_name(lib, format!("libs/{}", name)).unwrap();
        }

        let encoder = builder.into_inner().unwrap();
        encoder.finish().unwrap();
    }

    log("Appending bundle data...");
    let mut exe_bytes = fs::read(output_path).unwrap();
    let data_offset = exe_bytes.len() as u64;

    exe_bytes.extend_from_slice(&tar_data);

    let lucia_version = {
        let mut ver = [0u8; 8];
        let ver_str = VERSION.as_bytes();
        let len = ver_str.len().min(8);
        ver[..len].copy_from_slice(&ver_str[..len]);
        ver
    };

    let lucia_uuid = {
        let mut uuid = [0u8; 16];
        let uuid_str = BUILD_UUID.as_bytes();
        let len = uuid_str.len().min(16);
        uuid[..len].copy_from_slice(&uuid_str[..len]);
        uuid
    };

    let footer = BundleFooter::new(data_offset, lucia_version, lucia_uuid);
    exe_bytes.extend_from_slice(&footer.as_bytes());

    fs::write(output_path, exe_bytes).unwrap();

    if run_flag {
        std::process::Command::new(output_path).spawn().ok();
    }

    Ok(format!("Bundled executable created at {}", output_path))
}
