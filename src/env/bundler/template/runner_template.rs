mod common;
use common::*;

use std::{
    env,
    fs,
    io::{Read, Seek, SeekFrom},
    path::PathBuf,
    process::Command,
};

use tar::Archive;
use zstd::stream::read::Decoder;

fn main() {
    if let Err(e) = real_main() {
        eprintln!("Error: {}", e);
        eprintln!("Report this error to https://github.com/SirPigari/lucia-rust/issues/new");
        std::process::exit(1);
    }
}

fn real_main() -> Result<(), Box<dyn std::error::Error>> {
    let exe_path = env::current_exe()?;
    let mut exe = fs::File::open(&exe_path)?;
    let exe_len = exe.metadata()?.len();

    let footer_size = BundleFooter::size() as u64;
    if exe_len < footer_size {
        return Err("executable too small".into());
    }

    exe.seek(SeekFrom::End(-(footer_size as i64)))?;
    let mut footer_buf = vec![0u8; footer_size as usize];
    exe.read_exact(&mut footer_buf)?;

    let footer = BundleFooter::from_bytes(&footer_buf)
        .ok_or("invalid bundle footer")?;

    if footer.version != BUNDLE_VERSION {
        return Err(format!("unsupported bundle version {:x}", footer.version).into());
    }

    let data_offset = footer.data_offset;
    if data_offset >= exe_len {
        return Err("invalid data offset".into());
    }

    let bundle_id = format!(
        "bundle_{}_{}",
        data_offset,
        exe_len - data_offset - footer_size
    );

    let base_dir = bundle_base_dir()?;
    let extract_dir = base_dir.join(bundle_id);

    if !extract_dir.exists() {
        fs::create_dir_all(&extract_dir)?;
        exe.seek(SeekFrom::Start(data_offset))?;
        let decoder = Decoder::new(exe)?;
        let mut archive = Archive::new(decoder);
        archive.unpack(&extract_dir)?;
    }

    let command = fs::read_to_string(extract_dir.join("run_command.txt"))?
        .trim().to_string();

    let mut parts = command.split_whitespace();
    let exe_name = parts.next().ok_or("empty command")?;
    let mut args: Vec<String> = parts.map(|s| s.to_string()).collect();
    args.extend(env::args().skip(1));

    let mut child = Command::new(extract_dir.join(exe_name))
        .args(args)
        .current_dir(&extract_dir)
        .spawn()?;

    let _ = child.wait();
    Ok(())
}

fn bundle_base_dir() -> Result<PathBuf, Box<dyn std::error::Error>> {
    #[cfg(target_os = "windows")]
    {
        Ok(PathBuf::from(env::var("LOCALAPPDATA")?)
            .join("Lucia")
            .join("bundle"))
    }

    #[cfg(target_os = "macos")]
    {
        Ok(PathBuf::from(env::var("HOME")?)
            .join("Library")
            .join("Caches")
            .join("lucia")
            .join("bundle"))
    }

    #[cfg(all(unix, not(target_os = "macos")))]
    {
        if let Ok(xdg) = env::var("XDG_CACHE_HOME") {
            Ok(PathBuf::from(xdg).join("lucia").join("bundle"))
        } else {
            Ok(PathBuf::from(env::var("HOME")?)
                .join(".cache")
                .join("lucia")
                .join("bundle"))
        }
    }
}
