use std::fs::{self, File};
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use sha2::{Sha256, Digest};
use bincode::{
    encode_to_vec, decode_from_slice,
    config::{standard, Config as BinCodeConfig},
    Encode, Decode,
};
use zstd::bulk::{compress, decompress};

use crate::env::runtime::tokens::Token;
use crate::env::runtime::internal_structs::Cache;

fn compact_bincode_config() -> impl BinCodeConfig {
    standard()
        .with_little_endian()
        .with_no_limit()
}

fn hash_file_content(path: &str) -> std::io::Result<String> {
    let abs_path = std::fs::canonicalize(path)?;
    let abs_path_str = abs_path.to_string_lossy();
    let mut hasher = Sha256::new();
    hasher.update(abs_path_str.as_bytes());
    Ok(format!("{:x}", hasher.finalize()))
}

fn cache_dir_for_file(cache_dir: &Path, file_path: &str, file_hash: &str) -> PathBuf {
    let filename = Path::new(file_path)
        .file_stem()
        .and_then(|n| n.to_str())
        .unwrap_or("unknown");
    cache_dir.join(format!("{}-{}", filename, file_hash))
}

fn cache_file_path(cache_dir: &Path, file_path: &str, file_hash: &str, kind: &str) -> PathBuf {
    cache_dir_for_file(cache_dir, file_path, file_hash).join(format!("{}.bin", kind))
}

fn write_compressed<T: Encode + ?Sized>(
    path: &Path,
    value: &T,
) -> std::io::Result<()> {
    let serialized = encode_to_vec(value, compact_bincode_config())
        .expect("failed to serialize data");
    let compressed = compress(&serialized, 1).expect("failed to compress data");

    let mut file = File::create(path)?;
    file.write_all(&(serialized.len() as u64).to_le_bytes())?;
    file.write_all(&compressed)?;

    #[cfg(windows)]
    {
        use std::process::Command;
        if let Some(path_str) = path.parent().and_then(|p| p.to_str()) {
            let output = Command::new("compact")
                .args(&["/C", "/I", "/Q", path_str])
                .output();

            match output {
                Ok(output) if output.status.success() => (),
                Ok(output) => {
                    return Err(std::io::Error::new(
                        std::io::ErrorKind::Other,
                        format!("compact failed with status {}: {}", output.status, String::from_utf8_lossy(&output.stderr)),
                    ));
                }
                Err(e) => {
                    return Err(std::io::Error::new(
                        std::io::ErrorKind::Other,
                        format!("failed to execute compact: {}", e),
                    ));
                }
            }
        }
    }

    Ok(())
}

fn read_compressed<T: Decode<()>>(
    path: &Path,
) -> std::io::Result<T> {
    let mut file = File::open(path)?;
    let mut size_buf = [0u8; 8];
    file.read_exact(&mut size_buf)?;
    let decompressed_size = u64::from_le_bytes(size_buf) as usize;

    let mut compressed = Vec::new();
    file.read_to_end(&mut compressed)?;

    let decompressed = decompress(&compressed, decompressed_size)
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, format!("failed to decompress: {}", e)))?;

    let (value, _) = decode_from_slice(&decompressed, compact_bincode_config())
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, format!("failed to deserialize: {:?}", e)))?;

    Ok(value)
}

pub fn save_tokens_to_cache(
    cache_dir: &Path,
    file_path: &str,
    kind: &str,
    tokens: &[Token],
) -> std::io::Result<()> {
    let hash = hash_file_content(file_path)?;
    let dir = cache_dir_for_file(cache_dir, file_path, &hash);
    fs::create_dir_all(&dir)?;
    let path = cache_file_path(cache_dir, file_path, &hash, kind);
    write_compressed(&path, tokens)
}

pub fn load_tokens_from_cache(
    cache_dir: &Path,
    file_path: &str,
    kind: &str,
) -> std::io::Result<Option<Vec<Token>>> {
    let hash = hash_file_content(file_path)?;
    let path = cache_file_path(cache_dir, file_path, &hash, kind);

    if path.exists() {
        let tokens = read_compressed(&path)?;
        Ok(Some(tokens))
    } else {
        Ok(None)
    }
}

#[allow(dead_code)]
fn interpreter_cache_path(cache_dir: &Path) -> PathBuf {
    cache_dir.join("interpreter_cache.bin")
}

#[allow(dead_code)]
pub fn load_interpreter_cache(cache_dir: &Path) -> std::io::Result<Option<Cache>> {
    let path = interpreter_cache_path(cache_dir);

    if path.exists() {
        let cache = read_compressed(&path)?;
        Ok(Some(cache))
    } else {
        Ok(None)
    }
}

#[allow(dead_code)]
pub fn save_interpreter_cache(cache_dir: &Path, new_cache: &Cache) -> std::io::Result<()> {
    let path = interpreter_cache_path(cache_dir);

    let merged = match load_interpreter_cache(cache_dir)? {
        Some(mut existing) => {
            existing.operations.extend(new_cache.operations.clone());
            existing.constants.extend(new_cache.constants.clone());
            existing.iterables.extend(new_cache.iterables.clone());
            existing
        }
        None => new_cache.clone(),
    };

    fs::create_dir_all(cache_dir)?;
    write_compressed(&path, &merged)
}
