use std::fs;
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use sha2::{Sha256, Digest};
use bincode::{encode_to_vec, decode_from_slice, config::{standard, Config}};
use zstd::bulk::{compress, decompress};
use crate::env::runtime::tokens::Token;

fn hash_file_content(path: &str) -> std::io::Result<String> {
    let abs_path = std::fs::canonicalize(path)?;
    let abs_path_str = abs_path.to_string_lossy();
    let mut hasher = Sha256::new();
    hasher.update(abs_path_str.as_bytes());
    Ok(format!("{:x}", hasher.finalize()))
}

fn compact_bincode_config() -> impl Config {
    standard()
        .with_little_endian()
        .with_no_limit()
}

fn cache_dir_for_file(cache_dir: &Path, file_path: &str, file_hash: &str) -> PathBuf {
    let filename = Path::new(file_path)
        .file_stem()
        .and_then(|n| n.to_str())
        .unwrap_or("unknown");
    cache_dir.join(format!("{}-{}", filename, file_hash))
}

fn cache_file_path(cache_dir: &Path, file_path: &str, file_hash: &str, kind: &str) -> PathBuf {
    let dir = cache_dir_for_file(cache_dir, file_path, file_hash);
    dir.join(format!("{}.bin", kind))
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

    let serialized = encode_to_vec(tokens, compact_bincode_config())
        .expect("failed to serialize tokens");
    let compressed = compress(&serialized, 1)
        .expect("failed to compress tokens");

    let mut file = fs::File::create(path)?;
    // Write decompressed size as 8 bytes little endian
    file.write_all(&(serialized.len() as u64).to_le_bytes())?;
    // Write compressed data
    file.write_all(&compressed)?;
    Ok(())
}

pub fn load_tokens_from_cache(
    cache_dir: &Path,
    file_path: &str,
    kind: &str,
) -> std::io::Result<Option<Vec<Token>>> {
    let hash = hash_file_content(file_path)?;
    let path = cache_file_path(cache_dir, file_path, &hash, kind);

    if path.exists() {
        let mut file = fs::File::open(&path)?;
        let mut size_buf = [0u8; 8];
        file.read_exact(&mut size_buf)?;
        let decompressed_size = u64::from_le_bytes(size_buf) as usize;

        let mut compressed = Vec::new();
        file.read_to_end(&mut compressed)?;

        let decompressed = decompress(&compressed, decompressed_size)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, format!("failed to decompress token cache: {}", e)))?;

        let (tokens, _) = decode_from_slice(&decompressed, compact_bincode_config())
            .expect("failed to deserialize token cache");
        Ok(Some(tokens))
    } else {
        Ok(None)
    }
}
