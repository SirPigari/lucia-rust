use std::fs::{self, File};
use std::io::{Read, Write};
use std::path::{Path, PathBuf};

use sha2::{Sha256, Digest};
use bincode::{Encode, Decode, config::{standard, Configuration, BigEndian, LittleEndian}, encode_to_vec, decode_from_slice};
use zstd::bulk::{compress, decompress};
use serde::{Serialize, Deserialize};

use crate::env::runtime::tokens::Token;
use crate::env::runtime::internal_structs::{Cache, CacheFormat};

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

fn cache_file_path(cache_dir: &Path, file_path: &str, file_hash: &str, kind: &str, format: CacheFormat) -> PathBuf {
    let extension = match format {
        CacheFormat::Json => "json",
        CacheFormat::BinLe | CacheFormat::BinBe |
        CacheFormat::ZstdLeFast | CacheFormat::ZstdLeBest |
        CacheFormat::ZstdBeFast | CacheFormat::ZstdBeBest => "bin",
        CacheFormat::NoCache => panic!("NoCache is not allowed in cache_file_path"),
    };

    cache_dir_for_file(cache_dir, file_path, file_hash).join(format!("{}.{}", kind, extension))
}

fn get_bincode_config_le() -> Configuration<LittleEndian> {
    standard().with_little_endian().with_no_limit()
}

fn get_bincode_config_be() -> Configuration<BigEndian> {
    standard().with_big_endian().with_no_limit()
}

fn write_serialized<T: Serialize + Encode + ?Sized>(
    path: &Path,
    value: &T,
    format: CacheFormat,
) -> std::io::Result<()> {
    if format == CacheFormat::NoCache {
        panic!("Refusing to write with CacheFormat::NoCache");
    }

    let mut file = File::create(path)?;

    match format {
        CacheFormat::Json => {
            let json = serde_json::to_vec_pretty(value).expect("json serialization failed");
            file.write_all(&json)?;
        }
        CacheFormat::BinLe => {
            let encoded = encode_to_vec(value, get_bincode_config_le()).expect("bincode failed");
            file.write_all(&encoded)?;
        }
        CacheFormat::BinBe => {
            let encoded = encode_to_vec(value, get_bincode_config_be()).expect("bincode failed");
            file.write_all(&encoded)?;
        }
        CacheFormat::ZstdLeFast | CacheFormat::ZstdLeBest | CacheFormat::ZstdBeFast | CacheFormat::ZstdBeBest => {
            let level = format.compression_level().unwrap();
            let serialized = match format {
                CacheFormat::ZstdLeFast | CacheFormat::ZstdLeBest => encode_to_vec(value, get_bincode_config_le()).expect("bincode fail"),
                CacheFormat::ZstdBeFast | CacheFormat::ZstdBeBest => encode_to_vec(value, get_bincode_config_be()).expect("bincode fail"),
                _ => unreachable!(),
            };
            let compressed = compress(&serialized, level).expect("zstd compress fail");

            file.write_all(&(serialized.len() as u64).to_le_bytes())?;
            file.write_all(&compressed)?;

            #[cfg(windows)]
            {
                use std::process::Command;
                if let Some(path_str) = path.parent().and_then(|p| p.to_str()) {
                    let output = Command::new("compact")
                        .args(&["/C", "/I", "/Q", path_str])
                        .output();

                    if let Ok(output) = output {
                        if !output.status.success() {
                            return Err(std::io::Error::new(
                                std::io::ErrorKind::Other,
                                format!("compact failed: {}", String::from_utf8_lossy(&output.stderr)),
                            ));
                        }
                    }
                }
            }
        }
        CacheFormat::NoCache => unreachable!(),
    }

    Ok(())
}

fn read_serialized<T: for<'de> Deserialize<'de> + Decode<()> + Sized>(
    path: &Path,
    format: CacheFormat,
) -> std::io::Result<T> {
    if format == CacheFormat::NoCache {
        panic!("Refusing to read with CacheFormat::NoCache");
    }

    let mut file = File::open(path)?;

    match format {
        CacheFormat::Json => {
            let mut json = String::new();
            file.read_to_string(&mut json)?;
            let value = serde_json::from_str(&json)?;
            Ok(value)
        }
        CacheFormat::BinLe => {
            let mut data = Vec::new();
            file.read_to_end(&mut data)?;
            let value = decode_from_slice(&data, get_bincode_config_le())
                .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, format!("decode fail: {:?}", e)))?
                .0;
            Ok(value)
        }
        CacheFormat::BinBe => {
            let mut data = Vec::new();
            file.read_to_end(&mut data)?;
            let value = decode_from_slice(&data, get_bincode_config_be())
                .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, format!("decode fail: {:?}", e)))?
                .0;
            Ok(value)
        }
        CacheFormat::ZstdLeFast | CacheFormat::ZstdLeBest | CacheFormat::ZstdBeFast | CacheFormat::ZstdBeBest => {
            let mut size_buf = [0u8; 8];
            file.read_exact(&mut size_buf)?;
            let decompressed_size = u64::from_le_bytes(size_buf) as usize;

            let mut compressed = Vec::new();
            file.read_to_end(&mut compressed)?;

            let decompressed = decompress(&compressed, decompressed_size)
                .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, format!("decompress fail: {}", e)))?;

            let value = match format {
                CacheFormat::ZstdLeFast | CacheFormat::ZstdLeBest => decode_from_slice(&decompressed, get_bincode_config_le())
                    .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, format!("decode fail: {:?}", e)))?
                    .0,
                CacheFormat::ZstdBeFast | CacheFormat::ZstdBeBest => decode_from_slice(&decompressed, get_bincode_config_be())
                    .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, format!("decode fail: {:?}", e)))?
                    .0,
                _ => unreachable!(),
            };

            Ok(value)
        }
        CacheFormat::NoCache => unreachable!(),
    }
}

pub fn save_tokens_to_cache(
    cache_dir: &Path,
    file_path: &str,
    kind: &str,
    tokens: &[Token],
    format: CacheFormat,
) -> std::io::Result<()> {
    let hash = hash_file_content(file_path)?;
    let dir = cache_dir_for_file(cache_dir, file_path, &hash);
    fs::create_dir_all(&dir)?;
    let path = cache_file_path(cache_dir, file_path, &hash, kind, format);
    write_serialized(&path, tokens, format)
}

pub fn load_tokens_from_cache(
    cache_dir: &Path,
    file_path: &str,
    kind: &str,
    format: CacheFormat,
) -> std::io::Result<Option<Vec<Token>>> {
    let hash = hash_file_content(file_path)?;
    let path = cache_file_path(cache_dir, file_path, &hash, kind, format);

    if path.exists() {
        let tokens = read_serialized(&path, format)?;
        Ok(Some(tokens))
    } else {
        Ok(None)
    }
}

pub fn load_interpreter_cache(cache_dir: &Path, format: CacheFormat) -> std::io::Result<Option<Cache>> {
    if format == CacheFormat::NoCache {
        panic!("Refusing to load interpreter cache with NoCache");
    }

    let path = cache_dir.join("interpreter_cache.bin");

    if path.exists() {
        let cache = read_serialized(&path, format)?;
        Ok(Some(cache))
    } else {
        Ok(None)
    }
}

pub fn save_interpreter_cache(cache_dir: &Path, new_cache: &Cache, format: CacheFormat) -> std::io::Result<()> {
    if format == CacheFormat::NoCache {
        panic!("Refusing to save interpreter cache with NoCache");
    }

    let path = cache_dir.join("interpreter_cache.bin");

    let merged = match load_interpreter_cache(cache_dir, format)? {
        Some(mut existing) => {
            existing.operations.extend(new_cache.operations.clone());
            existing.constants.extend(new_cache.constants.clone());
            existing.iterables.extend(new_cache.iterables.clone());
            existing
        }
        None => new_cache.clone(),
    };

    fs::create_dir_all(cache_dir)?;
    write_serialized(&path, &merged, format)
}
