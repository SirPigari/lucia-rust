use std::fs;
use std::path::{PathBuf, Path};
use sha2::{Sha256, Digest};
use bincode::{encode_to_vec, decode_from_slice, config::standard};
use crate::env::runtime::tokens::Token;

fn hash_file_content(path: &str) -> std::io::Result<String> {
    let content = fs::read(path)?;
    let mut hasher = Sha256::new();
    hasher.update(&content);
    Ok(format!("{:x}", hasher.finalize()))
}

fn cache_file_path(cache_dir: &Path, hash: &str, kind: &str) -> PathBuf {
    let mut path = PathBuf::from(cache_dir);
    path.push(format!("{}.{}.bin", hash, kind));
    path
}

pub fn save_tokens_to_cache(cache_dir: &Path, file_path: &str, kind: &str, tokens: &[Token]) -> std::io::Result<()> {
    let hash = hash_file_content(file_path)?;
    let path = cache_file_path(cache_dir, &hash, kind);
    fs::create_dir_all(cache_dir)?;
    let bytes = encode_to_vec(tokens, standard()).expect("failed to serialize tokens");
    fs::write(path, bytes)?;
    Ok(())
}

pub fn load_tokens_from_cache(cache_dir: &Path, file_path: &str, kind: &str) -> std::io::Result<Option<Vec<Token>>> {
    let hash = hash_file_content(file_path)?;
    let path = cache_file_path(cache_dir, &hash, kind);
    if path.exists() {
        let bytes = fs::read(path)?;
        let (tokens, _): (Vec<Token>, _) = decode_from_slice(&bytes, standard()).expect("failed to deserialize token cache");
        Ok(Some(tokens))
    } else {
        Ok(None)
    }
}
