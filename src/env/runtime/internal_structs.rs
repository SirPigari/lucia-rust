use crate::env::runtime::value::Value;
use std::collections::HashMap;
use serde::{Serialize, Deserialize, Serializer, Deserializer};
use bincode::{Encode, Decode};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum State {
    Normal,
    Exit,
    Defer,
    Break,
    Continue,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CacheFormat {
    NoCache,
    Json,
    BinLe,
    BinBe,
    ZstdLeFast,
    ZstdLeBest,
    ZstdBeFast,
    ZstdBeBest,
}

impl CacheFormat {
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "no_cache" => Some(Self::NoCache),
            "json" => Some(Self::Json),
            "bin_le" => Some(Self::BinLe),
            "bin_be" => Some(Self::BinBe),
            "zstd_le_fast" => Some(Self::ZstdLeFast),
            "zstd_le_best" => Some(Self::ZstdLeBest),
            "zstd_be_fast" => Some(Self::ZstdBeFast),
            "zstd_be_best" => Some(Self::ZstdBeBest),
            _ => None,
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            Self::NoCache => "no_cache".to_string(),
            Self::Json => "json".to_string(),
            Self::BinLe => "bin_le".to_string(),
            Self::BinBe => "bin_be".to_string(),
            Self::ZstdLeFast => "zstd_le_fast".to_string(),
            Self::ZstdLeBest => "zstd_le_best".to_string(),
            Self::ZstdBeFast => "zstd_be_fast".to_string(),
            Self::ZstdBeBest => "zstd_be_best".to_string(),
        }
    }

    pub fn compression_level(self) -> Option<i32> {
        match self {
            Self::ZstdLeFast | Self::ZstdBeFast => Some(1),
            Self::ZstdLeBest | Self::ZstdBeBest => Some(10),
            _ => None,
        }
    }

    pub fn is_enabled(self) -> bool {
        self != Self::NoCache
    }

    pub fn is_zstd(self) -> bool {
        self.compression_level().is_some()
    }
}

impl Serialize for CacheFormat {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

impl<'de> Deserialize<'de> for CacheFormat {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = <&str>::deserialize(deserializer)?;
        CacheFormat::from_str(s).ok_or_else(|| {
            serde::de::Error::custom(format!("invalid cache format: {s}"))
        })
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Encode, Decode)]
pub struct Cache {
    pub operations: HashMap<String, Value>,
    pub constants: HashMap<String, Value>,
    pub iterables: HashMap<Value, Value>,
}

#[derive(Debug, Clone)]
pub struct InternalStorage {
    pub types: HashMap<String, Value>,
    pub lambda_counter: usize,
}

#[derive(Serialize)]
pub struct BuildInfo {
    pub name: &'static str,
    pub version: &'static str,
    pub uuid: &'static str,
    pub rustc_version: &'static str,
    pub rustc_channel: &'static str,
    pub target: &'static str,
    pub repository: &'static str,
    pub git_hash: &'static str,
    pub file_hash: &'static str,
    pub profile: &'static str,
    pub ci: &'static str,
    pub build_date: &'static str,
    pub dependencies: &'static str,
}