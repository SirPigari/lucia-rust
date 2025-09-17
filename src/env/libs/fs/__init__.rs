use std::collections::HashMap;
use std::fs::{self, OpenOptions};
use std::io::{Write};
use std::path::Path;
use std::sync::Arc;
use std::env as std_env;
use std::fs::File;

use crate::env::runtime::functions::{Function, NativeFunction, Parameter};
use crate::env::runtime::utils::{to_static, fix_path};
use crate::env::runtime::value::Value;
use crate::env::runtime::variables::Variable;
use crate::insert_native_fn;

use hound::{WavWriter, WavSpec, SampleFormat};
use symphonia::core::audio::{SampleBuffer};
use symphonia::core::codecs::DecoderOptions;
use symphonia::core::formats::FormatOptions;
use symphonia::core::io::MediaSourceStream;
use symphonia::core::meta::MetadataOptions;
use symphonia::default::get_probe;

// This module provides file system operations and utilities.
// It includes functions for reading and writing files, checking file existence, and manipulating file paths.
// Lucia version 2.0.0, module: fs@0.4.0

fn convert_audio_file(input: &str, output: &str) -> Result<(), String> {
    let file = File::open(input).map_err(|e| e.to_string())?;
    let mss = MediaSourceStream::new(Box::new(file), Default::default());

    let probed = get_probe()
        .format(
            &Default::default(),
            mss,
            &FormatOptions::default(),
            &MetadataOptions::default(),
        )
        .map_err(|e| e.to_string())?;
    let mut format = probed.format;

    let track = format
        .default_track()
        .ok_or_else(|| "no default track".to_string())?;
    let mut decoder = symphonia::default::get_codecs()
        .make(&track.codec_params, &DecoderOptions::default())
        .map_err(|e| e.to_string())?;

    let spec = WavSpec {
        channels: track
            .codec_params
            .channels
            .ok_or_else(|| "unknown channel count".to_string())?
            .count() as u16,
        sample_rate: track
            .codec_params
            .sample_rate
            .ok_or_else(|| "unknown sample rate".to_string())?,
        bits_per_sample: 16,
        sample_format: SampleFormat::Int,
    };
    let mut writer =
        WavWriter::create(Path::new(output), spec).map_err(|e| e.to_string())?;
    let mut sample_buf: Option<SampleBuffer<i16>> = None;

    loop {
        let packet = match format.next_packet() {
            Ok(p) => p,
            Err(_) => break,
        };

        let decoded = decoder.decode(&packet).map_err(|e| e.to_string())?;
        let buf = match &mut sample_buf {
            Some(buf) => {
                buf.copy_interleaved_ref(decoded);
                buf
            }
            None => {
                let spec = *decoded.spec();
                let capacity = decoded.capacity() as u64;
                let _ = sample_buf.insert(SampleBuffer::<i16>::new(capacity, spec));
                sample_buf.as_mut().unwrap()
            }
        };

        for sample in buf.samples() {
            writer.write_sample(*sample).map_err(|e| e.to_string())?;
        }
    }

    writer.finalize().map_err(|e| e.to_string())?;
    Ok(())
}

fn convert_audio_file_handler(args: &HashMap<String, Value>) -> Value {
    if let (Some(Value::String(input)), Some(Value::String(output))) =
        (args.get("input"), args.get("output"))
    {
        match convert_audio_file(input, output) {
            Ok(_) => Value::Null,
            Err(e) => Value::Error("ConversionError", to_static(e), None),
        }
    } else {
        Value::Error("TypeError", "expected 'input' and 'output' as strings", None)
    }
}

fn fix_path_handler(args: &HashMap<String, Value>) -> Value {
    if let Some(Value::String(path)) = args.get("path") {
        let fixed_path = fix_path(path.to_string());
        Value::String(fixed_path)
    } else {
        Value::Error("TypeError", "expected 'path' as string", None)
    }
}

fn basename_handler(args: &HashMap<String, Value>) -> Value {
    if let Some(Value::String(path)) = args.get("path") {
        if let Some(name) = Path::new(path).file_name() {
            if let Some(name_str) = name.to_str() {
                return Value::String(name_str.to_string());
            }
        }
        Value::Error("ValueError", "could not extract basename", None)
    } else {
        Value::Error("TypeError", "expected 'path' as string", None)
    }
}

fn extension_handler(args: &HashMap<String, Value>) -> Value {
    if let Some(Value::String(path)) = args.get("path") {
        if let Some(ext) = Path::new(path).extension() {
            if let Some(ext_str) = ext.to_str() {
                return Value::String(ext_str.to_string());
            }
        }
        Value::Error("ValueError", "could not extract extension", None)
    } else {
        Value::Error("TypeError", "expected 'path' as string", None)
    }
}

fn write_file_handler(args: &HashMap<String, Value>) -> Value {
    if let (Some(Value::String(path)), Some(Value::String(content))) =
        (args.get("path"), args.get("content"))
    {
        match fs::write(path, content) {
            Ok(_) => Value::Null,
            Err(e) => Value::Error("IOError", to_static(e.to_string()), None),
        }
    } else {
        Value::Error("TypeError", "expected 'path' and 'content' as strings", None)
    }
}

fn read_file_handler(args: &HashMap<String, Value>) -> Value {
    if let Some(Value::String(path)) = args.get("path") {
        match fs::read_to_string(path) {
            Ok(data) => Value::String(data),
            Err(e) => Value::Error("IOError", to_static(e.to_string()), None),
        }
    } else {
        Value::Error("TypeError", "expected 'path' as string", None)
    }
}

fn read_file_to_bytes_handler(args: &HashMap<String, Value>) -> Value {
    if let Some(Value::String(path)) = args.get("path") {
        match fs::read(path) {
            Ok(data) => Value::Bytes(data),
            Err(e) => Value::Error("IOError", to_static(e.to_string()), None),
        }
    } else {
        Value::Error("TypeError", "expected 'path' as string", None)
    }
}

fn append_file_handler(args: &HashMap<String, Value>) -> Value {
    if let (Some(Value::String(path)), Some(Value::String(content))) =
        (args.get("path"), args.get("content"))
    {
        match OpenOptions::new().append(true).create(true).open(path) {
            Ok(mut file) => {
                if let Err(e) = file.write_all(content.as_bytes()) {
                    Value::Error("IOError", to_static(e.to_string()), None)
                } else {
                    Value::Null
                }
            }
            Err(e) => Value::Error("IOError", to_static(e.to_string()), None),
        }
    } else {
        Value::Error("TypeError", "expected 'path' and 'content' as strings", None)
    }
}

fn file_exists_handler(args: &HashMap<String, Value>) -> Value {
    if let Some(Value::String(path)) = args.get("path") {
        let exists = Path::new(path).exists();
        Value::Boolean(exists)
    } else {
        Value::Error("TypeError", "expected 'path' as string", None)
    }
}

fn dir_exists_handler(args: &HashMap<String, Value>) -> Value {
    if let Some(Value::String(path)) = args.get("path") {
        let exists = Path::new(path).is_dir();
        Value::Boolean(exists)
    } else {
        Value::Error("TypeError", "expected 'path' as string", None)
    }
}

fn delete_file_handler(args: &HashMap<String, Value>) -> Value {
    if let Some(Value::String(path)) = args.get("path") {
        match fs::remove_file(path) {
            Ok(_) => Value::Null,
            Err(e) => Value::Error("IOError", to_static(e.to_string()), None),
        }
    } else {
        Value::Error("TypeError", "expected 'path' as string", None)
    }
}

fn make_dir_handler(args: &HashMap<String, Value>) -> Value {
    if let Some(Value::String(path)) = args.get("path") {
        match fs::create_dir_all(path) {
            Ok(_) => Value::Null,
            Err(e) => Value::Error("IOError", to_static(e.to_string()), None),
        }
    } else {
        Value::Error("TypeError", "expected 'path' as string", None)
    }
}

fn remove_dir_handler(args: &HashMap<String, Value>) -> Value {
    if let Some(Value::String(path)) = args.get("path") {
        match fs::remove_dir_all(path) {
            Ok(_) => Value::Null,
            Err(e) => Value::Error("IOError", to_static(e.to_string()), None),
        }
    } else {
        Value::Error("TypeError", "expected 'path' as string", None)
    }
}

fn read_dir_handler(args: &HashMap<String, Value>) -> Value {
    if let Some(Value::String(path)) = args.get("path") {
        match fs::read_dir(path) {
            Ok(read_dir) => {
                let mut entries = vec![];
                for entry in read_dir {
                    if let Ok(entry) = entry {
                        if let Some(name) = entry.file_name().to_str() {
                            entries.push(Value::String(name.to_string()));
                        }
                    }
                }
                Value::List(entries)
            }
            Err(e) => Value::Error("IOError", to_static(e.to_string()), None),
        }
    } else {
        Value::Error("TypeError", "expected 'path' as string", None)
    }
}

fn change_dir_handler(args: &HashMap<String, Value>) -> Value {
    if let Some(Value::String(path)) = args.get("path") {
        match std_env::set_current_dir(path) {
            Ok(_) => Value::Null,
            Err(e) => Value::Error("IOError", to_static(e.to_string()), None),
        }
    } else {
        Value::Error("TypeError", "expected 'path' as string", None)
    }
}

fn current_dir_handler(_: &HashMap<String, Value>) -> Value {
    match std_env::current_dir() {
        Ok(path) => Value::String(path.to_string_lossy().to_string()),
        Err(e) => Value::Error("IOError", to_static(e.to_string()), None),
    }
}

fn size_bytes_handler(args: &HashMap<String, Value>) -> Value {
    if let Some(Value::String(path)) = args.get("path") {
        match fs::metadata(path) {
            Ok(metadata) => Value::Int((metadata.len() as i64).into()),
            Err(e) => Value::Error("IOError", to_static(e.to_string()), None),
        }
    } else {
        Value::Error("TypeError", "expected 'path' as string", None)
    }
}

fn size_bits_handler(args: &HashMap<String, Value>) -> Value {
    if let Some(Value::String(path)) = args.get("path") {
        match fs::metadata(path) {
            Ok(metadata) => Value::Int(((metadata.len() * 8) as i64).into()),
            Err(e) => Value::Error("IOError", to_static(e.to_string()), None),
        }
    } else {
        Value::Error("TypeError", "expected 'path' as string", None)
    }
}

fn cwd_handler(_: &HashMap<String, Value>) -> Value {
    match std_env::current_dir() {
        Ok(path) => Value::String(fix_path(path.display().to_string())),
        Err(e) => Value::Error("IOError", to_static(e.to_string()), None),
    }
}

fn size_formatted_handler(args: &HashMap<String, Value>) -> Value {
    let path_val = args.get("path");
    let unit_val = args.get("unit");

    let path = match path_val {
        Some(Value::String(s)) => s,
        _ => return Value::Error("TypeError", "expected 'path' as string", None),
    };

    let unit = match unit_val {
        Some(Value::String(s)) => s.to_uppercase(),
        None => "AUTO".to_string(),
        _ => return Value::Error("TypeError", "'unit' must be string if provided", None),
    };

    let size_bytes = match fs::metadata(path) {
        Ok(meta) => meta.len() as f64,
        Err(e) => return Value::Error("IOError", to_static(e.to_string()), None),
    };

    let units = [
        ("B", 1_f64),
        ("KB", 1024_f64),
        ("MB", 1024_f64.powf(2.0)),
        ("GB", 1024_f64.powf(3.0)),
        ("TB", 1024_f64.powf(4.0)),
        ("PB", 1024_f64.powf(5.0)),
        ("PT", 1024_f64.powf(5.0)),
    ];

    let (multiplier, display_unit) = if unit == "AUTO" {
        let mut chosen = units[0];
        for u in units.iter() {
            if size_bytes / u.1 >= 1.0 {
                chosen = *u;
            }
        }
        (chosen.1, chosen.0)
    } else {
        match units.iter().find(|(name, _)| *name == unit) {
            Some(&(name, mul)) => (mul, name),
            None => return Value::Error("ValueError", "unsupported unit", None),
        }
    };
    
    let size_converted = size_bytes / multiplier;

    let formatted = if size_converted != 0.0 && size_converted.abs() < 0.01 {
        format!("{:.2e} {}", size_converted, display_unit)
    } else if size_converted.fract() == 0.0 {
        format!("{:.0} {}", size_converted, display_unit)
    } else {
        format!("{:.2} {}", size_converted, display_unit)
    };    

    Value::String(formatted)
}

pub fn register() -> HashMap<String, Variable> {
    let mut map = HashMap::new();

    insert_native_fn!(
        map,
        "write_file",
        write_file_handler,
        vec![
            Parameter::positional("path", "str"),
            Parameter::positional("content", "str")
        ],
        "void"
    );
    insert_native_fn!(
        map,
        "read_file",
        read_file_handler,
        vec![Parameter::positional("path", "str")],
        "str"
    );
    insert_native_fn!(
        map,
        "read_file_to_bytes",
        read_file_to_bytes_handler,
        vec![Parameter::positional("path", "str")],
        "bytes"
    );
    insert_native_fn!(
        map,
        "append_file",
        append_file_handler,
        vec![
            Parameter::positional("path", "str"),
            Parameter::positional("content", "str")
        ],
        "void"
    );
    insert_native_fn!(
        map,
        "file_exists",
        file_exists_handler,
        vec![Parameter::positional("path", "str")],
        "bool"
    );
    insert_native_fn!(
        map,
        "dir_exists",
        dir_exists_handler,
        vec![Parameter::positional("path", "str")],
        "bool"
    );
    insert_native_fn!(
        map,
        "delete_file",
        delete_file_handler,
        vec![Parameter::positional("path", "str")],
        "void"
    );
    insert_native_fn!(
        map,
        "make_dir",
        make_dir_handler,
        vec![Parameter::positional("path", "str")],
        "void"
    );
    insert_native_fn!(
        map,
        "remove_dir",
        remove_dir_handler,
        vec![Parameter::positional("path", "str")],
        "void"
    );
    insert_native_fn!(
        map,
        "read_dir",
        read_dir_handler,
        vec![Parameter::positional("path", "str")],
        "list"
    );
    insert_native_fn!(
        map,
        "change_dir",
        change_dir_handler,
        vec![Parameter::positional("path", "str")],
        "void"
    );
    insert_native_fn!(
        map,
        "current_dir",
        current_dir_handler,
        vec![],
        "str"
    );
    insert_native_fn!(
        map,
        "size_bytes",
        size_bytes_handler,
        vec![Parameter::positional("path", "str")],
        "int"
    );
    insert_native_fn!(
        map,
        "size_bits",
        size_bits_handler,
        vec![Parameter::positional("path", "str")],
        "int"
    );
    insert_native_fn!(
        map,
        "size_formatted",
        size_formatted_handler,
        vec![
            Parameter::positional("path", "str"),
            Parameter::positional_optional("unit", "str", "AUTO".into())
        ],
        "str"
    );
    insert_native_fn!(
        map,
        "convert_audio_file",
        convert_audio_file_handler,
        vec![
            Parameter::positional("input", "str"),
            Parameter::positional("output", "str")
        ],
        "void"
    );
    insert_native_fn!(
        map,
        "basename",
        basename_handler,
        vec![Parameter::positional("path", "str")],
        "str"
    );
    insert_native_fn!(
        map,
        "extension",
        extension_handler,
        vec![Parameter::positional("path", "str")],
        "str"
    );
    insert_native_fn!(
        map,
        "fix_path",
        fix_path_handler,
        vec![Parameter::positional("path", "str")],
        "str"
    );
    insert_native_fn!(
        map,
        "cwd",
        cwd_handler,
        vec![],
        "str"
    );

    map
}
