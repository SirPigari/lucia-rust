use std::collections::HashMap;
use std::sync::Arc;
use crate::env::runtime::functions::{Function, Parameter};
use crate::env::runtime::types::{Int, Float};
use crate::env::runtime::value::Value;
use crate::env::runtime::variables::Variable;
use crate::env::runtime::utils::to_static;
use std::time::{Instant, SystemTime, UNIX_EPOCH};
#[cfg(not(target_arch = "wasm32"))]
use std::time::Duration;
#[cfg(not(target_arch = "wasm32"))]
use std::thread;
use crate::{insert_native_fn};
use chrono::{Utc, Local, Timelike, Datelike, NaiveDateTime, Duration as ChronoDuration};

const FEMTOSECONDS_PER_SECOND: i128 = 1_000_000_000_000_000;

// This module handles time and date functionality.
// It includes functions to get the current time, format dates, parse date strings, and retrieve specific time components.
// Lucia version 2.0.0, module: time@0.3.0

fn parse_time(args: &HashMap<String, Value>) -> Value {
    let format = match args.get("format") {
        Some(Value::String(s)) => s.as_str(),
        _ => return Value::Error("TypeError", "Expected a string for 'format'", None),
    };

    let time_str = match args.get("time") {
        Some(Value::String(s)) => s.as_str(),
        _ => return Value::Error("TypeError", "Expected a string for 'time'", None),
    };

    match NaiveDateTime::parse_from_str(time_str, format) {
        Ok(dt) => {
            let mut map = HashMap::new();
            map.insert("year".to_string(), Value::Int(Int::from_i64(dt.year().into())));
            map.insert("month".to_string(), Value::Int(Int::from_i64(dt.month().into())));
            map.insert("day".to_string(), Value::Int(Int::from_i64(dt.day().into())));
            map.insert("hour".to_string(), Value::Int(Int::from_i64(dt.hour().into())));
            map.insert("minute".to_string(), Value::Int(Int::from_i64(dt.minute().into())));
            map.insert("second".to_string(), Value::Int(Int::from_i64(dt.second().into())));
            Value::Map {
                keys: map.keys().cloned().map(Value::String).collect(),
                values: map.values().cloned().collect(),
            }
        }
        Err(e) => Value::Error("ParseError", to_static(format!("{}", e)), None),
    }
}

// Apollo 11 moon landing time (UTC)
fn apollo_epoch() -> NaiveDateTime {
    NaiveDateTime::parse_from_str("1969-07-20 20:17:40", "%Y-%m-%d %H:%M:%S").unwrap()
}

fn apollo_time_to_datetime(femto: i128) -> Option<NaiveDateTime> {
    let secs = femto / FEMTOSECONDS_PER_SECOND as i128;
    let remainder_fs = femto % FEMTOSECONDS_PER_SECOND as i128;
    let nanos = (remainder_fs / 1000) as i64;

    let base = apollo_epoch();

    if secs >= 0 {
        base.checked_add_signed(ChronoDuration::seconds(secs as i64))?
            .checked_add_signed(ChronoDuration::nanoseconds(nanos))
    } else {
        base.checked_sub_signed(ChronoDuration::seconds(-secs as i64))?
            .checked_sub_signed(ChronoDuration::nanoseconds(-nanos))
    }
}

fn format_apollo_time_ms(args: &HashMap<String, Value>) -> Value {
    let time_val = match args.get("time") {
        Some(Value::Int(s)) => match s.to_string().parse::<i128>() {
            Ok(v) => v,
            Err(_) => return Value::Error("ParseError", "Failed to parse 'time' as i128", None),
        },
        _ => return Value::Error("TypeError", "Expected Int for 'time' (Apollo femtoseconds)", None),
    };

    let ms = (time_val as f64) / 1_000_000_000_000_f64;

    Value::Float(Float::from(ms))
}

fn format_apollo_time(args: &HashMap<String, Value>) -> Value {
    let time_val = match args.get("time") {
        Some(Value::Int(s)) => match s.to_string().parse::<i128>() {
            Ok(v) => v,
            Err(_) => return Value::Error("ParseError", "Failed to parse 'time' as i128", None),
        },
        _ => return Value::Error("TypeError", "Expected Int for 'time' (i128 integer)", None),
    };

    let format_str = match args.get("format") {
        Some(Value::String(fmt)) => fmt.as_str(),
        _ => "%Y-%m-%d %H:%M:%S%.f",
    };

    match apollo_time_to_datetime(time_val) {
        Some(dt) => Value::String(dt.format(format_str).to_string()),
        None => Value::Error("InvalidTime", "Invalid timestamp for Apollo time", None),
    }
}

fn get_current_apollo_time(_args: &HashMap<String, Value>) -> Value {
    use once_cell::sync::Lazy;

    static START_INSTANT: Lazy<Instant> = Lazy::new(Instant::now);

    static APOLLO_EPOCH_SYSTEM_TIME: Lazy<SystemTime> = Lazy::new(|| {
        UNIX_EPOCH + std::time::Duration::from_secs(1418295400)
    });

    static APOLLO_EPOCH_OFFSET_FS: Lazy<i128> = Lazy::new(|| {
        let dur = APOLLO_EPOCH_SYSTEM_TIME
            .duration_since(UNIX_EPOCH)
            .expect("Apollo epoch should be after UNIX_EPOCH");
        (dur.as_secs() as i128) * FEMTOSECONDS_PER_SECOND
            + (dur.subsec_nanos() as i128) * 1_000_000
    });

    let elapsed = START_INSTANT.elapsed();
    let elapsed_fs = (elapsed.as_secs() as i128) * FEMTOSECONDS_PER_SECOND
        + (elapsed.subsec_nanos() as i128) * 1_000_000;

    let femto_timestamp = *APOLLO_EPOCH_OFFSET_FS + elapsed_fs;

    Value::Int(Int::from_str(&femto_timestamp.to_string()).unwrap())
}

fn get_unix_timestamp(_args: &HashMap<String, Value>) -> Value {
    let ts = Utc::now().timestamp();
    Value::Int(Int::from_i64(ts))
}

fn get_utc_now(_args: &HashMap<String, Value>) -> Value {
    Value::String(Utc::now().to_rfc3339())
}

fn utc_now_simple(_args: &HashMap<String, Value>) -> Value {
    Value::String(Utc::now().format("%Y-%m-%d %H:%M:%S").to_string())
}

fn get_local_now(_args: &HashMap<String, Value>) -> Value {
    Value::String(Local::now().to_rfc3339())
}

fn local_now_simple(_args: &HashMap<String, Value>) -> Value {
    Value::String(Local::now().format("%Y-%m-%d %H:%M:%S").to_string())
}

fn get_year(_args: &HashMap<String, Value>) -> Value {
    Value::Int(Int::from_i64(Local::now().year() as i64))
}

fn get_month(_args: &HashMap<String, Value>) -> Value {
    Value::Int(Int::from_i64(Local::now().month() as i64))
}

fn get_day(_args: &HashMap<String, Value>) -> Value {
    Value::Int(Int::from_i64(Local::now().day() as i64))
}

fn get_hour(_args: &HashMap<String, Value>) -> Value {
    Value::Int(Int::from_i64(Local::now().hour() as i64))
}

fn get_minute(_args: &HashMap<String, Value>) -> Value {
    Value::Int(Int::from_i64(Local::now().minute() as i64))
}

fn get_second(_args: &HashMap<String, Value>) -> Value {
    Value::Int(Int::from_i64(Local::now().second() as i64))
}

fn format_datetime(args: &HashMap<String, Value>) -> Value {
    match args.get("format") {
        Some(Value::String(fmt)) => {
            match Local::now().format(fmt).to_string() {
                s => Value::String(s),
            }
        }
        _ => Value::Error("TypeError", "expected str argument 'format'", None),
    }
}

fn sleep(args: &HashMap<String, Value>) -> Value {
    if let Some(duration_val) = args.get("duration") {
        match duration_val {
            Value::Int(int_val) => {
                match int_val.to_i64() {
                    Ok(duration_ms) => {
                        if duration_ms < 0 {
                            return Value::Error(
                                "InvalidArgument",
                                "duration must be non-negative",
                                None,
                            );
                        }
                        #[cfg(target_arch = "wasm32")]
                        {
                            sleep!(duration_ms);
                        }
                        #[cfg(not(target_arch = "wasm32"))]
                        {
                            let duration = Duration::from_millis(duration_ms as u64);
                            thread::sleep(duration);
                        }
                        Value::Null
                    }
                    Err(_) => Value::Error(
                        "ConversionError",
                        "failed to convert Int to i64",
                        None,
                    ),
                }
            }
            _ => Value::Error(
                "InvalidArgument",
                "duration must be an integer",
                None,
            ),
        }
    } else {
        Value::Error(
            "MissingArgument",
            "duration argument missing",
            None,
        )
    }
}

pub fn register() -> HashMap<String, Variable> {
    let mut map = HashMap::new();

    insert_native_fn!(map, "timestamp", get_unix_timestamp, vec![], "int");
    insert_native_fn!(map, "utc_now", get_utc_now, vec![], "str");
    insert_native_fn!(map, "local_now", get_local_now, vec![], "str");
    insert_native_fn!(map, "utc_now_simple", utc_now_simple, vec![], "str");
    insert_native_fn!(map, "local_now_simple", local_now_simple, vec![], "str");

    insert_native_fn!(map, "year", get_year, vec![], "int");
    insert_native_fn!(map, "month", get_month, vec![], "int");
    insert_native_fn!(map, "day", get_day, vec![], "int");

    insert_native_fn!(map, "hour", get_hour, vec![], "int");
    insert_native_fn!(map, "minute", get_minute, vec![], "int");
    insert_native_fn!(map, "second", get_second, vec![], "int");

    insert_native_fn!(
        map,
        "format",
        format_datetime,
        vec![Parameter::positional("format", "str")],
        "str"
    );

    insert_native_fn!(
        map,
        "parse",
        parse_time,
        vec![
            Parameter::positional("time", "str"),
            Parameter::positional("format", "str")
        ],
        "map"
    );

    insert_native_fn!(
        map,
        "sleep",
        sleep,
        vec![Parameter::positional("duration", "int")],
        "void"
    );

    insert_native_fn!(
        map,
        "format_apollo_time",
        format_apollo_time,
        vec![
            Parameter::positional("time", "int"),
            Parameter::positional_optional("format", "str", "%Y-%m-%d %H:%M:%S%.f".into()),
        ],
        "str"
    );

    insert_native_fn!(
        map,
        "current_apollo_time",
        get_current_apollo_time,
        vec![],
        "int"
    );

    insert_native_fn!(
        map,
        "format_apollo_time_ms",
        format_apollo_time_ms,
        vec![Parameter::positional("time", "int")],
        "float"
    );

    map
}
