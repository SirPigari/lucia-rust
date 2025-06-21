use std::collections::HashMap;
use std::sync::Arc;
use crate::env::runtime::functions::{Function, NativeFunction, Parameter};
use crate::env::runtime::types::Int;
use crate::env::runtime::value::Value;
use crate::env::runtime::variables::Variable;
use crate::{insert_native_fn};
use chrono::{Utc, Local, Timelike, Datelike, NaiveDateTime, ParseError};

// This module handles time and date functionality.
// It includes functions to get the current time, format dates, parse date strings, and retrieve specific time components.
// Lucia version 2.0.0, module: time@0.3.0

fn parse_time(args: &HashMap<String, Value>) -> Value {
    let format = match args.get("format") {
        Some(Value::String(s)) => s.as_str(),
        _ => return Value::Error("TypeError", "Expected a string for 'format'"),
    };

    let time_str = match args.get("time") {
        Some(Value::String(s)) => s.as_str(),
        _ => return Value::Error("TypeError", "Expected a string for 'time'"),
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
        Err(e) => Value::Error("ParseError", Box::leak(Box::new(format!("{}", e)))),
    }
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
        _ => Value::Error("TypeError", "expected str argument 'format'"),
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

    map
}

pub fn init() -> Value {
    Value::Null
}
