use std::collections::HashMap;
use std::sync::Arc;
use crate::env::runtime::functions::{Function, Parameter};
use crate::env::runtime::types::{Int, Type};
use crate::env::runtime::value::Value;
use crate::env::runtime::variables::Variable;
use crate::env::runtime::statements::Statement;
use crate::env::runtime::utils::{to_static, parse_type};
use crate::env::runtime::internal_structs::EffectFlags;
use crate::env::runtime::structs_and_enums::Struct;
use std::time::{Duration, SystemTime};
#[cfg(not(target_arch = "wasm32"))]
use std::thread;
use once_cell::sync::Lazy;
use crate::{insert_native_fn, make_native_fn_pt, make_native_static_fn_pt, insert_native_var};
use chrono::{Utc, Local, Timelike, Datelike, NaiveDateTime, DateTime, FixedOffset, TimeZone, LocalResult};

static APOLLO_EPOCH_DATETIME: Lazy<DateTime<Utc>> = Lazy::new(|| {
    "1969-07-19T21:44:00Z"
        .parse::<DateTime<Utc>>()
        .expect("invalid date")
});

static APOLLO_EPOCH: Lazy<SystemTime> = Lazy::new(|| {
    let dt: DateTime<Utc> = *APOLLO_EPOCH_DATETIME;
    let ts = dt.timestamp();
    let nanos = dt.timestamp_subsec_nanos();

    if ts >= 0 {
        SystemTime::UNIX_EPOCH + Duration::new(ts as u64, nanos)
    } else {
        SystemTime::UNIX_EPOCH - Duration::new((-ts) as u64, nanos)
    }
});

// This module handles time and date functionality.
// It includes functions to get the current time, format dates, parse date strings, and retrieve specific time components.
// Lucia version 2.0.0, module: time@0.3.0

// NOTE: this is a mess
fn now_apollo_time(_args: &HashMap<String, Value>) -> Value {
    let now = SystemTime::now();
    let nanos = nanos_since_apollo(now);

    let mut map: HashMap<String, (Box<Value>, Type)> = HashMap::new();
    map.insert("nanos".to_string(), (Box::new(Value::Int(Int::from_i128(
        nanos
    ))), Type::new_simple("int")));
    map.insert("tz".to_string(), (Box::new(Value::String(Local::now().offset().to_string())), Type::new_simple("str")));

    Value::Struct(Struct {
        ty: APOLLO_TIME_STRUCT.clone(),
        fields: map,
    })
}

fn new_apollo_time(args: &HashMap<String, Value>) -> Value {
    let nanos = match args.get("nanos") {
        Some(Value::Int(i)) => match i.to_i128() {
            Ok(v) => v,
            Err(_) => return Value::Error("ConversionError", "failed to convert Int to i128", None),
        },
        _ => return Value::Error("TypeError", "expected int argument 'nanos'", None),
    };
    let tz = match args.get("tz") {
        Some(Value::String(s)) => {
            if check_offset(s) {
                s.clone()
            } else {
                return Value::Error("ValueError", "invalid timezone offset format (expected +HH:MM or -HH:MM)", None);
            }
        }
        _ => Local::now().offset().to_string(),
    };

    let mut map: HashMap<String, (Box<Value>, Type)> = HashMap::new();
    map.insert("nanos".to_string(), (Box::new(Value::Int(Int::from_i128(
        nanos
    ))), Type::new_simple("int")));
    map.insert("tz".to_string(), (Box::new(Value::String(tz)), Type::new_simple("str")));

    Value::Struct(Struct {
        ty: APOLLO_TIME_STRUCT.clone(),
        fields: map,
    })
}

fn nanos_since_apollo(time: SystemTime) -> i128 {
    match time.duration_since(*APOLLO_EPOCH) {
        Ok(duration) => duration.as_nanos() as i128,
        Err(e) => -(e.duration().as_nanos() as i128),
    }
}

fn check_offset(tz_str: &str) -> bool {
    if tz_str == "Z" {
        return true;
    }
    // expected format: +HH:MM or -HH:MM
    if tz_str.len() == 6 && (tz_str.starts_with('+') || tz_str.starts_with('-')) {
        if tz_str[3..4] == *":" {
            if let (Ok(hours), Ok(minutes)) = (tz_str[1..3].parse::<i32>(), tz_str[4..6].parse::<i32>()) {
                if hours >= 0 && hours <= 23 && minutes >= 0 && minutes <= 59 {
                    return true;
                }
            }
        }
    }
    false
}

fn parse_offset(tz_str: &str) -> FixedOffset {
    if tz_str == "Z" {
        return FixedOffset::east_opt(0).unwrap();
    }
    // expected format: +HH:MM or -HH:MM
    if tz_str.len() == 6 && (tz_str.starts_with('+') || tz_str.starts_with('-')) {
        let sign = if &tz_str[0..1] == "+" { 1 } else { -1 };
        if let (Ok(hours), Ok(minutes)) = (tz_str[1..3].parse::<i32>(), tz_str[4..6].parse::<i32>()) {
            return FixedOffset::east_opt(sign * (hours * 3600 + minutes * 60)).unwrap_or(FixedOffset::east_opt(0).unwrap());
        }
    }
    FixedOffset::east_opt(0).unwrap()
}

fn validate_chrono_format(fmt: &str) -> Result<(), &'static str> {
    let allowed = ['Y','y','m','d','H','M','S','f',':','z','Z','a','A','b','B','c','D','e','j','k','l','p','P','r','R','s','T','u','U','w','W','x','X'];
    let mut chars = fmt.chars().peekable();
    while let Some(c) = chars.next() {
        if c == '%' {
            match chars.next() {
                Some(next) if allowed.contains(&next) => continue,
                Some(cn) => return Err(to_static(format!("Invalid format specifier (%{})", cn))),
                None => return Err("Stray '%' at end of format string"),
            }
        }
    }
    Ok(())
}

macro_rules! make_comparison_fn {
    ($name:expr, $cmp:expr, $ty:expr) => {
        ($name.to_string(),
            make_native_fn_pt!(
                $name,
                |args: &HashMap<String, Value>| -> Value {
                    let self_struct = match args.get("self") {
                        Some(Value::Struct(s)) => s,
                        _ => return Value::Error("TypeError", "Expected self to be ApolloTime struct", None),
                    };
                    let other_struct = match args.get("other") {
                        Some(Value::Struct(s)) => s,
                        _ => return Value::Error("TypeError", "Expected other to be ApolloTime struct", None),
                    };

                    let self_nanos = match self_struct.fields.get("nanos") {
                        Some((v, _)) => match &**v { Value::Int(n) => n.to_i128().unwrap_or(0), _ => return Value::Error("ValueError", "self.nanos is not Int", None) },
                        None => return Value::Error("ValueError", "Missing nanos field in self", None),
                    };
                    let other_nanos = match other_struct.fields.get("nanos") {
                        Some((v, _)) => match &**v { Value::Int(n) => n.to_i128().unwrap_or(0), _ => return Value::Error("ValueError", "other.nanos is not Int", None) },
                        None => return Value::Error("ValueError", "Missing nanos field in other", None),
                    };
                    
                    let self_tz_str = self_struct.fields.get("tz")
                        .and_then(|(v, _)| if let Value::String(s) = &**v { Some(s.clone()) } else { None })
                        .unwrap_or_else(|| "Z".to_string());
                    let other_tz_str = other_struct.fields.get("tz")
                        .and_then(|(v, _)| if let Value::String(s) = &**v { Some(s.clone()) } else { None })
                        .unwrap_or_else(|| "Z".to_string());

                    let self_offset = parse_offset(&self_tz_str);
                    let other_offset = parse_offset(&other_tz_str);

                    let other_nanos_converted = if self_tz_str == other_tz_str {
                        other_nanos
                    } else {
                        let tz_diff_nanos = (other_offset.local_minus_utc() - self_offset.local_minus_utc()) as i128 * 1_000_000_000;
                        other_nanos - tz_diff_nanos
                    };

                    Value::Boolean($cmp(self_nanos, other_nanos_converted))
                },
                vec![
                    Parameter::instance("self", $ty, vec![]),
                    Parameter::positional_pt("other", $ty),
                ],
                &Type::new_simple("bool"),
                EffectFlags::PURE
            )
        )
    };
}

fn parse_iso_to_nanos(iso: &str) -> Result<(i128, String), String> {
    let dt_fixed: DateTime<FixedOffset> = DateTime::parse_from_rfc3339(iso)
        .map_err(|e| format!("Failed to parse ISO datetime: {}", e))?;

    let dt_utc: DateTime<Utc> = dt_fixed.with_timezone(&Utc);
    let epoch_utc: DateTime<Utc> = (*APOLLO_EPOCH).into();
    let duration = dt_utc.signed_duration_since(epoch_utc);
    let nanos = duration.num_nanoseconds().ok_or("Duration overflowed")?;

    let tz_str = if dt_fixed.offset().local_minus_utc() == 0 {
        "Z".to_string()
    } else {
        dt_fixed.format("%:z").to_string()
    };

    Ok((nanos as i128, tz_str))
}


fn apollo_nanos_to_system_time(nanos: i128) -> SystemTime {
    if nanos >= 0 {
        *APOLLO_EPOCH + Duration::from_nanos(nanos as u64)
    } else {
        *APOLLO_EPOCH - Duration::from_nanos((-nanos) as u64)
    }
}

static APOLLO_TIME_STRUCT: Lazy<Type> = Lazy::new(|| {
    let mut ty = Type::Struct {
        name: "ApolloTime".to_string(),
        fields: vec![
            ("nanos".to_string(), Statement::make_value(Value::Type(Type::new_simple("int"))), vec![]),
            ("tz".to_string(), Statement::make_value(Value::Type(Type::new_simple("str"))), vec![]),
        ],
        methods: vec![],
        generics: Vec::new(),
        wheres: Vec::new(),
    };

    let mut new_methods = Vec::new();

    let new_method = ("new".to_string(),
        make_native_static_fn_pt!("new", new_apollo_time, vec![Parameter::positional("nanos", "int"), Parameter::positional_optional("tz", "str", Value::Null)], &ty, EffectFlags::IO)
    );
    let now_method = ("now".to_string(),
        make_native_static_fn_pt!("now", now_apollo_time, vec![], &ty, EffectFlags::IO)
    );

    new_methods.push(new_method);
    new_methods.push(now_method);

    let display_fn = make_native_fn_pt!(
        "display",
        |args: &HashMap<String, Value>| -> Value {
            if let Some(Value::Struct(s)) = args.get("self") {
                if let Some((nanos_val, _)) = s.fields.get("nanos") {
                    if let Value::Int(nanos_int) = &**nanos_val {
                        if let Ok(nanos_i128) = nanos_int.to_i128() {
                            let tz_str = s.fields.get("tz")
                                .and_then(|(v, _)| if let Value::String(s) = &**v { Some(s.clone()) } else { None })
                                .unwrap_or_else(|| "Z".to_string());

                            let offset = parse_offset(&tz_str);

                            let apollo_epoch = *APOLLO_EPOCH;
                            let duration = if nanos_i128 >= 0 {
                                std::time::Duration::from_nanos(nanos_i128 as u64)
                            } else {
                                std::time::Duration::from_nanos((-nanos_i128) as u64)
                            };
                            let ts = if nanos_i128 >= 0 {
                                apollo_epoch.checked_add(duration)
                            } else {
                                apollo_epoch.checked_sub(duration)
                            };

                            if let Some(system_time) = ts {
                                let dt_utc: DateTime<Utc> = system_time.into();
                                let dt_local = dt_utc.with_timezone(&offset);
                                let iso_str = dt_local.format("%Y-%m-%dT%H:%M:%S%.f").to_string();
                                return Value::String(format!("ApolloTime({}; {})", iso_str, tz_str));
                            }
                        }
                    }
                }
            }
            Value::String("ApolloTime(invalid)".to_string())
        },
        vec![Parameter::instance("self", &ty, vec![])],
        &Type::new_simple("str"),
        EffectFlags::PURE
    );

    let op_display_method = ("op_display".to_string(),
        display_fn.clone()
    );

    let display_method = ("display".to_string(),
        display_fn
    );

    new_methods.push(op_display_method);
    new_methods.push(display_method);

    let format_method = ("format".to_string(),
        make_native_fn_pt!(
            "format",
            |args: &HashMap<String, Value>| -> Value {
                let s = match args.get("self") {
                    Some(Value::Struct(s)) => s,
                    _ => return Value::Error("TypeError", "Expected self to be ApolloTime struct", None),
                };

                let fmt_str = match args.get("format") {
                    Some(Value::String(s)) => s,
                    _ => return Value::Error("TypeError", "Expected format to be a string", None),
                };

                let nanos_i128 = match s.fields.get("nanos") {
                    Some((boxed_val, _)) => match &**boxed_val {
                        Value::Int(n) => match n.to_i128() {
                            Ok(v) => v,
                            Err(e) => return Value::Error("ValueError", to_static(format!("Failed to convert nanos: {}", e)), None),
                        },
                        _ => return Value::Error("ValueError", "nanos field is not an Int", None),
                    },
                    None => return Value::Error("ValueError", "Missing nanos field", None),
                };

                let tz_str = s.fields.get("tz")
                    .and_then(|(v, _)| if let Value::String(s) = &**v { Some(s.clone()) } else { None })
                    .unwrap_or_else(|| "Z".to_string());

                let offset = if tz_str == "Z" { FixedOffset::east_opt(0).unwrap() } else { parse_offset(&tz_str) };

                let apollo_epoch = *APOLLO_EPOCH;
                let duration = if nanos_i128 >= 0 {
                    std::time::Duration::from_nanos(nanos_i128 as u64)
                } else {
                    std::time::Duration::from_nanos((-nanos_i128) as u64)
                };

                let ts = if nanos_i128 >= 0 {
                    apollo_epoch.checked_add(duration)
                } else {
                    apollo_epoch.checked_sub(duration)
                };

                let system_time = match ts {
                    Some(t) => t,
                    None => return Value::Error("ValueError", "Timestamp overflowed", None),
                };

                let dt_utc: DateTime<Utc> = system_time.into();
                let dt_local = dt_utc.with_timezone(&offset);

                if let Err(e) = validate_chrono_format(&fmt_str) {
                    return Value::Error("ValueError", e, None);
                }
                let formatted = dt_local.format(&fmt_str).to_string();
                Value::String(formatted)
            },
            vec![
                Parameter::instance("self", &ty, vec![]),
                Parameter::positional("format", "str"),
            ],
            &Type::new_simple("str"),
            EffectFlags::PURE
        )
    );

    new_methods.push(format_method);

    let op_add = ("op_add".to_string(),
        make_native_fn_pt!(
            "op_add",
            |args: &HashMap<String, Value>| -> Value {
                let self_struct = match args.get("self") {
                    Some(Value::Struct(s)) => s,
                    _ => return Value::Error("TypeError", "Expected self to be ApolloTime struct", None),
                };

                let other_struct = match args.get("other") {
                    Some(Value::Struct(s)) => s,
                    _ => return Value::Error("TypeError", "Expected other to be ApolloTime struct", None),
                };

                let self_nanos = match self_struct.fields.get("nanos") {
                    Some((v, _)) => match &**v {
                        Value::Int(n) => n.to_i128().unwrap_or(0),
                        _ => return Value::Error("ValueError", "self.nanos is not an Int", None),
                    },
                    None => return Value::Error("ValueError", "Missing nanos field in self", None),
                };
                let other_nanos = match other_struct.fields.get("nanos") {
                    Some((v, _)) => match &**v {
                        Value::Int(n) => n.to_i128().unwrap_or(0),
                        _ => return Value::Error("ValueError", "other.nanos is not an Int", None),
                    },
                    None => return Value::Error("ValueError", "Missing nanos field in other", None),
                };

                let self_tz_str = self_struct.fields.get("tz")
                    .and_then(|(v, _)| if let Value::String(s) = &**v { Some(s.clone()) } else { None })
                    .unwrap_or_else(|| "Z".to_string());
                let other_tz_str = other_struct.fields.get("tz")
                    .and_then(|(v, _)| if let Value::String(s) = &**v { Some(s.clone()) } else { None })
                    .unwrap_or_else(|| "Z".to_string());

                let self_offset = parse_offset(&self_tz_str);
                let other_offset = parse_offset(&other_tz_str);

                let result_nanos = if self_tz_str == other_tz_str {
                    self_nanos.checked_add(other_nanos)
                } else {
                    let tz_diff_nanos = (other_offset.local_minus_utc() - self_offset.local_minus_utc()) as i128 * 1_000_000_000;
                    self_nanos.checked_add(other_nanos - tz_diff_nanos)
                };

                let new_nanos = match result_nanos {
                    Some(n) => n,
                    None => return Value::Error("OverflowError", "addition overflowed", None),
                };

                let mut new_fields = self_struct.fields.clone();
                new_fields.insert("nanos".to_string(), (Box::new(Value::Int(Int::from_i128(new_nanos))), Type::new_simple("int")));

                Value::Struct(Struct {
                    ty: APOLLO_TIME_STRUCT.clone(),
                    fields: new_fields,
                })
            },
            vec![
                Parameter::instance("self", &ty, vec![]),
                Parameter::positional_pt("other", &ty),
            ],
            &ty,
            EffectFlags::PURE
        )
    );

    let op_sub = ("op_sub".to_string(),
        make_native_fn_pt!(
            "op_sub",
            |args: &HashMap<String, Value>| -> Value {
                let self_struct = match args.get("self") {
                    Some(Value::Struct(s)) => s,
                    _ => return Value::Error("TypeError", "Expected self to be ApolloTime struct", None),
                };

                let other_struct = match args.get("other") {
                    Some(Value::Struct(s)) => s,
                    _ => return Value::Error("TypeError", "Expected other to be ApolloTime struct", None),
                };

                let self_nanos = match self_struct.fields.get("nanos") {
                    Some((v, _)) => match &**v {
                        Value::Int(n) => n.to_i128().unwrap_or(0),
                        _ => return Value::Error("ValueError", "self.nanos is not an Int", None),
                    },
                    None => return Value::Error("ValueError", "Missing nanos field in self", None),
                };
                let other_nanos = match other_struct.fields.get("nanos") {
                    Some((v, _)) => match &**v {
                        Value::Int(n) => n.to_i128().unwrap_or(0),
                        _ => return Value::Error("ValueError", "other.nanos is not an Int", None),
                    },
                    None => return Value::Error("ValueError", "Missing nanos field in other", None),
                };

                let self_tz_str = self_struct.fields.get("tz")
                    .and_then(|(v, _)| if let Value::String(s) = &**v { Some(s.clone()) } else { None })
                    .unwrap_or_else(|| "Z".to_string());
                let other_tz_str = other_struct.fields.get("tz")
                    .and_then(|(v, _)| if let Value::String(s) = &**v { Some(s.clone()) } else { None })
                    .unwrap_or_else(|| "Z".to_string());

                let self_offset = parse_offset(&self_tz_str);
                let other_offset = parse_offset(&other_tz_str);

                let result_nanos = if self_tz_str == other_tz_str {
                    self_nanos.checked_sub(other_nanos)
                } else {
                    let tz_diff_nanos = (other_offset.local_minus_utc() - self_offset.local_minus_utc()) as i128 * 1_000_000_000;
                    self_nanos.checked_sub(other_nanos - tz_diff_nanos)
                };

                let new_nanos = match result_nanos {
                    Some(n) => n,
                    None => return Value::Error("OverflowError", "subtraction overflowed", None),
                };

                let mut new_fields = self_struct.fields.clone();
                new_fields.insert("nanos".to_string(), (Box::new(Value::Int(Int::from_i128(new_nanos))), Type::new_simple("int")));

                Value::Struct(Struct {
                    ty: APOLLO_TIME_STRUCT.clone(),
                    fields: new_fields,
                })
            },
            vec![
                Parameter::instance("self", &ty, vec![]),
                Parameter::positional_pt("other", &ty),
            ],
            &ty,
            EffectFlags::PURE
        )
    );

    new_methods.push(op_add);
    new_methods.push(op_sub);

    let from_iso_method = ("from_iso".to_string(),
        make_native_static_fn_pt!(
            "from_iso",
            |args: &HashMap<String, Value>| -> Value {
                let iso_str = match args.get("s") {
                    Some(Value::String(s)) => s,
                    _ => return Value::Error("TypeError", "Expected string argument", None),
                };
                match parse_iso_to_nanos(&iso_str) {
                    Ok((nanos, tz_str)) => {
                        let mut fields = HashMap::new();
                        fields.insert("nanos".to_string(), (Box::new(Value::Int(Int::from_i128(nanos))), Type::new_simple("int")));
                        fields.insert("tz".to_string(), (Box::new(Value::String(tz_str)), Type::new_simple("str")));
                        Value::Struct(Struct { ty: APOLLO_TIME_STRUCT.clone(), fields })
                    }
                    Err(e) => Value::Error("ValueError", to_static(e), None),
                }
            },
            vec![Parameter::positional("s", "str")],
            &ty,
            EffectFlags::PURE
        )
    );
    let to_iso_method = ("to_iso".to_string(),
        make_native_fn_pt!(
            "to_iso",
            |args: &HashMap<String, Value>| -> Value {
                let s = match args.get("self") {
                    Some(Value::Struct(s)) => s,
                    _ => return Value::Error("TypeError", "Expected ApolloTime struct", None),
                };
                let nanos_i128 = match s.fields.get("nanos") {
                    Some((v, _)) => match &**v { 
                        Value::Int(n) => n.to_i128().unwrap_or(0), 
                        _ => return Value::Error("ValueError", "nanos not Int", None) 
                    },
                    None => return Value::Error("ValueError", "Missing nanos", None),
                };
                let tz_str = s.fields.get("tz")
                    .and_then(|(v, _)| if let Value::String(s) = &**v { Some(s.clone()) } else { None })
                    .unwrap_or_else(|| "Z".to_string());
                let offset = parse_offset(&tz_str);

                let ts = apollo_nanos_to_system_time(nanos_i128);
                let dt_utc: DateTime<Utc> = ts.into();
                let dt_local = dt_utc.with_timezone(&offset);

                Value::String(dt_local.format("%Y-%m-%dT%H:%M:%S%.f%:z").to_string())
            },
            vec![Parameter::instance("self", &ty, vec![])],
            &Type::new_simple("str"),
            EffectFlags::PURE
        )
    );

    new_methods.push(from_iso_method);
    new_methods.push(to_iso_method);

    let from_unix_method = ("from_unix".to_string(),
        make_native_static_fn_pt!(
            "from_unix",
            |args: &HashMap<String, Value>| -> Value {
                let ts_secs = match args.get("ts") {
                    Some(Value::Int(i)) => match i.to_i128() {
                        Ok(v) => v,
                        Err(_) => return Value::Error("ConversionError", "failed to convert Int to i128", None),
                    },
                    _ => return Value::Error("TypeError", "expected int argument 'ts'", None),
                };

                let unix_time = std::time::UNIX_EPOCH
                    .checked_add(std::time::Duration::from_secs(ts_secs as u64))
                    .unwrap_or(std::time::UNIX_EPOCH);

                let apollo_offset = match unix_time.duration_since(*APOLLO_EPOCH) {
                    Ok(dur) => dur.as_nanos() as i128,
                    Err(e) => -(e.duration().as_nanos() as i128),
                };

                let mut map: HashMap<String, (Box<Value>, Type)> = HashMap::new();
                map.insert("nanos".to_string(), (
                    Box::new(Value::Int(Int::from_i128(apollo_offset))),
                    Type::new_simple("int")
                ));
                map.insert("tz".to_string(), (
                    Box::new(Value::String(Local::now().offset().to_string())),
                    Type::new_simple("str")
                ));

                Value::Struct(Struct {
                    ty: APOLLO_TIME_STRUCT.clone(),
                    fields: map,
                })
            },
            vec![Parameter::positional("ts", "int")],
            &ty,
            EffectFlags::PURE
        )
    );

    let to_unix_method = ("to_unix".to_string(),
        make_native_fn_pt!(
            "to_unix",
            |args: &HashMap<String, Value>| -> Value {
                let s = match args.get("self") {
                    Some(Value::Struct(s)) => s,
                    _ => return Value::Error("TypeError", "Expected ApolloTime", None),
                };

                let nanos_i128 = match s.fields.get("nanos") {
                    Some((v, _)) => match &**v {
                        Value::Int(n) => n.to_i128().unwrap_or(0),
                        _ => return Value::Error("ValueError", "nanos not Int", None),
                    },
                    None => return Value::Error("ValueError", "Missing nanos", None),
                };

                let apollo_time = if nanos_i128 >= 0 {
                    (*APOLLO_EPOCH)
                        .checked_add(std::time::Duration::from_nanos(nanos_i128 as u64))
                        .unwrap_or(*APOLLO_EPOCH)
                } else {
                    (*APOLLO_EPOCH)
                        .checked_sub(std::time::Duration::from_nanos((-nanos_i128) as u64))
                        .unwrap_or(*APOLLO_EPOCH)
                };

                let unix_ts = match apollo_time.duration_since(std::time::UNIX_EPOCH) {
                    Ok(dur) => dur.as_secs() as i128,
                    Err(e) => -(e.duration().as_secs() as i128),
                };

                Value::Int(Int::from_i128(unix_ts))
            },
            vec![Parameter::instance("self", &ty, vec![])],
            &Type::new_simple("int"),
            EffectFlags::PURE
        )
    );

    new_methods.push(from_unix_method);
    new_methods.push(to_unix_method);

    let epoch_method = ("epoch".to_string(),
        make_native_static_fn_pt!(
            "epoch",
            |_args: &HashMap<String, Value>| -> Value {
                let mut map: HashMap<String, (Box<Value>, Type)> = HashMap::new();
                map.insert("nanos".to_string(), (Box::new(Value::Int(Int::from_i128(0))), Type::new_simple("int")));
                map.insert("tz".to_string(), (Box::new(Value::String("Z".to_string())), Type::new_simple("str")));

                Value::Struct(Struct {
                    ty: APOLLO_TIME_STRUCT.clone(),
                    fields: map,
                })
            },
            vec![],
            &ty,
            EffectFlags::PURE
        )
    );

    new_methods.push(epoch_method);

    let to_timezone_method = ("to_timezone".to_string(),
        make_native_fn_pt!(
            "to_timezone",
            |args: &HashMap<String, Value>| -> Value {
                let self_struct = match args.get("self") { Some(Value::Struct(s)) => s, _ => return Value::Error("TypeError", "Expected ApolloTime struct", None) };
                let new_tz_str = match args.get("tz") { Some(Value::String(s)) => s, _ => return Value::Error("TypeError", "Expected tz string", None) };

                let nanos_i128 = match self_struct.fields.get("nanos") {
                    Some((v, _)) => match &**v { Value::Int(n) => n.to_i128().unwrap_or(0), _ => return Value::Error("ValueError", "nanos not Int", None) },
                    None => return Value::Error("ValueError", "Missing nanos", None),
                };
                let old_tz_str = self_struct.fields.get("tz").and_then(|(v, _)| if let Value::String(s) = &**v { Some(s.clone()) } else { None }).unwrap_or_else(|| "Z".to_string());

                let old_offset = parse_offset(&old_tz_str);
                let new_offset = parse_offset(&new_tz_str);

                let nanos_utc = nanos_i128 - old_offset.local_minus_utc() as i128 * 1_000_000_000;
                let new_nanos = nanos_utc + new_offset.local_minus_utc() as i128 * 1_000_000_000;

                let mut fields = self_struct.fields.clone();
                fields.insert("nanos".to_string(), (Box::new(Value::Int(Int::from_i128(new_nanos))), Type::new_simple("int")));
                fields.insert("tz".to_string(), (Box::new(Value::String(new_tz_str.to_string())), Type::new_simple("str")));

                Value::Struct(Struct { ty: APOLLO_TIME_STRUCT.clone(), fields })
            },
            vec![Parameter::instance("self", &ty, vec![]), Parameter::positional("tz", "str")],
            &ty,
            EffectFlags::PURE
        )
    );
    new_methods.push(to_timezone_method);

    let from_method = ("from".to_string(),
        make_native_static_fn_pt!(
            "from",
            |args: &HashMap<String, Value>| -> Value {
                let datetime_str = match args.get("datetime") {
                    Some(Value::String(s)) => s,
                    _ => return Value::Error("TypeError", "Expected string argument 'datetime'", None),
                };
                let format_str = match args.get("format") {
                    Some(Value::String(s)) => s.as_str(),
                    Some(Value::Null) | None => "%Y-%m-%dT%H:%M:%S%.f",
                    _ => return Value::Error("TypeError", "Expected string or null for 'format'", None),
                };
                let tz_str = match args.get("tz") {
                    Some(Value::String(s)) => s.as_str(),
                    _ => return Value::Error("TypeError", "Expected string or null for 'tz'", None),
                };

                let offset = parse_offset(tz_str);
                match NaiveDateTime::parse_from_str(datetime_str, format_str) {
                    Ok(naive_dt) => {
                        let dt_fixed = match offset.from_local_datetime(&naive_dt) {
                            LocalResult::Single(dt) => dt,
                            LocalResult::Ambiguous(..) => return Value::Error("ValueError", "The specified local time is ambiguous in the given timezone", None),
                            LocalResult::None => return Value::Error("ValueError", "The specified local time does not exist in the given timezone", None),
                        };

                        let dt_utc: DateTime<Utc> = dt_fixed.with_timezone(&Utc);
                        let epoch_utc: DateTime<Utc> = (*APOLLO_EPOCH_DATETIME).into();
                        let duration = dt_utc.signed_duration_since(epoch_utc);
                        let nanos = match duration.num_nanoseconds() {
                            Some(n) => n as i128,
                            None => return Value::Error("OverflowError", "Duration overflowed", None),
                        };

                        let mut fields: HashMap<String, (Box<Value>, Type)> = HashMap::new();
                        fields.insert("nanos".to_string(), (Box::new(Value::Int(Int::from_i128(nanos))), Type::new_simple("int")));
                        fields.insert("tz".to_string(), (Box::new(Value::String(tz_str.to_string())), Type::new_simple("str")));

                        Value::Struct(Struct { ty: APOLLO_TIME_STRUCT.clone(), fields })
                    }
                    Err(e) => Value::Error("ParseError", to_static(format!("Failed to parse datetime: {}", e)), None),
                }
            },
            vec![
                Parameter::positional("datetime", "str"),
                Parameter::positional_optional_pt("format", &parse_type("?str"), Value::Null),
                Parameter::positional_optional("tz", "str", Value::String("Z".to_string()))
            ],
            &ty,
            EffectFlags::PURE
        )
    );

    new_methods.push(from_method);

    let get_nanos = ("get_nanos".to_string(), make_native_fn_pt!(
        "get_nanos",
        |args: &HashMap<String, Value>| {
            let s = match args.get("self") {
                Some(Value::Struct(s)) => s,
                _ => return Value::Error("TypeError", "Expected ApolloTime", None),
            };
            match s.fields.get("nanos") {
                Some((v, _)) => (**v).clone(),
                None => Value::Int(Int::from_i128(0)),
            }
        },
        vec![Parameter::instance("self", &ty, vec![])],
        &Type::new_simple("int"),
        EffectFlags::PURE
    ));

    let get_timezone_str = ("get_timezone_str".to_string(), make_native_fn_pt!(
        "get_timezone_str",
        |args: &HashMap<String, Value>| {
            let s = match args.get("self") {
                Some(Value::Struct(s)) => s,
                _ => return Value::Error("TypeError", "Expected ApolloTime", None),
            };
            match s.fields.get("tz") {
                Some((v, _)) => (**v).clone(),
                None => Value::String("Z".to_string()),
            }
        },
        vec![Parameter::instance("self", &ty, vec![])],
        &Type::new_simple("str"),
        EffectFlags::PURE
    ));

    let get_timezone = ("get_timezone".to_string(), make_native_fn_pt!(
        "get_timezone",
        |args: &HashMap<String, Value>| {
            let s = match args.get("self") {
                Some(Value::Struct(s)) => s,
                _ => return Value::Error("TypeError", "Expected ApolloTime", None),
            };
            let tz_str = s.fields.get("tz")
                .and_then(|(v, _)| if let Value::String(s) = &**v { Some(s.clone()) } else { None })
                .unwrap_or_else(|| "Z".to_string());
            Value::Int(Int::from_i128(parse_offset(&tz_str).local_minus_utc() as i128))
        },
        vec![Parameter::instance("self", &ty, vec![])],
        &Type::new_simple("int"),
        EffectFlags::PURE
    ));

    new_methods.push(get_nanos);
    new_methods.push(get_timezone_str);
    new_methods.push(get_timezone);

    let elapsed_method = ("elapsed".to_string(),
        make_native_fn_pt!(
            "elapsed",
            |args: &HashMap<String, Value>| -> Value {
                let self_struct = match args.get("self") {
                    Some(Value::Struct(s)) => s,
                    _ => return Value::Error("TypeError", "Expected ApolloTime struct", None),
                };

                let self_nanos = match self_struct.fields.get("nanos") {
                    Some((v, _)) => match &**v {
                        Value::Int(n) => match n.to_i128() {
                            Ok(val) => val,
                            Err(_) => return Value::Error("ValueError", "self.nanos too large to convert", None),
                        },
                        _ => return Value::Error("ValueError", "self.nanos is not an Int", None),
                    },
                    None => return Value::Error("ValueError", "Missing nanos field in self", None),
                };

                let self_tz = match self_struct.fields.get("tz") {
                    Some((v, _)) => match &**v {
                        Value::String(s) => s.clone(),
                        _ => "Z".to_string(),
                    },
                    None => "Z".to_string(),
                };

                let now_nanos = {
                    let now = Utc::now();
                    let epoch_utc: DateTime<Utc> = (*APOLLO_EPOCH_DATETIME).into();
                    let duration = now.signed_duration_since(epoch_utc);
                    match duration.num_nanoseconds() {
                        Some(n) => n as i128,
                        None => return Value::Error("OverflowError", "Current time too far from Apollo epoch", None),
                    }
                };

                let elapsed_nanos = match now_nanos.checked_sub(self_nanos) {
                    Some(n) => n,
                    None => return Value::Error("OverflowError", "Elapsed time calculation overflowed", None),
                };

                let mut fields = HashMap::new();
                fields.insert(
                    "nanos".to_string(),
                    (Box::new(Value::Int(Int::from_i128(elapsed_nanos))), Type::new_simple("int"))
                );

                fields.insert(
                    "tz".to_string(),
                    (Box::new(Value::String(self_tz)), Type::new_simple("str"))
                );

                Value::Struct(Struct {
                    ty: APOLLO_TIME_STRUCT.clone(),
                    fields
                })
            },
            vec![Parameter::instance("self", &ty, vec![])],
            &ty,
            EffectFlags::PURE
        )
    );

    new_methods.push(elapsed_method);

    let op_gt = make_comparison_fn!("op_gt", |self_nanos, other_nanos| self_nanos > other_nanos, &ty);
    let op_lt = make_comparison_fn!("op_lt", |self_nanos, other_nanos| self_nanos < other_nanos, &ty);
    let op_ge = make_comparison_fn!("op_ge", |self_nanos, other_nanos| self_nanos >= other_nanos, &ty);
    let op_le = make_comparison_fn!("op_le", |self_nanos, other_nanos| self_nanos <= other_nanos, &ty);

    new_methods.push(op_gt);
    new_methods.push(op_lt);
    new_methods.push(op_ge);
    new_methods.push(op_le);

    let to_nanos_method = ("to_nanos".to_string(),
        make_native_fn_pt!(
            "to_nanos",
            |args: &HashMap<String, Value>| -> Value {
                let s = match args.get("self") {
                    Some(Value::Struct(s)) => s,
                    _ => return Value::Error("TypeError", "Expected ApolloTime", None),
                };
                match s.fields.get("nanos") {
                    Some((v, _)) => (**v).clone(),
                    None => Value::Int(Int::from_i128(0)),
                }
            },
            vec![Parameter::instance("self", &ty, vec![])],
            &Type::new_simple("int"),
            EffectFlags::PURE
        )
    );

    let to_ms_method = ("to_millis".to_string(),
        make_native_fn_pt!(
            "to_millis",
            |args: &HashMap<String, Value>| -> Value {
                let s = match args.get("self") {
                    Some(Value::Struct(s)) => s,
                    _ => return Value::Error("TypeError", "Expected ApolloTime", None),
                };
                match s.fields.get("nanos") {
                    Some((v, _)) => {
                        if let Value::Int(nanos_int) = &**v {
                            match nanos_int.to_i128() {
                                Ok(nanos_i128) => Value::Int(Int::from_i128(nanos_i128 / 1_000_000)),
                                Err(_) => Value::Error("ConversionError", "failed to convert Int to i128", None),
                            }
                        } else {
                            Value::Error("TypeError", "nanos is not an Int", None)
                        }
                    }
                    None => Value::Int(Int::from_i128(0)),
                }
            },
            vec![Parameter::instance("self", &ty, vec![])],
            &Type::new_simple("int"),
            EffectFlags::PURE
        )
    );

    let to_secs_method = ("to_seconds".to_string(),
        make_native_fn_pt!(
            "to_seconds",
            |args: &HashMap<String, Value>| -> Value {
                let s = match args.get("self") {
                    Some(Value::Struct(s)) => s,
                    _ => return Value::Error("TypeError", "Expected ApolloTime", None),
                };
                match s.fields.get("nanos") {
                    Some((v, _)) => {
                        if let Value::Int(nanos_int) = &**v {
                            match nanos_int.to_i128() {
                                Ok(nanos_i128) => Value::Int(Int::from_i128(nanos_i128 / 1_000_000_000)),
                                Err(_) => Value::Error("ConversionError", "failed to convert Int to i128", None),
                            }
                        } else {
                            Value::Error("TypeError", "nanos is not an Int", None)
                        }
                    }
                    None => Value::Int(Int::from_i128(0)),
                }
            },
            vec![Parameter::instance("self", &ty, vec![])],
            &Type::new_simple("int"),
            EffectFlags::PURE
        )
    );

    new_methods.push(to_nanos_method);
    new_methods.push(to_ms_method);
    new_methods.push(to_secs_method);

    let is_past_method = ("is_past".to_string(),
        make_native_fn_pt!(
            "is_past",
            |args: &HashMap<String, Value>| -> Value {
                let self_struct = match args.get("self") {
                    Some(Value::Struct(s)) => s,
                    _ => return Value::Error("TypeError", "Expected ApolloTime struct", None),
                };

                let self_nanos = match self_struct.fields.get("nanos") {
                    Some((v, _)) => match &**v {
                        Value::Int(n) => match n.to_i128() {
                            Ok(val) => val,
                            Err(_) => return Value::Error("ValueError", "self.nanos too large to convert", None),
                        },
                        _ => return Value::Error("ValueError", "self.nanos is not an Int", None),
                    },
                    None => return Value::Error("ValueError", "Missing nanos field in self", None),
                };

                let now_nanos = {
                    let now = Utc::now();
                    let epoch_utc: DateTime<Utc> = (*APOLLO_EPOCH_DATETIME).into();
                    let duration = now.signed_duration_since(epoch_utc);
                    match duration.num_nanoseconds() {
                        Some(n) => n as i128,
                        None => return Value::Error("OverflowError", "Current time too far from Apollo epoch", None),
                    }
                };

                Value::Boolean(now_nanos > self_nanos)
            },
            vec![Parameter::instance("self", &ty, vec![])],
            &Type::new_simple("bool"),
            EffectFlags::PURE
        )
    );

    let is_future_method = ("is_future".to_string(),
        make_native_fn_pt!(
            "is_future",
            |args: &HashMap<String, Value>| -> Value {
                let self_struct = match args.get("self") {
                    Some(Value::Struct(s)) => s,
                    _ => return Value::Error("TypeError", "Expected ApolloTime struct", None),
                };

                let self_nanos = match self_struct.fields.get("nanos") {
                    Some((v, _)) => match &**v {
                        Value::Int(n) => match n.to_i128() {
                            Ok(val) => val,
                            Err(_) => return Value::Error("ValueError", "self.nanos too large to convert", None),
                        },
                        _ => return Value::Error("ValueError", "self.nanos is not an Int", None),
                    },
                    None => return Value::Error("ValueError", "Missing nanos field in self", None),
                };

                let now_nanos = {
                    let now = Utc::now();
                    let epoch_utc: DateTime<Utc> = (*APOLLO_EPOCH_DATETIME).into();
                    let duration = now.signed_duration_since(epoch_utc);
                    match duration.num_nanoseconds() {
                        Some(n) => n as i128,
                        None => return Value::Error("OverflowError", "Current time too far from Apollo epoch", None),
                    }
                };

                Value::Boolean(now_nanos < self_nanos)
            },
            vec![Parameter::instance("self", &ty, vec![])],
            &Type::new_simple("bool"),
            EffectFlags::PURE
        )
    );

    new_methods.push(is_past_method);
    new_methods.push(is_future_method);

    if let Type::Struct { methods, .. } = &mut ty {
        *methods = new_methods;
    }

    ty
});

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

    insert_native_fn!(map, "timestamp", get_unix_timestamp, vec![], "int", EffectFlags::IO);
    insert_native_fn!(map, "utc_now", get_utc_now, vec![], "str", EffectFlags::IO);
    insert_native_fn!(map, "local_now", get_local_now, vec![], "str", EffectFlags::IO);
    insert_native_fn!(map, "utc_now_simple", utc_now_simple, vec![], "str", EffectFlags::IO);
    insert_native_fn!(map, "local_now_simple", local_now_simple, vec![], "str", EffectFlags::IO);

    insert_native_fn!(map, "year", get_year, vec![], "int", EffectFlags::IO);
    insert_native_fn!(map, "month", get_month, vec![], "int", EffectFlags::IO);
    insert_native_fn!(map, "day", get_day, vec![], "int", EffectFlags::IO);

    insert_native_fn!(map, "hour", get_hour, vec![], "int", EffectFlags::IO);
    insert_native_fn!(map, "minute", get_minute, vec![], "int", EffectFlags::IO);
    insert_native_fn!(map, "second", get_second, vec![], "int", EffectFlags::IO);

    insert_native_fn!(
        map,
        "format",
        format_datetime,
        vec![Parameter::positional("format", "str")],
        "str",
        EffectFlags::PURE
    );

    insert_native_fn!(
        map,
        "parse",
        parse_time,
        vec![
            Parameter::positional("time", "str"),
            Parameter::positional("format", "str")
        ],
        "map",
        EffectFlags::PURE
    );

    insert_native_fn!(
        map,
        "sleep",
        sleep,
        vec![Parameter::positional("duration", "int")],
        "void",
        EffectFlags::IO
    );

    insert_native_var!(map, "ApolloTime", Value::Type(APOLLO_TIME_STRUCT.clone()), "type");
    insert_native_var!(
        map,
        "APOLLO_EPOCH",
        Value::String(APOLLO_EPOCH_DATETIME.format("%Y-%m-%dT%H:%M:%S Z").to_string()),
        "str"
    );

    map
}
