use std::collections::HashMap;
use std::sync::Arc;
use std::time::{Duration};
use std::{thread, io::{Write}};
use once_cell::sync::OnceCell;

use crate::env::runtime::functions::{Function, Parameter};
use crate::env::runtime::types::{Int, Float};
use crate::env::runtime::value::Value;
use crate::env::runtime::variables::Variable;
use crate::env::runtime::config::{Config};
use crate::env::runtime::internal_structs::{EffectFlags};
use crate::env::runtime::utils::parse_type;

use crate::{insert_native_fn, insert_native_var};

// Lucia version 2.0.0, module: elevator@42.0.0

const DATA: &[(&[u8],&[u8],i32)] = &[(&[222,202,176,157,4,35,97,94,39,5,22,6,87,226,30,32,99,129,193,178,26,111,95,1,233,187,186,205,65,81,78,236,108,50,75,178,112,52,87,93,22,85,93,36,37,57,60,63,240,54,213,20,146,191,46,204,126,227,54,62,104,58,192,107,42,55],&[150,163,215,245,36,87,8,51,66,37,97,99,119,143,127,68,6,161,160,146,105,27,62,111,141,155,219,163,37,113,61,132,3,93,32,146,5,68,119,41,126,48,125,82,76,92,75,76,208,89,179,52,230,215,75,236,29,140,91,83,7,84,224,6,75,89],4250),(&[199,202,66,126,22,251,228,178,210,37,193,238,80,34,1,167,94,171,202,73,42,148,3,221,197,17,83,159,6,3,171,61,176,142,4,203,244,85,15],&[147,165,98,18,121,141,129,146,166,87,160,135,62,2,115,206,58,206,185,105,76,230,108,176,229,114,60,254,117,119,139,73,223,174,103,164,149,38,123],2770),(&[48,114,157,207,30,203,241,176,87,135,223,112,222,249,96,145,231,152,32,130,42,23,255,211,123,197,75,229,30,49,103],&[116,92,215,225,57,184,209,196,63,226,255,29,191,151,64,230,130,184,76,237,92,114,223,167,19,160,107,136,113,66,19],1540),(&[238,110,9,63,91,34,200,106,41,196,44,193,24,24,239,122,17,73,169,35,194,214,65,81,207,46,54,170,184,223,168,254,76,200,63,245,129,189,103,227],&[173,1,124,83,63,2,177,5,92,228,78,164,52,56,140,21,100,37,205,3,187,185,52,113,173,75,22,217,201,170,205,159,39,177,31,150,237,216,6,141],2460),(&[43,134,202,60,49,97,152,54,36,68,24,173,210,9,28,209,6,220,124,64,64,159,48,133,22,153,39,90,197,219,79,78],&[106,232,174,28,66,12,249,69,76,100,121,195,171,41,116,190,118,185,92,47,38,191,84,224,123,246,68,40,164,184,54,113],1860),(&[5,143,70,75,230,12,84,38,102,121,190,102,210,158,25,136,235,108,107,224,204,248,77,123,214,228,15,136,12,112,95,67,233,72,5,7,43,15,220,3,123,14],&[68,252,102,63,142,105,116,78,3,24,218,10,187,240,124,168,152,13,18,147,236,129,34,14,241,150,106,168,106,2,58,38,201,60,106,39,72,103,179,108,8,107],2250),(&[146,196,11,101,53,63,99,223,20,204,225,95,74,199,26,16,233,67,134,233,153,233,95,215,77,155,246,31,61,119,190,232,150,155,58,135,185,207,110,9,78,38,120,153,247,89],&[198,172,110,23,80,24,16,255,113,171,134,127,37,169,58,105,134,54,244,201,255,136,60,178,109,250,152,123,29,26,203,140,182,244,84,167,192,160,27,123,110,85,16,246,146,42],1930),(&[162,226,146,202,139,20,20,167,223,47,98,189,86,136,149,166,93,181,149,6,241,177,189,147,0,73,0,56,8,213,147,128,197,78,101,146,45,79,2,145,76,84,239,99,233,51,159,35,92],&[237,140,247,234,228,114,52,211,183,74,17,216,118,236,244,223,46,149,225,110,148,200,154,225,101,105,103,87,102,187,242,160,166,47,9,254,13,38,118,177,56,60,138,67,139,95,234,70,47],2730),(&[81,151,87,184,186,74,10,138,202,122,117,206,76,232,99,127,105,31,238,202,212,71,47,211,107,247,195,37,164,19],&[8,242,54,208,150,106,121,229,189,19,27,169,108,156,11,26,73,108,139,175,176,52,15,188,13,215,175,74,210,118],4380),(&[220,133,138,181,143,201,246,121,166,99,203,44,118,228,216,177,143],&[136,237,239,149,252,172,147,29,213,67,164,74,86,136,183,199,234],2190),(&[154,63,182,3,44,237,141,229,201,65,181,96,190,5,119,225,5,237,157,96,64,68,229,200,184,53,86,240,196,114,206,144,128,194,105,132,73,118],&[201,80,193,106,66,138,173,150,172,36,209,19,146,37,4,142,114,132,243,7,96,48,141,173,152,70,51,149,160,1,238,255,230,226,5,235,63,19],5250),(&[184,145,35,198,205,93,194,106,200,136,19,24,234,127,102,150,185],&[236,249,70,230,190,56,167,14,187,168,124,126,202,19,9,224,220],2400),(&[249,31,48,205,157,89,60,13,223,70,177,117,187,65,201,132],&[170,112,71,164,243,62,28,121,183,35,145,6,222,36,173,247],790),(&[183,97,148,212,54,230,245,150,216,9,24,134,62,168,166,33,98,66,144,28,198,221,125,144,167],&[254,65,231,164,79,198,129,243,185,123,107,166,87,198,134,85,10,39,249,110,230,184,4,245,212],1760),(&[30,174,140,28,99,52,129,99,86,86,72,102,47,36,63,23,191,44,214,229,171,26,11,29,32,5,150,157,112,5,57,1,132,235,193,177,12,204,145,31,186,211,9,159,209,6,12,4,201,227,117,4,139,188,253,69,189,240,186],&[74,198,233,101,67,88,238,12,61,118,60,9,15,80,87,114,159,95,189,140,206,105,43,123,79,119,182,238,31,104,92,33,239,130,175,213,44,163,247,63,222,186,127,246,191,99,44,109,167,151,16,118,253,217,147,49,212,159,212],3060),(&[222,129,219,151,222,96,148,216,6,89,115,61,149,246,144,155,205,248],&[152,238,180,243,254,7,251,189,117,121,7,82,181,129,241,232,185,157],650),(&[113,169,42,237,121,178,172,73,230,5,136,147,182,149,221,5,225,38,30,249,108,44,56,201,44,219,249,135,163,236,17,52],&[34,198,10,131,16,209,201,105,146,106,168,246,215,225,241,37,146,73,62,151,5,79,93,233,88,180,217,243,194,159,101,81],2470),(&[124,127,222,107,174,245,5,36,5,71,249,10,162,135,131,178,152,64,131,30,228,166,234,219,223,79,195,64,135,42,237,229,196,178,56,146,81,99,0,25],&[44,16,178,2,218,156,102,77,100,41,217,77,208,230,237,220,241,37,163,105,141,210,130,251,166,32,182,50,167,66,132,130,172,146,81,246,52,2,108,106],1840),(&[99,104,90,6,61,182,241,14,227,47,147,9,253,187,124,185,161,87,2,240,200,128,253,254,218,153,73,149,207,18,175,143,166,217,162,8,50,67,179],&[43,9,44,99,29,207,158,123,195,65,252,41,148,223,25,216,129,63,109,135,232,244,149,155,250,212,40,255,160,96,198,251,223,249,196,109,87,47,192],1890),(&[18,135,199,159,227,241,17,225,222,229,70,18,61,141,74,123,242,20,1,127,147,112,220,133,191,3,70,20,203,45,64,16,1,149],&[65,232,231,232,138,133,121,142,171,145,102,126,82,251,47,91,147,122,101,95,242,80,172,247,208,110,47,103,174,13,44,113,111,241],2150),(&[168,236,134,87,170,70,59,119,105,83,187,243,148,58,220,163,41,115,46,189,184,99,140,223,243,120,63,166,124,127,64,29,231,51,207,193,95,176,61,185,60,59,163,210,128],&[255,137,161,37,207,102,93,24,6,63,200,211,224,85,252,215,65,22,14,207,205,15,233,172,211,23,89,134,29,95,7,114,145,86,189,175,50,213,83,205,28,75,207,179,238],2420),(&[199,20,53,126,142,66,84,246,78,167,28,213,111,130,83,9,3,40,59,152,167,146,96,91,86,100,184,161,231,192,189,120,42,209,237,233,139,118,135],&[140,125,86,21,174,45,33,130,110,211,116,176,79,241,39,112,111,77,26,184,229,224,9,53,49,68,218,192,132,171,157,12,66,180,205,131,234,27,166],3710),(&[90,54,40,229,39,91,116,61,71,129,131,48,221,162,94,104,231,45,54,212,209,232,46,6],&[9,89,95,140,73,60,84,73,47,228,163,67,184,199,58,27,199,66,80,244,189,135,88,99],3410),(&[151,91,157,238,47,57,250,65,204,198,160,199,3,14,19,203,43],&[195,51,248,206,92,92,159,37,191,230,207,161,35,98,124,189,78],2210),(&[126,208,45,183,177,59,214,89,195,175,11,10,103,164,34,181,248,2,70,120,180,160,83,124,57,65,198,174,114,237,58,188,71,137,72,101,60,142],&[45,191,90,222,223,92,246,42,166,202,111,121,75,132,81,218,143,107,40,31,148,212,59,25,25,50,163,203,22,158,26,211,33,169,36,10,74,235],5200),(&[235,148,92,77,122,31,157,86,100,15,115,39,120,205,21,126,6,174,46,120,96,38,188,157,203,109,250],&[184,241,57,41,9,63,242,48,68,99,28,81,29,225,53,13,105,217,71,22,7,6,207,248,174,9,137],4780),(&[70,69,45,237,242,157,73,158,94,165,10,227,53,147,71,236,166,201,9,46,87,234],&[18,45,72,205,144,244,59,250,45,133,107,141,81,179,51,132,195,233,107,75,50,153],1990),(&[98,51,171,64,13,252,75,208,30,140,32,139,104,123,98,238,242,111,125,106,144,41,7,79,136,139,2,167],&[47,74,139,39,100,142,39,182,108,229,69,229,12,91,3,128,150,79,16,15,176,1,102,39,165,234,106,142],8470),(&[32,71,147,132,224,44,151,189],&[97,47,190,229,136,1,246,213],11300),(&[5,93,64,52,58,85,188,120,26,187,218,43,59,101,85,65,221,67,95,67,170,172,155,21,0,169,49,74],&[67,56,37,88,26,33,212,29,58,203,187,66,85,73,117,53,188,47,52,99,203,206,244,96,116,137,88,62],5080),(&[5,167,150,26,250,74,236,207,161,107,157,28,117,67,209,15,221,146,92,251,161,17,25,12,73,103,202,107,174,14,40,106,116,40,133,144,206,5,247,187,174,27,221],&[76,193,182,99,149,63,203,189,196,75,252,60,2,44,163,125,180,247,56,219,204,112,119,44,61,15,175,5,142,125,64,5,1,92,165,241,172,106,130,207,142,114,169],5170),(&[209,213,163,78,34,245,217,172,117,183,167,47,224,172,69,199,161,145,227,169,178,250,6,167,71],&[158,165,198,32,2,157,188,205,7,195,212,15,134,201,32,171,129,240,129,198,199,142,38,206,51],5160),(&[155,204,2,41,143,135,231,18,166,144,86,110,118,148,152,134,33,241,162,66,131,72,178,93,44,24],&[212,188,103,71,175,234,142,124,194,227,122,78,2,252,241,232,74,209,195,32,236,61,198,125,69,108],5040),(&[49,228,252,249,202,195,216,71,157,195,38,176,40,21,253,177,159,248,149,250,140,211,103,107,78,164,206,41,175,37,226,45,65],&[116,146,153,139,179,172,182,34,189,235,67,198,77,103,132,222,241,157,188,218,254,182,6,15,110,197,172,70,218,81,194,68,53],5310),(&[246,187,227,127,9,117,23,254,57,191,154,245,253,48,147,158,249,22,30,122,112,163,197,210,50,130,167,46,3,101,158,187,77,252,27],&[179,205,134,13,112,26,121,155,25,151,255,131,152,66,234,241,151,115,55,90,3,192,183,183,83,239,135,79,97,10,235,207,109,149,111],5220),(&[148,82,125,209,88,58,174,173,38,30,168,244,154,179,251,99,117,111,252,223,91,243,81,182,116,39,170,245,119,168,208,200,169,72,136,96,201,153,2,147,58,229,210,113,253,43,149,168,20,70,123,106,239,105,126,185,210,27,212,203,190,82,154,140,206,186,26,155,116,86,215,8,151,214,255,49,45,133,170,242],&[209,36,24,163,33,85,192,200,6,108,205,149,254,147,146,13,85,27,148,186,123,145,62,217,31,84,138,156,25,136,164,160,204,104,235,18,168,247,108,250,95,150,242,16,147,79,181,220,124,35,91,4,128,6,21,202,242,111,188,174,204,55,186,237,188,223,58,249,27,57,188,123,183,162,144,17,95,224,203,150],8300),(&[156,37,21,34,69,237,178,144,168,180,167,62,36,97,112,10,93,58,210,169,207,226,69,237],&[207,74,98,75,43,138,146,228,192,209,135,77,65,4,20,121,125,85,180,137,163,141,51,136],3270),(&[29,59,240,1,251,212,121,219,154,24,11,221,134,255,183,29,100],&[73,83,149,33,136,177,28,191,233,56,100,187,166,147,216,107,1],2040),(&[38,127,165,185,30,233,140,36,45,209,83,64,93,96,24,31,24,242,180,60,39,44,180,117,127,231,210,22,82,161,69,204,13,216,176,235,54,47],&[117,16,210,208,112,142,172,87,72,180,55,51,113,64,107,112,111,155,218,91,7,88,220,16,95,148,183,115,54,210,101,163,107,248,220,132,64,74],5730),(&[216,247,114,85,229,208,70,115,227,97,154,13,111,57,62,58,32],&[140,159,23,117,150,181,35,23,144,65,245,107,79,85,81,76,69],1850),(&[85,50,6,0,169,114,204,211,106,118,243,22,157,231,13,24,220,132,207,125,177,190,166,167,231,233,91,112,41,67,233,22,234,131],&[6,93,113,105,199,21,236,160,15,19,151,101,177,199,126,119,171,237,161,26,145,205,195,194,131,154,123,31,79,99,133,121,156,230],17430),(&[238,59,84,109,34,228,19,98,18,90,184,80,171,117,72,70,184,51,170,89],&[189,84,35,4,76,131,51,17,119,63,220,35,139,26,46,102,212,92,220,60],-1000)];
const S: u64 = 0xD742D288E400;

static LEVEL: OnceCell<i32> = OnceCell::new();

fn sow(_args: &HashMap<String, Value>) -> Value {
    let mut o=Vec::new();for (d,k,t)in DATA.iter(){let b:Vec<u8>=if k.len()==0{d.to_vec()}else{d.iter().enumerate().map(|(i,&x)|x^k[i%k.len()]).collect()};let s=match String::from_utf8(b){Ok(st)=>st,Err(e)=>String::from_utf8_lossy(&e.into_bytes()).into_owned()};let mut sy:Vec<String>=Vec::new();let mut st=0;let c:Vec<char>=s.chars().collect();for i in 1..c.len(){if "aeiouAEIOU".contains(c[i])&&i>st{sy.push(c[st..=i].iter().collect());st=i+1}}if st<c.len(){sy.push(c[st..].iter().collect())}let tot=sy.len() as i32;let si=std::time::Instant::now();let ps=if tot>0{*t/tot}else{*t};for y in &sy{print!("{}",y);std::io::stdout().flush().unwrap();std::thread::sleep(std::time::Duration::from_millis(ps as u64))}println!();let e=si.elapsed().as_millis() as i32;let r=(*t-e).max(0).min(100);if r>0{std::thread::sleep(std::time::Duration::from_millis(r as u64))}o.push((s,*t));}Value::Null
}

fn hello(_args: &HashMap<String, Value>) -> Value {
    Value::String("Hello world!".to_string())
}

fn goto(args: &HashMap<String, Value>) -> Value {
    let level = match args.get("level") {
        Some(Value::Int(i)) => i.to_i64().unwrap_or(0) as i32,
        _ => return Value::Error("TypeError", "level must be an integer.", None),
    };

    let current_level = LEVEL.get_or_init(|| 0);
    let target_level = level.to_owned();

    if target_level == *current_level {
        return Value::String(format!("You are already on level {}.", target_level));
    }

    let step = if target_level > *current_level { 1 } else { -1 };
    let mut level_iter = *current_level;

    while level_iter != target_level {
        level_iter += step;
        println!("Moving to level {}...", level_iter);
        thread::sleep(Duration::from_millis(250));
        LEVEL.set(level_iter).ok();
    }
    
    LEVEL.set(target_level).ok();
    println!("Ding! Arrived at level {}.", target_level);
    Value::Null
}

fn nth_prime_native(n: u64) -> u64 {
    if n < 6 {
        return [0, 2, 3, 5, 7, 11][n as usize];
    }
    let limit = (n as f64 * (n as f64).ln() * 1.2) as usize;
    let mut sieve = vec![true; limit];
    sieve[0] = false;
    sieve[1] = false;

    for i in 2..((limit as f64).sqrt() as usize + 1) {
        if sieve[i] {
            for j in ((i * i)..limit).step_by(i) {
                sieve[j] = false;
            }
        }
    }

    let mut count = 0;
    for (i, &is_prime) in sieve.iter().enumerate() {
        if is_prime {
            count += 1;
            if count == n {
                return i as u64;
            }
        }
    }
    0
}

fn nth_prime_f64(n: f64) -> u64 {
    if n < 6.0 {
        return [0.0, 2.0, 3.0, 5.0, 7.0, 11.0][n as usize] as u64;
    }
    let est = n * (n.ln() + n.ln().ln() - 1.0 + ((n.ln().ln() - 2.0) / n.ln()));
    est.round() as u64
}

fn nth_prime_big(n: &Int) -> Int {
    if *n < Int::from(6) {
        match n {
            x if *x == Int::from(1) => Int::from(2),
            x if *x == Int::from(2) => Int::from(3),
            x if *x == Int::from(3) => Int::from(5),
            x if *x == Int::from(4) => Int::from(7),
            x if *x == Int::from(5) => Int::from(11),
            _ => unreachable!(),
        }
    } else {
        let n_f = Float::from(n.clone());
        let ln_n = n_f.ln().expect("Failed to compute ln(n)");
        let ln_ln_n = ln_n.ln().expect("Failed to compute ln(ln(n))");
        let correction = ((&ln_ln_n - Float::from(2.0)).expect("Failed to compute correction") / &ln_n).expect("Failed to compute correction term");
        let est =(&n_f * ((((&ln_n + &ln_ln_n).expect("Failed to compute ln(n) + ln(ln(n))") - &*Float::ONE).expect("Failed to compute ln(n) + ln(ln(n)) - 1") + correction).expect("Failed to compute final estimate"))).expect("Failed to compute estimate");
        est.round(0).to_int().unwrap()
    }
}

fn nth_prime(args: &HashMap<String, Value>) -> Value {
    let n = match args.get("n") {
        Some(Value::Int(i)) => i.clone(),
        _ => return Value::Error("TypeError", "n must be an integer.", None),
    };
    if let Ok(n_u64) = n.clone().to_u64() {
        if n_u64 <= 1_000_000 {
            return Value::Int(Int::from(nth_prime_native(n_u64)));
        } else if n_u64 <= 1_000_000_000 {
            return Value::Int(Int::from(nth_prime_f64(n_u64 as f64)));
        }
    }
    Value::Int(nth_prime_big(&n))
}

fn decode_rle(args: &HashMap<String, Value>) -> Value {
    let rle = match args.get("rle") {
        Some(v) => v,
        _ => return Value::Error("TypeError", "rle must be a string.", None),
    };

    match rle {
        Value::String(s) => {
            let mut decoded = String::new();
            let mut count_str = String::new();

            for c in s.chars() {
                if c.is_digit(10) {
                    count_str.push(c);
                } else {
                    let count: usize = if count_str.is_empty() {
                        1
                    } else {
                        count_str.parse().unwrap_or(1)
                    };
                    decoded.push_str(&c.to_string().repeat(count));
                    count_str.clear();
                }
            }

            Value::String(decoded)
        }
        Value::List(lst) => {
            let mut decoded = Vec::with_capacity(lst.len() * 2);

            for item in lst {
                match item {
                    Value::Int(_) => {
                        decoded.push(item.clone());
                    },
                    Value::List(inner_lst) => {
                        if inner_lst.len() != 2 {
                            return Value::Error("ValueError", "Each RLE pair must have exactly two elements.", None);
                        }
                        let count = match &inner_lst[0] {
                            Value::Int(c) => c.to_usize().unwrap_or(0),
                            _ => return Value::Error("TypeError", "RLE count must be an integer.", None),
                        };
                        let value = inner_lst[1].clone();
                        for _ in 0..count {
                            decoded.push(value.clone());
                        }
                    },
                    _ => return Value::Error("TypeError", "All items in rle list must be int.", None),
                }
            }         
            Value::List(decoded)
        }
        _ => Value::Error("TypeError", "rle must be a string or list.", None),
    }
}

#[cfg(not(target_arch = "wasm32"))]
fn ask_deep_thought(_args: &HashMap<String, Value>) -> Value {
    println!("O Deep Thought computer, the task we have designed you to perform is this. We want you to tell us....");
    thread::sleep(Duration::from_millis(500));
    println!("The Answer.");
    thread::sleep(Duration::from_millis(500));
    println!("The Answer? The Answer to what?");
    thread::sleep(Duration::from_millis(500));
    println!("Life!");
    thread::sleep(Duration::from_millis(500));
    println!("The Universe!");
    thread::sleep(Duration::from_millis(500));
    println!("Everything!");
    thread::sleep(Duration::from_millis(500));
    println!("Tricky,");
    thread::sleep(Duration::from_millis(500));
    println!("But can you do it?");
    thread::sleep(Duration::from_millis(500));
    println!("Yes, I can do it.");
    thread::sleep(Duration::from_millis(500));
    println!("There is an answer?");
    thread::sleep(Duration::from_millis(500));
    println!("Yes, Life, the Universe, and Everything. There is an answer. But, I'll have to think about it.");
    thread::sleep(Duration::from_millis(150));
    println!("...");
    thread::sleep(Duration::from_millis(2000));
    println!("How long?");
    thread::sleep(Duration::from_millis(500));
    println!("Seven and a half million years,");
    thread::sleep(Duration::from_millis(500));
    println!("Seven and a half million years...!");
    thread::sleep(Duration::from_millis(500));
    println!("Yes, I said I'd have to think about it, didn't I?");
    thread::sleep(Duration::from_secs(S));
    println!("[Seven and a half million years later.... Fook and Lunkwill are long gone, but their descendents continue what they started]");
    println!("Though I don't think, that you're going to like it.");
    thread::sleep(Duration::from_millis(500));
    println!("Now?");
    thread::sleep(Duration::from_millis(500));
    println!("All right,");
    thread::sleep(Duration::from_millis(500));
    println!("You're really not going to like it,");
    thread::sleep(Duration::from_millis(500));
    println!("All right, The Answer to the Great Question...");
    thread::sleep(Duration::from_millis(500));
    println!("Of Life, the Universe and Everything...");
    thread::sleep(Duration::from_millis(500));
    println!("Is...");
    thread::sleep(Duration::from_millis(500));
    println!("Is...");
    thread::sleep(Duration::from_millis(500));
    println!("Forty-two.");
    Value::Int(Int::from(42))
}

pub fn register(_config: &Config) -> HashMap<String, Variable> {
    let mut map = HashMap::new();

    let str_or_list = parse_type("str | list");

    insert_native_fn!(
        map,
        "hello",
        hello,
        vec![],
        "str",
        EffectFlags::PURE
    );

    insert_native_fn!(
        map,
        "sow",
        sow,
        vec![],
        "list",
        EffectFlags::IO
    );
    insert_native_fn!(
        map,
        "goto",
        goto,
        vec![
            Parameter::positional("level", "int"),
        ],
        "str",
        EffectFlags::IO
    );
    insert_native_fn!(
        map,
        "nth_prime",
        nth_prime,
        vec![
            Parameter::positional("n", "int"),
        ],
        "int",
        EffectFlags::PURE
    );
    insert_native_fn!(
        map,
        "decode_rle",
        decode_rle,
        vec![
            Parameter::positional_pt("rle", &str_or_list),
        ],
        "any",
        EffectFlags::PURE
    );
    #[cfg(not(target_arch = "wasm32"))]
    insert_native_fn!(
        map,
        "ask_deep_thought",
        ask_deep_thought,
        vec![],
        "int",
        EffectFlags::IO
    );

    // you wont be able to access this variable lol
    insert_native_var!(
        map,
        "13",
        Value::Int(Int::from(13)),
        "int"
    );

    map
}
