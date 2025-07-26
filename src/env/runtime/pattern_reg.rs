use crate::env::runtime::value::Value;
use crate::env::runtime::utils::{
    to_static,
    get_imagnum_error_message,
};
use crate::env::runtime::internal_structs::PatternMethod;
use std::f64::consts::PI;

pub fn extract_seed_end(
    seed: Vec<Value>,
    end: Value,
) -> Result<(Vec<f64>, f64), (&'static str, &'static str, &'static str)> {
    let mut seed_f64 = Vec::with_capacity(seed.len());

    for v in &seed {
        let f = match v {
            Value::Float(f) => f
                .to_f64()
                .map_err(|c| ("ConversionError", to_static(get_imagnum_error_message(c)), ""))?,
            Value::Int(i) => i
                .to_i64()
                .map_err(|c| ("ConversionError", to_static(get_imagnum_error_message(c)), ""))? as f64,
            _ => return Err(("TypeError", "value is not a Float or Int", "")),
        };
        seed_f64.push(f);
    }
    
    let end_f64 = match &end {
        Value::Float(f) => f
            .to_f64()
            .map_err(|c| ("ConversionError", to_static(get_imagnum_error_message(c)), ""))?,
        Value::Int(i) => i
            .to_i64()
            .map_err(|c| ("ConversionError", to_static(get_imagnum_error_message(c)), ""))? as f64,
        _ => return Err(("TypeError", "end value is not a Float or Int", "")),
    };       

    Ok((seed_f64, end_f64))
}

pub fn predict_sequence(
    seed: Vec<Value>,
    end: Value,
) -> Result<(Vec<f64>, PatternMethod), (&'static str, &'static str, &'static str)> {
    let (mut seed_f64, end_f64) = extract_seed_end(seed, end)?;

    if seed_f64.is_empty() {
        return Err((
            "InputError",
            "Seed cannot be empty",
            "Provide a non-empty seed vector",
        ));
    }

    if seed_f64.len() == 1 {
        let start = seed_f64[0];
        if end_f64 == start {
            return Ok((vec![start], PatternMethod::Arithmetic));
        }
    
        let step = if end_f64 > start { 1.0 } else { -1.0 };
    
        let mut res = Vec::new();
        let mut val = start;
    
        if (step > 0.0 && end_f64 < start) || (step < 0.0 && end_f64 > start) {
            return Err((
                "ValueError",
                "End value does not match the sequence direction",
                "",
            ));
        }
    
        while (step > 0.0 && val <= end_f64) || (step < 0.0 && val >= end_f64) {
            res.push(val);
            val += step;
        }
        return Ok((res, PatternMethod::Arithmetic));
    }
    
    // Fibonacci
    if try_fibonacci(&seed_f64) {
        while *seed_f64.last().unwrap() < end_f64 {
            let len = seed_f64.len();
            let next = seed_f64[len - 1] + seed_f64[len - 2];
            seed_f64.push(next);
        }
        if let Some(&last) = seed_f64.last() {
            if last != end_f64 {
                let closest = seed_f64.iter()
                    .min_by(|a, b| {
                        (*a - end_f64).abs().partial_cmp(&(*b - end_f64).abs()).unwrap()
                    })
                    .unwrap();
                let help_msg = if *closest != end_f64 {
                    to_static(format!("Did you mean to end with the closest Fibonacci number {} instead?", closest))
                } else {
                    ""
                };
                return Err((
                    "PatternError",
                    "Fibonacci sequence does not reach the exact end value",
                    help_msg,
                ));
            }
        }
        return Ok((seed_f64, PatternMethod::Fibonacci));
    }

    // Geometric
    if let Some(ratios) = try_geometric(&seed_f64) {
        let ratio = ratios[0];
        while *seed_f64.last().unwrap() < end_f64 {
            let next = seed_f64.last().unwrap() * ratio;
            seed_f64.push(next);
        }
        if let Some(&last) = seed_f64.last() {
            if last != end_f64 {
                return Err((
                    "PatternError",
                    "Geometric sequence does not reach the exact end value",
                    "",
                ));
            }
        }
        return Ok((seed_f64, PatternMethod::Geometric));
    }

    // Linear
    if let Some(diff) = try_linear(&seed_f64) {
        loop {
            let last = *seed_f64.last().unwrap();
            if last == end_f64 {
                break;
            }
            let next = last + diff;
    
            if (diff > 0.0 && next > end_f64) || (diff < 0.0 && next < end_f64) {
                let closest = *seed_f64.iter()
                    .min_by(|a, b| ((*a - end_f64).abs().partial_cmp(&(*b - end_f64).abs()).unwrap()))
                    .unwrap();
                return Err((
                    "PatternError",
                    "Linear sequence does not reach the exact end value",
                    to_static(format!("Did you mean to end at this closest linear value {} instead", closest))
                ));
            }
    
            seed_f64.push(next);
        }
        return Ok((seed_f64, PatternMethod::Linear));
    }

    // Factorial
    if try_factorial(&seed_f64) {
        let mut i = seed_f64.len();
        while *seed_f64.last().unwrap() < end_f64 {
            let next = (1..=i).map(|x| x as f64).product::<f64>();
            seed_f64.push(next);
            i += 1;
        }
        if let Some(&last) = seed_f64.last() {
            if last != end_f64 {
                let closest = seed_f64.iter()
                    .min_by(|a, b| ((*a - end_f64).abs().partial_cmp(&(*b - end_f64).abs()).unwrap()))
                    .unwrap();
                let help_msg = to_static(format!("Did you mean to end at this closest factorial value {} instead?", closest));
                return Err(("PatternError", "Factorial sequence does not reach the exact end value", help_msg));
            }
        }
        return Ok((seed_f64, PatternMethod::Factorial));
    }

    // Quadratic
    if let Some(second_diff) = try_quadratic(&seed_f64) {
        while *seed_f64.last().unwrap() < end_f64 {
            let len = seed_f64.len();
            let next = 2.0 * seed_f64[len - 1] - seed_f64[len - 2] + second_diff;
    
            if (second_diff > 0.0 && next > end_f64) || (second_diff < 0.0 && next < end_f64) {
                break;
            }
    
            seed_f64.push(next);
        }
    
        if let Some(&last) = seed_f64.last() {
            if last != end_f64 {
                let closest = seed_f64.iter()
                    .min_by(|a, b| ((*a - end_f64).abs().partial_cmp(&(*b - end_f64).abs()).unwrap()))
                    .unwrap();
                return Err((
                    "PatternError",
                    "Quadratic sequence does not reach the exact end value",
                    to_static(format!("Did you mean to end at this closest quadratic value {} instead?", closest)),
                ));
            }
        }
        let (a, b, c) = solve_quadratic_coeffs(&seed_f64).unwrap_or((0.0, 0.0, 0.0));
        return Ok((seed_f64, PatternMethod::Quadratic { a, b, c }));
    }

    // Exponential
    if let Some(base_ratio) = try_exponential(&seed_f64) {
        while *seed_f64.last().unwrap() < end_f64 {
            let next = seed_f64.last().unwrap() * base_ratio;
            seed_f64.push(next);
        }
        if let Some(&last) = seed_f64.last() {
            if last != end_f64 {
                return Err(("PatternError", "Exponential sequence does not reach the exact end value", ""));
            }
        }
        return Ok((seed_f64, PatternMethod::Exponential));
    }

    // Alternating
    if try_alternating(&seed_f64) {
        while seed_f64.len() == 0 || *seed_f64.last().unwrap() != end_f64 {
            let next = -seed_f64.last().unwrap();
            seed_f64.push(next);

            if (next > end_f64 && *seed_f64.first().unwrap() < end_f64)
                || (next < end_f64 && *seed_f64.first().unwrap() > end_f64)
            {
                break;
            }
        }

        if *seed_f64.last().unwrap() != end_f64 {
            return Err((
                "PatternError",
                "Alternating sequence does not reach the exact end value",
                "",
            ));
        }

        return Ok((seed_f64, PatternMethod::Alternating));
    }

    // Trigonometric
    if let Some(f) = try_trigonometric(&seed_f64) {
        let mut i = seed_f64.len();
        while seed_f64.len() == 0 || *seed_f64.last().unwrap() != end_f64 {
            let next = f(i as f64);
            seed_f64.push(next);

            if (next > end_f64 && *seed_f64.first().unwrap() < end_f64)
                || (next < end_f64 && *seed_f64.first().unwrap() > end_f64)
            {
                break;
            }

            i += 1;
        }

        if *seed_f64.last().unwrap() != end_f64 {
            return Err((
                "PatternError",
                "Trigonometric sequence does not reach the exact end value",
                "",
            ));
        }

        return Ok((seed_f64, PatternMethod::Trigonometric));
    }

    // Polynomial (Newton's finite differences)
    let mut diffs = finite_differences(&seed_f64);

    fn newton_next_element(diffs: &[Vec<f64>], n: usize) -> f64 {
        let mut result = diffs[0][0];
        let mut factorial = 1.0;
        let mut product = 1.0;

        for i in 1..diffs.len() {
            factorial *= i as f64;
            product *= n as f64 - (i as f64 - 1.0);
            result += (product / factorial) * diffs[i][0];
        }

        result
    }

    loop {
        let n = seed_f64.len();
        let next = check_finite(newton_next_element(&diffs, n))?;
        let last = *seed_f64.last().unwrap();

        if (last <= end_f64 && next > end_f64) || (last >= end_f64 && next < end_f64) {
            break;
        }
        seed_f64.push(next);
        diffs = finite_differences(&seed_f64);
    }

    if let Some(&last) = seed_f64.last() {
        if last != end_f64 {
            let closest = seed_f64.iter().min_by(|a, b| {
                (*a - end_f64).abs().partial_cmp(&(*b - end_f64).abs()).unwrap()
            }).unwrap();
    
            return Err((
                "PatternError",
                "Predicted polynomial sequence does not reach the exact end value",
                to_static(format!("Did you mean to end at this closest predicted value {} instead?", closest)),
            ));
        }
    } else {
        return Err((
            "PatternError",
            "No sequence predicted",
            "Check if the seed and end values are valid",
        ));
    }
    
    let coeffs = diffs.iter().map(|diff| diff[0]).collect::<Vec<f64>>();
    Ok((seed_f64, PatternMethod::Polynomial(coeffs)))
}

pub fn predict_sequence_until_length(
    seed: Vec<Value>,
    length: usize,
) -> Result<(Vec<f64>, PatternMethod), (&'static str, &'static str, &'static str)> {
    let mut seed_f64 = Vec::with_capacity(seed.len());

    for v in &seed {
        let f = match v {
            Value::Float(f) => f.to_f64().map_err(|c| ("ConversionError", to_static(get_imagnum_error_message(c)), ""))?,
            Value::Int(i) => i.to_i64().map_err(|c| ("ConversionError", to_static(get_imagnum_error_message(c)), ""))? as f64,
            _ => return Err(("TypeError", "Seed values must be Float or Int", "")),
        };
        seed_f64.push(f);
    }

    if seed_f64.is_empty() {
        return Err(("InputError", "Seed cannot be empty", "Provide a non-empty seed vector"));
    }

    if length < seed_f64.len() {
        return Err(("ValueError", "Length must be >= seed length", ""));
    }

    if seed_f64.len() == length {
        return Ok((seed_f64, PatternMethod::Arithmetic));
    }

    macro_rules! done {
        () => {
            seed_f64.len() >= length
        };
    }

    // handle 1-element seed (arithmetic sequence step = ±1)
    if seed_f64.len() == 1 {
        let _start = seed_f64[0];
        let step = 1.0;

        while !done!() {
            seed_f64.push(seed_f64.last().unwrap() + step);
        }
        seed_f64.truncate(length);
        return Ok((seed_f64, PatternMethod::Arithmetic));
    }

    // Fibonacci
    if try_fibonacci(&seed_f64) {
        while !done!() {
            let len = seed_f64.len();
            let next = seed_f64[len - 1] + seed_f64[len - 2];
            seed_f64.push(next);
        }
        seed_f64.truncate(length);
        return Ok((seed_f64, PatternMethod::Fibonacci));
    }

    // Geometric
    if let Some(ratios) = try_geometric(&seed_f64) {
        let ratio = ratios[0];
        while !done!() {
            let next = seed_f64.last().unwrap() * ratio;
            seed_f64.push(next);
        }
        seed_f64.truncate(length);
        return Ok((seed_f64, PatternMethod::Geometric));
    }

    // Linear
    if let Some(diff) = try_linear(&seed_f64) {
        while !done!() {
            let next = seed_f64.last().unwrap() + diff;
            seed_f64.push(next);
        }
        seed_f64.truncate(length);
        return Ok((seed_f64, PatternMethod::Linear));
    }

    // Factorial
    if try_factorial(&seed_f64) {
        let mut i = seed_f64.len();
        while !done!() {
            let next = (1..=i).map(|x| x as f64).product::<f64>();
            seed_f64.push(next);
            i += 1;
        }
        seed_f64.truncate(length);
        return Ok((seed_f64, PatternMethod::Factorial));
    }

    // Quadratic
    if let Some(second_diff) = try_quadratic(&seed_f64) {
        while !done!() {
            let len = seed_f64.len();
            let next = 2.0 * seed_f64[len - 1] - seed_f64[len - 2] + second_diff;
            seed_f64.push(next);
        }
        seed_f64.truncate(length);
        let (a, b, c) = solve_quadratic_coeffs(&seed_f64).unwrap_or((0.0, 0.0, 0.0));
        return Ok((seed_f64, PatternMethod::Quadratic { a, b, c }));
    }

    // Exponential
    if let Some(base_ratio) = try_exponential(&seed_f64) {
        while !done!() {
            let next = seed_f64.last().unwrap() * base_ratio;
            seed_f64.push(next);
        }
        seed_f64.truncate(length);
        return Ok((seed_f64, PatternMethod::Exponential));
    }

    // Alternating
    if try_alternating(&seed_f64) {
        while !done!() {
            let next = -seed_f64.last().unwrap();
            seed_f64.push(next);
        }
        seed_f64.truncate(length);
        return Ok((seed_f64, PatternMethod::Alternating));
    }

    // Trigonometric
    if let Some(f) = try_trigonometric(&seed_f64) {
        let mut i = seed_f64.len();
        while !done!() {
            seed_f64.push(f(i as f64));
            i += 1;
        }
        seed_f64.truncate(length);
        return Ok((seed_f64, PatternMethod::Trigonometric));
    }

    // Polynomial (Newton's finite differences)
    let mut diffs = finite_differences(&seed_f64);

    fn newton_next_element(diffs: &[Vec<f64>], n: usize) -> f64 {
        let mut result = diffs[0][0];
        let mut factorial = 1.0;
        let mut product = 1.0;

        for i in 1..diffs.len() {
            factorial *= i as f64;
            product *= n as f64 - (i as f64 - 1.0);
            result += (product / factorial) * diffs[i][0];
        }

        result
    }

    while !done!() {
        let n = seed_f64.len();
        let next = check_finite(newton_next_element(&diffs, n))?;
        seed_f64.push(next);
        diffs = finite_differences(&seed_f64);
    }

    seed_f64.truncate(length);
    let coeffs = diffs.iter().map(|diff| diff[0]).collect::<Vec<f64>>();
    Ok((seed_f64, PatternMethod::Polynomial(coeffs)))
}

fn is_close(a: f64, b: f64, epsilon: f64) -> bool {
    if a.is_nan() || b.is_nan() {
        false
    } else {
        (a - b).abs() < epsilon
    }
}

fn try_geometric(seq: &[f64]) -> Option<Vec<f64>> {
    if seq.len() < 2 {
        return None;
    }
    let ratios: Vec<f64> = seq.windows(2).map(|w| w[1] / w[0]).collect();
    let first_ratio = ratios[0];
    if !ratios.iter().all(|&r| is_close(r, first_ratio, 1e-10)) {
        return None;
    }
    Some(ratios)
}

fn try_fibonacci(seq: &[f64]) -> bool {
    if seq.len() < 3 {
        return false;
    }
    seq.windows(3).all(|w| is_close(w[2], w[1] + w[0], 1e-10))
}

fn try_linear(seq: &[f64]) -> Option<f64> {
    if seq.len() < 2 {
        return None;
    }
    let diffs: Vec<f64> = seq.windows(2).map(|w| w[1] - w[0]).collect();

    let first_diff = diffs[0];

    if diffs.iter().all(|&d| is_close(d, first_diff, 1e-8)) {
        Some(first_diff)
    } else {
        None
    }
}

fn try_factorial(seq: &[f64]) -> bool {
    if seq.len() < 2 {
        return false;
    }
    let mut fact = 1.0;
    for (i, &val) in seq.iter().enumerate() {
        if i > 0 {
            fact *= i as f64;
        }
        if !is_close(val, fact, 1e-6) {
            return false;
        }
    }
    true
}

fn try_exponential(seq: &[f64]) -> Option<f64> {
    if seq.len() < 2 {
        return None;
    }
    let ratios: Vec<f64> = seq.windows(2).map(|w| w[1] / w[0]).collect();

    if ratios.len() < 2 {
        return None;
    }
    let ratio_of_ratios: Vec<f64> = ratios.windows(2).map(|w| w[1] / w[0]).collect();

    if ratio_of_ratios.iter().all(|&r| is_close(r, ratio_of_ratios[0], 1e-6)) {
        Some(ratios[0])
    } else {
        None
    }
}

fn try_quadratic(seq: &[f64]) -> Option<f64> {
    if seq.len() < 3 {
        return None;
    }
    let first_diffs: Vec<f64> = seq.windows(2).map(|w| w[1] - w[0]).collect();
    let second_diffs: Vec<f64> = first_diffs.windows(2).map(|w| w[1] - w[0]).collect();

    let second_diff = second_diffs[0];
    if second_diffs.iter().all(|&d| is_close(d, second_diff, 1e-8)) {
        Some(second_diff)
    } else {
        None
    }
}

fn solve_quadratic_coeffs(seq: &[f64]) -> Option<(f64, f64, f64)> {
    if seq.len() < 3 {
        return None;
    }
    let y0 = seq[0];
    let y1 = seq[1];
    let y2 = seq[2];

    let a = (y2 - 2.0 * y1 + y0) / 2.0;
    let b = y1 - y0 - a;
    let c = y0;

    Some((a, b, c))
}

fn try_alternating(seq: &[f64]) -> bool {
    if seq.len() < 2 {
        return false;
    }

    for i in 1..seq.len() {
        if !is_close(seq[i], -seq[i - 1], 1e-10) {
            return false;
        }
    }

    true
}

fn try_trigonometric(seq: &[f64]) -> Option<fn(f64) -> f64> {
    if seq.len() < 4 {
        return None;
    }

    let sin_like = (0..seq.len()).all(|i| is_close(seq[i], (i as f64 * PI / 2.0).sin(), 1e-3));
    if sin_like {
        return Some(|x| (x * PI / 2.0).sin());
    }

    let cos_like = (0..seq.len()).all(|i| is_close(seq[i], (i as f64 * PI / 2.0).cos(), 1e-3));
    if cos_like {
        return Some(|x| (x * PI / 2.0).cos());
    }

    None
}

fn finite_differences(seq: &[f64]) -> Vec<Vec<f64>> {
    let mut table = vec![seq.to_vec()];
    while table.last().unwrap().len() > 1 {
        let prev = table.last().unwrap();
        let next_diff: Vec<f64> = prev.windows(2).map(|w| w[1] - w[0]).collect();
        table.push(next_diff);
    }
    table
}

fn check_finite(value: f64) -> Result<f64, (&'static str, &'static str, &'static str)> {
    if value.is_finite() {
        Ok(value)
    } else {
        Err((
            "PatternError",
            "Sequence value overflowed f64 limit",
            "",
        ))
    }
}
