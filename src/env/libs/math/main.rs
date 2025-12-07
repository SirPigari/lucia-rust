use std::collections::HashMap;
use std::sync::Arc;
use crate::env::runtime::functions::{Function, Parameter};
use crate::env::runtime::types::{Float, Int};
use crate::env::runtime::value::Value;
use crate::env::runtime::utils::{get_imagnum_error_message, parse_type};
use crate::env::runtime::internal_structs::EffectFlags;
use crate::env::runtime::variables::Variable;
use imagnum::ApproxEq;

use crate::{insert_native_fn, insert_native_var};

// This module provides mathematical functions and constants.
// It includes basic operations like sqrt, sin, cos, etc., and constants like PI, E, etc.
// Lucia version 2.0.0, module: math@1.0.0

const _E: &str =                "2.71828182845904523536028747135266249775724709369995957496696762772407663035354759457138217852516642742746639193200305992181741359662904357";
const _PI: &str =               "3.14159265358979323846264338327950288419716939937510582097494459230781640628620899862803482534211706798214808651328230664709384460955058223";
const _GOLDEN_RATIO: &str =     "1.61803398874989484820458683436563811772030917980576286213544862270526046281890244970720720418939113748475408807538689175212663386222353693";
const _TAU: &str =              "6.28318530717958647692528676655900576839433879875021164194988918461563281257241799725606965068423413596429617302656461329418768921910116446";
const _EULER_MASCHERONI: &str = "0.57721566490153286060651209008240243104215933593992359880576723488486772677766467093694706329174674951463144724980708248096050401448654284";
const _APERY: &str =            "1.20205690315959428539973816151144999076498629234049888179227155534183820578631309018645587360933525814619915779526071941849199599867328321";
const _SQRT2: &str =            "1.41421356237309504880168872420969807856967187537694807317667973799073247846210703885038753432764157273501384623091229702492483605585073721";
const _LN2: &str =              "0.69314718055994530941723212145817656807550013436025525412068000949339362196969471560586332699641868754200148102057068573368552023575813055";
const _ZETA2: &str =            "1.64493406684822643647241516664602518921894990120679843773555822937000747040320087383362890061975870530400431896233719067962872468700500779";
const _CATALAN: &str =          "0.91596559417721901505460351493238411077414937428167213426649811962176301977625476947935651292611510624857442261919619957903589880332585905";

const SMALL_PRIMES: &[i64] = &[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499, 503, 509, 521, 523, 541, 547, 557, 563, 569, 571, 577, 587, 593, 599, 601, 607, 613, 617, 619, 631, 641, 643, 647, 653, 659, 661, 673, 677, 683, 691, 701, 709, 719, 727, 733, 739, 743, 751, 757, 761, 769, 773, 787, 797, 809, 811, 821, 823, 827, 829, 839, 853, 857, 859, 863, 877, 881, 883, 887, 907, 911, 919, 929, 937, 941, 947, 953, 967, 971, 977, 983, 991, 997];

fn math_error(err_id: i8) -> Value {
    Value::Error("MathError", get_imagnum_error_message(err_id), None)
}

fn create_float_constant(name: &str, value: &str) -> Variable {
    Variable::new(
        name.to_string(),
        match Float::from_str(value) {
            Ok(num) => Value::Float(num),
            Err(_) => Value::Error("RuntimeError", "Invalid float format", None),
        },
        "float".to_string(),
        true,
        true,
        true,
    )
}

macro_rules! define_unary {
    ($name:ident, $int_fn:path, $float_fn:path, $return_int:path, $return_float:path) => {
        fn $name(args: &HashMap<String, Value>) -> Value {
            match args.get("x") {
                Some(Value::Int(i)) => match $int_fn(i) {
                    Ok(v) => $return_int(v),
                    Err(e) => math_error(e),
                },
                Some(Value::Float(f)) => match $float_fn(f) {
                    Ok(v) => $return_float(v),
                    Err(e) => math_error(e),
                },
                _ => Value::Error("TypeError", "expected int or float", None),
            }
        }
    };
}

fn int_abs(i: &Int) -> Result<Int, i8> { Ok(Int::abs(i)) }
fn float_abs(f: &Float) -> Result<Float, i8> { Ok(Float::abs(f)) }
fn int_log(i: &Int) -> Result<Float, i8> { Float::from_int(i).and_then(|f| Float::ln(&f)) }

define_unary!(sqrt, Int::sqrt, Float::sqrt, Value::Float, Value::Float);
define_unary!(sin, Int::sin, Float::sin, Value::Float, Value::Float);
define_unary!(cos, Int::cos, Float::cos, Value::Float, Value::Float);
define_unary!(tan, Int::tan, Float::tan, Value::Float, Value::Float);
define_unary!(ln, Int::ln, Float::ln, Value::Float, Value::Float);
define_unary!(abs, int_abs, float_abs, Value::Int, Value::Float);
define_unary!(exp, Int::exp, Float::exp, Value::Float, Value::Float);
define_unary!(floor, Int::floor, Float::floor, Value::Int, Value::Float);
define_unary!(ceil, Int::ceil, Float::ceil, Value::Int, Value::Float);
define_unary!(log10, int_log, Float::log10, Value::Float, Value::Float);

fn log(args: &HashMap<String, Value>) -> Value {
    let x = match args.get("x") {
        Some(v @ Value::Int(_)) | Some(v @ Value::Float(_)) => v,
        _ => return Value::Error("TypeError", "expected int or float", None),
    };
    let base = match args.get("base") {
        Some(v @ Value::Int(_)) | Some(v @ Value::Float(_)) => v,
        _ => return Value::Error("TypeError", "expected int or float", None),
    };
    let x_float = match x {
        Value::Int(i) => match Int::to_float(i) {
            Ok(f) => f,
            Err(e) => return math_error(e),
        },
        Value::Float(f) => f.clone(),
        _ => unreachable!(),
    };
    let base_float = match base {
        Value::Int(i) => match Int::to_float(i) {
            Ok(f) => f,
            Err(e) => return math_error(e),
        },
        Value::Float(f) => f.clone(),
        _ => unreachable!(),
    };
    match Float::log(&x_float, &base_float) {
        Ok(v) => Value::Float(v),
        Err(e) => math_error(e),
    }
}

fn conj(args: &HashMap<String, Value>) -> Value {
    match args.get("x") {
        Some(Value::Int(i)) => Value::Float(match Int::to_float(i) {
            Ok(f) => f.clone(),
            Err(e) => return math_error(e), 
        }),
        Some(Value::Float(f)) => Value::Float(f.conj()),
        _ => Value::Error("TypeError", "expected int or float", None),
    }
}

fn round(args: &HashMap<String, Value>) -> Value {
    let x = match args.get("x") {
        Some(v @ Value::Int(_)) | Some(v @ Value::Float(_)) => v,
        _ => return Value::Error("TypeError", "expected int or float", None),
    };

    let prec = match args.get("precision") {
        Some(Value::Int(i)) => i.to_usize().unwrap_or(0),
        Some(Value::Float(f)) => f.to_int().unwrap_or(Int::new()).to_usize().unwrap_or(0),
        _ => 0,
    };

    match x {
        Value::Int(i) => match Int::to_float(i) {
            Ok(f) => Value::Float(f.round(prec)),
            Err(e) => math_error(e),
        },
        Value::Float(f) => Value::Float(f.round(prec)),
        _ => unreachable!(),
    }
}

fn pow(args: &HashMap<String, Value>) -> Value {
    let x_val = args.get("x");
    let y_val = args.get("y");

    let x_float = match x_val {
        Some(Value::Float(f)) => f.clone(),
        Some(Value::Int(i)) => Int::to_float(i).expect("Int to float failed"),
        _ => return Value::Error("TypeError", "expected int or float", None),
    };

    let y_float = match y_val {
        Some(Value::Float(f)) => f.clone(),
        Some(Value::Int(i)) => Int::to_float(i).expect("Int to float failed"),
        _ => return Value::Error("TypeError", "expected int or float", None),
    };

    match (x_val, y_val) {
        (Some(Value::Int(x)), Some(Value::Int(y))) => match Int::pow(x, y) {
            Ok(v) => Value::Int(v),
            Err(e) => math_error(e),
        },
        _ => match Float::pow(&x_float, &y_float) {
            Ok(v) => Value::Float(v),
            Err(e) => math_error(e),
        },
    }
}

fn sign(args: &HashMap<String, Value>) -> Value {
    match args.get("x") {
        Some(Value::Int(i)) => {
            if i.is_zero() {
                Value::Int(Int::from(0))
            } else if i > &Int::from(0) {
                Value::Int(Int::from(1))
            } else {
                Value::Int(Int::from(-1))
            }
        },
        Some(Value::Float(f)) => {
            if f.is_zero() {
                Value::Int(Int::from(0))
            } else if f > &Float::from_f64(0.0) {
                Value::Int(Int::from(1))
            } else {
                Value::Int(Int::from(-1))
            }
        },
        _ => Value::Error("TypeError", "expected int or float", None),
    }
}

fn get_int_arg(args: &HashMap<String, Value>, name: &str) -> Result<Int, Value> {
    match args.get(name) {
        Some(Value::Int(i)) => Ok(i.clone()),
        _ => Err(Value::Error("TypeError", "expected int", None)),
    }
}

fn gcd_fn(args: &HashMap<String, Value>) -> Value {
    let mut a = match get_int_arg(args, "a") {
        Ok(v) => v,
        Err(e) => return e,
    };
    let mut b = match get_int_arg(args, "b") {
        Ok(v) => v,
        Err(e) => return e,
    };

    while !b.is_zero() {
        match a._modulo(&b) {
            Ok(r) => {
                a = b;
                b = r;
            }
            Err(e) => return math_error(e),
        }
    }
    Value::Int(Int::abs(&a))
}

fn lcm_fn(args: &HashMap<String, Value>) -> Value {
    let a = match get_int_arg(args, "a") {
        Ok(v) => v,
        Err(e) => return e,
    };
    let b = match get_int_arg(args, "b") {
        Ok(v) => v,
        Err(e) => return e,
    };

    if a.is_zero() || b.is_zero() {
        return Value::Int(Int::from(0));
    }

    let mut aa = a.clone();
    let mut bb = b.clone();
    while !bb.is_zero() {
        match aa._modulo(&bb) {
            Ok(r) => { aa = bb; bb = r; }
            Err(e) => return math_error(e),
        }
    }
    let g = Int::abs(&aa);

    match a._div(&g) {
        Ok(adg) => match adg._mul(&b) {
            Ok(l) => Value::Int(Int::abs(&l)),
            Err(e) => math_error(e),
        },
        Err(e) => math_error(e),
    }
}

fn divmod_fn(args: &HashMap<String, Value>) -> Value {
    let a = match get_int_arg(args, "a") {
        Ok(v) => v,
        Err(e) => return e,
    };
    let b = match get_int_arg(args, "b") {
        Ok(v) => v,
        Err(e) => return e,
    };

    if b.is_zero() {
        return Value::Error("ZeroDivisionError", "division by zero", None);
    }

    match a._div(&b) {
        Ok(q) => match a._modulo(&b) {
            Ok(r) => Value::Tuple(vec![Value::Int(q), Value::Int(r)]),
            Err(e) => math_error(e),
        },
        Err(e) => math_error(e),
    }
}

fn modpow_fn(args: &HashMap<String, Value>) -> Value {
    let base = match get_int_arg(args, "base") {
        Ok(v) => v,
        Err(e) => return e,
    };
    let exp = match get_int_arg(args, "exp") {
        Ok(v) => v,
        Err(e) => return e,
    };
    let modu = match get_int_arg(args, "mod") {
        Ok(v) => v,
        Err(e) => return e,
    };

    if modu.is_zero() {
        return Value::Error("ZeroDivisionError", "modulo by zero", None);
    }

    let mut result = Int::from(1);
    let mut base_acc = match base._modulo(&modu) {
        Ok(v) => v,
        Err(e) => return math_error(e),
    };
    let mut exponent = exp.clone();

    if exponent.is_negative() {
        return Value::Error("ValueError", "negative exponent", None);
    }

    let one = Int::from(1);
    let zero = Int::from(0);

    while !exponent.is_zero() {
        match exponent._bitand(&one) {
            Ok(rem) => {
                if rem != zero {
                    match result._mul(&base_acc) {
                        Ok(r) => match r._modulo(&modu) {
                            Ok(mr) => result = mr,
                            Err(e) => return math_error(e),
                        },
                        Err(e) => return math_error(e),
                    }
                }
            }
            Err(e) => return math_error(e),
        }

        match base_acc._mul(&base_acc) {
            Ok(sq) => match sq._modulo(&modu) {
                Ok(m) => base_acc = m,
                Err(e) => return math_error(e),
            },
            Err(e) => return math_error(e),
        }

        match exponent._shr(&one) {
            Ok(nx) => exponent = nx,
            Err(e) => return math_error(e),
        }
    }

    Value::Int(result)
}

fn factorial_fn(args: &HashMap<String, Value>) -> Value {
    let n = match get_int_arg(args, "n") {
        Ok(v) => v,
        Err(e) => return e,
    };

    if n.is_negative() {
        return Value::Error("ValueError", "factorial not defined for negative values", None);
    }

    let mut result = Int::from(1);
    let mut i = Int::from(2);

    while i <= n {
        match result._mul(&i) {
            Ok(r) => result = r,
            Err(e) => return math_error(e),
        }
        match i._add(&Int::from(1)) {
            Ok(nx) => i = nx,
            Err(e) => return math_error(e),
        }
    }

    Value::Int(result)
}

fn binom_fn(args: &HashMap<String, Value>) -> Value {
    let n = match get_int_arg(args, "n") {
        Ok(v) => v,
        Err(e) => return e,
    };
    let k = match get_int_arg(args, "k") {
        Ok(v) => v,
        Err(e) => return e,
    };

    if n.is_negative() || k.is_negative() {
        return Value::Error("ValueError", "binomial coefficients require non-negative integers", None);
    }

    if n < k {
        return Value::Int(Int::from(0));
    }

    let nk = match n._sub(&k) {
        Ok(v) => v,
        Err(e) => return math_error(e),
    };
    let kk = if k > nk { nk } else { k };

    if kk.is_zero() {
        return Value::Int(Int::from(1));
    }

    let mut res = Int::from(1);
    let mut i = Int::from(1);
    while i <= kk {
        match n._sub(&kk) {
            Ok(tmp) => match tmp._add(&i) {
                Ok(numer) => match res._mul(&numer) {
                    Ok(prod) => match prod._div(&i) {
                        Ok(divd) => res = divd,
                        Err(e) => return math_error(e),
                    },
                    Err(e) => return math_error(e),
                },
                Err(e) => return math_error(e),
            },
            Err(e) => return math_error(e),
        }

        match i._add(&Int::from(1)) {
            Ok(nx) => i = nx,
            Err(e) => return math_error(e),
        }
    }

    Value::Int(res)
}

fn is_prime_fn(args: &HashMap<String, Value>) -> Value {
    let n = match get_int_arg(args, "n") {
        Ok(v) => v,
        Err(e) => return e,
    };

    if n <= Int::from(1) {
        return Value::Boolean(false);
    }

    for p in SMALL_PRIMES.iter() {
        let pi = Int::from(*p);
        if n == pi {
            return Value::Boolean(true);
        }
        match n._modulo(&pi) {
            Ok(r) => if r.is_zero() { return Value::Boolean(false); },
            Err(e) => return math_error(e),
        }
    }

    let bases = [2i64, 325, 9375, 28178, 450775, 9780504, 1795265022];

    let one = Int::from(1);
    let two = Int::from(2);
    let mut d = match n._sub(&one) { Ok(v) => v, Err(e) => return math_error(e) };
    let mut s: usize = 0;
    loop {
        match d._modulo(&two) {
            Ok(rem) => {
                if rem.is_zero() {
                    match d._div(&two) {
                        Ok(nd) => { d = nd; s += 1; }
                        Err(e) => return math_error(e),
                    }
                } else { break; }
            }
            Err(e) => return math_error(e),
        }
    }

    for a64 in bases.iter() {
        let a = Int::from(*a64);
        if a >= n { continue; }

        let mut x = match modpow_int(&a, &d, &n) {
            Ok(v) => v,
            Err(e) => return math_error(e),
        };
        if x == one || x == match n._sub(&one) { Ok(v) => v, Err(e) => return math_error(e) } {
            continue;
        }

        let mut composite = true;
        for _ in 1..s {
            match x._mul(&x) {
                Ok(xx) => match xx._modulo(&n) {
                    Ok(mx) => { x = mx; }
                    Err(e) => return math_error(e),
                },
                Err(e) => return math_error(e),
            }
            if x == match n._sub(&one) { Ok(v) => v, Err(e) => return math_error(e) } {
                composite = false;
                break;
            }
        }

        if composite { return Value::Boolean(false); }
    }

    Value::Boolean(true)
}

fn modpow_int(base: &Int, exp: &Int, modu: &Int) -> Result<Int, i8> {
    let mut result = Int::from(1);
    let mut b = base._modulo(modu)?;
    let mut e = exp.clone();
    let one = Int::from(1);
    let zero = Int::from(0);
    while !e.is_zero() {
        let rem = e._bitand(&one)?;
        if rem != zero {
            result = result._mul(&b)?._modulo(modu)?;
        }
        b = b._mul(&b)?._modulo(modu)?;
        e = e._shr(&one)?;
    }
    Ok(result)
}

pub fn approx_pi(precision: usize) -> Float {
    let mut a = Float::from(1.0);
    let mut b = (Float::from(1.0) / Float::from(2.0)).expect("Failed divide").sqrt().expect("Failed sqrt");
    let mut t = (Float::from(1.0) / Float::from(4.0)).expect("Failed divide");
    let mut p = Float::from(1.0);

    let iterations = ((precision as f64) * 0.07).ceil() as usize + 10;

    for _ in 0..iterations {
        let a_next = ((&a + &b).expect("Failed add") / Float::from(2.0)).expect("Failed divide");
        let b_next = (&a * &b).expect("Failed multiply").sqrt().expect("Failed sqrt");
        let diff = (&a - &a_next).expect("Failed subtract");
        let t_next = (&t - &(&p * diff.pow(&Float::from(2.0)).expect("Failed pow")).expect("Failed multiply")).expect("Failed subtract");

        a = a_next;
        b = b_next;
        t = t_next;
        p = (&p * Float::from(2.0)).expect("Failed multiply");
    }

    ((&a + &b).expect("Failed add").pow(&Float::from(2.0)).expect("Failed pow")
        / (&t * Float::from(4.0)).expect("Failed multiply")).expect("Failed divide")
}

pub fn approx_e(precision: usize) -> Float {
    let mut sum = Float::from(1.0);
    let mut factorial = Float::from(1.0);

    let terms = (precision as f64 * 2.5).ceil() as usize;

    for n in 1..=terms {
        factorial = (&factorial * Float::from(n as f64)).expect("Failed to multiply");
        sum = (&sum + (Float::from(1.0) / &factorial).expect("Failed to divide")).expect("Failed to add");
    }

    sum
}

pub fn approx_phi() -> Float {
    ((Float::from(1.0) + Float::from(5.0).sqrt().expect("Square root failed")).expect("Failed to add") / Float::from(2.0)).expect("Failed to divide")
}

fn approx(args: &HashMap<String, Value>) -> Value {
    let a = match args.get("a") {
        Some(Value::Float(f)) => f.clone(),
        Some(Value::Int(i)) => match Int::to_float(i) {
            Ok(f) => f,
            Err(e) => return math_error(e),
        },
        _ => return Value::Error("TypeError", "expected int or float", None),
    };
    let b = match args.get("b") {
        Some(Value::Float(f)) => f.clone(),
        Some(Value::Int(i)) => match Int::to_float(i) {
            Ok(f) => f,
            Err(e) => return math_error(e),
        },
        _ => return Value::Error("TypeError", "expected int or float", None),
    };
    let epsilon: f64 = match args.get("epsilon") {
        Some(Value::Float(f)) => match f.clone().to_f64() {
            Ok(v) => v,
            Err(e) => return math_error(e),
        },
        _ => return Value::Error("TypeError", "expected int or float", None),
    };
    Value::Boolean(a.approx_eq(&b, epsilon))
}

fn delta(args: &HashMap<String, Value>) -> Value {
    let a = match args.get("a") {
        Some(Value::Float(f)) => f.clone(),
        Some(Value::Int(i)) => match Int::to_float(i) {
            Ok(f) => f,
            Err(e) => return math_error(e),
        },
        _ => return Value::Error("TypeError", "expected int or float", None),
    };
    let b = match args.get("b") {
        Some(Value::Float(f)) => f.clone(),
        Some(Value::Int(i)) => match Int::to_float(i) {
            Ok(f) => f,
            Err(e) => return math_error(e),
        },
        _ => return Value::Error("TypeError", "expected int or float", None),
    };
    Value::Boolean(a == b)
}

fn max_handler(args: &HashMap<String, Value>) -> Value {
    let a = match args.get("a") {
        Some(Value::Float(f)) => f.clone(),
        Some(Value::Int(i)) => match Int::to_float(i) {
            Ok(f) => f,
            Err(e) => return math_error(e),
        },
        _ => return Value::Error("TypeError", "expected int or float", None),
    };
    let b = match args.get("b") {
        Some(Value::Float(f)) => f.clone(),
        Some(Value::Int(i)) => match Int::to_float(i) {
            Ok(f) => f,
            Err(e) => return math_error(e),
        },
        _ => return Value::Error("TypeError", "expected int or float", None),
    };
    if a > b {
        Value::Float(a)
    } else {
        Value::Float(b)
    }
}

fn min_handler(args: &HashMap<String, Value>) -> Value {
    let a = match args.get("a") {
        Some(Value::Float(f)) => f.clone(),
        Some(Value::Int(i)) => match Int::to_float(i) {
            Ok(f) => f,
            Err(e) => return math_error(e),
        },
        _ => return Value::Error("TypeError", "expected int or float", None),
    };
    let b = match args.get("b") {
        Some(Value::Float(f)) => f.clone(),
        Some(Value::Int(i)) => match Int::to_float(i) {
            Ok(f) => f,
            Err(e) => return math_error(e),
        },
        _ => return Value::Error("TypeError", "expected int or float", None),
    };
    if a < b {
        Value::Float(a)
    } else {
        Value::Float(b)
    }
}

pub fn register() -> HashMap<String, Variable> {
    let mut map = HashMap::new();

    let funcs: Vec<(&str, fn(&HashMap<String, Value>) -> Value)> = vec![
        ("sqrt", sqrt),
        ("sin", sin),
        ("cos", cos),
        ("tan", tan),
        ("ln", ln),
        ("log10", log10),
        ("abs", abs),
        ("exp", exp),
        ("floor", floor),
        ("ceil", ceil),
        ("round", round),
    ];

    let int_float_type = parse_type("int | float");

    for (name, func) in funcs {
        insert_native_fn!(
            map,
            name,
            func,
            vec![Parameter::positional_pt("x", &int_float_type)],
            "float",
            EffectFlags::PURE
        );
    }

    insert_native_fn!(
        map,
        "log",
        log,
        vec![Parameter::positional_pt("x", &int_float_type), Parameter::positional_optional_pt("base", &int_float_type, Value::Float(Float::from_f64(10.0)))],
        "float",
        EffectFlags::PURE
    );
    insert_native_fn!(
        map,
        "conj",
        conj,
        vec![Parameter::positional_pt("x", &int_float_type)],
        "float",
        EffectFlags::PURE
    );
    insert_native_fn!(
        map,
        "powf",
        pow,
        vec![Parameter::positional("x", "float"), Parameter::positional("y", "float")],
        "float",
        EffectFlags::PURE
    );
    insert_native_fn!(
        map,
        "powi",
        pow,
        vec![Parameter::positional("x", "int"), Parameter::positional("y", "int")],
        "int",
        EffectFlags::PURE
    );
    insert_native_fn!(
        map,
        "pow",
        pow,
        vec![Parameter::positional_pt("x", &int_float_type), Parameter::positional_pt("y", &int_float_type)],
        "any",
        EffectFlags::PURE
    );
    insert_native_fn!(
        map,
        "sign",
        sign,
        vec![Parameter::positional_pt("x", &int_float_type)],
        "int",
        EffectFlags::PURE
    );

    insert_native_fn!(
        map,
        "gcd",
        gcd_fn,
        vec![Parameter::positional("a", "int"), Parameter::positional("b", "int")],
        "int",
        EffectFlags::PURE
    );
    insert_native_fn!(
        map,
        "lcm",
        lcm_fn,
        vec![Parameter::positional("a", "int"), Parameter::positional("b", "int")],
        "int",
        EffectFlags::PURE
    );
    insert_native_fn!(
        map,
        "divmod",
        divmod_fn,
        vec![Parameter::positional("a", "int"), Parameter::positional("b", "int")],
        "tuple",
        EffectFlags::PURE
    );
    insert_native_fn!(
        map,
        "modpow",
        modpow_fn,
        vec![Parameter::positional("base", "int"), Parameter::positional("exp", "int"), Parameter::positional("mod", "int")],
        "int",
        EffectFlags::PURE
    );
    insert_native_fn!(
        map,
        "factorial",
        factorial_fn,
        vec![Parameter::positional("n", "int")],
        "int",
        EffectFlags::PURE
    );
    insert_native_fn!(
        map,
        "binom",
        binom_fn,
        vec![Parameter::positional("n", "int"), Parameter::positional("k", "int")],
        "int",
        EffectFlags::PURE
    );
    insert_native_fn!(
        map,
        "is_prime",
        is_prime_fn,
        vec![Parameter::positional("n", "int")],
        "bool",
        EffectFlags::PURE
    );

    macro_rules! insert_irrational {
        ($map:expr, $name:expr, $val:expr) => {{
            let mut var = create_float_constant($name, $val);
            var.value = match var.value {
                Value::Float(mut f) => Value::Float(f.make_irrational()),
                v => v,
            };
            insert_native_var!($map, $name, var.value, "float");
        }};
    }

    insert_native_fn!(
        map,
        "pi_approx",
        |args: &HashMap<String, Value>| {
            let precision = match args.get("precision") {
                Some(Value::Int(i)) => i.to_usize().unwrap_or(53),
                Some(Value::Float(f)) => f.to_int().unwrap_or(Int::new()).to_usize().unwrap_or(53),
                _ => 53,
            };
            Value::Float(approx_pi(precision))
        },
        vec![Parameter::positional_optional_pt("precision", &parse_type("int"), Value::Int(Int::from(53)))],
        "float",
        EffectFlags::PURE
    );

    insert_native_fn!(
        map,
        "e_approx",
        |args: &HashMap<String, Value>| {
            let precision = match args.get("precision") {
                Some(Value::Int(i)) => i.to_usize().unwrap_or(53),
                Some(Value::Float(f)) => f.to_int().unwrap_or(Int::new()).to_usize().unwrap_or(53),
                _ => 53,
            };
            Value::Float(approx_e(precision))
        },
        vec![Parameter::positional_optional_pt("precision", &parse_type("int"), Value::Int(Int::from(53)))],
        "float",
        EffectFlags::PURE
    );

    insert_native_fn!(
        map,
        "phi_approx",
        |_: &HashMap<String, Value>| {
            Value::Float(approx_phi())
        },
        vec![],
        "float",
        EffectFlags::PURE
    );

    insert_native_fn!(
        map,
        "approx",
        approx,
        vec![
            Parameter::positional_pt("a", &int_float_type),
            Parameter::positional_pt("b", &int_float_type),
            Parameter::positional_optional_pt("epsilon", &int_float_type, Value::Float(Float::from_f64(1e-10))),
        ],
        "bool",
        EffectFlags::PURE
    );
    insert_native_fn!(
        map,
        "delta",
        delta,
        vec![
            Parameter::positional_pt("a", &int_float_type),
            Parameter::positional_pt("b", &int_float_type),
        ],
        "bool",
        EffectFlags::PURE
    );
    insert_native_fn!(
        map,
        "max",
        max_handler,
        vec![
            Parameter::positional_pt("a", &int_float_type),
            Parameter::positional_pt("b", &int_float_type),
        ],
        "float",
        EffectFlags::PURE
    );
    insert_native_fn!(
        map,
        "min",
        min_handler,
        vec![
            Parameter::positional_pt("a", &int_float_type),
            Parameter::positional_pt("b", &int_float_type),
        ],
        "float",
        EffectFlags::PURE
    );
    
    insert_irrational!(map, "E", _E);
    insert_irrational!(map, "PI", _PI);
    insert_irrational!(map, "GOLDEN_RATIO", _GOLDEN_RATIO);
    insert_irrational!(map, "TAU", _TAU);
    insert_irrational!(map, "EULER_MASCHERONI", _EULER_MASCHERONI);
    insert_irrational!(map, "APERY", _APERY);
    insert_irrational!(map, "SQRT2", _SQRT2);
    insert_irrational!(map, "LN2", _LN2);
    insert_irrational!(map, "ZETA2", _ZETA2);
    insert_irrational!(map, "CATALAN", _CATALAN);

    insert_native_var!(map, "U8MAX", Value::Int(Int::from(u8::MAX)), "int");
    insert_native_var!(map, "U8MIN", Value::Int(Int::from(u8::MIN)), "int");
    insert_native_var!(map, "U16MAX", Value::Int(Int::from(u16::MAX)), "int");
    insert_native_var!(map, "U16MIN", Value::Int(Int::from(u16::MIN)), "int");
    insert_native_var!(map, "U32MAX", Value::Int(Int::from(u32::MAX)), "int");
    insert_native_var!(map, "U32MIN", Value::Int(Int::from(u32::MIN)), "int");
    insert_native_var!(map, "U64MAX", Value::Int(Int::from(u64::MAX)), "int");
    insert_native_var!(map, "U64MIN", Value::Int(Int::from(u64::MIN)), "int");
    insert_native_var!(map, "U128MAX", Value::Int(Int::from(u128::MAX)), "int");
    insert_native_var!(map, "U128MIN", Value::Int(Int::from(u128::MIN)), "int");

    insert_native_var!(map, "I8MAX", Value::Int(Int::from(i8::MAX)), "int");
    insert_native_var!(map, "I8MIN", Value::Int(Int::from(i8::MIN)), "int");
    insert_native_var!(map, "I16MAX", Value::Int(Int::from(i16::MAX)), "int");
    insert_native_var!(map, "I16MIN", Value::Int(Int::from(i16::MIN)), "int");
    insert_native_var!(map, "I32MAX", Value::Int(Int::from(i32::MAX)), "int");
    insert_native_var!(map, "I32MIN", Value::Int(Int::from(i32::MIN)), "int");
    insert_native_var!(map, "I64MAX", Value::Int(Int::from(i64::MAX)), "int");
    insert_native_var!(map, "I64MIN", Value::Int(Int::from(i64::MIN)), "int");
    insert_native_var!(map, "I128MAX", Value::Int(Int::from(i128::MAX)), "int");
    insert_native_var!(map, "I128MIN", Value::Int(Int::from(i128::MIN)), "int");

    insert_native_var!(map, "F32MAX", Value::Float(Float::from(f32::MAX)), "float");
    insert_native_var!(map, "F32MIN", Value::Float(Float::from(f32::MIN)), "float");
    insert_native_var!(map, "F64MAX", Value::Float(Float::from(f64::MAX)), "float");
    insert_native_var!(map, "F64MIN", Value::Float(Float::from(f64::MIN)), "float");

    map
}
