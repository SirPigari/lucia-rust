use std::collections::HashMap;
use std::sync::Arc;
use crate::env::runtime::functions::{Function, Parameter};
use crate::env::runtime::types::{Float, Int};
use crate::env::runtime::value::Value;
use crate::env::runtime::utils::{get_imagnum_error_message, parse_type};
use crate::env::runtime::internal_structs::EffectFlags;
use crate::env::runtime::variables::Variable;

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

    map
}
