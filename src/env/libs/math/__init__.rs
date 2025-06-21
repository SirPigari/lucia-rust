use std::collections::HashMap;
use std::sync::Arc;
use crate::env::runtime::functions::{Function, NativeFunction, Parameter};
use crate::env::runtime::types::{Float, Int};
use crate::env::runtime::value::Value;
use crate::env::runtime::utils::{get_imagnum_error_message, to_static};
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

fn math_error(err_id: i16) -> Value {
    let msg = get_imagnum_error_message(err_id);
    Value::Error("MathError", to_static(msg))
}

fn create_float_constant(name: &str, value: &str) -> Variable {
    Variable::new(
        name.to_string(),
        match Float::from_str(value) {
            Ok(num) => Value::Float(num),
            Err(_) => Value::Error("RuntimeError", "Invalid float format"),
        },
        "float".to_string(),
        true,
        true,
        true,
    )
}

fn float_unary<F>(args: &HashMap<String, Value>, f: F) -> Value
where
    F: Fn(f64) -> f64,
{
    match args.get("x") {
        Some(Value::Float(v)) => match v.to_f64() {
            Ok(n) => Value::Float(Float::from_f64(f(n))),
            Err(e) => math_error(e),
        },
        Some(Value::Int(i)) => match i.to_i64() {
            Ok(n) => Value::Float(Float::from_f64(f(n as f64))),
            Err(e) => math_error(e),
        },
        _ => Value::Error("TypeError", "expected a float or int"),
    }
}

fn log_base(args: &HashMap<String, Value>) -> Value {
    let x_val = args.get("x");
    let base_val = args.get("base");

    match (x_val, base_val) {
        (Some(Value::Float(x)), Some(Value::Float(b))) => {
            match (x.to_f64(), b.to_f64()) {
                (Ok(xf), Ok(bf)) if xf > 0.0 && bf > 0.0 && bf != 1.0 => {
                    Value::Float(Float::from_f64(xf.log(bf)))
                }
                _ => Value::Error("MathError", "Invalid base or x for log"),
            }
        }
        _ => Value::Error("TypeError", "log(x, base) expects float args"),
    }
}

fn sqrt(args: &HashMap<String, Value>) -> Value { float_unary(args, f64::sqrt) }
fn sin(args: &HashMap<String, Value>) -> Value { float_unary(args, f64::sin) }
fn cos(args: &HashMap<String, Value>) -> Value { float_unary(args, f64::cos) }
fn tan(args: &HashMap<String, Value>) -> Value { float_unary(args, f64::tan) }
fn ln(args: &HashMap<String, Value>) -> Value { float_unary(args, f64::ln) }
fn log10(args: &HashMap<String, Value>) -> Value { float_unary(args, f64::log10) }
fn abs(args: &HashMap<String, Value>) -> Value { float_unary(args, f64::abs) }
fn exp(args: &HashMap<String, Value>) -> Value { float_unary(args, f64::exp) }
fn floor(args: &HashMap<String, Value>) -> Value { float_unary(args, f64::floor) }
fn ceil(args: &HashMap<String, Value>) -> Value { float_unary(args, f64::ceil) }
fn round(args: &HashMap<String, Value>) -> Value { float_unary(args, f64::round) }

fn create_native_fn(
    name: &str,
    handler: fn(&HashMap<String, Value>) -> Value,
    params: Vec<Parameter>,
    ret_type: &str,
) -> Function {
    Function::Native(Arc::new(NativeFunction::new(
        name,
        handler,
        params,
        ret_type,
        true,
        true,
        true,
        None,
    )))
}

pub fn register() -> HashMap<String, Variable> {
    let mut map = HashMap::new();

    let funcs: Vec<(&str, fn(&HashMap<String, Value>) -> Value)> = vec![
        ("sqrt", sqrt),
        ("sin", sin),
        ("cos", cos),
        ("tan", tan),
        ("ln", ln),
        ("log", log10),
        ("abs", abs),
        ("exp", exp),
        ("floor", floor),
        ("ceil", ceil),
        ("round", round),
    ];

    for (name, func) in funcs {
        insert_native_fn!(
            map,
            name,
            func,
            vec![Parameter::positional("x", "float")],
            "float"
        );
    }

    insert_native_fn!(
        map,
        "log_base",
        log_base,
        vec![Parameter::positional("x", "float"), Parameter::positional("base", "float")],
        "float"
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

pub fn init() -> Value {
    Value::Null
}
