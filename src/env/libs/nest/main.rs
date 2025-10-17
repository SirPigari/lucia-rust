// TODO
// Implement the 'nest' module with HTTP client and server utilities
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use crate::env::runtime::config::{Config, get_from_config};
use crate::env::runtime::errors::Error;
use crate::env::runtime::variables::Variable;
use crate::env::runtime::statements::Statement;
use crate::env::runtime::value::Value;
use crate::interpreter::Interpreter;
use crate::env::runtime::types::Type;
use crate::env::runtime::utils::to_static;
use once_cell::sync::Lazy;

use crate::insert_native_var;

static SERVER_STRUCT_TYPE: Lazy<Type> = Lazy::new(|| Type::Struct {
    name: "HttpServer".to_string(),
    fields: vec![
        ("addr".to_string(), Statement::make_value(Value::Type(Type::new_simple("str"))), vec![])
    ],
    methods: vec![
        
    ],
    generics: Vec::new(),
    wheres: Vec::new(),
});

static RESPONSE_ENUM_TYPE: Lazy<Type> = Lazy::new(|| Type::Enum {
    name: "Response".to_string(),
    variants: vec![
        ("Text".to_string(), Statement::make_value(Value::Type(Type::new_simple("str"))), 0),
        ("Json".to_string(), Statement::make_value(Value::Type(Type::new_simple("map"))), 1),
        ("Bytes".to_string(), Statement::make_value(Value::Type(Type::new_simple("bytes"))), 2),
        ("Status".to_string(), Statement::make_value(Value::Type(Type::new_simple("int"))), 3),
        ("Custom".to_string(), Statement::make_value(Value::Type(Type::new_simple("map"))), 4)
    ],
    generics: Vec::new(),
    wheres: Vec::new(),
});


pub fn register(_interpreter: Arc<Mutex<&mut Interpreter>>) -> HashMap<String, Variable> {
    let mut map = HashMap::new();

    insert_native_var!(map, "HttpServer", Value::Type(SERVER_STRUCT_TYPE.clone()), "type");
    insert_native_var!(map, "Response", Value::Type(RESPONSE_ENUM_TYPE.clone()), "type");

    map
}

pub fn init_nest(
    _config: Arc<Config>,
    file_path: String,
) -> Result<(), Error> {
    return Err(
        Error::new(
            "NotImplemented",
            "Nest module is not implemented yet.",
            to_static(file_path),
        )
    );
    #[allow(unreachable_code)]
    if !get_from_config(&_config, "allow_fetch").is_truthy() {
        return Err(Error::with_help(
            "FetchError",
            "Nest requires 'allow_fetch' = true in config",
            "Set 'allow_fetch' = true in config.",
            to_static(file_path),
        ));
    }

    Ok(())
}