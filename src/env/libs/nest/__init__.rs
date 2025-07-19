// TODO
// Implement the 'nest' module with HTTP client and server utilities
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use crate::env::runtime::config::{Config, get_from_config};
use crate::env::runtime::errors::Error;
use crate::env::runtime::variables::Variable;
use crate::interpreter::Interpreter;
use crate::env::runtime::utils::to_static;


pub fn register(_interpreter: Arc<Mutex<&mut Interpreter>>) -> HashMap<String, Variable> {
    let map = HashMap::new();

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