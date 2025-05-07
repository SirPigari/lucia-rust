use std::collections::HashMap;
use crate::env::helpers::config::{Config, CodeBlocks, ColorScheme};
use crate::env::helpers::utils::{print_colored, hex_to_ansi, Value, Error};
use lazy_static::lazy_static;

pub struct Interpreter {
    config: Config,
}

impl Interpreter {
    pub fn new(config: Config) -> Self {
        Interpreter { config }
    }

    pub fn interpret(&self, statements: Vec<Value>) {
        for statement in statements {
            match statement {
                Value::String(s) => {
                    println!("{}", s);
                }
                _ => {
                    // Handle other types of statements if needed
                }
            }
        }
    }
}
