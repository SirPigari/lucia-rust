use crate::Config;

pub struct Interpreter {
    config: Config,
}

impl Interpreter {
    pub fn new(config: Config) -> Self {
        Interpreter { config }
    }

    pub fn interpret(&self, statements: Vec<String>) {
        for statement in statements {
            println!("Executing: {}", statement);
        }
    }
}
