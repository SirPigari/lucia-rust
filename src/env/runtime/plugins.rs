use std::path::{Path, PathBuf};
use std::collections::HashMap;

use serde_json::Value as JsonValue;

use crate::interpreter::Interpreter;
use crate::lexer::Lexer;
use crate::parser::Parser;

use crate::env::runtime::errors::Error;
use crate::env::runtime::config::Config;
use crate::env::runtime::value::Value;
use crate::env::runtime::functions::Function;
use crate::env::runtime::utils::to_static;
use bincode::{Encode, Decode};
use serde::{Serialize, Deserialize};

#[derive(Debug, Clone, Serialize, Deserialize, Encode, Decode)]
pub struct Plugin {
    interpreter: Interpreter,
    execute: Function,
    cleanup: Option<Function>,
}

impl PartialEq for Plugin {
    fn eq(&self, other: &Self) -> bool {
        self.execute == other.execute && self.cleanup == other.cleanup
    }
}

impl Plugin {
    fn cleanup(&mut self) {
        if let Some(cleanup) = &self.cleanup {
            let _ = self.interpreter.call_function(
                cleanup,
                vec![],
                HashMap::new(),
                None,
            );
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Encode, Decode)]
pub struct PluginRuntime {
    plugins: Vec<Plugin>,
}

impl PluginRuntime {
    pub fn new() -> Self {
        Self {
            plugins: Vec::new(),
        }
    }

    pub fn load_plugins(&mut self, dir: &Path, config: Config) -> Vec<Error> {
        let mut plugins: Vec<(PathBuf, JsonValue)> = Vec::new();
        let mut errors: Vec<Error> = Vec::new();

        if let Ok(entries) = dir.read_dir() {
            for entry in entries.flatten() {
                let path = entry.path();
                if !path.is_dir() {
                    continue;
                }

                let manifest_path = path.join("manifest.json");
                if !manifest_path.exists() {
                    continue;
                }

                let Ok(manifest_content) = std::fs::read_to_string(&manifest_path) else {
                    continue;
                };

                let Ok(manifest_json) = serde_json::from_str::<JsonValue>(&manifest_content) else {
                    continue;
                };

                let Some(lib_type) = manifest_json.get("type").and_then(|v| v.as_str()) else {
                    continue;
                };

                if lib_type != "plugin" {
                    continue;
                }

                plugins.push((path.clone(), manifest_json));
            }
        }

        for (plugin_path, manifest_json) in plugins {
            let main_file = manifest_json
                .get("entry_point")
                .and_then(|v| v.as_str())
                .unwrap_or("main.lc");

            let main_path = plugin_path.join(main_file);
            if !main_path.exists() {
                continue;
            }

            let Ok(source_code) = std::fs::read_to_string(&main_path) else {
                continue;
            };

            let m = main_path.to_string_lossy();
            let lexer = Lexer::new(&source_code, &m);
            let tokens = lexer.tokenize();
            let mut parser = Parser::new(tokens);

            let ast = match parser.parse_safe() {
                Ok(ast) => ast,
                Err(e) => {
                    errors.push(e);
                    continue;
                }
            };

            let home_dir_path = PathBuf::from(&config.home_dir);
            let config_path = home_dir_path.join("config.json");

            let mut interpreter = Interpreter::new(
                config.clone(),
                &main_path.to_string_lossy(),
                &main_path.parent().unwrap().to_path_buf(),
                (home_dir_path.join("libs"), config_path, false),
                &[],
            );

            if let Err(e) = interpreter.interpret(ast, true) {
                errors.push(e);
                continue;
            }

            let init_fun = {
                let vars = &interpreter.variables;
                vars.get("init").and_then(|v| match v.get_value() {
                    Value::Function(f) => Some(f.clone()),
                    _ => None,
                })
            };

            if let Some(f) = init_fun {
                interpreter.call_function(&f, vec![], HashMap::new(), None);
                if let Some(err) = interpreter.err.clone() {
                    errors.push(err);
                    continue;
                }
            }

            let execute_fun = {
                let vars = &interpreter.variables;
                vars.get("execute").and_then(|v| match v.get_value() {
                    Value::Function(f) => Some(f.clone()),
                    _ => None,
                })
            };

            let cleanup_fun = {
                let vars = &interpreter.variables;
                vars.get("cleanup").and_then(|v| match v.get_value() {
                    Value::Function(f) => Some(f.clone()),
                    _ => None,
                })
            };

            let Some(execute) = execute_fun else {
                errors.push(Error::new_anonymous(
                    "PluginError",
                    to_static(format!(
                        "Plugin at '{}' does not have an 'execute' function.",
                        main_path.display()
                    )),
                ));
                continue;
            };

            self.plugins.push(Plugin {
                interpreter,
                execute,
                cleanup: cleanup_fun,
            });
        }

        errors
    }

    // TODO: Add use cases
    #[allow(dead_code)]
    pub fn call_hook(&mut self, hook_name: &str, args: Vec<Value>) -> Vec<Value> {
        let mut results: Vec<Value> = Vec::new();
        for plugin in &mut self.plugins {
            results.push(plugin.interpreter.call_function(
                &plugin.execute,
                vec![
                    Value::String(hook_name.to_owned()),
                    Value::List(args.clone()),
                ],
                HashMap::new(),
                None,
            ));
        }
        results
    }

    pub fn shutdown(&mut self) {
        for plugin in &mut self.plugins {
            plugin.cleanup();
        }
        self.plugins.clear();
    }
}

impl Drop for PluginRuntime {
    fn drop(&mut self) {
        self.shutdown();
    }
}
