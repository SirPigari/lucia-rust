fn call_function(&mut self, function_name: &str, pos_args: Vec<String>, named_args: HashMap<String, Value>) -> Value {
    match self.variables.get(function_name) {
        Some(var) => {
            let value = var.get_value();
            match value {
                Value::Function(func) => {
                    if let Some(state) = func.metadata().state.as_ref() {
                        if state == "deprecated" {
                            println!("Warning: Function '{}' is deprecated", function_name);
                        } else if let Some(alt_name) = state.strip_prefix("removed: ") {
                            return self.raise(
                                "NameError",
                                &format!(
                                    "Function '{}' has been removed. Use '{}' instead.",
                                    function_name, alt_name
                                ),
                            );
                        }
                    }

                    let result = func.call(args);
                    if let Value::Error(err_type, err_msg) = &result {
                        return self.raise(err_type, err_msg);
                    }

                    result
                }
                other => {
                    self.raise_with_help(
                        "TypeError",
                        &format!("'{}' is not callable", function_name),
                        &format!("Expected a Function, but got: {}", other.to_string()),
                    )
                }
            }
        }
        None => {
            let available_names: Vec<String> = self.variables.keys().cloned().collect();
            if let Some(closest) = find_closest_match(function_name, &available_names) {
                self.raise_with_help(
                    "NameError",
                    &format!("Function '{}' is not defined", function_name),
                    &format!("Did you mean '{}{}{}'?", check_ansi("\x1b[4m", &self.use_colors), closest, check_ansi("\x1b[24m", &self.use_colors)),
                )
            } else {
                self.raise("NameError", &format!("Function '{}' is not defined", function_name))
            }
        }
    }
}