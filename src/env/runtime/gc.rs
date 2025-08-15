use crate::env::runtime::value::Value;
use crate::env::runtime::variables::Variable;
use std::collections::HashMap;

pub struct GC<'a> {
    variables: &'a mut HashMap<String, Variable>,
}

impl<'a> GC<'a> {
    pub fn new(vars: &'a mut HashMap<String, Variable>) -> Self {
        Self { variables: vars }
    }

    pub fn sweep(&mut self, roots: &[String]) {
        self.mark_from_roots(roots);
        self.remove_unmarked();
    }

    fn mark_from_roots(&mut self, roots: &[String]) {
        let mut worklist = roots.to_vec();

        while let Some(key) = worklist.pop() {
            let children = {
                if let Some(var) = self.variables.get_mut(&key) {
                    if var.marked {
                        continue;
                    }
                    var.marked = true;

                    var.value.clone()
                } else {
                    continue;
                }
            };

            let mut new_keys = Vec::new();
            self.collect_from_value(&children, &mut new_keys);
            worklist.extend(new_keys);
        }
    }

    fn collect_children(&self, var: &Variable) -> Vec<String> {
        let mut out = Vec::new();
        self.collect_from_value(&var.value, &mut out);
        out
    }

    fn collect_from_value(&self, val: &Value, out: &mut Vec<String>) {
        match val {
            Value::Tuple(xs) | Value::List(xs) => {
                for v in xs {
                    self.collect_from_value(v, out);
                }
            }
            Value::Map { keys, values } => {
                for k in keys {
                    self.collect_from_value(k, out);
                }
                for v in values {
                    self.collect_from_value(v, out);
                }
            }
            Value::Pointer(p) => {
                self.collect_from_value(&*p, out);
            }
            _ => {}
        }
    }

    fn remove_unmarked(&mut self) {
        self.variables.retain(|_, v| {
            let keep = v.is_static() || v.marked;
            if keep {
                v.marked = false; // reset for next run
            }
            keep
        });
    }
}
