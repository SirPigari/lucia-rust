use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use crate::lexer::Lexer;
use crate::env::runtime::errors::Error;

pub struct Preprocessor {
    lib_dir: PathBuf,
    config_path: PathBuf,
    defines: HashMap<String, (String, String)>, // IDENTIFIER -> (TOKEN_TYPE, TOKEN_VALUE)
    aliases: HashMap<(String, String), (String, String)>, // (TOKEN_TYPE, TOKEN_VALUE) -> (TOKEN_TYPE, TOKEN_VALUE)
    macros: HashMap<String, (Vec<(String, Option<(String, String)>)>, Vec<(String, String)>)>, // MACRO_NAME -> (ARGS, BODY -> VEC(TOKEN_TYPE, TOKEN_VALUE))
    file_path: String,
}

impl Preprocessor {
    pub fn new<P: Into<PathBuf>>(lib_dir: P, config_path: P, file_path: &str) -> Self {
        Self {
            lib_dir: lib_dir.into(),
            config_path: config_path.into(),
            defines: HashMap::new(),
            aliases: HashMap::new(),
            macros: HashMap::new(),
            file_path: file_path.to_string(),
        }
    }

    pub fn process(
        &mut self,
        tokens: Vec<(String, String)>,
        current_dir: &Path,
    ) -> Result<Vec<(String, String)>, Error> {
        let filtered_tokens: Vec<(String, String)> = tokens
            .into_iter()
            .filter(|(ttype, _)| ttype != "WHITESPACE")
            .collect();

        self._process(filtered_tokens, current_dir)
    }

    fn _process(
        &mut self,
        tokens: Vec<(String, String)>,
        current_dir: &Path,
    ) -> Result<Vec<(String, String)>, Error> {
        let mut result = Vec::new();
        let mut i = 0;
        let mut skip_stack = Vec::new();
        let mut skipping = false;

        while i < tokens.len() {
            let (ref ttype, ref tval) = tokens[i];

            if ttype == "OPERATOR" && tval == "#" {
                i += 1;
                if i >= tokens.len() {
                    return Err(Error {
                        error_type: "PreprocessorError".to_string(),
                        msg: "Unexpected end after '#'".to_string(),
                        help: None,
                        line: (0, "".to_string()),
                        column: 0,
                        file: self.file_path.clone(),
                        ref_err: None,
                    });
                }
                let directive = &tokens[i].1;
                i += 1;

                match directive.as_str() {
                    "macro" => {
                        if i >= tokens.len() {
                            return Err(Error {
                                error_type: "PreprocessorError".to_string(),
                                msg: "#macro missing NAME".to_string(),
                                help: Some("Expected an identifier after #macro".to_string()),
                                line: (0, "".to_string()),
                                column: 0,
                                file: self.file_path.clone(),
                                ref_err: None,
                            });
                        }

                        let (name_type, name_val) = &tokens[i];
                        i += 1;

                        if name_type != "IDENTIFIER" {
                            return Err(Error {
                                error_type: "PreprocessorError".to_string(),
                                msg: format!("#macro NAME must be IDENTIFIER, got {}", name_type),
                                help: None,
                                line: (0, "".to_string()),
                                column: 0,
                                file: self.file_path.clone(),
                                ref_err: None,
                            });
                        }

                        let mut args: Vec<(String, Option<Vec<(String, String)>>)> = Vec::new();

                        if i < tokens.len() && tokens[i] == ("SEPARATOR".to_string(), "(".to_string()) {
                            i += 1;

                            while i < tokens.len() && tokens[i] != ("SEPARATOR".to_string(), ")".to_string()) {
                                if tokens[i] != ("OPERATOR".to_string(), "$".to_string()) {
                                    return Err(Error {
                                        error_type: "PreprocessorError".to_string(),
                                        msg: format!("#macro argument must start with $, got {}", tokens[i].1),
                                        help: Some("Expected arguments like $arg".to_string()),
                                        line: (0, "".to_string()),
                                        column: 0,
                                        file: self.file_path.clone(),
                                        ref_err: None,
                                    });
                                }
                                i += 1;

                                if i >= tokens.len() || tokens[i].0 != "IDENTIFIER" {
                                    return Err(Error {
                                        error_type: "PreprocessorError".to_string(),
                                        msg: "Expected IDENTIFIER after $ in macro argument".to_string(),
                                        help: None,
                                        line: (0, "".to_string()),
                                        column: 0,
                                        file: self.file_path.clone(),
                                        ref_err: None,
                                    });
                                }

                                let name = tokens[i].1.clone();
                                i += 1;

                                let mut default_val = None;

                                if i + 2 < tokens.len()
                                    && tokens[i] == ("OPERATOR".to_string(), "=".to_string())
                                    && tokens[i + 1] == ("SEPARATOR".to_string(), "(".to_string())
                                {
                                    i += 2;
                                    let mut val = Vec::new();
                                    let mut depth = 1;

                                    while i < tokens.len() {
                                        if tokens[i].1 == "(" {
                                            depth += 1;
                                        } else if tokens[i].1 == ")" {
                                            depth -= 1;
                                            if depth == 0 {
                                                i += 1;
                                                break;
                                            }
                                        }

                                        val.push(tokens[i].clone());
                                        i += 1;
                                    }

                                    if depth != 0 {
                                        return Err(Error {
                                            error_type: "PreprocessorError".to_string(),
                                            msg: "Unclosed default value in macro argument".to_string(),
                                            help: Some("Expected ')' to close default value expression".to_string()),
                                            line: (0, "".to_string()),
                                            column: 0,
                                            file: self.file_path.clone(),
                                            ref_err: None,
                                        });
                                    }

                                    default_val = Some(val);
                                }

                                args.push((name, default_val));

                                if i < tokens.len() && tokens[i] == ("SEPARATOR".to_string(), ",".to_string()) {
                                    i += 1;
                                }
                            }

                            if i >= tokens.len() || tokens[i] != ("SEPARATOR".to_string(), ")".to_string()) {
                                return Err(Error {
                                    error_type: "PreprocessorError".to_string(),
                                    msg: "Unclosed argument list in #macro".to_string(),
                                    help: Some("Expected ')' to close argument list".to_string()),
                                    line: (0, "".to_string()),
                                    column: 0,
                                    file: self.file_path.clone(),
                                    ref_err: None,
                                });
                            }
                            i += 1;
                        }

                        if tokens[i].0 != "SEPARATOR" || tokens[i].1 != ":" {
                            return Err(Error {
                                error_type: "PreprocessorError".to_string(),
                                msg: "Expected ':' to start macro body".to_string(),
                                help: None,
                                line: (0, "".to_string()),
                                column: 0,
                                file: self.file_path.clone(),
                                ref_err: None,
                            });
                        }

                        i += 1;

                        let mut body_tokens = Vec::new();
                        while i < tokens.len() {
                            if tokens[i].0 == "OPERATOR" && tokens[i].1 == "#" {
                                if i + 1 < tokens.len() && tokens[i + 1].1 == "endmacro" {
                                    i += 2;
                                    break;
                                }
                            }
                            body_tokens.push(tokens[i].clone());
                            i += 1;
                        }

                        let args = args.into_iter()
                            .map(|(name, default)| {
                                let default = default.and_then(|mut v| v.into_iter().next());
                                (name, default)
                            })
                            .collect::<Vec<_>>();        

                        self.macros.insert(name_val.clone(), (args, body_tokens));

                    }

                    "define" => {
                        if i >= tokens.len() {
                            return Err(Error {
                                error_type: "PreprocessorError".to_string(),
                                msg: "#define missing NAME".to_string(),
                                help: Some("Expected an identifier after #define".to_string()),
                                line: (0, "".to_string()),
                                column: 0,
                                file: self.file_path.clone(),
                                ref_err: None,
                            });
                        }
                        let (name_type, name_val) = &tokens[i];
                        i += 1;

                        if name_type != "IDENTIFIER" {
                            return Err(Error {
                                error_type: "PreprocessorError".to_string(),
                                msg: format!("#define NAME must be IDENTIFIER, got {}", name_type),
                                help: None,
                                line: (0, "".to_string()),
                                column: 0,
                                file: self.file_path.clone(),
                                ref_err: None,
                            });
                        }

                        if i >= tokens.len() {
                            return Err(Error {
                                error_type: "PreprocessorError".to_string(),
                                msg: "#define missing VALUE".to_string(),
                                help: Some("Expected token after #define NAME".to_string()),
                                line: (0, "".to_string()),
                                column: 0,
                                file: self.file_path.clone(),
                                ref_err: None,
                            });
                        }

                        let (val_type, val_val) = &tokens[i];
                        i += 1;

                        self.defines.insert(name_val.clone(), (val_type.clone(), val_val.clone()));
                    }

                    "undef" => {
                        if i >= tokens.len() {
                            return Err(Error {
                                error_type: "PreprocessorError".to_string(),
                                msg: "#undef missing NAME".to_string(),
                                help: None,
                                line: (0, "".to_string()),
                                column: 0,
                                file: self.file_path.clone(),
                                ref_err: None,
                            });
                        }
                        let (name_type, name_val) = &tokens[i];
                        i += 1;
                        if name_type != "IDENTIFIER" {
                            return Err(Error {
                                error_type: "PreprocessorError".to_string(),
                                msg: "#undef NAME must be IDENTIFIER".to_string(),
                                help: None,
                                line: (0, "".to_string()),
                                column: 0,
                                file: self.file_path.clone(),
                                ref_err: None,
                            });
                        }
                        self.defines.remove(name_val);
                    }

                    "ifdef" => {
                        if i >= tokens.len() {
                            return Err(Error {
                                error_type: "PreprocessorError".to_string(),
                                msg: "#ifdef missing NAME".to_string(),
                                help: None,
                                line: (0, "".to_string()),
                                column: 0,
                                file: self.file_path.clone(),
                                ref_err: None,
                            });
                        }
                        let (name_type, name_val) = &tokens[i];
                        i += 1;
                        if name_type != "IDENTIFIER" {
                            return Err(Error {
                                error_type: "PreprocessorError".to_string(),
                                msg: "#ifdef NAME must be IDENTIFIER".to_string(),
                                help: None,
                                line: (0, "".to_string()),
                                column: 0,
                                file: self.file_path.clone(),
                                ref_err: None,
                            });
                        }
                        let cond = self.defines.contains_key(name_val);
                        skip_stack.push(skipping);
                        skipping = !cond;
                    }

                    "ifndef" => {
                        if i >= tokens.len() {
                            return Err(Error {
                                error_type: "PreprocessorError".to_string(),
                                msg: "#ifndef missing NAME".to_string(),
                                help: None,
                                line: (0, "".to_string()),
                                column: 0,
                                file: self.file_path.clone(),
                                ref_err: None,
                            });
                        }
                        let (name_type, name_val) = &tokens[i];
                        i += 1;
                        if name_type != "IDENTIFIER" {
                            return Err(Error {
                                error_type: "PreprocessorError".to_string(),
                                msg: "#ifndef NAME must be IDENTIFIER".to_string(),
                                help: None,
                                line: (0, "".to_string()),
                                column: 0,
                                file: self.file_path.clone(),
                                ref_err: None,
                            });
                        }
                        let cond = !self.defines.contains_key(name_val);
                        skip_stack.push(skipping);
                        skipping = !cond;
                    }

                    "endif" => {
                        skipping = skip_stack.pop().unwrap_or(false);
                    }

                    "alias" => {
                        if i + 1 >= tokens.len() {
                            return Err(Error {
                                error_type: "PreprocessorError".to_string(),
                                msg: "#alias missing tokens".to_string(),
                                help: None,
                                line: (0, "".to_string()),
                                column: 0,
                                file: self.file_path.clone(),
                                ref_err: None,
                            });
                        }
                        let from_token = tokens[i].clone();
                        i += 1;
                        let to_token = tokens[i].clone();
                        i += 1;

                        self.aliases.insert(from_token, to_token);
                    }

                    "unalias" => {
                        if i >= tokens.len() {
                            return Err(Error {
                                error_type: "PreprocessorError".to_string(),
                                msg: "#unalias missing token".to_string(),
                                help: None,
                                line: (0, "".to_string()),
                                column: 0,
                                file: self.file_path.clone(),
                                ref_err: None,
                            });
                        }
                        let token = tokens[i].clone();
                        i += 1;

                        self.aliases.remove(&token);
                    }

                    "include" => {
                        if i >= tokens.len() {
                            return Err(Error {
                                error_type: "PreprocessorError".to_string(),
                                msg: "#include missing path".to_string(),
                                help: None,
                                line: (0, "".to_string()),
                                column: 0,
                                file: self.file_path.clone(),
                                ref_err: None,
                            });
                        }

                        let included_path = if tokens[i].0 == "OPERATOR" && tokens[i].1 == "<" {
                            i += 1;
                            let mut path_parts = Vec::new();

                            while i < tokens.len() && !(tokens[i].0 == "OPERATOR" && tokens[i].1 == ">") {
                                path_parts.push(tokens[i].1.clone());
                                i += 1;
                            }

                            if i >= tokens.len() || tokens[i].1 != ">" {
                                return Err(Error {
                                    error_type: "PreprocessorError".to_string(),
                                    msg: "Unclosed '<...>' in #include".to_string(),
                                    help: Some("Expected '>' after path".to_string()),
                                    line: (0, "".to_string()),
                                    column: 0,
                                    file: self.file_path.clone(),
                                    ref_err: None,
                                });
                            }

                            i += 1;

                            let combined_path = path_parts.join("");
                            let lib_base = self.lib_dir.join(&combined_path);

                            let try_paths = [
                                lib_base.clone(),
                                lib_base.with_extension("lc"),
                                lib_base.with_extension("lucia"),
                            ];

                            let mut found_path = None;
                            for p in try_paths.iter() {
                                if p.exists() {
                                    found_path = Some(p.clone());
                                    break;
                                }
                            }

                            match found_path {
                                Some(p) => p,
                                None => lib_base,
                            }
                        } else if tokens[i].0 == "STRING" {
                            let raw = &tokens[i].1;
                            i += 1;
                            current_dir.join(raw.trim_matches('"'))
                        } else {
                            return Err(Error {
                                error_type: "PreprocessorError".to_string(),
                                msg: "Invalid include path format".to_string(),
                                help: Some("Use either <lib> or \"relative/path\" syntax".to_string()),
                                line: (0, "".to_string()),
                                column: 0,
                                file: self.file_path.clone(),
                                ref_err: None,
                            });
                        };

                        if !included_path.exists() {
                            return Err(Error {
                                error_type: "PreprocessorError".to_string(),
                                msg: format!("Included file does not exist: {}", included_path.display()),
                                help: None,
                                line: (0, "".to_string()),
                                column: 0,
                                file: self.file_path.clone(),
                                ref_err: None,
                            });
                        }

                        if included_path.is_dir() {
                            let mut entries: Vec<_> = fs::read_dir(&included_path)
                                .map_err(|e| Error {
                                    error_type: "PreprocessorIOError".to_string(),
                                    msg: format!("Failed to read directory '{}': {}", included_path.display(), e),
                                    help: None,
                                    line: (0, "".to_string()),
                                    column: 0,
                                    file: self.file_path.clone(),
                                    ref_err: None,
                                })?
                                .filter_map(Result::ok)
                                .filter(|e| {
                                    if let Some(ext) = e.path().extension() {
                                        ext == "lc" || ext == "lucia"
                                    } else {
                                        false
                                    }
                                })
                                .collect();

                            entries.sort_by_key(|e| e.path());

                            for entry in entries {
                                let file_path = entry.path();
                                let content = fs::read_to_string(&file_path).map_err(|e| Error {
                                    error_type: "PreprocessorIOError".to_string(),
                                    msg: format!("Failed to read included file '{}': {}", file_path.display(), e),
                                    help: None,
                                    line: (0, "".to_string()),
                                    column: 0,
                                    file: self.file_path.clone(),
                                    ref_err: None,
                                })?;

                                let lexer = Lexer::new(&content);
                                let mut toks = lexer.tokenize(false)
                                    .into_iter()
                                    .filter(|(t, _)| t != "WHITESPACE")
                                    .collect::<Vec<_>>();                                
                                if let Some(last_token) = toks.last() {
                                    if last_token.0 != "EOF" {
                                        return Err(Error {
                                            error_type: "PreprocessorError".to_string(),
                                            msg: format!("File '{}' not closed properly", file_path.display()),
                                            help: Some("Probably corrupted.".to_string()),
                                            line: (0, "".to_string()),
                                            column: 0,
                                            file: self.file_path.clone(),
                                            ref_err: None,
                                        });
                                    } else {
                                        toks.pop();
                                    }
                                }

                                let included = self._process(toks, file_path.parent().unwrap_or(current_dir))?;
                                result.extend(included);
                            }

                            continue;
                        } else {
                            let content = fs::read_to_string(&included_path).map_err(|e| Error {
                                error_type: "PreprocessorIOError".to_string(),
                                msg: format!("Failed to read included file '{}': {}", included_path.display(), e),
                                help: None,
                                line: (0, "".to_string()),
                                column: 0,
                                file: self.file_path.clone(),
                                ref_err: None,
                            })?;

                            let lexer = Lexer::new(&content);
                            let mut toks = lexer.tokenize(false)
                                .into_iter()
                                .filter(|(t, _)| t != "WHITESPACE")
                                .collect::<Vec<_>>();                            
                            if toks.last().map_or(false, |t| t.0 == "EOF") {
                                toks.pop();
                            }

                            let included = self._process(toks, included_path.parent().unwrap_or(current_dir))?;
                            result.extend(included);
                        }
                    }

                    "config" => {
                        if i + 1 >= tokens.len() {
                            return Err(Error {
                                error_type: "PreprocessorError".to_string(),
                                msg: "#config requires a key and a value".to_string(),
                                help: Some("Use like: #config KEY VALUE".to_string()),
                                line: (0, "".to_string()),
                                column: 0,
                                file: self.file_path.clone(),
                                ref_err: None,
                            });
                        }

                        let key = tokens[i].1.clone();
                        i += 1;
                        let value = tokens[i].clone();
                        i += 1;

                        result.push(("IDENTIFIER".to_string(), "set_cfg".to_string()));
                        result.push(("SEPARATOR".to_string(), "(".to_string()));
                        result.push(("STRING".to_string(), format!("\"{}\"", key)));
                        result.push(("SEPARATOR".to_string(), ",".to_string()));
                        result.push(value);
                        result.push(("SEPARATOR".to_string(), ")".to_string()));

                        // return Err(Error {
                        //     error_type: "Unimplemented".to_string(),
                        //     msg: "set_cfg() is not implemented yet".to_string(),
                        //     help: Some("This feature will be added later".to_string()),
                        //     line: (0, "".to_string()),
                        //     column: 0,
                        //     file: self.file_path.clone(),
                        //     ref_err: None,
                        // });
                    }
                    _ => {
                        return Err(Error {
                            error_type: "PreprocessorError".to_string(),
                            msg: format!("Unknown preprocessor directive: {}", directive),
                            help: Some("Valid directives are: define, undef, ifdef, ifndef, endif, alias, unalias, include, config, macro and endmacro".to_string()),
                            line: (0, "".to_string()),
                            column: 0,
                            file: self.file_path.clone(),
                            ref_err: None,
                        });
                    }
                }
            } else {
                // macro_name!(args...)
                // definitely not stolen from Rust
                if !skipping && i + 2 < tokens.len() {
                    if tokens[i].0 == "IDENTIFIER"
                        && tokens[i + 1] == ("OPERATOR".to_string(), "!".to_string())
                        && tokens[i + 2] == ("SEPARATOR".to_string(), "(".to_string())
                    {
                        let macro_name = &tokens[i].1;
                        if let Some((param_names, body)) = self.macros.get(macro_name) {
                            i += 3;
                
                            let mut paren_count = 1;
                            let mut call_args_tokens = Vec::new();
                
                            while i < tokens.len() && paren_count > 0 {
                                let (ref ttype, ref tval) = tokens[i];
                                if ttype == "SEPARATOR" {
                                    if tval == "(" {
                                        paren_count += 1;
                                    } else if tval == ")" {
                                        paren_count -= 1;
                                        if paren_count == 0 {
                                            i += 1;
                                            break;
                                        }
                                    }
                                }
                                if paren_count > 0 {
                                    call_args_tokens.push(tokens[i].clone());
                                }
                                i += 1;
                            }
                
                            let mut args_values: Vec<Vec<(String, String)>> = Vec::new();
                            let mut current_arg = Vec::new();
                            let mut nested_paren = 0;
                            for tok in call_args_tokens {
                                if tok == ("SEPARATOR".to_string(), ",".to_string()) && nested_paren == 0 {
                                    args_values.push(current_arg);
                                    current_arg = Vec::new();
                                } else {
                                    if tok.0 == "SEPARATOR" && tok.1 == "(" {
                                        nested_paren += 1;
                                    } else if tok.0 == "SEPARATOR" && tok.1 == ")" {
                                        nested_paren -= 1;
                                    }
                                    current_arg.push(tok);
                                }
                            }
                            args_values.push(current_arg);
                
                            if args_values.len() == 1 && args_values[0].is_empty() {
                                args_values.clear();
                            }
                
                            if args_values.len() > param_names.len() {
                                return Err(Error {
                                    error_type: "PreprocessorError".to_string(),
                                    msg: format!(
                                        "Macro {} expected at most {} arguments, got {}",
                                        macro_name,
                                        param_names.len(),
                                        args_values.len()
                                    ),
                                    help: None,
                                    line: (0, "".to_string()),
                                    column: 0,
                                    file: self.file_path.clone(),
                                    ref_err: None,
                                });
                            }
                
                            let mut replacement_map: HashMap<&str, Vec<(String, String)>> = HashMap::new();
                            for (idx, (name, default)) in param_names.iter().enumerate() {
                                let value = if idx < args_values.len() {
                                    Some(args_values[idx].clone())
                                } else {
                                    default.clone().map(|d| vec![d])
                                };                                
                
                                if let Some(v) = value {
                                    replacement_map.insert(name.as_str(), v);
                                } else {
                                    return Err(Error {
                                        error_type: "PreprocessorError".to_string(),
                                        msg: format!("Missing required macro argument ${}", name),
                                        help: None,
                                        line: (0, "".to_string()),
                                        column: 0,
                                        file: self.file_path.clone(),
                                        ref_err: None,
                                    });
                                }
                            }
                
                            let mut expanded_tokens = Vec::new();
                            let mut body_i = 0;
                
                            while body_i < body.len() {
                                let (ttype, tval) = &body[body_i];
                            
                                if ttype == "OPERATOR" && tval == "$" {
                                    body_i += 1;
                                    if body_i >= body.len() {
                                        return Err(Error {
                                            error_type: "PreprocessorError".to_string(),
                                            msg: "Unexpected end of macro body after $".to_string(),
                                            help: None,
                                            line: (0, "".to_string()),
                                            column: 0,
                                            file: self.file_path.clone(),
                                            ref_err: None,
                                        });
                                    }
                            
                                    if body[body_i] == ("OPERATOR".to_string(), "!".to_string()) {
                                        body_i += 1;
                                        if body_i >= body.len() {
                                            return Err(Error {
                                                error_type: "PreprocessorError".to_string(),
                                                msg: "Expected identifier after $!".to_string(),
                                                help: None,
                                                line: (0, "".to_string()),
                                                column: 0,
                                                file: self.file_path.clone(),
                                                ref_err: None,
                                            });
                                        }
                            
                                        let (next_type, next_val) = &body[body_i];
                                        if next_type != "IDENTIFIER" {
                                            return Err(Error {
                                                error_type: "PreprocessorError".to_string(),
                                                msg: format!("Expected IDENTIFIER after $!, got {}", next_type),
                                                help: None,
                                                line: (0, "".to_string()),
                                                column: 0,
                                                file: self.file_path.clone(),
                                                ref_err: None,
                                            });
                                        }
                            
                                        if let Some(replacement) = replacement_map.get(next_val.as_str()) {
                                            let joined = replacement
                                                .iter()
                                                .map(|(ttype, val)| {
                                                    if ttype == "STRING" {
                                                        format!("\"{}\"", val)
                                                    } else {
                                                        val.clone()
                                                    }
                                                })
                                                .collect::<Vec<_>>()
                                                .join(" ");
                                            expanded_tokens.push(("STRING".to_string(), format!("\"{}\"", joined)));
                                        } else {
                                            return Err(Error {
                                                error_type: "PreprocessorError".to_string(),
                                                msg: format!("Unknown macro argument $!{}", next_val),
                                                help: None,
                                                line: (0, "".to_string()),
                                                column: 0,
                                                file: self.file_path.clone(),
                                                ref_err: None,
                                            });
                                        }
                            
                                    } else {
                                        // Regular $arg replacement
                                        let (next_type, next_val) = &body[body_i];
                                        if next_type != "IDENTIFIER" {
                                            return Err(Error {
                                                error_type: "PreprocessorError".to_string(),
                                                msg: format!("Expected IDENTIFIER after $, got {}", next_type),
                                                help: None,
                                                line: (0, "".to_string()),
                                                column: 0,
                                                file: self.file_path.clone(),
                                                ref_err: None,
                                            });
                                        }
                            
                                        if let Some(replacement) = replacement_map.get(next_val.as_str()) {
                                            expanded_tokens.extend(replacement.clone());
                                        } else {
                                            return Err(Error {
                                                error_type: "PreprocessorError".to_string(),
                                                msg: format!("Unknown macro argument ${}", next_val),
                                                help: None,
                                                line: (0, "".to_string()),
                                                column: 0,
                                                file: self.file_path.clone(),
                                                ref_err: None,
                                            });
                                        }
                                    }
                            
                                } else {
                                    expanded_tokens.push((ttype.clone(), tval.clone()));
                                }
                            
                                body_i += 1;
                            }                            
                
                            result.extend(expanded_tokens);
                            continue;
                        } else {
                            return Err(Error {
                                error_type: "PreprocessorError".to_string(),
                                msg: format!("Macro {} not found", macro_name),
                                help: None,
                                line: (0, "".to_string()),
                                column: 0,
                                file: self.file_path.clone(),
                                ref_err: None,
                            });
                        }
                    }
                }
                    
                if !skipping {
                    let mut ttype = ttype.clone();
                    let mut tval = tval.clone();
            
                    if let Some((alias_type, alias_val)) = self.aliases.get(&(ttype.clone(), tval.clone())) {
                        ttype = alias_type.clone();
                        tval = alias_val.clone();
                    }
            
                    if ttype == "IDENTIFIER" {
                        if let Some((def_type, def_val)) = self.defines.get(&tval) {
                            ttype = def_type.clone();
                            tval = def_val.clone();
                        }
                    }
            
                    result.push((ttype, tval));
                }
                i += 1;
            }            
        }

        Ok(result)
    }
}
