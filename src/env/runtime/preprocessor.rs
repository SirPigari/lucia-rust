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
    file_path: String,
}

impl Preprocessor {
    pub fn new<P: Into<PathBuf>>(lib_dir: P, config_path: P, file_path: &str) -> Self {
        Self {
            lib_dir: lib_dir.into(),
            config_path: config_path.into(),
            defines: HashMap::new(),
            aliases: HashMap::new(),
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

                        let included_path: PathBuf;

                        if tokens[i].0 == "OPERATOR" && tokens[i].1 == "<" {
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

                            if lib_base.is_dir() {
                                let mut entries: Vec<_> = fs::read_dir(&lib_base)
                                    .map_err(|e| Error {
                                        error_type: "PreprocessorIOError".to_string(),
                                        msg: format!("Failed to read directory '{}': {}", lib_base.display(), e),
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
                                    let mut toks = lexer.tokenize(false);
                                    if let Some(last_token) = toks.last() {
                                        if last_token.0 != "EOF" {
                                            return Err(Error {
                                                error_type: "PreprocessorError".to_string(),
                                                msg: format!("File '{}' not closed properly", file_path.display()),
                                                help: Some("Propably corrupted.".to_string()),
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
                                included_path = lib_base;
                            }
                        } else if tokens[i].0 == "STRING" {
                            let raw = &tokens[i].1;
                            i += 1;
                            included_path = current_dir.join(raw.trim_matches('"'));
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
                        }

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
                        let mut toks = lexer.tokenize(false);
                        if toks.last().map_or(false, |t| t.0 == "EOF") {
                            toks.pop();
                        }

                        let included = self._process(toks, included_path.parent().unwrap_or(current_dir))?;
                        result.extend(included);
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
                        let value = tokens[i].1.clone();
                        i += 1;

                        result.push(("IDENTIFIER".to_string(), "config".to_string()));
                        result.push(("OPERATOR".to_string(), ".".to_string()));
                        result.push(("IDENTIFIER".to_string(), "set".to_string()));
                        result.push(("OPERATOR".to_string(), "(".to_string()));
                        result.push(("STRING".to_string(), key));
                        result.push(("OPERATOR".to_string(), ",".to_string()));
                        result.push(("STRING".to_string(), value));
                        result.push(("OPERATOR".to_string(), ")".to_string()));

                        return Err(Error {
                            error_type: "Unimplemented".to_string(),
                            msg: "config.set() is not implemented yet".to_string(),
                            help: Some("This feature will be added later".to_string()),
                            line: (0, "".to_string()),
                            column: 0,
                            file: self.file_path.clone(),
                            ref_err: None,
                        });
                    }
                    _ => {
                        return Err(Error {
                            error_type: "PreprocessorError".to_string(),
                            msg: format!("Unknown preprocessor directive: {}", directive),
                            help: Some("Valid directives are: define, undef, ifdef, ifndef, endif, alias, unalias, include, config".to_string()),
                            line: (0, "".to_string()),
                            column: 0,
                            file: self.file_path.clone(),
                            ref_err: None,
                        });
                    }
                }
            } else {
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
