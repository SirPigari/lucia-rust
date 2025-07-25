use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use crate::lexer::Lexer;
use crate::env::runtime::errors::Error;
use crate::env::runtime::tokens::{Token, Location};
use crate::env::runtime::utils::{to_static, KEYWORDS};
use crate::env::runtime::precompile::precompile;

// u not getting more
const MAX_MACRO_RECURSION_DEPTH: usize = 16;

struct MangleContext {
    mangled_names: Vec<HashMap<String, String>>, // ORIGINAL_NAME -> MANGLED_NAME
    counter: usize,
    current_mangle_index: usize,
}

impl MangleContext {
    fn new() -> Self {
        Self {
            mangled_names: vec![HashMap::new()],
            counter: 0,
            current_mangle_index: 0,
        }
    }

    fn enter_scope(&mut self) {
        self.mangled_names.push(HashMap::new());
        self.current_mangle_index += 1;
    }
    fn exit_scope(&mut self) {
        if self.current_mangle_index > 0 {
            self.mangled_names.pop();
            self.current_mangle_index -= 1;
        }
    }
    fn mangle(&mut self, original: &str, mangle: &str) -> String {
        self.mangled_names[self.current_mangle_index].insert(original.to_string(), mangle.to_string());
        self.counter += 1;
        mangle.to_string()
    }
    fn get_mangled(&self, original: &str) -> Option<&String> {
        self.mangled_names[self.current_mangle_index].get(original)
    }
}

pub struct Preprocessor {
    lib_dir: PathBuf,
    defines: HashMap<String, Token>, // IDENTIFIER -> TOKEN
    aliases: HashMap<Token, Token>, // TOKEN -> ALIAS_TOKEN
    macros: HashMap<String, (Vec<(String, Option<Token>)>, Vec<Token>)>, // MACRO_NAME -> (ARGS, BODY)
    mangle_context: MangleContext,
    file_path: String,
}

impl Preprocessor {
    pub fn new<P: Into<PathBuf>>(lib_dir: P, file_path: &str) -> Self {
        Self {
            lib_dir: lib_dir.into(),
            defines: HashMap::new(),
            aliases: HashMap::new(),
            macros: HashMap::new(),
            mangle_context: MangleContext::new(),
            file_path: file_path.to_string(),
        }
    }

    pub fn process(
        &mut self,
        tokens: Vec<Token>,
        current_dir: &Path,
    ) -> Result<Vec<Token>, Error> {
        let filtered = tokens
            .into_iter()
            .filter(|tok| tok.0 != "WHITESPACE")
            .collect();
    
        self._process(filtered, current_dir)
    }    

    fn _process(
        &mut self,
        tokens: Vec<Token>,
        current_dir: &Path,
    ) -> Result<Vec<Token>, Error> {
        let mut result = Vec::new();
        let mut i = 0;
        let mut skip_stack = Vec::new();
        let mut skipping = false;
        let mut last_normal_token_location: Option<Location> = None;

        while i < tokens.len() {
            let token = &tokens[i];

            if token.0 != "WHITESPACE" && !token.0.starts_with("COMMENT_") {
                last_normal_token_location = token.2.clone();
            }

            if token.0 == "OPERATOR" && token.1 == "#" {
                i += 1;
                if i >= tokens.len() {
                    return Err(Error::new(
                        "PreprocessorError",
                        "Unexpected end after '#'",
                        &self.file_path,
                    ));
                }
                
                let directive = &tokens[i].1;
                i += 1;

                match directive.as_str() {
                    "macro" => {
                        if i >= tokens.len() {
                            return Err(Error::new(
                                "PreprocessorError",
                                "#macro missing NAME",
                                &self.file_path,
                            ));
                        }

                        let name_token = &tokens[i];

                        i += 1;

                        if name_token.0 != "IDENTIFIER" {
                            return Err(Error::new(
                                "PreprocessorError",
                                &format!("#macro NAME must be IDENTIFIER, got {}", name_token.0),
                                &self.file_path,
                            ));
                        }

                        if &tokens[i].1 == "!" {
                            i += 1;
                        }

                        let mut args: Vec<(String, Option<Token>)> = Vec::new();

                        if i < tokens.len() && matches!(tokens[i], Token(ref a, ref b, _) if a == "SEPARATOR" && b == "(") {
                            i += 1;
                        
                            while i < tokens.len() && !matches!(tokens[i], Token(ref a, ref b, _) if a == "SEPARATOR" && b == ")") {
                                if !matches!(tokens[i], Token(ref a, ref b, _) if a == "OPERATOR" && b == "$") {
                                    return Err(Error::new(
                                        "PreprocessorError",
                                        &format!("#macro argument must start with $, got {}", tokens[i].1),
                                        &self.file_path,
                                    ));
                                }
                                i += 1;
                            
                                if i >= tokens.len() || tokens[i].0 != "IDENTIFIER" {
                                    return Err(Error::new(
                                        "PreprocessorError",
                                        "Expected IDENTIFIER after $ in macro argument",
                                        &self.file_path,
                                    ));
                                }
                            
                                let mut name = tokens[i].1.clone();
                                i += 1;
                            
                                let mut is_variadic = false;
                                if i < tokens.len()
                                    && tokens[i].0 == "SEPARATOR"
                                    && tokens[i].1 == "..."
                                {
                                    is_variadic = true;
                                    i += 1;
                                    name.push_str("...");
                                }
                            
                                let mut default_val = None;
                            
                                if i + 2 < tokens.len()
                                    && matches!(tokens[i], Token(ref a, ref b, _) if a == "OPERATOR" && b == "=")
                                    && matches!(tokens[i + 1], Token(ref a, ref b, _) if a == "SEPARATOR" && b == "(")
                                {
                                    i += 2;
                                    let mut val_tokens = Vec::new();
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
                            
                                        val_tokens.push(tokens[i].clone());
                                        i += 1;
                                    }
                            
                                    if depth != 0 {
                                        return Err(Error::new(
                                            "PreprocessorError",
                                            "Unclosed default value in macro argument",
                                            &self.file_path,
                                        ));
                                    }
                            
                                    default_val = val_tokens.into_iter().next();
                                }
                            
                                args.push((name, default_val));
                            
                                if is_variadic && i < tokens.len() && matches!(tokens[i], Token(ref a, ref b, _) if a == "SEPARATOR" && b == ",") {
                                    return Err(Error::new(
                                        "PreprocessorError",
                                        "Variadic argument must be last",
                                        &self.file_path,
                                    ));
                                }
                            
                                if i < tokens.len() && matches!(tokens[i], Token(ref a, ref b, _) if a == "SEPARATOR" && b == ",") {
                                    i += 1;
                                }                                
                            }

                            if i >= tokens.len()
                                || !matches!(tokens[i], Token(ref a, ref b, _) if a == "SEPARATOR" && b == ")")
                            {
                                return Err(Error::new(
                                    "PreprocessorError",
                                    "Unclosed argument list in #macro",
                                    &self.file_path,
                                ));
                            }
                            i += 1;
                        }

                        if tokens[i].0 != "SEPARATOR" || tokens[i].1 != ":" {
                            return Err(Error::new(
                                "PreprocessorError",
                                "Expected ':' to start macro body",
                                &self.file_path,
                            ));
                        }

                        i += 1;

                        let mut body_tokens = Vec::new();
                        while i < tokens.len() {
                            if tokens[i].0 == "OPERATOR" && tokens[i].1 == "#" && i + 1 < tokens.len() {
                                if i + 1 < tokens.len() && ((tokens[i + 1].0 == "IDENTIFIER") && (tokens[i + 1].1 == "endmacro")) {
                                    i += 2;
                                    break;
                                }
                            }                            
                            body_tokens.push(tokens[i].clone());
                            i += 1;
                        }

                        self.macros.insert(name_token.1.clone(), (args, body_tokens));
                        continue;
                    }

                    "endmacro" => {
                        return Err(Error::new(
                            "PreprocessorError",
                            "#endmacro without matching #macro",
                            &self.file_path,
                        ));
                    }

                    "define" => {
                        if i >= tokens.len() {
                            return Err(Error::new(
                                "PreprocessorError",
                                "#define missing NAME",
                                &self.file_path,
                            ));
                        }

                        let name_token = &tokens[i];
                        i += 1;

                        if name_token.0 != "IDENTIFIER" {
                            return Err(Error::new(
                                "PreprocessorError",
                                &format!("#define NAME must be IDENTIFIER, got {}", name_token.0),
                                &self.file_path,
                            ));
                        }

                        if i >= tokens.len() {
                            return Err(Error::new(
                                "PreprocessorError",
                                "#define missing VALUE",
                                &self.file_path,
                            ));
                        }

                        let special_words = ["import", "if", "#", "for", "while", "match", "defer", "scope"];

                        let next_token = &tokens[i];

                        if special_words.contains(&next_token.1.as_str()) {
                            let value = Token("BOOLEAN".to_string(), "null".to_string(), last_normal_token_location.clone());
                            self.defines.insert(name_token.1.clone(), value);
                        } else {
                            let value = tokens[i].clone();
                            i += 1;
                            self.defines.insert(name_token.1.clone(), value);
                        }

                        continue;
                    }

                    "undef" => {
                        if i >= tokens.len() {
                            return Err(Error::new(
                                "PreprocessorError",
                                "#undef missing NAME",
                                &self.file_path,
                            ));
                        }
                        
                        let name_token = &tokens[i];
                        i += 1;
                        
                        if name_token.0 != "IDENTIFIER" {
                            return Err(Error::new(
                                "PreprocessorError",
                                "#undef NAME must be IDENTIFIER",
                                &self.file_path,
                            ));
                        }
                        
                        self.defines.remove(&name_token.1);
                        continue;
                    }

                    "ifdef" => {
                        if i >= tokens.len() {
                            return Err(Error::new(
                                "PreprocessorError",
                                "#ifdef missing NAME",
                                &self.file_path,
                            ));
                        }
                        
                        let name_token = &tokens[i];
                        i += 1;
                        
                        if name_token.0 != "IDENTIFIER" {
                            return Err(Error::new(
                                "PreprocessorError",
                                "#ifdef NAME must be IDENTIFIER",
                                &self.file_path,
                            ));
                        }
                        
                        let cond = self.defines.contains_key(&name_token.1);
                        skip_stack.push(skipping);
                        skipping = !cond;
                        continue;
                    }

                    "ifndef" => {
                        if i >= tokens.len() {
                            return Err(Error::new(
                                "PreprocessorError",
                                "#ifndef missing NAME",
                                &self.file_path,
                            ));
                        }
                        
                        let name_token = &tokens[i];
                        i += 1;
                        
                        if name_token.0 != "IDENTIFIER" {
                            return Err(Error::new(
                                "PreprocessorError",
                                "#ifndef NAME must be IDENTIFIER",
                                &self.file_path,
                            ));
                        }
                        
                        let cond = !self.defines.contains_key(&name_token.1);
                        skip_stack.push(skipping);
                        skipping = !cond;
                        continue;
                    }

                    "endif" => {
                        skipping = skip_stack.pop().unwrap_or(false);
                        continue;
                    }

                    "alias" => {
                        if i + 1 >= tokens.len() {
                            return Err(Error::new(
                                "PreprocessorError",
                                "#alias missing tokens",
                                &self.file_path,
                            ));
                        }
                        
                        let from_token = tokens[i].clone();
                        i += 1;
                        let to_token = tokens[i].clone();
                        i += 1;

                        self.aliases.insert(from_token, to_token);
                        continue;
                    }

                    "unalias" => {
                        if i >= tokens.len() {
                            return Err(Error::new(
                                "PreprocessorError",
                                "#unalias missing token",
                                &self.file_path,
                            ));
                        }
                        
                        let token = tokens[i].clone();
                        i += 1;

                        self.aliases.remove(&token);
                        continue;
                    }

                    "include" => {
                        if i >= tokens.len() {
                            return Err(Error::new(
                                "PreprocessorError",
                                "#include missing path",
                                &self.file_path,
                            ));
                        }

                        let included_path = if tokens[i].0 == "OPERATOR" && tokens[i].1 == "<" {
                            i += 1;
                            let mut path_parts = Vec::new();

                            while i < tokens.len() && !(tokens[i].0 == "OPERATOR" && tokens[i].1 == ">") {
                                path_parts.push(tokens[i].1.clone());
                                i += 1;
                            }

                            if i >= tokens.len() || tokens[i].1 != ">" {
                                return Err(Error::new(
                                    "PreprocessorError",
                                    "Unclosed '<...>' in #include",
                                    &self.file_path,
                                ));
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
                            return Err(Error::new(
                                "PreprocessorError",
                                "Invalid include path format",
                                &self.file_path,
                            ));
                        };

                        if !included_path.exists() {
                            return Err(Error::new(
                                "PreprocessorError",
                                &format!("Included file does not exist: {}", included_path.display()),
                                &self.file_path,
                            ));
                        }

                        if included_path.is_dir() {
                            let mut entries: Vec<_> = fs::read_dir(&included_path)
                                .map_err(|e| Error::new(
                                    "PreprocessorIOError",
                                    &format!("Failed to read directory '{}': {}", included_path.display(), e),
                                    &self.file_path,
                                ))?
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
                                let content = fs::read_to_string(&file_path).map_err(|e| Error::new(
                                    "PreprocessorIOError",
                                    &format!("Failed to read included file '{}': {}", file_path.display(), e),
                                    &self.file_path,
                                ))?;

                                let mut lexer = Lexer::new(&content, to_static(self.file_path.clone()), None);
                                let mut toks = lexer.tokenize()
                                    .into_iter()
                                    .filter(|tok| tok.0 != "WHITESPACE")
                                    .collect::<Vec<_>>();
                                    
                                if let Some(last_token) = toks.last() {
                                    if last_token.0 != "EOF" {
                                        return Err(Error::new(
                                            "PreprocessorError",
                                            &format!("File '{}' not closed properly", file_path.display()),
                                            &self.file_path,
                                        ));
                                    } else {
                                        toks.pop();
                                    }
                                }

                                let included = self._process(toks, file_path.parent().unwrap_or(current_dir))?;
                                result.extend(included);
                            }
                        } else {
                            let content = fs::read_to_string(&included_path).map_err(|e| Error::new(
                                "PreprocessorIOError",
                                &format!("Failed to read included file '{}': {}", included_path.display(), e),
                                &self.file_path,
                            ))?;

                            let mut lexer = Lexer::new(&content, to_static(self.file_path.clone()), None);
                            let mut toks = lexer.tokenize()
                                .into_iter()
                                .filter(|tok| tok.0 != "WHITESPACE")
                                .collect::<Vec<_>>();
                                
                            if toks.last().map_or(false, |t| t.0 == "EOF") {
                                toks.pop();
                            }

                            let included = self._process(toks, included_path.parent().unwrap_or(current_dir))?;
                            result.extend(included);
                        }
                        continue;
                    }

                    "config" => {
                        if i + 1 >= tokens.len() {
                            return Err(Error::new(
                                "PreprocessorError",
                                "#config requires a key and a value or reset key",
                                &self.file_path,
                            ));
                        }

                        let key = tokens[i].1.clone();
                        i += 1;

                        if key == "reset" {
                            if i >= tokens.len() {
                                return Err(Error::new(
                                    "PreprocessorError",
                                    "#config reset requires a key",
                                    &self.file_path,
                                ));
                            }
                            let reset_key = tokens[i].1.clone();
                            i += 1;

                            let loc = last_normal_token_location.clone();

                            result.push(Token("IDENTIFIER".to_string(), "00__set_cfg__".to_string(), loc.clone()));
                            result.push(Token("SEPARATOR".to_string(), "(".to_string(), loc.clone()));
                            result.push(Token("STRING".to_string(), format!("\"{}\"", reset_key), loc.clone()));
                            result.push(Token("SEPARATOR".to_string(), ",".to_string(), loc.clone()));
                            // 0x6969 = 26985
                            result.push(Token("NUMBER".to_string(), "26985".to_string(), loc.clone()));
                            result.push(Token("SEPARATOR".to_string(), ")".to_string(), loc));
                            continue;
                        } else {
                            if i >= tokens.len() || tokens[i].1 != "=" {
                                return Err(Error::new(
                                    "PreprocessorError",
                                    "Missing '=' after #config key",
                                    &self.file_path,
                                ));
                            }
                            i += 1;

                            if i >= tokens.len() {
                                return Err(Error::new(
                                    "PreprocessorError",
                                    "#config requires a value after '='",
                                    &self.file_path,
                                ));
                            }
                            let value = tokens[i].clone();
                            i += 1;

                            let loc = last_normal_token_location.clone();

                            result.push(Token("IDENTIFIER".to_string(), "00__set_cfg__".to_string(), loc.clone()));
                            result.push(Token("SEPARATOR".to_string(), "(".to_string(), loc.clone()));
                            result.push(Token("STRING".to_string(), format!("\"{}\"", key), loc.clone()));
                            result.push(Token("SEPARATOR".to_string(), ",".to_string(), loc.clone()));
                            result.push(value);
                            result.push(Token("SEPARATOR".to_string(), ")".to_string(), loc));
                            continue;
                        }
                    }

                    "syntax" => {
                        if i + 1 >= tokens.len() {
                            return Err(Error::new(
                                "PreprocessorError",
                                "#syntax requires a pattern and a replacement",
                                &self.file_path,
                            ));
                        }

                        let _pattern = &tokens[i].1;
                        i += 1;

                        if i >= tokens.len() || tokens[i].1 != "->" {
                            return Err(Error::new(
                                "PreprocessorError",
                                "Expected '->' after #syntax pattern",
                                &self.file_path,
                            ));
                        }
                        i += 1;

                        if i >= tokens.len() || tokens[i].0 != "STRING" {
                            return Err(Error::new(
                                "PreprocessorError",
                                "#syntax requires a STRING replacement",
                                &self.file_path,
                            ));
                        }

                        let _replacement = tokens[i].1.clone();
                        i += 1;
                        continue;
                    }

                    "[" | "!" => {
                        let is_negated = tokens[i - 1].1 == "!";

                        if is_negated {
                            if i >= tokens.len() || tokens[i].1 != "[" {
                                return Err(Error::new(
                                    "PreprocessorError",
                                    "Expected '[' after '!'",
                                    &self.file_path,
                                ));
                            }
                            i += 1;
                        }

                        if i >= tokens.len() {
                            return Err(Error::new(
                                "PreprocessorError",
                                "Expected key inside brackets",
                                &self.file_path,
                            ));
                        }

                        let key = tokens[i].1.clone();
                        i += 1;

                        if i >= tokens.len() || tokens[i].1 != "]" {
                            return Err(Error::new(
                                "PreprocessorError",
                                "Expected closing ']' after key",
                                &self.file_path,
                            ));
                        }
                        i += 1;

                        let loc = last_normal_token_location.clone();

                        result.push(Token("IDENTIFIER".to_string(), "00__set_dir__".to_string(), loc.clone()));
                        result.push(Token("SEPARATOR".to_string(), "(".to_string(), loc.clone()));
                        result.push(Token("STRING".to_string(), format!("\"{}\"", key), loc.clone()));
                        result.push(Token("SEPARATOR".to_string(), ",".to_string(), loc.clone()));
                        result.push(Token("BOOLEAN".to_string(), if is_negated { "false" } else { "true" }.to_string(), loc.clone()));
                        result.push(Token("SEPARATOR".to_string(), ")".to_string(), loc));
                        continue;
                    }

                    _ => {
                        return Err(Error::new(
                            "PreprocessorError",
                            &format!("Unknown preprocessor directive: {}", directive),
                            &self.file_path,
                        ));
                    }
                }
            } else if token.0 == "IDENTIFIER" && token.1 == "precompile" {
                i += 1;
                if i >= tokens.len() || tokens[i].0 != "SEPARATOR" || tokens[i].1 != "(" {
                    return Err(Error::new(
                        "PreprocessorError",
                        "Expected '(' after 'precompile'",
                        &self.file_path,
                    ));
                }
                i += 1;

                let mut precompile_tokens = Vec::new();
                while i < tokens.len() && !(tokens[i].0 == "SEPARATOR" && tokens[i].1 == ")") {
                    precompile_tokens.push(tokens[i].clone());
                    i += 1;
                }

                if i >= tokens.len() || tokens[i].0 != "SEPARATOR" || tokens[i].1 != ")" {
                    return Err(Error::new(
                        "PreprocessorError",
                        "Unclosed precompile directive",
                        &self.file_path,
                    ));
                }
                if precompile_tokens.is_empty() {
                    return Err(Error::new(
                        "PreprocessorError",
                        "precompile directive requires at least one token",
                        &self.file_path,
                    ));
                }

                let precompiled_tokens = precompile(precompile_tokens)?;
                result.extend(precompiled_tokens);
            } else {
                if !skipping 
                    && i + 2 < tokens.len()
                    && matches!(tokens[i], Token(ref a, _, _) if a == "IDENTIFIER")
                    && matches!(tokens[i + 1], Token(ref a, ref b, _) if a == "OPERATOR" && b == "!")
                    && matches!(tokens[i + 2], Token(ref a, ref b, _) if a == "SEPARATOR" && b == "(") 
                {
                    // macro_name!(args...)
                    // definitely not stolen from Rust
                    let macro_name = &tokens[i].1;
                    let call_loc = tokens[i].2.clone();
                    if let Some((param_names, body)) = self.macros.get(macro_name) {
                        i += 3;

                        let mut paren_count = 1;
                        let mut call_args_tokens = Vec::new();

                        while i < tokens.len() && paren_count > 0 {
                            let token = &tokens[i];
                            if token.0 == "SEPARATOR" {
                                if token.1 == "(" {
                                    paren_count += 1;
                                } else if token.1 == ")" {
                                    paren_count -= 1;
                                    if paren_count == 0 {
                                        i += 1;
                                        break;
                                    }
                                }
                            }
                            if paren_count > 0 {
                                call_args_tokens.push(token.clone());
                            }
                            i += 1;
                        }

                        let mut args_values: Vec<Vec<Token>> = Vec::new();
                        let mut current_arg = Vec::new();
                        let mut nested_paren = 0;

                        for tok in call_args_tokens {
                            let Token(ref a, ref b, _) = tok;

                            if a == "SEPARATOR" && b == "," && nested_paren == 0 {
                                args_values.push(current_arg);
                                current_arg = Vec::new();
                            } else {
                                if a == "SEPARATOR" && b == "(" {
                                    nested_paren += 1;
                                } else if a == "SEPARATOR" && b == ")" {
                                    nested_paren -= 1;
                                }
                                current_arg.push(tok);
                            }
                        }
                        args_values.push(current_arg);

                        if args_values.len() == 1 && args_values[0].is_empty() {
                            args_values.clear();
                        }

                        let variadic_pos = param_names.iter().position(|(n, _)| n.ends_with("..."));

                        if let Some(pos) = variadic_pos {
                            if args_values.len() < pos {
                                return Err(Error::new(
                                    "PreprocessorError",
                                    &format!(
                                        "Macro {} expected at least {} arguments before variadic, got {}",
                                        macro_name,
                                        pos,
                                        args_values.len()
                                    ),
                                    &self.file_path,
                                ));
                            }
                        } else if args_values.len() > param_names.len() {
                            return Err(Error::new(
                                "PreprocessorError",
                                &format!(
                                    "Macro {} expected at most {} arguments, got {}",
                                    macro_name,
                                    param_names.len(),
                                    args_values.len()
                                ),
                                &self.file_path,
                            ));
                        }

                        let mut replacement_map: HashMap<&str, Vec<Token>> = HashMap::new();

                        for (idx, (name, default)) in param_names.iter().enumerate() {
                            if Some(idx) == variadic_pos {
                                let mut variadic_tokens = Vec::new();

                                for (vi, rest_arg) in args_values.iter().enumerate().skip(idx) {
                                    if vi != idx {
                                        variadic_tokens.push(Token("SEPARATOR".to_string(), ",".to_string(), call_loc.clone()));
                                    }
                                    variadic_tokens.extend(rest_arg.clone());
                                }

                                let variadic_name = &name[..name.len() - 3];
                                replacement_map.insert(variadic_name, variadic_tokens);
                                break;
                            } else {
                                let value = if idx < args_values.len() {
                                    Some(args_values[idx].clone())
                                } else {
                                    default.clone().map(|d| vec![expand_macros_in_default_string(&d, &replacement_map)])
                                };

                                if let Some(v) = value {
                                    replacement_map.insert(name.as_str(), v);
                                } else {
                                    return Err(Error::new(
                                        "PreprocessorError",
                                        &format!("Missing required macro argument ${}", name),
                                        &self.file_path,
                                    ));
                                }
                            }
                        }

                        let mut expanded_tokens = Vec::new();
                        let mut body_i = 0;

                        while body_i < body.len() {
                            let token = &body[body_i];

                            if token.0 == "OPERATOR" && token.1 == "$" {
                                body_i += 1;
                                if body_i >= body.len() {
                                    return Err(Error::new(
                                        "PreprocessorError",
                                        "Unexpected end of macro body after $",
                                        &self.file_path,
                                    ));
                                }

                                let Token(a, b, _) = &body[body_i];

                                if a == "OPERATOR" && b == "!" {
                                    body_i += 1;
                                    if body_i >= body.len() {
                                        return Err(Error::new(
                                            "PreprocessorError",
                                            "Expected identifier after $!",
                                            &self.file_path,
                                        ));
                                    }

                                    let next_token = &body[body_i];
                                    if next_token.0 != "IDENTIFIER" {
                                        return Err(Error::new(
                                            "PreprocessorError",
                                            &format!("Expected IDENTIFIER after $!, got {}", next_token.0),
                                            &self.file_path,
                                        ));
                                    }

                                    let arg_name = if next_token.1.ends_with("...") {
                                        &next_token.1[..next_token.1.len() - 3]
                                    } else {
                                        next_token.1.as_str()
                                    };

                                    if let Some(replacement) = replacement_map.get(arg_name) {
                                        let joined = replacement
                                            .iter()
                                            .map(|t| {
                                                if t.0 == "STRING" {
                                                    format!("{}", t.1).replace("\"", "\\\"").replace("'", "\\'")
                                                } else {
                                                    t.1.clone()
                                                }
                                            })
                                            .collect::<Vec<_>>()
                                            .join(" ");

                                        let loc = token.2.clone().or(None);
                                        expanded_tokens.push(Token(
                                            "STRING".to_string(),
                                            format!("\"{}\"", joined),
                                            loc,
                                        ));
                                    } else {
                                        return Err(Error::new(
                                            "PreprocessorError",
                                            &format!("Unknown macro argument $!{}", next_token.1),
                                            &self.file_path,
                                        ));
                                    }

                                } else {
                                    let next_token = &body[body_i];
                                    if next_token.0 != "IDENTIFIER" {
                                        return Err(Error::new(
                                            "PreprocessorError",
                                            &format!("Expected IDENTIFIER after $, got {}", next_token.0),
                                            &self.file_path,
                                        ));
                                    }

                                    if let Some(replacement) = replacement_map.get(next_token.1.as_str()) {
                                        expanded_tokens.extend(replacement.clone());
                                    } else {
                                        return Err(Error::new(
                                            "PreprocessorError",
                                            &format!("Unknown macro argument ${}", next_token.1),
                                            &self.file_path,
                                        ));
                                    }
                                }
                            } else if token.0 == "STRING" && !token.1.is_empty() {
                                expanded_tokens.push(expand_macros_in_default_string(token, &replacement_map));
                            } else {
                                expanded_tokens.push(token.clone());
                            }

                            body_i += 1;
                        }

                        self.mangle_context.enter_scope();

                        let recursively_expanded = self.expand_tokens_with_macros(&expanded_tokens, skipping, call_loc.clone(), 0, &current_dir)?;
                        result.extend(self.mangle_tokens(&recursively_expanded));

                        self.mangle_context.exit_scope();
                        continue;
                    } else {
                        return Err(Error::new(
                            "PreprocessorError",
                            &format!("Macro {} not found", macro_name),
                            &self.file_path,
                        ));
                    }

                } else if !skipping {
                    let mut token = token.clone();
                
                    let alias = self.aliases.iter()
                        .find_map(|(k, v)| if k.0 == token.0 && k.1 == token.1 { Some(v) } else { None });
                    
                    if let Some(alias) = alias {
                        token = alias.clone();
                    }
                
                    if token.0 == "IDENTIFIER" {
                        if let Some(def) = self.defines.get(&token.1) {
                            token = def.clone();
                        }
                    }
                
                    result.push(token);
                }
            }
            
            i += 1;
        }

        Ok(result)
    }

    fn expand_tokens_with_macros(
        &mut self,
        tokens: &[Token],
        skipping: bool,
        call_loc: Option<Location>,
        depth: usize,
        current_dir: &Path,
    ) -> Result<Vec<Token>, Error> {
        let mut result = Vec::new();
        let mut i = 0;

        while i < tokens.len() {
            if !skipping
                && i + 2 < tokens.len()
                && matches!(tokens[i], Token(ref a, _, _) if a == "IDENTIFIER")
                && matches!(tokens[i + 1], Token(ref a, ref b, _) if a == "OPERATOR" && b == "!")
                && matches!(tokens[i + 2], Token(ref a, ref b, _) if a == "SEPARATOR" && b == "(")
            {
                let macro_name = &tokens[i].1;
                if let Some((param_names, body)) = self.macros.get(macro_name) {
                    i += 3;

                    let mut paren_count = 1;
                    let mut call_args_tokens = Vec::new();

                    while i < tokens.len() && paren_count > 0 {
                        let token = &tokens[i];
                        if token.0 == "SEPARATOR" {
                            if token.1 == "(" {
                                paren_count += 1;
                            } else if token.1 == ")" {
                                paren_count -= 1;
                                if paren_count == 0 {
                                    i += 1;
                                    break;
                                }
                            }
                        }
                        if paren_count > 0 {
                            call_args_tokens.push(token.clone());
                        }
                        i += 1;
                    }

                    let mut args_values: Vec<Vec<Token>> = Vec::new();
                    let mut current_arg = Vec::new();
                    let mut nested_paren = 0;

                    for tok in call_args_tokens {
                        let Token(ref a, ref b, _) = tok;

                        if a == "SEPARATOR" && b == "," && nested_paren == 0 {
                            args_values.push(current_arg);
                            current_arg = Vec::new();
                        } else {
                            if a == "SEPARATOR" && b == "(" {
                                nested_paren += 1;
                            } else if a == "SEPARATOR" && b == ")" {
                                nested_paren -= 1;
                            }
                            current_arg.push(tok);
                        }
                    }
                    args_values.push(current_arg);

                    if args_values.len() == 1 && args_values[0].is_empty() {
                        args_values.clear();
                    }

                    let variadic_pos = param_names.iter().position(|(n, _)| n.ends_with("..."));

                    if let Some(pos) = variadic_pos {
                        if args_values.len() < pos {
                            return Err(Error::new(
                                "PreprocessorError",
                                &format!(
                                    "Macro {} expected at least {} arguments before variadic, got {}",
                                    macro_name,
                                    pos,
                                    args_values.len()
                                ),
                                &self.file_path,
                            ));
                        }
                    } else if args_values.len() > param_names.len() {
                        return Err(Error::new(
                            "PreprocessorError",
                            &format!(
                                "Macro {} expected at most {} arguments, got {}",
                                macro_name,
                                param_names.len(),
                                args_values.len()
                            ),
                            &self.file_path,
                        ));
                    }

                    let mut replacement_map: HashMap<&str, Vec<Token>> = HashMap::new();

                    for (idx, (name, default)) in param_names.iter().enumerate() {
                        if Some(idx) == variadic_pos {
                            let mut variadic_tokens = Vec::new();

                            for (vi, rest_arg) in args_values.iter().enumerate().skip(idx) {
                                if vi != idx {
                                    variadic_tokens.push(Token("SEPARATOR".to_string(), ",".to_string(), call_loc.clone()));
                                }
                                variadic_tokens.extend(rest_arg.clone());
                            }

                            let variadic_name = &name[..name.len() - 3];
                            replacement_map.insert(variadic_name, variadic_tokens);
                            break;
                        } else {
                            let value = if idx < args_values.len() {
                                Some(args_values[idx].clone())
                            } else {
                                default.clone().map(|d| vec![expand_macros_in_default_string(&d, &replacement_map)])
                            };

                            if let Some(v) = value {
                                replacement_map.insert(name.as_str(), v);
                            } else {
                                return Err(Error::new(
                                    "PreprocessorError",
                                    &format!("Missing required macro argument ${}", name),
                                    &self.file_path,
                                ));
                            }
                        }
                    }

                    let mut expanded_tokens = Vec::new();
                    let mut body_i = 0;

                    while body_i < body.len() {
                        let token = &body[body_i];

                        if token.0 == "OPERATOR" && token.1 == "$" {
                            body_i += 1;
                            if body_i >= body.len() {
                                return Err(Error::new(
                                    "PreprocessorError",
                                    "Unexpected end of macro body after $",
                                    &self.file_path,
                                ));
                            }

                            let Token(a, b, _) = &body[body_i];

                            if a == "OPERATOR" && b == "!" {
                                body_i += 1;
                                if body_i >= body.len() {
                                    return Err(Error::new(
                                        "PreprocessorError",
                                        "Expected identifier after $!",
                                        &self.file_path,
                                    ));
                                }

                                let next_token = &body[body_i];
                                if next_token.0 != "IDENTIFIER" {
                                    return Err(Error::new(
                                        "PreprocessorError",
                                        &format!("Expected IDENTIFIER after $!, got {}", next_token.0),
                                        &self.file_path,
                                    ));
                                }

                                let arg_name = if next_token.1.ends_with("...") {
                                    &next_token.1[..next_token.1.len() - 3]
                                } else {
                                    next_token.1.as_str()
                                };

                                if let Some(replacement) = replacement_map.get(arg_name) {
                                    let joined = replacement
                                        .iter()
                                        .map(|t| {
                                            if t.0 == "STRING" {
                                                format!("\"{}\"", t.1)
                                            } else {
                                                t.1.clone()
                                            }
                                        })
                                        .collect::<Vec<_>>()
                                        .join(" ");

                                    let loc = token.2.clone().or(call_loc.clone());
                                    expanded_tokens.push(Token(
                                        "STRING".to_string(),
                                        format!("\"{}\"", joined),
                                        loc,
                                    ));
                                } else {
                                    return Err(Error::new(
                                        "PreprocessorError",
                                        &format!("Unknown macro argument $!{}", next_token.1),
                                        &self.file_path,
                                    ));
                                }
                            } else {
                                let next_token = &body[body_i];
                                if next_token.0 != "IDENTIFIER" {
                                    return Err(Error::new(
                                        "PreprocessorError",
                                        &format!("Expected IDENTIFIER after $, got {}", next_token.0),
                                        &self.file_path,
                                    ));
                                }

                                if let Some(replacement) = replacement_map.get(next_token.1.as_str()) {
                                    expanded_tokens.extend(replacement.clone());
                                } else {
                                    return Err(Error::new(
                                        "PreprocessorError",
                                        &format!("Unknown macro argument ${}", next_token.1),
                                        &self.file_path,
                                    ));
                                }
                            }
                        } else if token.0 == "STRING" && !token.1.is_empty() {
                            expanded_tokens.push(expand_macros_in_default_string(token, &replacement_map));
                        } else {
                            expanded_tokens.push(token.clone());
                        }

                        body_i += 1;
                    }

                    if depth >= MAX_MACRO_RECURSION_DEPTH {
                        result.push(Token("IDENTIFIER".into(), "throw".into(), call_loc.clone()));
                        result.push(Token("STRING".into(), "\"Maximum recursion depth exceeded in macro invocation\"".into(), call_loc.clone()));
                        result.push(Token("IDENTIFIER".into(), "from".into(), call_loc.clone()));
                        result.push(Token("STRING".into(), "\"RecursionError\"".into(), call_loc.clone()));
                    } else {
                        self.mangle_context.enter_scope();
                        let recursively_expanded = self.expand_tokens_with_macros(&expanded_tokens, skipping, call_loc.clone(), depth + 1, &current_dir)?;
                        let mut new_preprocessor = Preprocessor::new(
                            self.lib_dir.clone(),
                            &self.file_path.clone(),
                        );
                        let recursively_expanded_preprocessor = new_preprocessor.process(recursively_expanded, &current_dir)?;
                        let mangled = self.mangle_tokens(&recursively_expanded_preprocessor);
                        result.extend(mangled);
                        self.mangle_context.exit_scope();
                    }

                    continue;
                } else {
                    return Err(Error::new(
                        "PreprocessorError",
                        &format!("Macro {} not found", macro_name),
                        &self.file_path,
                    ));
                }
            } else if !skipping {
                let mut token = tokens[i].clone();

                let alias = self.aliases.iter()
                    .find_map(|(k, v)| if k.0 == token.0 && k.1 == token.1 { Some(v) } else { None });

                if let Some(alias) = alias {
                    token = alias.clone();
                }

                if token.0 == "IDENTIFIER" {
                    if let Some(def) = self.defines.get(&token.1) {
                        token = def.clone();
                    }
                }

                result.push(token);
            }

            i += 1;
        }

        Ok(result)
    }

    fn mangle_tokens(&mut self, tokens: &[Token]) -> Vec<Token> {
        let mut result = Vec::new();

        let mut i = 0;
        while i < tokens.len() {
            let token = &tokens[i];

            if token.0 == "IDENTIFIER" {
                let og_token = &token.1;

                if og_token == "_" {
                    result.push(token.clone());
                    i += 1;
                    continue;
                }

                let next_token = tokens.get(i + 1);

                let defines_identifier = next_token.map_or(false, |t| t.1 == ":" || t.1 == ":=" || t.1 == "in");
                let not_keyword = !KEYWORDS.contains(&og_token.as_str());

                if defines_identifier && not_keyword {
                    let uid = if let Some(loc) = &token.2 {
                        format!("{}{}", loc.line_number, loc.range.0)
                    } else {
                        let id = self.mangle_context.counter;
                        format!("gen{}", id)
                    };

                    let mangled = format!("__mangle_{}_{}", uid, og_token);
                    self.mangle_context.mangle(og_token, &mangled);
                    result.push(Token("IDENTIFIER".to_string(), mangled, token.2.clone()));
                    i += 1;
                    continue;
                }

                if let Some(mangled) = self.mangle_context.get_mangled(og_token) {
                    result.push(Token("IDENTIFIER".to_string(), mangled.clone(), token.2.clone()));
                    i += 1;
                    continue;
                }
            }

            result.push(token.clone());
            i += 1;
        }

        result
    }
}

fn expand_macros_in_default_string(
    token: &Token,
    replacement_map: &HashMap<&str, Vec<Token>>,
) -> Token {
    if token.0 != "STRING" {
        return token.clone();
    }

    let val = &token.1;
    if val.len() < 3 || !(val.starts_with('f') || val.starts_with('F')) {
        return token.clone();
    }

    let quote_char = val.chars().nth(1).unwrap_or('\0');
    if quote_char != '"' && quote_char != '\'' {
        return token.clone();
    }

    let string_inside = &val[2..val.len() - 1];
    let mut replaced = String::new();
    let mut rest = string_inside;

    while let Some(start_pos) = rest.find('$') {
        replaced.push_str(&rest[..start_pos]);
        rest = &rest[start_pos + 1..];

        if let Some(end_pos) = rest.find('$') {
            let key = &rest[..end_pos];
            rest = &rest[end_pos + 1..];

            if let Some(repl) = replacement_map.get(key) {
                replaced.push('{');
                replaced.push_str(
                    &repl.iter().map(|t| t.1.clone()).collect::<Vec<_>>().join(" ")
                );
                replaced.push('}');
            } else {
                replaced.push('$');
                replaced.push_str(key);
                replaced.push('$');
            }
        } else {
            replaced.push('$');
            replaced.push_str(rest);
            rest = "";
        }
    }

    replaced.push_str(rest);

    Token(
        "STRING".to_string(),
        format!("f{}{}{}", quote_char, replaced, quote_char),
        token.2.clone(),
    )
}
