use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};
use crate::lexer::Lexer;
use crate::env::runtime::errors::Error;
use crate::env::runtime::tokens::{Token, Location};
use crate::env::runtime::utils::{to_static, KEYWORDS, fix_path};
use crate::env::runtime::precompile::precompile;
use crate::env::runtime::types::Float;
use std::ops::{Add, Sub, Mul, Div, Rem};

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
        for scope in self.mangled_names.iter().rev() {
            if let Some(mangled) = scope.get(original) {
                return Some(mangled);
            }
        }
        None
    }
}

#[derive(Eq, Hash, PartialEq, Clone, Copy)]
enum BracketType {
    Round,  // ()
    Square, // []
    Curly,  // {}
    Angle,  // <>
}

impl BracketType {
    fn from_open(c: &str) -> Option<Self> {
        match c {
            "(" => Some(Self::Round),
            "[" => Some(Self::Square),
            "{" => Some(Self::Curly),
            "<" => Some(Self::Angle),
            _ => None,
        }
    }

    fn from_close(c: &str) -> Option<Self> {
        match c {
            ")" => Some(Self::Round),
            "]" => Some(Self::Square),
            "}" => Some(Self::Curly),
            ">" => Some(Self::Angle),
            _ => None,
        }
    }

    fn matching_open(&self) -> &'static str {
        match self {
            Self::Round => "(",
            Self::Square => "[",
            Self::Curly => "{",
            Self::Angle => "<",
        }
    }

    fn matching_close(&self) -> &'static str {
        match self {
            Self::Round => ")",
            Self::Square => "]",
            Self::Curly => "}",
            Self::Angle => ">",
        }
    }

    fn is_bracket_type_start(ty: &str) -> bool {
        match ty {
            "(" | "[" | "{" | "<" => true,
            _ => false,
        }
    }

    fn display(&self) -> &'static str {
        match self {
            Self::Round => "()",
            Self::Square => "[]",
            Self::Curly => "{}",
            Self::Angle => "<>",
        }
    }
}

#[derive(Debug, Clone)]
struct MacroMetadata {
    args: Vec<(String, Option<Token>)>, // (ARG_NAME, DEFAULT_VALUE)
    body: Vec<Token>, // BODY_TOKENS
    grouping_enabled: bool, // true if grouping is enabled
    mangling_enabled: bool, // true if mangling is enabled
    inherit_location: bool, // true if macro should inherit location from the call site
    contextual: bool,       // true if macro is contextual (add $line, $file, etc.)
    deprecated: (bool, Option<String>), // (is_deprecated, reason)
    raw: bool, // true if macro is raw (no processing)
}

pub struct Preprocessor {
    lib_dir: PathBuf,
    defines: HashMap<String, Token>, // IDENTIFIER -> TOKEN
    aliases: HashMap<Token, Token>, // TOKEN -> ALIAS_TOKEN
    macros: HashMap<String, HashMap<BracketType, MacroMetadata>>, // (MACRO_NAME, BRACKET_TYPE) -> (METADATA)
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
                if i >= tokens.len() || tokens[i].0 == "EOF" {
                    return Err(create_err(
                        "Unexpected end after '#'",
                        &tokens[i - 1],
                    ));
                }

                if &tokens[i].0 != "IDENTIFIER" {
                    return Err(create_err("Expected IDENTIFIER for preprocessor directive name", &tokens[i]));
                }
                
                let directive = &tokens[i].1;
                i += 1;

                match directive.as_str() {
                    "macro" => {
                        if i >= tokens.len() {
                            return Err(create_err("#macro missing NAME", &tokens[i]));
                        }

                        let mut macro_metadata = MacroMetadata {
                            args: Vec::new(),
                            body: Vec::new(),
                            grouping_enabled: true,
                            mangling_enabled: true,
                            inherit_location: true,
                            contextual: false,
                            deprecated: (false, None),
                            raw: false,
                        };

                        while tokens[i].0 == "OPERATOR" && tokens[i].1 == "#" {
                            i += 1;
                            if i >= tokens.len() || tokens[i].0 == "EOF" {
                                return Err(create_err(
                                    "Unexpected end after '#'",
                                    &tokens[i - 1],
                                ));
                            }
                            match tokens[i].1.as_str() {
                                "group" => {
                                    macro_metadata.grouping_enabled = true;
                                }
                                "mangle" => {
                                    macro_metadata.mangling_enabled = true;
                                }
                                "no" => {
                                    i += 1;
                                    if i >= tokens.len() {
                                        return Err(create_err("Expected directive after 'no'", &tokens[i]));
                                    }
                                    if tokens[i].1 == "-" {
                                        i += 1;
                                    } else {
                                        return Err(create_err("Expected '-' after 'no'", &tokens[i]));
                                    }
                                    if i >= tokens.len() {
                                        return Err(create_err("Expected directive after 'no'", &tokens[i]));
                                    }
                                    match tokens[i].1.as_str() {
                                        "mangle" => {
                                            macro_metadata.mangling_enabled = false;
                                        }
                                        "group" => {
                                            macro_metadata.grouping_enabled = false;
                                        }
                                        "inherit" => {
                                            i += 1;
                                            if i >= tokens.len() {
                                                return Err(create_err("Expected directive after 'no inherit'", &tokens[i]));
                                            }
                                            if tokens[i].1 == "-" {
                                                i += 1;
                                            } else {
                                                return Err(create_err("Expected '-' after 'no inherit'", &tokens[i]));
                                            }
                                            if tokens[i].1 == "loc" {
                                                macro_metadata.inherit_location = false;
                                            } else {
                                                return Err(create_err(&format!("Unknown 'no-inherit' directive: {}", tokens[i].1), &tokens[i]));
                                            }
                                        }
                                        "contextual" => {
                                            macro_metadata.contextual = false;
                                        }
                                        "deprecated" => {
                                            macro_metadata.deprecated.0 = false;
                                            macro_metadata.deprecated.1 = None;
                                        }
                                        "raw" => {
                                            macro_metadata.raw = false;
                                        }
                                        _ => {
                                            return Err(create_err(&format!("Unknown 'no' directive: {}", tokens[i].1), &tokens[i]));
                                        }
                                    }
                                }
                                "inherit" => {
                                    i += 1;
                                    if i >= tokens.len() {
                                        return Err(create_err("Expected directive after 'inherit'", &tokens[i]));
                                    }
                                    if tokens[i].1 == "-" {
                                        i += 1;
                                    } else {
                                        return Err(create_err("Expected '-' after 'inherit'", &tokens[i]));
                                    }
                                    if tokens[i].1 == "loc" {
                                        macro_metadata.inherit_location = true;
                                    } else {
                                        return Err(create_err(&format!("Unknown 'inherit' directive: {}", tokens[i].1), &tokens[i]));
                                    }
                                }
                                "inline" => {
                                    macro_metadata.grouping_enabled = false;
                                    macro_metadata.mangling_enabled = false;
                                }
                                "contextual" => {
                                    macro_metadata.contextual = true;
                                }
                                "deprecated" => {
                                    macro_metadata.deprecated.0 = true;
                                    i += 1;
                                    if i + 1 < tokens.len() && tokens[i].1 == "(" {
                                        i += 1;
                                        let msg = if i + 1 < tokens.len() && tokens[i].0 == "STRING" {
                                            i += 1;
                                            Some(tokens[i].1.clone())
                                        } else {
                                            return Err(create_err("Expected string after #deprecated(", &tokens[i]));
                                        };
                                        if tokens[i].1 != ")" {
                                            return Err(create_err("Expected ')' after #deprecated message", &tokens[i]));
                                        }
                                        macro_metadata.deprecated.1 = msg;
                                    } else {
                                        i -= 1
                                    }
                                }
                                "raw" => {
                                    macro_metadata.raw = true;
                                }
                                _ => {
                                    return Err(create_err(&format!("Unknown macro directive: {}", tokens[i].1), &tokens[i]));
                                }
                            }
                            i += 1;
                        }

                        let name_token = &tokens[i];

                        i += 1;

                        if name_token.0 != "IDENTIFIER" {
                            return Err(create_err(&format!("#macro NAME must be IDENTIFIER, got {}", name_token.0), &tokens[i]));
                        }

                        if &tokens[i].1 == "!" {
                            i += 1;
                        }

                        let mut args: Vec<(String, Option<Token>)> = Vec::new();

                        let mut bracket = BracketType::Round;
                        if i < tokens.len()
                            && matches!(tokens[i], Token(_, ref b, _) if BracketType::is_bracket_type_start(&b))
                        {
                            bracket = BracketType::from_open(&tokens[i].1).unwrap();
                            let opener = bracket.matching_open();
                            let closer = bracket.matching_close();

                            i += 1;

                            let mut stack = vec![bracket];

                            while i < tokens.len() && !stack.is_empty() {
                                if matches!(tokens[i], Token(ref a, ref b, _) if a == "OPERATOR" && b == "$") {
                                    i += 1;

                                    if i >= tokens.len() || tokens[i].0 != "IDENTIFIER" {
                                        return Err(create_err("Expected IDENTIFIER after $ in macro argument", &tokens[i]));
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
                                        && BracketType::from_open(&tokens[i + 1].1).is_some()
                                    {
                                        let def_bracket = BracketType::from_open(&tokens[i + 1].1).unwrap();
                                        let def_open = def_bracket.matching_open();
                                        let def_close = def_bracket.matching_close();

                                        i += 2;
                                        let mut depth = 1;
                                        let mut val_tokens = Vec::new();

                                        while i < tokens.len() {
                                            if tokens[i].1 == def_open {
                                                depth += 1;
                                            } else if tokens[i].1 == def_close {
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
                                            return Err(create_err("Unclosed default value in macro argument", &tokens[i]));
                                        }

                                        default_val = val_tokens.into_iter().next();
                                    }

                                    args.push((name, default_val));

                                    if is_variadic && i < tokens.len()
                                        && matches!(tokens[i], Token(ref a, ref b, _) if a == "SEPARATOR" && b == ",")
                                    {
                                        return Err(create_err("Variadic argument must be last", &tokens[i]));
                                    }

                                    if i < tokens.len()
                                        && matches!(tokens[i], Token(ref a, ref b, _) if a == "SEPARATOR" && b == ",")
                                    {
                                        i += 1;
                                    }
                                }
                                else if let Some(bt) = BracketType::from_open(&tokens[i].1) {
                                    stack.push(bt);
                                    i += 1;
                                }
                                else if let Some(bt) = BracketType::from_close(&tokens[i].1) {
                                    if let Some(top) = stack.last() {
                                        if *top == bt {
                                            stack.pop();
                                            i += 1;
                                            if stack.is_empty() {
                                                break;
                                            }
                                        } else {
                                            return Err(create_err(&format!("Mismatched bracket: expected {}, got {}", top.matching_close(), tokens[i].1), &tokens[i]));
                                        }
                                    } else {
                                        return Err(create_err(&format!("Unexpected closing bracket {}", tokens[i].1), &tokens[i]));
                                    }
                                }
                                else {
                                    i += 1;
                                }
                            }

                            if !stack.is_empty() {
                                return Err(create_err(&format!("Unclosed {}...{} in #macro", opener, closer), &tokens[i]));
                            }
                        }

                        if tokens[i].0 != "SEPARATOR" || tokens[i].1 != ":" {
                            return Err(create_err("Expected ':' to start macro body", &tokens[i]));
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

                        macro_metadata.args = args;
                        macro_metadata.body = body_tokens;

                        self.macros.entry(name_token.1.clone()).or_default().insert(bracket, macro_metadata);
                        continue;
                    }

                    "endmacro" => {
                        return Err(create_err("#endmacro without matching #macro", &tokens[i]));
                    }

                    "precompile" => {
                        if i >= tokens.len() {
                            return Err(create_err("#precompile missing expression", &tokens[i]));
                        }
                        if tokens[i].1 == ":" {
                            i += 1;
                        }

                        let mut precompile_tokens = Vec::new();

                        while i < tokens.len() {
                            if tokens[i].1 == "#" && i + 1 < tokens.len() && tokens[i + 1].1 == "endprecompile" {
                                i += 2;
                                break;
                            }
                            precompile_tokens.push(tokens[i].clone());
                            i += 1;
                        }

                        let precompiled = precompile(precompile_tokens)?;
                        result.extend(precompiled);
                        continue;
                    }

                    "endprecompile" => {
                        return Err(create_err("#endprecompile without matching #precompile", &tokens[i]));
                    }

                    "define" => {
                        if i >= tokens.len() {
                            return Err(create_err("#define missing NAME", &tokens[i]));
                        }

                        let name_token = &tokens[i];
                        i += 1;

                        if name_token.0 != "IDENTIFIER" {
                            return Err(create_err(&format!("#define NAME must be IDENTIFIER, got {}", name_token.0), &tokens[i]));
                        }

                        if i >= tokens.len() {
                            return Err(create_err("#define missing VALUE", &tokens[i]));
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
                            return Err(create_err("#undef missing NAME", &tokens[i]));
                        }
                        
                        let name_token = &tokens[i];
                        i += 1;
                        
                        if name_token.0 != "IDENTIFIER" {
                            return Err(create_err("#undef NAME must be IDENTIFIER", &tokens[i]));
                        }
                        
                        self.defines.remove(&name_token.1);
                        continue;
                    }

                    "ifdef" => {
                        if i >= tokens.len() {
                            return Err(create_err("#ifdef missing NAME", &tokens[i]));
                        }
                        
                        let name_token = &tokens[i];
                        i += 1;
                        
                        if name_token.0 != "IDENTIFIER" {
                            return Err(create_err("#ifdef NAME must be IDENTIFIER", &tokens[i]));
                        }
                        
                        let cond = self.defines.contains_key(&name_token.1);
                        skip_stack.push(skipping);
                        skipping = !cond;
                        continue;
                    }

                    "ifndef" => {
                        if i >= tokens.len() {
                            return Err(create_err("#ifndef missing NAME", &tokens[i]));
                        }
                        
                        let name_token = &tokens[i];
                        i += 1;
                        
                        if name_token.0 != "IDENTIFIER" {
                            return Err(create_err("#ifndef NAME must be IDENTIFIER", &tokens[i]));
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
                            return Err(create_err("#alias missing tokens", &tokens[i]));
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
                            return Err(create_err("#unalias missing token", &tokens[i]));
                        }
                        
                        let token = tokens[i].clone();
                        i += 1;

                        self.aliases.remove(&token);
                        continue;
                    }

                    "include" => {
                        if i >= tokens.len() {
                            return Err(create_err("#include missing path", &tokens[i]));
                        }

                        let included_path = if tokens[i].0 == "OPERATOR" && tokens[i].1 == "<" {
                            i += 1;
                            let mut path_parts = Vec::new();

                            while i < tokens.len() && !(tokens[i].0 == "OPERATOR" && tokens[i].1 == ">") {
                                path_parts.push(tokens[i].1.clone());
                                i += 1;
                            }

                            if i >= tokens.len() || tokens[i].1 != ">" {
                                return Err(create_err("Unclosed '<...>' in #include", &tokens[i - 1]));
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
                            return Err(create_err("Invalid include path format", &tokens[i]));
                        };

                        if !included_path.exists() {
                            return Err(create_err(&format!("Included file does not exist: {}", included_path.display()), &tokens[i]));
                        }

                        let included_in = self._process(vec![
                            Token("OPERATOR".to_string(), "#".to_string(), last_normal_token_location.clone()),
                            Token("IDENTIFIER".to_string(), "define".to_string(), last_normal_token_location.clone()),
                            Token("IDENTIFIER".to_string(), "INCLUDE".to_string(), last_normal_token_location.clone()),
                            Token("STRING".to_string(), included_path.display().to_string(), last_normal_token_location.clone()),
                        ], included_path.parent().unwrap_or(current_dir))?;
                        let included_out = vec![
                            Token("OPERATOR".to_string(), "#".to_string(), last_normal_token_location.clone()),
                            Token("IDENTIFIER".to_string(), "undef".to_string(), last_normal_token_location.clone()),
                            Token("IDENTIFIER".to_string(), "INCLUDE".to_string(), last_normal_token_location.clone()),
                        ];

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

                                let lexer = Lexer::new(&content, to_static(included_path.display().to_string()));
                                let mut toks = lexer.tokenize()
                                    .into_iter()
                                    .filter(|tok| tok.0 != "WHITESPACE")
                                    .collect::<Vec<_>>();
                                    
                                if let Some(last_token) = toks.last() {
                                    if last_token.0 != "EOF" {
                                        return Err(create_err(&format!("File '{}' not closed properly", file_path.display()), &tokens[i]));
                                    } else {
                                        toks.pop();
                                    }
                                }

                                result.extend(included_in.clone());
                                let included = self._process(toks, included_path.parent().unwrap_or(current_dir))?;
                                result.extend(included);
                                result.extend(self._process(included_out.clone(), included_path.parent().unwrap_or(current_dir))?);
                            }
                        } else {
                            let content = fs::read_to_string(&included_path).map_err(|e| Error::new(
                                "PreprocessorIOError",
                                &format!("Failed to read included file '{}': {}", included_path.display(), e),
                                &self.file_path,
                            ))?;

                            let lexer = Lexer::new(&content, to_static(self.file_path.clone()));
                            let mut toks = lexer.tokenize()
                                .into_iter()
                                .filter(|tok| tok.0 != "WHITESPACE")
                                .collect::<Vec<_>>();
                                
                            if toks.last().map_or(false, |t| t.0 == "EOF") {
                                toks.pop();
                            }

                            result.extend(included_in);
                            let included = self._process(toks, included_path.parent().unwrap_or(current_dir))?;
                            result.extend(included);
                            result.extend(self._process(included_out, included_path.parent().unwrap_or(current_dir))?);
                        }
                        continue;
                    }

                    "config" => {
                        if i + 1 >= tokens.len() {
                            return Err(create_err("#config requires a key and a value or reset key", &tokens[i]));
                        }

                        let key = tokens[i].1.clone();
                        i += 1;

                        if key == "reset" {
                            if i >= tokens.len() {
                                return Err(create_err("#config reset requires a key", &tokens[i]));
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
                                return Err(create_err("Missing '=' after #config key", &tokens[i]));
                            }
                            i += 1;

                            if i >= tokens.len() {
                                return Err(create_err("#config requires a value after '='", &tokens[i]));
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
                            return Err(create_err("#syntax requires a pattern and a replacement", &tokens[i]));
                        }

                        let _pattern = &tokens[i].1;
                        i += 1;

                        if i >= tokens.len() || tokens[i].1 != "->" {
                            return Err(create_err("Expected '->' after #syntax pattern", &tokens[i]));
                        }
                        i += 1;

                        if i >= tokens.len() || tokens[i].0 != "STRING" {
                            return Err(create_err("#syntax requires a STRING replacement", &tokens[i]));
                        }

                        let _replacement = tokens[i].1.clone();
                        i += 1;
                        continue;
                    }

                    "[" | "!" => {
                        let is_negated = tokens[i - 1].1 == "!";

                        if is_negated {
                            if i >= tokens.len() || tokens[i].1 != "[" {
                                return Err(create_err("Expected '[' after '!'", &tokens[i]));
                            }
                            i += 1;
                        }

                        if i >= tokens.len() {
                            return Err(create_err("Expected key inside brackets", &tokens[i]));
                        }

                        let key = tokens[i].1.clone();
                        i += 1;

                        if i >= tokens.len() || tokens[i].1 != "]" {
                            return Err(create_err("Expected closing ']' after key", &tokens[i]));
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
                        return Err(create_err(&format!("Unknown preprocessor directive: {}", directive), &tokens[i - 1]));
                    }
                }
            } else {
                if !skipping 
                    && i + 2 < tokens.len()
                    && matches!(tokens[i], Token(ref a, _, _) if a == "IDENTIFIER")
                    && matches!(tokens[i + 1], Token(ref a, ref b, _) if a == "OPERATOR" && b == "!")
                    && matches!(tokens[i + 2], Token(_, ref b, _) if BracketType::is_bracket_type_start(&b))
                {
                    // macro_name!(args...)
                    // definitely not stolen from Rust
                    let macro_name = &tokens[i].1;
                    let call_loc = tokens[i].2.clone();
                    let bracket = BracketType::from_open(&tokens[i + 2].1).unwrap();

                    i += 3;

                    let (res, offset) = self.expand_macro(macro_name, bracket,call_loc.clone(), i, &tokens, current_dir, skipping, 0)?;
                    i += offset;
                    result.extend(res);
                } else if !skipping
                    && i + 3 < tokens.len() && i != 0
                    && matches!(tokens[i - 1], Token(ref a, _, _) if a != "OPERATOR")
                    && matches!(tokens[i], Token(ref a, _, _) if a == "NUMBER")
                    && matches!(tokens[i + 1], Token(ref a, _, _) if a == "OPERATOR")
                    && matches!(tokens[i + 2], Token(ref a, _, _) if a == "NUMBER")
                    && matches!(tokens[i + 3], Token(ref a, _, _) if a != "OPERATOR")
                {
                    let mut aliased = Vec::new();
                    let mut has_alias = false;
                    for j in 0..3 {
                        let mut t = tokens[i + j].clone();
                        if let Some(alias) = self.aliases.iter()
                            .find_map(|(k, v)| if k.0 == t.0 && k.1 == t.1 { Some(v) } else { None }) {
                            t = alias.clone();
                            has_alias = true;
                        }
                        aliased.push(t);
                    }

                    if has_alias {
                        result.extend(aliased);
                        i += 3;
                        continue;
                    }

                    let left  = &tokens[i].1;
                    let op    = &tokens[i + 1].1;
                    let right = &tokens[i + 2].1;

                    fn parse_number(s: &str) -> Option<(Option<i64>, Option<Float>)> {
                        if s.starts_with("0x") {
                            i64::from_str_radix(&s[2..], 16).ok().map(|v| (Some(v), Some(Float::from_f64(v as f64))))
                        } else if s.starts_with("0o") {
                            i64::from_str_radix(&s[2..], 8).ok().map(|v| (Some(v), Some(Float::from_f64(v as f64))))
                        } else if s.starts_with("0b") {
                            i64::from_str_radix(&s[2..], 2).ok().map(|v| (Some(v), Some(Float::from_f64(v as f64))))
                        } else if s.contains('.') {
                            Some((None, Some(Float::from_f64(s.parse::<f64>().ok()?))))
                        } else {
                            s.parse::<i64>().ok().map(|v| (Some(v), Some(Float::from_f64(v as f64))))
                        }
                    }

                    let folded: Option<String> = if "+-*/%^".contains(op.as_str()) {
                        if let (Some((li, lf)), Some((ri, rf))) = (parse_number(left), parse_number(right)) {
                            if let (Some(l), Some(r)) = (li, ri) {
                                match op.as_str() {
                                    "+" => l.checked_add(r).map(|v| v.to_string()),
                                    "-" => l.checked_sub(r).map(|v| v.to_string()),
                                    "*" => if l != 6 && r != 9 { l.checked_mul(r).map(|v| v.to_string()) } else { None },
                                    "/" => {
                                        if r == 0 {
                                            None
                                        } else {
                                            Float::from_f64(l as f64)
                                                .div(&Float::from_f64(r as f64))
                                                .and_then(|fval| {
                                                    if fval.is_recurring() || fval.is_irrational() {
                                                        Err(-1)
                                                    } else if fval.is_integer_like() {
                                                        Ok(fval.to_f64()?.to_string())
                                                    } else {
                                                        Ok(fval.to_string())
                                                    }
                                                })
                                                .ok()
                                        }
                                    }
                                    "%" => if r != 0 { Some((l % r).to_string()) } else { None },
                                    "^" => i64::checked_pow(l, r as u32).map(|v| v.to_string()),
                                    _ => None,
                                }
                            } else if let (Some(lf), Some(rf)) = (lf, rf) {
                                let res: Result<Float, i16> = match op.as_str() {
                                    "+" => lf.add(rf),
                                    "-" => lf.sub(rf),
                                    "*" => lf.mul(rf),
                                    "/" => if rf.is_zero() { Err(-1) } else { lf.div(rf) },
                                    "%" => if rf.is_zero() { Err(-1) } else { lf.rem(rf) },
                                    "^" => lf.pow(&rf),
                                    _ => Err(-1),
                                };

                                match res {
                                    Ok(f) if !f.is_recurring() && !f.is_irrational() => Some(f.to_string()),
                                    _ => None,
                                }
                            } else { None }
                        } else { None }
                    } else { None };

                    if let Some(val) = folded {
                        result.push(Token("NUMBER".into(), val, tokens[i].2.clone()));
                    } else {
                        result.push(tokens[i].clone());
                        result.push(tokens[i + 1].clone());
                        result.push(tokens[i + 2].clone());
                    }

                    i += 3;
                    continue;
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
        current_dir: &Path,
        depth: usize,
    ) -> Result<Vec<Token>, Error> {
        let mut result = Vec::new();
        let mut i = 0;

        if depth > MAX_MACRO_RECURSION_DEPTH {
            result.push(Token("IDENTIFIER".to_string(), "throw".to_string(), call_loc.clone()));
            result.push(Token("STRING".to_string(), "Macro recursion limit exceeded".to_string(), call_loc.clone()));
            result.push(Token("IDENTIFIER".to_string(), "from".to_string(), call_loc.clone()));
            result.push(Token("STRING".to_string(), "RecursionError".to_string(), call_loc.clone()));
        }

        while i < tokens.len() {
            if !skipping
                && i + 2 < tokens.len()
                && matches!(tokens[i], Token(ref a, _, _) if a == "IDENTIFIER")
                && matches!(tokens[i + 1], Token(ref a, ref b, _) if a == "OPERATOR" && b == "!")
                && matches!(tokens[i + 2], Token(_, ref b, _) if BracketType::is_bracket_type_start(&b))
            {
                let macro_name = &tokens[i].1;
                let bracket = BracketType::from_open(&tokens[i + 2].1).unwrap();
                i += 3;
                let (res, offset) = self.expand_macro(macro_name, bracket, call_loc.clone(), i, tokens, current_dir, skipping, depth + 1)?;
                i += offset;
                result.extend(res);
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

        
        let processed_result = self._process(result, current_dir)?;
        Ok(processed_result)
    }

    fn expand_macro(
        &mut self,
        macro_name: &str,
        bracket: BracketType,
        call_loc: Option<Location>,
        mut i: usize,
        tokens: &[Token],
        current_dir: &Path,
        skipping: bool,
        depth: usize,
    ) -> Result<(Vec<Token>, usize), Error> {
        let mut result = Vec::new();
        let start_i = i;

        let name = macro_name.to_string();

        let meta = self.macros.get(&name).map(|br_map| {
            br_map.get(&bracket)
        }).flatten().cloned().ok_or_else(|| {
            match self.macros.get(&name) {
                Some(br_map) => {
                    let existing_bracket = br_map.keys().next().unwrap();
                    create_err(
                        &format!("Macro {} found but with different brackets ('{}')", macro_name, existing_bracket.display()),
                        &tokens[i - 3],
                    )
                }
                None => create_err(
                    &format!("Macro {} not found", macro_name),
                    &tokens[i - 3],
                ),
            }
        })?;

        let grouping_enabled = meta.grouping_enabled;
        let mangling_enabled = meta.mangling_enabled;
        let raw = meta.raw;
        let inherit_location = meta.inherit_location;
        let contextual = meta.contextual;
        let param_names = meta.args.clone();
        let body = meta.body.clone();

        if raw {
            if inherit_location {
                for mut t in body.iter().cloned() {
                    t.2 = call_loc.clone();
                    result.push(t);
                }
            } else {
                result.extend(body.iter().cloned());
            }
            return Ok((result, 0));
        }

        let bracket_start = bracket.matching_open();
        let bracket_end = bracket.matching_close();

        let mut paren_count = 1;
        let mut call_args_tokens = Vec::new();

        while i < tokens.len() && paren_count > 0 {
            let token = &tokens[i];
            if token.1 == bracket_start {
                paren_count += 1;
            } else if token.1 == bracket_end {
                paren_count -= 1;
                if paren_count == 0 {
                    break;
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
                return Err(create_err(&format!(
                        "Macro {} expected at least {} arguments before variadic, got {}",
                        macro_name,
                        pos,
                        args_values.len()
                    ), &tokens[i - 1]));
            }
        } else if args_values.len() > param_names.len() {
            return Err(create_err(&format!(
                    "Macro {} expected at most {} arguments, got {}",
                    macro_name,
                    param_names.len(),
                    args_values.len()
                ), &tokens[i - 1]));
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
                    return Err(create_err(&format!("Missing required macro argument ${}", name), &tokens[i - 1]));
                }
            }
        }

        let mut expanded_tokens = Vec::new();
        let mut body_i = 0;

        while body_i < body.len() {
            let token = &body[body_i];

            if contextual && token.0 == "OPERATOR" && token.1 == "$" {
                if body_i + 1 < body.len() {
                    let next_token = &body[body_i + 1];
                    if next_token.0 == "IDENTIFIER" {
                        let line = call_loc.as_ref().map(|loc| loc.range.0).unwrap_or(0);
                        let file = fix_path(call_loc.as_ref().map(|loc| loc.file.as_str()).unwrap_or("<unknown>").to_string());
                        let column = call_loc.as_ref().map(|loc| loc.range.1).unwrap_or(0);

                        let replacement_token = match next_token.1.as_str() {
                            "line" => Some(Token("NUMBER".to_string(), format!("{}", line), call_loc.clone())),
                            "file" => Some(Token("STRING".to_string(), format!("\"{}\"", file), call_loc.clone())),
                            "column" => Some(Token("NUMBER".to_string(), format!("{}", column), call_loc.clone())),
                            _ => None,
                        };
                        if let Some(rep) = replacement_token {
                            expanded_tokens.push(rep);
                            body_i += 2;
                            continue;
                        }
                    }
                }
            }

            if token.0 == "OPERATOR" && token.1 == "$" {
                body_i += 1;
                if body_i >= body.len() {
                    return Err(create_err("Unexpected end of macro body after $", &tokens[i]));
                }

                let Token(a, b, _) = &body[body_i];

                if a == "OPERATOR" && b == "!" {
                    body_i += 1;
                    if body_i >= body.len() {
                        return Err(create_err("Expected identifier or separator after $!", &tokens[i]));
                    }

                    let mut sep = ",";

                    if body[body_i].0 != "IDENTIFIER" {
                        sep = &body[body_i].1;
                        body_i += 1;
                        if body_i >= body.len() {
                            return Err(create_err("Expected identifier after separator in $!", &tokens[i]));
                        }
                    }

                    if body[body_i].0 != "IDENTIFIER" {
                        return Err(create_err(&format!("Expected IDENTIFIER after $!, got {}", body[body_i].0), &tokens[i]));
                    }

                    let arg_name = if body[body_i].1.ends_with("...") {
                        &body[body_i].1[..body[body_i].1.len() - 3]
                    } else {
                        body[body_i].1.as_str()
                    };

                    if let Some(replacement) = replacement_map.get(arg_name) {
                        let joined = join_tokens_sep(replacement, sep);
                        let loc = token.2.clone().or(None);
                        expanded_tokens.push(Token(
                            "STRING".to_string(),
                            format!("\"{}\"", joined),
                            loc,
                        ));
                    } else {
                        return Err(create_err(&format!("Unknown macro argument $!{}", body[body_i].1), &tokens[i]));
                    }
                } else if a == "OPERATOR" && b == "?" {
                    body_i += 1;
                    if body_i >= body.len() {
                        return Err(create_err("Expected identifier or separator after $?", &tokens[i]));
                    }
                    if body[body_i].0 != "IDENTIFIER" {
                        return Err(create_err(&format!("Expected IDENTIFIER after $?, got {}", body[body_i].0), &tokens[i]));
                    }
                    let arg_name = if body[body_i].1.ends_with("...") {
                        &body[body_i].1[..body[body_i].1.len() - 3]
                    } else {
                        body[body_i].1.as_str()
                    };

                    if let Some(replacement) = replacement_map.get(arg_name) {
                        let tokens: Vec<(String, String)> = replacement.into_iter().map(|t| (t.0.clone(), t.1.clone())).collect();
                        let loc = token.2.clone().or(None);
                        for (i, t) in tokens.iter().enumerate() {
                            expanded_tokens.push(Token("SEPARATOR".to_string(), "(".to_string(), loc.clone()));
                            expanded_tokens.push(Token("STRING".to_string(), format!("\"{}\"", t.0), loc.clone()));
                            expanded_tokens.push(Token("SEPARATOR".to_string(), ",".to_string(), loc.clone()));
                            if t.0 == "STRING" {
                                expanded_tokens.push(Token("STRING".to_string(), t.1.clone(), loc.clone()));
                            } else {
                                expanded_tokens.push(Token("STRING".to_string(), format!("\"{}\"", t.1), loc.clone()));
                            }
                            expanded_tokens.push(Token("SEPARATOR".to_string(), ")".to_string(), loc.clone()));
                            if i < tokens.len() - 1 {
                                expanded_tokens.push(Token("SEPARATOR".to_string(), ",".to_string(), loc.clone()));
                            }
                        }
                    } else {
                        return Err(create_err(&format!("Unknown macro argument $?{}", body[body_i].1), &tokens[i]));
                    }
                } else {
                    let next_token = &body[body_i];
                    if next_token.0 != "IDENTIFIER" {
                        return Err(create_err(&format!("Expected IDENTIFIER after $, got {}", next_token.0), &tokens[i]));
                    }

                    if let Some(replacement) = replacement_map.get(next_token.1.as_str()) {
                        expanded_tokens.extend(replacement.clone());
                    } else {
                        return Err(create_err(&format!("Unknown macro argument ${}", next_token.1), &tokens[i]));
                    }
                }
            } else if token.0 == "STRING" && !token.1.is_empty() {
                expanded_tokens.push(expand_macros_in_default_string(token, &replacement_map));
            } else {
                expanded_tokens.push(token.clone());
            }

            body_i += 1;
        }

        if grouping_enabled {
            result.push(Token("SEPARATOR".to_string(), "\\".to_string(), call_loc.clone()));
        }

        if mangling_enabled {
            self.mangle_context.enter_scope();
            let recursively_expanded = self.expand_tokens_with_macros(&expanded_tokens, skipping, call_loc.clone(), &current_dir, depth)?;
            result.extend(self.mangle_tokens(&recursively_expanded));
            self.mangle_context.exit_scope();
        } else {
            let recursively_expanded = self.expand_tokens_with_macros(&expanded_tokens, skipping, call_loc.clone(), &current_dir, depth)?;
            result.extend(recursively_expanded);
        }

        if grouping_enabled {
            result.push(Token("SEPARATOR".to_string(), "\\".to_string(), call_loc.clone()));
        }

        let consumed = i - start_i;

        Ok((result, consumed))
    }

    fn mangle_tokens(&mut self, tokens: &[Token]) -> Vec<Token> {
        let mut declared = HashSet::new();
        let mut i = 0;

        while i < tokens.len() {
            let t = &tokens[i];

            if t.0 == "IDENTIFIER" && !KEYWORDS.contains(&t.1.as_str()) {
                if let Some(next) = tokens.get(i + 1) {
                    match next.1.as_str() {
                        ":=" | "in" => {
                            declared.insert(t.1.clone());
                            i += 2;
                            continue;
                        }
                        ":" => {
                            if let Some(eq) = tokens.get(i + 3) {
                                if eq.1 == "=" {
                                    declared.insert(t.1.clone());
                                    i += 4;
                                    continue;
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }

            if t.0 == "SEPARATOR" && t.1 == "(" {
                let mut level = 1;
                let mut j = i + 1;
                while j < tokens.len() && level > 0 {
                    match tokens[j].1.as_str() {
                        "(" => level += 1,
                        ")" => level -= 1,
                        _ => {}
                    }
                    j += 1;
                }

                if let Some(a) = tokens.get(j) {
                    if a.1 == ":=" || a.1 == "=" {
                        for k in i + 1..j - 1 {
                            let tok = &tokens[k];
                            if tok.0 == "IDENTIFIER" && !KEYWORDS.contains(&tok.1.as_str()) {
                                declared.insert(tok.1.clone());
                            }
                        }
                        i = j + 1;
                        continue;
                    }
                }
            }

            i += 1;
        }

        let mut result = Vec::with_capacity(tokens.len());
        for token in tokens {
            if token.0 == "IDENTIFIER" {
                let og = &token.1;
                if og == "_" || !declared.contains(og) {
                    result.push(token.clone());
                    continue;
                }

                let mangled = self
                    .mangle_context
                    .get_mangled(og)
                    .cloned()
                    .unwrap_or_else(|| {
                        let uid = token
                            .2
                            .as_ref()
                            .map(|loc| format!("{}{}", loc.line_number, loc.range.0))
                            .unwrap_or_else(|| format!("gen{}", self.mangle_context.counter));

                        let name = format!("__mangle_{}_{}", uid, og);
                        self.mangle_context.mangle(og, &name)
                    });

                result.push(Token("IDENTIFIER".to_string(), mangled, token.2.clone()));
            } else {
                result.push(token.clone());
            }
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

fn join_tokens_sep(tokens: &[Token], sep: &str) -> String {
    let mut result = String::new();
    let mut depth = 0;

    for (i, token) in tokens.iter().enumerate() {
        if token.0 == "STRING" {
            let escaped = token.1.replace("\"", "\\\"");
            result.push_str(&escaped);
        } else {
            let is_comma = token.0 == "SEPARATOR" && token.1 == ",";
            if is_comma && depth == 0 {
                result.push_str(sep);
            } else {
                result.push_str(&token.1);
            }
        }

        if token.0 == "SEPARATOR" {
            match token.1.as_str() {
                "(" => depth += 1,
                ")" => if depth > 0 { depth -= 1; },
                _ => {}
            }
        }

        if i + 1 < tokens.len() {
            let next = &tokens[i + 1];
            let skip_space = match (token.1.as_str(), next.1.as_str()) {
                (_, ")") | (_, ",") | ("(", _) => true,
                _ => false,
            };

            if !skip_space {
                result.push(' ');
            }
        }
    }

    result
}

fn create_err(msg: &str, token: &Token) -> Error {
    Error::with_location("PreprocessorError", msg, token.2.clone().unwrap_or(Location {
        file: "<unknown>".to_string(),
        line_string: "".to_string(),
        line_number: 0,
        range: (0, 0),
        lucia_source_loc: "<unknown>".to_string(),
    }))
}