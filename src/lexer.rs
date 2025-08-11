use regex::Regex;
use fancy_regex::Regex as FancyRegex;
use crate::env::runtime::tokens::{Token, Location};
use crate::env::runtime::utils::unescape_string_full;
use std::collections::HashMap;
use std::sync::OnceLock;

#[derive(Debug, Clone)]
pub struct SyntaxRule {
    regex: FancyRegex,
    replacement: String,
}

pub struct Lexer {
    code: String,
    file_path: String,
    line_offsets: Vec<usize>,
    syntax_rules: HashMap<String, SyntaxRule>,
}

impl Lexer {
    pub fn new(code: &str, file_path: &str, syntax_rules_passed: Option<HashMap<String, SyntaxRule>>) -> Self {
        let mut syntax_rules: HashMap<String, SyntaxRule> = HashMap::new();
        if syntax_rules_passed.is_some() {
            syntax_rules = syntax_rules_passed.unwrap();
        }

        let mut line_offsets = Vec::new();
        line_offsets.push(0);
        for (idx, ch) in code.char_indices() {
            if ch == '\n' {
                line_offsets.push(idx + 1);
            }
        }

        Lexer {
            code: code.to_string(),
            file_path: file_path.to_string(),
            line_offsets,
            syntax_rules,
        }
    }

    pub fn get_syntax_rules(&self) -> HashMap<String, SyntaxRule> {
        self.syntax_rules.clone()
    }

    fn extract_syntax_rules(&mut self, code: &str) -> String {
        let syntax_line_re = Regex::new(
            r#"#syntax\s+([a-zA-Z_][\w]*)\s*:\s*"((?:\\.|[^"\\])*)"\s*->\s*"((?:\\.|[^"\\])*)""#
        ).unwrap();
        let remsyntax_re = Regex::new(r#"#remsyntax\s+([a-zA-Z_][\w]*)"#).unwrap();
    
        let rules = &mut self.syntax_rules;
    
        for line in code.lines() {
            for caps in syntax_line_re.captures_iter(line) {
                let name = caps.get(1).unwrap().as_str().to_string();
                let pattern_raw = caps.get(2).unwrap().as_str();
                let replacement_raw = caps.get(3).unwrap().as_str();
    
                let pattern = unescape_string_full(pattern_raw).unwrap_or_else(|_| pattern_raw.to_string());
                let replacement = unescape_string_full(replacement_raw).unwrap_or_else(|_| replacement_raw.to_string());
    
                if pattern.is_empty() || replacement.is_empty() {
                    let error_token = r#"throw "Pattern or replacement cannot be empty" from "LexerError""#;
                    return format!("{}\n{}", error_token, code);
                }
    
                let regex = match FancyRegex::new(&pattern) {
                    Ok(r) => r,
                    Err(_) => {
                        let error_token = r#"throw "Invalid regex pattern in syntax rule" from "LexerError""#;
                        return format!("{}\n{}", error_token, code);
                    }
                };
    
                rules.insert(name, SyntaxRule { regex, replacement });
            }
    
            for caps in remsyntax_re.captures_iter(line) {
                let name = caps.get(1).unwrap().as_str();
                rules.remove(name);
            }
        }
    
        let mut cleaned_lines = Vec::new();
    
        for line in code.lines() {
            let mut last_directive_end = 0;
            for caps in syntax_line_re.captures_iter(line) {
                let full = caps.get(0).unwrap();
                last_directive_end = last_directive_end.max(full.end());
            }
            for caps in remsyntax_re.captures_iter(line) {
                let full = caps.get(0).unwrap();
                last_directive_end = last_directive_end.max(full.end());
            }
    
            let (before, after) = line.split_at(last_directive_end);
    
            let mut after_transformed = after.to_string();
            for rule in rules.values() {
                after_transformed = rule.regex.replace_all(&after_transformed, &rule.replacement).to_string();
            }
    
            let transformed_line = format!("{}{}", before, after_transformed);
    
            let transformed_line_cleaned = syntax_line_re.replace_all(&transformed_line, "").to_string();
            let transformed_line_cleaned = remsyntax_re.replace_all(&transformed_line_cleaned, "").to_string();
    
            cleaned_lines.push(transformed_line_cleaned);
        }
    
        cleaned_lines.join("\n")
    }
    
    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut code = self.code.clone();
        
        let new_code = self.extract_syntax_rules(&code);
        if new_code != code {
            code = new_code;
        }
    
        static REGEX: OnceLock<Regex> = OnceLock::new();
        let regex = REGEX.get_or_init(|| {
            let symbol_operators = [
                "->", ">=", "<=", "==", "!=", "+=", "-=", "*=", "/=", "=>", "=!", "=+", "=-", "=*", "=/", "=^", "=", "<<", ">>", ":=", "^^^", "^^",
                "++", "--", "+", "-", "^", "*", "/", ">", "<", "!", "%", "||", "&&", "??",
                "|", "#", "~", "$", "?", "&", "^=", "%="
            ];

            let word_ops_no_boundaries = [
                "-li"
            ];

            let word_operators = [
                "in", "or", "and", "not", "isnt", "isn't", "is", "xor", "xnor", "nein", "lshift", "rshift", "band", "bor", "bnot",
            ];

            let mut word_with_bounds_sorted = word_operators.iter()
                .map(|s| regex::escape(*s))
                .collect::<Vec<_>>();
            word_with_bounds_sorted.sort_by(|a, b| b.len().cmp(&a.len()));

            let mut word_no_bounds_sorted = word_ops_no_boundaries.iter()
                .map(|s| regex::escape(*s))
                .collect::<Vec<_>>();
            word_no_bounds_sorted.sort_by(|a, b| b.len().cmp(&a.len()));

            let mut symbol_ops_sorted = symbol_operators.iter()
                .map(|s| regex::escape(*s))
                .collect::<Vec<_>>();
            symbol_ops_sorted.sort_by(|a, b| b.len().cmp(&a.len()));

            let word_with_bounds_pattern = format!(r"\b({})\b", word_with_bounds_sorted.join("|"));
            let word_no_bounds_pattern = word_no_bounds_sorted.join("|");
            let symbol_ops_pattern = symbol_ops_sorted.join("|");

            let operator_pattern = format!("{}|{}|{}", word_with_bounds_pattern, word_no_bounds_pattern, symbol_ops_pattern);

            let number_pattern = r"(?x)
                -?
                (
                    \d+\#[0-9a-zA-Z_]+
                    | 0[bB][01]+(?:_[01]+)*
                    | 0[oO][0-7]+(?:_[0-7]+)*
                    | 0[xX][\da-fA-F]+(?:_[\da-fA-F]+)*
                    | \.\d+(?:_\d+)*
                    |                                         
                    (?:\d+(?:_\d)*
                        (?:\.\d+(?:_\d+)*)?
                    )
                    (?:[eE][+-]?\d+)?
                )
            ";

            let token_specifications = [
                ("COMMENT_INLINE", r"<#.*?#>"),
                ("COMMENT_SINGLE", r"//.*"),
                ("COMMENT_MULTI", r"/\*[\s\S]*?\*/"),
                ("RAW_STRING", r#"(?i)([fb]*r[fb]*)("([^"]*)"|'([^']*)')"#),
                ("STRING", r#"(?i)([fb]{0,3})("([^"\\]|\\.)*"|'([^'\\]|\\.)*')"#),
                ("BOOLEAN", r"\b(true|false|null)\b"),
                ("NUMBER", &number_pattern),
                ("OPERATOR", &operator_pattern),
                ("IDENTIFIER", r"\bnon-static\b|[a-zA-Z_]\w*"),
                ("SEPARATOR", r"\.\.\.|\.\.|[(){}\[\];:.,\?\\]"),
                ("WHITESPACE", r"\s+"),
                ("INVALID", r"."),
            ];

            let regex_parts: Vec<String> = token_specifications.iter()
                .map(|(name, pattern)| format!(r"(?P<{}>{})", name, pattern))
                .collect();

            Regex::new(&regex_parts.join("|")).unwrap()
        });
    
        let capture_names: Vec<&str> = regex.capture_names().flatten().collect();
    
        let mut tokens = Vec::new();
        let mut byte_index = 0;
    
        while byte_index < code.len() {
            if let Some(mat) = regex.find_at(&code, byte_index) {
                let token_start = mat.start();
                let token_end = mat.end();
    
                let line_number = match self.line_offsets.binary_search(&token_start) {
                    Ok(line) => line,
                    Err(next_line) => if next_line == 0 { 0 } else { next_line - 1 },
                };
    
                let line_len = code.len();
    
                let line_offset = if line_number < self.line_offsets.len() {
                    self.line_offsets[line_number]
                } else {
                    line_len
                };
    
                let search_slice = if line_offset <= line_len {
                    &code[line_offset..]
                } else {
                    ""
                };
    
                let line_end = match search_slice.find('\n') {
                    Some(pos) => pos + line_offset,
                    None => line_len,
                };
    
                let column_start = token_start.saturating_sub(line_offset);
                let column_end = token_end.saturating_sub(line_offset);
    
                let location = Some(Location::new(
                    self.file_path.clone(),
                    code[line_offset..line_end].to_string(),
                    line_number + 1,
                    (column_start, column_end),
                ));
    
                let match_text = &code[token_start..token_end];
                let cap = regex.captures(match_text).unwrap();
    
                let mut skip_token = false;
                for &name in &capture_names {
                    if let Some(value) = cap.name(name) {
                        let val = value.as_str();
    
                        if name.starts_with("COMMENT_") {
                            skip_token = true;
                            break;
                        }
    
                        if name != "WHITESPACE" && !skip_token {
                            tokens.push(Token(name.to_string(), val.to_string(), location.clone()));
                            skip_token = true;
                            break;
                        }
                    }
                }
    
                if skip_token {
                    byte_index = token_end;
                    continue;
                }
    
                byte_index = token_end;
            } else {
                break;
            }
        }
    
        tokens.push(Token("EOF".into(), "\0".into(), None));
        tokens
    }
}
