use regex::Regex;
use crate::env::runtime::tokens::{Token, Location};
use std::sync::OnceLock;

pub struct Lexer<'a> {
    code: &'a str,
    file_path: &'a str,
    line_offsets: Vec<usize>,
}

impl<'a> Lexer<'a> {
    pub fn new(code: &'a str, file_path: &'a str) -> Self {
        let mut line_offsets = Vec::with_capacity(code.lines().count());
        let mut offset = 0;
        for line in code.lines() {
            line_offsets.push(offset);
            offset += line.len() + 1;
        }
        Lexer { code, file_path, line_offsets }
    }

    pub fn tokenize(&self) -> Vec<Token> {
        static REGEX: OnceLock<Regex> = OnceLock::new();

        let regex = REGEX.get_or_init(|| {
            let operators = [
                "->", ">=", "<=", "==", "!=", "+=", "-=", "*=", "/=", "=",
                "++", "--", "+", "-", "^", "*", "/", ">", "<", "!", "%", "||", "&&",
                "|", "#", "~", "$", "?", "&", "^=", "%="
            ];

            let word_operators = [
                "in", "or", "and", "not", "isnt", "isn't", "is", "xor", "xnor", "nein"
            ];

            let operator_pattern = format!(
                "({})|\\b({})\\b",
                operators.iter().map(|pattern| regex::escape(*pattern)).collect::<Vec<_>>().join("|"),
                word_operators.iter().map(|pattern| regex::escape(*pattern)).collect::<Vec<_>>().join("|"),
            );            

            let token_specifications = [
                ("COMMENT_INLINE", r"<#.*?#>"),
                ("COMMENT_SINGLE", r"//.*"),
                ("COMMENT_MULTI", r"/\*[\s\S]*?\*/"),
                ("RAW_STRING", r#"(?i)([fb]*r[fb]*)("([^"]*)"|'([^']*)')"#),
                ("STRING", r#"(?i)([fb]{0,3})("([^"\\]|\\.)*"|'([^'\\]|\\.)*')"#),
                ("BOOLEAN", r"\b(true|false|null)\b"),
                ("NUMBER", r"-?\d(?:_?\d)*(?:\.\d(?:_?\d)*)?"),
                ("OPERATOR", &operator_pattern),
                ("IDENTIFIER", r"\bnon-static\b|\b[a-zA-Z_]\w*\b"),
                ("SEPARATOR", r"\.\.\.|\.\.|[(){}\[\];:.,\?]"),
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
        let mut prev_token: Option<&str> = None;
        let mut byte_index = 0;

        while byte_index < self.code.len() {
            if let Some(cap) = regex.captures(&self.code[byte_index..]) {
                let mat = cap.get(0).unwrap();
                let token_str = mat.as_str();
                let token_start = byte_index;
                let token_end = byte_index + token_str.len();

                let line_number = match self.line_offsets.binary_search(&token_start) {
                    Ok(line) => line,
                    Err(next_line) => if next_line == 0 { 0 } else { next_line - 1 },
                };
                let line_offset = self.line_offsets[line_number];
                let line_end = self.code[line_offset..].find('\n').unwrap_or(self.code.len() - line_offset) + line_offset;
                let line_str = &self.code[line_offset..line_end];
                let column_start = token_start - line_offset;
                let column_end = token_end - line_offset;

                let location = Some(Location::new(
                    self.file_path.to_string(),
                    line_str.to_string(),
                    line_number + 1,
                    (column_start, column_end),
                ));

                let mut skip_token = false;
                for &name in &capture_names {
                    if let Some(value) = cap.name(name) {
                        let val = value.as_str();

                        if name.starts_with("COMMENT_") {
                            skip_token = true;
                            break;
                        }

                        if name == "NUMBER" {
                            // remove underscores from number literals
                            let clean_val = val.replace('_', "");
                            tokens.push(Token(name.to_string(), clean_val, location.clone()));
                            prev_token = Some(name);
                            skip_token = true;
                            break;
                        }

                        if name != "WHITESPACE" && !skip_token {
                            tokens.push(Token(name.to_string(), val.to_string(), location.clone()));
                            prev_token = Some(name);
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
