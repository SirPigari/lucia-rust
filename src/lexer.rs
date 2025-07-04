use regex::Regex;
use crate::env::runtime::tokens::{Token, Location};

pub struct Lexer<'a> {
    code: &'a str,
    file_path: String,
}

impl<'a> Lexer<'a> {
    pub fn new(code: &'a str, file_path: String) -> Self {
        Lexer { code, file_path }
    }

    pub fn tokenize(&self) -> Vec<Token> {
        let operators = vec![
            "->", ">=", "<=", "==", "!=", "+=", "-=", "*=", "/=", "=",
            "++", "--", "+", "-", "^", "*", "/", ">", "<", "!", "%", "||", "&&",
            "|", "#", "~", "$", "?", "&", "^=", "%=",
        ];

        let word_operators = vec![
            "in", "or", "and", "not", "isnt", "isn't", "is", "xor", "xnor", "nein"
        ];

        let operator_pattern = format!(
            "({})|\\b({})\\b",
            operators.iter().map(|op| regex::escape(op)).collect::<Vec<_>>().join("|"),
            word_operators.iter().map(|op| regex::escape(op)).collect::<Vec<_>>().join("|"),
        );

        let token_specifications = vec![
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

        let full_regex = Regex::new(&regex_parts.join("|")).unwrap();
        let capture_names: Vec<&str> = full_regex.capture_names().flatten().collect();

        let mut tokens = Vec::new();
        let mut prev_token: Option<String> = None;

        let mut byte_index = 0;
        let lines: Vec<&str> = self.code.lines().collect();
        let mut line_offsets: Vec<usize> = Vec::with_capacity(lines.len());

        {
            let mut offset = 0;
            for line in &lines {
                line_offsets.push(offset);
                offset += line.len() + 1;
            }
        }

        while byte_index < self.code.len() {
            if let Some(cap) = full_regex.captures(&self.code[byte_index..]) {
                let mat = cap.get(0).unwrap();
                let token_str = mat.as_str();
                let token_start = byte_index;
                let token_end = byte_index + token_str.len();

                let (line_number, line_offset) = {
                    let mut found = (lines.len() - 1, *line_offsets.last().unwrap_or(&0));
                    for (i, &offset) in line_offsets.iter().enumerate() {
                        let next_offset = if i + 1 < line_offsets.len() {
                            line_offsets[i + 1]
                        } else {
                            self.code.len() + 1
                        };
                        if token_start >= offset && token_start < next_offset {
                            found = (i, offset);
                            break;
                        }
                    }
                    found
                };

                let line_str = lines[line_number];
                let column_start = token_start - line_offset;
                let column_end = token_end - line_offset;

                let location = Some(Location::new(
                    self.file_path.clone(),
                    line_str.to_string(),
                    line_number + 1,
                    (column_start, column_end),
                ));

                for &name in &capture_names {
                    if let Some(value) = cap.name(name) {
                        let mut val = value.as_str().to_string();

                        if name.starts_with("COMMENT") {
                            val = val.replace("    ", "\t").replace("  ", "\t").replace(" ", "\t");
                        } else if name == "NUMBER" {
                            val = val.replace("_", "");
                        }

                        if name == "NUMBER" && val.starts_with('-') && val.len() > 1 {
                            if let Some(ref prev) = prev_token {
                                let needs_split = match prev.as_str() {
                                    "SEPARATOR" | "OPERATOR" | "EOF" => false,
                                    _ => true,
                                };

                                if needs_split {
                                    tokens.push(Token("OPERATOR".into(), "-".into(), location.clone()));
                                    tokens.push(Token("NUMBER".into(), val[1..].to_string(), location));
                                    prev_token = Some("NUMBER".into());
                                    break;
                                }
                            }
                        }

                        if name != "WHITESPACE" {
                            tokens.push(Token(name.to_string(), val.clone(), location.clone()));
                            prev_token = Some(name.to_string());
                        }

                        break;
                    }
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
