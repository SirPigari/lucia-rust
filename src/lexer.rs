use regex::Regex;

pub struct Lexer<'a> {
    code: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn new(code: &'a str) -> Self {
        Lexer { code }
    }

    pub fn tokenize(&self, include_comments: bool) -> Vec<(String, String)> {
        let operators = vec![
            "->", ">=", "<=", "==", "!=", "+=", "-=", "*=", "/=", "=",
            "+", "-", "^", "*", "/", ">", "<", "!", "%", "||", "&&",
            "|", "#", "~", "$", "?"
        ];

        let word_operators = vec![
            "in", "or", "and", "not", "isnt", "isn't", "is", "xor", "xnor", "nein"
        ];

        let operator_pattern = format!(
            "({})|\\b({})\\b",
            operators.iter()
                .map(|op| regex::escape(op))
                .collect::<Vec<String>>()
                .join("|"),
            word_operators.iter()
                .map(|op| regex::escape(op))
                .collect::<Vec<String>>()
                .join("|")
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

        let mut regex_parts = Vec::new();
        for (name, pattern) in token_specifications {
            regex_parts.push(format!(r"(?P<{}>{})", name, pattern));
        }

        let full_regex = Regex::new(&regex_parts.join("|")).unwrap();

        let mut tokens = Vec::new();

        let mut prev_token: Option<(String, String)> = None;

        let capture_names: Vec<&str> = full_regex.capture_names().flatten().collect();

        for cap in full_regex.captures_iter(self.code) {
            for &name in &capture_names {
                if let Some(value) = cap.name(name) {
                    if (name == "COMMENT_SINGLE" || name == "COMMENT_MULTI" || name == "COMMENT_INLINE") && !include_comments {
                        break;
                    }

                    let mut value = value.as_str().to_string();

                    if name == "COMMENT_SINGLE" || name == "COMMENT_MULTI" || name == "COMMENT_INLINE" {
                        value = value.replace("    ", "\t")
                                     .replace("\t", "\t")
                                     .replace("  ", "\t")
                                     .replace(" ", "\t");
                    } else if name == "NUMBER" {
                        value = value.replace("_", "");
                    }

                    if name == "NUMBER" && value.starts_with('-') && value.len() > 1 {
                        if let Some((ref prev_type, ref prev_val)) = prev_token {
                            let allowed_prev = match prev_type.as_str() {
                                "SEPARATOR" => matches!(prev_val.as_str(), "(" | "[" | "{" | "," | ":" | ";"),
                                "OPERATOR" => true,
                                "EOF" => true,
                                _ => false,
                            };

                            if !allowed_prev {
                                tokens.push(("OPERATOR".to_string(), "-".to_string()));
                                tokens.push(("NUMBER".to_string(), value[1..].to_string()));
                                prev_token = Some(("NUMBER".to_string(), value[1..].to_string()));
                                continue;
                            }
                        }
                    }

                    tokens.push((name.to_string(), value.clone()));
                    prev_token = Some((name.to_string(), value));

                    break;
                }
            }
        }

        tokens.push(("EOF".to_string(), "\0".to_string()));

        tokens
    }
}
