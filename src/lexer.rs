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
            "|", "#", "~",
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

        let prefixes = [
            "rfb", "rfb", "rbf", "rbf", "frb", "fbr", "brf", "bfr",
            "rf", "rb", "fr", "fb", "br", "bf",
            "r", "f", "b"
        ];

        for cap in full_regex.captures_iter(self.code) {
            for capture_name in full_regex.capture_names() {
                if let Some(capture_name) = capture_name {
                    if let Some(value) = cap.name(capture_name) {
                        if (capture_name == "COMMENT_SINGLE" || capture_name == "COMMENT_MULTI" || capture_name == "COMMENT_INLINE") && !include_comments {
                            continue;
                        }

                        let mut value = value.as_str().to_string();

                        if capture_name == "COMMENT_SINGLE" || capture_name == "COMMENT_MULTI" || capture_name == "COMMENT_INLINE" {
                            value = value.replace("    ", "\\t")
                                         .replace("\t", "\t")
                                         .replace("  ", "\t")
                                         .replace(" ", "\t");
                        } else if capture_name == "NUMBER" {
                            value = value.replace("_", "");
                        }

                        tokens.push((capture_name.to_string(), value));
                    }
                }
            }
        }

        tokens.push(("EOF".to_string(), "\0".to_string()));

        tokens
    }
}
