use crate::env::runtime::tokens::ConcreteToken;
use crate::lexer::Lexer;
use std::fs;
use std::path::Path;

const INDENT: &str = "    "; // 4 spaces per LC2C convention

#[derive(Debug, Clone, Copy, PartialEq)]
enum Context {
    TopLevel,
    Block,
}

pub fn format_tokens(tokens: &[ConcreteToken]) -> String {
    let mut formatter = Formatter::new(tokens);
    formatter.format();
    formatter.result
}

pub fn format_file<P: AsRef<Path>>(input_path: P, output_path: Option<P>) -> Result<String, String> {
    let source = fs::read_to_string(input_path.as_ref())
        .map_err(|e| format!("Failed to read file: {}", e))?;
    
    let file_path = input_path.as_ref()
        .to_str()
        .ok_or_else(|| "Invalid file path".to_string())?;
    
    let lexer = Lexer::new(&source, file_path);
    let tokens = lexer.tokenize_concrete();
    
    let formatted = format_tokens(&tokens);
    
    let output = output_path.as_ref()
        .map(|p| p.as_ref())
        .unwrap_or(input_path.as_ref());
    
    fs::write(output, &formatted)
        .map_err(|e| format!("Failed to write file: {}", e))?;
    
    Ok(formatted)
}

struct Formatter {
    tokens: Vec<(String, String)>,
    standalone_comments: Vec<(usize, String)>,
    inline_comments: Vec<(usize, String)>,
    result: String,
    i: usize,
    indent_level: usize,
    context: Context,
    needs_blank_line: bool,
    last_was_block_end: bool,
    consecutive_expression_count: usize,
    last_statement_was_expression: bool,
}

impl Formatter {
    fn new(tokens: &[ConcreteToken]) -> Self {
        let mut regular_tokens = Vec::new();
        let mut standalone_comments = Vec::new();
        let mut inline_comments = Vec::new();

        let mut last_was_newline = true;
        
        for (_token_idx, token) in tokens.iter().enumerate() {
            let kind_str = token.0.to_string();
            let value_str = token.1.to_string();
            
            if kind_str == "EOF" {
                continue;
            }
            
            if kind_str == "COMMENT" {
                let mut comment_text = value_str.clone();
                let is_multiline = comment_text.contains('\n');
                let started_inline = !last_was_newline;
                
                if started_inline && !is_multiline && comment_text.starts_with("/*") && comment_text.ends_with("*/") {
                    comment_text = comment_text.replacen("/*", "<#", 1);
                    comment_text = comment_text.replacen("*/", "#>", 1);
                }
                
                if started_inline {
                    inline_comments.push((regular_tokens.len(), comment_text));
                } else {
                    standalone_comments.push((regular_tokens.len(), comment_text));
                }
                
                if is_multiline {
                    last_was_newline = true;
                } else {
                    last_was_newline = false;
                }
            } else if kind_str == "WHITESPACE" {
                if value_str.contains('\n') {
                    regular_tokens.push(("NEWLINE".to_string(), "\n".to_string()));
                    last_was_newline = true;
                }
            } else {
                regular_tokens.push((kind_str, value_str));
                last_was_newline = false;
            }
        }

        Self {
            tokens: regular_tokens,
            standalone_comments,
            inline_comments,
            result: String::new(),
            i: 0,
            indent_level: 0,
            context: Context::TopLevel,
            needs_blank_line: false,
            last_was_block_end: false,
            consecutive_expression_count: 0,
            last_statement_was_expression: false,
        }
    }

    fn format(&mut self) {
        while self.i < self.tokens.len() {
            self.format_statement();
        }
        
        if !self.result.is_empty() && !self.result.ends_with('\n') {
            self.result.push('\n');
        }
    }

    fn format_statement(&mut self) {
        if self.i >= self.tokens.len() {
            return;
        }

        self.write_standalone_comments();

        if self.i >= self.tokens.len() {
            return;
        }

        let (kind, value) = (self.tokens[self.i].0.clone(), self.tokens[self.i].1.clone());

        if kind == "NEWLINE" {
            if self.i + 1 < self.tokens.len() {
                let next_value = &self.tokens[self.i + 1].1;
                if next_value == "." || next_value == "|>" {
                    self.i += 1;
                    return;
                }
            }
            self.i += 1;
            return;
        }

        match kind.as_str() {
            "IDENTIFIER" => self.format_identifier(),
            "OPERATOR" if value == "#" => {
                let mut temp_i = self.i + 1;
                while temp_i < self.tokens.len() && self.tokens[temp_i].0 == "WHITESPACE" {
                    temp_i += 1;
                }
                if temp_i < self.tokens.len() && self.tokens[temp_i].1 == "macro" {
                    if !self.result.is_empty() && !self.result.ends_with("\n\n") {
                        if !self.result.ends_with('\n') {
                            self.result.push('\n');
                        }
                        self.result.push('\n');
                    }
                }
                self.format_preprocessor();
            }
            "SEPARATOR" if value == "#" => {
                let mut temp_i = self.i + 1;
                while temp_i < self.tokens.len() && self.tokens[temp_i].0 == "WHITESPACE" {
                    temp_i += 1;
                }
                if temp_i < self.tokens.len() && self.tokens[temp_i].1 == "macro" {
                    if !self.result.is_empty() && !self.result.ends_with("\n\n") {
                        if !self.result.ends_with('\n') {
                            self.result.push('\n');
                        }
                        self.result.push('\n');
                    }
                }
                self.format_preprocessor();
            }
            _ => {
                self.write_indent();
                self.write_token();
                self.result.push('\n');
                self.i += 1;
            }
        }
    }

    fn format_identifier(&mut self) {
        let value = self.tokens[self.i].1.clone();

        match value.as_str() {
            "fun" | "gen" | "typedef" => {
                if self.consecutive_expression_count >= 3 {
                    if !self.result.ends_with("\n\n") {
                        self.result.push('\n');
                    }
                }
                self.consecutive_expression_count = 0;
                self.last_statement_was_expression = false;
                
                self.add_blank_line_if_needed();
                self.format_function_or_type();
                self.needs_blank_line = true;
            }
            
            "import" => {
                if self.consecutive_expression_count >= 3 {
                    if !self.result.ends_with("\n\n") {
                        self.result.push('\n');
                    }
                }
                self.consecutive_expression_count = 0;
                self.last_statement_was_expression = false;
                
                self.format_import();
            }
            
            "if" | "while" | "for" | "match" | "try" => {
                if self.consecutive_expression_count >= 3 {
                    if !self.result.ends_with("\n\n") {
                        self.result.push('\n');
                    }
                }
                self.consecutive_expression_count = 0;
                self.last_statement_was_expression = false;
                
                self.add_blank_line_before_block();
                self.format_control_flow();
                self.last_was_block_end = true;
                self.needs_blank_line = true;
            }
            
            "return" | "break" | "continue" | "throw" => {
                if self.consecutive_expression_count >= 3 {
                    if !self.result.ends_with("\n\n") {
                        self.result.push('\n');
                    }
                }
                self.consecutive_expression_count = 0;
                self.last_statement_was_expression = false;
                
                self.format_simple_statement();
            }
            
            _ => {
                self.format_expression_statement();
                
                if self.indent_level == 0 {
                    if self.last_statement_was_expression {
                        self.consecutive_expression_count += 1;
                    } else {
                        self.consecutive_expression_count = 1;
                    }
                    self.last_statement_was_expression = true;
                    self.needs_blank_line = true;
                } else {
                    self.consecutive_expression_count = 0;
                    self.last_statement_was_expression = false;
                }
            }
        }
    }

    fn format_function_or_type(&mut self) {
        let keyword = self.tokens[self.i].1.clone();
        
        self.write_indent();
        self.write_token();
        self.result.push(' ');
        self.i += 1;

        while self.i < self.tokens.len() {
            let value = self.tokens[self.i].1.clone();
            if self.tokens[self.i].0 == "IDENTIFIER" && matches!(value.as_str(), "public" | "static" | "final" | "mutable" | "private") {
                self.write_token();
                self.result.push(' ');
                self.i += 1;
            } else {
                break;
            }
        }

        if self.i < self.tokens.len() {
            self.write_token();
            self.i += 1;
        }

        if keyword == "typedef" {
            self.format_typedef_body();
        } else {
            self.format_function_signature();
            self.format_block();
        }
    }

    fn format_typedef_body(&mut self) {
        if self.peek_token_value() == Some("<".to_string()) {
            self.format_generics();
        }

        if self.peek_token_value() == Some("=".to_string()) {
            self.result.push(' ');
            self.write_token();
            self.result.push(' ');
            self.i += 1;
        }

        while self.i < self.tokens.len() {
            let value = self.tokens[self.i].1.clone();
            
            if value == "{" {
                self.result.push(' ');
                self.format_struct_or_enum_body();
                break;
            } else if self.is_statement_end() {
                break;
            } else {
                self.write_token();
                if self.should_add_space_after_current() {
                    self.result.push(' ');
                }
                self.i += 1;
            }
        }
        
        self.result.push('\n');
    }

    fn format_struct_or_enum_body(&mut self) {
        self.write_token(); // {
        self.result.push('\n');
        self.i += 1;
        self.indent_level += 1;

        while self.i < self.tokens.len() {
            let value = self.tokens[self.i].1.clone();
            
            if value == "}" {
                self.indent_level -= 1;
                self.write_indent();
                self.write_token();
                self.i += 1;
                break;
            }

            self.write_indent();
            
            while self.i < self.tokens.len() {
                let value = self.tokens[self.i].1.clone();
                
                if value == "," || value == "}" {
                    if value == "," {
                        self.write_token();
                        self.i += 1;
                    }
                    self.result.push('\n');
                    break;
                }
                
                self.write_token();
                
                let next_val = self.peek_token_value();
                if next_val == Some(":".to_string()) || next_val == Some("=".to_string()) {
                    self.result.push(' ');
                } else if value == ":" || value == "=" {
                    self.result.push(' ');
                } else if self.should_add_space_after_current() {
                    self.result.push(' ');
                }
                
                self.i += 1;
            }
        }
    }

    fn format_function_signature(&mut self) {
        if self.peek_token_value() == Some("(".to_string()) {
            self.write_token();
            self.i += 1;

            let mut first_param = true;
            while self.i < self.tokens.len() && self.tokens[self.i].1 != ")" {
                if !first_param {
                    self.result.push_str(", ");
                }
                first_param = false;

                while self.i < self.tokens.len() {
                    let value = self.tokens[self.i].1.clone();
                    if value == "," || value == ")" {
                        if value == "," {
                            self.i += 1;
                        }
                        break;
                    }
                    
                    self.write_token();
                    let next = self.peek_token_value();
                    if value == ":" || value == "=" || next == Some(":".to_string()) || next == Some("=".to_string()) {
                        self.result.push(' ');
                    }
                    self.i += 1;
                }
            }

            if self.i < self.tokens.len() && self.tokens[self.i].1 == ")" {
                self.write_token();
                self.i += 1;
            }
        }

        while self.i < self.tokens.len() {
            let value = self.tokens[self.i].1.clone();
            
            if value == ":" {
                break;
            }
            
            if !matches!(value.as_str(), "[" | "(" | "]" | ")") && 
               !self.result.ends_with(' ') &&
               !self.result.ends_with('[') &&
               !self.result.ends_with('(') {
                self.result.push(' ');
            }
            self.write_token();
            self.i += 1;
        }

        if self.i < self.tokens.len() && self.tokens[self.i].1 == ":" {
            self.write_token();
            self.result.push('\n');
            self.i += 1;
        }
    }

    fn format_control_flow(&mut self) {
        self.write_indent();
        self.write_token();
        self.i += 1;

        if self.peek_token_value() == Some("(".to_string()) {
            self.result.push(' ');
            self.write_token(); // (
            self.i += 1;

            while self.i < self.tokens.len() && self.tokens[self.i].1 != ")" {
                let (_kind, value) = (self.tokens[self.i].0.clone(), self.tokens[self.i].1.clone());
                
                if value == "in" && self.peek_token_value() == Some("[".to_string()) {
                    self.write_token();
                    self.i += 1;
                    self.result.push(' ');
                    continue;
                }
                
                self.write_token();
                if self.should_add_space_after_current() && self.peek_token_value() != Some(")".to_string()) {
                    self.result.push(' ');
                }
                self.i += 1;
            }

            if self.i < self.tokens.len() && self.tokens[self.i].1 == ")" {
                self.write_token();
                self.i += 1;
            }
        }

        if self.i < self.tokens.len() && self.tokens[self.i].1 == ":" {
            self.write_token();
            self.result.push('\n');
            self.i += 1;
        }

        self.format_block();
    }

    fn format_block(&mut self) {
        self.indent_level += 1;

        while self.i < self.tokens.len() {
            let value = self.tokens[self.i].1.clone();
            
            if value == "end" || value == "}" {
                self.indent_level -= 1;
                self.write_indent();
                self.write_token();
                self.result.push('\n');
                self.i += 1;
                if self.indent_level == 0 {
                    self.result.push('\n');
                }
                break;
            }
            
            if value == "catch" || value == "elif" {
                self.indent_level -= 1;
                self.write_indent();
                self.write_token();
                self.i += 1;
                
                if self.peek_token_value() == Some("(".to_string()) {
                    self.result.push(' ');
                    self.write_token(); // (
                    self.i += 1;
                    
                    while self.i < self.tokens.len() && self.tokens[self.i].1 != ")" {
                        self.write_token();
                        if self.should_add_space_after_current() && self.peek_token_value() != Some(")".to_string()) {
                            self.result.push(' ');
                        }
                        self.i += 1;
                    }
                    
                    if self.i < self.tokens.len() && self.tokens[self.i].1 == ")" {
                        self.write_token();
                        self.i += 1;
                    }
                }
                
                if self.i < self.tokens.len() && self.tokens[self.i].1 == ":" {
                    self.write_token();
                    self.result.push('\n');
                    self.i += 1;
                }
                
                self.indent_level += 1;
                continue;
            }
            
            if value == "else" {
                self.indent_level -= 1;
                self.write_indent();
                self.write_token();
                self.i += 1;
                
                if self.i < self.tokens.len() && self.tokens[self.i].1 == ":" {
                    self.write_token();
                    self.result.push('\n');
                    self.i += 1;
                }
                
                self.indent_level += 1;
                continue;
            }

            self.format_statement();
        }
    }

    fn format_import(&mut self) {
        self.write_indent();
        self.write_token(); // import
        self.i += 1;
        
        if self.peek_token_value() == Some("(".to_string()) {
            self.result.push(' ');
        }
        
        let mut import_line = String::new();
        let mut prev_value = String::new();
        while self.i < self.tokens.len() && !self.is_statement_end() {
            let value = self.tokens[self.i].1.clone();
            
            let mut next_idx = self.i + 1;
            while next_idx < self.tokens.len() && self.tokens[next_idx].0 == "NEWLINE" {
                next_idx += 1;
            }
            let next_is_bang = next_idx < self.tokens.len() && self.tokens[next_idx].1 == "!";
            
            if value == "from" || value == "as" {
                if !import_line.is_empty() && !import_line.ends_with(' ') {
                    import_line.push(' ');
                }
                import_line.push_str(&value);
                import_line.push(' ');
            } else if value == "," {
                import_line.push_str(", ");
            } else if value == "(" || value == ")" {
                import_line.push_str(&value);
            } else if value == "!" {
                import_line.push_str(&value);
            } else {
                if !import_line.is_empty() && !import_line.ends_with(' ') && !import_line.ends_with('(') && prev_value != "!" && !next_is_bang {
                    import_line.push(' ');
                }
                import_line.push_str(&value);
            }
            
            prev_value = value;
            self.i += 1;
        }

        self.result.push_str(&import_line);
        self.result.push('\n');
        
        if self.i < self.tokens.len() {
            let mut next_idx = self.i;
            while next_idx < self.tokens.len() && self.tokens[next_idx].0 == "NEWLINE" {
                next_idx += 1;
            }
            if next_idx < self.tokens.len() && self.tokens[next_idx].1 != "import" {
                self.result.push('\n');
            }
        }
    }

    fn format_simple_statement(&mut self) {
        self.write_indent();
        
        while self.i < self.tokens.len() && !self.is_statement_end() {
            self.write_token();
            self.i += 1;
            
            self.write_pending_comments_inline();
            
            if self.i > 0 && self.should_add_space_after_at(self.i - 1) && !self.result.ends_with(' ') {
                self.result.push(' ');
            }
        }
        
        while self.result.ends_with(' ') || self.result.ends_with('\t') {
            self.result.pop();
        }
        self.result.push('\n');
    }

    fn format_expression_statement(&mut self) {
        self.write_indent();
        
        let start_pos = self.result.len();
        let start_idx = self.i;
        
        let mut method_chain_count = 0;
        let mut temp_i = self.i;
        while temp_i < self.tokens.len() && !self.is_statement_end_at(temp_i) {
            let value = &self.tokens[temp_i].1;
            if value == "." && temp_i + 1 < self.tokens.len() && self.tokens[temp_i + 1].0 == "IDENTIFIER" {
                method_chain_count += 1;
            }
            temp_i += 1;
        }
        
        self.format_single_line_expression();
        
        let line_length = self.result.len() - start_pos;
        if line_length > 100 && method_chain_count >= 2 {
            self.result.truncate(start_pos);
            self.i = start_idx;
            self.format_chained_expression();
        }
        
        while self.result.ends_with(' ') || self.result.ends_with('\t') {
            self.result.pop();
        }
        self.result.push('\n');
    }
    
    fn is_statement_end_at(&self, index: usize) -> bool {
        if index >= self.tokens.len() {
            return true;
        }
        let (kind, value) = (&self.tokens[index].0, &self.tokens[index].1);
        
        if kind == "NEWLINE" && index + 1 < self.tokens.len() {
            let next_value = &self.tokens[index + 1].1;
            if next_value == "." || next_value == "|>" {
                return false;
            }
        }
        
        kind == "NEWLINE" || matches!(value.as_str(), ";" | "end" | "}")
    }
    
    fn format_chained_expression(&mut self) {
        let mut found_chain_start = false;
        let mut in_chain = false;
        
        while self.i < self.tokens.len() && !self.is_statement_end() {
            let (kind, value) = (self.tokens[self.i].0.clone(), self.tokens[self.i].1.clone());
            
            if kind == "NEWLINE" || kind == "WHITESPACE" {
                self.i += 1;
                self.write_pending_comments_inline();
                continue;
            }
            
            if !found_chain_start && (value == "." || value == "|>") {
                found_chain_start = true;
                in_chain = true;
                self.result.push('\n');
                self.indent_level += 1;
                self.write_indent();
                self.write_token();
                if value == "|>" {
                    self.result.push(' ');
                }
                self.i += 1;
                self.write_pending_comments_inline();
                continue;
            }
            
            if in_chain && (value == "." || value == "|>") {
                self.result.push('\n');
                self.write_indent();
                self.write_token();
                if value == "|>" {
                    self.result.push(' ');
                }
                self.i += 1;
                self.write_pending_comments_inline();
                continue;
            }
            
            if (self.is_operator() || value == ":=" || value == "=") && !self.result.ends_with(' ') && !self.result.ends_with('\n') && value != "!" {
                self.result.push(' ');
            }
            
            self.write_token();
            self.i += 1;
            
            self.write_pending_comments_inline();
            
            let mut next_idx = self.i;
            while next_idx < self.tokens.len() && (self.tokens[next_idx].0 == "NEWLINE" || self.tokens[next_idx].0 == "WHITESPACE") {
                next_idx += 1;
            }
            
            if next_idx < self.tokens.len() {
                let next_value = &self.tokens[next_idx].1;
                
                if self.is_operator() || value == "=" || value == ":=" {
                    if !self.is_statement_end_at(next_idx) && next_value != ")" && next_value != "]" && value != "!" {
                        self.result.push(' ');
                    }
                } else if value == ":" {
                    self.result.push(' ');
                } else if value == "," {
                    self.result.push(' ');
                } else if value == "=>" {
                    self.result.push(' ');
                } else if kind == "IDENTIFIER" {
                    if next_value != "." && next_value != "|>" && next_value != "(" && next_value != "[" 
                        && next_value != ")" && next_value != "]" && next_value != "," && next_value != ":" && next_value != "!" {
                        self.result.push(' ');
                    }
                }
            }
        }
        
        if found_chain_start {
            self.indent_level -= 1;
        }
    }
    
    fn format_single_line_expression(&mut self) {
        while self.i < self.tokens.len() && !self.is_statement_end() {
            let (kind, value) = (self.tokens[self.i].0.clone(), self.tokens[self.i].1.clone());
            
            if kind == "NEWLINE" {
                self.i += 1;
                self.write_pending_comments_inline();
                continue;
            }
            
            if value == "in" && self.peek_token_value() == Some("[".to_string()) {
                self.write_token();
                self.i += 1;
                self.write_pending_comments_inline();
                self.result.push(' ');
                continue;
            }
            
            let is_macro_bang = value == "!";
            
            if (self.is_operator() || value == ":=" || value == "=") && !self.result.ends_with(' ') && !is_macro_bang {
                self.result.push(' ');
            }
            
            let is_operator_token = (self.is_operator() || value == ":=" || value == "=" || value == "=>") && !is_macro_bang;
            
            self.write_token();
            self.i += 1;
            
            self.write_pending_comments_inline();
            
            if is_operator_token {
                if !self.is_statement_end() {
                    self.result.push(' ');
                }
            } else if value == ":" {
                self.result.push(' ');
            } else if value == "," {
                self.result.push(' ');
            } else if kind == "IDENTIFIER" {
                let mut next_idx = self.i;
                while next_idx < self.tokens.len() && self.tokens[next_idx].0 == "NEWLINE" {
                    next_idx += 1;
                }
                if next_idx < self.tokens.len() {
                    let next_value = &self.tokens[next_idx].1;
                    if next_value != "." && next_value != "|>" && next_value != "(" && next_value != "[" && next_value != ")" && next_value != "]" && next_value != "," && next_value != ":" && next_value != "!" {
                        self.result.push(' ');
                    }
                }
            }
        }
    }

    fn format_preprocessor(&mut self) {
        let mut tokens_on_line = Vec::new();
        let start_idx = self.i;
        
        while self.i < self.tokens.len() {
            let (kind, value) = (self.tokens[self.i].0.clone(), self.tokens[self.i].1.clone());
            if kind == "NEWLINE" {
                break;
            }
            tokens_on_line.push((kind, value));
            self.i += 1;
        }
        
        self.i = start_idx;
        let is_endmacro = tokens_on_line.len() >= 2 && tokens_on_line[1].1 == "endmacro";
        
        if is_endmacro && self.indent_level > 0 {
            self.indent_level -= 1;
        }
        
        self.write_indent();
        
        tokens_on_line.clear();
        
        while self.i < self.tokens.len() {
            let (kind, value) = (self.tokens[self.i].0.clone(), self.tokens[self.i].1.clone());
            
            if kind == "NEWLINE" {
                self.i += 1;
                break;
            }
            
            tokens_on_line.push((kind, value));
            self.i += 1;
        }
        
        if tokens_on_line.is_empty() {
            self.result.push('\n');
            return;
        }
        
        if !tokens_on_line.is_empty() && tokens_on_line[0].1 == "#" {
            self.result.push('#');
            tokens_on_line.remove(0);
        }
        
        let directive = if !tokens_on_line.is_empty() {
            let dir = tokens_on_line[0].1.clone();
            self.result.push_str(&dir);
            tokens_on_line.remove(0);
            dir
        } else {
            self.result.push('\n');
            return;
        };
        
        let needs_space = !matches!(directive.as_str(), "endmacro" | "endif" | "else");
        if needs_space && !tokens_on_line.is_empty() {
            self.result.push(' ');
        }
        
        let is_include = directive == "include";
        let is_macro = directive == "macro";
        
        for (idx, (kind, value)) in tokens_on_line.iter().enumerate() {
            self.result.push_str(value);
            
            if idx + 1 < tokens_on_line.len() {
                let next_value = &tokens_on_line[idx + 1].1;
                
                if is_include {
                } else if is_macro {
                    if value == ":" {
                    } else if value == "," {
                        self.result.push(' ');
                    } else if value == ")" && tokens_on_line[idx + 1].0 == "IDENTIFIER" {
                        self.result.push(' ');
                    } else if value != "(" && next_value != ")" && next_value != "(" 
                        && value != "$" && !next_value.starts_with('$') && value != "!" {
                        if kind == "IDENTIFIER" && tokens_on_line[idx + 1].0 == "IDENTIFIER" {
                            self.result.push(' ');
                        }
                    }
                }
            }
        }
        
        while self.result.ends_with(' ') || self.result.ends_with('\t') {
            self.result.pop();
        }
        self.result.push('\n');
        
        if directive == "macro" {
            self.indent_level += 1;
        }
        
        if directive == "include" || directive == "endmacro" {
            self.result.push('\n');
        }
    }

    fn format_generics(&mut self) {
        self.write_token(); // <
        self.i += 1;

        let mut first = true;
        while self.i < self.tokens.len() && self.tokens[self.i].1 != ">" {
            if !first && self.tokens[self.i].1 == "," {
                self.write_token();
                self.result.push(' ');
                self.i += 1;
                continue;
            }
            first = false;
            
            self.write_token();
            self.i += 1;
        }

        if self.i < self.tokens.len() && self.tokens[self.i].1 == ">" {
            self.write_token();
            self.i += 1;
        }
    }

    fn write_indent(&mut self) {
        self.result.push_str(&INDENT.repeat(self.indent_level));
    }

    fn write_token(&mut self) {
        if self.i < self.tokens.len() {
            self.result.push_str(&self.tokens[self.i].1);
        }
    }
    
    fn write_standalone_comments(&mut self) {
        let mut indices_to_remove = Vec::new();
        
        for (idx, (pos, value)) in self.standalone_comments.iter().enumerate() {
            if *pos == self.i {
                self.result.push_str(&INDENT.repeat(self.indent_level));
                self.result.push_str(value);
                self.result.push('\n');
                indices_to_remove.push(idx);
            }
        }
        
        for idx in indices_to_remove.iter().rev() {
            self.standalone_comments.remove(*idx);
        }
    }
    
    fn write_pending_comments_inline(&mut self) {
        let mut indices_to_remove = Vec::new();
        
        for (idx, (pos, value)) in self.inline_comments.iter().enumerate() {
            if *pos == self.i {
                if value.starts_with("//") {
                    if !self.result.ends_with(' ') {
                        self.result.push_str("  ");
                    }
                    self.result.push_str(value);
                } else {
                    if !self.result.ends_with(' ') {
                        self.result.push(' ');
                    }
                    self.result.push_str(value);
                }
                indices_to_remove.push(idx);
            }
        }
        
        for idx in indices_to_remove.iter().rev() {
            self.inline_comments.remove(*idx);
        }
    }

    fn peek_token_value(&self) -> Option<String> {
        if self.i < self.tokens.len() {
            Some(self.tokens[self.i].1.clone())
        } else {
            None
        }
    }

    fn is_statement_end(&self) -> bool {
        self.is_statement_end_at(self.i)
    }

    fn is_operator(&self) -> bool {
        if self.i >= self.tokens.len() {
            return false;
        }
        
        let (kind, value) = &self.tokens[self.i];
        kind == "OPERATOR" && !matches!(value.as_str(), "." | "(" | ")" | "[" | "]" | "&")
    }

    fn should_add_space_after_current(&self) -> bool {
        if self.i >= self.tokens.len() - 1 {
            return false;
        }
        
        self.should_add_space_after_at(self.i)
    }
    
    fn should_add_space_after_at(&self, index: usize) -> bool {
        if index >= self.tokens.len() - 1 {
            return false;
        }
        
        let curr_value = &self.tokens[index].1;
        
        let mut next_idx = index + 1;
        while next_idx < self.tokens.len() && self.tokens[next_idx].0 == "NEWLINE" {
            next_idx += 1;
        }
        
        if next_idx >= self.tokens.len() {
            return false;
        }
        
        let next_value = &self.tokens[next_idx].1;
        
        if matches!(curr_value.as_str(), ",") {
            return true;
        }
        
        if curr_value == "in" && next_value == "[" {
            return true;
        }
        
        if curr_value == "!" {
            return false;
        }
        
        let allow_bracket_space = curr_value == "in";
        !matches!(curr_value.as_str(), "." | "(" | "[" | "{" | "&" | "]" | "!" ) &&
         (allow_bracket_space || !matches!(next_value.as_str(), "." | ")" | "]" | "}" | "," | ";" | "|>" | ":" | "[" | "(" | "&" | "!"))
    }

    fn add_blank_line_if_needed(&mut self) {
        if self.needs_blank_line && !self.result.is_empty() && !self.result.ends_with("\n\n") {
            self.result.push('\n');
        }
        self.needs_blank_line = false;
    }

    fn add_blank_line_before_block(&mut self) {
        if self.context == Context::Block && self.last_was_block_end {
            if !self.result.is_empty() && !self.result.ends_with("\n\n") {
                self.result.push('\n');
            }
        }
        self.last_was_block_end = false;
    }
}