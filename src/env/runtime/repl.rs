use std::io::{self, Write};
use std::path::Path;

use reedline::{
    default_emacs_keybindings, Emacs, Span,
    ColumnarMenu, ReedlineMenu, MenuBuilder,
    Keybindings, EditCommand, ReedlineEvent,
    KeyCode, KeyModifiers, Reedline,
    Signal, Prompt, PromptEditMode,
    PromptHistorySearch, Completer, Suggestion,
    Validator, ValidationResult, Highlighter,
    StyledText, FileBackedHistory,
};
use nu_ansi_term::Style;

use crate::env::runtime::utils::{KEYWORDS, supports_color};
use crate::env::runtime::types::VALID_TYPES;

const COLOR_KEYWORD_RGB: (u8, u8, u8)       = (197, 134, 192);
const COLOR_TYPE_RGB: (u8, u8, u8)          = (86 , 156, 214);
const COLOR_STRING_RGB: (u8, u8, u8)        = (206, 145, 120);
const COLOR_COMMENTS_RGB: (u8, u8, u8)      = (106, 153, 85 );
const COLOR_NUMBER_RGB: (u8, u8, u8)        = (156, 220, 254);
const COLOR_PREPROCESSOR_RGB: (u8, u8, u8)  = (245, 176, 239);

const COLOR_RESET_ANSI: &str          = "\x1b[0m";

pub struct ReplCompleter {
    pub completions: Vec<String>,
}

impl Completer for ReplCompleter {
    fn complete(&mut self, line: &str, pos: usize) -> Vec<Suggestion> {
        let slice = if pos <= line.len() { &line[..pos] } else { line };

        let ends_with_ws = slice.ends_with(' ');
        let trimmed_empty = slice.trim().is_empty();
        let last_line_is_empty = slice.rsplit('\n').next().map_or(false, |s| s.trim().is_empty());

        if ends_with_ws || trimmed_empty || last_line_is_empty {
            return vec![
                Suggestion {
                    value: String::from("    "),
                    description: None,
                    style: None,
                    extra: None,
                    span: Span::new(pos, pos),
                    append_whitespace: false,
                }
            ];
        }

        let norm = slice.replace("\r\n", "  ").replace('\n', " ");
        let mut parts = norm.split(' ').rev();

        let mut ws_count = 0;
        let mut span_line = String::new();
        let mut out = Vec::new();

        while let Some(s) = parts.next() {
            if s.is_empty() {
                ws_count += 1;
                continue;
            }

            if span_line.is_empty() {
                span_line = s.to_string();
            } else {
                span_line = format!("{s} {span_line}");
            }

            for cand in &self.completions {
                if cand.starts_with(&span_line) {
                    let span = Span::new(
                        pos - span_line.len() - ws_count,
                        pos,
                    );

                    let value = cand.clone();

                    if value.len() > (span.end - span.start) {
                        out.push(Suggestion {
                            value,
                            description: None,
                            style: None,
                            extra: None,
                            span,
                            append_whitespace: false,
                        });
                    }
                }
            }
        }

        out.sort_by(|a, b| a.value.cmp(&b.value));
        out.dedup_by(|a, b| a.value == b.value);

        out
    }
}

pub struct ReplValidator;

impl Validator for ReplValidator {
    fn validate(&self, input: &str) -> ValidationResult {
        let trimmed = input.trim_end();
        if trimmed.is_empty() || trimmed.starts_with("//") || trimmed.starts_with(':') {
            return ValidationResult::Complete;
        }

        if trimmed.ends_with('\\') || trimmed.ends_with(':') || trimmed.ends_with("/*") {
            return ValidationResult::Incomplete;
        }

        let mut single_quotes = 0;
        let mut double_quotes = 0;
        let mut escaped = false;
        let mut parens = 0;
        let mut brackets = 0;
        let mut braces = 0;
        let mut in_multiline_comment = false;

        let mut chars = trimmed.chars().peekable();
        while let Some(ch) = chars.next() {
            if in_multiline_comment {
                if ch == '*' && chars.peek() == Some(&'/') {
                    chars.next();
                    in_multiline_comment = false;
                }
                continue;
            } else if ch == '/' && chars.peek() == Some(&'*') {
                chars.next();
                in_multiline_comment = true;
                continue;
            }

            if escaped {
                escaped = false;
                continue;
            }

            match ch {
                '\\' => escaped = true,
                '\'' => single_quotes += 1,
                '"'  => double_quotes += 1,
                '('  => parens += 1,
                ')'  => parens -= 1,
                '['  => brackets += 1,
                ']'  => brackets -= 1,
                '{'  => braces += 1,
                '}'  => braces -= 1,
                _    => {}
            }
        }

        if in_multiline_comment || single_quotes % 2 != 0 || double_quotes % 2 != 0 {
            return ValidationResult::Incomplete;
        }

        if parens > 0 || brackets > 0 || braces > 0 {
            return ValidationResult::Incomplete;
        }

        ValidationResult::Complete
    }
}

pub struct ReplHighlighter;

impl Highlighter for ReplHighlighter {
    fn highlight(&self, line: &str, _pos: usize) -> StyledText {
        let mut styled = StyledText::new();
        let mut word = String::new();
        let mut in_str = false;
        let mut quote = '\0';
        let mut in_multiline_comment = false;
        let mut chars = line.chars().peekable();

        while let Some(ch) = chars.next() {
            if in_multiline_comment {
                word.push(ch);
                if ch == '*' && chars.peek() == Some(&'/') {
                    word.push(chars.next().unwrap());
                    let (r, g, b) = COLOR_COMMENTS_RGB;
                    let s = Style::new().fg(nu_ansi_term::Color::Rgb(r, g, b));
                    styled.push((s, word.clone()));
                    word.clear();
                    in_multiline_comment = false;
                }
                continue;
            }

            if in_str {
                word.push(ch);
                if ch == quote {
                    let (r, g, b) = COLOR_STRING_RGB;
                    let s = Style::new().fg(nu_ansi_term::Color::Rgb(r, g, b));
                    styled.push((s, word.clone()));
                    word.clear();
                    in_str = false;
                }
                continue;
            }

            if ch == '#' {
                if !word.is_empty() {
                    push_highlighted_word(&mut styled, &word);
                    word.clear();
                }

                word.push('#');

                while let Some(ch) = chars.peek() {
                    if !(ch.is_ascii_alphanumeric() || *ch == '-') {
                        break;
                    }
                    word.push(chars.next().unwrap());
                }
                let (r,g,b) = COLOR_PREPROCESSOR_RGB;
                let s = Style::new().fg(nu_ansi_term::Color::Rgb(r,g,b));
                styled.push((s, word.clone()));
                word.clear();
                continue;
            }

            if ch == '/' && chars.peek() == Some(&'/') {
                if !word.is_empty() {
                    push_highlighted_word(&mut styled, &word);
                    word.clear();
                }
                word.push_str("//");
                chars.next();

                while let Some(&c) = chars.peek() {
                    word.push(chars.next().unwrap());
                    if c == '\n' || c == '\r' {
                        break;
                    }
                }

                let (r, g, b) = COLOR_COMMENTS_RGB;
                let s = Style::new().fg(nu_ansi_term::Color::Rgb(r, g, b));
                styled.push((s, word.clone()));
                word.clear();
                continue;
            }

            if ch == '/' && chars.peek() == Some(&'*') {
                if !word.is_empty() {
                    push_highlighted_word(&mut styled, &word);
                    word.clear();
                }
                chars.next();
                word.push_str("/*");
                in_multiline_comment = true;
                continue;
            }

            if ch == '<' && chars.peek() == Some(&'#') {
                if !word.is_empty() {
                    push_highlighted_word(&mut styled, &word);
                    word.clear();
                }
                chars.next();
                word.push_str("<#");
                while let Some(c) = chars.next() {
                    word.push(c);
                    if c == '#' && chars.peek() == Some(&'>') {
                        word.push(chars.next().unwrap());
                        break;
                    }
                }
                let (r, g, b) = COLOR_COMMENTS_RGB;
                let s = Style::new().fg(nu_ansi_term::Color::Rgb(r, g, b));
                styled.push((s, word.clone()));
                word.clear();
                continue;
            }

            if ch == '"' || ch == '\'' {
                if !word.is_empty() {
                    push_highlighted_word(&mut styled, &word);
                    word.clear();
                }
                in_str = true;
                quote = ch;
                word.push(ch);
                continue;
            }

            if ch.is_alphanumeric() || ch == '_' {
                word.push(ch);
            } else {
                if !word.is_empty() {
                    push_highlighted_word(&mut styled, &word);
                    word.clear();
                }
                styled.push((Style::new(), ch.to_string()));
            }
        }

        if !word.is_empty() {
            let (r, g, b) = if in_str || in_multiline_comment { 
                if in_str { COLOR_STRING_RGB } else { COLOR_COMMENTS_RGB } 
            } else { 
                (0, 0, 0)
            };
            if in_str || in_multiline_comment {
                let s = Style::new().fg(nu_ansi_term::Color::Rgb(r, g, b));
                styled.push((s, word));
            } else {
                push_highlighted_word(&mut styled, &word);
            }
        }

        styled
    }
}

pub struct ReplPrompt {
    left: String,
    multiline: String,
}

impl ReplPrompt {
    pub fn new(left: String, multiline: String) -> Self {
        Self { left, multiline }
    }
}

impl Prompt for ReplPrompt {
    fn render_prompt_left(&self) -> std::borrow::Cow<'_, str> {
        std::borrow::Cow::Owned(format!("{}{}{}", COLOR_RESET_ANSI, self.left, COLOR_RESET_ANSI))
    }

    fn render_prompt_right(&self) -> std::borrow::Cow<'_, str> {
        std::borrow::Cow::Borrowed("")
    }

    fn render_prompt_indicator(&self, _prompt_mode: PromptEditMode) -> std::borrow::Cow<'_, str> {
        std::borrow::Cow::Borrowed("")
    }

    fn render_prompt_multiline_indicator(&self) -> std::borrow::Cow<'_, str> {
        std::borrow::Cow::Owned(format!("{}{}{}", COLOR_RESET_ANSI, self.multiline, COLOR_RESET_ANSI))
    }

    fn render_prompt_history_search_indicator(
        &self,
        _history_search: PromptHistorySearch,
    ) -> std::borrow::Cow<'_, str> {
        std::borrow::Cow::Borrowed("")
    }
}

fn push_highlighted_word(styled: &mut StyledText, word: &str) { 
    if VALID_TYPES.contains(&word) {
        let (r,g,b) = COLOR_TYPE_RGB;
        let s = Style::new().fg(nu_ansi_term::Color::Rgb(r,g,b));
        styled.push((s, word.to_string()));
    } else if KEYWORDS.contains(&word) {
        let (r,g,b) = COLOR_KEYWORD_RGB;
        let s = Style::new().fg(nu_ansi_term::Color::Rgb(r,g,b));
        styled.push((s, word.to_string()));
    } else if word.chars().all(|c| c.is_ascii_digit()) {
        let (r,g,b) = COLOR_NUMBER_RGB;
        let s = Style::new().fg(nu_ansi_term::Color::Rgb(r,g,b));
        styled.push((s, word.to_string()));
    } else {
        styled.push((Style::new(), word.to_string()));
    }
}

fn add_menu_keybindings(keybindings: &mut Keybindings) {
    keybindings.add_binding(
        KeyModifiers::NONE,
        KeyCode::Tab,
        ReedlineEvent::UntilFound(vec![
            ReedlineEvent::Menu("completion_menu".to_string()),
            ReedlineEvent::MenuNext,
        ]),
    );
    keybindings.add_binding(
        KeyModifiers::ALT,
        KeyCode::Enter,
        ReedlineEvent::Edit(vec![EditCommand::InsertNewline]),
    );
    keybindings.add_binding(
        KeyModifiers::SHIFT,
        KeyCode::Enter,
        ReedlineEvent::Edit(vec![EditCommand::InsertNewline]),
    );
}

pub fn read_input(
    prompt: &str,
    multiline_prompt: &str,
    history: &mut Vec<String>,
    completions: &[String],
    history_path: &Path,
) -> String {
    if !supports_color() {
        print!("{}", prompt);
        io::stdout().flush().unwrap();
        let mut buf = String::new();
        io::stdin().read_line(&mut buf).unwrap();
        return buf.trim_end().to_string();
    }

    let completer    = ReplCompleter { completions: completions.to_vec() };
    let validator    = ReplValidator;
    let highlighter  = ReplHighlighter;
    let history_file = FileBackedHistory::with_file(10_000, history_path.to_owned())
        .expect("Failed to open history file");

    let columnar_menu = ColumnarMenu::default()
        .with_name("completion_menu")
        .with_columns(100)
        .with_column_width(None)
        .with_column_padding(2)
        .with_marker("")
        .with_text_style(Style::default())
        .with_selected_text_style(Style::default().reverse())
        .with_description_text_style(Style::default())
        .with_match_text_style(Style::default())
        .with_selected_match_text_style(Style::default().reverse())
        .with_only_buffer_difference(false);

    let completion_menu = Box::new(columnar_menu);

    let mut keybindings = default_emacs_keybindings();
    add_menu_keybindings(&mut keybindings);

    let edit_mode = Box::new(Emacs::new(keybindings));

    let mut editor = Reedline::create()
        .with_completer(Box::new(completer))
        .with_validator(Box::new(validator))
        .with_highlighter(Box::new(highlighter))
        .with_history(Box::new(history_file))
        .with_menu(ReedlineMenu::EngineCompleter(completion_menu))
        .with_edit_mode(edit_mode);

    let mut full_input = String::new();

    loop {
        let prompt_instance = if full_input.is_empty() {
            ReplPrompt::new(prompt.to_string(), multiline_prompt.to_string())
        } else {
            ReplPrompt::new(multiline_prompt.to_string(), multiline_prompt.to_string())
        };

        match editor.read_line(&prompt_instance) {
            Ok(Signal::Success(line)) => {
                full_input.push_str(&line);
                full_input.push('\n');
                match <ReplValidator as Validator>::validate(&ReplValidator, &full_input) {
                    ValidationResult::Complete => break,
                    ValidationResult::Incomplete => {},
                }
            }
            Ok(Signal::CtrlC) => {
                println!("^C");
                std::process::exit(0);
            }
            Ok(Signal::CtrlD) => {
                std::process::exit(0);
            }
            Err(err) => {
                eprintln!("Error reading input: {:?}", err);
                break;
            }
        }
    }

    let input = full_input.trim_end().to_string();
    history.push(input.clone());
    input
}

