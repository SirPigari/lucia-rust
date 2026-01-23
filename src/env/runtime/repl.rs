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
    StyledText, FileBackedHistory, History, PromptHistorySearchStatus,
};
use nu_ansi_term::Style;

use crate::env::runtime::utils::{KEYWORDS, supports_color, hex_to_ansi};
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
                    match_indices: None,
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
                    let matched_len = span_line.len();
                    let match_indices = Some((0..matched_len).collect::<Vec<usize>>());

                    let value = cand.clone();

                    if value.len() > (span.end - span.start) {
                        out.push(Suggestion {
                            match_indices,
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

        let mut escaped = false;
        let mut parens = 0;
        let mut brackets = 0;
        let mut braces = 0;
        let mut in_multiline_comment = false;
        let mut in_single_string = false;
        let mut in_double_string = false;

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
                '\'' if !in_double_string => in_single_string = !in_single_string,
                '"'  if !in_single_string => in_double_string = !in_double_string,
                _ if in_double_string || in_single_string => continue,
                '('  => parens += 1,
                ')'  => parens -= 1,
                '['  => brackets += 1,
                ']'  => brackets -= 1,
                '{'  => braces += 1,
                '}'  => braces -= 1,
                _    => {}
            }
        }

        if in_multiline_comment || in_single_string || in_double_string {
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
        let mut in_multiline_comment = false;
        let mut chars = line.chars().peekable();

        while let Some(ch) = chars.next() {
            if in_multiline_comment {
                word.push(ch);
                if ch == '*' && chars.peek() == Some(&'/') {
                    word.push(chars.next().unwrap());
                    in_multiline_comment = false;
                }
                let (r, g, b) = COLOR_COMMENTS_RGB;
                let s = Style::new().fg(nu_ansi_term::Color::Rgb(r, g, b));
                styled.push((s, word.clone()));
                word.clear();
                continue;
            }

            if ch == '/' && chars.peek() == Some(&'/') {
                if !word.is_empty() { push_highlighted_word(&mut styled, &word); word.clear(); }
                word.push_str("//");
                chars.next();
                while let Some(&c) = chars.peek() {
                    if c == '\n' || c == '\r' { break; }
                    word.push(chars.next().unwrap());
                }
                let (r, g, b) = COLOR_COMMENTS_RGB;
                let s = Style::new().fg(nu_ansi_term::Color::Rgb(r, g, b));
                styled.push((s, word.clone()));
                word.clear();
                continue;
            }

            if ch == '/' && chars.peek() == Some(&'*') {
                if !word.is_empty() { push_highlighted_word(&mut styled, &word); word.clear(); }
                chars.next();
                word.push_str("/*");
                in_multiline_comment = true;
                let (r, g, b) = COLOR_COMMENTS_RGB;
                let s = Style::new().fg(nu_ansi_term::Color::Rgb(r, g, b));
                styled.push((s, word.clone()));
                word.clear();
                continue;
            }

            if ch == '<' && chars.peek() == Some(&'#') {
                if !word.is_empty() { push_highlighted_word(&mut styled, &word); word.clear(); }
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

            let mut is_string_start = false;

            if ch == '"' || ch == '\'' {
                is_string_start = true;
            } else if ch == 'f' || ch == 'r' || ch == 'b' {
                let mut it = chars.clone();
                let mut saw_prefix = true;

                while let Some(c) = it.next() {
                    if c == 'f' || c == 'r' || c == 'b' {
                        saw_prefix = true;
                        continue;
                    }
                    if (c == '"' || c == '\'') && saw_prefix {
                        is_string_start = true;
                    }
                    break;
                }
            }

            if is_string_start {
                if !word.is_empty() {
                    push_highlighted_word(&mut styled, &word);
                    word.clear();
                }

                let mut prefixes = String::new();
                let mut raw = false;
                let mut is_f = false;

                let mut first = ch;
                loop {
                    if first == 'f' || first == 'r' || first == 'b' {
                        prefixes.push(first);
                        if first == 'f' { is_f = true; }
                        if first == 'r' { raw = true; }

                        if let Some(&next) = chars.peek() {
                            if next == 'f' || next == 'r' || next == 'b' {
                                first = chars.next().unwrap();
                                continue;
                            }
                        }
                    }
                    break;
                }

                let quote_char = if first == '"' || first == '\'' {
                    first
                } else if let Some(&q) = chars.peek() {
                    if q == '"' || q == '\'' {
                        chars.next().unwrap()
                    } else {
                        word.push_str(&prefixes);
                        continue;
                    }
                } else {
                    word.push_str(&prefixes);
                    continue;
                };

                let mut string_word = prefixes.clone();
                string_word.push(quote_char);

                let (r, g, b) = COLOR_STRING_RGB;
                let s = Style::new().fg(nu_ansi_term::Color::Rgb(r, g, b));
                styled.push((s, string_word.clone()));
                string_word.clear();

                let mut brace_depth = 0;
                let mut in_escape = false;

                while let Some(c) = chars.next() {
                    if !raw && c == '\\' && !in_escape {
                        in_escape = true;
                        string_word.push(c);
                        continue;
                    }

                    if in_escape {
                        string_word.push(c);
                        in_escape = false;
                        continue;
                    }

                    if is_f && c == '{' {
                        if brace_depth == 0 && !string_word.is_empty() {
                            styled.push((s, string_word.clone()));
                            string_word.clear();
                        }

                        styled.push((s, "{".to_string()));
                        brace_depth += 1;

                        let mut code = String::new();
                        while let Some(inner) = chars.next() {
                            if inner == '{' {
                                brace_depth += 1;
                                code.push(inner);
                            } else if inner == '}' {
                                brace_depth -= 1;
                                if brace_depth == 0 {
                                    let inner_styled = self.highlight(&code, 0);
                                    styled.buffer.extend(inner_styled.buffer);
                                    styled.push((s, "}".to_string()));
                                    break;
                                } else {
                                    code.push(inner);
                                }
                            } else {
                                code.push(inner);
                            }
                        }
                        continue;
                    }

                    string_word.push(c);

                    if c == quote_char && brace_depth == 0 {
                        break;
                    }
                }

                if !string_word.is_empty() {
                    styled.push((s, string_word.clone()));
                }

                continue;
            }

            if ch == '#' {
                if !word.is_empty() { push_highlighted_word(&mut styled, &word); word.clear(); }

                word.push('#');
                while let Some(&ch) = chars.peek() {
                    if !(ch.is_ascii_alphanumeric() || ch == '-') { break; }
                    word.push(chars.next().unwrap());
                }

                let (r,g,b) = COLOR_PREPROCESSOR_RGB;
                let s = Style::new().fg(nu_ansi_term::Color::Rgb(r,g,b));
                styled.push((s, word.clone()));
                word.clear();
                continue;
            }

            if ch.is_alphanumeric() || ch == '_' {
                word.push(ch);
            } else {
                if !word.is_empty() { push_highlighted_word(&mut styled, &word); word.clear(); }
                styled.push((Style::new(), ch.to_string()));
            }
        }

        if !word.is_empty() {
            push_highlighted_word(&mut styled, &word);
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
        history_search: PromptHistorySearch,
    ) -> std::borrow::Cow<'_, str> {
        match history_search.status {
            PromptHistorySearchStatus::Passing => std::borrow::Cow::Owned(format!(
                "{}search: ",
                COLOR_RESET_ANSI
            )), 
            PromptHistorySearchStatus::Failing => std::borrow::Cow::Owned(format!(
                "{}search: {}{}{}",
                COLOR_RESET_ANSI,
                hex_to_ansi("#F44350", true),
                history_search.term,
                COLOR_RESET_ANSI
            )),
        }
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
    keybindings.add_binding(
        KeyModifiers::CONTROL,
        KeyCode::Char('a'),
        ReedlineEvent::Edit(vec![EditCommand::SelectAll]),
    );
    keybindings.add_binding(
        KeyModifiers::CONTROL,
        KeyCode::Char('r'),
        ReedlineEvent::SearchHistory,
    );
    keybindings.add_binding(
        KeyModifiers::CONTROL,
        KeyCode::Char('l'),
        ReedlineEvent::ClearScreen,
    );
    keybindings.add_binding(
        KeyModifiers::CONTROL,
        KeyCode::Char('e'),
        ReedlineEvent::OpenEditor,
    );
}

#[cfg_attr(feature = "single_executable", allow(unused_variables))]
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
    #[cfg(not(feature = "single_executable"))]
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
        .with_menu(ReedlineMenu::EngineCompleter(completion_menu))
        .with_edit_mode(edit_mode);

    #[cfg(not(feature = "single_executable"))]
    {
        editor = editor.with_history(Box::new(history_file));
    }

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

pub fn read_input_no_repl(prompt: &str, multiline_prompt: Option<&str>) -> Result<String, (String, String)> {
    if !supports_color() {
        print!("{}", prompt);
        io::stdout().flush().unwrap();

        let mut buf = String::new();
        match io::stdin().read_line(&mut buf) {
            Ok(_) => {},
            Err(err) => return Err(("IOError".to_string(), format!("Failed to read input: {:?}", err))),
        }
        return Ok(buf.trim_end().to_string());
    }

    let mut tmp_path = std::env::temp_dir();
    tmp_path.push(format!("lucia_reedline_history_{}", std::process::id()));

    let mut full_input = String::new();

    let history = FileBackedHistory::with_file(10_000, tmp_path.clone())
        .expect("Failed to create temp history file");
    let history_box = Box::new(history);
        
    let mut keybindings = default_emacs_keybindings();
        keybindings.add_binding(
        KeyModifiers::CONTROL,
        KeyCode::Char('a'),
        ReedlineEvent::Edit(vec![EditCommand::SelectAll]),
    );
    if multiline_prompt.is_some() {
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
    } else {
        keybindings.add_binding(
            KeyModifiers::NONE,
            KeyCode::Enter,
            ReedlineEvent::Submit,
        );
        keybindings.add_binding(
            KeyModifiers::ALT,
            KeyCode::Enter,
            ReedlineEvent::Submit,
        );
        keybindings.add_binding(
            KeyModifiers::SHIFT,
            KeyCode::Enter,
            ReedlineEvent::Submit,
        );
    }

    let mut editor = Reedline::create()
        .with_history(history_box)
        .with_edit_mode(Box::new(Emacs::new(keybindings)));


    loop {
        let prompt_instance = if full_input.is_empty() {
            ReplPrompt::new(prompt.to_string(), multiline_prompt.unwrap_or("").to_string())
        } else if let Some(multi) = multiline_prompt {
            ReplPrompt::new(multi.to_string(), multi.to_string())
        } else {
            break;
        };

        match editor.read_line(&prompt_instance) {
            Ok(Signal::Success(line)) => {
                full_input.push_str(&line);
                full_input.push('\n');

                if multiline_prompt.is_none() {
                    break;
                }
            }
            Ok(Signal::CtrlD) => {
                let mut history_sync = FileBackedHistory::with_file(10_000, tmp_path.clone())
                    .expect("Failed to sync temp history");
                let _ = history_sync.sync();
                let _ = std::fs::remove_file(&tmp_path);
                return Err((
                    "InterruptedError".to_string(),
                    "Input was interrupted by user".to_string(),
                ));
            }
            Ok(Signal::CtrlC) => {
                std::process::exit(0);
            }
            Err(err) => {
                return Err((
                    "IOError".to_string(),
                    format!("Error reading input: {:?}", err),
                ));
            }
        }
    }

    let input = full_input.trim_end().to_string();

    let mut history_sync = FileBackedHistory::with_file(10_000, tmp_path.clone())
        .expect("Failed to sync temp history");
    let _ = history_sync.sync();
    let _ = std::fs::remove_file(&tmp_path);

    Ok(input)
}

#[cfg(not(target_arch = "wasm32"))]
pub fn read_input_breakpoint(
    prompt: &str,
    multiline_prompt: &str,
    history: &mut Vec<String>,
    completions: &[String],
) -> String {
    read_input(prompt, multiline_prompt, history, completions, &std::env::temp_dir().join("lucia_bp_history"))
}