use std::io::{self, Write};

#[derive(Debug)]
pub struct Error {
    pub error_type: String,
    pub msg: String,
}

impl Error {
    pub fn new(error_type: &str, msg: &str) -> Self {
        Self {
            error_type: error_type.to_string(),
            msg: msg.to_string(),
        }
    }

    pub fn error_type(&self) -> &str {
        &self.error_type
    }

    pub fn msg(&self) -> &str {
        &self.msg
    }

    pub fn format_error(&self) -> String {
        format!("{}: {}", self.error_type, self.msg)
    }
}


#[derive(Debug, Clone)]
pub enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Null,
    Map { keys: Vec<Value>, values: Vec<Value> },
    List(Vec<Value>),
    ListCompletion { pattern: Vec<Value>, end: Option<Box<Value>> },
}

pub fn get_line_info(source: &str, line_number: usize) -> Option<String> {
    source.lines().nth(line_number.saturating_sub(1)).map(|s| s.to_string())
}


pub fn clear_terminal() {
    // Clear terminal screen for supported platforms (Windows, Linux, etc)
    print!("{}[2J", 27 as char); // ANSI Escape Code
    io::stdout().flush().unwrap();
}

pub fn hex_to_ansi(hex_color: &str, use_colors: Option<bool>) -> String {
    // Simple conversion of hex color to ANSI escape code
    let use_colors = use_colors.unwrap_or(true);

    if !use_colors {
        return "".to_string();
    }

    if hex_color == "reset" {
        return "\x1b[0m".to_string();
    }

    // Basic regex-like pattern for a 6-digit color code
    let hex = if hex_color.starts_with('#') { &hex_color[1..] } else { hex_color };

    if hex.len() == 6 {
        let r = u8::from_str_radix(&hex[0..2], 16).unwrap();
        let g = u8::from_str_radix(&hex[2..4], 16).unwrap();
        let b = u8::from_str_radix(&hex[4..6], 16).unwrap();
        return format!("\x1b[38;2;{};{};{}m", r, g, b);
    }

    "\x1b[0m".to_string() // Return reset if invalid color
}

pub fn print_colored(message: &str, color: &str, use_colors: Option<bool>) {
    let use_colors = use_colors.unwrap_or(true);
    let colored_message = format!("{}{}{}", hex_to_ansi(color, Some(use_colors)), message, hex_to_ansi("reset", Some(use_colors)));
    println!("{}", colored_message);
}

pub fn read_input(prompt: &str) -> String {
    print!("{}", prompt);
    io::stdout().flush().unwrap();
    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap();
    input.trim().to_string()
}
