use crate::env::runtime::tokens::Token;

const INDENT: &str = "    ";

// TODO
// fix this later

pub fn format_tokens(tokens: &[&Token]) -> String {
    let tokens: Vec<(String, String)> = tokens.iter()
        .map(|Token(kind, value, _)| (kind.clone(), value.clone()))
        .filter(|(kind, _)| kind != "EOF")
        .collect();

    let mut result = String::new();
    let mut indent_level = 0;
    let mut i = 0;

    while i < tokens.len() {
        let (kind, _) = &tokens[i];
        match kind.as_str() {
            "IDENTIFIER" => fmt_ident(&tokens, &mut i, &mut result, &mut indent_level, false),
            _ => {
                format_single_token(&tokens[i], &mut result);
                i += 1;
            }
        }
    }

    result
}

fn fmt_ident(tokens: &[(String, String)], i: &mut usize, result: &mut String, indent_level: &mut usize, inside_block: bool) {
    if *i >= tokens.len() || tokens[*i].0 != "IDENTIFIER" {
        return;
    }

    let value = &tokens[*i].1;

    match value.as_str() {
        "import" => fmt_import(tokens, i, result, *indent_level, inside_block),
        "if" | "while" | "for" => fmt_control(tokens, i, result, indent_level),
        _ => {
            result.push_str(&format!("{}{}\n", INDENT.repeat(*indent_level), value));
            *i += 1;
        }
    }
}

fn fmt_import(tokens: &[(String, String)], i: &mut usize, result: &mut String, indent_level: usize, inside_block: bool) {
    let mut line = String::new();
    line.push_str(&format!("{}import", INDENT.repeat(indent_level)));
    *i += 1;

    while *i < tokens.len() {
        let (_, next_value) = &tokens[*i];
        if tokens[*i].0 == "IDENTIFIER" && (next_value == "import" || next_value == "if" || next_value == "while" || next_value == "for") {
            break;
        }
        line.push(' ');
        line.push_str(next_value);
        *i += 1;
    }

    let next_import = *i < tokens.len() && tokens[*i].0 == "IDENTIFIER" && tokens[*i].1 == "import";

    result.push_str(&line);
    result.push('\n');

    if !inside_block && !next_import {
        result.push('\n');
    }
}

fn fmt_control(tokens: &[(String, String)], i: &mut usize, result: &mut String, indent_level: &mut usize) {
    let keyword = &tokens[*i].1;
    result.push_str(&format!("{}{}", INDENT.repeat(*indent_level), keyword));
    *i += 1;

    if *i < tokens.len() && tokens[*i].0 == "SEPARATOR" && tokens[*i].1 == "(" {
        result.push('(');
        *i += 1;
        while *i < tokens.len() && !(tokens[*i].0 == "SEPARATOR" && tokens[*i].1 == ")") {
            result.push_str(&tokens[*i].1);
            result.push(' ');
            *i += 1;
        }
        if *i < tokens.len() && tokens[*i].0 == "SEPARATOR" && tokens[*i].1 == ")" {
            if result.ends_with(' ') {
                result.pop();
            }
            result.push(')');
            *i += 1;
        }
    } else {
        while *i < tokens.len() && !(tokens[*i].0 == "SEPARATOR" && tokens[*i].1 == ":") {
            result.push(' ');
            result.push_str(&tokens[*i].1);
            *i += 1;
        }
    }

    if *i < tokens.len() && tokens[*i].0 == "SEPARATOR" && tokens[*i].1 == ":" {
        result.push(':');
        *i += 1;
    }
    result.push('\n');

    *indent_level += 1;

    while *i < tokens.len() {
        if tokens[*i].0 == "IDENTIFIER" && tokens[*i].1 == "end" {
            *indent_level -= 1;
            result.push_str(&format!("{}end\n", INDENT.repeat(*indent_level)));
            *i += 1;
            break;
        }

        match tokens[*i].0.as_str() {
            "IDENTIFIER" => fmt_ident(tokens, i, result, indent_level, true),
            _ => {
                format_single_token(&tokens[*i], result);
                *i += 1;
            }
        }
    }
}

fn format_single_token(token: &(String, String), result: &mut String) {
    result.push_str(&token.1);
}
