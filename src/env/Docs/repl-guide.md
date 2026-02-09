# Lucia REPL Guide

Welcome to **Lucia 2.0.0 REPL** — an interactive shell for running Lucia code live.

---

## Starting the REPL

After launching, you'll see:

```lucia-repl
Lucia-2.0.0 REPL
Type 'exit()' to exit or 'help()' for help.
>>>
```

Type your code and hit **Enter** to evaluate.

---

## Tokens and Statements

- **Tokens** are the lexical tokens processed from your input.  
- **Statements** are the AST nodes describing the parsed structure of your code.

Both are shown before evaluation results when debug mode is enabled.

---

## Last result variables

- `_` stores the last produced value (if not `null`).  
- `_err` stores the last encountered error.

---

## Output behavior

If your input evaluates to a value other than `null`, it prints automatically.

Examples:

```lucia-repl
>>> 42
Tokens: [("NUMBER", "42")]
Statements: [{"type": "NUMBER", "value": "42"}]
42

>>> "hello"
Tokens: [("STRING", "\"hello\"")]
Statements: [{"type": "STRING", "value": "\"hello\"", "mods": []}]
"hello"

>>> null
Tokens: [("BOOLEAN", "null")]
Statements: [{"type": "BOOLEAN", "value": "null"}]
```

---

## Examples

### Simple print

```lucia-repl
>>> println("hello, world")
Tokens: [("IDENTIFIER", "println"), ("SEPARATOR", "("), ("STRING", "\"hello world\""), ("SEPARATOR", ")")]
Statements: [{"type": "CALL", "name": "println", "pos_args": [{"type": "STRING", "value": "\"hello world\"", "mods": []}], "named_args": {}}]
hello, world
```

### While true loop

```lucia-repl
>>> while true: println("looping") end
Tokens: [("IDENTIFIER", "while"), ("BOOLEAN", "true"), ("SEPARATOR", ":"), ("IDENTIFIER", "println"), ("SEPARATOR", "("), ("STRING", "\"looping\""), ("SEPARATOR", ")"), ("IDENTIFIER", "end")]
Statements: [{"type": "WHILE", "condition": {"type": "BOOLEAN", "value": "true"}, "body": [{"type": "CALL", "name": "println", "pos_args": [{"type": "STRING", "value": "\"looping\"", "mods": []}], "named_args": {}}]}] 
looping
looping
... (press Ctrl+T to stop)
```

---

## Control commands

Press **Ctrl+T** anytime to stop running code (including infinite loops).

---

## REPL Macros

Start a line with `:` to trigger a macro.

```lucia-repl
>>> ::
Available REPL macros:
  :exit                - Exit the REPL
  :clear / :cls        - Clear the terminal screen
  :help / :?           - Show general help
  :macro-help / ::     - Show this help message for REPL macros
  :clear-history / :ch - Clear the REPL command history
  :traceback / :t      - Show the last error traceback information
>>>
```

---

## Syntax Highlighting

The REPL provides **hardcoded syntax highlighting** for:

- keywords/booleans  
- strings  
- comments  
- numbers  
- preprocessor directives  
- types

If the terminal **does not support color**, the REPL automatically falls back to plain stdin with **no highlighting or other special features**.

---

## Multiline Input

The REPL supports multiline expressions:

- **Alt+Enter** inserts a new line.  
- Incomplete expressions automatically continue:
  - unclosed quotes  
  - trailing `:`  
  - unclosed parentheses / brackets / braces  
  - unfinished expressions

The validator checks completeness before evaluation.

---

## Command History

Your REPL history is automatically saved to:

```lucia-repl
<lucia>/env/.cache/repl.history
```

It is loaded on startup and updated as you execute each command.  
You can navigate through your previous commands using **Up/Down arrows**, and edit them directly before re-executing.

The macro `:clear-history` or `:ch` clears the saved history.  
Since the file is **human-readable**, you can also open and edit it manually if needed.

---

## Additional Input Features

- **Tab completions**: Press **Tab** to auto-complete variables and identifiers that are currently defined.  
- **Indent insertion**: On a whitespace or empty line, double-pressing **Tab** inserts **4 spaces** to simulate a tab.

---

## Debug Output

When debug mode is enabled, the REPL prints:

- `Tokens:`  
- `Statements:`  
- internal messages inside `<>`

These appear in color only when supported.

---

## Summary

- Tokens show how input is lexed  
- Statements show how input is parsed  
- `_` stores the last value  
- `_err` stores the last error  
- Macros help control the REPL  
- Multiline input and highlighting improve editing  
- History persists between sessions  

Type `help()` anytime in the REPL for general help.

---

Have fun coding with Lucia!
