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
- **Statements** are the Abstract Syntax Tree (AST) nodes parsed from the tokens, representing your code's structure.

The REPL shows both after you enter code, before output or errors.

---

## Last result variables

- `_` stores the **last evaluated value** (if not `null`).  
- `_err` stores the **last error** encountered.

You can use these to access previous results or errors easily.

---

## Output behavior

If your input evaluates to a value **other than `null`**, it will be printed automatically after tokens and statements.

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
>>> print("hello, world")
Tokens: [("IDENTIFIER", "print"), ("SEPARATOR", "("), ("STRING", "\"hello, world\""), ("SEPARATOR", ")")]
Statements: [{"type": "CALL", "name": "print", "pos_arguments": [{"type": "STRING", "value": ""hello, world"", "mods": []}], "named_arguments": {}}]
hello, world
```

### While true loop

```lucia-repl
>>> while true: print("looping") end
Tokens: [("IDENTIFIER", "while"), ("BOOLEAN", "true"), ("SEPARATOR", ":"), ("IDENTIFIER", "print"), ("SEPARATOR", "("), ("STRING", "\"looping\""), ("SEPARATOR", ")"), ("IDENTIFIER", "end")]
Statements: [{"type": "WHILE", "condition": {"type": "BOOLEAN", "value": "true"}, "body": [{"type": "CALL", "name": "print", "pos_arguments": [{"type": "STRING", "value": ""looping"", "mods": []}], "named_arguments": {}}]}]   
looping
looping
... (press Ctrl+T to stop)
```

---

## Control commands

- Press **Ctrl+T** anytime to stop running code (including infinite loops).

---

## REPL Macros

Start a line with `:` to use macros for quick actions.

| Macro                     | Description                       |
|---------------------------|-----------------------------------|
| `:exit`                   | Exit the REPL (`exit()` called)   |
| `:clear` / `:cls`         | Clear the terminal screen         |
| `:help` / `:?`            | Show general help (runs `help()`) |
| `:macro-help` / `::`      | Show the list of REPL macros      |
| `:trace` / `:traceback`   | Show the last error traceback     |

Example:

```lucia-repl
:help
```

prints basic help info.

---

## Note on Debug Output

The displayed tokens (`Tokens:`), statements (`Statements:`), and messages inside `<>` (like `<Declared variable ...>`) are **debug features**.  
They are only printed if the `debug` option is set to `true` in your `config.json`.  
See [config-guide.md](config-guide.md) for details on configuring this.

These debug outputs are printed in a **separate color**, which is defined in the config and used only if your terminal supports color output.

---

## Summary

- **Tokens**: the list of recognized lexical units from your input  
- **Statements**: the parsed structure (AST) representing your code  
- **Errors** provide detailed location and hints to fix syntax issues  
- **REPL Macros** let you control the REPL environment efficiently  
- Special variables `_` and `_err` track last value and error  

Type `help()` anytime in the REPL for more info.

---

Have fun coding with Lucia!
