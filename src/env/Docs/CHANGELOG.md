# Changelog

## 2.0.0

### Major Changes

- **Complete rewrite**: The entire interpreter is now written in <a href="https://www.rust-lang.org/"><img src="https://upload.wikimedia.org/wikipedia/commons/0/0f/Original_Ferris.svg" alt="Rust" style="height:1em; vertical-align: text-bottom; position: relative; top: 2px;"/>Rust</a>, providing better performance, memory safety, and maintainability

- **New syntax**:  
  - `:=` for automatic type inference
  - `=>` for lambdas
  - `typedef` for type definitions (structs, enums, aliases)
  - `gen` for generators
  - `match` for pattern matching
  - `scope` for limited scope blocks
  - `defer` for cleanup operations
  - `&` and `*` for references and dereferencing
  - `++` for pointer flattening and postfix/prefix addition
  - `--` for postfix/prefix subtraction  
  - `![effect]` and `!<[effect]>` for function effects
  - `where` clauses for local bindings
  - `for Type:` impl blocks for methods
  - `forget` for removing variables/elements
  - `?` operator for optional errors
  - List comprehensions `[x for x in ...]`
  - Raw strings `r"..."` and bytes `b"..."`
- **Type system**: Static typing with type inference, immutable by default
- **Expression-based**: Everything is an expression, including blocks and control flow

### New Features

- **Advanced data types**:
  - Structs with `typedef struct`
  - Enums with variants and associated data `typedef enum`
  - Generics for structs and enums
  - Tuples with destructuring
  - Impl types for traits/interfaces
  - Tuple unpacking in assignments
- **Pattern matching**: `match` statements with pattern destructuring
- **References**: `&` for references, `*` for dereferencing
- **Arrays**: Fixed-size arrays with `array()` function
- **Scope blocks**: `scope` keyword for limited scope expressions
- **Defer statements**: Cleanup operations with `defer`
- **Final variables**: Immutable variables with `final` keyword
- **Preprocessor system**:
  - `#include` for file inclusion
  - `#define` for constants
  - `#ifdef`/`#ifndef` for conditional compilation
  - `#macro` for defining macros
  - `#precompile` for compile-time evaluation
  - `#link` for FFI bindings
  - `#config` for configuration directives
- **Operator overloading**: Custom operators for user-defined types
- **Where clauses**: Local bindings in expressions
- **Function effects**: Effect system for functions (`![effect]`)
- **Generators**: Lazy evaluation with `gen` keyword
- **Low-level features**: LASM (Lucia Assembly) for assembly operations
- **FFI**: Foreign function interface with C libraries

### Standard Library Additions

- `collections`: Advanced collection utilities
- `hash`: Hashing functions
- `json`: JSON parsing and serialization
- `regex`: Regular expression support
- `raylib_lucia`: Graphics and game development with Raylib
- `lasm`: Low-level assembly operations
- `libload`: Dynamic library loading
- `nest`: HTTP client and server utilities
- `clib`: C standard library bindings
- `elevator`: System-level utilities

### Enhanced Features

- **REPL improvements**:
  - Syntax highlighting for keywords, strings, comments, numbers, types
  - Multiline input support with Alt+Enter
  - Command history with Up/Down arrow navigation
  - Tab completion for variables and identifiers
  - REPL macros (:exit, :clear, :help, :clear-history, :traceback)
  - Ctrl+T to stop running code
- **Configuration system**:
  - Expanded config.json with new options (debug_mode, cache_format, allow_unsafe, allow_inline_config, stack_size, color_scheme)
  - Inline #config directives in scripts
- **String interpolation**: Improved f-strings with formatting options (binary, octal, hex, precision, case, alignment, fill)
- **List operations**: Map, filter, sort with custom comparators, comprehensions `[x for x in ...]`
- **Error handling**: Try-catch blocks with improved error types, `throw from` syntax, `?` operator for optional errors
- **Modules**: Enhanced import system with selective imports, aliases, file imports
- **Inline conditionals**: `if x > 0 then "positive" else "negative"`
- **Bitwise operations**: `lshift`, `rshift`, `band`, `bor`, `xor`, `xnor`, `bnot`
- **Number literals**: Scientific notation, hex (`0x`), octal (`0o`), binary (`0b`), custom base (`base#number`), underscores for readability
- **Function features**: Default parameters, recursion, modifiers (`public`, `static`, `final`)
- **Macros**: User-defined macros with parameters, built-in macros (`assert!`, `todo!`, `dbg!`, etc.)
- **Type checking**: `is` and `isnt` operators
- **Logical aliases**: `and`/`or` as alternatives to `&&`/`||`
- **String/bytes literals**: Raw strings `r"..."` and byte strings `b"..."`

### Bug Fixes and Improvements

- Improved performance through Rust implementation
- Better memory management
- Enhanced error messages
- More robust type checking
- Fixed various bugs from Python version

---

These are pasted from [lucia-python](https://github.com/SirPigari/lucia?tab=readme-ov-file#changelog), so the links won't work:

## 1.3.1
- Updated [CREDITS.md](env/CREDITS.md)
- Added `fetch` function to builtins
- Added `requests` library
- Added `time` library
- Added `goldenRatio` variable to `math` library
- Updated [config.json](env/config.json) (see [here](env/Docs/config-guide.md#9-allow_fetch) and [here](env/Docs/config-guide.md#10-execute_code_blocks))
- Updated [fibonacci.lc](env/Docs/examples/fibonnaci.lc) example to use Binet's formula
- Added [requestsExample.lc](env/Docs/examples/requestsExample.lc) example
- Updated testSuiteVersion to 0.0.3b
- Fixed typos
- Added `flattenToList` function to `map` type
- Added `finilize` built-in function to finalize a function (`mutable` -> `final`)
- Fixed bugs:
  - Issues with `config` library
  - Issues with [activate.py](env/activate.py) file
  - Issues with REPL terminal mode
  - Many bugs in [pparser.py](pparser.py)
  - Issues with importing modules (again)
- Added `__json__` method to built-in types
- Fixed skill issue on `print` function
- Added `expect` function to built-ins
- Removed unused `exports.json` file
- Added support for `_` variable in REPL mode (last result)
- Updated [LuciaInstaller.nsi](installer/LuciaInstaller.nsi) file

---

## 1.3.0
- Added `f-strings`
- Added `code blocks`, see [here](env/Docs/language-syntax.md#code-blocks)
- Fixed `help` function
- Minor changes to some error messages
- Added C library (system lib, cannot be imported)
- Updated `test` library for more tests
- Included TCC in [`env/bin/tcc/`](env/bin/tcc)
- Added more options to installer
- Added `setprec` built-in function to set the precision of floats
- Added `getprec` built-in function to get the precision of floats
- Fixed minor bugs and issues
- Added minor updates, I don't remember them myself
- Modified `lexer.py` to include `WHITESPACE` token and then be removed in `pparser.py`
- Variables now cannot be named one of these: ASM, C, PY
- Added support for single-quote strings (`'`)

---

## 1.2.1
- Changed the lucia logo, new logo: ![Lucia Logo](env/assets/lucia_logo_small.png "Lucia Logo")
- Fixed typos
- Fixed bugs:
  - Error when python function returned None, now it returns `null`
  - Fixed issues with REPL terminal mode
  - **IMPORTANT** Fixed `forget` statement
  - **IMPORTANT** Fixed problems with `or` and `and` operators
  - **IMPORTANT** Fixed issue with default values in function parameters
- Added function `getcwd` to `os` library
- Added functions to python library:
  - `pycall` - Calls a python function from a python file.
  - `pybool` - Converts a value to a boolean.
  - `pyversion` - Returns the python version.
- Fixed bugs with `random` library
- Added functions to `test` library:
  - `testIndex` - Tests indexes in lucia.
  - `testFunctionCalls` - Tests function calls in lucia.
  - `testLibs` - Tests libraries in lucia.
  - `testOperators` - Tests operators in lucia.
- Added [`update_version.py`](env/helpers/update_version.py)

---

## 1.2.0
- Added `flatten` function to `list` and `map` modules:
- Added `Exception` and `Warning` keywords:
  - `Exception` is used to create new exceptions.
  - `Warning` is used to create new warnings.
- Added `__type__` function to exceptions and warnings
- Fixed errors while importing modules
- Added more tests to `test` library
- Updated installer
- Added new operators:
  - `in` - Checks if a value is in a list or map.
  - `or` - Logical OR operator.
  - `and` - Logical AND operator.
  - `not` - Logical NOT operator.
  - `isnt` or `isn't` - Checks if two values are not equal.
  - `is` - Checks if two values are equal.
  - `xor` - Logical XOR operator.
  - `xnor` - Logical XNOR operator.
- Updated `lucia.py`
- Added error handling for missing `end` keyword
- Added `#config` predef
- Modified how try-catch works ([see here](env/Docs/language-syntax.md#try-and-catch))

---

## 1.1.2
- Fixed many bugs
- Added documentation
- Fixed installer

---

## 1.1.1
- Added predefs:
  - Syntax:
    - `#predef <name> -> <value>`
    - Predefs:
      - `#alias` - Aliases a function or variable (example: `#alias function -> fun`)
      - `#del` - Deletes an alias defined before (example: `#del function`)
- Added `test_all` function to `test` module.
- Fixed MANY bugs
- Added 'in' operator (`~`):
  - Syntax: `<value> ~ <list or map>`
  - Returns `true` if the value is in the list, `false` otherwise.
- Fixed float operations

---

## 1.1.0
- Added `try`-`catch` statements
- Added `console` library:
  - `overwrite`: Clears the current line in the terminal and prints new text.
  - `log`: Standard print function alias.
  - `supports_color`: Checks if the terminal supports colored output.
  - `debug`: Prints debug messages with configurable color.
  - `info`: Prints informational messages with configurable color.
  - `error`: Prints error messages with configurable color.
  - `fatal`: Prints fatal error messages with a darkened color.
  - `warn`: Prints warning messages with configurable color.
  - `progress_bar`: Displays a progress bar for an iterable.
  - `styled_print`: Prints styled text with foreground and background color options.
  - `clear`: Clears the terminal screen.
- Added more precision to floats
- Fixed bugs:
  - Fixed nested functions calls not working
  - Fixed issues with built-in functions calls
- Added built-in functions:
  - `wait` - waits for a certain amount of time in milliseconds
  - `declen` - returns the decimal length of a number
- Added `static` and `public` to variables
- Fixed issues with debugs
- Fixed issues with float operations
- Added operators:
  - `| number |` - returns the absolute value of a number
- Added debug logs to loops iterations
- Default precision for floats is now 28 but is set automatically
