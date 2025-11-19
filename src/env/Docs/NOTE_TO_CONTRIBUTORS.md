# Note to contributors

Hello there! Thank you for checking out *Lucia* and deciding to contribute to it!
In this note i want to explain, walk trhough and apologise for this codebase.

## Building

To build lucia from source you need to ofcoure have rust, cargo and make.

Then run:

```console
make build
```

or to build and run directly:

```console
make
```

for only running the last build:

```console
make run
```

to test your changes:

```console
make test
```

before running make test you need to compile test runners (only once):

```console
make build-tests
```

Run `make help` for more commands.

## Files

Here is the tree:

```
lucia
├── build.rs
├── Cargo.toml
├── .gitattributes
├── .github
│   └── workflows
│       ├── benchmark-nightly.yml
│       ├── build-wasm.yml
│       ├── test-linux.yml
│       ├── test-macos.yml
│       └── test-windows.yml
├── .gitignore
├── LICENSE
├── Makefile
├── README.md
└── src
    ├── env
    │   ├── assets
    │   │   ├── installer
    │   │   │   └── LuciaInstaller.nsi
    │   │   ├── installer.ico
    │   │   ├── logo_lucia.png
    │   │   ├── lucia_icon.ico
    │   │   ├── lucia-mode.el
    │   │   ├── lucia-syntax.vsix
    │   │   ├── tests.lucia
    │   │   └── uninstaller.ico
    │   ├── bin
    │   ├── bundler
    │   │   ├── bundle.rs
    │   │   └── template
    │   │       └── runner_template.exe
    │   ├── config.json
    │   ├── Docs
    │   │   ├── benchmarks.md
    │   │   ├── config-guide.md
    │   │   ├── contributing.md
    │   │   ├── conventions.md
    │   │   ├── examples
    │   │   │   ├── 01_hello_world.lc
    │   │   │   ├── 02_fib.lc
    │   │   │   ├── 03_prime_checker.lc
    │   │   │   ├── 04_bubble_sort.lc
    │   │   │   ├── 05_simple_repl_calc.lc
    │   │   │   ├── 06_hello_world_lasm.lc
    │   │   │   ├── 07_tic_tac_toe.lc
    │   │   │   ├── 08_game_of_life.lc
    │   │   │   ├── 09_rule110.lc
    │   │   │   ├── 10_bouncing_square_raylib.lc
    │   │   │   ├── 11_custom_lang.lc
    │   │   │   ├── output
    │   │   │   ├── output.asm
    │   │   │   └── output.o
    │   │   ├── .gitignore
    │   │   ├── installation-guide.md
    │   │   ├── introduction.md
    │   │   ├── known-issues.md
    │   │   ├── NOTE_TO_CONTRIBUTORS.md
    │   │   ├── repl-guide.md
    │   │   ├── todos.md
    │   │   └── versioning.md
    │   ├── libs
    │   │   ├── clib
    │   │   │   └── main.rs
    │   │   ├── collections
    │   │   │   └── main.rs
    │   │   ├── elevator
    │   │   │   └── main.rs
    │   │   ├── fs
    │   │   │   └── main.rs
    │   │   ├── json
    │   │   │   └── main.rs
    │   │   ├── lasm
    │   │   │   ├── cpu.rs
    │   │   │   └── main.rs
    │   │   ├── libload
    │   │   │   ├── ffi.rs
    │   │   │   └── main.rs
    │   │   ├── math
    │   │   │   └── main.rs
    │   │   ├── nest
    │   │   │   └── main.rs
    │   │   ├── os
    │   │   │   └── main.rs
    │   │   ├── random
    │   │   │   └── main.rs
    │   │   ├── regex
    │   │   │   ├── main.rs
    │   │   │   └── regex_engine.rs
    │   │   ├── requests
    │   │   │   ├── main.lc
    │   │   │   └── manifest.json
    │   │   ├── std
    │   │   │   ├── assert.lc
    │   │   │   ├── _import.lc
    │   │   │   ├── io.lc
    │   │   │   ├── lazy.lc
    │   │   │   ├── macros.lc
    │   │   │   ├── _main.lc
    │   │   │   ├── manifest.json
    │   │   │   ├── math.lc
    │   │   │   ├── ops.lc
    │   │   │   └── types.lc
    │   │   └── time
    │   │       └── main.rs
    │   ├── libs.json
    │   └── runtime
    │       ├── cache.rs
    │       ├── config.rs
    │       ├── errors.rs
    │       ├── fmt.rs
    │       ├── functions.rs
    │       ├── generators.rs
    │       ├── internal_structs.rs
    │       ├── libs.rs
    │       ├── modules.rs
    │       ├── native.rs
    │       ├── pattern_reg.rs
    │       ├── precompile.rs
    │       ├── preprocessor.rs
    │       ├── repl.rs
    │       ├── statements.rs
    │       ├── static_checker.rs
    │       ├── structs_and_enums.rs
    │       ├── tokens.rs
    │       ├── types.rs
    │       ├── utils.rs
    │       ├── value.rs
    │       ├── variables.rs
    │       └── wasm.rs
    ├── interpreter.rs
    ├── lexer.rs
    ├── main.rs
    └── parser.rs
```

Forward, reffering to a file without a direct path just filename will be to a file with the same name in this tree.

## Entry-point

The main entry point is, obviosly, the [main](../../main.rs).

Other main files are:
- [lexer](../../lexer.rs)
- [parser](../../parser.rs)
- [interpreter](../../interpreter.rs)

## Main file

THIS FILE IS A MESS
DO NOT TRY TO UNDERSTAND IT
There are a few things that are important:
- [the `mod`](../../main.rs#L15)
- [flag parsing](../../main.rs#L23é3)
- [repl loop](../../main.rs#L1547)

## Lexer

This file is few lines long and pretty straight-forward.
It parses stuff with a loop over the chars iterator.
I was using regex before but it was slow as fuck

Conclusion: i dont recommend using regex for your lexer

## Parser

This file is a long ass match statement
Pretty easy to follow UNTIL you go to parsing *match* statement.
Leave that part out.

## Interpreter

LONG FUCKING FILE OF ONE SINGLE IMPL BLOCK
HORRIBLE IMPLENTATION TO WHOMEVER TRIES TO EDIT THIS GOD SHOULD PROTECT YOU
IM SORRY FOR IT

## Runtime

The runtime directory is a dir with files like `utils`, `value`, `preprocessor`...
It has many files with just implementation with functions used otherwhere.

Files:
- `cache.rs` - File for caching
- `config.rs` - Config struct and utils
- `errors.rs` - Error struct
- `fmt.rs` - Formatting utilities (TODO)
- `functions.rs` - Function struct for lucia runtime (HORRIBLE IMPLENTATION)
- `generators.rs` - Generators struct for lucia runtime
- `internal_structs.rs` - Internal structs used in runtime
- `libs.rs` - Library management for runtime
- `modules.rs` - Module struct for libraries and stuff for lucia runtime
- `native.rs` - Native functions and integrations for runtime
- `pattern_reg.rs` - Pattern recognition (regocnition as misspelling) for lists
- `precompile.rs` - Precompilation utilities for lucia preprocessor time
- `preprocessor.rs` - Preprocessor implementation for lucia
- `repl.rs` - Input reading for REPL
- `statements.rs` - Statement struct (ALSO HORRIBLE IMPLENTATION)
- `static_checker.rs` - Static checking utilities for lucia (TODO)
- `structs_and_enums.rs` - Structs and Enums implementation for lucia runtime
- `tokens.rs` - Token struct for lucia runtime
- `types.rs` - Types implementation for lucia runtime
- `utils.rs` - Utilities for lucia runtime (BIGASS FILE)
- `value.rs` - Value enum for lucia runtime
- `variables.rs` - Variables implementation for lucia runtime
- `wasm.rs` - WASM main file, moved when compiling to WASM

## Assets

This is a directory with assets like installer files, images, mode files (emacs, vscode) and symlinks.

### Installer

The installer files are in `src/env/assets/installer/`.

The main file is `LuciaInstaller.nsi`, which is the NSIS script for building the installer.

use `make installer` to build the installer.

to faster build of installer run `make installer-dbg` (debug build).

## Libraries

The libraries are in `src/env/libs/`.

Each library has its own directory with a `main.rs` file (or `main.lc` for lucia libs) which is the entry point for the library.

Each library using rust must be registered in `src/env/runtime/libs.rs` and `src/env/libs.json` and mainly in the `mod` in `src/main.rs`.

## Tests

This directory contains tests for lucia.

Each files name starts with a number following the pattern `XX_description.lc` where `XX` is the test number.

Tests should use the `std/assert` to assert things.

## END
this is not finnsihed. im working on it and ill add more details here later.
im sorry if you decided to contribute and this note is incomplete.
feel free to ask me on [reddit](https://reddit.com/u/SirPigari) 

thank you