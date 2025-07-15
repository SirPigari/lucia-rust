# Contributing to Lucia

Thanks for your interest in contributing to Lucia.

Lucia is a expressive scripting programming language written in Rust. Contributions to its core, standard library, tooling, documentation, and ecosystem are welcome.

## Requirements for Contributions

- All changes **must pass all tests**:
  ```bash
  make test
  ```
- Code **must work cross-platform** (Windows, Linux, macOS).
- Changes **must pass the GitHub workflow tests** before merging.

## Project Structure

```
.github/workflows/              # workflows
src/
  env/
    assets/                     # Icons, VSIX extension, and other assets
    bin/                        # Binaries (lucia.exe and others)
    Docs/
      examples/                 # Many .lc example files
      # Markdown docs and related files
    libs/                       # Lucia standard libs and packages installed with lym (Lucia's package manager)
    runtime/                    # All other Rust source files (.rs)
    config.json
  interpreter.rs
  lexer.rs
  parser.rs
  main.rs

tests/
  source/
    run_tests.rs
    run_benchmarks.rs
  # Tests numbered from 01 upwards

.gitignore
.gitattributes
build.rs
Cargo.toml
LICENSE
Makefile
README.md
```

## Adding New Features or Tests

- Every new feature **must include tests** added to the `tests` directory.
- Tests should use 
  ```lucia
  #include <std/macros>
  ```
  for assertion macros.
- Test files must follow the numbering scheme starting from `01` and incrementing by one (`01`, `02`, ..., `99`).
- Keep test names unique and descriptive, following the numbering sequence.

## Workflow

1. Fork the repo and clone your fork.
2. Create a branch for your feature or fix.
3. Implement your changes and tests.
4. Run all tests:
   ```bash
   make test
   ```
5. Ensure your changes pass on all platforms you can test.
6. Push your branch and open a pull request with a clear description.

Thanks for helping make Lucia better!
