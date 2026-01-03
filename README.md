<div align="center">
  <img src="src/env/assets/logo_lucia.png" alt="Lucia logo" style="height: 180px;">
  <p>A lightweight, expressive scripting language with Python-like syntax, built in <a href="https://www.rust-lang.org/"><img src="https://upload.wikimedia.org/wikipedia/commons/0/0f/Original_Ferris.svg" alt="Rust" style="height:1em; vertical-align: text-bottom; position: relative; top: 2px;"/>Rust</a>.</p>
</div>

---

[![Linux](https://github.com/SirPigari/lucia-rust/actions/workflows/test-linux.yml/badge.svg?branch=main)](https://github.com/SirPigari/lucia-rust/actions/workflows/test-linux.yml)
[![macOS](https://github.com/SirPigari/lucia-rust/actions/workflows/test-macos.yml/badge.svg?branch=main)](https://github.com/SirPigari/lucia-rust/actions/workflows/test-macos.yml)
[![Windows](https://github.com/SirPigari/lucia-rust/actions/workflows/test-windows.yml/badge.svg?branch=main)](https://github.com/SirPigari/lucia-rust/actions/workflows/test-windows.yml)

---

> NOTE:  
> Lucia is not done yet. Anything can change at any moment.

## Why Lucia?

Lucia is built to be clean, expressive, and simple to learn -  
but with enough depth to handle serious scripting needs.  

It's designed for speed and safety, without sacrificing elegance.

---

## Features

- Clean, easy-to-read syntax inspired by Python and Lua (Kotlin too)  
- Static types  
- No abstraction for references - everything is explicit  
- Entirely written in Rust for reliability and speed  
- Designed to run efficiently across all major platforms  
- Beginner-friendly, while allowing advanced usage  
- Integrated package manager [**Lym**](https://github.com/SirPigari/lym)  
- Open source under GPLv3  

---

## Documentation

- [x] [Introduction](src/env/Docs/introduction.md)
- [x] [Installation guide](src/env/Docs/installation-guide.md)
- [x] [Config guide](src/env/Docs/config-guide.md)
- [x] [Syntax](src/env/Docs/language-syntax.md)
- [x] [C FFI](src/env/Docs/c-ffi.md)
- [x] [Embedding](src/env/Docs/embedding.md)
- [x] [Examples](src/env/Docs/examples/)
- [x] [Conventions](src/env/Docs/conventions.md)
- [ ] [Standard libraries API](src/env/Docs/standard-libs.md)
- [x] [REPL Guide](src/env/Docs/repl-guide.md)
- [x] [Versioning](src/env/Docs/versioning.md)
- [x] [Contributing guide](src/env/Docs/contributing.md)
- [x] [Changelog](src/env/Docs/CHANGELOG.md)
- [x] [Known issues/bugs](src/env/Docs/known-issues.md)
- [x] [TODOs](src/env/Docs/todos.md)

See [Lucia Docs reader](https://sirpigari.github.io/lucia-apl/docs/reader)

---

## Benchmarks

Lucia runs at `375` hello worlds per second in debug, or `712` in release.

For more benchmarks refer to [benchmarks.md](src/env/Docs/benchmarks.md)

## Support the Project

[![Buy me a coffee](https://img.shields.io/badge/Buy_Me_A_Coffee-Donate-pink?logo=buy-me-a-coffee&logoColor=pink)](https://coff.ee/sirpigari)

If you don't want to donate, but still want to help:

- Star the repository  
- Contribute to the codebase  
- Report issues or suggest features  
- Share the project with others

---

## Roadmap

- [x] Rewrite to Rust  
- [x] Optimize lexer  
- [x] Optimize AST  
- [ ] Optimize parser  
- [ ] Optimize function calls
- [ ] Optimize interpreter  
- [ ] Do all [TODOs](src/env/Docs/todos.md)

---

## Platform Support

Tested on:

- ![Windows](https://img.shields.io/badge/Windows-Supported-blue?logo=windows&logoColor=white)  
- ![macOS](https://img.shields.io/badge/macOS-Supported-lightgrey?logo=apple)  
- ![Ubuntu Linux](https://img.shields.io/badge/Ubuntu_Linux-Supported-E95420?logo=ubuntu&logoColor=white)  
- ![Arch Linux](https://img.shields.io/badge/Arch_Linux-Supported-1793D1?logo=arch-linux&logoColor=white)  

Built on:

- Everything above
- ![WASM 32](https://img.shields.io/badge/WebAssembly-Supported-654FF0?logo=webassembly&logoColor=white)

---

## Community

Join us on Discord: [discord.gg/placeholder-rn](https://discord.gg/placeholder-rn)

---

## Legacy

Check out the legacy (Python) version of Lucia:  
[SirPigari/lucia](https://github.com/SirPigari/lucia)

---

## License

This project is licensed under the [GPLv3 License](LICENSE).
