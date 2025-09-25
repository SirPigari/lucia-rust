# Versioning

Lucia follows [**Semantic Versioning**](https://semver.org/).

## Current Version

The language version has jumped from `1.3.1` (Python implementation) to **`2.0.0`** to mark the rewrite in Rust and major architecture changes.

## Version Format

Versions use this format:

```
MAJOR.MINOR.PATCH
```

- **MAJOR** version when you make incompatible API or language changes.
- **MINOR** version when you add functionality in a backwards-compatible manner.
- **PATCH** version when you make backwards-compatible bug fixes.

## Examples

- `2.0.0` — First stable Rust implementation, breaking changes from Python version.
- `2.1.0` — Added new language features compatible with Rust implementation.
- `2.1.2` — Bug fixes and improvements without breaking compatibility.

## Versioning Rules

- Breaking changes **must increment the MAJOR** version.
- New features that do not break compatibility **increment the MINOR** version.
- Bug fixes and minor improvements **increment the PATCH** version.
- Pre-release versions and build metadata can be added following semver specs if needed.

## Releasing

- Before a release, all tests must pass.
- Update the version number in `Cargo.toml` under `[package]`:
  
  ```toml
  version = "2.0.0"
  ```

- Update version references in documentation and markdown files.
- Tag the release in git with the version number (e.g., `v2.0.0`).
- Update changelog and docs to reflect the changes.

---

Following this keeps Lucia’s versioning clear and consistent through major rewrites.
