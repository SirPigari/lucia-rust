# Versioning

Lucia follows [**Semantic Versioning**](https://semver.org/).

## Current Version

The language version has jumped from `1.3.1` (Python implementation) to **`2.0.0`** to mark the rewrite in Rust and major architecture changes.

## Note

The current version is `2.0.0` and will be until the first stable release of the Rust implementation.
After that, all future versions will be updated right after merge of new features or bug fixes.

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

## Suffixes

- Pre-release versions can be denoted with a hyphen and identifier (e.g., `2.1.0-alpha`, `2.1.0-beta.1`).
- Build metadata can be added with a plus sign (e.g., `2.1.0+build.123`).
- Pre-release and build metadata do not affect version precedence.
- Release candidates should be marked as `-rc.X` (e.g., `2.0.0-rc.1`).
- Final stable releases should not have any suffixes.
- If any version above `2.0.0` uses the python as base, it must have the suffix `-py` (e.g., `2.1.0-py`) (Though this is not likely, because python is outdated for this and would require significant changes).
- Builds for specific CPU architectures should be marked with `+arch.X` (e.g., `2.2.0+arch.x86_64`).
- Debug or development builds should be marked with `-dev` (e.g., `2.2.0-dev`).
- Any experimental or unstable features should be marked with `-e.X` (e.g., `2.2.0-e.1`).
- WASM specific builds should be marked with `-wasm` (e.g., `2.3.0-wasm`).
- Merging any 2 suffixes is allowed (e.g., `2.2.0-rc.1+build.456`, `2.3.0-e.2-wasm`).

### Table
| Suffix        | Description                                      |
|---------------|--------------------------------------------------|
| `-alpha`      | Early testing version with potential instability |
| `-beta`       | More stable than alpha, but still in testing     |
| `-rc.X`       | Release candidate, nearly stable                 |
| `-e.X`        | Experimental features                            |
| `-py`         | Python-based implementation                      |
| `-wasm`       | WebAssembly specific build                       |
| `-dev`        | Development or debug build                       |
| `+build.X`    | Build metadata                                   |
| `+arch.X`     | CPU architecture specific build                  |

## Breaking Changes

Breaking changes include, but are not limited to:
- Changes to core language syntax or semantics.
- Removal or modification of existing features that affect compatibility.
- Changes to standard library functions or modules that alter their behavior (only *removal* of functions or modules count, adding new ones does not).
- Changes to runtime behavior that affect existing code.
- Changes to error handling that alter how exceptions are raised or caught.
- Changes to the module system that affect how modules are imported or exported.
- Changes to type system that affect type checking or inference.
- Changes to built-in functions or objects that alter their behavior or interface.
- Any other changes that would require users to modify their existing code to work with the new version.

Example of breaking changes:

Changing a signature of a built-in function (e.g., changing parameters or return type):
  - From: `fun len(v: any) -> int`
  - To: `fun len(v: impl op_len[any] -> int) -> int`

Example of non-breaking changes:

Adding a new built-in function:
  - New: `fun is_even(n: int) -> bool`

Changing the implementation of a built-in function without changing its signature or behavior:
  - Old: `fun add(a: int, b: int) -> int { return a + b; }`
  - New: `fun add(a: int, b: int) -> int { /* optimized implementation */ return a + b; }`

## Versioning Rules

- Breaking changes **must increment the MAJOR** version.
- New features that do not break compatibility **increment the MINOR** version.
- Bug fixes and minor improvements **increment the PATCH** version.
- Pre-release versions and build metadata can be added following semver specs if needed.

## Releasing

- Before a release, all tests must pass (`make test`)
- Update the version number in `Cargo.toml` under `[package]`:
  
  ```toml
  version = "2.0.0"
  ```

- Update version references in documentation and markdown files.
- Tag the release in git with the version number (e.g., `v2.0.0`).
- Update changelog and docs to reflect the changes.

### Before releasing make sure that:
[ ] All breaking changes are documented.
[ ] The version number follows the versioning rules.
[ ] All tests pass and code is stable.
[ ] Documentation is updated with new version information.
[ ] Changelog reflects all changes since the last release.
[ ] Git tag is created for the release.
[ ] Any debug or development code is removed.
[ ] All [examples](./examples/) are updated to work with the new version.
[ ] The release candidate is tested thoroughly.
[ ] The release is announced in relevant channels.

---

Following this keeps Lucia's versioning clear and consistent through major rewrites.
