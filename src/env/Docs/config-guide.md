# Guide to `config.json`

The `config.json` file contains configurable options controlling the behavior and appearance of the Lucia programming environment.

---

## Default `config.json` Structure

```json
{
  "moded": false,
  "debug": false,
  "debug_mode": "normal",
  "supports_color": true,
  "use_lucia_traceback": true,
  "warnings": true,
  "cache_format": "no_cache",
  "allow_fetch": true,
  "allow_unsafe": false,
  "allow_inline_config": true,
  "disable_runtime_type_checking": false,
  "home_dir": "lucia/src/env",
  "libs_paths": [
    "lucia/src/env/libs"
  ],
  "stack_size": 16777216,
  "version": "2.0.0",
  "type_checker": {
    "enabled": false,
    "strict": false,
    "run_unchecked": false,
    "nested_functions": true,
    "nested_loops": true,
    "warn_on_any": false,
    "warnings": true,
    "treat_warnings_as_errors": false,
    "max_errors": 100,
    "ignore_warnings": [],
    "ignore_errors": [],
    "ignore_modules": [],
    "pointer_types": true,
    "experimental_constant_folding": false,
    "check_imports": true,
    "allow_dynamic_casts": false,
    "log_level": "error",
    "verbose": false,
    "track_value_origins": true,
    "fail_fast": true,
    "max_nested_depth": null,
    "try_to_auto_fix": false
  },
  "color_scheme": {
    "exception": "#F44350",
    "warning": "#F5F534",
    "help": "#21B8DB",
    "debug": "#434343",
    "input_arrows": "#136163",
    "note": "#1CC58B",
    "output_text": "#BCBEC4",
    "info": "#9209B3"
  }
}
```

---

## Configuration Options

### 1. `moded`

- **Type:** `bool`  
- **Description:** Activates “mode” functionality. When `true`, Lucia may skip version checks and preserve the config. Default: `false`

### 2. `debug`

- **Type:** `bool`  
- **Description:** Enables debug output. More info slows down execution. Default: `false`

### 3. `debug_mode`

- **Type:** `string`  
- **Values:** `"full"`, `"normal"`, `"minimal"`  
- **Description:** Level of debug information:  
  - `"full"` → tokens, AST, interpreter info  
  - `"normal"` → interpreter info only  
  - `"minimal"` → tokens and AST only  
- **Default:** `"normal"`

### 4. `supports_color`

- **Type:** `bool`  
- **Description:** Enables colored output in terminals that support ANSI color codes. Default: `true`

### 5. `use_lucia_traceback`

- **Type:** `bool`  
- **Description:** Shows full Lucia traceback on errors. Default: `true`

### 6. `warnings`

- **Type:** `bool`  
- **Description:** Enables warnings in the console. Default: `true`

### 7. `cache_format`

- **Type:** `string`  
- **Description:** Format used for storing compilation caches. Default: `"no_cache"`  

| Value            | Description                                                  | Speed        | Size       |
|------------------|--------------------------------------------------------------|--------------|------------|
| `no_cache`       | Disables caching entirely                                    | N/A          | None       |
| `bin_le`         | Uncompressed binary (little endian)                          | Fastest      | Large      |
| `bin_be`         | Uncompressed binary (big endian)                             | Fastest      | Large      |
| `zstd_le_fast`   | Zstd-compressed binary (little endian), fast compression     | Fast         | Medium     |
| `zstd_be_fast`   | Zstd-compressed binary (big endian), fast compression        | Fast         | Medium     |
| `zstd_le_best`   | Zstd-compressed binary (little endian), slow compression     | Slow         | Small      |
| `zstd_be_best`   | Zstd-compressed binary (big endian), slow compression        | Slow         | Small      |
| `json`           | Human-readable JSON, very slow I/O                           | Slowest      | Very Large |

- **Note:** Cache is experimental.

### 8. `allow_fetch`

- **Type:** `bool`  
- **Description:** Enables the `fetch` function to retrieve external data. Default: `true`

### 9. `allow_unsafe`

- **Type:** `bool`  
- **Description:** Enables unsafe operations in Lucia code for low-level access. Default: `false`

### 10. `allow_inline_config`

- **Type:** `bool`  
- **Description:** Allows configuration directives inline in scripts. Default: `true`

### 11. `home_dir`

- **Type:** `string`  
- **Description:** Base directory for Lucia resources. Default: `"lucia/src/env"`

### 12. `libs_paths`

- **Type:** `list`  
- **Description:** List of directories to search for libraries when using `import`. Default: `["lucia/src/env/libs"]`

### 13. `stack_size`

- **Type:** `int`  
- **Description:** Maximum stack size for running programs in bytes. Default: `16777216` (16 MB)

### 14. `version`

- **Type:** `string`  
- **Description:** Current version of Lucia. Update with new releases. Default: `"2.0.0"`

### 15. `type_checker`

- **Type:** `object`
- **Description:** Configuration for the optional type checker. (experimental)

### 16. `color_scheme`

- **Type:** `object`  
- **Description:** Defines colors for terminal output. Default values:  

| Property       | Description                      | Default Color |
| -------------- | -------------------------------- | ------------- |
| `exception`    | Exception messages               | `#F44350`     |
| `warning`      | Warning messages                 | `#F5F534`     |
| `help`         | Help messages                    | `#21B8DB`     |
| `debug`        | Debug output                     | `#434343`     |
| `input_arrows` | Input prompt arrows              | `#136163`     |
| `note`         | Notes or informational messages  | `#1CC58B`     |
| `output_text`  | Standard program output          | `#BCBEC4`     |
| `info`         | Info messages                    | `#9209B3`     |
