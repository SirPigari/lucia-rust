# Guide to `config.json`

The `config.json` file contains configurable options controlling the behavior and appearance of the Lucia programming environment.

---

## Location of `config.json`

The `config.json` file is located in the root directory of your Lucia environment (`src/env/config.json`). You can access and edit this file to adjust the settings to your preferences.

---

## Structure of `config.json`

```json
{
  "moded": false,
  "debug": false,
  "debug_mode": "normal",
  "supports_color": true,
  "use_lucia_traceback": true,
  "warnings": true,
  "use_preprocessor": true,
  "allow_fetch": true,
  "allow_unsafe": false,
  "home_dir": "lucia/src/env",
  "recursion_limit": 9999,
  "version": "2.0.0",
  "color_scheme": {
    "exception": "#F44350",
    "warning": "#F5F534",
    "help": "#21B8DB",
    "debug": "#434343",
    "comment": "#757575",
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
- **Description:** Activates or deactivates mode functionality in Lucia. When set to `true`, the "mode" feature is enabled.  
- **Note:** If `moded` is `true`, the Lucia runtime won't complain about version mismatches and the config will not be overwritten.

### 2. `debug`

- **Type:** `bool`  
- **Description:** Enables or disables debugging mode. Set to `true` to turn on debugging.  
- **Note:** Enabling this slows down the program due to extra print statements.

### 3. `debug_mode`

- **Type:** `string`  
- **Values:** `"full"`, `"normal"`, `"minimal"`  
- **Description:** Level of debug information to provide:  
  - **full:** tokens, AST, and interpreter info  
  - **normal:** interpreter info only  
  - **minimal:** tokens and AST only

### 4. `supports_color`

- **Type:** `bool`  
- **Description:** Enables ANSI color codes in terminal output.

### 5. `use_lucia_traceback`

- **Type:** `bool`  
- **Description:** If set to `false`, it prints a minimized version of the error without traceback info instead of the full Lucia traceback.

### 6. `warnings`

- **Type:** `bool`  
- **Description:** Enables or disables warnings.
- **Note:** May get removed in future.

### 7. `use_preprocessor`

- **Type:** `bool`  
- **Description:** Enables preprocessor directives like `#include` or `#alias`.

### 8. `allow_fetch`

- **Type:** `bool`  
- **Description:** Enables the `fetch` function to retrieve external data.

### 9. `allow_unsafe`

- **Type:** `bool`  
- **Description:** Enables use of unsafe operations within Lucia code, which may be needed for certain low-level features.

### 10. `home_dir`

- **Type:** `string`  
- **Description:** Directory path where the Lucia environment lives. Used to find resources and assets.

### 11. `recursion_limit`

- **Type:** `int`  
- **Description:** Maximum allowed recursion depth in Lucia programs. Default is `9999`.

### 12. `version`

- **Type:** `string`  
- **Description:** Current version of Lucia. Should be updated with each new release (currently `"2.0.0"`).

### 13. `color_scheme`

- **Type:** `object`  
- **Description:** Defines terminal colors for various output elements. Each property specifies a hex color string.

| Property       | Description                      |
| -------------- | -------------------------------- |
| `exception`    | Color for exception messages     |
| `warning`      | Color for warning messages       |
| `help`         | Color for help messages          |
| `debug`        | Color for debug output           |
| `comment`      | Color for comments               |
| `input_arrows` | Color for input prompt arrows    |
| `note`         | Color for notes/messages         |
| `output_text`  | Color for output text            |
| `info`         | Color for informational messages |

-**Note:** `comment` and `warning` may get removed in future.
