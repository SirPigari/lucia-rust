# KNOWN ISSUES — Lucia

This document tracks known bugs, limitations, and problematic behavior in the Lucia language. Please update as new issues are discovered or resolved.

---

## Format  
Each issue should follow this structure:

### [Short Title]
- **ID**: `lucia-XXXX`  
- **Severity**: [Low | Medium | High | Critical]  
- **Status**: [Open | In Progress | Resolved | Wontfix]  
- **Component**: [Lexer | Preprocessor | Parser | Interpreter | REPL | other]  
- **Discovered in**: `vX.Y.Z`

- **Description**:  
  Brief explanation of the issue. Include code examples if relevant.

- **Reproduction**:  
  Steps to reproduce the issue.

- **Expected Behavior**:  
  Expected behavior.

- **Actual Behavior**:  
  Actual Behavior.

- **Workaround**:  
  Describe any known workaround, or state "None".

---

## Known Issues

### Invalid Tokenization After Number  
- **ID**: `lucia-0002`  
- **Severity**: Medium  
- **Status**: Resolved  
- **Component**: Lexer  
- **Discovered in**: `v2.0.0`

- **Description**:  
  When an identifier immediately follows a number (e.g. `00help()`), the lexer incorrectly tokenizes the identifier into overlapping substrings: `"help"`, `"elp"`, `"lp"`, and `"p"` instead of as a single identifier token.

  ```lucia-repl
  >>> 00help()
  Tokens: [("NUMBER", "00"), ("IDENTIFIER", "help"), ("IDENTIFIER", "elp"), ("IDENTIFIER", "lp"), ("IDENTIFIER", "p"), ("SEPARATOR", "("), ("SEPARATOR", ")")]
  ```

- **Reproduction**:
  1. Launch the Lucia REPL.
  2. Input: `00help()`
  3. Observe incorrect token stream.

- **Expected Behavior**:  
  The lexer should correctly recognize `help` as a single `IDENTIFIER` token after the number:

  ```lucia-repl
  Tokens: [("NUMBER", "00"), ("IDENTIFIER", "help"), ("SEPARATOR", "("), ("SEPARATOR", ")")]
  ```

- **Actual Behavior**:  
  The identifier is tokenized into multiple overlapping substrings instead of one token.

- **Workaround**:
  Insert whitespace between the number and the identifier (e.g., `00 help()`) to ensure correct tokenization.

### Nested index assignment  
- **ID**: `lucia-0001`  
- **Severity**: Medium  
- **Status**: Open  
- **Component**: Interpreter  
- **Discovered in**: `v2.0.0`

- **Description**:  
  Assigning to nested indexes directly (e.g. `l[1][1] = 3`) does not work correctly.  
  The interpreter treats `l[1]` as a value, not a reference, so the inner list is not modified - instead, it's overwritten.

- **Reproduction**:  
  ```lucia
  l: list = [0, [1, 2]]
  l[1][1] = 3
  ```

- **Expected Behavior**:  
  ```lucia
  l == [0, [1, 3]]
  ```

- **Actual Behavior**:  
  ```lucia
  l == [0, 3]
  ```

- **Workaround**:  
  ```lucia
  l: list = [0, [1, 2]]
  temp: list = l[1]
  temp[1] = 3
  l[1] = temp
  // forget temp
  ```
