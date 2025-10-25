;; Lucia-mode
(define-derived-mode lucia-mode prog-mode "Lucia"
  "Major mode for editing Lucia language files."
  (setq font-lock-defaults
        '((

          ;; comments
          ("//.*$" . font-lock-comment-face)

          ;; operators
          ("->\\|>=\\|<=\\|==\\|!=\\|\\+=\\|-=\\|\\*=\\|/=\\|=\\|<<\\|>>\\|\\+\\+\\|--\\|\\+\\|-\\|\\^\\|\\*\\|/\\|>\\|<\\|!\\|%\\|\\|\\|\\|&&\\||\\|#\\|~\\|\\$\\|\\?|&\\|\\^=\\|%=\\|\\.\\.\\.\\|\\.\\.\\|[(){}\\[\\];:.,\\?]" . font-lock-builtin-face)

          ;; identifiers
          ("\\bnon-static\\b\\|\\b[a-zA-Z_][a-zA-Z0-9_]*\\b" . font-lock-variable-name-face)

          ;; types
          ("\\b\\(void\\|any\\|int\\|float\\|bool\\|str\\|map\\|list\\|function\\|generator\\|bytes\\|tuple\\|auto\\|type\\|impl\\)\\b" . font-lock-type-face)

          ;; keywords
          ("\\b\\(for\\|while\\|as\\|from\\|import\\|throw\\|end\\|catch\\|try\\|fun\\|return\\|static\\|non-static\\|public\\|private\\|final\\|mutable\\|if\\|else\\|then\\|in\\|forget\\|and\\|or\\|not\\|isnt\\|is\\(?:n't\\)?\\|xor\\|xnor\\|nein\\|match\\|break\\|continue\\|defer\\|scope\\|pass\\|band\\|lshift\\|rshift\\|bor\\|bnot\\|typedef\\|where\\|gen\\|export\\|struct\\|enum\\)\\b(?!\\s*=)" . font-lock-keyword-face)

          ;; numbers
          ("-?\\(\\d+\\#[0-9a-zA-Z_]+\\|0[bB][01]+(?:_[01]+)*\\|0[oO][0-7]+(?:_[0-7]+)*\\|0[xX][\\da-fA-F]+(?:_[\\da-fA-F]+)*\\|\\.\\d+(?:_\\d+)*\\|(?:\\d+(?:_\\d)*)(?:\\.\\d+(?:_\\d+)*)?(?:[eE][+-]?\\d+)?\\)" . font-lock-constant-face)

          ;; booleans/null
          ("\\b\\(true\\|false\\|null\\)\\b" . font-lock-constant-face)

          ;; preprocessor / macros
          ("\\(#(?:\\w+(?:-\\w+)?)(?:\\s+#(?:\\w+(?:-\\w+)?))*\\)\\b" . font-lock-preprocessor-face)
          ("#config\\.[A-Z_]+\\b" . font-lock-preprocessor-face)
        )))
	(font-lock-mode 1))

(add-to-list 'auto-mode-alist '("\\.\\(lucia\\|lc\\)\\'" . lucia-mode))

(provide 'lucia-mode)
