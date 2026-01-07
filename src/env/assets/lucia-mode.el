;;; lucia-mode.el --- Major mode for Lucia programming language -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Markofwitch

;; Author: Markofwitch
;; Version: 1.0.0
;; Keywords: languages
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:

;; Major mode for editing Lucia programming language files (.lc, .lucia)
;; Provides syntax highlighting, indentation, and basic editing support.

;;; Code:

(defvar lucia-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Comments
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?\n "> b" st)
    (modify-syntax-entry ?< ". 1c" st)
    (modify-syntax-entry ?# ". 2c" st)
    (modify-syntax-entry ?> ". 4c" st)
    
    ;; Strings
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\' "\"" st)
    (modify-syntax-entry ?\\ "\\" st)
    
    ;; Operators
    (modify-syntax-entry ?+ "." st)
    (modify-syntax-entry ?- "." st)
    (modify-syntax-entry ?= "." st)
    (modify-syntax-entry ?% "." st)
    (modify-syntax-entry ?& "." st)
    (modify-syntax-entry ?| "." st)
    (modify-syntax-entry ?^ "." st)
    (modify-syntax-entry ?! "." st)
    (modify-syntax-entry ?~ "." st)
    (modify-syntax-entry ?$ "." st)
    (modify-syntax-entry ?? "." st)
    
    ;; Delimiters
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\} "){" st)
    
    st)
  "Syntax table for `lucia-mode'.")

(defconst lucia-keywords
  '("for" "while" "as" "from" "import" "throw" "end" "catch" "try" "fun"
    "return" "static" "non-static" "public" "private" "final" "mutable"
    "if" "else" "then" "in" "forget" "and" "or" "not" "isnt" "is" "isn't"
    "xor" "xnor" "nein" "match" "break" "continue" "defer" "scope" "pass"
    "band" "lshift" "rshift" "bor" "bnot" "typedef" "where" "gen" "export"
    "struct" "enum")
  "Lucia language keywords.")

(defconst lucia-types
  '("void" "any" "int" "float" "bool" "str" "map" "list" "function"
    "generator" "bytes" "tuple" "auto" "type" "impl")
  "Lucia language types.")

(defconst lucia-constants
  '("true" "false" "null")
  "Lucia language constants.")

(defconst lucia-font-lock-keywords
  (list
   ;; Preprocessor directives
   '("#\\(?:\\w+\\(?:-\\w+\\)?\\)\\(?:\\s-+#\\(?:\\w+\\(?:-\\w+\\)?\\)\\)*" . font-lock-preprocessor-face)
   '("#config\\.[A-Z_]+" . font-lock-preprocessor-face)
   
   ;; Keywords
   `(,(regexp-opt lucia-keywords 'words) . font-lock-keyword-face)
   
   ;; Types
   `(,(regexp-opt lucia-types 'words) . font-lock-type-face)
   
   ;; Constants (booleans and null)
   `(,(regexp-opt lucia-constants 'words) . font-lock-constant-face)
   
   ;; Numbers (including various bases and underscores)
   '("\\b-?\\(?:\\d+#[0-9a-zA-Z_]+\\|0[bB][01]+\\(?:_[01]+\\)*\\|0[oO][0-7]+\\(?:_[0-7]+\\)*\\|0[xX][[:xdigit:]]+\\(?:_[[:xdigit:]]+\\)*\\|\\.\\d+\\(?:_\\d+\\)*\\|\\(?:\\d+\\(?:_\\d+\\)*\\)\\(?:\\.\\d+\\(?:_\\d+\\)*\\)?\\(?:[eE][+-]?\\d+\\)?\\)\\b" . font-lock-constant-face)
   
   ;; Macros (name followed by !)
   '("\\([a-zA-Z_][a-zA-Z0-9_]*\\)!" 1 font-lock-preprocessor-face)
   
   ;; Function calls
   '("\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\s-*(" 1 font-lock-function-name-face)
   
   ;; String prefixes (f, r, b and combinations)
   '("\\(?:[frb]*[frb]\\)\\(?:\"\\|'\\)" . font-lock-string-face)
   
   ;; Operators
   '("\\(->\\|>=\\|<=\\|==\\|!=\\|\\+=\\|-=\\|\\*=\\|/=\\|\\^=\\|%=\\|<<\\|>>\\|\\+\\+\\|--\\|\\.\\.\\.\\|\\.\\.\\|[+\\-*/%<>=!&|^~$?:#]\\)" . font-lock-builtin-face))
  "Keyword highlighting for `lucia-mode'.")

(defun lucia-indent-line ()
  "Indent current line as Lucia code."
  (interactive)
  (let ((indent-col 0)
        (cur-indent (current-indentation))
        (point-offset (- (current-column) cur-indent)))
    (save-excursion
      (beginning-of-line)
      (if (bobp)
          (setq indent-col 0)
        (let ((not-indented t))
          (if (looking-at "^[ \t]*\\(end\\|else\\|catch\\|}\\|)\\|]\\)")
              (progn
                (save-excursion
                  (forward-line -1)
                  (setq indent-col (max 0 (- (current-indentation) tab-width))))
                (setq not-indented nil))
            (save-excursion
              (while not-indented
                (forward-line -1)
                (if (bobp)
                    (setq not-indented nil)
                  (cond
                   ((looking-at "^[ \t]*\\(end\\|else\\|catch\\|}\\)")
                    (setq indent-col (current-indentation))
                    (setq not-indented nil))
                   ((looking-at "^.*\\(if\\|else\\|for\\|while\\|fun\\|try\\|catch\\|struct\\|enum\\|match\\|{\\).*$")
                    (setq indent-col (+ (current-indentation) tab-width))
                    (setq not-indented nil))
                   ((looking-at "^[ \t]*$")
                    t)
                   (t
                    (setq indent-col (current-indentation))
                    (setq not-indented nil))))))))))
    (indent-line-to indent-col)
    (when (> point-offset 0)
      (forward-char point-offset))))

;;;###autoload
(define-derived-mode lucia-mode prog-mode "Lucia"
  "Major mode for editing Lucia programming language files."
  :syntax-table lucia-mode-syntax-table
  
  ;; Code for syntax highlighting
  (setq-local font-lock-defaults '(lucia-font-lock-keywords))
  
  ;; Comments
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "\\(?://+\\|/\\*+\\|<#\\)\\s-*")
  
  ;; Indentation
  (setq-local indent-line-function 'lucia-indent-line)
  (setq-local tab-width 4)
  
  ;; Electric indentation
  (setq-local electric-indent-chars
              (append "{}()[]:" electric-indent-chars)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.lc\\'" . lucia-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.lucia\\'" . lucia-mode))

(provide 'lucia-mode)

;;; lucia-mode.el ends here