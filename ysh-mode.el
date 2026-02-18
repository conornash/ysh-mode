;;; ysh-mode.el --- Major mode for YSH (Oils shell) -*- lexical-binding: t; -*-

;; Author: Claude Code
;; Version: 0.1.0
;; Keywords: languages, shell
;; URL: https://www.oilshell.org/
;; Package-Requires: ((emacs "27.1"))

;; YSH syntax highlighting based on the oils.vim Vim plugin.
;; Covers: keywords, builtins, strings (5 kinds + triple-quoted variants),
;; comments, variable substitutions, sigil pairs, expression atoms,
;; proc/func definitions, backslash escapes, and J8 string escapes.

;;; Commentary:

;; YSH is the expression language in the Oils project (https://www.oilshell.org/).
;; It extends shell with typed data, expressions, proc/func definitions,
;; J8 strings, sigil pairs like $[] @[] ^[], and more.
;;
;; This mode provides syntax highlighting modeled after the oils.vim plugin at:
;; https://github.com/oilshell/oil.vim

(require 'rx)

;; ---------------------------------------------------------------------
;; Custom faces — mirroring the Vim highlight groups
;; ---------------------------------------------------------------------

(defgroup ysh nil
  "Major mode for editing YSH files."
  :group 'languages
  :prefix "ysh-")

(defface ysh-expr-face
  '((t :inherit font-lock-type-face))
  "Face for YSH expression contexts (mapped from Vim `yshExpr` → Type)."
  :group 'ysh)

(defface ysh-var-sub-face
  '((t :inherit font-lock-variable-name-face))
  "Face for variable substitutions like $name, ${name}."
  :group 'ysh)

(defface ysh-sigil-pair-face
  '((t :inherit font-lock-constant-face))
  "Face for sigil-pair delimiters $() $[] @() @[] ^() ^[]."
  :group 'ysh)

(defface ysh-func-name-face
  '((t :inherit font-lock-function-name-face))
  "Face for function names in `func` declarations."
  :group 'ysh)

(defface ysh-proc-name-face
  '((t :inherit font-lock-function-name-face))
  "Face for proc names in `proc` declarations."
  :group 'ysh)

(defface ysh-backslash-face
  '((t :inherit font-lock-constant-face))
  "Face for backslash-quoted characters (Vim `backslashQuoted` → Character)."
  :group 'ysh)

(defface ysh-j8-escape-face
  '((t :inherit font-lock-constant-face))
  "Face for J8 string escapes: \\n \\yff \\u{3bc} etc."
  :group 'ysh)

(defface ysh-j8-error-face
  '((t :inherit font-lock-warning-face))
  "Face for invalid backslash escapes in J8 strings."
  :group 'ysh)

;; ---------------------------------------------------------------------
;; Regex building blocks (from lib-regex.vim)
;; ---------------------------------------------------------------------

(defconst ysh--var-name-re "[a-zA-Z_][a-zA-Z0-9_]*"
  "Regex matching a YSH variable / function name.")

(defconst ysh--proc-name-re "[a-zA-Z_-][a-zA-Z0-9_-]*"
  "Regex matching a YSH proc name (hyphens allowed).")

(defconst ysh--first-word-prefix "\\(?:^\\|[;|&]\\)\\s-*"
  "Anchors a keyword to the first word position in a command.")

;; Expression keyword regex — includes if/else for ternary expressions
(defconst ysh--expr-keyword-re
  (concat "\\<"
          (regexp-opt '("and" "or" "not" "is" "as" "capture"
                        "if" "else")
                      t)
          "\\>")
  "Regex matching YSH expression keywords.
Includes `if' and `else' because they appear in ternary expressions:
  = 42 if true else 41
The shell keyword rules (first-word anchored) handle the block forms.")

;; Expression-opening keywords that start an expression context
(defconst ysh--expr-opener-re
  (concat "\\(?:"
          "\\<\\(?:var\\|const\\|setvar\\|setglobal\\|call\\|if\\|elif\\|while\\)\\>"
          "\\|"
          "^\\s-*=\\s-"  ; bare = at first-word position
          "\\)")
  "Regex matching keywords that open an expression context on the same line.
The bare `=' uses a separate pattern since `=' is punctuation (no \\\\>).")

(defun ysh--match-expr-keyword (limit)
  "Font-lock matcher for expression keywords up to LIMIT.
Matches `and', `or', `not', `is', `as', `capture' only when they
appear to be in an expression context — i.e., on a line that contains
an expression-opening keyword before the match.  This prevents
`echo and' from highlighting `and' as a keyword."
  (let ((found nil))
    (while (and (not found)
                (re-search-forward ysh--expr-keyword-re limit t))
      (let* ((beg (match-beginning 0))
             (ppss (save-excursion (syntax-ppss beg))))
        ;; Skip if inside a string or comment
        (unless (or (nth 3 ppss) (nth 4 ppss))
          ;; Check if there's an expression-opening keyword earlier on this line.
          ;; save-match-data is critical: the inner search must not clobber
          ;; the match-data that font-lock will use to apply the face.
          (let ((has-opener
                 (save-excursion
                   (save-match-data
                     (goto-char (line-beginning-position))
                     (re-search-forward ysh--expr-opener-re beg t)))))
            (when has-opener
              (setq found t))))))
    found))

;; Variable substitution regex — matches $name, ${name...}, $0, ${12...}
(defconst ysh--var-sub-re
  (concat "\\$\\(?:"
          "[a-zA-Z_][a-zA-Z0-9_]*"  ; $name
          "\\|{[a-zA-Z_][a-zA-Z0-9_]*[^}]*}"  ; ${name...}
          "\\|[0-9]"                ; $0 .. $9
          "\\|{[0-9]+[^}]*}"       ; ${12...}
          "\\)")
  "Regex matching YSH variable substitutions.")

(defun ysh--match-var-sub (limit)
  "Font-lock matcher for variable substitutions up to LIMIT.
Matches $name, ${name}, $0, ${11} etc. but skips matches that:
 - are inside single-quoted strings (where $ is literal)
 - are preceded by backslash (\\$name is escaped)"
  (let ((found nil))
    (while (and (not found)
                (re-search-forward ysh--var-sub-re limit t))
      (let* ((beg (match-beginning 0))
             (ppss (save-excursion (syntax-ppss beg)))
             (in-string (nth 3 ppss))
             (prev-char (and (> beg 1) (char-before beg))))
        (cond
         ;; Skip if preceded by backslash (escaped)
         ((eql prev-char ?\\) nil)
         ;; Skip if inside a comment
         ((nth 4 ppss) nil)
         ;; Inside a string: only match in double-quoted ("), not single (')
         (in-string
          (when (eql in-string ?\")
            (setq found t)))
         ;; Not in a string: always match
         (t (setq found t)))))
    found))

;; ---------------------------------------------------------------------
;; Font-lock keywords
;; ---------------------------------------------------------------------

(defconst ysh-font-lock-keywords
  (let ((first ysh--first-word-prefix))
    `(
      ;; ----- Comments (from lib-comment-string.vim) -----
      ;; # at beginning of line or preceded by whitespace
      ("^#.*$" . font-lock-comment-face)
      ("[ \t]\\(#.*\\)$" 1 font-lock-comment-face)

      ;; ----- proc / func declarations -----
      ;; `proc my-name` — proc name may contain hyphens
      (,(concat first "\\(proc\\)\\s-+\\(" ysh--proc-name-re "\\)")
       (1 font-lock-keyword-face)
       (2 'ysh-proc-name-face))
      ;; `func myName`
      (,(concat first "\\(func\\)\\s-+\\(" ysh--var-name-re "\\)")
       (1 font-lock-keyword-face)
       (2 'ysh-func-name-face))

      ;; ----- Expression-taking keywords (from lib-command-expr-dq.vim) -----
      ;; const var setvar setglobal call — anchored to first-word position
      (,(concat first "\\(const\\|var\\|setvar\\|setglobal\\|call\\)\\>")
       1 font-lock-keyword-face)
      ;; Bare `= expr` at start of line
      (,(concat first "\\(=\\)\\s-") 1 font-lock-keyword-face)

      ;; ----- Shell / YSH keywords (from lib-command-expr-dq.vim) -----
      ;; Anchored to first-word position so "echo for" does NOT highlight "for".
      (,(concat first
                (regexp-opt
                 '("if" "elif" "else" "case" "while" "for" "in" "time"
                   "break" "continue" "return")
                 t)
                "\\>")
       1 font-lock-keyword-face)

      ;; ----- Expression keywords (contained in expr contexts) -----
      ;; These only apply in expression context (after var/const/setvar/etc).
      ;; Matcher function checks context to avoid "echo and" false positives.
      (ysh--match-expr-keyword 0 font-lock-keyword-face)

      ;; ----- Builtin procs / commands -----
      (,(concat first
                (regexp-opt
                 '("echo" "write" "read" "cd" "pushd" "popd"
                   "source" "use" "shopt" "exit"
                   "assert" "try" "boolstatus"
                   "json" "pp" "type" "append"
                   "hay" "haynode"
                   "fork" "forkwait"
                   "runproc" "invoke"
                   "shvar" "ctx"
                   "test" "exec"
                   "command" "builtin" "true" "false")
                 t)
                "\\>")
       1 font-lock-builtin-face)

      ;; ----- Backslash-quoted chars (from stage3.vim) -----
      ;; \# \' \" \$ \@ \( \) \{ \} \\ \[ \]
      ;; MUST come before var-sub rules so \$ gets backslash-face.
      ;; Note: ] must be first in the character class (Emacs 31 bug with \]).
      ("\\\\[]#'\"$@(){}\\\\[]" 0 'ysh-backslash-face t)

      ;; ----- Variable substitutions (from lib-details.vim) -----
      ;; MUST come before numeric literals so $0 beats plain 0.
      ;; Override t: apply inside double-quoted strings (overrides string-face).
      ;; The matcher function skips single-quoted string contexts.
      (ysh--match-var-sub 0 'ysh-var-sub-face t)
      ;; @splice  (at start of line or after whitespace)
      (,(concat "\\(?:^\\|\\s-\\)\\(@" ysh--var-name-re "\\)") 1 'ysh-var-sub-face)

      ;; ----- Sigil pairs: delimiters (from lib-command-expr-dq.vim) -----
      ;; $( $[ @( @[ ^( ^[  and their closing counterparts
      ("\\(\\$\\|@\\|\\^\\)\\([([\\[]\\)" (1 'ysh-sigil-pair-face) (2 'ysh-sigil-pair-face))
      ;; :| array literal opener
      ("\\(:|\\)" 1 'ysh-sigil-pair-face)

      ;; ----- Expression atoms (from lib-details.vim) -----
      ;; null true false
      ("\\<\\(null\\|true\\|false\\)\\>" . font-lock-constant-face)
      ;; Numeric literals
      ("\\<[0-9]+\\(?:\\.[0-9]+\\)?\\(?:[eE][-+]?[0-9]+\\)?\\>" . font-lock-constant-face)
      ;; 0x hex literals
      ("\\<0[xX][0-9a-fA-F]+\\>" . font-lock-constant-face)

      ;; ----- Special variables -----
      ("\\<\\(ARGV\\|ARGS\\|ENV\\|_reply\\|_status\\|_error\\)\\>" . font-lock-variable-name-face)

      ;; ----- Pipe / logical operators -----
      ("\\(|\\|&&\\|||\\)" 1 font-lock-preprocessor-face)

      ;; ----- `is-main` pattern -----
      ("\\<is-main\\>" . font-lock-builtin-face)

      ;; ----- Shebang line -----
      ("\\`#!.*$" . font-lock-comment-face)
      ))
  "Font-lock keywords for `ysh-mode`.")

;; ---------------------------------------------------------------------
;; Syntax table — strings and comments
;; ---------------------------------------------------------------------

(defvar ysh-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; # is punctuation by default; syntax-propertize promotes it to
    ;; comment-starter only when preceded by whitespace/metacharacters/BOL.
    ;; This prevents mid-word # (echo not#comment) from starting comments.
    (modify-syntax-entry ?# "." st)
    (modify-syntax-entry ?\n ">" st)

    ;; Double-quote — treated as punctuation by default; string parsing is
    ;; handled by syntax-propertize-function to support nested double quotes
    ;; inside $[...] expression subs (the canonical Stage 2 problem).
    (modify-syntax-entry ?\" "." st)

    ;; Single quote — treated as punctuation; string parsing is handled
    ;; by syntax-propertize-function to avoid triple-quote confusion.
    (modify-syntax-entry ?' "." st)

    ;; Parens, brackets, braces
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)
    (modify-syntax-entry ?{ "(}" st)
    (modify-syntax-entry ?} "){" st)

    ;; $ and @ are part of symbol names in variable substitutions
    (modify-syntax-entry ?$ "'" st)
    (modify-syntax-entry ?@ "'" st)

    ;; Backslash is escape
    (modify-syntax-entry ?\\ "\\" st)

    ;; Underscore and hyphen in identifiers
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?- "_" st)

    st)
  "Syntax table for `ysh-mode`.")

;; ---------------------------------------------------------------------
;; Syntactic font-lock for multi-line strings
;; ---------------------------------------------------------------------

(defun ysh--syntax-propertize-extend-region (start end)
  "Extend the syntax-propertize region backward if START is inside a string.
This ensures triple-quoted string openers are included in the region."
  (save-excursion
    (let ((state (syntax-ppss start)))
      (when (nth 3 state)  ; inside a string
        (let ((string-start (nth 8 state)))  ; position of string opener
          (when (and string-start (< string-start start))
            (cons string-start end)))))))

(defun ysh--scan-dq-content (bound)
  "Scan forward through double-quoted string content up to BOUND.
Point should be just after the opening \".  This function handles:
 - Backslash escapes (\\x)
 - $[...] expression subs (with recursive DQ string support)
 - Closing \" (marked with string syntax)
Leaves point after the closing \"."
  (while (< (point) bound)
    (let ((ch (char-after)))
      (cond
       ;; Closing "
       ((eql ch ?\")
        (put-text-property (point) (1+ (point))
                           'syntax-table (string-to-syntax "\""))
        (forward-char 1)
        (setq bound 0))  ; exit loop
       ;; Backslash escape — skip \x
       ((and (eql ch ?\\) (< (1+ (point)) bound))
        (forward-char 2))
       ;; $[ — expression sub: scan for matching ], handling nested strings
       ((and (eql ch ?$)
             (< (1+ (point)) bound)
             (eql (char-after (1+ (point))) ?\[))
        (forward-char 2)  ; skip $[
        (ysh--scan-expr-sub bound))
       ;; Anything else — advance
       (t (forward-char 1))))))

(defun ysh--scan-expr-sub (bound)
  "Scan forward through a $[...] expression sub up to BOUND.
Point should be just after the opening $[.  Tracks bracket depth and
skips over nested strings WITHOUT marking them — the outer DQ string's
delimiters already span the whole range, so inner content just gets
string-face from the enclosing string."
  (let ((depth 1))
    (while (and (> depth 0) (< (point) bound))
      (let ((ch (char-after)))
        (cond
         ;; Nested [ increases depth
         ((eql ch ?\[) (setq depth (1+ depth)) (forward-char 1))
         ;; ] decreases depth
         ((eql ch ?\]) (setq depth (1- depth)) (forward-char 1))
         ;; " inside expr sub — skip over the inner string WITHOUT marking.
         ;; The inner " stays as punctuation; the outer string covers it.
         ((eql ch ?\")
          (forward-char 1)  ; skip opening "
          (while (and (< (point) bound)
                      (not (eql (char-after) ?\")))
            (when (eql (char-after) ?\\)
              (forward-char 1))  ; skip escape
            (forward-char 1))
          (when (< (point) bound) (forward-char 1)))  ; skip closing "
         ;; ' inside expr sub — skip single-quoted string
         ((eql ch ?\')
          (forward-char 1)
          (while (and (< (point) bound) (not (eql (char-after) ?\')))
            (forward-char 1))
          (when (< (point) bound) (forward-char 1)))
         ;; Anything else
         (t (forward-char 1)))))))

(defun ysh--syntax-propertize (start end)
  "Apply syntax properties for YSH string forms between START and END.
Handles (in order):
 1. Triple-quoted single strings: [rbu]?\\='''...\\='''
 2. Triple-quoted double strings: $?\\=\"\\=\"\\=\"...\\=\"\\=\"\\=\"
 3. J8 single-quoted strings: [bu]\\='...\\='  (backslash escapes)
 4. Raw single-quoted strings: r\\='...\\='  (no escapes)
 5. Plain single-quoted strings: \\='...\\='  (no escapes)
 6. Dollar double-quoted strings: $\\=\"...\\=\"
 7. Comment markers: # preceded by whitespace/metacharacters/BOL

Triple-quoted closers are searched up to `point-max' so that
JIT-lock sub-region boundaries do not prevent finding them."

  ;; --- 1. Triple-single-quoted: [rbu]?''' ... ''' ---
  (goto-char start)
  (while (re-search-forward "\\(?:[rbu]\\)?\\('''\\)" end t)
    (let ((open-start (match-beginning 1)))
      (unless (nth 8 (save-excursion (syntax-ppss open-start)))
        (put-text-property open-start (1+ open-start)
                           'syntax-table (string-to-syntax "|"))
        (put-text-property (1+ open-start) (+ open-start 2)
                           'syntax-table (string-to-syntax "."))
        (put-text-property (+ open-start 2) (+ open-start 3)
                           'syntax-table (string-to-syntax "."))
        (when (re-search-forward "'''" (point-max) t)
          (let ((close-end (point)))
            (put-text-property (- close-end 3) (- close-end 2)
                               'syntax-table (string-to-syntax "."))
            (put-text-property (- close-end 2) (- close-end 1)
                               'syntax-table (string-to-syntax "."))
            (put-text-property (- close-end 1) close-end
                               'syntax-table (string-to-syntax "|")))))))

  ;; --- 2. Triple-double-quoted: $?""" ... """ ---
  (goto-char start)
  (while (re-search-forward "\\(?:\\$\\)?\\(\"\"\"\\)" end t)
    (let ((open-start (match-beginning 1)))
      (unless (nth 8 (save-excursion (syntax-ppss open-start)))
        (put-text-property open-start (1+ open-start)
                           'syntax-table (string-to-syntax "|"))
        (put-text-property (1+ open-start) (+ open-start 2)
                           'syntax-table (string-to-syntax "."))
        (put-text-property (+ open-start 2) (+ open-start 3)
                           'syntax-table (string-to-syntax "."))
        (when (re-search-forward "\"\"\"" (point-max) t)
          (let ((close-end (point)))
            (put-text-property (- close-end 3) (- close-end 2)
                               'syntax-table (string-to-syntax "."))
            (put-text-property (- close-end 2) (- close-end 1)
                               'syntax-table (string-to-syntax "."))
            (put-text-property (- close-end 1) close-end
                               'syntax-table (string-to-syntax "|")))))))

  ;; --- 3. J8 single-quoted: [bu]'...' (backslash escapes active) ---
  ;; Use string-quote syntax (") so that \ escapes work — b'\'' is valid.
  ;; Scan forward manually since the escape-aware regex is fragile.
  (goto-char start)
  (while (re-search-forward "\\<[bu]\\('\\)" end t)
    (let ((open-pos (match-beginning 1)))
      (unless (or (get-text-property open-pos 'syntax-table)
                  (nth 8 (save-excursion (syntax-ppss open-pos))))
        ;; Mark opener with string-quote syntax (interacts with \ escape)
        (put-text-property open-pos (1+ open-pos)
                           'syntax-table (string-to-syntax "\""))
        ;; Scan for closing ' — skip \. pairs
        (let ((found nil))
          (while (and (not found) (< (point) (point-max))
                      (not (eql (char-after) ?\n)))
            (cond
             ((and (eql (char-after) ?\\) (< (1+ (point)) (point-max)))
              (forward-char 2))  ; skip \x
             ((eql (char-after) ?\')
              (put-text-property (point) (1+ (point))
                                 'syntax-table (string-to-syntax "\""))
              (forward-char 1)
              (setq found t))
             (t (forward-char 1))))))))

  ;; --- 4. Raw single-quoted: r'...' (no escapes, \ is literal) ---
  ;; Use string-quote syntax (") but disable \ escaping inside.
  (goto-char start)
  (while (re-search-forward "\\<r\\('\\)\\([^'\n]*\\)\\('\\)" end t)
    (let ((open-pos (match-beginning 1))
          (content-beg (match-beginning 2))
          (content-end (match-end 2))
          (close-pos (match-beginning 3)))
      (unless (or (get-text-property open-pos 'syntax-table)
                  (nth 8 (save-excursion (syntax-ppss open-pos))))
        (put-text-property open-pos (1+ open-pos)
                           'syntax-table (string-to-syntax "\""))
        (put-text-property close-pos (1+ close-pos)
                           'syntax-table (string-to-syntax "\""))
        ;; Mark all \ inside as punctuation to prevent escape behavior
        (save-excursion
          (goto-char content-beg)
          (while (search-forward "\\" content-end t)
            (put-text-property (1- (point)) (point)
                               'syntax-table (string-to-syntax ".")))))))

  ;; --- 5. Plain single-quoted: '...' (no escapes, \ is literal) ---
  ;; Use string-quote syntax (") but disable \ escaping inside.
  ;; Skip positions already propertized by earlier passes (triple-quotes).
  (goto-char start)
  (while (re-search-forward "\\('\\)\\([^'\n]*\\)\\('\\)" end t)
    (let ((open-pos (match-beginning 1))
          (content-beg (match-beginning 2))
          (content-end (match-end 2))
          (close-pos (match-beginning 3)))
      (unless (or (get-text-property open-pos 'syntax-table)
                  (nth 8 (save-excursion (syntax-ppss open-pos))))
        (put-text-property open-pos (1+ open-pos)
                           'syntax-table (string-to-syntax "\""))
        (put-text-property close-pos (1+ close-pos)
                           'syntax-table (string-to-syntax "\""))
        ;; Mark all \ inside as punctuation to prevent escape behavior
        (save-excursion
          (goto-char content-beg)
          (while (search-forward "\\" content-end t)
            (put-text-property (1- (point)) (point)
                               'syntax-table (string-to-syntax ".")))))))

  ;; --- 6. Double-quoted strings: "..." and $"..." ---
  ;; Handles nested double quotes inside $[...] expression subs.
  ;; The syntax table marks " as punctuation; we handle all DQ strings here.
  ;; This is the core of Stage 2: recursive sublanguages.
  (goto-char start)
  (while (re-search-forward "\\$?\"" end t)
    (let ((open-pos (match-beginning 0))
          ;; For $"...", the " is one char after the $
          (quote-pos (1- (point))))
      (unless (or (get-text-property quote-pos 'syntax-table)
                  (nth 8 (save-excursion (syntax-ppss open-pos))))
        ;; Mark opening " with string syntax
        (put-text-property quote-pos (1+ quote-pos)
                           'syntax-table (string-to-syntax "\""))
        ;; Scan forward through DQ string content
        (ysh--scan-dq-content (point-max)))))

  ;; --- 7. Comment markers ---
  ;; # starts a comment only at BOL or after whitespace/metacharacters.
  ;; The syntax table defaults # to punctuation; we promote it here.
  (goto-char start)
  (while (re-search-forward "\\(?:^\\|[ \t;|&]\\)\\(#\\)" end t)
    (unless (nth 8 (save-excursion (syntax-ppss (match-beginning 1))))
      (put-text-property (match-beginning 1) (match-end 1)
                         'syntax-table (string-to-syntax "<")))))

;; ---------------------------------------------------------------------
;; Indentation (simple heuristic)
;; ---------------------------------------------------------------------

(defcustom ysh-indent-offset 2
  "Number of spaces for each indentation level in `ysh-mode`."
  :type 'integer
  :group 'ysh)

(defun ysh-indent-line ()
  "Indent the current line in `ysh-mode`."
  (interactive)
  (let ((indent (ysh--calculate-indent)))
    (when indent
      (save-excursion
        (beginning-of-line)
        (delete-horizontal-space)
        (indent-to indent))
      (when (< (current-column) indent)
        (back-to-indentation)))))

(defun ysh--calculate-indent ()
  "Calculate indentation for the current YSH line."
  (save-excursion
    (beginning-of-line)
    (cond
     ;; First line
     ((bobp) 0)
     ;; Closing brace/bracket/paren — match opener
     ((looking-at "^\\s-*[})]")
      (ysh--indent-of-matching-open))
     ;; Default: base on previous non-blank line
     (t
      (let ((prev-indent 0)
            (prev-opens nil))
        (save-excursion
          (forward-line -1)
          (while (and (not (bobp)) (looking-at "^\\s-*$"))
            (forward-line -1))
          (setq prev-indent (current-indentation))
          (end-of-line)
          ;; Check if previous line opens a block
          (setq prev-opens
                (save-excursion
                  (beginning-of-line)
                  (looking-at ".*[{(]\\s-*\\(?:#.*\\)?$"))))
        (if prev-opens
            (+ prev-indent ysh-indent-offset)
          prev-indent))))))

(defun ysh--indent-of-matching-open ()
  "Return indentation of the line containing the matching open brace/paren."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (condition-case nil
        (progn
          (forward-char 1)    ; move past the closing delimiter
          (backward-sexp 1)   ; jump to matching opener
          (current-indentation))
      (scan-error 0))))

;; ---------------------------------------------------------------------
;; String font-lock (multi-line aware)
;; ---------------------------------------------------------------------

(defconst ysh-font-lock-strings
  `(
    ;; ----- Triple-quoted strings (must come before single-line) -----
    ;; r''' ... '''
    ("\\<r\\('''\\(?:.\\|\n\\)*?'''\\)" 1 font-lock-string-face t)
    ;; b''' u''' ... '''
    ("\\<[bu]\\('''\\(?:.\\|\n\\)*?'''\\)" 1 font-lock-string-face t)
    ;; plain ''' ... '''
    ("[^a-zA-Z0-9_']\\('''\\(?:.\\|\n\\)*?'''\\)" 1 font-lock-string-face t)
    ;; $""" ... """
    ("\\$\\(\"\"\"\\(?:.\\|\n\\)*?\"\"\"\\)" 1 font-lock-string-face t)
    ;; plain """ ... """
    ("[^a-zA-Z0-9_\"]\\(\"\"\"\\(?:.\\|\n\\)*?\"\"\"\\)" 1 font-lock-string-face t)

    ;; ----- J8 / prefix single-line strings -----
    ;; b'...' u'...'
    ("\\<[bu]\\('[^'\n]*'\\)" 1 font-lock-string-face t)
    ;; r'...'
    ("\\<r\\('[^'\n]*'\\)" 1 font-lock-string-face t)
    ;; $"..."
    ("\\$\\(\"\\(?:[^\"\\]\\|\\\\.\\)*\"\\)" 1 font-lock-string-face t)

    ;; ----- J8 escape sequences inside b'' u'' strings -----
    ;; Valid JSON escapes: \\ \" \/ \b \f \n \r \t
    ("\\<[bu]'[^']*\\(\\\\[\\\\\"'/bfnrt]\\)[^']*'" 1 'ysh-j8-escape-face t)
    ;; \' in J8 strings
    ("\\<[bu]'[^']*\\(\\\\[']\\)[^']*'" 1 'ysh-j8-escape-face t)
    ;; \yHH hex bytes
    ("\\<[bu]'[^']*\\(\\\\y[0-9a-fA-F]\\{2\\}\\)[^']*'" 1 'ysh-j8-escape-face t)
    ;; \u{HHHHHH} or \U{HHHHHH}
    ("\\<[bu]'[^']*\\(\\\\[uU]{[0-9a-fA-F]\\{1,6\\}}\\)[^']*'" 1 'ysh-j8-escape-face t)
    )
  "Font-lock rules for YSH string literals.")

;; ---------------------------------------------------------------------
;; Mode definition
;; ---------------------------------------------------------------------

(defconst ysh-font-lock-all
  (append ysh-font-lock-keywords ysh-font-lock-strings)
  "Combined font-lock keywords for `ysh-mode`.")

;;;###autoload
(define-derived-mode ysh-mode prog-mode "YSH"
  "Major mode for editing YSH (Oils shell) files.

Provides syntax highlighting based on the oils.vim Vim plugin,
covering keywords, builtins, all 5 string types (+ triple-quoted),
variable substitutions, sigil pairs, expression atoms, proc/func
definitions, J8 string escapes, and backslash escaping.

\\{ysh-mode-map}"
  :group 'ysh
  :syntax-table ysh-mode-syntax-table

  ;; Comments
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "#+ *")

  ;; Syntax propertize for multi-line / prefixed strings
  (setq-local syntax-propertize-function #'ysh--syntax-propertize)
  (add-hook 'syntax-propertize-extend-region-functions
            #'ysh--syntax-propertize-extend-region nil t)

  ;; Font-lock
  (setq-local font-lock-defaults
              '(ysh-font-lock-all
                nil   ; keywords-only — nil means also use syntax table
                nil   ; case-fold
                nil   ; syntax-alist
                ))
  ;; Support multi-line constructs
  (setq-local font-lock-multiline t)

  ;; Indentation
  (setq-local indent-line-function #'ysh-indent-line)
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width ysh-indent-offset)

  ;; Misc
  (setq-local parse-sexp-ignore-comments t)
  (setq-local beginning-of-defun-function #'ysh-beginning-of-defun)
  (setq-local end-of-defun-function #'ysh-end-of-defun))

;; ---------------------------------------------------------------------
;; Navigation
;; ---------------------------------------------------------------------

(defun ysh-beginning-of-defun (&optional arg)
  "Move to the beginning of the current proc/func definition.
With ARG, move back ARG definitions."
  (interactive "^p")
  (setq arg (or arg 1))
  (re-search-backward
   (concat "^\\s-*\\(?:proc\\|func\\)\\s-+" ysh--proc-name-re)
   nil t arg))

(defun ysh-end-of-defun (&optional arg)
  "Move to the end of the current proc/func definition.
With ARG, move forward ARG definitions."
  (interactive "^p")
  (setq arg (or arg 1))
  (when (looking-at (concat "^\\s-*\\(?:proc\\|func\\)\\s-+" ysh--proc-name-re))
    (forward-line 1))
  (re-search-forward "^}" nil t arg))

;; ---------------------------------------------------------------------
;; Auto-mode and interpreter support
;; ---------------------------------------------------------------------

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ysh\\'" . ysh-mode))

;;;###autoload
(add-to-list 'interpreter-mode-alist '("ysh" . ysh-mode))

(provide 'ysh-mode)
;;; ysh-mode.el ends here
