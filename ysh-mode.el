;;; ysh-mode.el --- Major mode for YSH (Oils shell) -*- lexical-binding: t; -*-

;; Author: Claude Code
;; Version: 0.1.0
;; Keywords: languages, shell
;; URL: https://www.oilshell.org/
;; Package-Requires: ((emacs "28.1"))

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
;;
;; It also provides an xref backend, so `M-.' (`xref-find-definitions')
;; jumps to proc, func, var/const, parameter and loop-variable
;; definitions, following `source' and `use' into other files.  No
;; language server is needed.  If you do run one through Eglot, Eglot's
;; own xref backend takes precedence while it manages the buffer.

(require 'rx)
(require 'cl-lib)
(require 'xref)

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

(defconst ysh--backslash-re "\\\\[]#'\"$@(){}\\\\[]"
  "Regexp for a backslash-quoted character.
Note: `]' must come first in the character class \(Emacs 31 mishandles
\\] inside a class\).")

(defun ysh--match-backslash (limit)
  "Font-lock matcher for backslash-quoted characters up to LIMIT.
Skips matches inside single-quoted strings, where a backslash is a
literal character \(r\\='C:\\\\=' and \\='a\\\\b\\=' contain no escapes\), and
inside comments.  J8 strings keep their escape highlighting: the
`ysh-j8-escape-face' rules run later and override."
  (let ((found nil))
    (while (and (not found)
                (re-search-forward ysh--backslash-re limit t))
      (let* ((beg (match-beginning 0))
             (ppss (save-excursion (syntax-ppss beg))))
        (unless (or (nth 4 ppss)
                    (eql (nth 3 ppss) ?\'))
          (setq found t))))
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
      ;; Matcher function (not a bare regexp) so that backslashes inside
      ;; single-quoted strings stay string-faced.
      (ysh--match-backslash 0 'ysh-backslash-face t)

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

(defun ysh--escaped-p (pos)
  "Return non-nil if the character at POS is backslash-escaped.
An odd number of backslashes immediately before POS escapes it, so in
command mode \\=\\=' is a quoted quote, while \\=\\\\=' is a literal backslash
followed by a string opener."
  (let ((n 0)
        (p pos))
    (while (and (> p (point-min))
                (eql (char-before p) ?\\))
      (setq n (1+ n))
      (setq p (1- p)))
    (= 1 (mod n 2))))

(defun ysh--sq-closer-escaped (limit)
  "Return the position of the closing \\=' of a J8 string, or nil.
Scanning starts at point, which must be just after the opening \\='.
Backslash escapes (\\x) are skipped, so b\\='\\\\=''\\=' does not close early.
A newline or LIMIT ends the search unterminated.  Point is not moved."
  (save-excursion
    (catch 'ysh--closer
      (while (< (point) limit)
        (let ((ch (char-after)))
          (cond
           ((eql ch ?\n) (throw 'ysh--closer nil))
           ((and (eql ch ?\\) (< (1+ (point)) limit)) (forward-char 2))
           ((eql ch ?\') (throw 'ysh--closer (point)))
           (t (forward-char 1)))))
      nil)))

(defun ysh--sq-closer-raw (limit)
  "Return the position of the closing \\=' of a raw/plain string, or nil.
Scanning starts at point, which must be just after the opening \\='.
Backslashes are literal \(r\\='C:\\\\=' is a complete string\), so the very
next \\=' on the line closes it.  Point is not moved."
  (save-excursion
    (let ((stop (min limit (line-end-position))))
      (when (search-forward "'" stop t)
        (1- (point))))))

(defun ysh--propertize-single-quotes (start end)
  "Propertize every single-quoted YSH string form between START and END.
Handles, in one left-to-right scan:
  [rbu]?\\='''...\\='''   triple-quoted (may span lines)
  [bu]\\='...\\='         J8, backslash escapes active
  r\\='...\\='            raw, backslashes literal
  \\='...\\='             plain, backslashes literal

The single scan is the point: after a string is propertized, point is
left past its closing \\=', so a closing quote can never be re-examined as
an opener.  Separate per-form passes got this wrong \(\[\\='B\\=', \\='KiB\\='] read the
B as a J8 prefix on the quote that closed \\='B\\='\), which desynced quote
parity for the rest of the buffer.

Unterminated openers are left as punctuation rather than opening a
string that would swallow the rest of the buffer."
  (goto-char start)
  (while (and (< (point) end)
              (re-search-forward "\\(?:\\<\\([rbu]\\)\\)?\\('\\)" end t))
    (let* ((prefix (and (match-beginning 1) (char-after (match-beginning 1))))
           (qpos (match-beginning 2)))
      (if (or (get-text-property qpos 'syntax-table)
              (nth 8 (save-excursion (syntax-ppss qpos)))
              ;; echo \'single \'single — a quoted quote opens nothing.
              (ysh--escaped-p qpos))
          ;; Already claimed, inside an open string, or backslash-quoted.
          (goto-char (1+ qpos))
        (if (and (eql (char-after (+ qpos 1)) ?\')
                 (eql (char-after (+ qpos 2)) ?\'))
            ;; --- Triple-quoted: fences at the outer quotes, closer may
            ;; lie beyond END (JIT-lock sub-region), so search to point-max.
            (progn
              (put-text-property qpos (1+ qpos)
                                 'syntax-table (string-to-syntax "|"))
              (put-text-property (1+ qpos) (+ qpos 2)
                                 'syntax-table (string-to-syntax "."))
              (put-text-property (+ qpos 2) (+ qpos 3)
                                 'syntax-table (string-to-syntax "."))
              (goto-char (+ qpos 3))
              (if (re-search-forward "'''" (point-max) t)
                  (let ((close-end (point)))
                    (put-text-property (- close-end 3) (- close-end 2)
                                       'syntax-table (string-to-syntax "."))
                    (put-text-property (- close-end 2) (- close-end 1)
                                       'syntax-table (string-to-syntax "."))
                    (put-text-property (- close-end 1) close-end
                                       'syntax-table (string-to-syntax "|")))
                (goto-char (point-max))))
          ;; --- Single-quoted: b'/u' honour escapes, r'/plain do not.
          (goto-char (1+ qpos))
          (let* ((j8 (memq prefix '(?b ?u)))
                 (closer (if j8
                             (ysh--sq-closer-escaped (point-max))
                           (ysh--sq-closer-raw (point-max)))))
            (when closer
              (put-text-property qpos (1+ qpos)
                                 'syntax-table (string-to-syntax "\""))
              (put-text-property closer (1+ closer)
                                 'syntax-table (string-to-syntax "\""))
              ;; Raw and plain strings: neutralize \ so it cannot escape.
              (unless j8
                (save-excursion
                  (while (search-forward "\\" closer t)
                    (put-text-property (1- (point)) (point)
                                       'syntax-table (string-to-syntax ".")))))
              (goto-char (1+ closer)))))))))

(defun ysh--syntax-propertize (start end)
  "Apply syntax properties for YSH string forms between START and END.
Handles (in order):
 1. All single-quoted forms, in one scan (see
    `ysh--propertize-single-quotes'): [rbu]?\\='''...\\=''',
    [bu]\\='...\\=', r\\='...\\=', \\='...\\='
 2. Triple-quoted double strings: $?\\=\"\\=\"\\=\"...\\=\"\\=\"\\=\"
 3. Double-quoted strings: \\=\"...\\=\" and $\\=\"...\\=\"
 4. Comment markers: # preceded by whitespace/metacharacters/BOL

Searching is case-sensitive: YSH string prefixes are lowercase only, so
\\=['B'] must not be read as a J8 string.

Triple-quoted closers are searched up to `point-max' so that
JIT-lock sub-region boundaries do not prevent finding them."
  (let ((case-fold-search nil))

  ;; --- 1. All single-quote forms, left to right ---
  (ysh--propertize-single-quotes start end)

  ;; --- 2. Triple-double-quoted: $?""" ... """ ---
  ;; The (< (point) end) guard matters: the closer search below runs to
  ;; point-max, so point can end up past END, and `re-search-forward'
  ;; signals "Invalid search bound" when its bound is behind point.
  (goto-char start)
  (while (and (< (point) end)
              (re-search-forward "\\(?:\\$\\)?\\(\"\"\"\\)" end t))
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

  ;; --- 3. Double-quoted strings: "..." and $"..." ---
  ;; Handles nested double quotes inside $[...] expression subs.
  ;; The syntax table marks " as punctuation; we handle all DQ strings here.
  ;; This is the core of Stage 2: recursive sublanguages.
  (goto-char start)
  (while (and (< (point) end)
              (re-search-forward "\\$?\"" end t))
    (let ((open-pos (match-beginning 0))
          ;; For $"...", the " is one char after the $
          (quote-pos (1- (point))))
      (unless (or (get-text-property quote-pos 'syntax-table)
                  (nth 8 (save-excursion (syntax-ppss open-pos)))
                  ;; echo \"double — a quoted quote opens nothing.
                  (ysh--escaped-p quote-pos))
        ;; Mark opening " with string syntax
        (put-text-property quote-pos (1+ quote-pos)
                           'syntax-table (string-to-syntax "\""))
        ;; Scan forward through DQ string content
        (ysh--scan-dq-content (point-max)))))

  ;; --- 4. Comment markers ---
  ;; # starts a comment only at BOL or after whitespace/metacharacters.
  ;; The syntax table defaults # to punctuation; we promote it here.
  (goto-char start)
  (while (re-search-forward "\\(?:^\\|[ \t;|&]\\)\\(#\\)" end t)
    (unless (nth 8 (save-excursion (syntax-ppss (match-beginning 1))))
      (put-text-property (match-beginning 1) (match-end 1)
                         'syntax-table (string-to-syntax "<"))))))

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

    ;; ----- Prefixed single-line strings -----
    ;; b'...' u'...' r'...' need no keyword rule: `ysh--syntax-propertize'
    ;; marks their quotes with string syntax, so syntactic fontification
    ;; already paints the body.  A keyword rule here would re-match
    ;; ['b', 'c'] as b + "', '" and override the correct faces.
    ;; $"..."
    ("\\$\\(\"\\(?:[^\"\\]\\|\\\\.\\)*\"\\)" 1 font-lock-string-face t)

    ;; ----- J8 escape sequences inside b'' u'' strings -----
    ;; The `[^'[:alnum:]_]' prefix guard keeps ['b', 'a\nb'] from reading
    ;; the one-character string 'b' as a J8 prefix on the next quote.
    ;; Valid JSON escapes: \\ \" \/ \b \f \n \r \t
    ("\\(?:^\\|[^'[:alnum:]_]\\)[bu]'[^']*\\(\\\\[\\\\\"'/bfnrt]\\)[^']*'"
     1 'ysh-j8-escape-face t)
    ;; \' in J8 strings
    ("\\(?:^\\|[^'[:alnum:]_]\\)[bu]'[^']*\\(\\\\[']\\)[^']*'"
     1 'ysh-j8-escape-face t)
    ;; \yHH hex bytes
    ("\\(?:^\\|[^'[:alnum:]_]\\)[bu]'[^']*\\(\\\\y[0-9a-fA-F]\\{2\\}\\)[^']*'"
     1 'ysh-j8-escape-face t)
    ;; \u{HHHHHH} or \U{HHHHHH}
    ("\\(?:^\\|[^'[:alnum:]_]\\)[bu]'[^']*\\(\\\\[uU]{[0-9a-fA-F]\\{1,6\\}}\\)[^']*'"
     1 'ysh-j8-escape-face t)
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
  (setq-local end-of-defun-function #'ysh-end-of-defun)

  ;; Jump to definition (M-.)
  (add-hook 'xref-backend-functions #'ysh-xref-backend nil t))

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
;; Xref: jump to definition
;; ---------------------------------------------------------------------
;;
;; A coarse, syntax-aware scan for definitions — no parser, no server.
;; Recognised definition forms:
;;
;;   proc NAME (params) { ... }       func NAME(params) { ... }
;;   NAME() { ... }                   (shell function)
;;   var A, B = ...                   const A = ...
;;   for A, B in ...                  proc/func parameters
;;   use path/to/MODULE.ysh           (defines MODULE, jumps to the file)
;;
;; Lookup order for an identifier at point:
;;   1. locals of the enclosing proc/func (nearest preceding definition)
;;   2. top-level definitions in the buffer
;;   3. top-level definitions in files reached through `source' / `use'
;;      (transitively)
;;   4. definitions anywhere else in the buffer
;;   5. top-level definitions in other .ysh files of the project
;; The first step that finds anything wins.  Matches inside strings and
;; comments are ignored.

(defcustom ysh-xref-search-project t
  "When non-nil, fall back to all .ysh files in the project.
This is consulted only when no definition is found in the buffer or
in files it reaches through `source' and `use'."
  :type 'boolean
  :group 'ysh)

(defcustom ysh-xref-max-imports 100
  "Maximum number of `source'/`use' files followed for one lookup."
  :type 'integer
  :group 'ysh)

(cl-defstruct (ysh-xref--def (:constructor ysh-xref--make-def)
                             (:copier nil))
  "A definition found by the xref scanner."
  name kind pos line col scope summary target)

(cl-defstruct (ysh-xref--info (:constructor ysh-xref--make-info)
                              (:copier nil))
  "Definitions and imports of one file or buffer."
  file buffer defs scopes imports sources picks modules)

(defvar ysh-xref--buffer-cache (make-hash-table :test 'eq :weakness 'key)
  "Scan results for live buffers: BUFFER -> (TICK . INFO).")

(defvar ysh-xref--file-cache (make-hash-table :test 'equal)
  "Scan results for unvisited files: TRUENAME -> ((MTIME SIZE) . INFO).")

(defconst ysh-xref--defun-re
  (concat "^[ \t]*\\(proc\\|func\\)[ \t]+\\(" ysh--proc-name-re "\\)")
  "Regexp matching a proc/func header; group 1 kind, group 2 name.")

(defconst ysh-xref--shfunc-re
  (concat "^[ \t]*\\(" ysh--proc-name-re "\\)[ \t]*([ \t]*)[ \t]*{")
  "Regexp matching a shell-style function header NAME() {.")

(defconst ysh-xref--stmt-prefix "\\(?:^\\|[;{|&]\\)[ \t]*"
  "Anchors a statement keyword to command position.")

(defun ysh-xref--skip-p (pos)
  "Return non-nil if POS is inside a string or comment."
  (nth 8 (save-excursion (syntax-ppss pos))))

(defun ysh-xref--line-summary (pos)
  "Return the trimmed text of the line containing POS.
Preserves match data (`string-trim' would clobber it)."
  (save-excursion
    (save-match-data
      (goto-char pos)
      (string-trim (buffer-substring-no-properties
                    (line-beginning-position) (line-end-position))))))

(defun ysh-xref--new-def (name kind pos scope)
  "Build a definition NAME of KIND at POS with SCOPE."
  (save-excursion
    (goto-char pos)
    (ysh-xref--make-def :name name :kind kind :pos pos
                        :line (line-number-at-pos pos t)
                        :col (current-column)
                        :scope scope
                        :summary (ysh-xref--line-summary pos))))

(defun ysh-xref--param-starts (beg end)
  "Return positions where parameters start in the list BEG..END.
BEG is the opening paren, END is just after the closing one.  Splits
on `,' and `;' at bracket depth zero, outside strings."
  (let ((starts (list (1+ beg)))
        (depth 0))
    (save-excursion
      (goto-char (1+ beg))
      (while (< (point) (1- end))
        (let ((ch (char-after)))
          (unless (ysh-xref--skip-p (point))
            (cond
             ((memq ch '(?\( ?\[ ?{)) (setq depth (1+ depth)))
             ((memq ch '(?\) ?\] ?})) (setq depth (1- depth)))
             ((and (= depth 0) (memq ch '(?, ?\;)))
              (push (1+ (point)) starts)))))
        (forward-char 1)))
    (nreverse starts)))

(defun ysh-xref--scan-params (beg end scope)
  "Return parameter definitions in the list BEG..END with SCOPE."
  (let (defs)
    (save-excursion
      (dolist (start (ysh-xref--param-starts beg end))
        (goto-char start)
        (forward-comment (buffer-size))
        (skip-chars-forward ".@")
        (when (and (< (point) end) (looking-at ysh--var-name-re))
          (push (ysh-xref--new-def (match-string-no-properties 0)
                                   "param" (point) scope)
                defs))))
    (nreverse defs)))

(defun ysh-xref--scan-defuns ()
  "Scan the buffer for proc/func/shell-function definitions.
Return a list of (KIND NAME NAME-POS PARAMS-BEG PARAMS-END SCOPE-END)."
  (let (out)
    (goto-char (point-min))
    (while (re-search-forward ysh-xref--defun-re nil t)
      (let ((kind (match-string-no-properties 1))
            (name (match-string-no-properties 2))
            (npos (match-beginning 2))
            pbeg pend bend)
        (unless (ysh-xref--skip-p (match-beginning 1))
          (save-excursion
            (skip-chars-forward " \t")
            (when (eq (char-after) ?\()
              (setq pbeg (point))
              (condition-case nil
                  (progn (forward-sexp 1) (setq pend (point)))
                (scan-error (setq pbeg nil))))
            (skip-chars-forward " \t\n")
            (when (eq (char-after) ?{)
              (let ((open (point)))
                (setq bend (condition-case nil
                               (progn (forward-sexp 1) (point))
                             (scan-error (point-max))))
                (unless pbeg (setq pbeg open)))))
          (when bend
            (push (list kind name npos pbeg pend bend) out)))))
    (goto-char (point-min))
    (while (re-search-forward ysh-xref--shfunc-re nil t)
      (let ((name (match-string-no-properties 1))
            (npos (match-beginning 1))
            (open (1- (match-end 0))))
        (unless (or (ysh-xref--skip-p npos)
                    (member name '("proc" "func")))
          (save-excursion
            (goto-char open)
            (push (list "function" name npos open nil
                        (condition-case nil
                            (progn (forward-sexp 1) (point))
                          (scan-error (point-max))))
                  out)))))
    (nreverse out)))

(defun ysh-xref--innermost-scope (pos scopes)
  "Return the innermost (BEG . END) in SCOPES containing POS, or nil."
  (let (best)
    (dolist (s scopes)
      (when (and (<= (car s) pos) (< pos (cdr s))
                 (or (null best) (> (car s) (car best))))
        (setq best s)))
    best))

(defun ysh-xref--scan-names (scopes kind)
  "Parse comma-separated names at point as definitions of KIND.
SCOPES is used to attach each name to its enclosing proc/func.
Return the definitions; point is left after the last name."
  (let (defs (more t))
    (while more
      (skip-chars-forward " \t")
      (if (not (looking-at ysh--var-name-re))
          (setq more nil)
        (let ((pos (point))
              (name (match-string-no-properties 0))
              (name-end (match-end 0)))
          (push (ysh-xref--new-def name kind pos
                                   (ysh-xref--innermost-scope pos scopes))
                defs)
          (goto-char name-end)
          (skip-chars-forward " \t")
          (if (eq (char-after) ?,)
              (forward-char 1)
            (setq more nil)))))
    (nreverse defs)))

(defun ysh-xref--parse-picks (rest)
  "Return the names after `--pick' in REST, the tail of a `use' line."
  (let ((words (split-string (replace-regexp-in-string "[ \t]#.*" "" rest)))
        picking names)
    (dolist (w words)
      (cond
       ((equal w "--pick") (setq picking t))
       ((string-prefix-p "-" w) (setq picking nil))
       (picking (push w names))))
    (nreverse names)))

(defun ysh-xref--scan-imports ()
  "Return (KIND RAW-PATH POS LINE SUMMARY PICKS) per `source'/`use'.
PICKS lists the names given to `use ... --pick'."
  (let (out)
    (goto-char (point-min))
    (while (re-search-forward
            (concat ysh-xref--stmt-prefix
                    "\\(source\\|use\\)[ \t]+"
                    ;; path: $(...), "...", or plain characters
                    "\\(\\(?:\\$([^)\n]*)\\|\"[^\"\n]*\"\\|[^ \t\n;|&\"]\\)+\\)"
                    "\\([^\n;|&]*\\)")
            nil t)
      (unless (or (ysh-xref--skip-p (match-beginning 1))
                  ;; `use --extern grep' names no file
                  (eq (char-after (match-beginning 2)) ?-))
        (let ((kind (match-string-no-properties 1))
              (raw (match-string-no-properties 2))
              (pos (match-beginning 2))
              (rest (match-string-no-properties 3)))
          (push (list kind raw pos
                      (line-number-at-pos pos t)
                      (ysh-xref--line-summary pos)
                      (and (equal kind "use") (ysh-xref--parse-picks rest)))
                out))))
    (nreverse out)))

(defun ysh-xref--scan ()
  "Scan the current buffer for definitions and imports.
The buffer must use `ysh-mode-syntax-table' and `ysh--syntax-propertize'.
Return (DEFS SCOPES IMPORTS); see `ysh-xref--scan-imports' for IMPORTS."
  (save-excursion
    (save-restriction
      (widen)
      (save-match-data
        (let ((case-fold-search nil)
              defs scopes)
          (syntax-propertize (point-max))
          ;; procs, funcs, and their parameters
          (let ((defuns (ysh-xref--scan-defuns)))
            (setq scopes (mapcar (lambda (d) (cons (nth 3 d) (nth 5 d)))
                                 defuns))
            (dolist (d defuns)
              (pcase-let ((`(,kind ,name ,npos ,pbeg ,pend ,bend) d))
                (push (ysh-xref--new-def
                       name kind npos
                       (ysh-xref--innermost-scope npos scopes))
                      defs)
                (when pend
                  (setq defs (append (reverse (ysh-xref--scan-params
                                               pbeg pend (cons pbeg bend)))
                                     defs))))))
          ;; var / const
          (goto-char (point-min))
          (while (re-search-forward
                  (concat ysh-xref--stmt-prefix "\\(var\\|const\\)[ \t]+")
                  nil t)
            (unless (ysh-xref--skip-p (match-beginning 1))
              (let ((kind (match-string-no-properties 1)))
                (setq defs (append (reverse (ysh-xref--scan-names scopes kind))
                                   defs)))))
          ;; for loop variables: for A, B in ...
          (goto-char (point-min))
          (while (re-search-forward
                  (concat ysh-xref--stmt-prefix "\\(for\\)[ \t]+(?")
                  nil t)
            (unless (ysh-xref--skip-p (match-beginning 1))
              (let ((names (ysh-xref--scan-names scopes "for")))
                (when (looking-at "[ \t]*in\\>")
                  (setq defs (append (reverse names) defs))))))
          (list (sort defs (lambda (a b) (< (ysh-xref--def-pos a)
                                            (ysh-xref--def-pos b))))
                scopes
                (ysh-xref--scan-imports)))))))

(defun ysh-xref--setup-scan-buffer ()
  "Give the current (temporary) buffer YSH syntax for scanning."
  (set-syntax-table ysh-mode-syntax-table)
  (setq-local syntax-propertize-function #'ysh--syntax-propertize)
  (add-hook 'syntax-propertize-extend-region-functions
            #'ysh--syntax-propertize-extend-region nil t)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local parse-sexp-lookup-properties t))

(defun ysh-xref--resolve-path (raw dir)
  "Resolve the `source'/`use' argument RAW relative to DIR.
Understands `$_this_dir', `${_this_dir}', `$[_this_dir]' and
`$(dirname $0)' (all meaning the directory of the script).  Returns
an existing file's truename, or nil (embedded `///' scripts, other
unexpanded variables, missing files)."
  (let ((s (string-trim raw "[\"']" "[\"']")))
    (unless (string-prefix-p "///" s)
      (setq s (replace-regexp-in-string
               "\"?\\$(dirname[ \t]+\"?\\$0\"?)\"?"
               (lambda (_) (directory-file-name dir)) s t t))
      (setq s (replace-regexp-in-string
               "\\$\\(?:{_this_dir}\\|\\[_this_dir\\]\\|_this_dir\\)\\([^a-zA-Z0-9_]\\|\\'\\)"
               (lambda (m)
                 (concat (directory-file-name dir)
                         (substring m (- (length m)
                                         (length (match-string 1 m))))))
               s t t))
      (unless (string-match-p "[$`]" s)
        (let ((roots (delete-dups
                      (delq nil (list dir
                                      (ysh-xref--project-root dir)
                                      default-directory)))))
          (cl-loop for root in roots
                   for f = (expand-file-name s root)
                   when (file-regular-p f) return (file-truename f)))))))

(defun ysh-xref--project-root (dir)
  "Return the project root containing DIR, or nil."
  (when (require 'project nil t)
    (let* ((default-directory dir)
           (proj (project-current nil)))
      (when proj
        (if (fboundp 'project-root)
            (project-root proj)
          (car (with-no-warnings (project-roots proj))))))))

(defun ysh-xref--build-info (scan dir file buffer)
  "Turn the result of `ysh-xref--scan' into an info record.
SCAN was produced in a buffer whose file lives in DIR.  FILE is the
file's truename (or nil) and BUFFER the live buffer (or nil)."
  (pcase-let ((`(,defs ,scopes ,raw-imports) scan))
    (let (imports sources picks modules)
      (dolist (imp raw-imports)
        (pcase-let ((`(,kind ,raw ,pos ,line ,summary ,names) imp))
          (let ((path (ysh-xref--resolve-path raw dir)))
            (when path
              (push path imports)
              (when (equal kind "source") (push path sources))
              (when names (push (cons path names) picks)))
            (when (equal kind "use")
              (let* ((clean (string-trim raw "[\"']" "[\"']"))
                     (mod (file-name-base (directory-file-name clean))))
                (when (string-match-p (concat "\\`" ysh--proc-name-re "\\'")
                                      mod)
                  (push (cons mod path) modules)
                  ;; Jumps to the module file when it resolves, else to
                  ;; the `use' line itself.
                  (push (ysh-xref--make-def
                         :name mod :kind "module" :pos pos :line line :col 0
                         :scope nil :summary summary :target path)
                        defs)))))))
      (ysh-xref--make-info :file file :buffer buffer
                           :defs defs :scopes scopes
                           :imports (nreverse imports)
                           :sources (nreverse sources)
                           :picks (nreverse picks)
                           :modules (nreverse modules)))))

(defun ysh-xref--info-for-buffer (buf)
  "Return the (cached) scan info for the live buffer BUF."
  (with-current-buffer buf
    (let* ((stamp (buffer-chars-modified-tick))
           (hit (gethash buf ysh-xref--buffer-cache)))
      (if (and hit (equal (car hit) stamp))
          (cdr hit)
        (let* ((dir default-directory)
               (file (and buffer-file-name (file-truename buffer-file-name)))
               ;; ysh-mode buffers are scanned in place; others (e.g.
               ;; `ysh-ts-mode') via a copy that has YSH syntax.
               ;; Positions are identical either way.
               (scan (if (derived-mode-p 'ysh-mode)
                         (ysh-xref--scan)
                       (let ((text (save-restriction
                                     (widen)
                                     (buffer-substring-no-properties
                                      (point-min) (point-max)))))
                         (with-temp-buffer
                           (insert text)
                           (ysh-xref--setup-scan-buffer)
                           (ysh-xref--scan)))))
               (info (ysh-xref--build-info scan dir file buf)))
          (puthash buf (cons stamp info) ysh-xref--buffer-cache)
          info)))))

(defun ysh-xref--info-for-file (file)
  "Return the (cached) scan info for FILE, a truename.
Uses the visiting buffer if there is one, so unsaved edits count."
  (let ((buf (find-buffer-visiting file)))
    (cond
     (buf (ysh-xref--info-for-buffer buf))
     ((file-readable-p file)
      (let* ((attrs (file-attributes file))
             (stamp (list (file-attribute-modification-time attrs)
                          (file-attribute-size attrs)))
             (hit (gethash file ysh-xref--file-cache)))
        (if (and hit (equal (car hit) stamp))
            (cdr hit)
          (let* ((dir (file-name-directory file))
                 (scan (with-temp-buffer
                         (insert-file-contents file)
                         (ysh-xref--setup-scan-buffer)
                         (ysh-xref--scan)))
                 (info (ysh-xref--build-info scan dir file nil)))
            (puthash file (cons stamp info) ysh-xref--file-cache)
            info)))))))

(defun ysh-xref--import-infos (info &optional sources-only)
  "Return infos for files reached from INFO via `source'/`use'.
With SOURCES-ONLY, follow only `source' \(whose definitions land in
the caller's scope).  Breadth-first, transitive, each file once,
capped by `ysh-xref-max-imports'."
  (let* ((next (if sources-only
                   #'ysh-xref--info-sources
                 #'ysh-xref--info-imports))
         (seen (and (ysh-xref--info-file info)
                    (list (ysh-xref--info-file info))))
         (queue (copy-sequence (funcall next info)))
         out)
    (while (and queue (< (length out) ysh-xref-max-imports))
      (let ((f (pop queue)))
        (unless (member f seen)
          (push f seen)
          (let ((i (ysh-xref--info-for-file f)))
            (when i
              (push i out)
              (setq queue (append queue (funcall next i))))))))
    (nreverse out)))

(defun ysh-xref--project-ysh-files (dir)
  "Return .ysh files in the project containing DIR (or in DIR itself)."
  (let* ((default-directory dir)
         (proj (and (require 'project nil t) (project-current nil))))
    (mapcar #'file-truename
            (if proj
                (cl-remove-if-not (lambda (f) (string-suffix-p ".ysh" f))
                                  (project-files proj))
              (directory-files dir t "\\.ysh\\'")))))

(defun ysh-xref--make-item (def info)
  "Return an xref item for DEF, found in INFO."
  (let ((target (ysh-xref--def-target def))
        (buf (ysh-xref--info-buffer info)))
    (xref-make
     (or (ysh-xref--def-summary def)
         (format "%s %s" (ysh-xref--def-kind def) (ysh-xref--def-name def)))
     (cond
      (target (xref-make-file-location target 1 0))
      ((buffer-live-p buf)
       (xref-make-buffer-location buf (ysh-xref--def-pos def)))
      (t (xref-make-file-location (ysh-xref--info-file info)
                                  (ysh-xref--def-line def)
                                  (ysh-xref--def-col def)))))))

(defun ysh-xref--named (name info &optional pred)
  "Return definitions of NAME in INFO satisfying PRED (if given)."
  (cl-remove-if-not (lambda (d)
                      (and (equal (ysh-xref--def-name d) name)
                           (or (null pred) (funcall pred d))))
                    (ysh-xref--info-defs info)))

(defun ysh-xref--top-level-p (def)
  "Return non-nil if DEF is not inside a proc/func."
  (null (ysh-xref--def-scope def)))

(defun ysh-xref--lookup (name pos module)
  "Return xref items for definitions of NAME in the current buffer.
POS, if non-nil, is where NAME was referenced (enables scoping).
MODULE, if non-nil, is the module name qualifying NAME."
  (let* ((info (ysh-xref--info-for-buffer (current-buffer)))
         (here (and pos (ysh-xref--innermost-scope
                         pos (ysh-xref--info-scopes info))))
         (imports 'unset)
         (sourced 'unset)
         (items-of (lambda (defs inf)
                     (mapcar (lambda (d) (ysh-xref--make-item d inf)) defs)))
         (top-items (lambda (inf)
                      (funcall items-of
                               (ysh-xref--named name inf
                                                #'ysh-xref--top-level-p)
                               inf)))
         (get-imports (lambda ()
                        (when (eq imports 'unset)
                          (setq imports (ysh-xref--import-infos info)))
                        imports))
         (get-sourced (lambda ()
                        (when (eq sourced 'unset)
                          (setq sourced (ysh-xref--import-infos info t)))
                        sourced)))
    (or
     ;; 0. Module-qualified: `mod my-proc' or `mod.name'
     (let ((path (and module (cdr (assoc module
                                         (ysh-xref--info-modules info))))))
       (when path
         (let ((minfo (ysh-xref--info-for-file path)))
           (when minfo
             (funcall items-of
                      (ysh-xref--named name minfo #'ysh-xref--top-level-p)
                      minfo)))))
     ;; 1. Locals of the enclosing proc/func: nearest preceding one
     (when here
       (let* ((locals (ysh-xref--named
                       name info
                       (lambda (d) (equal (ysh-xref--def-scope d) here))))
              (before (cl-remove-if (lambda (d) (> (ysh-xref--def-pos d) pos))
                                    locals)))
         (funcall items-of (if before (last before) locals) info)))
     ;; 2. Top-level definitions in this buffer
     (funcall items-of (ysh-xref--named name info #'ysh-xref--top-level-p)
              info)
     ;; 3. In scope through `source' (transitively) or `use ... --pick'
     (append
      (cl-loop for inf in (funcall get-sourced)
               append (funcall top-items inf))
      (cl-loop for inf in (cons info (funcall get-sourced))
               append (cl-loop for (path . names) in (ysh-xref--info-picks inf)
                               when (member name names)
                               append (let ((pinf (ysh-xref--info-for-file
                                                   path)))
                                        (and pinf (funcall top-items pinf))))))
     ;; 3b. Other reachable files: modules pulled in by plain `use',
     ;;     whose names are only reachable qualified (`mod my-proc').
     (cl-loop for inf in (funcall get-imports)
              unless (memq inf (funcall get-sourced))
              append (funcall top-items inf))
     ;; 4. Anything else in this buffer (e.g. a prompted name that is
     ;;    local to some other proc)
     (funcall items-of (ysh-xref--named name info) info)
     ;; 5. Top-level definitions elsewhere in the project
     (when ysh-xref-search-project
       (let ((skip (delq nil (cons (ysh-xref--info-file info)
                                   (mapcar #'ysh-xref--info-file
                                           (funcall get-imports))))))
         (cl-loop for f in (ysh-xref--project-ysh-files default-directory)
                  unless (member f skip)
                  append (let ((inf (ysh-xref--info-for-file f)))
                           (and inf
                                (funcall items-of
                                         (ysh-xref--named
                                          name inf #'ysh-xref--top-level-p)
                                         inf)))))))))

(defun ysh-xref--identifier-at-point ()
  "Return the YSH identifier at point, or nil.
The string carries text properties used by `ysh-xref--lookup':
`ysh-xref-pos' (where it was found), `ysh-xref-buffer', `ysh-xref-short'
\(the hyphen-free word at point, for `$x-1' and `echo $name-suffix'),
and `ysh-xref-module' (for `mod my-proc' and `mod.name')."
  (save-excursion
    (cond
     ((looking-at "[$@]{?[a-zA-Z_]") (skip-chars-forward "$@{"))
     ((looking-at "{[a-zA-Z_]") (forward-char 1)))
    (let* ((p (point))
           (beg (progn (skip-chars-backward "a-zA-Z0-9_-") (point)))
           (end (progn (skip-chars-forward "a-zA-Z0-9_-") (point)))
           (sbeg (progn (goto-char p) (skip-chars-backward "a-zA-Z0-9_")
                        (point)))
           (send (progn (skip-chars-forward "a-zA-Z0-9_") (point)))
           (short (and (< sbeg send)
                       (buffer-substring-no-properties sbeg send)))
           (sigil (or (memq (char-before beg) '(?$ ?@))
                      (and (eq (char-before beg) ?{)
                           (eq (char-before (1- beg)) ?$))))
           (name (if sigil
                     short
                   (let ((s (buffer-substring-no-properties beg end)))
                     (setq s (string-trim s "-+" "-+"))
                     (and (not (string-empty-p s)) s))))
           (start (if sigil sbeg beg))
           (module
            (progn
              (goto-char start)
              (cond
               ((looking-back "\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\."
                              (line-beginning-position))
                (match-string-no-properties 1))
               ((and (looking-back (concat ysh-xref--stmt-prefix
                                           "\\(" ysh--proc-name-re "\\)[ \t]+")
                                   (line-beginning-position))
                     ;; `proc NAME', `var NAME' ... are not `mod NAME'
                     (not (member (match-string-no-properties 1)
                                  '("proc" "func" "var" "const" "setvar"
                                    "setglobal" "call" "for" "if" "elif"
                                    "while" "case" "return" "use"
                                    "source"))))
                (match-string-no-properties 1))))))
      (when (and name (string-match-p "\\`[a-zA-Z_]" name))
        (propertize name
                    'ysh-xref-pos start
                    'ysh-xref-buffer (current-buffer)
                    'ysh-xref-short short
                    'ysh-xref-module module)))))

;;;###autoload
(defun ysh-xref-backend ()
  "Xref backend for YSH buffers."
  'ysh)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql 'ysh)))
  (ysh-xref--identifier-at-point))

(cl-defmethod xref-backend-definitions ((_backend (eql 'ysh)) identifier)
  (let* ((here (eq (get-text-property 0 'ysh-xref-buffer identifier)
                   (current-buffer)))
         (pos (and here (get-text-property 0 'ysh-xref-pos identifier)))
         (module (and here (get-text-property 0 'ysh-xref-module identifier)))
         (short (and here (get-text-property 0 'ysh-xref-short identifier)))
         (name (substring-no-properties identifier)))
    (or (ysh-xref--lookup name pos module)
        ;; `x-1' in an expression, `$name-suffix' in a word: retry with
        ;; the hyphen-free word at point.
        (and short (not (equal short name))
             (ysh-xref--lookup short pos module)))))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql 'ysh)))
  (let ((info (ysh-xref--info-for-buffer (current-buffer))))
    (delete-dups
     (mapcar #'ysh-xref--def-name
             (apply #'append (ysh-xref--info-defs info)
                    (mapcar #'ysh-xref--info-defs
                            (ysh-xref--import-infos info)))))))

(cl-defmethod xref-backend-apropos ((_backend (eql 'ysh)) pattern)
  (let* ((re (xref-apropos-regexp pattern))
         (info (ysh-xref--info-for-buffer (current-buffer)))
         (infos (cons info (ysh-xref--import-infos info)))
         (match (lambda (d) (string-match-p re (ysh-xref--def-name d)))))
    (when ysh-xref-search-project
      (let ((skip (delq nil (mapcar #'ysh-xref--info-file infos))))
        (dolist (f (ysh-xref--project-ysh-files default-directory))
          (unless (member f skip)
            (let ((i (ysh-xref--info-for-file f)))
              (when i (setq infos (append infos (list i)))))))))
    (cl-loop for inf in infos
             append (mapcar (lambda (d) (ysh-xref--make-item d inf))
                            (cl-remove-if-not
                             (lambda (d)
                               (and (funcall match d)
                                    (or (eq inf info)
                                        (ysh-xref--top-level-p d))))
                             (ysh-xref--info-defs inf))))))

;; ---------------------------------------------------------------------
;; Auto-mode and interpreter support
;; ---------------------------------------------------------------------

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ysh\\'" . ysh-mode))

;;;###autoload
(add-to-list 'interpreter-mode-alist '("ysh" . ysh-mode))

(provide 'ysh-mode)
;;; ysh-mode.el ends here
