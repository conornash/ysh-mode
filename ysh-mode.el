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
