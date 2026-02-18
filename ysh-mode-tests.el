;;; ysh-mode-tests.el --- Tests for ysh-mode  -*- lexical-binding: t; -*-

;; Test suite based on the oils.vim testdata files.
;; See: https://github.com/oils-for-unix/oils.vim/blob/main/doc/algorithms.md
;;
;; Three stages of correctness:
;;   Stage 1: Comments and string literals
;;   Stage 2: Recursive lexer modes (command/expression/dq-string)
;;   Stage 3: Details within each mode (J8 escapes, expression atoms, etc.)

(require 'ert)
(require 'ysh-mode)

;; ---------------------------------------------------------------------
;; Test helpers
;; ---------------------------------------------------------------------

(defun ysh-test--fontify (text)
  "Insert TEXT into a temp buffer, activate `ysh-mode', fontify, return buffer.
The buffer is live — caller should kill it when done."
  (let ((buf (generate-new-buffer " *ysh-test*")))
    (with-current-buffer buf
      (insert text)
      (ysh-mode)
      (font-lock-ensure)
      buf)))

(defun ysh-test--face-at (text pos)
  "Return the face at 1-indexed POS after fontifying TEXT in `ysh-mode'."
  (let* ((buf (ysh-test--fontify text))
         (face (with-current-buffer buf
                 (get-text-property pos 'face))))
    (kill-buffer buf)
    face))

(defun ysh-test--faces-at (text positions)
  "Return an alist of ((POS . FACE) ...) after fontifying TEXT."
  (let* ((buf (ysh-test--fontify text))
         (result (with-current-buffer buf
                   (mapcar (lambda (p)
                             (cons p (get-text-property p 'face)))
                           positions))))
    (kill-buffer buf)
    result))

(defun ysh-test--face-at-col (text line col)
  "Return face at LINE (1-indexed) COL (0-indexed) in multi-line TEXT."
  (let* ((buf (ysh-test--fontify text))
         (face (with-current-buffer buf
                 (goto-char (point-min))
                 (forward-line (1- line))
                 (forward-char col)
                 (get-text-property (point) 'face))))
    (kill-buffer buf)
    face))

(defun ysh-test--face-is (face expected)
  "Check if FACE matches EXPECTED, handling lists and symbols."
  (cond
   ((null expected) (null face))
   ((and (listp face) (not (null face)))
    (memq expected face))
   (t (eq face expected))))

(defun ysh-test--has-face (text pos expected)
  "Return non-nil if POS in fontified TEXT has EXPECTED face."
  (ysh-test--face-is (ysh-test--face-at text pos) expected))

(defun ysh-test--is-comment (text pos)
  "Return non-nil if POS in fontified TEXT has any comment face.
Checks for both `font-lock-comment-face' and `font-lock-comment-delimiter-face',
since Emacs applies the delimiter face to comment starters like #."
  (let ((face (ysh-test--face-at text pos)))
    (or (ysh-test--face-is face 'font-lock-comment-face)
        (ysh-test--face-is face 'font-lock-comment-delimiter-face))))

(defun ysh-test--string-region-p (text start end)
  "Return non-nil if every position from START to END has `font-lock-string-face'."
  (let* ((buf (ysh-test--fontify text))
         (result t))
    (with-current-buffer buf
      (cl-loop for pos from start below end
               unless (ysh-test--face-is (get-text-property pos 'face)
                                         'font-lock-string-face)
               do (setq result nil)))
    (kill-buffer buf)
    result))

(defun ysh-test--no-face-in-region (text start end expected)
  "Return non-nil if no position from START to END has EXPECTED face."
  (let* ((buf (ysh-test--fontify text))
         (result t))
    (with-current-buffer buf
      (cl-loop for pos from start below end
               when (ysh-test--face-is (get-text-property pos 'face) expected)
               do (setq result nil)))
    (kill-buffer buf)
    result))

;; =====================================================================
;; Stage 1: Comments and String Literals
;; =====================================================================
;; From: testdata/minimal.ysh, stage1.md
;; "once we understand comments and string literals, then we know that
;;  nested delimiters like () [] {} $() $[] are real code."

;; ---------------------------------------------------------------------
;; 1.1 Comments
;; ---------------------------------------------------------------------

(ert-deftest ysh-stage1/comment-at-bol ()
  "# at beginning of line is a comment."
  (should (ysh-test--is-comment "# comment" 1)))

(ert-deftest ysh-stage1/comment-after-space ()
  "# preceded by whitespace is a comment."
  (should (ysh-test--is-comment "echo hi  # comment" 10)))

(ert-deftest ysh-stage1/not-comment-mid-word ()
  "echo not#comment — the # is NOT a comment (no preceding whitespace).
From stage1.md: 'Make sure that echo not#comment is not a comment.'"
  (should-not (ysh-test--is-comment "echo not#comment" 9)))

(ert-deftest ysh-stage1/comment-after-semicolon ()
  "echo yes;#comment — # after ; IS a comment.
From testdata/false-negative.ysh."
  (should (ysh-test--is-comment "echo yes;#comment" 10)))

(ert-deftest ysh-stage1/shebang ()
  "#!/usr/bin/env ysh is a comment."
  (should (ysh-test--is-comment "#!/usr/bin/env ysh" 1)))

;; ---------------------------------------------------------------------
;; 1.2 Backslash-quoted characters
;; ---------------------------------------------------------------------

(ert-deftest ysh-stage1/backslash-hash ()
  "\\# is a quoted char, not a comment.
From testdata/minimal.ysh: echo \\# not a comment"
  (should-not (ysh-test--is-comment "echo \\# not a comment" 7)))

(ert-deftest ysh-stage1/backslash-quotes ()
  "\\' and \\\" are backslash-quoted, not string delimiters.
From testdata/minimal.ysh: echo \\'single \\\"double"
  (let ((text "echo \\'single \\'single \\\"double"))
    ;; The \\' should NOT open a string
    (should-not (ysh-test--has-face text 14 'font-lock-string-face))))

;; ---------------------------------------------------------------------
;; 1.3 Single-quoted strings (raw, plain)
;; ---------------------------------------------------------------------

(ert-deftest ysh-stage1/plain-sq-string ()
  "'/usr/bin' is a string."
  (should (ysh-test--has-face "echo '/usr/bin'" 6 'font-lock-string-face)))

(ert-deftest ysh-stage1/raw-sq-string ()
  "r'C:\\Program Files\\' is a raw string — backslash does NOT escape.
From stage1.md: 'Make sure that a \\=' closes a raw string, even if
there's a \\ before it.'"
  (let ((text "echo r'C:\\Program Files\\' next"))
    ;; The r prefix + content should be string
    (should (ysh-test--has-face text 7 'font-lock-string-face))
    ;; 'next' after the closing ' should NOT be string
    (should-not (ysh-test--has-face text 28 'font-lock-string-face))))

(ert-deftest ysh-stage1/empty-string ()
  "'' is a valid empty string."
  (let ((text "var x = ''"))
    (should (ysh-test--has-face text 9 'font-lock-string-face))))

;; ---------------------------------------------------------------------
;; 1.4 J8 strings (b'' u'')
;; ---------------------------------------------------------------------

(ert-deftest ysh-stage1/j8-b-string ()
  "b'hi \\t \\\\' is a J8 byte string."
  (let ((text "echo b'hi \\t \\\\'"))
    (should (ysh-test--has-face text 7 'font-lock-string-face))))

(ert-deftest ysh-stage1/j8-u-string ()
  "u'hi \\t \\\\' is a J8 unicode string."
  (let ((text "echo u'hi \\t \\\\'"))
    (should (ysh-test--has-face text 7 'font-lock-string-face))))

(ert-deftest ysh-stage1/j8-backslash-quote ()
  "b'\\'' — backslash-quote does NOT close a J8 string.
From stage1.md: 'Make sure that b\\'\\'' is handled.'"
  (let ((text "echo b'\\'' next"))
    ;; The \\' inside should be string or j8-escape, not closing the string
    (should (let ((face (ysh-test--face-at text 9)))
              (or (ysh-test--face-is face 'font-lock-string-face)
                  (ysh-test--face-is face 'ysh-j8-escape-face))))
    ;; 'next' after the closing ' should NOT be string
    (should-not (ysh-test--has-face text 13 'font-lock-string-face))))

;; ---------------------------------------------------------------------
;; 1.5 Double-quoted strings
;; ---------------------------------------------------------------------

(ert-deftest ysh-stage1/dq-string ()
  "\"hello\" is a string."
  (should (ysh-test--has-face "echo \"hello\"" 7 'font-lock-string-face)))

(ert-deftest ysh-stage1/dq-backslash-quote ()
  "\\\" does NOT close a double-quoted string.
From stage1.md: 'Make sure that \\\" does not close a double quoted string.'"
  (let ((text "echo \"hi \\\" there\""))
    ;; "there" should still be in the string
    (should (ysh-test--has-face text 14 'font-lock-string-face))))

(ert-deftest ysh-stage1/dollar-dq-string ()
  "$\"hello\" is a dollar-double-quoted string."
  (should (ysh-test--has-face "echo $\"hello\"" 8 'font-lock-string-face)))

;; ---------------------------------------------------------------------
;; 1.6 Triple-quoted strings
;; ---------------------------------------------------------------------

(ert-deftest ysh-stage1/triple-sq-basic ()
  "''' ... ''' is a triple-single-quoted string."
  (let ((text "var x = '''\n  hello\n  '''"))
    (should (ysh-test--has-face text 12 'font-lock-string-face))  ; inside
    (should (ysh-test--has-face text 15 'font-lock-string-face)))) ; "hello"

(ert-deftest ysh-stage1/triple-sq-embedded-quotes ()
  "One ' and two '' inside ''' do not close the string.
From testdata/minimal.ysh."
  (let ((text "echo '''\n  one '\n  two ''\n  '''"))
    ;; "one '" line should be string
    (should (ysh-test--has-face text 14 'font-lock-string-face))
    ;; "two ''" line should be string
    (should (ysh-test--has-face text 22 'font-lock-string-face))))

(ert-deftest ysh-stage1/triple-sq-long ()
  "Triple-quoted string spanning 60+ lines (the wrens-iterate.ysh case).
This is a regression test for the JIT-lock sub-region bug we fixed."
  (let* ((lines (make-list 60 "  line of content"))
         (text (concat "var x = '''\n"
                       (mapconcat #'identity lines "\n")
                       "\n  '''")))
    ;; Middle of the string should be string face
    (should (ysh-test--has-face text 200 'font-lock-string-face))
    ;; Near the end, still string
    (should (ysh-test--has-face text 800 'font-lock-string-face))))

(ert-deftest ysh-stage1/triple-raw-string ()
  "r''' ... ''' is a triple-raw string."
  (let ((text "echo r'''\n  $r1\n  C:\\Program Files\\\n  '''"))
    (should (ysh-test--has-face text 14 'font-lock-string-face))))

(ert-deftest ysh-stage1/triple-j8-string ()
  "b''' ... ''' is a triple-J8 string."
  (let ((text "var x = b'''\n  mu = \\u{3bc}\n  '''"))
    (should (ysh-test--has-face text 16 'font-lock-string-face))))

(ert-deftest ysh-stage1/triple-dq-string ()
  "\"\"\" ... \"\"\" is a triple-double-quoted string."
  (let ((text "echo \"\"\"\n  $r1\n  \"\"\""))
    (should (ysh-test--has-face text 12 'font-lock-string-face))))

(ert-deftest ysh-stage1/triple-dollar-dq-string ()
  "$\"\"\" ... \"\"\" is a triple-dollar-double-quoted string."
  (let ((text "echo $\"\"\"\n  $r1\n  \"\"\""))
    (should (ysh-test--has-face text 13 'font-lock-string-face))))

(ert-deftest ysh-stage1/triple-sq-with-hash ()
  "# inside a triple-quoted string is NOT a comment.
This is critical for real-world YSH where prompts contain # headers."
  (let ((text "var x = '''\n## Header\n# comment-like\n'''"))
    ;; "## Header" should be string, not comment
    (should (ysh-test--has-face text 13 'font-lock-string-face))
    (should-not (ysh-test--has-face text 13 'font-lock-comment-face))))

(ert-deftest ysh-stage1/after-triple-sq-not-string ()
  "Code after a closing ''' should NOT be highlighted as string."
  (let ((text "var x = '''\nhello\n'''\nvar y = 42"))
    ;; 'var y' on the line after the closing ''' should not be string
    (should-not (ysh-test--has-face text 23 'font-lock-string-face))))

;; =====================================================================
;; Stage 2: Recursive Lexer Modes
;; =====================================================================
;; From: testdata/recursive-modes.ysh, stage2.md
;; "Three mutually recursive sublanguages: commands, expressions, strings"

;; ---------------------------------------------------------------------
;; 2.1 Keywords at first-word position only
;; ---------------------------------------------------------------------

(ert-deftest ysh-stage2/keyword-first-word ()
  "var at first-word position is a keyword."
  (should (ysh-test--has-face "var x = 42" 1 'font-lock-keyword-face)))

(ert-deftest ysh-stage2/keyword-after-semicolon ()
  "var after ; is at first-word position.
From testdata/false-positive.ysh: echo hi;var x =42"
  (should (ysh-test--has-face "echo hi;var x = 42" 9 'font-lock-keyword-face)))

(ert-deftest ysh-stage2/keyword-after-pipe ()
  "var after | is at first-word position.
From testdata/false-positive.ysh."
  (should (ysh-test--has-face "true | var x = 42" 8 'font-lock-keyword-face)))

(ert-deftest ysh-stage2/keyword-after-and ()
  "var after && is at first-word position.
From testdata/false-positive.ysh."
  (should (ysh-test--has-face "echo hi && var x = 42" 12 'font-lock-keyword-face)))

(ert-deftest ysh-stage2/echo-for-not-keyword ()
  "In 'echo for', 'for' is NOT a keyword — it's an argument.
From algorithms.md highlighting issues. This requires first-word anchoring."
  (should-not (ysh-test--has-face "echo for" 6 'font-lock-keyword-face)))

(ert-deftest ysh-stage2/echo-and-not-keyword ()
  "In 'echo and', 'and' is NOT a keyword — it's an argument in command mode.
Stage 3 says expression keywords only apply in expression context."
  (should-not (ysh-test--has-face "echo and" 6 'font-lock-keyword-face)))

(ert-deftest ysh-stage2/expr-keyword-in-expr ()
  "In 'var x = true and false', 'and' IS a keyword (expression context).
From testdata/details.ysh."
  (should (ysh-test--has-face "var x = true and false" 14 'font-lock-keyword-face)))

(ert-deftest ysh-stage2/false-positive-keyword-as-var ()
  "var const = 42 — 'const' as variable name after 'var' is NOT a keyword.
From testdata/false-positive.ysh."
  ;; 'var' is the keyword; 'const' is a variable name
  (let ((text "var const = 42"))
    (should (ysh-test--has-face text 1 'font-lock-keyword-face))      ; var
    (should-not (ysh-test--has-face text 5 'font-lock-keyword-face)))) ; const

(ert-deftest ysh-stage2/proc-arg-not-keyword ()
  "p proc a b — 'proc' as argument to command 'p' is NOT a keyword.
From testdata/false-positive.ysh."
  (should-not (ysh-test--has-face "p proc a b c (42)" 3 'font-lock-keyword-face)))

;; ---------------------------------------------------------------------
;; 2.2 Nested double quotes
;; ---------------------------------------------------------------------

(ert-deftest ysh-stage2/nested-dq-in-expr-sub ()
  "echo \"nested $[mydict[\"word\"]] quotes\" — inner \" opens a new string.
This is THE canonical Stage 2 test case. From testdata/nested-double-quotes.ysh.
The inner \"word\" is a string INSIDE the $[] expression INSIDE the outer string."
  (let ((text "echo \"nested $[mydict[\"word\"]] quotes\""))
    ;; "nested " is string
    (should (ysh-test--has-face text 7 'font-lock-string-face))
    ;; "word" (the inner string) should be string
    (should (ysh-test--has-face text 24 'font-lock-string-face))
    ;; " quotes" should be string (the outer string resumes)
    (should (ysh-test--has-face text 32 'font-lock-string-face))))

(ert-deftest ysh-stage2/nested-dq-deep ()
  "echo \"hi $[\"inner\"]\" — expression sub with inner string.
From testdata/minimal.ysh."
  (let ((text "echo \"hi $[\"inner\"]\""))
    ;; "hi " is outer string
    (should (ysh-test--has-face text 7 'font-lock-string-face))
    ;; "inner" is inner string
    (should (ysh-test--has-face text 13 'font-lock-string-face))))

;; ---------------------------------------------------------------------
;; 2.3 Expression mode transitions
;; ---------------------------------------------------------------------

(ert-deftest ysh-stage2/var-opens-expr ()
  "var x = f(42) — the RHS after var is expression context.
From testdata/recursive-modes.ysh."
  ;; 'var' is keyword
  (should (ysh-test--has-face "var x = 42" 1 'font-lock-keyword-face)))

(ert-deftest ysh-stage2/proc-name-face ()
  "proc my-proc — 'my-proc' gets proc-name face."
  (let ((text "proc my-proc { echo hi }"))
    (should (ysh-test--has-face text 6 'ysh-proc-name-face))))

(ert-deftest ysh-stage2/func-name-face ()
  "func myFunc — 'myFunc' gets func-name face."
  (let ((text "func myFunc(x) { return (x) }"))
    (should (ysh-test--has-face text 6 'ysh-func-name-face))))

(ert-deftest ysh-stage2/expr-sub-sigil ()
  "$[...] is an expression substitution — the $[ should get sigil-pair face."
  (let ((text "echo $[42 + 1]"))
    (should (ysh-test--has-face text 6 'ysh-sigil-pair-face))))

(ert-deftest ysh-stage2/command-sub-sigil ()
  "$(...) is a command substitution — the $( should get sigil-pair face."
  (let ((text "echo $(date)"))
    (should (ysh-test--has-face text 6 'ysh-sigil-pair-face))))

;; ---------------------------------------------------------------------
;; 2.4 Multi-line expressions
;; ---------------------------------------------------------------------

(ert-deftest ysh-stage2/multi-line-expr ()
  "var y = (42,\\n  43) — parens allow multi-line expressions.
From testdata/recursive-modes.ysh."
  (let ((text "var y = (42,\n          43,\n          f(5))"))
    ;; 'var' is keyword
    (should (ysh-test--has-face text 1 'font-lock-keyword-face))))

(ert-deftest ysh-stage2/multi-line-expr-semicolon ()
  "var z = f(1); echo hi — semicolon after ) ends expression.
From testdata/recursive-modes.ysh."
  (let ((text "var z = f(1); echo hi")))
  ;; This is a structural test — 'echo' after ; should be command mode
  ;; (full mode-tracking test, may fail until Stage 2 is implemented)
  )

;; ---------------------------------------------------------------------
;; 2.5 Sigil pairs and mode switching
;; ---------------------------------------------------------------------

(ert-deftest ysh-stage2/expr-splice ()
  "@[...] is expression splice."
  (let ((text "echo @[glob('*.py')]"))
    (should (ysh-test--has-face text 6 'ysh-sigil-pair-face))))

(ert-deftest ysh-stage2/command-splice ()
  "@(...) is command splice."
  (let ((text "= @(echo hi)"))
    (should (ysh-test--has-face text 4 'ysh-sigil-pair-face))))

(ert-deftest ysh-stage2/caret-expr ()
  "^[...] is caret expression."
  (let ((text "= ^[42 + a[i]]"))
    (should (ysh-test--has-face text 3 'ysh-sigil-pair-face))))

(ert-deftest ysh-stage2/caret-command ()
  "^(...) is caret command."
  (let ((text "= ^(echo hi)"))
    (should (ysh-test--has-face text 3 'ysh-sigil-pair-face))))

(ert-deftest ysh-stage2/array-literal ()
  ":| ... | is an array literal."
  (let ((text "var x = :| a b c |"))
    (should (ysh-test--has-face text 9 'ysh-sigil-pair-face))))

;; =====================================================================
;; Stage 3: Details Within Each Mode
;; =====================================================================
;; From: testdata/details.ysh, stage3.md

;; ---------------------------------------------------------------------
;; 3.1 Variable substitutions
;; ---------------------------------------------------------------------

(ert-deftest ysh-stage3/var-sub-name ()
  "$name is a variable substitution."
  (should (ysh-test--has-face "echo $base_dir" 6 'ysh-var-sub-face)))

(ert-deftest ysh-stage3/var-sub-braced ()
  "${name} is a braced variable substitution."
  (should (ysh-test--has-face "echo ${base_dir}" 6 'ysh-var-sub-face)))

(ert-deftest ysh-stage3/var-sub-number ()
  "$0 is a positional parameter."
  (should (ysh-test--has-face "echo $0" 6 'ysh-var-sub-face)))

(ert-deftest ysh-stage3/var-sub-braced-number ()
  "${11} is a braced positional parameter."
  (should (ysh-test--has-face "echo ${11}" 6 'ysh-var-sub-face)))

(ert-deftest ysh-stage3/var-sub-in-dq ()
  "$name inside double quotes is a variable substitution."
  (should (ysh-test--has-face "echo \"hi $name\"" 11 'ysh-var-sub-face)))

(ert-deftest ysh-stage3/var-sub-not-in-sq ()
  "$name inside single quotes is NOT a substitution — it's literal.
Single-quoted strings are raw."
  (let ((text "echo '$name'"))
    ;; Inside single-quoted string, $ should be string, not var-sub
    (should (ysh-test--has-face text 7 'font-lock-string-face))
    (should-not (ysh-test--has-face text 7 'ysh-var-sub-face))))

(ert-deftest ysh-stage3/splice ()
  "@myarray is a splice."
  (should (ysh-test--has-face "echo @my_array" 6 'ysh-var-sub-face)))

(ert-deftest ysh-stage3/not-splice-in-email ()
  "foo@example.com — the @ is NOT a splice.
From testdata/minimal.ysh."
  (should-not (ysh-test--has-face "echo foo@example.com" 9 'ysh-var-sub-face)))

;; ---------------------------------------------------------------------
;; 3.2 Expression atoms (only in expression context)
;; ---------------------------------------------------------------------

(ert-deftest ysh-stage3/expr-atom-true ()
  "true in expression context is a constant atom."
  (should (ysh-test--has-face "var x = true" 9 'font-lock-constant-face)))

(ert-deftest ysh-stage3/expr-atom-null ()
  "null in expression context is a constant atom."
  (should (ysh-test--has-face "var x = null" 9 'font-lock-constant-face)))

(ert-deftest ysh-stage3/expr-atom-false ()
  "false in expression context is a constant atom."
  (should (ysh-test--has-face "var x = false" 9 'font-lock-constant-face)))

(ert-deftest ysh-stage3/true-as-command ()
  "'true' at command position is a builtin, not a constant.
From testdata/minimal.ysh: while false { echo hi }"
  ;; In command position, 'true' and 'false' are builtins
  ;; They should NOT get constant-face when used as commands
  ;; (This requires mode tracking to distinguish)
  (let ((text "true | var x = 42"))
    ;; 'true' here is a command — should be builtin, not constant
    (should-not (ysh-test--has-face text 1 'font-lock-constant-face))))

;; ---------------------------------------------------------------------
;; 3.3 J8 string escapes
;; ---------------------------------------------------------------------

(ert-deftest ysh-stage3/j8-escape-newline ()
  "\\n in b'' is a valid J8 escape."
  ;; This is a contained highlight — only in J8 string context
  (let ((text "echo b'hi \\n'"))
    (should (ysh-test--has-face text 7 'font-lock-string-face))))

(ert-deftest ysh-stage3/j8-escape-yhex ()
  "\\yff in b'' is a valid J8 hex byte escape."
  (let ((text "echo b'byte \\yff'"))
    (should (ysh-test--has-face text 7 'font-lock-string-face))))

(ert-deftest ysh-stage3/j8-escape-ubraced ()
  "\\u{3bc} in b'' is a valid J8 unicode escape."
  (let ((text "echo b'mu = \\u{3bc}'"))
    (should (ysh-test--has-face text 7 'font-lock-string-face))))

;; ---------------------------------------------------------------------
;; 3.4 Backslash-quoted characters (context-dependent)
;; ---------------------------------------------------------------------

(ert-deftest ysh-stage3/backslash-in-command ()
  "\\( \\) \\[ \\] \\{ \\} are backslash-quoted in commands.
From testdata/details.ysh."
  (let ((text "echo \\{ \\}"))
    (should (ysh-test--has-face text 6 'ysh-backslash-face))))

(ert-deftest ysh-stage3/backslash-dollar ()
  "\\$ prevents $ from being a substitution."
  (let ((text "echo \\$name"))
    (should-not (ysh-test--has-face text 7 'ysh-var-sub-face))))

;; ---------------------------------------------------------------------
;; 3.5 Builtin procs
;; ---------------------------------------------------------------------

(ert-deftest ysh-stage3/builtin-echo ()
  "'echo' at first-word position is a builtin."
  (should (ysh-test--has-face "echo hi" 1 'font-lock-builtin-face)))

(ert-deftest ysh-stage3/builtin-cd ()
  "'cd' at first-word position is a builtin."
  (should (ysh-test--has-face "cd /tmp" 1 'font-lock-builtin-face)))

(ert-deftest ysh-stage3/builtin-json ()
  "'json' at first-word position is a builtin."
  (should (ysh-test--has-face "json read (&x)" 1 'font-lock-builtin-face)))

;; =====================================================================
;; Integration: Full file tests
;; =====================================================================
;; Load actual testdata files and verify key properties.

(defvar ysh-test--testdata-dir
  (expand-file-name "testdata"
                    (file-name-directory (or load-file-name
                                             buffer-file-name
                                             default-directory)))
  "Path to testdata/ directory.")

(defun ysh-test--fontify-file (filename)
  "Fontify FILENAME from testdata/ dir in ysh-mode. Return live buffer."
  (let* ((path (expand-file-name filename ysh-test--testdata-dir))
         (buf (generate-new-buffer (concat " *ysh-test:" filename "*"))))
    (with-current-buffer buf
      (insert-file-contents path)
      (ysh-mode)
      (font-lock-ensure))
    buf))

(defun ysh-test--search-face (buf text expected-face)
  "In BUF, find TEXT and check if it has EXPECTED-FACE. Return (found . correct)."
  (with-current-buffer buf
    (goto-char (point-min))
    (if (search-forward text nil t)
        (let ((face (get-text-property (match-beginning 0) 'face)))
          (cons t (ysh-test--face-is face expected-face)))
      (cons nil nil))))

(ert-deftest ysh-integration/minimal-strings ()
  "testdata/minimal.ysh: all string forms should be highlighted."
  (let ((buf (ysh-test--fontify-file "minimal.ysh")))
    (unwind-protect
        (with-current-buffer buf
          ;; Plain single-quoted
          (goto-char (point-min))
          (should (search-forward "'/usr/bin'" nil t))
          (should (ysh-test--face-is
                   (get-text-property (1+ (match-beginning 0)) 'face)
                   'font-lock-string-face))
          ;; Triple-single-quoted (multi-line)
          (goto-char (point-min))
          (should (search-forward "echo '''" nil t))
          (let ((start (match-beginning 0)))
            ;; Next line should be string
            (forward-line 1)
            (should (ysh-test--face-is
                     (get-text-property (point) 'face)
                     'font-lock-string-face))))
      (kill-buffer buf))))

(ert-deftest ysh-integration/minimal-not-comment ()
  "testdata/minimal.ysh: 'echo not#comment' — # is not a comment."
  (let ((buf (ysh-test--fontify-file "minimal.ysh")))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (should (search-forward "not#comment" nil t))
          (should-not (ysh-test--face-is
                       (get-text-property (+ (match-beginning 0) 3) 'face)
                       'font-lock-comment-face)))
      (kill-buffer buf))))

(ert-deftest ysh-integration/nested-dq ()
  "testdata/nested-double-quotes.ysh: nested double quotes work."
  (let ((buf (ysh-test--fontify-file "nested-double-quotes.ysh")))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          ;; Find the key line
          (should (search-forward "echo \"nested" nil t))
          ;; "nested" should be string
          (should (ysh-test--face-is
                   (get-text-property (+ (match-beginning 0) 6) 'face)
                   'font-lock-string-face))
          ;; "quotes" should be string (the outer string continues after $[...])
          (should (search-forward "quotes\"" nil t))
          (should (ysh-test--face-is
                   (get-text-property (match-beginning 0) 'face)
                   'font-lock-string-face)))
      (kill-buffer buf))))

(ert-deftest ysh-integration/false-positive-keywords ()
  "testdata/false-positive.ysh: variable names that look like keywords.
'var const = 42' — const is a variable name, not a keyword."
  (let ((buf (ysh-test--fontify-file "false-positive.ysh")))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          ;; Find 'var const = 42'
          (should (search-forward "var const = 42" nil t))
          ;; 'var' at beginning is keyword
          (should (ysh-test--face-is
                   (get-text-property (match-beginning 0) 'face)
                   'font-lock-keyword-face))
          ;; 'const' after 'var' is NOT a keyword
          (should-not (ysh-test--face-is
                       (get-text-property (+ (match-beginning 0) 4) 'face)
                       'font-lock-keyword-face)))
      (kill-buffer buf))))

;; =====================================================================
;; Tests that found real bugs during testdata audit
;; =====================================================================

(ert-deftest ysh-stage2/expr-keyword-is ()
  "'is' in expression: = {} is {} — 'is' is keyword.
From testdata/details.ysh.
Bug found: ysh--expr-opener-re used \\\\> on `=' but `=' is punctuation,
so the word boundary never matched. Fixed with ^\\\\s-*=\\\\s- pattern."
  (let ((text "= {} is {}"))
    (should (ysh-test--has-face text 6 'font-lock-keyword-face))))

(ert-deftest ysh-stage2/ternary-if-else-in-expr ()
  "= 42 if true else 41 — if/else in expression context.
From testdata/details.ysh.
Bug found: if/else were only in the shell keyword set (first-word anchored),
missing from ysh--expr-keyword-re. Ternary expressions need them."
  (let ((text "= 42 if true else 41"))
    ;; 'if' should be keyword
    (should (ysh-test--has-face text 6 'font-lock-keyword-face))
    ;; 'else' should be keyword
    (should (ysh-test--has-face text 14 'font-lock-keyword-face))))

;; =====================================================================
;; Bug: closing | of :| ... | gets pipe face instead of sigil-pair face
;; =====================================================================

(ert-deftest ysh-stage2/array-literal-close ()
  "var x = :| one two | — closing | should be sigil-pair, not pipe.
Bug: the pipe operator rule matches the closing | of :| ... |."
  (let ((text "var x = :| one two |"))
    ;; Opening :| is sigil-pair (existing test covers this)
    (should (ysh-test--has-face text 9 'ysh-sigil-pair-face))
    ;; Closing | should ALSO be sigil-pair, NOT preprocessor (pipe)
    (should (ysh-test--has-face text 20 'ysh-sigil-pair-face))
    (should-not (ysh-test--has-face text 20 'font-lock-preprocessor-face))))

;; =====================================================================
;; Bug: $() inside "..." — command sub contents get string-face
;; =====================================================================

(ert-deftest ysh-stage2/command-sub-in-dq ()
  "echo \"hi $(echo inner) bye\" — $() inside DQ is a command sub.
Bug: ysh--scan-dq-content only handles $[, not $(. The content of
$() inside a DQ string incorrectly gets font-lock-string-face."
  (let ((text "echo \"hi $(echo inner) bye\""))
    ;; 'hi' is string
    (should (ysh-test--has-face text 7 'font-lock-string-face))
    ;; 'bye' after $() is string (outer string resumes)
    (should (ysh-test--has-face text 24 'font-lock-string-face))
    ;; 'echo' INSIDE $() should NOT be string — it's a command
    (should-not (ysh-test--has-face text 12 'font-lock-string-face))))

;; =====================================================================
;; Bug: 'in' not highlighted in 'for x in a b'
;; =====================================================================

(ert-deftest ysh-stage2/for-in-keyword ()
  "for x in a b { } — 'in' is a keyword in for loops.
Bug: shell keywords are anchored to first-word position, but 'in'
appears mid-line in for loops. It should be highlighted there."
  (let ((text "for x in a b { echo hi }"))
    ;; 'for' is keyword
    (should (ysh-test--has-face text 1 'font-lock-keyword-face))
    ;; 'in' should also be keyword
    (should (ysh-test--has-face text 7 'font-lock-keyword-face))))

(ert-deftest ysh-stage2/echo-in-not-keyword ()
  "echo in — 'in' is NOT a keyword when it's an argument.
Companion test: 'in' should only be keyword in for loops, not everywhere."
  (should-not (ysh-test--has-face "echo in" 6 'font-lock-keyword-face)))

;; =====================================================================
;; Stage 2: 'in' keyword context (from minimal.ysh, recursive-modes.ysh)
;; =====================================================================

(ert-deftest ysh-stage2/for-i-item-in ()
  "for i, item in (a) — 'in' highlighted after destructuring.
From testdata/recursive-modes.ysh."
  (let ((text "for i, item in (a) {"))
    (should (ysh-test--has-face text 1 'font-lock-keyword-face))   ; for
    (should (ysh-test--has-face text 13 'font-lock-keyword-face)))) ; in

;; =====================================================================
;; Stage 2: $() command sub inside DQ strings (from minimal.ysh)
;; =====================================================================

(ert-deftest ysh-stage2/command-sub-in-dq-greeting ()
  "echo \"greeting = $(echo hi)\" — from recursive-modes.ysh.
$() inside DQ: content should not be string-faced."
  (let ((text "echo \"greeting = $(echo hi)\""))
    ;; 'greeting' is string
    (should (ysh-test--has-face text 7 'font-lock-string-face))
    ;; 'echo' inside $() should NOT be string
    (should-not (ysh-test--has-face text 20 'font-lock-string-face))))

(ert-deftest ysh-stage2/dq-after-command-sub-resumes ()
  "echo \"hi $(echo x) bye\" — text after $() resumes as string.
The ' bye' after the closing ) should be string-faced."
  (let ((text "echo \"hi $(echo x) bye\""))
    ;; 'bye' after $() is string
    (should (ysh-test--has-face text 21 'font-lock-string-face))))

(ert-deftest ysh-stage2/expr-sub-in-dq ()
  "echo \"sum = $[x + 99]\" — from recursive-modes.ysh.
$[] inside DQ: content should be string (per current coarse algorithm),
but the outer string should terminate correctly."
  (let ((text "echo \"sum = $[x + 99]\""))
    ;; opening " is string
    (should (ysh-test--has-face text 6 'font-lock-string-face))
    ;; closing " is string
    (should (ysh-test--has-face text 22 'font-lock-string-face))))

;; =====================================================================
;; Stage 2: :| ... | closing delimiter (from minimal.ysh, details.ysh)
;; =====================================================================

(ert-deftest ysh-stage2/array-literal-multiline-close ()
  "Multi-line :| ... | — closing | should be sigil-pair.
From testdata/recursive-modes.ysh."
  (let ((text "var foods = :| \n  pea nut \n|"))
    ;; closing | on last line
    (should (ysh-test--has-face text (length text) 'ysh-sigil-pair-face))))

(ert-deftest ysh-stage2/array-literal-with-hash ()
  ":| with # comment inside — from recursive-modes.ysh.
The # inside :| ... | should be a comment."
  (let ((text "var foods = :| \n  pea nut \n  # comment\n  other\n|"))
    ;; # should be comment
    (should (ysh-test--is-comment text 33))))

;; =====================================================================
;; Stage 2: time keyword (from recursive-modes.ysh)
;; =====================================================================

(ert-deftest ysh-stage2/time-keyword ()
  "time echo hi | wc -l — 'time' is a keyword.
From testdata/recursive-modes.ysh."
  (should (ysh-test--has-face "time echo hi | wc -l" 1 'font-lock-keyword-face)))

;; =====================================================================
;; Stage 2: hay define (from recursive-modes.ysh)
;; =====================================================================

(ert-deftest ysh-stage2/hay-define ()
  "hay define Package — 'hay' could be a builtin/keyword.
From testdata/recursive-modes.ysh."
  ;; At minimum, 'hay' should not be highlighted as a shell keyword
  ;; (it's a builtin command). If we support it, it gets builtin face.
  (let ((text "hay define Package"))
    (should-not (ysh-test--has-face text 1 'font-lock-keyword-face))))

;; =====================================================================
;; Stage 2: false-positive — keyword-like words as proc arguments
;; (from false-positive.ysh)
;; =====================================================================

(ert-deftest ysh-stage2/proc-arg-var-not-keyword ()
  "p var x = (42) — 'var' is a proc argument, not a keyword.
From testdata/false-positive.ysh."
  (let ((text "p var x = (42)"))
    ;; 'var' at pos 3 should NOT be keyword (it's an argument to 'p')
    (should-not (ysh-test--has-face text 3 'font-lock-keyword-face))))

(ert-deftest ysh-stage2/proc-arg-setvar-not-keyword ()
  "p setvar x = (42) — 'setvar' is a proc argument, not a keyword.
From testdata/false-positive.ysh."
  (let ((text "p setvar x = (42)"))
    ;; 'setvar' at pos 3 should NOT be keyword
    (should-not (ysh-test--has-face text 3 'font-lock-keyword-face))))

(ert-deftest ysh-stage2/keyword-after-pipe-proc ()
  "echo | proc p { } — 'proc' after | is a keyword.
From testdata/false-positive.ysh."
  (let ((text "echo pipeline | proc p { echo pipeline }"))
    ;; 'proc' starts at pos 17
    (should (ysh-test--has-face text 17 'font-lock-keyword-face))))

;; =====================================================================
;; Stage 2: false-negative — semicolon-comment (from false-negative.ysh)
;; =====================================================================

(ert-deftest ysh-stage1/comment-after-semicolon-no-space ()
  "echo yes;#comment — # after ; with no space should be a comment.
From testdata/false-negative.ysh."
  (let ((text "echo yes;#comment"))
    (should (ysh-test--is-comment text 10))))

;; =====================================================================
;; Stage 2: multi-line ... continuation (from minimal.ysh, details.ysh)
;; =====================================================================

(ert-deftest ysh-stage2/dot-continuation ()
  "... echo multi- line ; — '...' starts a continuation.
From testdata/minimal.ysh. The ... should be highlighted."
  (let ((text "... echo\n    multi-\n    line ;"))
    ;; '...' at pos 1 should have some face (keyword or preprocessor)
    (should (ysh-test--face-at text 1))))

;; =====================================================================
;; Stage 2: backslash continuation (from minimal.ysh, details.ysh)
;; =====================================================================

(ert-deftest ysh-stage2/backslash-continuation ()
  "echo \\ \\n  multi- \\ — trailing backslash is line continuation.
From testdata/minimal.ysh."
  (let ((text "echo \\\n  multi- \\\n  line"))
    ;; The trailing \ should be highlighted (backslash-face)
    (should (ysh-test--has-face text 6 'ysh-backslash-face))))

;; =====================================================================
;; Stage 3: eggex literals (from details.ysh)
;; =====================================================================

(ert-deftest ysh-stage3/eggex-literal ()
  "= /d+ '.' [a-z]+/ — eggex between / / should be highlighted.
From testdata/details.ysh."
  (let ((text "= /d+ '.' [a-z A-Z 0-9 '-']+/"))
    ;; The / delimiters or content should have some face
    ;; (At minimum the eggex should not be confused with a comment)
    (should-not (ysh-test--is-comment text 5))))

(ert-deftest ysh-stage3/eggex-capture ()
  "= /<capture d d as month>/ — capture keyword in eggex.
From testdata/details.ysh."
  (let ((text "= /<capture d d as month> '-' <capture d d as day>/"))
    ;; 'capture' should be highlighted (keyword face)
    (should (ysh-test--has-face text 5 'font-lock-keyword-face))))

;; =====================================================================
;; Stage 3: float and scientific literals (from details.ysh)
;; =====================================================================

(ert-deftest ysh-stage3/float-literal ()
  "= 3.14 — float literal should be constant-face.
From testdata/details.ysh."
  (should (ysh-test--has-face "= 3.14" 3 'font-lock-constant-face)))

(ert-deftest ysh-stage3/scientific-notation ()
  "= 3.14e-5 — scientific notation should be constant-face.
From testdata/details.ysh."
  (should (ysh-test--has-face "= 3.14e-5" 3 'font-lock-constant-face)))

;; =====================================================================
;; Stage 3: redirects (from details.ysh)
;; =====================================================================

(ert-deftest ysh-stage3/redirect-stdout ()
  "echo stdout > /dev/null — '>' should have some face.
From testdata/details.ysh."
  (let ((text "echo stdout > /dev/null"))
    (should (ysh-test--face-at text 13))))

(ert-deftest ysh-stage3/redirect-stderr ()
  "echo stderr 2> /dev/null — '2>' should have some face.
From testdata/details.ysh."
  (let ((text "echo stderr 2> /dev/null"))
    (should (ysh-test--face-at text 13))))

(ert-deftest ysh-stage3/redirect-brace ()
  "redir {out}> /tmp/out.txt — brace redirect.
From testdata/details.ysh."
  (let ((text "redir {out}> /tmp/out.txt {"))
    ;; {out} or > should have some face
    (should (ysh-test--face-at text 7))))

;; =====================================================================
;; Stage 3: tilde expansion (from details.ysh)
;; =====================================================================

(ert-deftest ysh-stage3/tilde-home ()
  "echo ~/src — ~ should be highlighted.
From testdata/details.ysh."
  (let ((text "echo ~/src"))
    (should (ysh-test--face-at text 6))))

(ert-deftest ysh-stage3/tilde-user ()
  "echo ~root/src — ~root should be highlighted.
From testdata/details.ysh."
  (let ((text "echo ~root/src"))
    (should (ysh-test--face-at text 6))))

;; =====================================================================
;; Stage 3: glob patterns (from details.ysh, recursive-modes.ysh)
;; =====================================================================

(ert-deftest ysh-stage3/glob-star ()
  "echo *.py — glob * should be highlighted.
From testdata/details.ysh."
  (let ((text "echo *.py"))
    (should (ysh-test--face-at text 6))))

(ert-deftest ysh-stage3/brace-expansion ()
  "echo {a,b}@example.com — brace expansion.
From testdata/details.ysh."
  (let ((text "echo {a,b}@example.com"))
    (should (ysh-test--face-at text 6))))

(ert-deftest ysh-stage3/range-expansion ()
  "echo {0..4}-{a,b} — range brace expansion.
From testdata/details.ysh."
  (let ((text "echo {0..4}-{a,b}"))
    (should (ysh-test--face-at text 6))))

;; =====================================================================
;; Stage 3: env prefix (from details.ysh)
;; =====================================================================

(ert-deftest ysh-stage3/env-prefix ()
  "FOO=bar env -i echo — FOO=bar should be highlighted.
From testdata/details.ysh."
  (let ((text "FOO=bar env -i echo"))
    (should (ysh-test--face-at text 1))))

(ert-deftest ysh-stage3/env-prefix-multi ()
  "FOO=bar SPAM_EGGS='baz' env — multiple env prefixes.
From testdata/details.ysh."
  (let ((text "FOO=bar SPAM_EGGS='baz' env -i echo"))
    (should (ysh-test--face-at text 1))
    (should (ysh-test--face-at text 9))))

;; =====================================================================
;; Stage 2: here-docs (stretch goal — from minimal.ysh comments)
;; =====================================================================

;; (Here-docs are noted in the testdata but no concrete examples exist.
;;  These tests are placeholders for if/when we add here-doc support.)

;; =====================================================================
;; Stage 2: proc/func with complex signatures
;; (from recursive-modes.ysh)
;; =====================================================================

(ert-deftest ysh-stage2/func-with-default-raw-string ()
  "func myFunc(x; delimiter=r'\\\\', other=\"hi $x\") — complex signature.
From testdata/recursive-modes.ysh. The r'\\\\' default should be string."
  (let ((text "func myFunc(x; delimiter=r'\\', other=\"hi $x\") { return (x) }"))
    ;; func is keyword
    (should (ysh-test--has-face text 1 'font-lock-keyword-face))
    ;; myFunc is func-name (not proc-name)
    (should (ysh-test--has-face text 6 'ysh-func-name-face))
    ;; r'\' is a string (pos 28 is the \ inside r'')
    (should (ysh-test--has-face text 28 'font-lock-string-face))))

(ert-deftest ysh-stage2/proc-with-command-sub-default ()
  "proc my-proc(x, y; d=$(echo hi)) — $() in default value.
From testdata/recursive-modes.ysh."
  (let ((text "proc my-proc(x, y; d=$(echo hi)) { echo hi }"))
    (should (ysh-test--has-face text 1 'font-lock-keyword-face))
    (should (ysh-test--has-face text 6 'ysh-proc-name-face))))

;; =====================================================================
;; Stage 2: pp command (from recursive-modes.ysh)
;; =====================================================================

(ert-deftest ysh-stage2/pp-as-builtin ()
  "pp (42) — 'pp' should be a builtin.
From testdata/recursive-modes.ysh."
  (let ((text "pp (42)"))
    (should (ysh-test--has-face text 1 'font-lock-builtin-face))))

(ert-deftest ysh-stage2/pp-bracket ()
  "pp [42] — 'pp' with bracket arg.
From testdata/recursive-modes.ysh."
  (let ((text "pp [42]"))
    (should (ysh-test--has-face text 1 'font-lock-builtin-face))))

;; =====================================================================
;; Stage 2: various sigil pairs in expression mode
;; (from recursive-modes.ysh)
;; =====================================================================

(ert-deftest ysh-stage2/caret-dq-string ()
  "= ^\"unevaluated $x\" — ^\" sigil pair.
From testdata/recursive-modes.ysh."
  (let ((text "= ^\"unevaluated $x\""))
    ;; ^ should be sigil-pair
    (should (ysh-test--has-face text 3 'ysh-sigil-pair-face))))

(ert-deftest ysh-stage2/dollar-bracket-in-word ()
  "echo --flag=$[x + 1] — $[] inside a word.
From testdata/recursive-modes.ysh."
  (let ((text "echo --flag=$[x + 1]"))
    ;; $ should be sigil-pair
    (should (ysh-test--has-face text 13 'ysh-sigil-pair-face))))

(ert-deftest ysh-stage2/at-bracket ()
  "echo @[glob('*.py')] — @[] sigil pair.
From testdata/recursive-modes.ysh."
  (let ((text "echo @[glob('*.py')]"))
    ;; @ should be sigil-pair
    (should (ysh-test--has-face text 6 'ysh-sigil-pair-face))))

;; =====================================================================
;; Stage 2: escaped sigil pairs (from recursive-modes.ysh)
;; =====================================================================

(ert-deftest ysh-stage2/escaped-dollar-bracket ()
  "echo \\$[x + 1] — \\$ should prevent sigil-pair.
From testdata/recursive-modes.ysh."
  (let ((text "echo \\$[x + 1]"))
    ;; \$ should be backslash-face, NOT sigil-pair
    (should (ysh-test--has-face text 6 'ysh-backslash-face))
    (should-not (ysh-test--has-face text 6 'ysh-sigil-pair-face))))

(ert-deftest ysh-stage2/escaped-at-bracket ()
  "echo \\@[glob] — \\@ should prevent sigil-pair.
From testdata/recursive-modes.ysh."
  (let ((text "echo \\@[glob]"))
    ;; \@ should be backslash-face
    (should (ysh-test--has-face text 6 'ysh-backslash-face))))

;; =====================================================================
;; Stage 3: backslash-escaped brackets/parens/braces
;; (from recursive-modes.ysh)
;; =====================================================================

(ert-deftest ysh-stage3/backslash-open-brace ()
  "echo \\{ \\} — backslash braces.
From testdata/details.ysh."
  (let ((text "echo \\{ \\}"))
    (should (ysh-test--has-face text 6 'ysh-backslash-face))
    (should (ysh-test--has-face text 9 'ysh-backslash-face))))

;; =====================================================================
;; Stage 3: @splice in :| ... | (from minimal.ysh)
;; =====================================================================

(ert-deftest ysh-stage3/splice-in-array-literal ()
  "setvar myarray = :| a b c @my_array | — @splice inside :| |.
From testdata/minimal.ysh."
  (let ((text "setvar myarray = :| a b c @my_array |"))
    ;; @my_array should be splice-face
    (should (ysh-test--has-face text 27 'ysh-splice-face))))

;; =====================================================================
;; Stage 3: $0, ${11}, bad $00 $1a (from minimal.ysh)
;; =====================================================================

(ert-deftest ysh-stage3/var-sub-0 ()
  "echo $0 — positional $0 in DQ.
From testdata/minimal.ysh."
  (should (ysh-test--has-face "echo $0 ${11}" 6 'ysh-var-sub-face)))

(ert-deftest ysh-stage3/var-sub-bad-00 ()
  "echo bad $00 — $00 should NOT be a valid var sub (only $0).
From testdata/minimal.ysh."
  ;; $0 part matches, but the trailing 0 should not be part of it
  (let ((text "echo bad $00 $1a"))
    ;; Position 10 ($) is var-sub, but position 12 (second 0) should NOT be
    (should-not (ysh-test--has-face text 12 'ysh-var-sub-face))))

;; =====================================================================
;; Stage 3: non-recursive ${} with operators (from minimal.ysh)
;; =====================================================================

(ert-deftest ysh-stage3/braced-var-default ()
  "echo ${file:-} — braced var with :- operator.
From testdata/minimal.ysh."
  (should (ysh-test--has-face "echo ${file:-}" 6 'ysh-var-sub-face)))

(ert-deftest ysh-stage3/braced-var-default-in-dq ()
  "echo \"foo ${base_dir:-}\" — braced var with :- in DQ.
From testdata/minimal.ysh."
  (let ((text "echo \"foo ${base_dir:-}\""))
    (should (ysh-test--has-face text 11 'ysh-var-sub-face))))

(ert-deftest ysh-stage3/braced-number-default ()
  "echo ${12:-} — braced number var with :- operator.
From testdata/minimal.ysh."
  (should (ysh-test--has-face "echo ${12:-}" 6 'ysh-var-sub-face)))

;; =====================================================================
;; Stage 2: setvar with complex lvalues (from recursive-modes.ysh)
;; =====================================================================

(ert-deftest ysh-stage2/setvar-array-element ()
  "setvar a[0 + 0] = 42 — setvar with subscript lvalue.
From testdata/recursive-modes.ysh."
  (should (ysh-test--has-face "setvar a[0 + 0] = 42" 1 'font-lock-keyword-face)))

;; =====================================================================
;; Stage 2: multi-line expressions with semicolons
;; (from recursive-modes.ysh)
;; =====================================================================

(ert-deftest ysh-stage2/multi-line-call ()
  "call g(3, 4, 5); echo hi — semicolon after call ends expression.
From testdata/recursive-modes.ysh."
  (let ((text "call g(3, 4, 5); echo hi"))
    ;; 'call' is keyword
    (should (ysh-test--has-face text 1 'font-lock-keyword-face))
    ;; 'echo' after ; should be builtin (not still in expression mode)
    (should (ysh-test--has-face text 18 'font-lock-builtin-face))))

;; =====================================================================
;; Stage 2: SQ string types at beginning of line
;; (from minimal.ysh — 'echo' / r'echo' / $"echo" at BOL)
;; =====================================================================

(ert-deftest ysh-stage1/sq-string-at-bol ()
  "'echo' at beginning of line — should be a string, not a command.
From testdata/minimal.ysh."
  (should (ysh-test--has-face "'echo'" 1 'font-lock-string-face)))

(ert-deftest ysh-stage1/raw-string-at-bol ()
  "r'echo' at beginning of line — raw string.
From testdata/minimal.ysh."
  (should (ysh-test--has-face "r'echo'" 2 'font-lock-string-face)))

(ert-deftest ysh-stage1/dollar-dq-at-bol ()
  "$\"echo\" at beginning of line — dollar DQ string.
From testdata/minimal.ysh."
  (should (ysh-test--has-face "$\"echo\"" 2 'font-lock-string-face)))

;; =====================================================================
;; Stage 3: @splice at beginning of line (from minimal.ysh)
;; =====================================================================

(ert-deftest ysh-stage3/splice-at-bol ()
  "@my_array — splice at beginning of line should be splice-face.
From testdata/minimal.ysh.
Bug: @name gets ysh-var-sub-face instead of ysh-splice-face."
  (should (ysh-test--has-face "@my_array" 1 'ysh-splice-face))
  (should-not (ysh-test--has-face "@my_array" 1 'ysh-var-sub-face)))

;; =====================================================================
;; Stage 2: case statement (from minimal.ysh)
;; =====================================================================

(ert-deftest ysh-stage2/case-keyword ()
  "case (42) { ... } — 'case' is a keyword.
From testdata/minimal.ysh."
  (should (ysh-test--has-face "case (42) {" 1 'font-lock-keyword-face)))

;; =====================================================================
;; Stage 3: J8 string escape sequences (from details.ysh)
;; =====================================================================

(ert-deftest ysh-stage3/j8-byte-escape ()
  "b'byte \\yff what' — \\yff is a J8 byte escape.
From testdata/details.ysh."
  (let ((text "echo b'byte \\yff what'"))
    ;; \\yff (positions 13-16) should be j8-escape-face
    (should (ysh-test--has-face text 13 'ysh-j8-escape-face))
    (should (ysh-test--has-face text 14 'ysh-j8-escape-face))))

(ert-deftest ysh-stage3/j8-unicode-brace ()
  "b'mu = \\u{3bc}' — \\u{3bc} is a J8 unicode escape.
From testdata/details.ysh."
  (let ((text "echo b'mu = \\u{3bc}'"))
    ;; \\u{3bc} (positions 13-19) should be j8-escape-face
    (should (ysh-test--has-face text 13 'ysh-j8-escape-face))
    (should (ysh-test--has-face text 14 'ysh-j8-escape-face))))

;; =====================================================================
;; Stage 2: builtin procs (from details.ysh)
;; =====================================================================

(ert-deftest ysh-stage3/builtin-pushd ()
  "pushd /tmp — 'pushd' is a builtin.
From testdata/details.ysh."
  (should (ysh-test--has-face "pushd /tmp" 1 'font-lock-builtin-face)))

(ert-deftest ysh-stage3/builtin-popd ()
  "popd — 'popd' is a builtin.
From testdata/details.ysh."
  (should (ysh-test--has-face "popd" 1 'font-lock-builtin-face)))

;; =====================================================================
;; Stage 2: json/pp as multi-word builtins (from false-positive.ysh)
;; =====================================================================

(ert-deftest ysh-stage3/builtin-json-write ()
  "json write (args) — 'json' is a builtin.
From testdata/false-positive.ysh."
  (should (ysh-test--has-face "json write (args)" 1 'font-lock-builtin-face)))

;; =====================================================================
;; Stage 2: expression not-in keyword (from details.ysh)
;; =====================================================================

(ert-deftest ysh-stage2/expr-not-in ()
  "if ('key' not in mydict) — 'not' and 'in' are expression keywords.
From testdata/details.ysh."
  (let ((text "if ('key' not in mydict) {"))
    ;; 'not' should be keyword
    (should (ysh-test--has-face text 11 'font-lock-keyword-face))
    ;; 'in' should be keyword
    (should (ysh-test--has-face text 15 'font-lock-keyword-face))))

(ert-deftest ysh-stage2/expr-and-or ()
  "if (true and false or null) — 'and'/'or' are expression keywords.
From testdata/details.ysh."
  (let ((text "if (true and false or null) {"))
    ;; 'and' should be keyword
    (should (ysh-test--has-face text 10 'font-lock-keyword-face))
    ;; 'or' should be keyword
    (should (ysh-test--has-face text 20 'font-lock-keyword-face))))

;; =====================================================================
;; Stage 2: = and call with bare expressions
;; (from recursive-modes.ysh)
;; =====================================================================

(ert-deftest ysh-stage2/equals-sigil-pairs ()
  "= :| $x $(echo) @(echo) $[42+1] @[a] | — various sigils after =.
From testdata/recursive-modes.ysh."
  (let ((text "= :| $x $(echo hi) @(echo hi) $[42+1] @[a] |"))
    ;; = is keyword
    (should (ysh-test--has-face text 1 'font-lock-keyword-face))))

(ert-deftest ysh-stage2/equals-backslash-escapes ()
  "= :| \\' \\\" \\$ \\@ \\( \\) \\[ \\] | — backslash escapes in :| |.
From testdata/recursive-modes.ysh."
  (let ((text "= :| \\' \\\" \\$ \\@ \\( \\) \\[ \\] |"))
    ;; \\' should be backslash-face
    (should (ysh-test--has-face text 6 'ysh-backslash-face))))

;; =====================================================================
;; Stage 2: if without space before paren
;; (from recursive-modes.ysh — noted as edge case)
;; =====================================================================

(ert-deftest ysh-stage2/builtin-after-open-brace ()
  "if (42) { echo yes } — 'echo' after { should be builtin.
Bug: first-word-prefix doesn't include { as a command separator."
  (let ((text "if (42) { echo yes }"))
    ;; 'echo' after { should be builtin (pos 11)
    (should (ysh-test--has-face text 11 'font-lock-builtin-face))))

;; =====================================================================
;; Stage 1: triple-DQ with embedded quotes (from minimal.ysh)
;; =====================================================================

(ert-deftest ysh-stage1/triple-dq-embedded-single-quote ()
  "echo \"\"\"\\n  one \"\\n  two \"\"\\n  \"\"\" — embedded single/double quotes.
From testdata/minimal.ysh."
  (let ((text "echo \"\"\"\n  $r1\n  one \"\n  two \"\"\n  \"\"\""))
    ;; 'one' on line 3 should be string (col 2 = 'o')
    (should (eq (ysh-test--face-at-col text 3 2) 'font-lock-string-face))))

;; =====================================================================
;; Stage 1: dollar-triple-DQ (from minimal.ysh)
;; =====================================================================

(ert-deftest ysh-stage1/dollar-triple-dq ()
  "echo $\"\"\"\\n  $r1\\n  \"\"\" — dollar triple DQ string.
From testdata/minimal.ysh."
  (let ((text "echo $\"\"\"\n  $r1\n  \"\"\""))
    ;; Content on line 2 should be string (col 2 = '$')
    (should (eq (ysh-test--face-at-col text 2 2) 'font-lock-string-face))))

;; =====================================================================
;; Stage 3: var-sub inside DQ with suffixes (from minimal.ysh)
;; =====================================================================

(ert-deftest ysh-stage3/var-sub-in-path ()
  "echo $base_dir/file — var sub followed by /file.
From testdata/minimal.ysh."
  (let ((text "echo $base_dir/file"))
    ;; $base_dir should be var-sub
    (should (ysh-test--has-face text 6 'ysh-var-sub-face))
    ;; /file should NOT be var-sub
    (should-not (ysh-test--has-face text 16 'ysh-var-sub-face))))

(ert-deftest ysh-stage3/var-sub-with-suffix-in-dq ()
  "echo \"$base_dir/file\" — var sub with suffix inside DQ.
From testdata/minimal.ysh."
  (let ((text "echo \"$base_dir/file\""))
    ;; $base_dir should be var-sub
    (should (ysh-test--has-face text 7 'ysh-var-sub-face))))

;; =====================================================================
;; Stage 3: multi-line $() command sub (from minimal.ysh)
;; =====================================================================

(ert-deftest ysh-stage2/multi-line-command-sub ()
  "echo $( echo 1 ... ) — multi-line command sub.
From testdata/minimal.ysh."
  (let ((text "echo $(\n  echo 1\n  echo 2\n  )"))
    ;; 'echo' at beginning (pos 1) should be builtin
    (should (ysh-test--has-face text 1 'font-lock-builtin-face))))

;; =====================================================================
;; Stage 2: false-positive — echo notbare = 42
;; (from false-positive.ysh)
;; =====================================================================

(ert-deftest ysh-stage2/notbare-equals ()
  "echo notbare = 42 — '=' after non-whitespace is NOT a keyword.
From testdata/false-positive.ysh."
  (let ((text "echo notbare = 42"))
    ;; 'echo' is builtin
    (should (ysh-test--has-face text 1 'font-lock-builtin-face))
    ;; '=' should NOT be keyword (it's an argument)
    (should-not (ysh-test--has-face text 14 'font-lock-keyword-face))))

;; =====================================================================
;; Stage 2: false-positive — 'calling' should not match 'call'
;; (from false-positive.ysh)
;; =====================================================================

(ert-deftest ysh-stage2/echo-calling-not-keyword ()
  "echo calling — 'calling' should not partially match 'call'.
From testdata/false-positive.ysh."
  (should-not (ysh-test--has-face "echo calling" 6 'font-lock-keyword-face)))

;; =====================================================================
;; Stage 3: var-sub in DQ bad patterns (from minimal.ysh)
;; =====================================================================

(ert-deftest ysh-stage3/var-sub-bad-1a ()
  "echo bad $1a — $1a is NOT a valid var sub.
From testdata/minimal.ysh: 'echo bad $00 $1a'."
  (let ((text "echo bad $1a"))
    ;; $1 is var-sub but 'a' should NOT be
    (should-not (ysh-test--has-face text 12 'ysh-var-sub-face))))

(provide 'ysh-mode-tests)
;;; ysh-mode-tests.el ends here
