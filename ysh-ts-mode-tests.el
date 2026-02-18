;;; ysh-ts-mode-tests.el --- Tests for ysh-ts-mode  -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT test suite for ysh-ts-mode (tree-sitter-based YSH highlighting).
;; These tests mirror the key ysh-mode tests, verifying that the tree-sitter
;; mode produces equivalent highlighting for the same inputs.
;;
;; Requires: Emacs 29+ with tree-sitter, libtree-sitter-ysh grammar installed.

;;; Code:

(require 'ert)
(require 'ysh-ts-mode)

;; ---------------------------------------------------------------------
;; Test helpers
;; ---------------------------------------------------------------------

(defun ysh-ts-test--face-at (text pos)
  "Fontify TEXT in ysh-ts-mode and return the face at 1-indexed POS."
  (with-temp-buffer
    (insert text)
    (ysh-ts-mode)
    (font-lock-ensure)
    (get-text-property pos 'face)))

(defun ysh-ts-test--face-is (face expected)
  "Return non-nil if FACE is or contains EXPECTED."
  (cond
   ((eq face expected) t)
   ((and (listp face) (memq expected face)) t)
   (nil)))

(defun ysh-ts-test--has-face (text pos expected)
  "Return non-nil if POS in fontified TEXT has EXPECTED face."
  (ysh-ts-test--face-is (ysh-ts-test--face-at text pos) expected))

;; Skip all tests if tree-sitter ysh grammar is not available
(defun ysh-ts-test--grammar-available-p ()
  "Return non-nil if tree-sitter ysh grammar is usable."
  (and (fboundp 'treesit-available-p)
       (treesit-available-p)
       (treesit-language-available-p 'ysh)))

;; ---------------------------------------------------------------------
;; Stage 1: Comments and strings
;; ---------------------------------------------------------------------

(ert-deftest ysh-ts/comment ()
  "# at beginning of line is a comment."
  (skip-unless (ysh-ts-test--grammar-available-p))
  (should (ysh-ts-test--has-face "# comment" 1 'font-lock-comment-face)))

(ert-deftest ysh-ts/dq-string ()
  "\"hello\" is a string."
  (skip-unless (ysh-ts-test--grammar-available-p))
  (should (ysh-ts-test--has-face "echo \"hello\"" 7 'font-lock-string-face)))

(ert-deftest ysh-ts/sq-string ()
  "'hello' is a string."
  (skip-unless (ysh-ts-test--grammar-available-p))
  (should (ysh-ts-test--has-face "echo 'hello'" 7 'font-lock-string-face)))

(ert-deftest ysh-ts/triple-sq-string ()
  "'''...''' is a multi-line string."
  (skip-unless (ysh-ts-test--grammar-available-p))
  (should (ysh-ts-test--has-face "var x = '''\nhello\n'''" 13
                                 'font-lock-string-face)))

;; ---------------------------------------------------------------------
;; Stage 2: Keywords and context
;; ---------------------------------------------------------------------

(ert-deftest ysh-ts/keyword-var ()
  "var is a keyword."
  (skip-unless (ysh-ts-test--grammar-available-p))
  (should (ysh-ts-test--has-face "var x = 42" 1 'font-lock-keyword-face)))

(ert-deftest ysh-ts/keyword-if ()
  "if is a keyword."
  (skip-unless (ysh-ts-test--grammar-available-p))
  (should (ysh-ts-test--has-face "if (true) { echo yes }" 1
                                 'font-lock-keyword-face)))

(ert-deftest ysh-ts/keyword-proc ()
  "proc is a keyword."
  (skip-unless (ysh-ts-test--grammar-available-p))
  (should (ysh-ts-test--has-face "proc p { echo hi }" 1
                                 'font-lock-keyword-face)))

(ert-deftest ysh-ts/keyword-func ()
  "func is a keyword."
  (skip-unless (ysh-ts-test--grammar-available-p))
  (should (ysh-ts-test--has-face "func f(x) { return (x) }" 1
                                 'font-lock-keyword-face)))

(ert-deftest ysh-ts/proc-name ()
  "proc name is highlighted."
  (skip-unless (ysh-ts-test--grammar-available-p))
  (should (ysh-ts-test--has-face "proc my-proc { echo hi }" 6
                                 'font-lock-function-name-face)))

(ert-deftest ysh-ts/func-name ()
  "func name is highlighted."
  (skip-unless (ysh-ts-test--grammar-available-p))
  (should (ysh-ts-test--has-face "func myFunc(x) { return (x) }" 6
                                 'font-lock-function-name-face)))

(ert-deftest ysh-ts/echo-for-not-keyword ()
  "In 'echo for', 'for' should NOT be a keyword — it's an argument."
  (skip-unless (ysh-ts-test--grammar-available-p))
  (should-not (ysh-ts-test--has-face "echo for" 6 'font-lock-keyword-face)))

(ert-deftest ysh-ts/nested-dq ()
  "Nested double quotes in $[...] — the canonical test."
  (skip-unless (ysh-ts-test--grammar-available-p))
  (let ((text "echo \"nested $[mydict[\"word\"]] quotes\""))
    ;; 'nested' should be string
    (should (ysh-ts-test--has-face text 7 'font-lock-string-face))))

;; ---------------------------------------------------------------------
;; Stage 3: Details
;; ---------------------------------------------------------------------

(ert-deftest ysh-ts/number ()
  "42 is a number."
  (skip-unless (ysh-ts-test--grammar-available-p))
  (should (ysh-ts-test--has-face "var x = 42" 9 'font-lock-number-face)))

(ert-deftest ysh-ts/command-name ()
  "echo is a command name — should get some face."
  (skip-unless (ysh-ts-test--grammar-available-p))
  (let ((face (ysh-ts-test--face-at "echo hi" 1)))
    (should face)))  ; any face, not nil

;; ---------------------------------------------------------------------
;; Integration: testdata files parse without ERROR nodes
;; ---------------------------------------------------------------------

(defvar ysh-ts-test--testdata-dir
  (expand-file-name "testdata"
                    (file-name-directory (or load-file-name
                                             buffer-file-name
                                             default-directory)))
  "Path to testdata/ directory.")

(defun ysh-ts-test--count-errors (filename)
  "Parse FILENAME with tree-sitter ysh and return count of ERROR nodes."
  (with-temp-buffer
    (insert-file-contents (expand-file-name filename ysh-ts-test--testdata-dir))
    (let* ((parser (treesit-parser-create 'ysh))
           (root (treesit-parser-root-node parser))
           (errors 0))
      (treesit-search-subtree root
        (lambda (node)
          (when (string= (treesit-node-type node) "ERROR")
            (setq errors (1+ errors)))
          nil))
      errors)))

(ert-deftest ysh-ts/parse-nested-dq ()
  "testdata/nested-double-quotes.ysh parses with zero ERROR nodes."
  (skip-unless (ysh-ts-test--grammar-available-p))
  (should (= 0 (ysh-ts-test--count-errors "nested-double-quotes.ysh"))))

(provide 'ysh-ts-mode-tests)
;;; ysh-ts-mode-tests.el ends here
