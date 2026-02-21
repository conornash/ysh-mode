;;; ysh-ts-mode.el --- Tree-sitter support for YSH  -*- lexical-binding: t; -*-

;; Author: Conor Nash
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: languages, ysh, oils, shell
;; URL: https://github.com/conornash/ysh-mode

;;; Commentary:
;;
;; Tree-sitter-based major mode for YSH (the shell language from the Oils
;; project, https://www.oilshell.org/).
;;
;; Requires the tree-sitter-ysh grammar:
;;   https://github.com/danyspin97/tree-sitter-ysh
;;
;; This is the tree-sitter counterpart to `ysh-mode' (font-lock-based).
;; Like Python's `python-mode' and `python-ts-mode', both modes coexist:
;; `ysh-mode' works on any Emacs 27.1+, `ysh-ts-mode' requires Emacs 29+
;; with tree-sitter and a compiled grammar.

;;; Code:

(require 'treesit)

(declare-function treesit-parser-create "treesit.c")

;; ---------------------------------------------------------------------
;; Custom faces (shared concept with ysh-mode)
;; ---------------------------------------------------------------------

;; We use standard Emacs faces where possible so themes work out of the box.
;; ysh-mode defines custom faces for var-sub, backslash, etc; ysh-ts-mode
;; can use those when ysh-mode is loaded, otherwise falls back to builtins.

;; ---------------------------------------------------------------------
;; Font-lock rules
;; ---------------------------------------------------------------------

(defvar ysh-ts-mode--font-lock-settings
  (treesit-font-lock-rules

   ;; --- Level 1: comments ---
   :language 'ysh
   :feature 'comment
   '((comment) @font-lock-comment-face)

   ;; --- Level 1: strings ---
   :language 'ysh
   :feature 'string
   '((string) @font-lock-string-face)

   ;; --- Level 1: keywords ---
   :language 'ysh
   :feature 'keyword
   '(["var" "setvar" "const" "setglobal" "call"
      "for" "in" "while"
      "if" "elif" "else" "case"
      "func" "proc" "typed"
      "return"
      "and" "or" "not" "is"]
     @font-lock-keyword-face)

   ;; --- Level 2: definitions ---
   :language 'ysh
   :feature 'definition
   '((function_definition (function_name) @font-lock-function-name-face)
     (proc_definition (proc_name) @font-lock-function-name-face))

   ;; --- Level 2: commands ---
   :language 'ysh
   :feature 'command
   :override t
   '((command_call (command_name) @font-lock-function-call-face))

   ;; --- Level 2: builtins ---
   :language 'ysh
   :feature 'builtin
   :override t
   '((command_call
      (command_name) @font-lock-builtin-face
      (:match "\\`\\(?:echo\\|write\\|read\\|cd\\|pushd\\|popd\\|source\\|use\\|shopt\\|exit\\|assert\\|try\\|boolstatus\\|json\\|pp\\|type\\|append\\|hay\\|haynode\\|fork\\|forkwait\\|runproc\\|invoke\\|shvar\\|ctx\\|test\\|exec\\|command\\|builtin\\|true\\|false\\)\\'" @font-lock-builtin-face)))

   ;; --- Level 2: variables ---
   :language 'ysh
   :feature 'variable
   '((variable_name) @font-lock-variable-name-face)

   ;; --- Level 2: numbers ---
   :language 'ysh
   :feature 'number
   '((number) @font-lock-number-face)

   ;; --- Level 3: escape sequences ---
   :language 'ysh
   :feature 'escape
   :override t
   '([(escape_sequence)
      (escape_special_characters)
      (escaped_double_quote)
      (escaped_newline)]
     @font-lock-escape-face)

   ;; --- Level 3: operators ---
   :language 'ysh
   :feature 'operator
   '(["=" "+=" "-=" "===" "<" ">" "<=" ">=" "&&" "||" "|" "+" "-" "*"
      "/" "**" "++" ":" "..." "." "->" "=>" ":-"]
     @font-lock-operator-face)

   ;; --- Level 3: punctuation ---
   :language 'ysh
   :feature 'punctuation
   '(["(" ")" "[" "]" "{" "}" "," ";" "$" "@"] @font-lock-punctuation-face))

  "Tree-sitter font-lock settings for `ysh-ts-mode'.")

;; ---------------------------------------------------------------------
;; Indentation
;; ---------------------------------------------------------------------

(defcustom ysh-ts-mode-indent-offset 2
  "Number of spaces for each indentation level in `ysh-ts-mode'."
  :type 'integer
  :group 'ysh)

(defvar ysh-ts-mode--indent-rules
  `((ysh
     ;; Closing delimiters align with parent
     ((match "}" "block") parent-bol 0)
     ((match "}" "proc_block") parent-bol 0)
     ((match "}" "func_block") parent-bol 0)

     ;; Block contents indented
     ((parent-is "block") parent-bol ysh-ts-mode-indent-offset)
     ((parent-is "proc_block") parent-bol ysh-ts-mode-indent-offset)
     ((parent-is "func_block") parent-bol ysh-ts-mode-indent-offset)

     ;; Compound statement bodies
     ((parent-is "if_statement") parent-bol ysh-ts-mode-indent-offset)
     ((parent-is "for_statement") parent-bol ysh-ts-mode-indent-offset)
     ((parent-is "while_statement") parent-bol ysh-ts-mode-indent-offset)
     ((parent-is "case_statement") parent-bol ysh-ts-mode-indent-offset)

     ;; Function/proc definitions
     ((parent-is "function_definition") parent-bol ysh-ts-mode-indent-offset)
     ((parent-is "proc_definition") parent-bol ysh-ts-mode-indent-offset)

     ;; Lists, argument lists
     ((parent-is "argument_list") parent-bol ysh-ts-mode-indent-offset)
     ((parent-is "list") parent-bol ysh-ts-mode-indent-offset)

     ;; Default
     (no-node parent-bol 0)))
  "Tree-sitter indentation rules for `ysh-ts-mode'.")

;; ---------------------------------------------------------------------
;; Imenu
;; ---------------------------------------------------------------------

(defvar ysh-ts-mode--imenu-settings
  '(("Proc" "\\`proc_definition\\'" nil
     (lambda (node) (treesit-node-text
                     (treesit-node-child-by-field-name node "name") t)))
    ("Func" "\\`function_definition\\'" nil
     (lambda (node) (treesit-node-text
                     (treesit-search-subtree node "function_name") t))))
  "Imenu settings for `ysh-ts-mode'.")

;; ---------------------------------------------------------------------
;; Navigation
;; ---------------------------------------------------------------------

(defvar ysh-ts-mode--sentence-nodes
  "\\`\\(?:variable_declaration\\|variable_assignment\\|command_call\\|expression_mode\\|if_statement\\|for_statement\\|while_statement\\|case_statement\\|proc_definition\\|function_definition\\)\\'"
  "Regexp matching tree-sitter nodes that count as sentences.")

(defvar ysh-ts-mode--defun-nodes
  "\\`\\(?:proc_definition\\|function_definition\\)\\'"
  "Regexp matching tree-sitter nodes that count as defuns.")

;; ---------------------------------------------------------------------
;; Mode definition
;; ---------------------------------------------------------------------

;;;###autoload
(define-derived-mode ysh-ts-mode prog-mode "YSH"
  "Major mode for editing YSH, powered by tree-sitter.

Requires the tree-sitter-ysh grammar from
https://github.com/danyspin97/tree-sitter-ysh

\\{ysh-ts-mode-map}"
  :group 'ysh
  :syntax-table nil

  (unless (treesit-ready-p 'ysh)
    (error "Tree-sitter ysh grammar is not available"))

  (treesit-parser-create 'ysh)

  ;; Font-lock
  (setq-local treesit-font-lock-settings ysh-ts-mode--font-lock-settings)
  (setq-local treesit-font-lock-feature-list
              '((comment string keyword)
                (definition command builtin variable number)
                (escape operator punctuation)
                ()))

  ;; Indentation
  (setq-local treesit-simple-indent-rules ysh-ts-mode--indent-rules)

  ;; Imenu
  (setq-local treesit-simple-imenu-settings ysh-ts-mode--imenu-settings)

  ;; Navigation
  (setq-local treesit-defun-type-regexp ysh-ts-mode--defun-nodes)
  (setq-local treesit-sentence-type-regexp ysh-ts-mode--sentence-nodes)

  ;; Comments
  (setq-local comment-start "# ")
  (setq-local comment-end "")

  (treesit-major-mode-setup))

;;; Auto-mode: ysh-ts-mode does NOT auto-claim .ysh files.
;;; Users who prefer tree-sitter can add to their init:
;;;   (add-to-list 'auto-mode-alist '("\\.ysh\\'" . ysh-ts-mode))

(provide 'ysh-ts-mode)
;;; ysh-ts-mode.el ends here
