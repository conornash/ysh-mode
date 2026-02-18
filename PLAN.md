# ysh-mode Development Plan

Rewrite ysh-mode from flat regex matching to an accurate, context-aware
highlighter implementing the three-stage coarse parsing algorithm from
[oils.vim](https://github.com/oils-for-unix/oils.vim/blob/main/doc/algorithms.md).

## Background

Andy Chu (YSH creator) reviewed the initial implementation and pointed to the
algorithms document describing three approaches to YSH syntax highlighting:

1. **Coarse Parsing** — regexes + a context stack (Vim, TextMate, Emacs)
2. **Context-Free Parsing** — Tree-sitter (resilient incremental CFG)
3. **Full Parsing** — `ysh --tool syntax-tree` (not useful for editors)

The key insight: YSH has **three mutually recursive sublanguages** — commands,
expressions, and double-quoted strings — which nest arbitrarily:

```
echo "nested $[mydict["word"]] quotes"
      ^string  ^expr   ^string  ^expr ^string
```

The coarse parsing algorithm breaks this into three stages, documented with
screenshots and test data at <https://pages.oils.pub/oils-vim/>.

### References Read

| Source | Content |
|--------|---------|
| [algorithms.md](https://github.com/oils-for-unix/oils.vim/blob/main/doc/algorithms.md) | Three-stage overview, comparisons |
| [stage1.md](https://github.com/oils-for-unix/oils.vim/blob/main/doc/stage1.md) | Comments + 5 string types (10 forms) |
| [stage2.md](https://github.com/oils-for-unix/oils.vim/blob/main/doc/stage2.md) | Recursive lexer modes — the hard part |
| [stage3.md](https://github.com/oils-for-unix/oils.vim/blob/main/doc/stage3.md) | Details within each mode |
| [lexer-modes.vim](https://github.com/oils-for-unix/oils.vim/blob/main/syntax/lexer-modes.vim) | `@dqMode`, `@commandMode`, `@exprMode` clusters |
| [lib-comment-string.vim](https://github.com/oils-for-unix/oils.vim/blob/main/syntax/lib-comment-string.vim) | String region definitions |
| [lib-command-expr-dq.vim](https://github.com/oils-for-unix/oils.vim/blob/main/syntax/lib-command-expr-dq.vim) | Mode transition rules |
| [lib-details.vim](https://github.com/oils-for-unix/oils.vim/blob/main/syntax/lib-details.vim) | Var subs, expression keywords, J8 escapes |
| [lib-regex.vim](https://github.com/oils-for-unix/oils.vim/blob/main/syntax/lib-regex.vim) | Shared regex building blocks |
| testdata/*.ysh | 6 test files covering each stage + edge cases |
| [tree-sitter-ysh](https://github.com/danyspin97/tree-sitter-ysh) | 873-line grammar.js + 210-line scanner.c |
| [ysh-ts-mode](https://github.com/tskinn/ysh-ts-mode) | Existing Emacs tree-sitter mode (~160 lines) |
| Emacs python.el | Reference for `syntax-propertize` triple-quote handling |
| Emacs sh-script.el | Reference for shell `syntax-propertize` with here-docs |

## Architecture

Two parallel modes, following the Python (`python-mode` + `python-ts-mode`) pattern:

| | `ysh-mode` (font-lock) | `ysh-ts-mode` (tree-sitter) |
|---|---|---|
| Engine | `syntax-propertize` + context-tracking fontification | `tree-sitter-ysh` grammar |
| Requires | Emacs 27.1+ | Emacs 29+ with tree-sitter |
| Accuracy | Coarse parsing (Stages 1–3) | Full structural parse |
| Dependency | None | tree-sitter runtime + compiled grammar |

Both modes share: faces, customization, indentation, navigation, keybindings.

## Test-Driven Development

All development follows red/green: failing tests are written first, then code
is changed to make them pass. The test suite (`ysh-mode-tests.el`) is
structured around the three stages and uses the oils.vim `testdata/` files as
the correctness reference.

Run tests:
```sh
emacs --batch -L . -l ysh-mode-tests.el -f ert-run-tests-batch-and-exit
```

## Phases

### Phase 1: Stage 1 — Comments and String Literals ✅

**Status: COMPLETE** (22/22 tests green)

Fixed `syntax-propertize` to correctly handle:

- **Comments**: Changed `#` from unconditional comment-starter in the syntax
  table to punctuation by default. `syntax-propertize` promotes `#` to comment
  only when preceded by whitespace, BOL, or metacharacters (`;|&`). Fixes
  `echo not#comment` false positive.

- **Single-quoted strings**: Emacs 31's `\@!` (negative lookahead) is
  non-functional. Replaced with separate handlers per string type:
  - J8 (`b'...'`, `u'...'`): Manual scan with backslash-escape skipping,
    `"` syntax so `\'` works
  - Raw (`r'...'`): Regex match, `"` syntax, `\` inside marked as
    punctuation to prevent escape
  - Plain (`'...'`): Same as raw

- **Triple-quoted strings**: Closing delimiter search uses `(point-max)`
  instead of `end` to handle JIT-lock sub-regions. Region extension hook
  extends backward when `start` is mid-string.

- **Earlier-pass protection**: Each `syntax-propertize` pass checks
  `get-text-property` before overwriting properties set by earlier passes
  (prevents triple-quote delimiters from being clobbered by the plain
  single-quote handler).

Key discovery: Emacs 31.0.50 has completely non-functional `\@!` and `\@=`
regex assertions — even the manual's own examples fail. All code avoids these.

### Phase 2: Stage 2 — Recursive Lexer Modes 🔴

**Status: 4 failing tests**

| Test | Issue |
|------|-------|
| `echo-for-not-keyword` | `for` highlighted as keyword in `echo for` |
| `echo-and-not-keyword` | `and` highlighted as keyword in `echo and` |
| `nested-dq-in-expr-sub` | Inner `"` in `$[mydict["word"]]` breaks outer string |
| `nested-dq-deep` | Inner `"` in `$["inner"]` breaks outer string |

This is the hardest phase — implementing the recursive mode tracking that
Vim expresses with `syn cluster` + `syn region contains=`.

**Approach**: A linear scanner function that walks the buffer maintaining a
mode stack. Translating from `lib-command-expr-dq.vim`:

| Trigger | From → To | Vim equivalent |
|---------|-----------|----------------|
| `var const setvar setglobal call` first-word | command → expression (until `;`/`\n`/`#`) | `exprAfterKeyword` |
| `= ` first-word | command → expression | `equalsRegex` |
| `proc func` first-word | command → signature | `nextgroup=procName` |
| `$[` | any → expression (until `]`) | `exprSub` |
| `@[` | any → expression (until `]`) | `exprSplice` |
| `^[` | any → expression (until `]`) | `caretExpr` |
| `$(` | any → command (until `)`) | `commandSub` |
| `@(` | any → command (until `)`) | `commandSplice` |
| `^(` | any → command (until `)`) | `caretCommand` |
| `"` | command/expression → dq-string (until `"`) | `dqString` |
| `$"` | any → dq-string (until `"`) | `dollarDqString` |
| `(` | expression → nested-expression (until `)`) | `nestedParen` |
| `[` | expression → nested-expression (until `]`) | `nestedBracket` |
| `{` | expression → nested-expression (until `}`) | `nestedBrace` |
| `:\|` | expression → array-mode (until `\|`) | `yshArrayLiteral` |
| ` (` space-paren | command → expression (until `)`) | `spaceParen` |
| ` [` space-bracket | command → expression (until `]`) | `lazyTypedArgs` |

The scanner applies a `ysh-context` text property (`command`/`expression`/
`dq-string`) to each region. Font-lock keyword matchers then check this
property to decide whether to apply faces.

### Phase 3: Stage 3 — Details Within Each Mode 🔴

**Status: 5 failing tests**

| Test | Issue |
|------|-------|
| `var-sub-number` | `$0` not getting var-sub face (`$` has expression-prefix syntax) |
| `var-sub-braced-number` | `${11}` not getting var-sub face |
| `var-sub-in-dq` | `$name` inside `"..."` not highlighted as var-sub |
| `backslash-in-command` | `\{` not getting backslash face |
| `backslash-dollar` | `\$name` — the `$` still gets var-sub face despite `\` |

These require fixing font-lock keyword interactions with the syntax layer:

- **Variable substitutions** (`$name`, `${name}`, `$0`, `${12}`): The `$` has
  syntax class `'` (expression prefix) in the syntax table, which may interfere
  with regex matching. Need to ensure font-lock rules match correctly and apply
  inside double-quoted strings but not single-quoted strings.

- **Backslash-quoted characters**: `\$` should prevent `$name` from being
  treated as a substitution. The backslash face and the var-sub face need
  correct priority ordering in font-lock-keywords.

- **Context-dependent backslash meaning** (future):
  - Commands: `\# \' \" \$ \@ \( \) \{ \} \\ \[ \]`
  - DQ strings: `\$ \" \\` (NOT `\n`)
  - J8 strings: `\n \t \' \\ \yff \u{...}`
  - Expressions: `\n` is a valid newline

### Phase 4: Test Suite & Validation

- [ ] Batch test runner comparing face output against oils.vim HTML
- [ ] `htmlfontify-buffer` export for visual comparison with
      <https://pages.oils.pub/oils-vim/>
- [ ] Regression test with real-world files (wrens-iterate.ysh)

### Phase 5: ysh-ts-mode (Tree-sitter, parallel track)

- [ ] Evaluate `tree-sitter-ysh` grammar accuracy against testdata
- [ ] Improve `ysh-ts-mode` font-lock rules using tree-sitter node types
- [ ] Share infrastructure via `ysh-base-mode` (faces, indent, nav, imenu)
- [ ] `syntax-propertize` for tree-sitter mode (needed for `forward-sexp` etc.)

## File Structure

```
ysh-mode/
├── PLAN.md              ← this file
├── README.org
├── ysh-mode.el          # Font-lock mode (Stages 1–3 coarse parsing)
├── ysh-mode-tests.el    # ERT test suite (72 tests)
├── testdata/            # Copied from oils.vim
│   ├── minimal.ysh
│   ├── recursive-modes.ysh
│   ├── details.ysh
│   ├── false-positive.ysh
│   ├── false-negative.ysh
│   └── nested-double-quotes.ysh
└── (future)
    ├── ysh-base.el      # Shared faces, customization, indent, nav
    └── ysh-ts-mode.el   # Tree-sitter mode
```

## Test Scorecard

| Stage | Tests | Pass | Fail | Notes |
|-------|-------|------|------|-------|
| Stage 1 (comments + strings) | 22 | 22 | 0 | ✅ Complete |
| Stage 2 (recursive modes) | 18 | 14 | 4 | Needs mode-tracking scanner |
| Stage 3 (mode details) | 18 | 13 | 5 | Needs font-lock keyword fixes |
| Integration | 4 | 4 | 0 | ✅ |
| **Total** | **72** | **63** | **9** | |
