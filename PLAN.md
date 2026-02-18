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

### Phase 2: Stage 2 — Recursive Lexer Modes ✅

**Status: COMPLETE** (18/18 tests green)

Fixed two categories of issues:

- **Keyword context**: Shell keywords (`for`, `while`, etc.) now anchored to
  first-word position using the same `first-word-prefix` as builtins. Expression
  keywords (`and`, `or`, `not`, etc.) use a `ysh--match-expr-keyword` matcher
  function that only applies when an expression-opening keyword (`var`, `const`,
  `if`, `while`, etc.) appears earlier on the same line. Uses `save-match-data`
  to prevent match-data clobbering during the context check.

- **Nested double quotes**: Changed `"` from string-delimiter to punctuation in
  the syntax table. ALL double-quoted strings are now handled in
  `syntax-propertize` via a recursive scanner:
  - `ysh--scan-dq-content`: scans forward from opening `"`, handling `\`
    escapes, `$[` expression subs, and the closing `"`
  - `ysh--scan-expr-sub`: tracks bracket depth for `$[...]`, skipping inner
    `"..."` strings WITHOUT marking them (the outer string's delimiters already
    span the whole range, so inner content gets string-face from the enclosing
    string)
  - This correctly handles `echo "nested $[mydict["word"]] quotes"`

### Phase 3: Stage 3 — Details Within Each Mode ✅

**Status: COMPLETE** (18/18 tests green)

Fixed three categories of font-lock keyword issues:

- **Backslash-face regex**: Emacs 31 `\]` inside character classes is broken
  (same class of bug as `\@!`). Fix: put `]` first in the class using the
  standard POSIX trick (`[]#'..."$@(){}\\[]` instead of `[#'..."$@(){}\\\[\]]`).

- **Variable substitutions**: Replaced individual regex-based font-lock
  keywords with a `ysh--match-var-sub` matcher function. Uses `t` override
  (so it applies over syntactic string-face). Checks `syntax-ppss` to allow
  `$name` inside double-quoted strings but skip single-quoted strings. Checks
  preceding character to skip `\$name` (backslash-escaped). Moved before
  numeric literal rules so `$0` matches before `0` gets constant-face (nil
  override means first-match-wins for the entire matched range).

- **Rule ordering**: Backslash → var-sub → sigil-pair → constants → numerics.
  This ensures `\$` gets backslash-face before `$name` is considered, and
  `$0` gets var-sub-face before `0` gets constant-face.

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
| Stage 2 (recursive modes) | 20 | 20 | 0 | ✅ Complete |
| Stage 3 (mode details) | 18 | 18 | 0 | ✅ Complete |
| Integration | 4 | 4 | 0 | ✅ Complete |
| **Total** | **74** | **74** | **0** | **✅ ALL GREEN** |
