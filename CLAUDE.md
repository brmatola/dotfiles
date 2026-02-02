# Dotfiles

Last verified: 2026-02-01

## Purpose

This repository is the **single source of truth** for my computer configuration. Everything lives here; target locations get symlinks.

## Core Principle

**Edit here, not there.** Never edit config files directly in `~/.config/`, `~/.*`, etc. Edit in this repo, commit, and the symlinks handle the rest.

## Commands

```bash
./install.sh              # Full setup: brew, symlinks, doom sync
brew bundle dump --file=Brewfile --force  # Capture new brew packages
doom sync                 # After changing Doom config
```

## Structure

| Directory | Symlinked To | Purpose |
|-----------|--------------|---------|
| `zsh/` | `~/.zshrc`, `~/.zprofile`, `~/.zshenv` | Shell config |
| `git/` | `~/.gitconfig` | Git config |
| `emacs/doom/` | `~/.config/doom/` | Doom Emacs config |
| `claude/` | `~/.claude/` | Claude Code settings & instructions |
| `macos/` | (not symlinked) | macOS defaults scripts |
| `plans/` | (not symlinked) | Implementation plans |

## Adding New Configs

1. Create the config file in this repo under appropriate directory
2. Add symlink creation to `install.sh`
3. Run `./install.sh` to create symlink
4. Commit both the config and install.sh changes

## Conventions

- **Flat where possible** - Prefer `zsh/zshrc` over `zsh/config/main.zsh`
- **Match target names** - File in repo should match what it becomes (minus dot prefix)
- **Brewfile is authoritative** - All brew packages go here, not installed ad-hoc
- **Plans track work** - `plans/active/` for in-progress, `plans/complete/` for done

## Key Files

- `install.sh` - The installer; also documents what gets symlinked where
- `Brewfile` - All Homebrew packages (formulae and casks)
- `claude/CLAUDE.md` - My global Claude instructions (becomes `~/.claude/CLAUDE.md`)

## Boundaries

- **Safe to add**: New config directories, new symlinks in install.sh
- **Update carefully**: Existing configs (test changes work)
- **Never commit**: Secrets, API keys, tokens (use env vars or separate secure storage)

---

## Emacs Lisp Development

Development standards for the Claude module and other Emacs Lisp code.

### Testing

```bash
# Run all tests
./emacs/doom/modules/claude/test/run-tests.sh

# Run individual test file
cd emacs/doom/modules/claude
emacs --batch -l ert -l claude-state.el -l test/claude-state-test.el -f ert-run-tests-batch-and-exit
```

### Linting

```bash
# Check for byte-compile warnings, missing headers, etc.
./emacs/doom/modules/claude/test/lint.sh
```

### Module Structure

The Claude module has a strict dependency order:

```
claude-state.el      # No deps - state machine, metadata, naming
    ↓
claude-vterm.el      # Depends: claude-state
claude-worktree.el   # Depends: claude-state
    ↓
claude-reconcile.el  # Depends: claude-state, claude-vterm
    ↓
claude-workspace.el  # Depends: claude-state, claude-vterm, claude-worktree
claude-monitor.el    # Depends: claude-state
claude-dashboard.el  # Depends: claude-state
claude-cleanup.el    # Depends: claude-state, claude-vterm
    ↓
claude.el            # Entry point - requires all above
```

### Batch vs Doom

| Can run in batch mode | Requires Doom/vterm |
|-----------------------|---------------------|
| `claude-state.el` | `claude-workspace.el` |
| `claude-worktree.el` | `claude-monitor.el` (modeline) |
| `claude-vterm.el` (with stubs) | `claude-dashboard.el` |
| `claude-reconcile.el` (with stubs) | `claude-cleanup.el` |

Tests use stubs for Doom workspace functions (`+workspace-exists-p`, etc.) and vterm functions to run in batch mode.

### Coding Standards

- **lexical-binding**: Every file must have `;;; file.el --- Desc -*- lexical-binding: t; -*-`
- **provides**: Every file must end with `(provide 'filename)`
- **docstrings**: Use `` `symbol' `` not `'symbol'` for symbols in docstrings
- **line width**: Keep docstrings under 80 characters
- **declare-function**: Use for functions from other packages loaded at runtime

### Writing Tests

```elisp
;; test/my-test.el
(require 'ert)
(add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name)))

;; Set up test environment
(defvar claude-worktree-dir "/tmp/test-worktrees")
(defvar claude-metadata-dir "/tmp/test-metadata")

(require 'claude-state)

(ert-deftest my-test-case ()
  "Description of what this tests."
  (let ((claude-metadata-dir (make-temp-file "test" t)))
    (unwind-protect
        (progn
          ;; Test code here
          (should (equal expected actual)))
      ;; Cleanup
      (delete-directory claude-metadata-dir t))))
```
