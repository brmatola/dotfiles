# Dotfiles

Last verified: 2026-02-14

## Purpose

This repository is the **single source of truth** for my computer configuration. Everything lives here; target locations get symlinks.

## Core Principle

**Edit here, not there.** Never edit config files directly in `~/.config/`, `~/.*`, etc. Edit in this repo, commit, and the symlinks handle the rest.

## Commands

```bash
npm test                  # Run all Emacs module tests
npm run lint              # Lint Emacs module elisp files
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

### Testing & Linting

```bash
npm test          # Run all tests
npm run lint      # Byte-compile checks, missing headers, etc.
```

### Module Structure

The Claude module delegates workspace lifecycle to grove CLI:

```
claude-grove.el      # No deps — async grove CLI wrapper
    ↓
claude-monitor.el    # No deps — attention detection (vterm buffer output)
    ↓
claude-dashboard.el  # Depends: claude-grove, claude-monitor
                     # Mission control: rendering, Doom workspaces, vterm, actions
    ↓
claude.el            # Entry point — requires all above, global keybinding (SPC ;)
```

### Batch vs Doom

| Can run in batch mode | Requires Doom/vterm |
|-----------------------|---------------------|
| `claude-grove.el` | `claude-dashboard.el` (actions) |
| `claude-monitor.el` | `claude.el` (keybindings) |

Tests use stubs for Doom workspace functions (`+workspace-exists-p`, etc.) and vterm functions to run in batch mode.

### External Dependencies

- **grove CLI** — workspace and repo management (`npm install -g @twiglylabs/grove`)

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

;; Stub Doom workspace functions
(unless (fboundp '+workspace-exists-p)
  (defun +workspace-exists-p (_name) nil))

(require 'claude-grove)
(require 'claude-monitor)

(ert-deftest my-test-case ()
  "Description of what this tests."
  ;; Test code here
  (should (equal expected actual)))
```
