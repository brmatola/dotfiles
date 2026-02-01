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
