# Dotfiles

Personal configuration for macOS development environment.

## Quick Start

```bash
git clone https://github.com/YOUR_USERNAME/dotfiles.git ~/dotfiles
cd ~/dotfiles
./install.sh
```

## What's Included

- **zsh** - Shell configuration
- **git** - Git config
- **emacs/doom** - Doom Emacs config
- **claude** - Claude Code settings and instructions
- **Brewfile** - Homebrew packages
- **macos/** - macOS system preferences

## Structure

```
dotfiles/
├── install.sh          # Main installer
├── Brewfile            # Homebrew packages
├── macos/
│   └── defaults.sh     # macOS preferences
├── zsh/
│   ├── zshrc
│   ├── zprofile
│   └── zshenv
├── git/
│   └── gitconfig
├── emacs/
│   └── doom/
│       ├── config.el
│       ├── init.el
│       └── packages.el
└── claude/
    ├── CLAUDE.md
    └── settings.json
```

## Updating

After making changes to configs in `~/dotfiles/`, just commit and push:

```bash
cd ~/dotfiles
git add -A
git commit -m "Update config"
git push
```

On another machine, pull and the symlinks will pick up changes automatically:

```bash
cd ~/dotfiles
git pull
```

If you've added new Homebrew packages:

```bash
brew bundle dump --file=~/dotfiles/Brewfile --force
```
