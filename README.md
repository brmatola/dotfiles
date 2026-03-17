# Dotfiles

Personal configuration for macOS development environment.

## Quick Start

```bash
git clone https://github.com/brmatola/dotfiles.git ~/dotfiles
cd ~/dotfiles
./install.sh
```

## What's Included

- **zsh** - Shell configuration (zshenv, zprofile, zshrc)
- **git** - Git config with conditional identity for twiglylabs
- **claude** - Claude Code settings, skills, agents, and commands
- **vscode** - VS Code settings, keybindings, and extensions
- **Brewfile** - All Homebrew packages (formulae and casks)
- **macos/** - macOS system preferences
- **dock/** - Dock layout via dockutil
- **k8s/** - Local Kubernetes (Colima + Kind + registry)

## Structure

```
dotfiles/
├── install.sh          # Main installer
├── Brewfile            # Homebrew packages
├── npm-globals.txt     # Global npm packages
├── macos/
│   ├── defaults.sh     # macOS preferences
│   └── widgets.md      # Widget layout (manual)
├── dock/
│   └── setup.sh        # Dock layout
├── zsh/
│   ├── zshenv          # EDITOR/VISUAL
│   ├── zprofile         # Homebrew, PATH
│   └── zshrc           # nvm, completions
├── git/
│   ├── gitconfig
│   └── gitconfig-twiglylabs
├── k8s/
│   ├── setup.sh        # Colima + Kind + registry
│   ├── colima.yaml
│   └── kind-cluster.yaml
├── vscode/
│   ├── settings.json
│   ├── keybindings.json
│   └── extensions.txt
└── claude/
    ├── CLAUDE.md
    ├── settings.json
    ├── keybindings.json
    ├── skills/
    ├── agents/
    └── commands/
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
