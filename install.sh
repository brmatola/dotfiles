#!/bin/bash
set -e

DOTFILES_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
echo "Installing dotfiles from: $DOTFILES_DIR"

###############################################################################
# Xcode Command Line Tools                                                    #
###############################################################################

if ! xcode-select -p &>/dev/null; then
    echo "Installing Xcode Command Line Tools..."
    xcode-select --install
    echo "Please complete the Xcode installation and re-run this script."
    exit 1
fi

###############################################################################
# Homebrew                                                                    #
###############################################################################

if ! command -v brew &>/dev/null; then
    echo "Installing Homebrew..."
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

    # Add brew to path for this session
    eval "$(/opt/homebrew/bin/brew shellenv)"
fi

echo "Updating Homebrew and installing packages..."
brew update
brew bundle --file="$DOTFILES_DIR/Brewfile"

###############################################################################
# Symlinks                                                                    #
###############################################################################

create_symlink() {
    local src="$1"
    local dest="$2"

    if [ -e "$dest" ] && [ ! -L "$dest" ]; then
        echo "Backing up existing $dest to ${dest}.backup"
        mv "$dest" "${dest}.backup"
    fi

    if [ -L "$dest" ]; then
        rm "$dest"
    fi

    mkdir -p "$(dirname "$dest")"
    ln -s "$src" "$dest"
    echo "Linked: $dest -> $src"
}

echo ""
echo "Creating symlinks..."

# Zsh
create_symlink "$DOTFILES_DIR/zsh/zshrc" "$HOME/.zshrc"
create_symlink "$DOTFILES_DIR/zsh/zprofile" "$HOME/.zprofile"
create_symlink "$DOTFILES_DIR/zsh/zshenv" "$HOME/.zshenv"

# Git
create_symlink "$DOTFILES_DIR/git/gitconfig" "$HOME/.gitconfig"
create_symlink "$DOTFILES_DIR/git/gitconfig-twiglylabs" "$HOME/.gitconfig-twiglylabs"

# Claude
mkdir -p "$HOME/.claude"
create_symlink "$DOTFILES_DIR/claude/CLAUDE.md" "$HOME/.claude/CLAUDE.md"
create_symlink "$DOTFILES_DIR/claude/settings.json" "$HOME/.claude/settings.json"
# Only link skills if directory has content
if [ -d "$DOTFILES_DIR/claude/skills" ] && [ "$(ls -A "$DOTFILES_DIR/claude/skills" 2>/dev/null)" ]; then
    create_symlink "$DOTFILES_DIR/claude/skills" "$HOME/.claude/skills"
fi
# Only link agents if directory has content
if [ -d "$DOTFILES_DIR/claude/agents" ] && [ "$(ls -A "$DOTFILES_DIR/claude/agents" 2>/dev/null)" ]; then
    create_symlink "$DOTFILES_DIR/claude/agents" "$HOME/.claude/agents"
fi
# Only link commands if directory has content
if [ -d "$DOTFILES_DIR/claude/commands" ] && [ "$(ls -A "$DOTFILES_DIR/claude/commands" 2>/dev/null)" ]; then
    create_symlink "$DOTFILES_DIR/claude/commands" "$HOME/.claude/commands"
fi

# VS Code
VSCODE_USER_DIR="$HOME/Library/Application Support/Code/User"
mkdir -p "$VSCODE_USER_DIR"
create_symlink "$DOTFILES_DIR/vscode/settings.json" "$VSCODE_USER_DIR/settings.json"
create_symlink "$DOTFILES_DIR/vscode/keybindings.json" "$VSCODE_USER_DIR/keybindings.json"

###############################################################################
# Claude Code CLI                                                             #
###############################################################################

if ! command -v claude &>/dev/null; then
    echo ""
    echo "Installing Claude Code CLI..."
    npm install -g @anthropic-ai/claude-code
else
    echo "Claude Code CLI already installed: $(claude --version 2>/dev/null || echo 'installed')"
fi

###############################################################################
# VS Code Extensions                                                          #
###############################################################################

if command -v code &>/dev/null && [ -f "$DOTFILES_DIR/vscode/extensions.txt" ]; then
    echo ""
    echo "Installing VS Code extensions..."
    while IFS= read -r ext; do
        [ -z "$ext" ] && continue
        code --install-extension "$ext" --force 2>/dev/null || echo "  Failed: $ext"
    done < "$DOTFILES_DIR/vscode/extensions.txt"
fi

###############################################################################
# nvm + Node                                                                  #
###############################################################################

export NVM_DIR="$HOME/.nvm"
if [ -s "/opt/homebrew/opt/nvm/nvm.sh" ]; then
    echo ""
    echo "Setting up nvm..."
    \. "/opt/homebrew/opt/nvm/nvm.sh"
    if ! nvm ls --no-colors 2>/dev/null | grep -q "lts"; then
        echo "Installing latest Node LTS..."
        nvm install --lts
    fi
    nvm alias default lts/*
fi

###############################################################################
# Global npm packages                                                         #
###############################################################################

if command -v npm &>/dev/null && [ -f "$DOTFILES_DIR/npm-globals.txt" ]; then
    echo ""
    echo "Installing global npm packages..."
    while IFS= read -r pkg; do
        [ -z "$pkg" ] && continue
        if ! npm list -g "$pkg" &>/dev/null; then
            echo "  Installing $pkg..."
            npm install -g "$pkg" || echo "  Failed: $pkg"
        else
            echo "  Already installed: $pkg"
        fi
    done < "$DOTFILES_DIR/npm-globals.txt"
fi

###############################################################################
# Local K8s Environment (Colima + Kind + Registry)                            #
###############################################################################

echo ""
echo "Setting up local Kubernetes environment..."
bash "$DOTFILES_DIR/k8s/setup.sh" setup

###############################################################################
# Ollama                                                                      #
###############################################################################

if command -v ollama &>/dev/null; then
    echo ""
    echo "Ollama installed. Start the service with: brew services start ollama"
fi

###############################################################################
# macOS Preferences                                                           #
###############################################################################

echo ""
read -p "Apply macOS preferences? (y/n) " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    bash "$DOTFILES_DIR/macos/defaults.sh"
fi

###############################################################################
# Dock Layout                                                                 #
###############################################################################

echo ""
read -p "Configure dock layout? (y/n) " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    bash "$DOTFILES_DIR/dock/setup.sh"
fi

###############################################################################
# Post-install Reminders                                                      #
###############################################################################

echo ""
echo "=========================================="
echo "  Installation complete!"
echo "=========================================="
echo ""
echo "Manual steps to complete:"
echo ""
echo "1. GitHub accounts (personal + twiglylabs)"
echo "   - gh auth login                          # personal account"
echo "   - gh auth login                          # twiglylabs account"
echo "   - gh auth setup-git                      # wire up git credential helper"
echo "   - gh auth switch --user <name>           # switch active account"
echo ""
echo "2. AWS Credentials"
echo "   - Copy ~/.aws from old machine, OR"
echo "   - Run: aws configure"
echo ""
echo "3. Git LFS"
echo "   - Run: git lfs install"
echo ""
echo "4. Claude Code Login"
echo "   - Run: claude login"
echo ""
echo "5. Mac App Store"
echo "   - Sign in to the App Store to install mas-managed apps"
echo ""
echo "6. Restart your terminal to apply shell changes"
echo ""
