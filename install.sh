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
# Post-install Reminders                                                      #
###############################################################################

echo ""
echo "=========================================="
echo "  Installation complete!"
echo "=========================================="
echo ""
echo "Manual steps to complete:"
echo ""
echo "1. SSH Keys"
echo "   - Copy ~/.ssh from old machine, OR"
echo "   - Generate new: ssh-keygen -t ed25519 -C \"your@email.com\""
echo "   - Add to GitHub: https://github.com/settings/keys"
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
