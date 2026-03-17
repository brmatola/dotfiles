#!/bin/bash
set -e
trap 'echo "ERROR: Script failed at line $LINENO" >&2' ERR

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

    if [ ! -e "$src" ] && [ ! -d "$src" ]; then
        echo "WARNING: source does not exist: $src"
        return
    fi

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
create_symlink "$DOTFILES_DIR/claude/keybindings.json" "$HOME/.claude/keybindings.json"
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

# SSH
mkdir -p "$HOME/.ssh"
chmod 700 "$HOME/.ssh"
create_symlink "$DOTFILES_DIR/ssh/config" "$HOME/.ssh/config"

# VS Code
VSCODE_USER_DIR="$HOME/Library/Application Support/Code/User"
mkdir -p "$VSCODE_USER_DIR"
create_symlink "$DOTFILES_DIR/vscode/settings.json" "$VSCODE_USER_DIR/settings.json"
create_symlink "$DOTFILES_DIR/vscode/keybindings.json" "$VSCODE_USER_DIR/keybindings.json"

###############################################################################
# Git LFS                                                                     #
###############################################################################

echo ""
echo "Setting up Git LFS..."
git lfs install

###############################################################################
# Rust (rustup)                                                               #
###############################################################################

if ! command -v rustup &>/dev/null; then
    if command -v rustup-init &>/dev/null; then
        echo ""
        echo "Installing Rust toolchain..."
        rustup-init -y --no-modify-path
        [ -f "$HOME/.cargo/env" ] && . "$HOME/.cargo/env"
    fi
else
    echo "Rust toolchain already installed: $(rustc --version 2>/dev/null || echo 'installed')"
fi

###############################################################################
# nvm + Node                                                                  #
###############################################################################

export NVM_DIR="$HOME/.nvm"
if [ -s "/opt/homebrew/opt/nvm/nvm.sh" ]; then
    echo ""
    echo "Setting up nvm..."
    \. "/opt/homebrew/opt/nvm/nvm.sh"
    echo "Installing latest Node LTS..."
    nvm install --lts
    nvm alias default lts/*

    # Enable corepack so pnpm/yarn are managed per-project (survives nvm use)
    echo "Enabling corepack (pnpm, yarn)..."
    corepack enable
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
# Claude Code CLI                                                             #
###############################################################################

if ! command -v claude &>/dev/null; then
    if command -v npm &>/dev/null; then
        echo ""
        echo "Installing Claude Code CLI..."
        npm install -g @anthropic-ai/claude-code
    else
        echo "WARNING: npm not available, skipping Claude Code CLI install"
    fi
else
    echo "Claude Code CLI already installed: $(claude --version 2>/dev/null || echo 'installed')"
fi

###############################################################################
# VS Code Extensions                                                          #
###############################################################################

hash -r  # Refresh command cache after brew installs
export PATH="$PATH:/Applications/Visual Studio Code.app/Contents/Resources/app/bin"
if command -v code &>/dev/null && [ -f "$DOTFILES_DIR/vscode/extensions.txt" ]; then
    echo ""
    echo "Installing VS Code extensions..."
    while IFS= read -r ext; do
        [ -z "$ext" ] && continue
        code --install-extension "$ext" --force 2>/dev/null || echo "  Failed: $ext"
    done < "$DOTFILES_DIR/vscode/extensions.txt"
fi

###############################################################################
# Local K8s Environment (Colima + Kind + Registry)                            #
###############################################################################

echo ""
read -p "Set up local Kubernetes environment (Colima + Kind)? (y/n) " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    bash "$DOTFILES_DIR/k8s/setup.sh" setup
fi

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
echo "1. SSH Key"
echo "   - ssh-keygen -t ed25519 -C \"brmatola@gmail.com\""
echo "   - eval \"\$(ssh-agent -s)\" && ssh-add ~/.ssh/id_ed25519"
echo "   - Add public key to GitHub: https://github.com/settings/keys"
echo ""
echo "2. GitHub accounts (personal + twiglylabs)"
echo "   - gh auth login                          # personal account"
echo "   - gh auth login                          # twiglylabs account"
echo "   - gh auth setup-git                      # wire up git credential helper"
echo "   - gh auth switch --user <name>           # switch active account"
echo ""
echo "3. AWS Credentials"
echo "   - Copy ~/.aws from old machine, OR"
echo "   - Run: aws configure"
echo ""
echo "4. Claude Code Login"
echo "   - Run: claude login"
echo ""
echo "5. Mac App Store"
echo "   - Sign in to the App Store, then run: brew bundle --file=~/dotfiles/Brewfile"
echo "   - This will install mas-managed apps (Things 3, etc.)"
echo ""
echo "6. Twiglylabs Tooling"
echo "   - mkdir -p ~/repos/twiglylabs/tooling"
echo "   - Clone: grove, trellis, sap, bark, canopy"
echo "   - cd ~/repos/twiglylabs/tooling && pnpm install"
echo ""
echo "7. Restart your terminal to apply shell changes"
echo ""
