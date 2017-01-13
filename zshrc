source ~/dotfiles/antigen/antigen.zsh
# Load the oh-my-zsh library
antigen use oh-my-zsh
# Bundles from the default repo (robbyrussell's oh-my-zsh)
antigen bundle git
antigen bundle pip
antigen bundle git
antigen bundle npm
antigen bundle autopep8
# Syntax Highlighting Bundle
antigen bundle zsh-users/zsh-syntax-highlighting
# Themeing
antigen theme https://github.com/caiogondim/bullet-train-oh-my-zsh-theme bullet-train
BULLETTRAIN_VIRTUALENV_FG=black

# Use rbenv and pyenv
eval "$(pyenv init -)"

# Set default editor to nvim
export EDITOR='nvim'
export PATH=$PATH:/usr/local/dart-sdk/bin:/Users/brmatola/.pub-cache/bin
export PATH=$PATH:/Users/brmatola/.cargo/bin
export EVENT_NOKQUEUE=1

# Alias for neovim
alias nv='nvim'
# Aliases for Tmux
alias tmux='tmux -2'
alias ta='tmux attach -t'
alias tnew='tmux new -s'
alias tls='tmux ls'
alias tkill='tmux kill-session -t'
# Aliases for editing configs
alias ev='nv ~/.vimrc'
alias et='nv ~/.tmux.conf'
alias ez='nv ~/.zshrc'
alias en='nv ~/.config/nvim/init.vim'
# quick directory movement
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
# Alias for simple compiling
alias c++='c++ -std=c++11'
# Other aliases
alias cl='clear'
alias rmrf='rm -rf'

# Path to global python (3.5) installation
globalpyinstall=/opt/local/Library/Frameworks/Python.framework/Versions/3.5


# Tell antigen that you're done
antigen apply

function mkcd
{
    dir="$*"
        mkdir -p "$dir" && cd "$dir";
}
