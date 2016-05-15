source ~/dotfiles/antigen/antigen.zsh

# Setup Powerline
powerline-daemon -q
source /usr/local/lib/python3.5/site-packages/powerline/bindings/zsh/powerline.zsh

# Set default editor to vim
export EDITOR='vim'

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

# Aliases for Tmux
alias tmux='tmux -2'
alias ta='tmux attach -t'
alias tnew='tmux new -s'
alias tls='tmux ls'
alias tkill='tmux kill-session -t'

# Aliases for editing configs
alias ev='vim ~/.vimrc'
alias et='vim ~/.tmux.conf'
alias ez='vim ~/.zshrc'

# Alias for homebrew vim (python3 based)
alias vim='/usr/local/Cellar/vim/7.4.1817/bin/vim'

# Load the theme
antigen theme https://github.com/caiogondim/bullet-train-oh-my-zsh-theme bullet-train

# Tell antigen that you're done
antigen apply

# Use vim mode

function mkcd
{
    dir="$*"
    mkdir -p "$dir" && cd "$dir";
}
