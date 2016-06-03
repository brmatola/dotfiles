source ~/dotfiles/antigen/antigen.zsh

# Prioritize executables in macports directory
export PATH=/opt/local/bin:/opt/local/sbin:$PATH
export PATH=$PATH:.

# Setup Powerline
powerline-daemon -q
source /opt/local/Library/Frameworks/Python.framework/Versions/3.5/lib/python3.5/site-packages/powerline/bindings/zsh/powerline.zsh

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

# Path to global python (3.5) installation
globalpyinstall=/opt/local/Library/Frameworks/Python.framework/Versions/3.5

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
