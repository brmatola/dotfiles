## Dotfiles

These are my personal dotfiles for configuring vim and zsh on a new machine.  The script makesymlink.sh will move any existing dotfiles into a dotfiles_old directory before symlinking the files in this folder into ~.

While it will run vundle's plugininstall from the command line, the final installation of youcompleteme requires a little bit of manual work.  Check their installation instructions for details.

Also, note that there are a number of submodules in this repo.  After cloning, run git submodule init; git submodule update to pull them in.
