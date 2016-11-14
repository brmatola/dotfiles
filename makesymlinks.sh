#!/bin/bash
#######################
# .make.sh
# This script symlinks from the home directory to any dotfiles in ~/dotfiles
#######################

# Download all submodules
git submodule update --init --recursive


######### Variables

dir=~/dotfiles			# dotfiles directory
olddir=~/dotfiles_old		# old dotfiles backup directory
files="zshrc zshenv tmux.conf"

##########

# change to the dotfiles directory
echo -n "Changing to the $dir directory ..."
cd $dir
echo "done"

# move any existing dotfiles in homedir to dotfiles_old directory, then symlink
for file in $files; do
	echo "Creating symlink to $file in home directory."
	ln -s $dir/$file ~/.$file
done

# setup nvim in ~/.config
echo "Creating nvim configuration at ~/.config/nvim"
rm -rf ~/.config/nvim/
mkdir ~/.config/nvim/
ln -s $dir/nvim/autoload ~/.config/nvim/autoload
ln -s $dir/nvim/config ~/.config/nvim/config
ln -s $dir/nvim/init.vim ~/.config/nvim/init.vim
ln -s $dir/nvim/syntax ~/.config/nvim/syntax
