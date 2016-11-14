#!/bin/bash
#######################
# .make.sh
# This script symlinks from the home directory to any dotfiles in ~/dotfiles
#######################


######### Variables

dir=~/dotfiles			# dotfiles directory
olddir=~/dotfiles_old		# old dotfiles backup directory
files="vim vimrc zshrc zshenv"

##########

# create dotfiles_old in homemdir
echo -n "Creating $olddir for backup of any existing dotfiles in ~ ..."
mkdir -p $olddir
echo "done"


# change to the dotfiles directory
echo -n "Changing to the $dir directory ..."
cd $dir
echo "done"

# move any existing dotfiles in homedir to dotfiles_old directory, then symlink
for file in $files; do
	echo "Moving any existing dotfiles from ~ to $olddir"
	mv ~/.$file ~/dotfiles_old/
	echo "Creating symlink to $file in home directory."
	ln -s $dir/$file ~/.$file
done

# setup nvim in ~/.config
rm -rf ~/.config/nvim/
mkdir ~/.config/nvim/
ln -s $dir/nvim/autoload ~/.config/nvim/autoload
ln -s $dir/nvim/config ~/.config/nvim/config
ln -s $dir/nvim/init.vim ~/.config/nvim/init.vim
ln -s $dir/nvim/syntax ~/.config/nvim/syntax

unamestr='uname'
if [[ "$unamestr" == 'Darwin' ]]; then
    ~/dotfiles/vim/bundle/YouCompleteMe/install.py --clang-completer
else echo 'You need to finish install of youcompleteme manually'
fi

