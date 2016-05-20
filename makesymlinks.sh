#!/bin/bash
#######################
# .make.sh
# This script symlinks from the home directory to any dotfiles in ~/dotfiles
#######################


######### Variables

dir=~/dotfiles			# dotfiles directory
olddir=~/dotfiles_old		# old dotfiles backup directory
files="vim vimrc zshrc" 

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

# Run vundle plugininstall automatically
vim +PluginInstall +qall
echo 'You need to finish install of youcompleteme manually'
