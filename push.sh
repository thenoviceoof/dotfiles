#!/bin/bash

if [ -e ~/.emacs ]
then
    mv ~/.emacs ~/.emacs_bak
fi
cp .emacs ~/

################################################################################
# zsh

# This assumes that oh-my-zsh has been installed
cp frisky.zsh-theme ~/.oh-my-zsh/themes/
cp .oh-my-zsh/* ~/.oh-my-zsh/custom/

if [ -e ~/.zshrc ]
then
    mv ~/.zshrc ~/.zshrc_bak
fi
cp .zshrc ~/

################################################################################
# local binaries

mkdir -p ~/.local/bin

cp e.sh ~/.local/bin/
if [ ! -e ~/.local/bin/e ]
then
    ln -s ~/.local/bin/e.sh ~/.local/bin/e
fi

cp t.sh ~/.local/bin/
if [ ! -e ~/.local/bin/t ]
then
    ln -s ~/.local/bin/t.sh ~/.local/bin/t
fi

cp genpass.py ~/.local/bin/
if [ ! -e ~/.local/bin/genpass ]
then
    ln -s ~/.local/bin/genpass.py ~/.local/bin/genpass
fi

cp bloglint.sh ~/.local/bin/
if [ ! -e ~/.local/bin/bloglint ]
then
    ln -s ~/.local/bin/bloglint.sh ~/.local/bin/bloglint
fi

################################################################################
# Other

mkdir -p ~/.xmonad
cp .xmonad/xmonad.hs ~/.xmonad/xmonad.hs

cp .gitignore_global ~/
git config --global core.excludesfile ~/.gitignore_global

################################################################################
# Link in external tools if they're available

# Android studio
if [ -d ~/Android/android-studio/bin -a ! -e ~/.local/bin/android-studio ]
then
    ln -s ~/Android/android-studio/bin/studio.sh ~/.local/bin/android-studio
fi

# adb at the default SDK location
if [ -d ~/Android/Sdk/platform-tools -a ! -e ~/.local/bin/adb ]
then
    ln -s ~/Android/Sdk/platform-tools/adb ~/.local/bin/adb
fi

# Intellij (flutter dev)
if [ -d ~/.local/tools/intellij-idea -a ! -e ~/.local/bin/intellij ]
then
    ln -s ~/.local/tools/intellij-idea/bin/idea.sh ~/.local/bin/intellij
fi

# TODO: add go_appengine command line tool
