#!/bin/bash

if [ -e ~/.emacs ]
then
    mv ~/.emacs ~/.emacs_bak
fi
cp .emacs ~/

# This assumes that oh-my-zsh has been installed
cp frisky.zsh-theme ~/.oh-my-zsh/themes/
cp .oh-my-zsh/* ~/.oh-my-zsh/custom/

if [ -e ~/.zshrc ]
then
    mv ~/.zshrc ~/.zshrc_bak
fi
cp .zshrc ~/

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

mkdir -p ~/.xmonad
cp .xmonad/xmonad.hs ~/.xmonad/xmonad.hs

cp .gitignore_global ~/
git config --global core.excludesfile ~/.gitignore_global
