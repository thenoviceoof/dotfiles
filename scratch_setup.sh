#!/bin/bash
################################################################################
# NOTES:
# - Used to provision borune with Ubuntu 2016.04

echo "thenoviceoof's SUPER-DUPER SWEET AS HELL SETUP AND PROVISIONING SCRIPT"
echo "============================================================"
echo "WARNING: This script is only MOSTLY idempotent"
echo "           Safety not likely on non-clean systems"
echo "Proceed?"
select yn in "Yes" "No"; do
    case $yn in
        Yes ) break;;
        No ) exit;;
    esac
done

################################################################################
# Install

echo "========================================"
echo "Installing from apt-get"

# General tools
sudo apt-get -y install emacs vim git-core keepass2 chromium-browser

# Glue tools
# DEPRECATED: gnome-session/gnome-do
sudo apt-get -y install xmonad gnome-session-flashback gnome-do

# Graphics
sudo apt-get -y install blender inkscape mypaint

# Other installation
sudo apt-get -y install python-pip python-virtualenv

# Fonts
sudo apt-get -y install fonts-inconsolata

# Emacs modes
sudo apt-get -y install haskell-mode
mkdir -p ~/.emacs.d/
if [ ! -d ~/.emacs.d/coffee-mode ]; then
    pushd ~/.emacs.d
    git clone https://github.com/defunkt/coffee-mode.git
    popd
fi
if [ ! -d ~/.emacs.d/clojure-mode ]; then
    pushd ~/.emacs.d
    git clone https://github.com/clojure-emacs/clojure-mode.git
    popd
fi
if [ ! -d ~/.emacs.d/rust-mode ]; then
    pushd ~/.emacs.d
    git clone https://github.com/rust-lang/rust-mode
    popd
fi

# Make sure oh-my-zsh is present
if [ ! -d ~/.oh-my-zsh ]
then
    echo "========================================"
    echo "Installing oh-my-zsh"
    zsh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
fi

# pip stuff
pip install --user proselint

# Personal stuff
pip install --user pyli

################################################################################
# Clean up default directories

echo "========================================"
echo "Cleaning up directory structure"
rm -rf ~/Documents ~/Music ~/Pictures ~/Public ~/Templates ~/Videos
mkdir -p ~/git ~/tmp

################################################################################
# Configuration

# Fetch dotfiles
cd ~/git
if [ ! -d ~/git/dotfiles ]
then
   git clone https://github.com/thenoviceoof/dotfiles.git
fi

# Push the configuration files out
echo "========================================"
echo "Installing configuration files"
cd ~/git/dotfiles
./push.sh

################################################################################
# Other 1st-run configuration

echo "========================================"
echo "Running other configuration"

# default keyboard: replace caps lock with ctrl
#     This only works with the login shell, so we also need to add it
#     to the profile
cat /etc/default/keyboard | sed s/XKBOPTIONS=\"\"/XKBOPTIONS=\"ctrl:nocaps\"/ >/tmp/keyboard_configuration
sudo mv /tmp/keyboard_configuration /etc/default/keyboard

# profile: replace caps lock with ctrl on login
EXISTING_CTRL_KEYMAP=`grep "setxkbmap -option \"ctrl:nocaps\"" ~/.profile`
if [ -z "$EXISTING_CTRL_KEYMAP" ]
then
    printf "\nsetxkbmap -option \"ctrl:nocaps\"\n" >>~/.profile
fi

# git
git config --global user.email "thenoviceoof@gmail.com"
git config --global user.name "thenoviceoof"

# DEPRECATED

# gnome-terminal: default to more rows
TERM_PROFILE=`gsettings get org.gnome.Terminal.ProfilesList default | cut -d "'" -f 2`
dconf write /org/gnome/terminal/legacy/profiles:/:$TERM_PROFILE/default-size-rows 40

# gnome-terminal: Set Inconsolata as default
dconf write /org/gnome/terminal/legacy/profiles:/:$TERM_PROFILE/use-system-font false
dconf write /org/gnome/terminal/legacy/profiles:/:$TERM_PROFILE/font "'Inconsolata Medium 12'"

# gnome-do: change invoke keys
gconftool-2 -s /apps/gnome-do/preferences/Do/Platform/Common/AbstractKeyBindingService/Summon_Do "<Control>Tab" --type=string

# Change gnome 3.0 clocks
dconf write /com/canonical/indicator/datetime/show-date true
dconf write /com/canonical/indicator/datetime/show-seconds true
dconf write /com/canonical/indicator/datetime/time-format "'24-hour'"
#     This change impacts the calendar widget
dconf write /com/canonical/indicator/datetime/show-week-numbers true

################################################################################
# Generate ssh key

if [ ! -e ~/.ssh/id_rsa ]
then
    echo "========================================"
    echo "Generating ssh key"
    ssh-keygen -t rsa -b 4096 -C "thenoviceoof@gmail.com"
fi

################################################################################
# Reminders for manual updates

echo "========================================"
echo "REMINDERS!"
echo "===================="

echo "REMEMBER: change gnome-do preferences to launch on startup"
echo "REMEMBER: update Github with new keys"
echo "REMEMBER: update git/dotfiles to read/write config"
