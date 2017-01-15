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
sudo apt-get -y install emacs vim git-core keepass2 chromium-browser \
     zsh aptitude vlc redshift-gtk avahi-dnsconfd

# Glue tools
# DEPRECATED: gnome-session/gnome-do
sudo apt-get -y install xmonad gnome-session-flashback gnome-do \
     xmobar trayer volti xautolock

# Graphics
sudo apt-get -y install blender inkscape mypaint gimp

# Audio
sudo apt-get -y install audacity

# Dev tools
sudo apt-get -y install python-pip python-virtualenv zfs nmap bpython r-base

# Writing tools
sudo apt-get -y install markdown
pip install --user proselint

# Fonts
sudo apt-get -y install fonts-inconsolata

# Themes
sudo apt-get -y install gnome-themes-standard

# Set my login shell to zsh
sudo chsh -s /bin/zsh

# Make sure oh-my-zsh is present
if [ ! -d ~/.oh-my-zsh ]
then
    echo "========================================"
    echo "Installing oh-my-zsh"
    zsh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
    # TODO: find a way to quit oh-my-zsh automatically
fi

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
# Installations post-config

# Install emacs modes
emacs --script emacs_install.el

################################################################################
# Other 1st-run configuration

echo "========================================"
echo "Running other configuration"

# default keyboard: replace caps lock with ctrl
#     This only works with the login shell, so we also need to add it
#     to the profile
cat /etc/default/keyboard | \
    sed s/XKBOPTIONS=\"\"/XKBOPTIONS=\"ctrl:nocaps\"/ \
    >/tmp/keyboard_configuration
sudo mv /tmp/keyboard_configuration /etc/default/keyboard

# profile: replace caps lock with ctrl on login, turn on X server kill keys
KEYMAP_OPTIONS="ctrl:nocaps,terminate:ctrl_alt_bksp"
EXISTING_CTRL_KEYMAP=`grep "setxkbmap -option \"$KEYMAP_OPTIONS\"" ~/.profile`
if [ -z "$EXISTING_CTRL_KEYMAP" ]
then
    printf "\nsetxkbmap -option \"$KEYMAP_OPTIONS\"\n" >>~/.profile
fi

# git
git config --global user.email "thenoviceoof@gmail.com"
git config --global user.name "thenoviceoof"

# Set up a more palatable GTK 2.0 theme
if [ ! -e ~/.gtkrc-2.0 ]
then
    cat <<EOF >~/.gtkrc-2.0
gtk-icon-theme-name = "Adwaita"
gtk-theme-name = "Adwaita"
EOF
fi

# Set up ibus with the right options.
dconf write /desktop/ibus/general/engines-order \
      "['xkb:us:dvorak:eng', 'xkb:us::eng']"
dconf write /desktop/ibus/general/preload-engines \
      "['xkb:us:dvorak:eng', 'xkb:us::eng']"
# Don't let the default ibus switch hotkey collide with xmonad.
dconf write /desktop/ibus/general/hotkey/triggers "['<Control><Alt>space']"

# Set up redshift cron hack.
EXISTING_REDSHIFT_CRON=`crontab -l | grep "REDSHIFTCRON"`
if [ -z "$EXISTING_REDSHIFT_CRON" ]
then
    crontab -l 2>/dev/null | cat - <<EOF | crontab -
# REDSHIFTCRON Turn on redshift near bedtime.
0 23 * * * /home/thenoviceoof/.local/bin/.redshift-replace.sh
EOF
fi

# Set up backlight permissions/script.
if [ ! -e /etc/sudoers.d/backlight ]
then
    sudo bash -c "cat <<EOF >/etc/sudoers.d/backlight
# Change screen brightness.
thenoviceoof ALL=(ALL) NOPASSWD: /home/thenoviceoof/.local/bin/.screen-brightness-change
EOF"
fi
if [ ! -e ~/.local/bin/.screen-brightness-change ]
then
    cp screen-brightness-change.sh ~/.local/bin/.screen-brightness-change
    # Make sure permissions are locked down.
    chmod u+x,go-x,-w ~/.local/bin/.screen-brightness-change
fi

########################################
# PARTIALLY DEPRECATED (gnome-terminal)

TERM_PROFILE=`gsettings get org.gnome.Terminal.ProfilesList default | cut -d "'" -f 2`

# gnome-terminal: default to more rows
dconf write /org/gnome/terminal/legacy/profiles:/:$TERM_PROFILE/default-size-rows 40

# gnome-terminal: Set Inconsolata as default
dconf write /org/gnome/terminal/legacy/profiles:/:$TERM_PROFILE/use-system-font false
dconf write /org/gnome/terminal/legacy/profiles:/:$TERM_PROFILE/font "'Inconsolata Medium 12'"

# gnome-terminal: Set colors
dconf write /org/gnome/terminal/legacy/profiles:/:$TERM_PROFILE/use-theme-colors false
dconf write /org/gnome/terminal/legacy/profiles:/:$TERM_PROFILE/use-theme-transparency false
dconf write /org/gnome/terminal/legacy/profiles:/:$TERM_PROFILE/foreground-color "'rgb(255,255,255)'"
dconf write /org/gnome/terminal/legacy/profiles:/:$TERM_PROFILE/background-color "'rgb(20,22,25)'"

########################################
# DEPRECATED

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

echo "REMEMBER: update Github with new keys"
echo "REMEMBER: update git/dotfiles to read/write config"
echo "REMEMBER: change chromium-browser to use classical theming"
