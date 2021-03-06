#!/bin/bash
# Setup specific for the XPS 15 laptop.

# Fix the touchpad being touchy
# http://wiki.yobi.be/wiki/Laptop_Dell_XPS_15#Touchpad
BLACKLIST_TOUCHPAD=`grep i2c-designware-platform /etc/modprobe.d/blacklist.conf`
if [ -z "$BLACKLIST_TOUCHPAD" ]
then
   sudo bash -c "cat <<EOF >>/etc/modprobe.d/blacklist.conf

# Remove this extra touchpad module.
blacklist i2c-designware-platform
EOF"
fi

# Setup the synaptics xorg conf, for turning on palm rejection
if [ ! -d /etc/X11/xorg.conf.d ]
then
    sudo mkdir /etc/X11/xorg.conf.d
fi
if [ ! -e /etc/X11/xorg.conf.d/50-custom-synaptics.conf ]
then
    EXISTING_CONFIG=`grep "# Custom options" /etc/X11/xorg.conf.d/50-custom-synaptics.conf`
    if [ -z "$EXISTING_CONFIG" ]
    then
        sudo bash -c 'cat <<EOF >>/etc/X11/xorg.conf.d/50-custom-synaptics.conf

# Custom options
# Turn on palm rejection
Section "InputClass"
    Identifier "Palm Rejection"
    Driver "synaptics"
    MatchIsTouchpad "on"
    Option "PalmDetect" "1"
    Option "PalmMinWidth" "5"
    Option "PalmMinZ" "0"
    Option "PressureMotionMinZ" "50"
    Option "FingerLow" "35"
    Option "FingerHigh" "50"
EndSection

# Turn on better coasting options.
Section "InputClass"
    Identifier "Touchpad Flings"
    Driver "synaptics"
    MatchIsTouchpad "on"
    Option "CoastingSpeed" "80"
    Option "CoastingFriction" "60"
EndSection
EOF'
    fi
fi

# Start the syndaemon on login.
SYNDAEMON_COMMAND="syndaemon -i 2 -d -t -k"
EXISTING_SYNDAEMON=`grep "$SYNDAEMON_COMMAND" ~/.profile`
if [ -z "$EXISTING_SYNDAEMON" ]
then
    printf "\n$SYNDAEMON_COMMAND\n" >>~/.profile
fi

# Reverse the direction of the touchpad.
if [ ! -e ~/.local/bin/.reverse-touchpad.sh ]
then
    cat <<EOF >~/.local/bin/.reverse-touchpad.sh
# Set up touchpad reversal.
TOUCHPAD_ID=\`xinput list | grep "Synaptics TouchPad" | sed 's/^.*id=\([[:digit:]]\+\).*$/\1/'\`
PROP_ID=\`xinput list-props \$TOUCHPAD_ID | grep "Synaptics Scrolling Distance" | sed 's/^.*(\([[:digit:]]\+\)).*$/\1/' \`
xinput set-prop \$TOUCHPAD_ID \$PROP_ID -75 -75
EOF
    chmod u+x ~/.local/bin/.reverse-touchpad.sh
    REVERSE_COMMAND="~/.local/bin/.reverse-touchpad.sh"
    EXISTING_REVERSE=`grep "$REVERSE_COMMAND" ~/.profile`
    if [ -z "$EXISTING_REVERSE" ]
    then
        printf "\n$REVERSE_COMMAND\n" >>~/.profile
    fi
fi

# Multiplex the dedicated/integrated graphics.
# Get the (current) newest driver for the right card support.
# TODO: get bumblebee working without a ton of hacks
sudo apt-get -y install nvidia-367 nvidia-prime
