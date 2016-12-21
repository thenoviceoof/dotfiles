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
if [ ! -e /etc/X11/xorg.conf.d/50-synaptics.conf ]
then
    sudo cp /usr/share/X11/xorg.conf.d/50-synaptics.conf \
         /etc/X11/xorg.conf.d/50-synaptics.conf
    EXISTING_CONFIG=`grep "# Custom option" /etc/X11/xorg.conf.d/50-synaptics.conf`
    if [ -z "$EXISTING_CONFIG" ]
    then
        sudo bash -c 'cat <<EOF >>/etc/X11/xorg.conf.d/50-synaptics.conf

# Custom option
# Turn on palm rejection
Section "InputClass"
    Identifier "Palm Rejection"
    Driver "synaptics"
    MatchIsTouchpad "on"
    Option "PalmDetect" "1"
    Option "PalmMinWidth" "5"
    Option "PalmMinZ" "20"
    Option "PressureMotionMinZ" "50"
    Option "FingerLow" "40"
    Option "FingerHigh" "55"
EndSection
EOF'
    fi
fi

# Multiplex the dedicated/integrated graphics.
# Get the (current) newest driver for the right card support.
# TODO: get bumblebee working without a ton of hacks
sudo apt-get -y install nvidia-367 nvidia-prime

# For testing with glxgears
sudo apt-get -y install mesa-utils
