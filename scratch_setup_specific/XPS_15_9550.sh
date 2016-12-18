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

# Multiplex the dedicated/integrated graphics.
# Get the (current) newest driver for the right card support.
# TODO: get bumblebee working without a ton of hacks
sudo apt-get -y install nvidia-367 nvidia-prime

# For testing with glxgears
sudo apt-get -y install mesa-utils
