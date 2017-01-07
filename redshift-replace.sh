#!/bin/sh
# Note: this script is a one-off hack. It doesn't work if you reboot the machine, etc.

# Stop the existing redshift.
killall redshift-gtk
killall redshift

# Start again, but with manual settings.
redshift-gtk -c /home/thenoviceoof/.config/redshift-night.conf
