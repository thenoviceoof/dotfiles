#!/bin/sh
# This script is kind of idempotent: re-running it will not do
# anything, if it is already in the correct state.

# Which state are we in?
CURRENT_REDSHIFT=`pgrep redshift-gtk >/dev/null; echo $?`
CURRENT_NIGHT=`pgrep -f redshift-night >/dev/null; echo $?`

# Which state should we be in?
HOUR=`date "+%H"`
SHOULD_NIGHT=`test $HOUR -gt 14 -o $HOUR -lt 6; echo $?`

# Stop the existing redshift.
if [ $SHOULD_NIGHT -ne $CURRENT_NIGHT ] && [ $CURRENT_REDSHIFT -eq 0 ]
then
    killall redshift-gtk
    killall redshift

    # Wait for a bit before doing the replacement.
    sleep 5
fi

# Start again, but with manual settings.
if [ $SHOULD_NIGHT -ne $CURRENT_NIGHT ] || [ $CURRENT_REDSHIFT -eq 1 ]
then
    if [ $SHOULD_NIGHT -eq 0 ]
    then
        DISPLAY=:0 redshift-gtk -c /home/thenoviceoof/.config/redshift-night.conf &
    else
        DISPLAY=:0 redshift-gtk &
    fi
fi
