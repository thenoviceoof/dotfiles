#!/bin/bash

# Changes the backlight by a certain percentage, with a +/- prefix.

if [ -d /sys/class/backlight/intel_backlight ] && [ "$#" -eq 1 ]
then
    MAXBRIGHT=`cat /sys/class/backlight/intel_backlight/max_brightness`
    CURBRIGHT=`cat /sys/class/backlight/intel_backlight/brightness`
    BRIGHT=`python -c "print $CURBRIGHT + int($1 * float($MAXBRIGHT)/100)"`

    # Clamp to valid values.
    if [ "$BRIGHT" -gt "$MAXBRIGHT" ]
    then
        BRIGHT=$MAXBRIGHT
    fi
    if [ "$BRIGHT" -lt "1" ]
    then
        BRIGHT="1"
    fi

    echo "$BRIGHT" >/sys/class/backlight/intel_backlight/brightness
fi
