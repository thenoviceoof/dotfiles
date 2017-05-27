#!/bin/sh

# By default, uses 3 requests.
# Timeout increases 5, 10, 15 (uses deci-seconds as units).
mount -t nfs -o soft,timeo=50,port=3048,mountport=3049,tcp localhost:/mnt/Data/DataShare ~/d
