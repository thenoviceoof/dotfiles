#!/bin/sh

mount -t nfs -o port=3048,mountport=3049,tcp localhost:/mnt/Data/DataShare ~/d
