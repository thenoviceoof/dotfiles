#!/bin/bash

set -e

# SSHFS wins over NFS: more responsive (ish), fewer moving parts,
# easier set up, just as performant.
sshfs -o reconnect,ServerAliveInterval=5,ServerAliveCountMax=2 kalnas:/mnt/Data/DataShare d/
