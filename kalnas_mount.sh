#!/bin/bash

set -e

# SSHFS wins over NFS: more responsive (ish), fewer moving parts,
# easier set up, just as performant.
sshfs -o ServerAliveInterval=30 -o reconnect kalnas:/mnt/Data/DataShare d
