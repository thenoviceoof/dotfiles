#!/bin/sh

# Port for nfsd.
ssh kalnas -L 3048:localhost:2049 -f -N
# Port for mountd.
ssh kalnas -L 3049:localhost:32222 -f -N
# Use the ssh tunnels to access the NFS share.
sudo mount -t nfs -o port=3048,mountport=3049,tcp localhost:/mnt/Data/DataShare ~/d
