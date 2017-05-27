#!/bin/sh

# Port for nfsd.
NFSD_TUNNEL=`lsof -i -P -n | grep 127.0.0.1:3048`
if [ -z "$NFSD_TUNNEL" ]; then
   ssh kalnas -L 3048:localhost:2049 -f -N
fi
# Port for mountd.
NFSD_TUNNEL=`lsof -i -P -n | grep 127.0.0.1:3049`
if [ -z "$NFSD_TUNNEL" ]; then
    ssh kalnas -L 3049:localhost:32222 -f -N
fi
# Use the ssh tunnels to access the NFS share.
sudo mount -t nfs -o port=3048,mountport=3049,tcp localhost:/mnt/Data/DataShare ~/d
