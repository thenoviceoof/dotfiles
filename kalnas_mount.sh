#!/bin/bash

set -e

# Port for nfsd.
NFSD_TUNNEL=`lsof -i -P -n | grep 127.0.0.1:3048 || true`
if [ -z "$NFSD_TUNNEL" ]; then
    echo "Starting NFS tunnel..."
    autossh -M 0 -f -T -N -o "ServerAliveInterval 5" -o "ServerAliveCountMax 3" kalnas -L 3048:localhost:2049
fi
# Port for mountd.
NFSD_TUNNEL=`lsof -i -P -n | grep 127.0.0.1:3049 || true`
if [ -z "$NFSD_TUNNEL" ]; then
    echo "Starting mountd tunnel..."
    autossh -M 0 -f -T -N -o "ServerAliveInterval 5" -o "ServerAliveCountMax 3" kalnas -L 3049:localhost:32222
fi
# Use the ssh tunnels to access the NFS share.
# Put the mount command in it's own file, so we can run it with sudo.
echo "Mounting FS..."
sudo ~/.local/bin/.kalnas_mount_command.sh
