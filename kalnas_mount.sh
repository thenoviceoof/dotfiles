#!/bin/bash

set -e

SSHCMD="autossh -M 0 -o 'ServerAliveInterval 5' -o 'ServerAliveCountMax 3'"

# Port for nfsd.
NFSD_TUNNEL=`lsof -i -P -n | grep 127.0.0.1:3048 || true`
if [ -z "$NFSD_TUNNEL" ]; then
    $SSHCMD kalnas -L 3048:localhost:2049 -f
    echo "Starting NFS tunnel..."
fi
# Port for mountd.
NFSD_TUNNEL=`lsof -i -P -n | grep 127.0.0.1:3049 || true`
if [ -z "$NFSD_TUNNEL" ]; then
    $SSHCMD kalnas -L 3049:localhost:32222 -f
    echo "Starting mountd tunnel..."
fi
# Use the ssh tunnels to access the NFS share.
# Put the mount command in it's own file, so we can run it with sudo.
echo "Mounting FS..."
sudo ~/.local/bin/.kalnas_mount_command.sh
