#!/bin/bash
################################################################################
# Starts org-mode editor.

TODOPATH="$HOME/todo.org"

# First, back up the database.
BACKUPDIR="$HOME/backups/todo"
mkdir -p "$BACKUPDIR"
DATE=`date +"%Y%m%d"`
BACKUPNAME="todo-backup_$DATE.org"
BACKUPPATH="$BACKUPDIR/$BACKUPNAME"
if [ -f "$BACKUPPATH" ]
then
    echo "Backup already exists"
else
    REMOTEPATH="pi@raspberrypi.local:/data/backup/todo/$BACKUPNAME"
    cp "$TODOPATH" "$BACKUPPATH"
    scp "$TODOPATH" "$REMOTEPATH"
fi

emacs -nw "$TODOPATH"