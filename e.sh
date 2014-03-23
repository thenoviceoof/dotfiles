#!/bin/bash
################################################################################
# switched back to emacs: I usually have long lived emacs windows, and
# the whole frame system was making me sad:
#  - git would try to open a new commit window, and that would screw
#    up old frames
#  - opening new frames in different directories would not shift the
#    CWD to the new directory, or would shift the default-directory
#    for all frames

emacs -nw "$@"
# emacsclient -nw --alternate-editor="" -c "$@"
