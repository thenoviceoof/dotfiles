#!/bin/sh

if [ $1 ]
then
	emacsclient -nw --alternate-editor='' -e "(progn (select-frame (make-frame)) (cd \"`pwd`\") (find-file \"$1\"))"
else
	emacsclient -nw --alternate-editor='' -e "(progn (select-frame (make-frame)) (cd \"`pwd`\"))"
fi
