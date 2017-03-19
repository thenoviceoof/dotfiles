#!/bin/sh

# Make sure errors kill the process.
# TODO: get proselint failures to stop killing the process.
# set -e

# Clean up old html files.
rm -f *.html

for file in $(ls | grep -v html | grep -v \.sh); do
    echo "############################################################"
    echo $file
    echo ""

    tmpfile=$(mktemp)
    tmphtmlfile=$(mktemp)
    cat $file | perl -0777 -pe 's/\n\n\n.*//igs' >$tmpfile

    # Run proselint.
    echo "---------- Proselint"
    proselint $tmpfile
    echo ""

    # Find unescaped markdown link paretheses.
    echo "---------- Markdown parentheses"
    grep '](http[^()]*([^()\]*)[^()]*)' $tmpfile
    echo ""

    ############################################################
    # Professor scripts
    # http://matt.might.net/articles/shell-scripts-for-passive-voice-weasel-words-duplicates/

    echo "---------- Weasel words"
    # Weasel words
    professor_weasels="many|various|very|fairly|several|extremely\
|exceedingly|quite|remarkably|few|surprisingly\
|mostly|largely|huge|tiny|((are|is) a number)\
|excellent|interestingly|significantly\
|substantially|clearly|vast|relatively|completely"
    egrep -i -n --color "\\b($professor_weasels)\\b" $tmpfile
    echo ""

    # Passive voice detection is too noisy.

    # Lexical illusions are covered by proselint, but in a silly way
    # that doesn't actually extend across lines.

    # Do lexical illusion detection myself
    echo "---------- Lexical illusions (repeated)"
    grep -Pzo '\n.*\s(\w+)\n\1\s.*\n' $tmpfile
    echo ""

    ############################################################
    # Massage input

    # Replace ... with a proper ellipsis
    workingfile=$(mktemp)
    cat $tmpfile | sed 's/\.\.\./…/' >$workingfile
    mv $workingfile $tmpfile

    ############################################################
    # Generate markdown.
    echo "---------- Generating markdown..."
    markdown $tmpfile >$tmphtmlfile

    ############################################################
    # Massage output

    # Make sure HTML tags don't end a line: copy pasting to Google
    # Docs when a display/markdown line break coincide with an HTML
    # tag ends the markdown line elides the whitespace between the
    # HTML tag and the content on the next markdown line. For example:
    #
    #     <style>p { width: 1em; }</style>
    #
    #     Test *no*
    #     whitespace.
    #
    # will be rendered as *no*whitespace after paste. This hacks
    # around the issue by reformatting HTML lines that end in an HTML
    # tag. The problem still exists for display breaks with tags on
    # both sides of the break.
    workingfile=$(mktemp)
    cat $tmphtmlfile | \
        perl -0777pe 's/(<\/.+>)\n(.)/$1 $2/g' | \
        perl -0777pe 's/(.)\n(<.+>)/$1 $2/g' >$workingfile
    mv $workingfile $tmphtmlfile

    ############################################################
    # Write the output file
    echo "---------- Writing output file..."

    htmlfile="$file.html"
    cat <<EOF >$htmlfile
<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8" />
</head>

<body>
EOF
    cat $tmphtmlfile >>$htmlfile
    cat <<EOF >>$htmlfile
</body>
</html>
EOF
    echo ""

    # Clean up.
    rm $tmphtmlfile
    rm $tmpfile
done
