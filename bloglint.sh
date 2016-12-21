#!/bin/sh

# Clean up old html files.
rm -f *.html

for file in $(ls | grep -v html | grep -v \.sh); do
    echo "############################################################"
    echo $file
    echo ""

    tmpfile=$(mktemp)
    cat $file | perl -0777 -pe 's/\n\n\n.*//igs' >$tmpfile

    # Run proselint.
    echo "---------- Proselint"
    proselint $tmpfile
    echo ""

    # Find unescaped markdown link paretheses.
    echo "---------- Markdown parentheses"
    grep '](http.*(.*[^\]).*)' $tmpfile
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

    # Lexical illusions are covered by proselint, but in a silly that
    # doesn't actually extend across lines.

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
    mdfile="$file.html"
    cat <<EOF >$mdfile
<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8" />
</head>

<body>
EOF
    cat $tmpfile | markdown >>$mdfile
    cat <<EOF >>$mdfile
</body>
</html>
EOF
    echo ""

    # Clean up.
    rm $tmpfile
done
