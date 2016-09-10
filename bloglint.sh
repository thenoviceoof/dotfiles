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

    ## TODO: add professor scripts here

    # Generate markdown.
    echo "---------- Generating markdown..."
    mdfile="$file.html"
    cat $tmpfile | markdown >$mdfile
    echo ""

    # Clean up.
    rm $tmpfile
done
