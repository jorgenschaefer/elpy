#!/bin/sh

if [ -z "$1" ]
then
    echo "usage: python-check.sh <file-to-check>" >&2
    exit 1
fi

for checker in pyflakes pep8 pylint
do
    if which "$checker" &>/dev/null
    then
        echo "*** $checker"
        echo
        if [ "$checker" = "pylint" ]
        then
            "$checker" --output-format=parseable "$1"
        else
            "$checker" "$1"
        fi
        echo
    fi
done
