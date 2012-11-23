#!/bin/sh

if [ -z "$1" ]
then
    echo "usage: python-check.sh <file-to-check>" >&2
    exit 1
fi

EXITVALUE=0

for checker in pyflakes pep8 pylint
do
    if which "$checker" &>/dev/null
    then
        echo "*** $checker"
        echo
        if [ "$checker" = "pylint" ]
        then
            "$checker" --output-format=parseable --include-ids=y \
                --reports=no --errors-only "$1"
        else
            "$checker" "$1"
        fi
        RV="$?"
        if [ "$EXITVALUE" = "0" ]
        then 
            EXITVALUE="$RV"
        fi
        echo
    fi
done

exit "$EXITVALUE"
