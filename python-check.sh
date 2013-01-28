#!/bin/sh

if [ -z "$1" ]
then
    echo "usage: python-check.sh <file-to-check>" >&2
    exit 1
fi

EXITVALUE=0

for checker in pyflakes pep8
do
    if which "$checker" &>/dev/null
    then
        echo "*** $checker"
        echo
        "$checker" "$1"
        RV="$?"
        if [ "$EXITVALUE" = "0" ]
        then
            EXITVALUE="$RV"
        fi
        echo
    fi
done

exit "$EXITVALUE"
