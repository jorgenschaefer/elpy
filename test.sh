#!/bin/sh

if [ ! -f "elpy.el" ]
then
    echo "Run this script from the elpy directory." >&2
    exit 1
fi

set -ex

nosetests

for EMACS in ~/Programs/emacsen/*24*/src/emacs ~/Programs/Emacs/src/emacs
do
    $EMACS -q -batch -L `pwd` -l ert -l elpy-tests.el \
        -f ert-run-tests-batch-and-exit
done
