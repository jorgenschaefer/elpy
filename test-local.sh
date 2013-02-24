#!/bin/sh

if [ ! -f "elpy.el" ]
then
    echo "Run this script from the elpy directory." >&2
    exit 1
fi

set -ex

for NOSE in ~/.local/bin/nosetests-*
do
    find -name '*.pyc' -exec rm {} +
    $NOSE --version
    $NOSE
done

for EMACS in ~/Programs/emacsen/*24*/src/emacs \
             ~/Programs/Emacs/src/emacs
do
    find -name '*.elc' -exec rm {} +
    $EMACS --version
    $EMACS -q -batch -L `pwd` -l ert -l elpy-tests.el \
        -f ert-run-tests-batch-and-exit
done
