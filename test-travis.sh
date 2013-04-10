#!/bin/sh

set -ex

nosetests

export PYTHONPATH="`pwd`"

$EMACS --version
$EMACS -q -batch -L `pwd` -l ert -l elpy-tests.el \
        -f ert-run-tests-batch-and-exit
