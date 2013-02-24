#!/bin/sh

set -ex

nosetests

$EMACS --version
$EMACS -q -batch -L `pwd` -l ert -l elpy-tests.el \
        -f ert-run-tests-batch-and-exit
