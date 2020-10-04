#!/bin/sh

# Adpated from https://gist.github.com/rejeep/ebcd57c3af83b049833b
#
# This script will setup Evm (Emacs Version Manager) and Cask on
# Travis to use for Emacs Lisp testing.
#
# In .travis.yml, add this:
#
#  - curl -fsSkL https://gist.github.com/rejeep/ebcd57c3af83b049833b/raw > x.sh && source ./x.sh
#
# Emacs 26.3 is installed in the above script because Cask requires
# Emacs to be installed. Because of this, when installing other
# environments in the .travis.yml configuration, use the --skip
# option, for example:
#
#  - evm install $EVM_EMACS --use --skip

export PATH="/home/travis/.evm/bin:$PATH"
export PATH="/home/travis/.cask/bin:$PATH"

git clone https://github.com/rejeep/evm.git /home/travis/.evm
evm config path /tmp
evm install emacs-25.3-travis --use --skip

curl -fsSkL https://raw.github.com/cask/cask/master/go | python
