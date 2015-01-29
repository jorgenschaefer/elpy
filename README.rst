=======================================
Elpy, the Emacs Lisp Python Environment
=======================================

Elpy is an Emacs package to bring powerful Python editing to Emacs. It
combines a number of other packages, both written in Emacs Lisp as
well as Python.

.. image:: https://secure.travis-ci.org/jorgenschaefer/elpy.png?branch=master
   :target: http://travis-ci.org/jorgenschaefer/elpy?branch=master

.. image:: https://coveralls.io/repos/jorgenschaefer/elpy/badge.png?branch=master
   :target: https://coveralls.io/r/jorgenschaefer/elpy?branch=master

Documentation
=============

Elpy is fully documented at readthedocs.org:

http://elpy.readthedocs.org/en/latest/index.html

Quick Installation
==================

First, install the required Python packages:::

  # Either of these
  pip install rope
  pip install jedi
  # flake8 for code checks
  pip install flake8
  # importmagic for auto imports
  pip install importmagic

Evaluate this in your ``*scratch*`` buffer:

.. code-block:: lisp

  (require 'package)
  (add-to-list 'package-archives
               '("elpy" . "http://jorgenschaefer.github.io/packages/"))


Then run ``M-x package-refresh-contents`` to load the contents of the
new repository, and ``M-x package-install RET elpy RET`` to install
elpy.

Finally, add the following to your ``.emacs``:

.. code-block:: lisp

  (package-initialize)
  (elpy-enable)

Done.

License
=======

This project is free software: You can redistribute it and/or modify
it under the terms of the `GNU General Public License`__, either
version 3 of the License, or (at your option) any later version.

.. __: LICENSE
