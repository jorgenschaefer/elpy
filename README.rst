=======================================
Elpy, the Emacs Lisp Python Environment
=======================================

Elpy is an Emacs package to bring powerful Python editing to Emacs. It
combines a number of other packages, both written in Emacs Lisp as
well as Python.

.. image:: https://secure.travis-ci.org/jorgenschaefer/elpy.png
   :alt: Build Status
   :target: http://travis-ci.org/jorgenschaefer/elpy
   :width: 77px
   :height: 19px

Documentation
=============

You can find the documentation `on the wiki`__.

.. __: https://github.com/jorgenschaefer/elpy/wiki

Quick Installation
==================

You can also read the `detailed installation instructions`__.

.. __: https://github.com/jorgenschaefer/elpy/wiki/Installation

First, install the required Python packages:::

  pip install elpy rope

(Note: If you are installing the development version of elpy, do not
install the elpy Python package, but simply put the repository in your
``PYTHONPATH``.)

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
