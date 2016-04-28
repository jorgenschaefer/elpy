====================
Contributing to Elpy
====================

We'd be happy for you to contribute to Elpy.


Setup
=====

You can set up a working environment for Elpy using ``pip`` and
``cask``. After installing Cask_, just run the ``setup`` script:

.. code-block::

   virtualenv ~/.virtualenvs/elpy
   source ~/.virtualenvs/elpy/bin/activate
   ./scripts/setup

.. _Cask: https://cask.readthedocs.io/en/latest/#user-guide


You can now run ``./scripts/test`` to run Elpy's test suite.


Coding Guidelines
=================

Python
------

Please follow `PEP 8`_ in Python source, and provide automated tests
that cover your code. Aim for full coverage of your code.

.. _PEP 8: http://legacy.python.org/dev/peps/pep-0008/


Emacs Lisp
----------

Follow standard Emacs Lisp coding guidelines. All globally defined
symbols should start with ``elpy-``. Mark internal functions with a
double dash, like ``elpy-config--insert-configuration-problems``. Do
not be afraid of long symbol names.

Emacs Lisp tests can be found in the ``test/`` directory. They
generally follow the file name format ``<function-name>-test.el``.
Please provide at least one test per function you define.
