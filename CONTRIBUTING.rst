====================
Contributing to Elpy
====================

We'd be happy for you to contribute to Elpy.


Setup
=====

You can set up a working environment for Elpy using ``pip`` and
``cask``. After installing Cask_, and follow these steps:

.. code-block::

   virtualenv ~/.virtualenvs/elpy
   source ~/.virtualenvs/elpy/bin/activate
   pip install -r requirements.txt
   pip install -r requirements-dev.txt
   cask install

For Python 3, use ``requirements3.txt``.

You can now run ``make test``, ``make python-test`` or ``make
elisp-test`` to run tests for both or just one language.

.. _Cask: http://cask.readthedocs.org/en/latest/#user-guide


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
generally follow the file name format ``test-<function-name>.el``.
Please provide at least one test per function you define.
