==============
Extending Elpy
==============

.. default-domain:: el


.. _Writing Modules:

Writing Modules
===============

Modules are a way of easily extending Elpy with modular extensions. In
essence, a module is a function which is called once to initialize
itself globally, then once every time elpy-mode is enabled or
disabled, and also once if elpy is disabled globally.

To achieve this, a module function receives one or more arguments, the
first of which is the command specifier symbol, which can be one of
the following:

``global-init``
    Called once, when Elpy is enabled using :command:`elpy-enable`.
``global-stop``
    Called once, when Elpy is disabled using :command:`elpy-disable`.
``buffer-init``
    Called in a buffer when ``elpy-mode`` is enabled.
``buffer-stop``
    Called in a buffer when ``elpy-mode`` is disabled.

To activate a module, the user has to add the function to
:option:`elpy-modules`.


.. _Writing Test Runners:

Writing Test Runners
====================

A test runner is a function that receives four arguments, described in
the docstring of :function:`elpy-test-at-point`. If only the first
argument is given, the test runner should find tests under this
directory and run them. If the others are given, the test runner
should run the specified test only, or as few as it can.

Test runners should use an interactive spec of ``(interactive
(elpy-test-at-point))`` so they can be called directly by the user.
For their main work, they can use the helper function
:function:`elpy-test-run`. See the
:function:`elpy-test-discover-runner` for an example.

To make it possible to set the test runner as a file-, directory- or
project-local variable, the function symbol should get the
``elpy-test-runner`` property with a value of ``t``.

.. _Running Tests:

Running Tests:
==============

You can set up a working environment for Elpy using ``pip`` and
``cask``. After installing Cask_, create a new virtual environment
and run the ``setup`` script in it:

.. code-block:: sh

   virtualenv ~/.virtualenvs/elpy
   source ~/.virtualenvs/elpy/bin/activate
   ./scripts/setup

.. _Cask: https://cask.readthedocs.io/en/latest/#user-guide


You can now run ``./scripts/test`` to run Elpy's test suite.

If you cannot (or do not want to) use virtual environments on your
system, you can set the environment variable
ELPY_TEST_DONT_USE_VIRTUALENV to skip tests involving virtual
environments: ``ELPY_TEST_DONT_USE_VIRTUALENV=t ./scripts/test``.
