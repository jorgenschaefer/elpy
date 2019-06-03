
.. image:: https://secure.travis-ci.org/jorgenschaefer/elpy.svg?branch=master
   :target: http://travis-ci.org/jorgenschaefer/elpy?branch=master

.. image:: https://readthedocs.org/projects/elpy/badge/?version=latest
   :target: https://elpy.readthedocs.io/en/latest/?badge=latest
   :alt: Documentation Status

.. image:: https://coveralls.io/repos/jorgenschaefer/elpy/badge.svg?branch=master
   :target: https://coveralls.io/r/jorgenschaefer/elpy?branch=master

.. image:: https://melpa.org/packages/elpy-badge.svg
   :target: https://melpa.org/#/elpy

.. image:: https://stable.melpa.org/packages/elpy-badge.svg
   :target: https://stable.melpa.org/#/elpy


==========================
Elpy, the Emacs Python IDE
==========================

Elpy is an Emacs package to bring powerful Python editing to Emacs.
It combines and configures a number of other packages, both written in
Emacs Lisp as well as Python. Elpy is fully documented at `Readthedocs`_.

.. _Readthedocs: https://elpy.readthedocs.io/en/latest/index.html

Features
========

- `Code completion`_
- `Code Navigation`_
- `Interactive Python shell`_
- `Virtualenv support`_
- `On-the-fly syntax checking`_
- `Documentation display`_
- `Debugging`_
- `Test running`_
- `Profiling`_
- Snippet Expansion (using `yasnippet`_)
- Code hinting

.. _Code completion: https://elpy.readthedocs.io/en/latest/ide.html#completion
.. _Code Navigation: https://elpy.readthedocs.io/en/latest/ide.html#navigation
.. _On-the-fly syntax checking: https://elpy.readthedocs.io/en/latest/ide.html#syntax-checking
.. _Interactive Python shell: https://elpy.readthedocs.io/en/latest/ide.html#interactive-python
.. _Documentation display: https://elpy.readthedocs.io/en/latest/ide.html#documentation
.. _Debugging: https://elpy.readthedocs.io/en/latest/ide.html#debugging
.. _Test running: https://elpy.readthedocs.io/en/latest/ide.html#testing
.. _Profiling: https://elpy.readthedocs.io/en/latest/ide.html#profiling
.. _Virtualenv support: https://elpy.readthedocs.io/en/latest/concepts.html#virtual-envs
.. _yasnippet: https://github.com/joaotavora/yasnippet


Installation
============

The most straightforward way of installing elpy is to use `use-package`:

.. code-block:: elisp

  (use-package elpy
    :ensure t
    :init
    (elpy-enable))

Elpy relies on some python packages that need to be installed as well:

.. code-block:: bash

  pip install jedi rope flake8 autopep8 yapf black

For other alternatives, see the `documentation`_.

.. _documentation: https://elpy.readthedocs.io/en/latest/introduction.html#installation


Quickstart
==========

Once installed, Elpy will automatically provide code completion, syntax error highlighting and code hinting (in the modeline) for python files. Elpy offers a lot of features, but the following keybindings should be enough to get started:

- :kbd:`C-c C-c` evaluates the current script (or region if something is selected) in an interactive python shell. The python shell will be automatically displayed aside of your script (if not already there).
- :kbd:`C-RET` evaluates the current statement (current line plus the following nested lines).
- :kbd:`C-c C-z` switches between your script and the interactive shell.
- :kbd:`C-c C-d` displays documentation for the thing under cursor (function or module). The documentation will pop in a different buffer, that can be closed with :kbd:`q`.

Please have a look at the documentation at `Readthedocs`_ if you want to know more about what Elpy can do.

.. _Readthedocs: https://elpy.readthedocs.io/en/latest/index.html


Contact
=======

For questions regarding Elpy, do not hesitate to open an `issue on
github`_ or visit us on IRC, channel ``#emacs-elpy`` on
``irc.freenode.net``.

.. _issue on github: https://github.com/jorgenschaefer/elpy/issues/new

If you would like to support this work, you can become a patreon:
https://www.patreon.com/jorgenschaefer
Please note that this is completely voluntary, and does not make you
more important than others when it comes to issues, feature requests
or anything. I appreciate donations, but do not feel compelled to
spend money, and do not feel bad if you don't.
