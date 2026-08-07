
.. image:: https://github.com/jorgenschaefer/elpy/workflows/Tests/badge.svg?branch=master
   :target: https://github.com/jorgenschaefer/elpy/actions?query=workflow%3ATests

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

IMPORTANT NOTE: Looking for a maintainer
========================================

I find myself (@galaunay, current maintainer), unable to maintain Elpy at the moment, and probably for a while.
If you are interested in getting involved in Elpy, please contact me by mail, **the project definitely needs you** !

In the meantime, I will keep an eye on the PRs to integrate any fix and/or new features proposed, but I will definitely not be able to treat the issues in a satisfying manner.
If you are stuck with an issue, please have a look at the `documentation`_, there is a lot of answers there. 
@gfederix also made some bug fixing in his `fork`_, so you can try using this version of Elpy.

.. _documentation: https://elpy.readthedocs.io/en/latest/index.html
.. _fork: https://github.com/gfederix/elpy

Features
========

- `Code completion`_
- `Code Navigation`_
- `Interactive Python shell`_
- `Virtualenv support`_
- `On-the-fly syntax checking`_
- `Access to documentation`_
- `Variable explorer`_
- `Debugging`_
- `Testing`_
- `Profiling`_
- `Snippet Expansion`_
- Code hinting

.. _Code completion: https://elpy.readthedocs.io/en/latest/ide.html#completion
.. _Code Navigation: https://elpy.readthedocs.io/en/latest/ide.html#navigation
.. _On-the-fly syntax checking: https://elpy.readthedocs.io/en/latest/ide.html#syntax-checking
.. _Interactive Python shell: https://elpy.readthedocs.io/en/latest/ide.html#interactive-python
.. _Access to documentation: https://elpy.readthedocs.io/en/latest/ide.html#documentation
.. _Variable explorer: https://elpy.readthedocs.io/en/latest/ide.html#variable-explorer
.. _Debugging: https://elpy.readthedocs.io/en/latest/ide.html#debugging
.. _Testing: https://elpy.readthedocs.io/en/latest/ide.html#testing
.. _Profiling: https://elpy.readthedocs.io/en/latest/ide.html#profiling
.. _Virtualenv support: https://elpy.readthedocs.io/en/latest/concepts.html#virtual-envs
.. _Snippet Expansion: https://elpy.readthedocs.io/en/latest/ide.html#snippets


Installation
============

Elpy is available on Melpa, the most straightforward way to install it is to use `use-package`:

.. code-block:: elisp

  (use-package elpy
    :ensure t
    :init
    (elpy-enable))

For other installation alternatives, see the documentation section `Installation`_.

.. _Installation: https://elpy.readthedocs.io/en/latest/introduction.html#installation


Quickstart
==========

Once installed, Elpy will automatically provide code completion, syntax error highlighting and code hinting (in the modeline) for python files. Elpy offers a lot of features, but the following keybindings should be enough to get started:

- :kbd:`C-c C-c` evaluates the current python script (or region if something is selected) in an interactive python shell. The python shell is automatically displayed aside of your script.
- :kbd:`C-RET` evaluates the current statement (current line plus the following nested lines).
- :kbd:`C-c C-z` switches between your script and the interactive shell.
- :kbd:`C-c C-d` displays documentation for the thing under cursor. The documentation will pop in a different buffer, that can be closed with :kbd:`q`.

Please have a look at the documentation at `Readthedocs`_ if you want to know more about what Elpy can do.

.. _Readthedocs: https://elpy.readthedocs.io/en/latest/index.html

External resources
===================

- `Emacs: The Best Python Editor?`_ by Jon Fincher
- `Managing a Python development environment in Emacs`_ by Diego Fernández Giraldo
- `Configuring GNU emacs with elpy on MacOS`_ by Kenneth H. East

.. _Managing a Python development environment in Emacs: https://medium.com/analytics-vidhya/managing-a-python-development-environment-in-emacs-43897fd48c6a
.. _Emacs\: The Best Python Editor?: https://realpython.com/emacs-the-best-python-editor
.. _Configuring GNU emacs with elpy on MacOS: https://east.fm/posts/configuring-gnu-emacs-with-elpy-on-macos/index.html#

Contact
=======

For questions regarding Elpy, do not hesitate to open an `issue on
github`_ or visit us on IRC, channel ``#emacs-elpy`` on
``irc.freenode.net``.

.. _issue on github: https://github.com/jorgenschaefer/elpy/issues/new
