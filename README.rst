=======================================
Elpy, the Emacs Lisp Python Environment
=======================================

Elpy is an Emacs package to bring powerful Python editing to Emacs. It
combines and configures a number of other packages, both written in
Emacs Lisp as well as Python.

.. image:: https://secure.travis-ci.org/jorgenschaefer/elpy.png?branch=master
   :target: http://travis-ci.org/jorgenschaefer/elpy?branch=master

.. image:: https://coveralls.io/repos/jorgenschaefer/elpy/badge.png?branch=master
   :target: https://coveralls.io/r/jorgenschaefer/elpy?branch=master

.. image:: https://melpa.org/packages/elpy-badge.svg
   :target: https://melpa.org/#/elpy

.. image:: https://stable.melpa.org/packages/elpy-badge.svg
   :target: https://stable.melpa.org/#/elpy


Documentation
=============

Elpy is fully documented at readthedocs.io:

https://elpy.readthedocs.io/en/latest/index.html

Quick Installation
==================

Python packages
---------------

First, install the required Python packages:::

  # Completion and code navigation
  pip install jedi
  # Code checks
  pip install flake8
  # Automatic formatting (PEP8, Yapf or Black)
  pip install autopep8
  pip install yapf
  pip install black

One-line install: **pip install jedi flake8 autopep8 black yapf**

With `use-package`
------------------

.. code-block:: elisp

  (use-package elpy
    :ensure t
    :init
    (elpy-enable))

Or if you want to defer Elpy loading:

.. code-block:: elisp

  (use-package elpy
    :ensure t
    :defer t
    :init
    (advice-add 'python-mode :before 'elpy-enable))

Manually from Melpa
-------------------

Evaluate this in your ``*scratch*`` buffer:

.. code-block:: elisp

  (require 'package)
  (add-to-list 'package-archives
               '("melpa-stable" . "https://stable.melpa.org/packages/"))


Then run ``M-x package-refresh-contents`` to load the contents of the
new repository, and ``M-x package-install RET elpy RET`` to install
elpy.

Elpy can then be activated by running ``M-x elpy-enable``.
This can be made automatic by adding the following to your ``.emacs``:

.. code-block:: elisp

  (elpy-enable)

From apt (Debian ≥10 an Ubuntu ≥18.10)
--------------------------------------

Users of Debian ≥10 or Ubuntu ≥18.10 can skip the instructions above
this line and may simply install Elpy and all of its recommended
dependencies with the following command:::

  sudo apt install elpa-elpy

Elpy can then be activated by running ``M-x elpy-enable``.
This can be made automatic by adding the following to your ``.emacs``:

.. code-block:: elisp

  (elpy-enable)


Contact
=======

For questions regarding Elpy, do not hesitate to open an `issue on
github`_ or visit us on IRC, channel ``#emacs-elpy`` on
``irc.freenode.net``.

.. _issue on github: https://github.com/jorgenschaefer/elpy/issues/new

License
=======

This project is free software: You can redistribute it and/or modify
it under the terms of the `GNU General Public License`__, either
version 3 of the License, or (at your option) any later version.

.. __: LICENSE

Thank You
=========

If you would like to support this work, you can become a patreon:

https://www.patreon.com/jorgenschaefer

Please note that this is completely voluntary, and does not make you
more important than others when it comes to issues, feature requests
or anything. I appreciate donations, but do not feel compelled to
spend money, and do not feel bad if you don't.
