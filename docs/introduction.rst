============
Introduction
============

.. default-domain:: el

Overview
========

Elpy is an extension for the Emacs_ text editor to work with Python_
projects. This documentation tries to explain how to use Elpy to work
on Python project using Emacs, but it does not aim to be an
introduction to either Emacs or Python.

.. _Emacs: http://www.gnu.org/s/emacs/
.. _Python: http://www.python.org/

You can read a `quick tour`_ of Emacs, or read the built-in tutorial
by running ``C-h t`` in the editor. That is, you hold down the
``control`` key and hit ``h`` (the canonical *help* key in Emacs),
release both, and hit ``t`` (for tutorial).

.. _quick tour: https://www.gnu.org/software/emacs/tour/

For Python, you can read the `basic tutorial`_. If you already know
Python, you should check out some `best practices`_.

.. _basic tutorial: https://docs.python.org/3/tutorial/index.html
.. _best practices: http://docs.python-guide.org/en/latest/

Once you have these basics, you can go on to install Elpy.


Installation
============

With use-package
----------------

Simply add the following lines to you ``.emacs``:

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

The main Elpy package is installed via the Emacs package interface,
``package.el``. First, you have to add Elpy's package archive to your
list of archives, though. Add the following code to your ``.emacs``
file and restart Emacs:

.. code-block:: elisp

   (require 'package)
   (add-to-list 'package-archives
                '("melpa-stable" . "https://stable.melpa.org/packages/"))

Now you can run `M-x package-refresh-contents` to download a fresh
copy of the archive contents, and ``M-x package-install RET elpy RET``
to install elpy. If you want to enable Elpy by default, you can simply
add the following to your .emacs:

.. code-block:: elisp

   (package-initialize)
   (elpy-enable)

Congratulations, Elpy is now successfully installed!


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


In order to use all the features (such as navigation with ``M-.``),
you'll need to install some python libraries.  You can do that easily
by typing ``M-x elpy-config RET``, and following the instructions.
