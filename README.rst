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

Documentation
=============

Elpy is fully documented at readthedocs.io:

https://elpy.readthedocs.io/en/latest/index.html

Quick Installation
==================

First, install the required Python packages:::

  # Either of these
  pip install rope
  pip install jedi
  # flake8 for code checks
  pip install flake8
  # and autopep8 for automatic PEP8 formatting
  pip install autopep8
  # and yapf for code formatting
  pip install yapf

One-line install: **pip install jedi flake8 autopep8**

Evaluate this in your ``*scratch*`` buffer:

.. code-block:: lisp

  (require 'package)
  (add-to-list 'package-archives
               '("melpa-stable" . "https://stable.melpa.org/packages/"))


Then run ``M-x package-refresh-contents`` to load the contents of the
new repository, and ``M-x package-install RET elpy RET`` to install
elpy.

Users of Debian ≥10 or Ubuntu ≥18.10 can skip the instructions above
this line and may simply install Elpy and all of its recommended
dependencies with the following command:::

  sudo apt install elpa-elpy
  # To extend Elpy's capabilities by installing packages at the "Suggests"
  # level of dependency, noted by apt during installation, do:
  sudo apt install python3-autopep8 yapf3 black python3-pip \
                   python3-jupyter-console

Elpy can then be activated by running ``M-x elpy-enable``

Finally, to make this automatic, add the following to your ``.emacs``:

.. code-block:: lisp

  (package-initialize)
  (elpy-enable)
  ;; (package-initialize) should already exist at the top of the init
  ;; file on Debian-derived systems, thus (elpy-enable) should be all
  ;; that is required.

Done.

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
