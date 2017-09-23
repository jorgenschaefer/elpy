============
IDE Features
============

.. default-domain:: el

Projects
========

Elpy supports the notion of *projects*, a related collection of files
under a common directory. This common directory is called the
:index:`project root`. A number of Elpy's commands work on all files
inside the project root.

.. command:: elpy-find-file
   :kbd: C-c C-f

   Find a file in the current project. This uses a search-as-you-type
   interface for all files under the project root.

   A prefix argument enables "do what I mean" mode. On an import
   statement, it will try to open the module imported. Elsewhere in a
   file, it will look for an associated test or implementation file,
   and if found, open that. If this fails, either way, it will fall
   back to the normal find file in project behavior.

   If the current file is called ``foo.py``, then this will search for
   a ``test_foo.py`` in the same directory, or in a ``test`` or
   ``tests`` subdirectory. If the current file is already called
   ``test_foo.py``, it will try and find a ``foo.py`` nearby.

   This command uses `find-file-in-project`_ under the hood, so see
   there for more options.

.. _find-file-in-project: https://github.com/technomancy/find-file-in-project

.. command:: elpy-rgrep-symbol
   :kbd: C-c C-s

   Search the files in the current project for a string. By default,
   this uses the symbol at point. With a prefix argument, it will
   prompt for a regular expression to search.

   This is basically a ``grep -r`` through the project.

In addition to these two commands, :command:`elpy-check` also supports
optionally checking all files in the current project.

Elpy's idea of the project root and which files belong to a project
and which don't can be influenced as well.

.. command:: elpy-set-project-root

   Set the current project root directory. This directory should
   contain all files related to the current project.

.. option:: elpy-project-ignored-directories

   When Elpy searches for files in the current project, it will ignore
   files in directories listed here.

.. option:: elpy-project-root-finder-functions

   To find the project root, Elpy can utilize a number of heuristics.
   With this option, you can configure which are used.

To configure Elpy specifically for a single project, you can use
Emacs' `Directory Variables`_. Elpy provides a simple interface to
this.

.. command:: elpy-set-project-variable

   Set or change the value of a project-wide variable. With a prefix
   argument, the value for the variable is removed.

   This only takes effect in new buffers.

.. _Directory Variables: https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html


Completion
==========

When you type Python code, Elpy will try and figure out possible
completions and provide them in a suggestion window. If Elpy doesn't
do so automatically, you can force it to complete right where you are.

.. command:: elpy-company-backend
   :kbd: M-TAB

   Provide completion suggestions for a completion at point.

You can use cursor keys or :kbd:`M-n` and :kbd:`M-p` to scroll through
the options, :kbd:`RET` to use the selected completion, or :kbd:`TAB`
to complete the common part.

On any completion option, :kbd:`C-d` or :kbd:`<f1>` will display a
temporary window with documentation. :kbd:`C-w` will display a
temporary window showing the source code of the completion to get some
context.

Elpy uses `Company Mode`_ for the completion interface, so its
documentation is a good place for further information.

.. _Company Mode: https://company-mode.github.io/


Navigation
==========

Elpy supports some advanced navigation features inside Python
projects.

.. command:: elpy-goto-definition
   :kbd: M-.

   Go to the location where the identifier at point is defined. This
   is not always easy to make out, so the result can be wrong. Also,
   the backends can not always identify what kind of symbol is at
   point. Especially after a few indirections, they have basically no
   hope of guessing right, so they don't.

.. command:: elpy-goto-definition-other-window
   :kbd: C-x 4 M-.

   Same as `elpy-go-to-definition` (with the same caveats) but goes to
   the definition of the symbol at point in other window, if defined.

.. command:: pop-tag-mark
   :kbd: M-*

   Go back to the last place where :kbd:`M-.` was used, effectively
   turning :kbd:`M-.` and :kbd:`M-*` into a forward and backward
   motion for definition lookups.

.. command:: elpy-occur-definitions
   :kbd: C-c C-o

   Search the buffer for a list of definitions of classes and functions.


If you use an Emacs version superior to 25, elpy will define the
necessary backends for the `xref`_ package.

.. command:: xref-find-definitions
   :kbd: M-.

   Find the definition of the identifier at point.

.. command:: xref-find-definition-other-window
   :kbd: C-x 4 .

   Like :kbd:`M-.` but switch to the other window.

.. command:: xref-find-definition-other-frame
   :kbd: C-x 5 .

   Like :kbd:`M-.` but switch to the other frame.

.. command:: xref-pop-marker-stack
   :kbd: M-,

   Go back to the last place where :kbd:`M-.` was used, effectively
   turning :kbd:`M-.` and :kbd:`M-,` into a forward and backward
   motion for definition lookups.

.. command:: xref-find-references
   :kbd: M-?

   Find references to the identifier at point.
   With prefix argument, prompt for the identifier.

.. command:: xref-find-apropos
   :kbd: C-M-.

   Find all meaningful symbols that match a given pattern.

.. _xref: https://www.gnu.org/software/emacs/manual/html_node/emacs/Xref.html


Interactive Python
==================

Emacs can run a Python interpreter in a special buffer, making it much
easier to send code snippets over.

.. command:: elpy-shell-switch-to-shell
   :kbd: C-c C-z

   Switch to buffer with a Python interpreter running, starting one if
   necessary.

   Do note that Elpy only starts a single interactive Python process.
   This process will inherit the current virtual env. If you want to
   start a Python interpreter with a different virtual env, you can
   either kill the existing one, or rename the buffer.

.. command:: elpy-shell-send-region-or-buffer
   :kbd: C-c C-c

   Whenever you are in an Elpy buffer, :kbd:`C-c C-c` will send Python
   code to the subprocess. If there is an active region, that region
   will be sent; if not, the whole buffer is sent.

   This command will also escape any uses of the ``if __name__ ==
   '__main__'`` idiom, to prevent accidental execution of a script. If
   you want this to be evaluated, pass a prefix argument with
   :kbd:`C-u`.

.. command:: elpy-shell-send-current-statement
   :kbd: C-RET

   Send current statement to Python shell.

   This command sends statements to shell without indentation. If you
   send nested statements, shell will trow ``IndentationError``. To send
   nested statements, it is recommended to select region and run
   ``elpy-shell-send-region-or-buffer``

.. command:: python-shell-send-defun
   :kbd: C-M-x

   Similar to :kbd:`C-c C-c`, this will send the code of the current
   top level class or function to the interactive Python process.

.. command:: elpy-shell-kill
   :kbd: C-c C-k

   Kill the current python shell.
   If ``elpy-dedicated-shells`` is non-nil,
   kill the current buffer dedicated shell.

.. command:: elpy-shell-kill-all
   :kbd: C-c C-K

   Kill all active python shells.

.. command:: elpy-use-ipython
.. command:: elpy-use-cpython

   Use these commands, either interactively or from your ``.emacs``,
   to set the interactive interpreter to either ipython or cpython. As
   ipython requires some more setup work in older Emacsen, these will
   take care of the right setup for you.

   As an IPython user, you might be interested in the `Emacs IPython
   Notebook`_, too.

.. _IPython: http://ipython.org/
.. _Emacs IPython Notebook: https://tkf.github.io/emacs-ipython-notebook/


Syntax Checking
===============

Whenever you save a file, Elpy will run a syntax check and highlight
possible errors or warnings inline.

.. command:: elpy-flymake-next-error
   :kbd: C-c C-n
.. command:: elpy-flymake-previous-error
   :kbd: C-c C-p

   You can navigate between any error messages with these keys. The
   current error will be shown in the minibuffer.

Elpy uses the built-in `Flymake`_ library to find syntax errors on the
fly, so see there for more configuration options.

.. _Flymake: https://www.gnu.org/software/emacs/manual/html_node/flymake/index.html#Top


.. command:: elpy-check
   :kbd: C-c C-v

   Alternatively, you can run a syntax check on the current file where
   the output is displayed in a new buffer, giving you an overview and
   allowing you to jump to the errors from there.

   With a prefix argument, this will run the syntax check on all files
   in the current project.

.. option:: python-check-command

   To change which command is used for syntax checks, you can
   customize this option. By default, Elpy uses the ``flake8``
   program, which you have to install separately. The
   :command:`elpy-config` command will prompt you to do this if Elpy
   can't find the program.

   It is possible to create a single virtual env for the sole purpose
   of installing ``flake8`` in there, and then simply link the command
   script to a directory inside your :envvar:`PATH`, meaning you do
   not need to install the program in every virtual env separately.


Documentation
=============

Elpy provides a single interface to documentation.

.. command:: elpy-doc
   :kbd: C-c C-d

   When point is on a symbol, Elpy will try and find the documentation
   for that object, and display that. If it can't find the
   documentation for whatever reason, it will try and look up the
   symbol at point in pydoc. If it's not there, either, it will prompt
   the user for a string to look up in pydoc.

   With a prefix argument, Elpy will skip all the guessing and just
   prompt the user for a string to look up in pydoc.


Testing
=======

Testing is an important part of programming. Elpy provides a central
interface to testing, which allows for a good workflow for tests.

Elpy's test interface is built around Emacs' `compilation framework`_.
Elpy will run test commands as a compilation job, with all the
advantages this brings.

.. _compilation framework: https://www.gnu.org/software/emacs/manual/html_node/emacs/Compilation.html

.. command:: elpy-test
   :kbd: C-c C-t

   Start a test run. This uses the currently configured test runner to
   discover and run tests. If point is inside a test case, the test
   runner will run exactly that test case. Otherwise, or if a prefix
   argument is given, it will run all tests.

.. command:: elpy-set-test-runner

   This changes the current test runner. Elpy supports the standard
   unittest discovery runner, the Django discovery runner, nose and
   py.test. You can also write your own, as described in :ref:`Writing
   Test Runners`.

   Note on Django runners: by default, elpy runs Django tests with
   :kbd:`django-admin.py`. You must set the environment variable
   :envvar:`DJANGO_SETTINGS_MODULE` accordingly. Alternatively, you can set
   **elpy-test-django-with-manage** to **t** in order to use your
   project's :kbd:`manage.py`. You then don't need to set the environment
   variable, but change virtual envs (see `virtualenvwrapper.el`_).

This enables a good workflow. You write a test and use :kbd:`C-c C-t`
to watch it fail. You then go to your implementation file, for example
using :kbd:`C-u C-c C-f`, and make the test pass. You can use a key
bound to ``recompile`` (I use :kbd:`<f5>` for this) to just re-run
that one test. Once that passes, you can use :kbd:`C-c C-t` again to
run all tests to make sure they all pass as well. Repeat.

For an even more automated way, you can use `tdd.el`_, which will run
your last compile command whenever you save a file.

.. _tdd.el: https://github.com/jorgenschaefer/emacs-tdd/

.. _virtualenvwrapper.el: https://github.com/porterjamesj/virtualenvwrapper.el


Refactoring
===========

Elpy supports various forms of refactoring Python code.

.. command:: elpy-multiedit-python-symbol-at-point
   :kbd: C-c C-e

   Edit all occurrences of the symbol at point at once. This will
   highlight all such occurrences, and editing one of them will edit
   all. This is an easy way to rename identifiers.

   If the backend does not support finding occurrences (currently only
   Jedi does), or if a prefix argument is given, this will edit
   syntactic occurrences instead of semantic ones. This can match more
   occurrences than it should, so be careful. You can narrow the
   current buffer to the current function using :kbd:`C-x n d` to
   restrict where this matches.

   Finally, if there is a region active, Elpy will edit all
   occurrences of the text in the region.


.. command:: elpy-format-code
   :kbd: C-c C-r f

   Format code using the available formatter.

   This command formats code using `yapf`_ or `autopep8`_ formatter. If a
   region is selected, only that region is formatted. Otherwise current
   buffer is formatted.

.. _autopep8: https://github.com/hhatto/autopep8
.. _yapf: https://github.com/google/yapf

.. command:: elpy-importmagic-fixup
   :kbd: C-c C-r i

   Query for new imports of unresolved symbols, and remove unreferenced
   imports. Also sort the imports in the import statement blocks.



.. command:: elpy-refactor
   :kbd: C-c C-r r

   Run the Elpy refactoring interface for Python code.

   This command uses `rope`_ package and provides various refactoring
   options depending on the context.

.. _rope: https://github.com/python-rope/rope


Django
======

Elpy has basic Django support such as parsing either `manage.py` or `django-admin.py` (If it
does not find `manage.py` it falls back to `django-admin.py`) for command completion assistance.
Can also start `runserver` automatically and you can give an ip address and port.

.. command:: elpy-django-command
   :kbd: C-c C-x c

   Choose what command you'd like to run via `django-admin.py` or `manage.py`.

.. command:: elpy-django-runserver
   :kbd: C-c C-x r


   Start the development server command, `runserver`. Default arguments are `127.0.0.1` for
   ip address and `8000` for port. These can be changed via ``elpy-django-server-ipaddr`` and
   ``elpy-django-server-port``.


Profiling
=========

Elpy allows to profile asynchronously python scripts using `cProfile`.

.. command:: elpy-profile-buffer-or-region

   Send the current buffer or region to the profiler and display the result with
   ``elpy-profile-visualizer``.
   The default visualizer is `snakeviz`_, a browser-based graphical profile viewer that can be installed with `pip install snakeviz`.
   If the profiling fails, the python error output is displayed.
.. _snakeviz: https://jiffyclub.github.io/snakeviz/
