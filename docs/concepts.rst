========
Concepts
========

.. default-domain:: el

The RPC Process
===============

Elpy works by starting a Python process in the background and
communicating with it through a basic *Remote Procedure Call* (RPC)
interface. Ideally, you should never see this process and not worry
about it, but when things don't work as expected, it's good to know
what's going on in the background.

Every project and virtual env combination gets their own RPC process.
You can see them both in the process list (:kbd:`M-x list-process`) as
well as in the buffer list (:kbd:`C-x C-b`) as buffers named
``*elpy-rpc[...]*``.

RPC processes are used to provide code completion, documentation and
other features. To do so, they use python packages (jedi, yapf, rope, ...)
that are installed in a dedicated virtualenv
(``.emacs.d/elpy/rpc-venv`` by default). Those packages can be updated
through the configuration panel (accessible with :kbd:`M-x
elpy-config`).

By default, Elpy will also find the :index:`library root` of the
current file and pass that to the RPC functions. The library root is
the directory from which the current file can be imported.

There are a few options and commands related to the RPC process.

.. command:: elpy-rpc-restart

   Close all running RPC processes. Elpy will re-start them on demand
   with current settings.

.. option:: elpy-rpc-python-command

   The Python interpreter Elpy should use to run the RPC process. This
   defaults to ``"python"``, which should be correct for most cases,
   as a virtual env should make that the right interpreter.

   Please do note that this is *not* an interactive interpreter, so do
   not set this to ``"ipython"`` or similar.

.. option:: elpy-rpc-virtualenv-path

   Path to the virtualenv used by the RPC.

   Can be `'default` (create a dedicated virtualenv
   ``.emacs.d/elpy/rpc-venv``), `'global` (use the global system
   environment), `'current` (use the currently active environment), a
   virtualenv path or a function returning a virtualenv path.

   If the default virtual environment does not exist, it will be
   created using `elpy-rpc-python-command` and populated with the
   needed packages from `elpy-rpc--get-package-list`.

.. option:: elpy-rpc-large-buffer-size

   The size in character starting from which Elpy will transfer buffer
   contents via temporary files instead of via the normal RPC
   mechanism.

   When Elpy communicates with the RPC process, it often needs to tell
   Python about the contents of the current buffer. As the RPC
   protocol encodes all data in JSON, this can be a bit slow for large
   buffers. To speed things up, Elpy can transfer file contents in
   temporary files, which is a lot faster for large files, but
   slightly slower for small ones.

.. option:: elpy-rpc-pythonpath

   A directory to add to the :envvar:`PYTHONPATH` for the RPC process.
   This should point to the directory where the ``elpy`` module is
   installed. Usually, there is no need to change this.


Backends
========

For introspection and analysis of Python sources, Elpy mainly relies
on `Jedi`_, a python package for static code analysis.

Due to the dynamic nature of python and its minimalist structure syntax, python code can be difficult to understand in certain situations.
Jedi documentation provides some tips_ to make jedi job easier.

.. _Jedi: https://github.com/davidhalter/jedi/
.. _tips: https://jedi.readthedocs.io/en/latest/docs/features.html#recipes


Virtual Envs
============

Elpy has full support for Python's virtual envs. Every RPC process is
associated with a specific virtual env and completions are done based
on that environment.

Outside of RPC processes, though, it is not easy to have more than one
virtual env active at the same time. Elpy allows you to set a single
global virtual env and change it whenever you like, though.

.. command:: pyvenv-workon
.. command:: pyvenv-activate
.. command:: pyvenv-deactivate

   These commands are the main interaction point with virtual envs,
   mirroring the normal :program:`activate` and :program:`deactivate`
   commands of virtual envs and the :program:`workon` command of
   ``virtualenvwrapper.sh``.

   The :command:`pyvenv-workon` command will allow auto-completion of
   existing virtual envs and also supports virtualenvwrapper's setup
   hooks to set environment variables.


Elpy won't pollute your Emacs command namespaces, but it might be an
idea to create an alias for the workon command:

.. code-block:: cl

   (defalias 'workon 'pyvenv-workon)


Modules
=======

As the last concept, Elpy has a number of optional features you can
enable or disable as per your preferences.

.. option:: elpy-modules

   The list of modules to activate by default. See the section on
   :ref:`Writing Modules` for details on how to write your own modules.
