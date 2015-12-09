========
Concepts
========

.. default-domain:: el

Configuration
=============

You can easily configure Elpy to your own preferences. All *Customize
Options* below are accessible via this interface. Elpy builds heavily
upon existing extensions for Emacs. The configuration interface tries
to include the options for those as well.

.. command:: elpy-config

   Show the current Elpy configuration, point out possible problems,
   and provide a quick interface to relevant customization options.

   Missing packages can be installed right from this interface. Be
   aware that this does use your currently-selected virtual env. If
   there is no current virtual env, it will suggest installing
   packages globally. This is rarely what you want.

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
on external libraries. It currently supports two different ones which
can not be used at the same time. But you can switch between them.
They have certain advantages and disadvantages respectively to each
other, making the choice not trivial.

`Rope`_ is a refactoring library that also provides code
introspection. It's quite good at finding completions, but can not
provide locations for
:command:`elpy-multiedit-python-symbol-at-point`. It also has problems
with large projects.

`Jedi`_ is a more lightweight library, but has some problems coping
with badly-formatted Python.

.. _Jedi: https://github.com/davidhalter/jedi/
.. _Rope: https://github.com/python-rope/rope

.. option:: elpy-rpc-backend

   Elpy will use whichever library is installed, preferring Rope if
   both are available. If you dislike Elpy's default preference, you
   can set this option to the backend it should prefer.


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
