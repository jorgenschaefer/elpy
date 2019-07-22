==========
Quickstart
==========

.. default-domain:: el

Once installed, Elpy will automatically provide `code completion`_, `syntax error highlighting`_ and function signature (in the modeline) for python files.

Useful keybindings
==================

Elpy has quite a lot of keybindings, but the following ones should be enough to get you started:

.. command:: elpy-shell-send-region-or-buffer
   :kbd: C-c C-c

   Evaluate the current script (or region if something is selected) in an interactive python shell.
   The python shell is automatically displayed aside of your script.

.. command:: elpy-shell-send-statement-and-step
   :kbd: C-RET

   Evaluate the current statement (current line plus the following nested lines).
   Useful for evaluating a function or class definition or a for loop.

.. command:: elpy-shell-switch-to-shell
   :kbd: C-c C-z

   Switch between your script and the interactive shell.

.. command:: elpy-doc
   :kbd: C-c C-d

   Display documentation for the thing under cursor (function or module).
   The documentation will pop in a different buffer, that can be closed with :kbd:`q`.


Go further
==========

Elpy offers a lot of features, including `code navigation`_, `debugging`_, `testing`_, `profiling`_ and support for `virtual environments`_.
Feel free to explore the documentation, everything is there !


.. _code completion: https://elpy.readthedocs.io/en/latest/ide.html#completion
.. _syntax error highlighting: https://elpy.readthedocs.io/en/latest/ide.html#syntax-checking
.. _code navigation: https://elpy.readthedocs.io/en/latest/ide.html#navigation
.. _debugging: https://elpy.readthedocs.io/en/latest/ide.html#debugging
.. _testing: https://elpy.readthedocs.io/en/latest/ide.html#testing
.. _profiling: https://elpy.readthedocs.io/en/latest/ide.html#profiling
.. _virtual environments: https://elpy.readthedocs.io/en/latest/concepts.html#virtual-envs
