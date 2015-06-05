=======
Editing
=======

.. default-domain:: el

Emacs Basics
============

Elpy is an extension to Emacs, and as such the standard bindings in
Emacs are available. This manual is not meant to be an introduction to
Emacs, but this section will still highlight some features in Emacs
that are especially useful for Python editing.

Movement keys in Emacs often use ``fbnp`` for forward, backward, next
(down) and previous (up). ``k`` and ``backspace`` (``DEL``) are for
deleting. These are combined with the ``Control``, ``Meta`` and
``Control-Meta`` modifiers. Control generally refers to the simplest
form. ``C-f`` moves one character forward. Meta changes this to affect
*words*, that is, consecutive sequences of alphanumeric characters.
The Control-Meta combination then affects whole expressions.

In the following table, ``|`` refers to the position of point.

+--------------------+-----------+--------------------+
| Before             | Key       | After              |
+====================+===========+====================+
| ``|hello_world``   | ``C-f``   | ``h|ello_world``   |
+--------------------+-----------+--------------------+
| ``|hello_world``   | ``M-f``   | ``hello|_world``   |
+--------------------+-----------+--------------------+
| ``|hello_world``   | ``C-M-f`` | ``hello_world|``   |
+--------------------+-----------+--------------------+

Expression-based commands will also work on strings, tuples,
dictionaries, or any balanced groups of parentheses. This works for
all movement keys (:kbd:`f`, :kbd:`b`, :kbd:`n`, :kbd:`p``), with
*next* and *previous* moving to the next or previous group of parens.
It also works with forward and backward deletion (:kbd:`d` and
:kbd:`DEL`/:kbd:`<backspace>`, respectively) for character and word
groups, but not for expressions. To delete the expression after point,
use :kbd:`C-M-k`. For the expression before point, you can use
:kbd:`C-M-b C-M-k`.

If you enable :command:`subword-mode`, Emacs will also consider
CamelCase to be two words instead of one for the purpose of these
operations.

In addition to the above, Emacs also supports moving up or down inside
nested parentheses groups. :kbd:`C-M-d` will move *down* into the next
enclosed group of parentheses, while :kbd:`C-M-u` will move *up* to
the directly enclosing group of parentheses.

Finally, a lot of Elpy's commands change their behavir when the
:index:`prefix argument` is given. That is, hit :kbd:`C-u` before the
command. In Elpy, the prefix argument often disables any attempt by
the command at being smart, in case it would get it wrong.


Indentation Blocks
==================

Elpy adds a new concept to Emacs, called :index:`indentation blocks`.
These are blocks defined by their indentation level, the natural block
of Python code. They are manipulated with the cursor keys in
combination with either the control or meta keys (but not both).

.. command:: elpy-nav-forward-block
   :kbd: C-down
.. command:: elpy-nav-previous-iblock
   :kbd: C-up
.. command:: elpy-nav-backward-indent
   :kbd: C-left
.. command:: elpy-nav-forward-indent
   :kbd: C-right

   The control key allows navigation. Up and down will skip between
   blocks of the same indentation level, allowing you to quickly find
   the end of a long for loop, for example. Left and right jump to the
   closest preceding or following occurrence of a higher or lower
   indentation level.

.. command:: elpy-nav-move-line-or-region-down
   :kbd: M-down
.. command:: elpy-nav-move-line-or-region-up
   :kbd: M-up
.. command:: elpy-nav-move-line-or-region-left
   :kbd: M-left
.. command:: elpy-nav-move-line-or-region-right
   :kbd: M-right

   With meta, the cursor keys will move indentation blocks. Up and down
   will swap the position of the current block with the one above or
   below it. Left and right will change the indentation level.
