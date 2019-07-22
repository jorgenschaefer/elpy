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
all movement keys (:kbd:`f`, :kbd:`b`, :kbd:`n`, :kbd:`p`), with
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

Finally, a lot of Elpy's commands change their behavior when the
:index:`prefix argument` is given. That is, hit :kbd:`C-u` before the
command. In Elpy, the prefix argument often disables any attempt by
the command at being smart, in case it would get it wrong.


Moving By Indentation
=====================

.. command:: elpy-nav-forward-block
   :kbd: C-down
.. command:: elpy-nav-backward-block
   :kbd: C-up

   These commands are used to navigate between lines with same
   indentation as the current line. Point should be placed on the
   first non-whitespace character of the line and then use `C-down`
   to move forward or `C-up` to move backward.

.. command:: elpy-nav-backward-indent
   :kbd: C-left
.. command:: elpy-nav-forward-indent
   :kbd: C-right

   These commands are used to navigate between indentation levels.
   `C-left` moves point to previous indent level or over previous word.
   `C-right` moves point to next indent level or over the next word.


Moving the Current Region
=========================

.. command:: elpy-nav-move-line-or-region-down
   :kbd: M-down
.. command:: elpy-nav-move-line-or-region-up
   :kbd: M-up
.. command:: elpy-nav-indent-shift-left
   :kbd: M-left
.. command:: elpy-nav-indent-shift-right
   :kbd: M-right

   Elpy can move the selected region (or the current line if no region is
   selected) by using the cursor keys with meta. Left and right will
   dedent or indent the code, while up and down will move it line-wise up
   or down, respectively.
