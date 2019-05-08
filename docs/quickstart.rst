Quickstart
==========

Elpy will automatically be enabled when opening a python file (ending with `.py`).

Elpy will out-of-the-box provides code completion, syntax error highlighting and display of function signature in the modeline.

Useful keybindings
------------------

`C-c C-d` will display the inline documentation for the thing currently under the cursor (function or module).
The documentation will pop in a different buffer, that can be closed with `q`.

`C-c C-c` will evaluate the current script (or region if you have something selected) in an interactive python shell.
A buffer with the python shell will pop in a different buffer.
You can switch between your python script and the shell with `C-c C-z`.


Go further
----------

Elpy provides a set of modules, that are not all activated by default.
You can check which one are available and activate new ones using Emacs configuration system: `M-x customize-variable RET elpy-modules RET`

Todo: Add a word on some interesting advanced functionnalities
