# Pyde, the Python Development Environment

Emacs has excellent Python support through a number of packages. The
only problem is that every user needs to set up all of these packages
to work nicely with each other. This mode does not a lot more than to
combine those packages and give them a default configuration for
Python.

## Features

- **Code completion (using auto-complete and rope):**
  Emacs will suggest completions as you type and, after a short
  delay, pop up a select box with proposed completions, including
  docstrings for those completions when available.
- **Snippet Expansion (using yasnippet and auto-complete):**
  Some completion options are highlighted and will expand into full
  code snippets that you just need to fill out.
- **Code hinting (using eldoc and rope):**
  While you write, the minibuffer will show the call signature of
  the current function.
- **Code Navigation (using rope and python.el):**
  Quickly jump to the definition of a function or class, find
  callers of the current function, or browse all definitions in the
  current file. `find-file-at-point' will also find module source
  files from import statements.
- **Inline Documentation (using rope):**
  Read the help() output of the object at point with a quick key shortcut.
- **Python web documentation:**
  Simply access the Python web documentation using a tab-completed
  list of modules and objects.
- **Refactoring (using rope):**
  Use any of multiple powerful refactoring tools, such extracting
  the region to a variable or a separate function, renaming
  identifiers, modules or packages, or just automatically clean up
  your imports.
- **Easy IPython support for those who use it:**
  Simply run (pyde-use-ipython).

## Installation

Pyde requires Emacs 24.

First, you need to install the Python dependencies:

```
easy_install --user rope ropemode ropemacs
```

Sadly, Pymacs itself is not available via pypi, so you need to install
it by hand:

```
git clone https://github.com/pinard/Pymacs.git
cd Pymacs
make
python setup.py install --user
```

Then, add the following to your .emacs:

```Lisp
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
```

Once this is evaluated, you should be able to run `M-x package-install
RET pyde RET`.

Then, add further lines:

```Lisp
(package-initialize)
(pyde-enable)
```

If you want to use IPython (make sure it's installed), add:

```
(pyde-use-ipython)
```

If you find the (Python Pyde yas AC Rope ElDoc Fill) mode line
annoying, also add:

```
(pyde-clean-modeline)
```

## Keybindings

### Indentation and Filling

```
TAB          Indent line or region, or complete
RET          Newline without indentation
C-j          Newline with indentation
C-c <        Shift indentation of line or region left
C-c >        Shift indentation of line or region right
C-M-q        Re-indent the current expression
M-q          Fill the current comment or string
```

### Python Shell Interaction

Emacs can run a Python interpreter in a separate buffer for
interactive work.

```
C-c C-z      Switch to a Python interpreter

C-M-x        Send the current class or function to the interpreter
C-c C-c      Send the current region (if active) or buffer to the interpreter
```

### Code Navigation

Navigate quickly through your source file and project.

```
C-c C-j      Jump to a definition in the current buffer
C-c C-f      Find a file in the current project
C-c C-g C-d  Go to the definition of the function at point
C-c C-g C-c  Find callers of the function at point
C-c C-g C-i  Find implementations of the function at point
C-c C-g C-g  Jump to the global variable used at point

C-M-up       Move to the last opening paren
M-a          Move one statement backwards
M-e          Move one statement forwards
```

### Documentation

Browse documentation quickly, either from Python directly or from the
web.

```
C-c C-v      Run a python checking program (e.g. pyflakes)

C-c C-d      Show documentation of the object at point
C-c C-w C-s  Search the Python web documentation
C-c C-w C-w  Browse the Python web documentation
```

### Refactoring

While Rope provides auto-completion, it's actually a refactoring tool.
Pyde wraps that in a simple interactive refactoring session.

```
C-c C-r      Start refactoring interaction
```

### Project support

Rope uses projects. Usually, you only need to set it up once and say
where the project root is, but these allow you to set up and configure
projects on the fly.

```
C-c C-p C-o  Open a new Rope project
C-c C-p C-c  Close the current Rope project
C-c C-p C-p  Configure the current Rope project
```

## Other Tweaks

The following would overwrite keys that can get in the way when using
auto-complete-mode. I found it's better to just get used to having
a-c-mode around, but if it really annoys you, use these.

```Lisp
(define-key ac-completing-map (kbd "<up>") nil)
(define-key ac-completing-map (kbd "<down>") nil)
(define-key ac-completing-map (kbd "RET") nil)
(define-key ac-completing-map (kbd "<return>") nil)
```

## Dependencies from Source

If you like to live on the edge, get the dependencies as source:

- Pymacs: `git clone git://github.com/pinard/Pymacs.git`
- Ropemode: `hg clone https://bitbucket.org/agr/ropemode`
- Ropemacs: `hg clone https://bitbucket.org/agr/ropemacs`
- auto-complete: `git clone git://github.com/auto-complete/auto-complete.git`
- yasnippet: `git clone git://github.com/capitaomorte/yasnippet.git`

## Name

_Pyde_ rhymes with _Hyde._ Dr. Reckyll was not available for comment.
