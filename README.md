# Elpy, the Emacs Lisp Python Environment

Elpy is an Emacs package to bring powerful Python editing to Emacs. It
combines a number of other packages, both written in Emacs Lisp as
well as Python.

## Documentation

You can find the documentation [on the wiki][wiki].

[wiki]: https://github.com/jorgenschaefer/elpy/wiki

## Quick Installation

You can also read the [detailed installation instructions][Installation].

[Installation]: https://github.com/jorgenschaefer/elpy/wiki/Installation

First, install the required Python packages:

```
pip install elpy rope pyflakes pep8
```

(Note: If you are installing the development version of elpy, do not
install the elpy Python package, but simply put the repository in your
`PYTHONPATH`.)

Evaluate this in your `*scratch*` buffer:

```Lisp
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
```

Then run `M-x package-install RET elpy RET`.

Finally, add the following to your `.emacs`:

```Lisp
(package-initialize)
(elpy-enable)
```

Done.
