## New in Elpy 1.4.2

- Minor bugfix to prevent an error from projectile-project-root to
  interfere with Elpy’s project finding strategy.

## New in Elpy 1.4.1

- Elpy now sets project-wide preferences for Rope, enabling completion
  in the sys package, among others.
- An error is avoided in the Jedi backend when trying to go to symbols
  in compiled packages.
- A compatibility alias was added for nose.el, which insists on
  breaking backwards compatibility with Emacs 24.x.

## New in Elpy 1.4.0

- Elpy has moved to its own ELPA. Make sure to update your
  package-archives (as described above).
- For a file in a Projectile-managed project is opened, Elpy will now
  use Projectileâ€™s project root.
- When the user has set a valid python-check-command, elpy will now
  refrain from overriding it.
- On Windows, elpy is now using the pythonw.exe interpreter for the
  RPC process, as that seems to be causing fewer issues.
- And various smaller bugfixes.

## New in Elpy 1.3.0

- virtualenv.el has been replaced by pyvenv.el, as that library offers
  more features.
- elpy-rpc-restart now works globally, not just in Elpy buffers.
- Elpy does not try to complete in comments anymore.
- The new command elpy-rpc-traceback gives access to the last stack
  trace in the Elpy backend, helping with debugging problems.
- The flymake check function is now run with the current directory as
  / to avoid accidental imports.
- Ensure correct handling of yas-snippet-dirs by Elpy. This variable
  can be a string, so ensure it’s a list before adding to it.
- The new variable elpy-show-installation-instructions can be used to
  disable the installation screen.
- Fix a very nasty bug causing spurious empty lines in a buffer or
  consume 100% CPU in certain situations when using the Jedi backend.
  Thanks to Matthias Dahl for finding this bug.
- Various other bugfixes.

## New in Elpy 1.2.1

Bugfix release.

- The refactoring was not ported to the new asynchronous API,
  resulting in an error when refactoring was attempted.
- The project root now always returns a directory. Too many parts of
  elpy relies on this. If the project root turns out to be your home
  directory, elpy will warn you about it.
- Elpy now works correctly with Emacs 24.2. There were some
  compatibility functions missing.
- Blocking RPC calls now do not block for one second even if there is
  process output.

## New in Elpy 1.2

- Elpy now uses asynchronous RPC. This means that Emacs should not
  freeze anymore while eldoc or auto-complete functions run.
- `elpy-shell-send-region-or-buffer` will now remove common
  indentation of the region, making it possible to easily send parts
  of an if statement or function body without manually adjusting the
  indentation.
- The Python package depends on `flake8`, and will also try to be
  smarter when detecting `flake8` for on-the-fly checking.
- `elpy-check` can be run with a prefix argument to check the whole
  project, instead of only the current file.
- `elpy-rgrep-symbol` now ignores a few common directories (`.tox`,
  `build`, `dist`).
- When using the rope backend, Elpy will not create the `.ropeproject`
  folders anymore. This should keep projects a lot cleaner.

## New in Elpy 1.1

- Elpy now always uses the root directory of the package as the
  project root; this should avoid some confusion and improve
  auto-completion suggestions
- `elpy-shell-send-region-or-buffer` now accepts a prefix argument to
  run code wrapped behind `if __name__ == '__main__'`, which is
  ignored by default
- `elpy-project-root` is now a safe local variable and can be set from
  file variables
- Elpy now supports project-specific RPC processes, see
  `elpy-rpc-project-specific` for how to use this
- `M-*` now works to go back where you came from after a `M-.`
- Elpy now ships with a few dedicated snippets for YASnippet
- Support and require Jedi 0.6.0

## New in Elpy 1.0

- Version 0.9 was a release candidate, so this release focused on bug
  fixes instead of new features.
- `elpy-enable` now takes an optional argument that skips variable
  initialization for those users who prefer their own defaults for
  other modes.
- `python-check.sh` has been removed from Elpy, as the flake8 tool from
  pypi does everything it does, only better.
- Elpy will now start the helper subprocess in the root directory,
  avoiding accidental Python path clobbering.

## New in Elpy 0.9

- Elpy now officially support Python 2.6, 2.7 and 3.3 on Emacs 24.2
  and 24.3, with continuous integration tests thanks to
  [Travis CI](https://travis-ci.org/).
- Extended support for Pydoc. `C-u C-c C-d` will now prompt for an
  auto-completed symbol to run Pydoc on. The pydoc output will be
  formatted and placed in a help buffer for easy review.
- Refactoring support is back. `C-c C-r` will pop up a refactoring
  wizard offering various refactoring options. Most of them depend on
  the presence of Rope, though, even if Jedi is used as a completion
  backend.
- The Rope backend has been extended to provide completions for
  modules in an import clause.
- New refactoring option: Add missing imports. This will search for
  undefined symbols in the current file and automatically add
  appropriate imports.
- `C-c C-c (elpy-rgrep-symbol)` now prompts for a regexp when a prefix
  argument is given instead of using the symbol at point.

## New in Elpy 0.8

### Python Backend Rewrite

- Elpy does not use Pymacs, Ropemacs and Ropemode anymore, but instead
  provides its own Python interface with the elpy package on PyPI.
- This not only should improve performance, but also enables using
  Jedi as an alternative backend for completion. Use `M-x
  elpy-set-backend` to change between rope and jedi. For now, this
  does disable all refactoring support, though.

### Project Support

- Elpy now has built-in project support. The interface is rather
  simple: You can set `elpy-project-root` to the correct value in
  `.dir-locals.el`, or just rely on the automatic detection. If you
  change your mind, you can always just `elpy-set-project-root`.
- New dependency: Find File in Project (ffip), bound to `C-c C-f` by
  default. This will allow you to find files anywhere in your project
  using a search-as-you-type interface like ido.
- New dependency: nose, bound to `C-c C-t` by default. This will run
  the nosetests binary in the root of your current library directory.
  You can restrict the tests being run to the current test or the
  current module by adding prefix arguments.
- New function: Recursive grep for symbol, bound to `C-c C-s` by
  default. This will search for the symbol at point in the whole
  project.

### New dependencies

- idomenu, bound to `C-c C-j` by default. This replaces the standard
  imenu interface with an ido-based search-as-you-type interface for
  definitions in the current buffer.
- virtualenv.el, replacing pyvirtualenv.el). Use `M-x
  virtualenv-workon` to enable a virtualenv.
- iedit.el, bound to `M-,` by default. This highlights all occurrences
  of the symbol at point or the active region in the current buffer or
  narrowing. When you edit any of them, all others will be edited the
  same. This allows some basic and very quick refactoring.
- New variable `elpy-default-minor-modes` which is run by `elpy-mode`
  on startup. If you don’t want to use some modes, remove them from
  here.

### Key Bindings and Functions

- The key bindings have been reworked and cleaned up. Sorry, this
  might cause confusion.
- Yasnippet is now on its own keybinding, `C-c C-i`, instead of
  sharing the auto-complete interface. This was done because some
  snippets conflicted with legitimate, unsnippy completions.
- New function: Occur Definitions, bound to `C-c C-o` by default. This
  will run the standard occur command to show definitions (classes and
  functions) in your current buffer, giving you a very quick outline
  and the ability to jump to different definitions quickly.
- New function: Show Defun, bound to `C-c C-q` by default. This will
  show the current method and possibly class in the mode line, which
  is helpful in long functions.
- New functions: Forward/backward definition, bound to `M-n` and `M-p`
  as well as `<M-down>` and `<M-up>` by default. These will jump to
  the next or previous definition (class or function), helping with
  quick navigation through a file.

### Miscellaneous

- The documentation function (`C-c C-d`) now uses pydoc when a prefix
  arg is given.
- The web search function (`C-c C-w`) now searches for the current
  symbol by default. The tab-completing web documentation interface
  was removed and is scheduled to be replaced with a new pydoc
  interface in future versions.
- The `python-check.sh` is now shipped with elpy. If you load elpy.el
  before you load python.el, it should be the default
  `python-check-command`.
