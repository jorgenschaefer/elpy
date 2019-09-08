===
FAQ
===


Q: Can I tell elpy not to load one of the minor modes it enables by default?
============================================================================

**A:** You can simply remove the appropriate symbol from
``elpy-modules``.


Q: How do I make Elpy work well with org-mode?
==============================================

**A:** You can try the following customization by @mankoff:

.. code-block:: elisp

    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "--pylab=osx --pdb --nosep --classic"
          python-shell-prompt-regexp ">>> "
          python-shell-prompt-output-regexp ""
          python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
          python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
          python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

See `#191`_ for a discussion and background.

.. _#191: https://github.com/jorgenschaefer/elpy/issues/191


Q: Can I get documentation as pop-ups next to the completion pop-up?
====================================================================

**A:** Not by default with company-mode, but there’s expez’ excellent
company-quickhelp_ package you can install for this feature. It’s
available from MELPA.

.. _company-quickhelp: https://github.com/expez/company-quickhelp


Q: How to solve company, yasnippet conflicts?
=============================================

**A:** Add this snippet to your emacs configuration to avoid that.

.. code-block:: elisp

    (defun company-yasnippet-or-completion ()
      "Solve company yasnippet conflicts."
      (interactive)
      (let ((yas-fallback-behavior
             (apply 'company-complete-common nil)))
        (yas-expand)))

    (add-hook 'company-mode-hook
              (lambda ()
                (substitute-key-definition
                 'company-complete-common
                 'company-yasnippet-or-completion
                 company-active-map)))


Q: How do I install development version of elpy?
================================================

**A:** Remove existing elpy if you already installed it.

.. code-block:: bash

    rm -rf ~/.emacs.d/elpa/elpy*

Install dependencies

.. code-block:: elisp

    (package-install 'company)
    (package-install 'yasnippet)
    (package-install 'pyvenv)
    (package-install 'highlight-indentation)
    (package-install 'find-file-in-project)
    (package-install 's)

Get latest code from github

.. code-block:: bash

    mkdir ~/Projects
    cd ~/Projects
    git clone https://github.com/jorgenschaefer/elpy

Add the following to your ~/.emacs.d/init.el or ~/.emacs:

.. code-block:: elisp

    (add-to-list 'load-path "~/Projects/elpy")
    (load "elpy" nil t)
    (elpy-enable)

To update, run ``git pull --rebase``, ``M-x load-library RET elpy RET`` and ``M-x elpy-rpc-reload``.
