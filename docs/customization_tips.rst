==================
Customization tips
==================

.. default-domain:: el

Use flycheck instead of flymake
===============================

.. code-block:: elisp

    (when (load "flycheck" t t)
      (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
      (add-hook 'elpy-mode-hook 'flycheck-mode))


Enable emacs 26 flymake indicators in an otherwise light modeline
=================================================================

.. code-block:: elisp

    (setq elpy-remove-modeline-lighter t)

    (advice-add 'elpy-modules-remove-modeline-lighter
                :around (lambda (fun &rest args)
                          (unless (eq (car args) 'flymake-mode)
                            (apply fun args))))

See also the `associated issue`_

.. _associated issue: https://github.com/jorgenschaefer/elpy/issues/1422


An alternative to ``elpy-goto-definition``
==========================================

You may sometimes find when you try to navigate to a function/class definition with elpy-goto-definition_ (``M-.``), that instead of jumping to the definition, you get the message "No definition found". If you see this error often (because of the nature of the code you work on), you can use the following function instead of/in addition to ``elpy-goto-definition``:

.. _elpy-goto-definition: http://elpy.readthedocs.org/en/latest/ide.html#command-elpy-goto-definition

.. code-block:: elisp

    (defun elpy-goto-definition-or-rgrep ()
      "Go to the definition of the symbol at point, if found. Otherwise, run `elpy-rgrep-symbol'."
        (interactive)
        (ring-insert find-tag-marker-ring (point-marker))
        (condition-case nil (elpy-goto-definition)
            (error (elpy-rgrep-symbol
                       (concat "\\(def\\|class\\)\s" (thing-at-point 'symbol) "(")))))

This function will try to find the definition of the symbol at point using ``elpy-goto-definition``, but will do elpy-rgrep-symbol_  instead, if the former function fails to return a result. You can bind this function to the key combination of your choice, or you can bind it to ``M-.`` to use it as a replacement for the the default ``goto-definition`` function:

.. code-block:: elisp

    (define-key elpy-mode-map (kbd "M-.") 'elpy-goto-definition-or-rgrep)

.. _elpy-rgrep-symbol: http://elpy.readthedocs.org/en/latest/ide.html#command-elpy-rgrep-symbol


Enable full font locking of inputs in the python shell
======================================================

.. code-block:: elisp

    (advice-add 'elpy-shell--insert-and-font-lock
                :around (lambda (f string face &optional no-font-lock)
                          (if (not (eq face 'comint-highlight-input))
                              (funcall f string face no-font-lock)
                            (funcall f string face t)
                            (python-shell-font-lock-post-command-hook))))

    (advice-add 'comint-send-input
                :around (lambda (f &rest args)
                          (if (eq major-mode 'inferior-python-mode)
                              (cl-letf ((g (symbol-function 'add-text-properties))
                                        ((symbol-function 'add-text-properties)
                                         (lambda (start end properties &optional object)
                                           (unless (eq (nth 3 properties) 'comint-highlight-input)
                                             (funcall g start end properties object)))))
                                (apply f args))
                            (apply f args))))

See details in https://github.com/jorgenschaefer/elpy/issues/1428 and https://debbugs.gnu.org/cgi/bugreport.cgi?bug=32344.
