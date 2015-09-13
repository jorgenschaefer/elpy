(ert-deftest elpy-module-yasnippet-global-init ()
  (elpy-testcase ()
    (elpy-module-yasnippet 'global-init)

    (should (member (concat (file-name-directory (locate-library "elpy"))
                            "snippets/")
                    yas-snippet-dirs))))

(ert-deftest elpy-module-yasnippet-global-stop ()
  (elpy-testcase ()
    (elpy-module-yasnippet 'global-stop)

    (should-not (member (concat (file-name-directory (locate-library "elpy"))
                                "snippets/")
                        yas-snippet-dirs))))


(ert-deftest elpy-module-yasnippet-buffer-init ()
  (elpy-testcase ()
    (python-mode)
    (elpy-mode)

    (elpy-module-yasnippet 'buffer-init)

    (should yas-minor-mode)))

(ert-deftest elpy-module-yasnippet-buffer-stop ()
  (elpy-testcase ()
    (python-mode)
    (elpy-mode)
    (yas-minor-mode 1)

    (elpy-module-yasnippet 'buffer-stop)

    (should-not yas-minor-mode)))
