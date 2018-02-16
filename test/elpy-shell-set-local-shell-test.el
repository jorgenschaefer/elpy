(ert-deftest elpy-shell-set-local-shell-should-set-a-local-shell ()
  (elpy-testcase ()
    (python-mode)
    (elpy-mode 1)
    (elpy-shell-set-local-shell "localshell")
    (save-excursion
      (elpy-shell-switch-to-shell)
      (should (string= (buffer-name)
                       (format "*Python[localshell]*"))))
    (elpy-shell-set-local-shell "anotherlocalshell")
    (save-excursion
      (elpy-shell-switch-to-shell)
      (should (string= (buffer-name)
                       (format "*Python[anotherlocalshell]*"))))
    (elpy-shell-set-local-shell "Global")
    (save-excursion
      (elpy-shell-switch-to-shell)
      (should (string= (buffer-name)
                       "*Python*")))))
