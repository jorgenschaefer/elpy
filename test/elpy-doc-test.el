;;; This is a highly interactive function. It's annoyingly difficult
;;; to test those, so just check that it works.

(ert-deftest elpy-doc ()
  (elpy-testcase ()
    (python-mode)
    (elpy-mode)
    (insert "sys")
    (elpy-doc)
    (with-current-buffer "*Python Doc*"
      (should (re-search-forward "This module provides access")))))

(ert-deftest elpy-doc-should-find-documentation-from-inside-arguments ()
  (elpy-testcase ()
    (python-mode)
    (elpy-mode)
    (insert "import socket\n"
            "socket.getaddrinfo(socket.gethostname(")
    (save-excursion
      (insert "))\n"))

    (elpy-doc)

    (with-current-buffer "*Python Doc*"
      (should (re-search-forward "gethostname")))

    (erase-buffer)


    (insert "import socket\n"
            "socket.getaddrinfo(socket.gethostname(), ")
    (save-excursion
      (insert ")\n"))

    (elpy-doc)

    (with-current-buffer "*Python Doc*"
      (should (re-search-forward "getaddrinfo")))))

(ert-deftest elpy-doc-should-gather-doc-in-shell ()
  (elpy-testcase ()
    (python-mode)
    (elpy-mode)
    (let ((elpy-get-info-from-shell t)
          (elpy-get-info-from-shell-timeout 1)
          (python-shell-completion-native-enable nil))
      (elpy-shell-get-or-create-process)
      (python-shell-send-string "def foo(a, b):\n    \"\"\"Foo doc\"\"\"\n    return a + b")
      (python-shell-send-string "import os.path")
      (python-shell-send-string "print('DONE')")
      (with-current-buffer  "*Python*"
        (elpy/wait-for-output "DONE"))
      ;; Get doc for 'foo'
      (insert "\nfoo")
      (elpy-doc)
      (with-current-buffer "*Python Doc*"
        (should (re-search-forward "Foo doc")))
      ;; Get doc for 'os.path'
      (insert "\nos.path")
      (elpy-doc)
      (with-current-buffer "*Python Doc*"
        (should (re-search-forward "Help on module posixpath"))))))
