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
