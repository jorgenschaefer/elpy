;;; This is a highly interactive function. It's annoyingly difficult
;;; to test those, so just check that it works.

(ert-deftest elpy-doc ()
  (elpy-testcase ()
    (elpy-enable)
    (python-mode)
    (insert "sys")
    (elpy-doc)
    (with-current-buffer "*Python Doc*"
      (should (re-search-forward "This module provides access")))))

;; Does not work with jedi 16.0. Jedi sometimes consider parenthesis as strings
;; (ert-deftest elpy-doc-should-find-documentation-from-inside-arguments ()
;;   (elpy-testcase ()
;;     (elpy-enable)
;;     (python-mode)
;;     (insert "import socket\n"
;;             "socket.getaddrinfo(socket.gethostname(")
;;     (save-excursion
;;       (insert "))\n"))

;;     (elpy-doc)

;;     (with-current-buffer "*Python Doc*"
;;       (should (re-search-forward "gethostname")))

;;     (erase-buffer)


;;     (insert "import socket\n"
;;             "socket.getaddrinfo(socket.gethostname(), ")
;;     (save-excursion
;;       (insert ")\n"))

;;     (elpy-doc)

;;     (with-current-buffer "*Python Doc*"
;;       (should (re-search-forward "getaddrinfo")))))
