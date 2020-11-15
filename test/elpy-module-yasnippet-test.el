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
    (elpy-enable)
    (python-mode)

    (should yas-minor-mode)))

(ert-deftest elpy-module-yasnippet-buffer-stop ()
  (elpy-testcase ()
    (elpy-enable)
    (python-mode)
    (yas-minor-mode 1)

    (elpy-module-yasnippet 'buffer-stop)

    (should-not yas-minor-mode)))

(ert-deftest elpy-snippet-split-args ()
  (elpy-testcase ()
    (elpy-enable)
    (python-mode)
    (let ((args "self, arg1, arg2 :int, arg3=4, arg4='message', arg5 :int, arg6 :float=65.4, *args, **kwargs"))
      (should (equal (elpy-snippet-split-args args)
	     '("self" "arg1" "arg2" "arg3" "arg4" "arg5" "arg6" "*args" "**kwargs"))))
    ;; badly formatted arguments
    (let ((args "self,arg1 , arg2:int , arg3 =4 ,arg4 = 'message',arg5 :int , arg6:float =65.4,*args ,**kwargs"))
      (should (equal (elpy-snippet-split-args args)
	     '("self" "arg1" "arg2" "arg3" "arg4" "arg5" "arg6" "*args" "**kwargs"))))))
