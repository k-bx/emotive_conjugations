((python-mode . ((eval . (setq here (locate-dominating-file (buffer-file-name) ".dir-locals.el")))
                 (eval . (setq flycheck-python-pylint-executable (concat here "venv/bin/pylint")))
                 (eval . (setq jedi:environment-root (concat here "venv")))
                 (jedi:server-args . ())
                 (eval . (setenv "PYTHONPATH"
                                 (concat
                                  here "scripts/download-articles" ":"
                                  (getenv "PYTHONPATH"))))))
 )
