(use-package python
  :ensure nil ;; package is bundled with emacs

  :custom
  (python-indent-offset 4)
  (python-indent-guess-indent-offset nil)

  :config
  (add-hook 'python-mode-hook
          (lambda ()
            ;; disable electric indent
            (setq-local electric-indent-mode nil)
            ;; highlight lines longer than 79 characters (pep8)
            (setq-local fill-column 79)
            ;; use flat index in imenu
            (setq-local imenu-create-index-function
                        'python-imenu-create-flat-index))))

(use-package pyvenv
  :config (progn
            (add-hook 'python-mode-hook 'pyvenv-mode)))

(use-package pip-requirements)

(provide 'init-python-mode)
