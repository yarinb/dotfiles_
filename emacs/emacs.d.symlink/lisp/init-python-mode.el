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


(use-package pyenv-mode
  :ensure t
  :after python
  :config

  (defun projectile-pyenv-mode-set ()
    "Set pyenv version matching project name."
    (let ((project (projectile-project-name)))
      (if (member project (pyenv-mode-versions))
          (pyenv-mode-set project)
        (pyenv-mode-unset))))

  (when (executable-find "pyenv")
    (add-to-list 'exec-path (expand-file-name "shims" (or (getenv "PYENV_ROOT") "~/.pyenv"))))
  :hook((python-mode)
        (projectile-switch-project . projectile-pyenv-mode-set)))



(use-package pip-requirements)


(provide 'init-python-mode)
