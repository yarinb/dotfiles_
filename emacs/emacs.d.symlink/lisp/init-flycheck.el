(use-package flycheck
  :hook
  (after-init . global-flycheck-mode)
  :init
  ;; disable noisy checkers
  (setq-default flycheck-disabled-checkers
                '(emacs-lisp emacs-lisp-checkdoc go-golint))

  :custom
  (flycheck-indication-mode 'right-fringe)
  (flycheck-emacs-lisp-load-path 'inherit))

(provide 'init-flycheck)
