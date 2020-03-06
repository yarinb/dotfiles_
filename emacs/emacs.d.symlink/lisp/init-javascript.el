(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode))
  :init
  (progn
    (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
    (add-hook 'js2-mode-hook #'imenu-add-menubar-index)
    ;; (add-hook 'js2-mode-hook #'idle-highlight-mode)
    (add-hook 'js2-mode-hook #'electric-pair-mode)
    (add-hook 'js2-mode-hook #'hs-minor-mode)
    (add-hook 'js2-mode-hook #'flycheck-mode)
    (flycheck-add-mode 'javascript-eslint 'js2-mode)
    ;; set indent level to 2
    (setq-default js-indent-level 2)
    (setq-default js2-strict-missing-semi-warning nil))

  :bind (:map js-mode-map
              ;; C-c p runs formats json with jq
              ("C-c p" . json-pretty-print-buffer)))


(use-package rjsx-mode
  :config
  (add-hook 'rjsx-mode-hook #'flycheck-mode)
  (flycheck-add-mode 'javascript-eslint 'rjsx-mode))

(use-package tern
    :ensure t
    :diminish tern-mode
    :config
    (add-hook 'js2-mode-hook 'tern-mode))


(use-package company-tern
  :init
  (add-to-list 'company-backends 'company-tern))

(provide 'init-javascript)
