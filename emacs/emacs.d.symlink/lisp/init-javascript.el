(use-package json-mode
  :ensure t
  :mode "\\.js\\(?:on\\|[hl]int\\(?:rc\\)?\\)\\'")
(when (version< emacs-version "27.0")
  (use-package js2-mode
    :mode "\\.js\\'"
    :init
    ;; set indent level to 2
    (setq-default js-indent-level 2)
    (setq-default js2-strict-missing-semi-warning nil)
    (setq js-chain-indent t
          ;; Don't mishighlight shebang lines
          js2-skip-preprocessor-directives t
          ;; let flycheck handle this
          js2-mode-show-parse-errors nil
          js2-mode-show-strict-warnings nil
          ;; Flycheck provides these features, so disable them: conflicting with
          ;; the eslint settings.
          js2-strict-trailing-comma-warning nil
          js2-strict-missing-semi-warning nil
          ;; maximum fontification
          js2-highlight-level 3
          js2-highlight-external-variables t
          js2-idle-timer-delay 0.1)
    :config
    (add-hook 'js2-mode-hook #'electric-pair-mode)
    (add-hook 'js2-mode-hook #'rainbow-delimiters-mode)
    :bind (:map js-mode-map
                ;; C-c p runs formats json with jq
                ("C-c C-f" . json-pretty-print-buffer)))

  (use-package rjsx-mode
    :defer t))

(when emacs/>=27p
  (use-package js-mode
    :ensure nil
    :init
     (setq-default js-indent-level 2)
    :config
    (add-hook 'js-mode-hook 'electric-indent-mode)
    (add-hook 'js-mode-hook 'electric-pair-mode)))


(use-package tide
  :ensure t
  :after (company flycheck)
  :init
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    ;; company is an optional dependency. You have to
    ;; install it separately via package-install
    ;; `M-x package-install [ret] company`
    (company-mode +1))
  (defun my/setup-tsx-mode ()
    (when (string-equal "tsx" (file-name-extension buffer-file-name))
      (setup-tide-mode)))

  (defun my/setup-jsx-mode ()
    (when (string-equal "jsx" (file-name-extension buffer-file-name))
      (setup-tide-mode)))
  (add-hook 'typescript-mode-hook #'setup-tide-mode)
  (add-hook 'js2-mode-hook #'setup-tide-mode)
  (add-hook 'web-mode-hook #'my/setup-tsx-mode)
  (add-hook 'rjsx-mode-hook #'my/setup-jsx-mode)
  :requires flycheck

  :config
  (add-to-list 'company-backends 'company-tide)
  ;; aligns annotation to the right hand side
  ;; (setq company-tooltip-align-annotations t)

  ;; formats the buffer before saving
  ;; (add-hook 'before-save-hook 'tide-format-before-save)
  (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)
  (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)



(use-package web-mode
  :ensure t
  :mode ("\\.html?\\'"
         "\\.erb\\'"
         "\\.mustache\\'"
         "\\.djhtml\\'"
         )
  :config
  ;; configure jsx-tide checker to run after your default jsx checker
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'typescript-tslint 'web-mode))


(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint
          (and root
               (expand-file-name "node_modules/.bin/eslint"
                                 root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

(provide 'init-javascript)
