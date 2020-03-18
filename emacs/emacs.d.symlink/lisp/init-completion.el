(use-package company
  ;; :diminish company-mode
  :ensure t
  :hook (prog-mode . company-mode)
  :init
  (setq company-idle-delay 0.2
        company-echo-delay 0.2
        company-show-numbers t
        company-dabbrev-downcase nil
        company-dabbrev-code-everywhere t
        company-dabbrev-code-modes t
        company-dabbrev-code-ignore-case t
        company-tooltip-align-annotations t
        company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-transformers '(company-sort-by-occurrence
                               company-sort-by-backend-importance)
        company-frontends '(company-pseudo-tooltip-frontend ; show tooltip even for single candidate
                       company-echo-metadata-frontend))
  :config
  ;; Add yasnippet support for all company backends
  ;; https://github.com/syl20bnr/spacemacs/pull/179
  (defvar company-mode/enable-yas t "Enable yasnippet for all backends.")

  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend)    (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

  (add-to-list 'company-begin-commands 'outshine-self-insert-command))


(defun my-lsp-setup-python ()
  "Microsoft Python Language Server does not have a syntax checker, setup one for it."
  (progn
    (require 'lsp-python-ms)
    (lsp)))

(use-package lsp-python-ms
  :ensure t)

(use-package lsp-mode
  :ensure t
  :defer t
  :hook
  (((python-mode . my-lsp-setup-python)
    (sh-mode html-mode web-mode js-mode json-mode
     css-mode less-mode sass-mode scss-mode
     js2-mode typescript-mode) . lsp)
   (lsp-mode . lsp-enable-which-key-integration))
  :init
  (setq lsp-keymap-prefix "s-l"
        lsp-auto-guess-root t
        lsp-prefer-flymake nil  ; don't try to use flymake.
        lsp-prefer-capf t)      ; Detect project root
  :commands lsp
)

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :init
  (setq
        lsp-ui-doc-enable t
        lsp-ui-doc-header t
        lsp-ui-doc-include-signature t
        lsp-ui-doc-position 'top
        lsp-ui-sideline-enable t
        lsp-ui-imenu-enable t
        lsp-ui-sideline-ignore-duplicate t)
  :config
  ;; Use lsp-ui-doc-webkit only in GUI
  (setq lsp-ui-doc-use-webkit t)
  :bind
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references] . lsp-ui-peek-find-references)
        ("C-c u" . lsp-ui-imenu)))

(use-package company-lsp
  :ensure t
  :after company
  :init
  (setq company-lsp-cache-candidates 'auto
        company-lsp-enable-snippet t)
  :config (push 'company-lsp company-backends))

(provide 'init-completion)
