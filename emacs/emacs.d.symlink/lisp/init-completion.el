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

(provide 'init-completion)
