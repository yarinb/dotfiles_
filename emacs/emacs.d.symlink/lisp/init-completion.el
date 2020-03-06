(use-package company
  :diminish company-mode
  :init
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2
        company-show-numbers t
        company-require-match nil)
  (setq-default company-backends
                '(company-nxml company-css company-cmake company-capf company-dabbrev-code
                               company-gtags company-etags company-keywords
                               company-files company-dabbrev))
  :config
  (add-hook 'after-init-hook 'global-company-mode)

  ;; Add yasnippet support for all company backends
  ;; https://github.com/syl20bnr/spacemacs/pull/179
  (defvar company-mode/enable-yas t "Enable yasnippet for all backends.")

  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend)    (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

  (add-to-list 'company-begin-commands 'outshine-self-insert-command)

  (use-package company-quickhelp
    :config
    (company-quickhelp-mode t))
  )

(provide 'init-completion)
