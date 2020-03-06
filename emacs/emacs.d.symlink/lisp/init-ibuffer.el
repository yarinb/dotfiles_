(eval-when-compile
  (require 'init-const))

(use-package ibuffer
  :ensure nil ;; package is bundled with emacs
  :init
  ;; disable confirmation for killing unmodified buffers
  (setq ibuffer-expert t)
  (setq ibuffer-display-summary nil)
  ;;(setq ibuffer-use-other-window nil)
  ;; sort buffers by Last view time
  (setq ibuffer-default-sorting-mode 'recency)
  
  (setq ibuffer-use-header-line t)
  (setq ibuffer-default-shrink-to-minimum-size nil)
  ;; hide empty filter groups
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-saved-filter-groups
        '(("Main"
           ("Directories" (mode . dired-mode))
           ("Org" (mode . org-mode))
           ("Programming" (or
                           (mode . c-mode)
                           (mode . conf-mode)
                           (mode . css-mode)
                           (mode . emacs-lisp-mode)
                           (mode . html-mode)
                           (mode . mhtml-mode)
                           (mode . python-mode)
                           (mode . ruby-mode)
                           (mode . scss-mode)
                           (mode . shell-script-mode)
                           (mode . yaml-mode)))
           ("Markdown" (mode . markdown-mode))
           ("Magit" (or
                     (mode . magit-blame-mode)
                     (mode . magit-cherry-mode)
                     (mode . magit-diff-mode)
                     (mode . magit-log-mode)
                     (mode . magit-process-mode)
                     (mode . magit-status-mode)))
           ("Emacs" (or
                     (name . "^\\*Help\\*$")
                     (name . "^\\*Custom.*")
                     (name . "^\\*Org Agenda\\*$")
                     (name . "^\\*info\\*$")
                     (name . "^\\*scratch\\*$")
                     (name . "^\\*Backtrace\\*$")
                     (name . "^\\*Messages\\*$"))))))

  ;; use ibuffer
  :bind (("C-x C-b" . ibuffer)
         :map ibuffer-mode-map
         ;; make C-o and o behave as in dired
         ("o" . ibuffer-visit-buffer-other-window)
         ("C-o" . ibuffer-visit-buffer-other-window-noselect))

  :config
  ;; group ibuffer by projectile project
  (add-hook 'ibuffer-hook 'ibuffer-projectile-set-filter-groups))

;; Group ibuffer's list by project root
(use-package ibuffer-projectile
    :hook ((ibuffer . (lambda ()
                        (ibuffer-projectile-set-filter-groups)
                        (unless (eq ibuffer-sorting-mode 'alphabetic)
                          (ibuffer-do-sort-by-alphabetic)))))
    :config
    (setq ibuffer-projectile-prefix "Project: "))

(provide 'init-ibuffer)
