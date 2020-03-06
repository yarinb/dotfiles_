(use-package ivy
  :ensure t
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-height-alist '((t lambda (_caller) (/ (window-height) 4))))
  (setq ivy-re-builders-alist
        '((counsel-M-x . ivy--regex-fuzzy)
          (ivy-switch-buffer . ivy--regex-fuzzy)
          (ivy-switch-buffer-other-window . ivy--regex-fuzzy)
          (t . ivy--regex-plus)))
  :hook ((after-init . ivy-mode)))


(use-package swiper
  :ensure t
  :after ivy
  :config
  (setq swiper-action-recenter t)
  (setq swiper-goto-start-of-match t)
  (setq swiper-include-line-number-in-search t)
  :bind (("C-s" . swiper)
         :map swiper-map
         ("M-%" . swiper-query-replace)))

(use-package counsel
  :ensure t
  :after ivy
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("s-f" . counsel-find-file)
         ("s-F" . find-file-other-window)
         ("C-x b" . ivy-switch-buffer)
         ("s-b" . ivy-switch-buffer)
         ("C-x B" . counsel-switch-buffer-other-window)
         ("s-B" . counsel-switch-buffer-other-window)
         ("C-x d" . counsel-dired)
         ("s-d" . counsel-dired)
         ("s-D" . dired-other-window)
         ("C-x C-r" . counsel-recentf)))

(provide 'init-ivy)
