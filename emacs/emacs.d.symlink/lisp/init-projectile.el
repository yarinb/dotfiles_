(use-package projectile
  :ensure t
  :diminish projectile-mode

  :init
  ;; switching project opens the top-level directory
  (setq projectile-switch-project-action 'projectile-dired)

  ;; ignore remote projects
  (setq projectile-ignored-project-function 'file-remote-p)

  ;; enable caching
  (setq projectile-enable-caching t)

  ;; use Ivy for completions
  (setq projectile-completion-system 'ivy)

  :bind (;; C-x f finds file in project
         ("C-x f" . projectile-find-file)
         ;; C-c g runs git grep in project
         ("C-c g" . projectile-grep))

  :config
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-directories "elpa")
  ;; enable projectile mode in all buffers
  (projectile-global-mode 1)
  (use-package counsel-projectile
    :ensure t
    :config
    (counsel-projectile-mode 1)
    :bind-keymap ("s-p" . projectile-command-map)))

(provide 'init-projectile)
