;; use appropiate git-mode for .gitconfig and .gitignore extensions
(use-package gitignore-mode
  :ensure t
  :mode ("gitignore\\'" . gitignore-mode))

(use-package gitconfig-mode
  :ensure t
  :mode ("gitconfig\\'" . gitconfig-mode))


;; use flyspell when writing commit messages
(use-package git-commit
  :ensure t
  :config
  (add-hook 'git-commit-mode-hook
            (lambda ()
              (when (featurep 'flyspell) (flyspell-mode 1)))))

;; magit config
(defun magit-diff-visit-file-other-window (&optional noselect)
  "Visit current file in another window."
  (interactive)
  (let ((current-window (selected-window))
        ;; magit-diff-visit-file visits in other-window with prefix arg
        (current-prefix-arg t))
    (call-interactively 'magit-diff-visit-file)
    (when noselect
      (select-window current-window))))

(defun magit-diff-visit-file-other-window-noselect ()
  "Visit current file in another window, but don't select it."
  (interactive)
  (magit-diff-visit-file-other-window t))

(use-package magit
  :ensure t
  :init
  ;; disable gravatars
  (setq magit-revision-show-gravatars nil)

  ;; hide recent commits in magit-status
  (setq magit-log-section-commit-count 0)

  :bind (("C-x m" . magit-status)
         ("C-c b" . magit-blame)
         :map magit-status-mode-map
         ;; make C-o and o behave as in dired
         ("o" . magit-diff-visit-file-other-window)
         ("C-o" . magit-diff-visit-file-other-window-noselect)))

;; follow symlinks to files under version control
(setq vc-follow-symlinks t)


(use-package diff-hl
  :ensure t
  ;; :defer t
  ;; :init (setq diff-hl-draw-borders nil)
  :config
  ;; Highlight on-the-fly
  ;; (diff-hl-flydiff-mode 1)
  ;; set fringe style
  ;; (setq fringes-outside-margins t)
  ;; Integration with magit
  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

  :hook ((after-init . global-diff-hl-mode)
         (dired-mode . diff-hl-dired-mode))
  :bind (:map diff-hl-command-map
         ("SPC" . diff-hl-mark-hunk)))


(provide 'init-git)
