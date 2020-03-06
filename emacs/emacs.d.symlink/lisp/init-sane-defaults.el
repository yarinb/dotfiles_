(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

;; Encoding
;; UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))

;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(prefer-coding-system 'utf-8)

(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)

(setq locale-coding-system 'utf-8
      default-process-coding-system '(utf-8 . utf-8))

;; Don't autosave files or create lock/history/backup files. We don't want
;; copies of potentially sensitive material floating around, and we'll rely on
;; git and our own good fortune instead. Fingers crossed!
(setq auto-save-default nil
      create-lockfiles nil
      make-backup-files nil
      ;; But have a place to store them in case we do use them...
      auto-save-list-file-name (concat emacsd/cache-dir "autosave")
      backup-directory-alist `(("." . ,(concat emacsd/cache-dir "backup/"))))

;; use soft tabs
(setq-default indent-tabs-mode nil)

;; replace highlighted text
(delete-selection-mode 1)

;; make prompts accept y or n
(defalias 'yes-or-no-p #'y-or-n-p)

;; no confirmation for non-existent files or buffers
(setq confirm-nonexistent-file-or-buffer nil)

;; disable audible bell
(setq ring-bell-function #'ignore)

;; change cursor-type
(setq-default cursor-type 'bar)

;; Mouse & Smooth Scroll
;; Scroll one line at a time (less "jumpy" than defaults)
(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
        mouse-wheel-progressive-speed nil))

(use-package smooth-scrolling
  :ensure t
  :init
  (setq smooth-scroll-margin 1)
  (smooth-scrolling-mode 1)
  :config
  (setq scroll-margin 1
        scroll-step 1
        scroll-conservatively 9999))

;; reload buffers automatically when files change on disk
(use-package autorevert
  :ensure nil ;; package is bundled with emacs
  :diminish auto-revert-mode
  :init
  ;; revert buffers quietly
  (setq auto-revert-verbose nil)
  :config
  (global-auto-revert-mode 1))

;; integrate with X clipboard
(setq select-enable-clipboard t)

;; ensure clipboard strings are saved into kill ring
(setq save-interprogram-paste-before-kill t)

;; move deleted files to trash
(setq delete-by-moving-to-trash t)

;; show keystrokes immediately
(setq echo-keystrokes 0.1)

;; add directory to buffer name if filename is not unique
(setq uniquify-buffer-name-style 'forward)

;; automatically determine major-mode for newly created buffers
(setq-default major-mode
              (lambda ()
                (unless buffer-file-name
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode t)))))

;; single space ends a sentence
(setq-default sentence-end-double-space nil)

;; default to 80 chars fill-column
(setq-default fill-column 80)

;; confirm when exiting
(setq confirm-kill-emacs #'y-or-n-p)

;; backup files
(setq make-backup-files t
      backup-directory-alist `(("." . ,(expand-file-name (concat user-emacs-directory "backups"))))
      backup-by-copying t
      delete-old-versions t
      version-control t
      vc-make-backup-files t
      kept-new-versions 6
      kept-old-versions 2)

;; preserve point position when scrolling
(setq scroll-preserve-screen-position 'always)

;; always add newline at the end of the file
(setq require-final-newline t)

;; remember position in previously visited files
(use-package saveplace
  :ensure nil ;; package is bundled with emacs
  :hook (after-init . save-place-mode))

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :init (setq recentf-max-saved-items 300
              recentf-exclude
              '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
                "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
                "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
                "^/tmp/" "^/var/folders/.+$" ; "^/ssh:"
                (lambda (file) (file-in-directory-p file package-user-dir))))
  :config (push (expand-file-name recentf-save-file) recentf-exclude))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
              history-length 1000
              savehist-additional-variables '(mark-ring
                                              global-mark-ring
                                              search-ring
                                              regexp-search-ring
                                              extended-command-history)
              savehist-autosave-interval 300))

(use-package simple
  :ensure nil
  :hook ((after-init . size-indication-mode)
         ((prog-mode markdown-mode conf-mode) . enable-trailing-whitespace))
  :init
  (setq column-number-mode t
        line-number-mode t
        ;; kill-whole-line t               ; Kill line including '\n'
        line-move-visual nil
        track-eol t                     ; Keep cursor at end of lines. Require line-move-visual is nil.
        set-mark-command-repeat-pop t)  ; Repeating C-SPC after popping mark pops it again

  ;; Visualize TAB, (HARD) SPACE, NEWLINE
  (setq-default show-trailing-whitespace nil) ; Don't show trailing whitespace by default
  (defun enable-trailing-whitespace ()
    "Show trailing spaces and delete on saving."
    (setq show-trailing-whitespace t)
    (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)))

(use-package time
  :ensure nil
  :unless (display-graphic-p)
  :hook (after-init . display-time-mode)
  :init (setq display-time-24hr-format t
              display-time-day-and-date t))

;; Fullscreen
(when (display-graphic-p)
  ;; WORKAROUND: To address blank screen issue with child-frame in fullscreen
  (when (and sys/mac-cocoa-p emacs/>=26p)
    (add-hook 'window-setup-hook (lambda () (setq ns-use-native-fullscreen nil))))

  (bind-keys ("C-<f11>" . toggle-frame-fullscreen)
             ("C-s-f" . toggle-frame-fullscreen) ; Compatible with macOS
             ("S-s-<return>" . toggle-frame-fullscreen)
             ("M-S-<return>" . toggle-frame-fullscreen)))

(provide 'init-sane-defaults)
