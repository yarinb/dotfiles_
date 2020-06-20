(eval-when-compile
  (require 'init-const))
;; disable splash
(setq inhibit-startup-message t)

(when sys/mac-x-p
  ;; in Mac, set the titlebar the same color as the theme backgroun. Neat.
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  (add-hook 'after-load-theme-hook
            (lambda ()
              (let ((bg (frame-parameter nil 'background-mode)))
                (set-frame-parameter nil 'ns-appearance bg)
                (setcdr (assq 'ns-appearance default-frame-alist) bg))))
 )

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;; Menu/Tool/Scroll bars
(unless emacs/>=27p        ; Move to early init-file in 27
  (unless sys/mac-x-p
    (push '(menu-bar-lines . 0) default-frame-alist))
  (push '(tool-bar-lines . 0) default-frame-alist)
  (push '(vertical-scroll-bars) default-frame-alist))


;; disable dialogs
(setq use-dialog-box nil)

;; highlight current line
(use-package hl-line
  :config
  (global-hl-line-mode 1)
  :hook
  (prog-mode . hl-line-mode))

;; Show native line numbers if possible, otherwise use `linum'
(if (fboundp 'display-line-numbers-mode)
    (use-package display-line-numbers
      :ensure nil
      :hook (prog-mode . display-line-numbers-mode))
  (use-package linum-off
    :demand
    :defines linum-format
    :hook (after-init . global-linum-mode)
    :init (setq linum-format "%4d ")
    :config
    ;; Highlight current line number
    (use-package hlinum
      :defines linum-highlight-in-all-buffersp
      :custom-face (linum-highlight-face ((t (:inherit default :background nil :foreground nil))))
      :hook (global-linum-mode . hlinum-activate)
      :init (setq linum-highlight-in-all-buffersp t))))

;; Cool icons
(when (display-graphic-p)
  (use-package all-the-icons
    :ensure t
    :config
    (cond
     ((string-equal system-type "darwin")
      (if (not (file-exists-p
		(concat (getenv "HOME") "/Library/Fonts/all-the-icons.ttf")))
	  (all-the-icons-install-fonts "t"))))
    (use-package all-the-icons-dired
      :ensure t
      :init (progn
	      (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)))))

;; set theme
(use-package gruvbox-theme
  :ensure t)
(use-package color-theme-sanityinc-tomorrow
  :ensure t)
(use-package modus-operandi-theme
  :ensure t)
(use-package modus-vivendi-theme
  :ensure t)
;; show matching parenthesis
(show-paren-mode 1)

;; highlight lines exceeding fill-column
(use-package whitespace
  :ensure t
  :diminish whitespace-mode

  :init
  (setq whitespace-style '(face empty lines-tail trailing))
  (setq whitespace-line-column nil)

  :config
  ;; make whitespace-mode respect a mode-specific fill-column value
  (add-hook 'hack-local-variables-hook
            (lambda ()
              (when (derived-mode-p 'prog-mode)
                (whitespace-mode 1)))))

;; show empty lines
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;; disable word wrapping
(setq-default truncate-lines t)

;; display line and column numbers in mode-line
(setq line-number-mode t
      column-number-mode t)


;; If you don't customize it, this is the theme you get.
(setq-default custom-enabled-themes '(gruvbox-dark-hard))

;; Make sure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(add-hook 'after-init-hook 'reapply-themes)


;;------------------------------------------------------------------------------
;; Toggle between light and dark
;;------------------------------------------------------------------------------
(defun light ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes '(gruvbox-light-medium))
  (reapply-themes))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(gruvbox-dark-hard))
  (reapply-themes))

(provide 'init-appearance)
