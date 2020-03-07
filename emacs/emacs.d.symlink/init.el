
(when (version< emacs-version "25.1")
  (error "This requires Emacs 25.1 and above!"))

;; Speed up startup
(defvar my-gc-cons-threshold (if (display-graphic-p) 100000000 1000000)
  "The default value to use for `gc-cons-threshold'. If you experience freezing,
decrease this. If you experience stuttering, increase this.")

(defvar my-gc-cons-upper-limit (if (display-graphic-p) 400000000 100000000)
  "The temporary value for `gc-cons-threshold' to defer it.")

(defvar my-gc-timer (run-with-idle-timer 10 t #'garbage-collect)
  "Run garbarge collection when idle 10s.")

(defvar default-file-name-handler-alist file-name-handler-alist)

(setq file-name-handler-alist nil)
(setq gc-cons-threshold my-gc-cons-upper-limit)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore defalut values after startup."
            (setq file-name-handler-alist default-file-name-handler-alist)
            (setq gc-cons-threshold my-gc-cons-threshold)

            ;; GC automatically while unfocusing the frame
            ;; `focus-out-hook' is obsolete since 27.1
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              (add-hook 'focus-out-hook 'garbage-collect))

            ;; Avoid GCs while using `ivy'/`counsel'/`swiper' and `helm', etc.
            ;; @see http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
            (defun my-minibuffer-setup-hook ()
              (setq gc-cons-threshold my-gc-cons-upper-limit))

            (defun my-minibuffer-exit-hook ()
              (setq gc-cons-threshold my-gc-cons-threshold))

            (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
            (add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)))

(let ((lisp-directory (expand-file-name "lisp" user-emacs-directory)))
  ;; add ~/.emacs.d/lisp to load path
  (add-to-list 'load-path lisp-directory)

  ;; save customizations as local (unversioned) settings
  (setq custom-file (expand-file-name "init-local.el" lisp-directory)))

(require 'init-benchmarking) ;; Measure startup time
(require 'init-funcs)

;; load files
(require 'init-package)
(require 'init-appearance)
(require 'init-sane-defaults)
(require 'init-help)
(when (eq system-type 'darwin)
   (require 'init-mac))
(require 'init-windows)
(require 'init-fonts)
(require 'init-dired)
(require 'init-editing)
(require 'init-server)

(require 'init-ivy)
(require 'init-projectile)
(require 'init-grep)

(require 'init-sessions)
;; (require 'init-imenu)
(require 'init-ibuffer)
(require 'init-git)

(require 'init-completion)
(require 'init-flycheck)
;(require 'init-flyspell)
;(require 'init-tramp)
(require 'init-treemacs)
(require 'init-org)

;; Not sure I need those
(require 'init-rainbow-delimiters)

;; Language support
(require 'init-lisp)
;(require 'init-javascript)
;(require 'init-yaml-mode)
;(require 'init-markdown-mode)
;(require 'init-go-mode)
(require 'init-python-mode)

;; local settings (optional)
(load custom-file 'noerror)
