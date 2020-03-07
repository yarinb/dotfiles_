;;; -*- lexical-binding: t -*-
;;; init-help.el --- It's dangerous to go alone! Take this.

(require 'init-package)

;; quick close help window with 'q'
(setq help-window-select t)

;; which-key prompts you with available options when you type a partial
;; command sequence. Try it out: hit C-x and just wait for two seconds.
(use-package which-key
  :ensure t
  :diminish
  :hook ((after-init . which-key-mode))
  :config
  (which-key-mode)
  ;; Allow C-h to trigger which-key before it is done automatically
  (setq which-key-show-early-on-C-h t)
  ;; Set the delay before which-key appears.
  (setq-default which-key-idle-delay 1.0)
  (setq-default which-key-idle-secondary-delay 0.05)
  ;; which-key will truncate special keys by default, eg. SPC turns into
  ;; an orange D. Turn this off to avoid confusion.
  (setq-default which-key-special-keys nil)
  ;; Hit C-h C-k to have which-key show you all top level key bindings.
  :bind (("C-h C-k" . which-key-show-top-level)
    ("C-h M-m" . which-key-show-major-mode)))

;; Get an instant cheat sheet for your current major mode
;; with C-h C-m.
(use-package discover-my-major
  :ensure t
  :commands (discover-my-major discover-my-mode)
  :bind ("C-h C-m" . discover-my-major))



(provide 'init-help)
