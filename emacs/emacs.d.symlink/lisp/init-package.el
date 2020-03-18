(require 'cl-lib)
(require 'package)

;; Emacs comes with a package manager for installing more features.
;; The default package repository doesn't contain much, so we tell it
;; to use MELPA as well.
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; To get the package manager going, we invoke its initialize function.
;; Initialize packages
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

;; If we're online, we attempt to fetch the package directories if
;; we don't have a local copy already. This lets us start installing
;; packages right away from a clean install.
(unless package-archive-contents (package-refresh-contents))

;; `Paradox' is an enhanced interface for package management, which also
;; provides some helpful utility functions we're going to be using
;; extensively. Thus, the first thing we do is install it if it's not there
;; already.
(when (not (package-installed-p 'paradox))
  (package-install 'paradox))

;; We're going to be using `use-package' to manage our dependencies.
;; In its simplest form, we can call eg. `(use-package lolcode-mode)'
;; to install the `lolcode-mode' package. We'd also declare one or more
;; entry points so the module isn't loaded unneccesarily at startup.
;; For instance, `(use-package my-module :commands (my-function))' will
;; defer loading `my-module' until you actually call `(my-function)'.
;;
;; Read about it in detail at https://github.com/jwiegley/use-package

;; First, we make sure it's installed, using a function provided by
;; Paradox, which we've just installed the hard way.
;; This is only needed once, near the top of the file
(eval-when-compile
  (paradox-require 'use-package))

(paradox-require 'diminish)

;; We enable `use-package-always-ensure' which makes
;; use-package install every declared package automatically from ELPA,
;; instead of expecting you to do it manually.
;; Should set before loading `use-package'
(eval-and-compile
  (setq use-package-always-ensure nil)
  (setq use-package-always-defer t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))


;; speed up loading of use-package and dependencies
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(use-package delight
  :ensure t
  :after use-package)

;; Update GPG keyring for GNU ELPA
(use-package gnu-elpa-keyring-update)

;; Auto update packages
(use-package auto-package-update
  :init
  (setq auto-package-update-delete-old-versions t
        auto-package-update-hide-results t)
  (defalias 'upgrade-packages #'auto-package-update-now))


(provide 'init-package)
