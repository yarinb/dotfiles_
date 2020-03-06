;; configure modifiers

(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)
(setq ns-function-modifier 'hyper)

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; don't open files from the workspace in a new frame
(setq ns-pop-up-frames nil)
;; Render thinner fonts
(setq ns-use-thin-smoothing t)
;; use old-style fullscreen
(setq ns-use-native-fullscreen nil)

;; move deleted files to ~/.Trash
(setq trash-directory "~/.Trash")

;; use paths from shell
(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "GOPATH")
        exec-path-from-shell-check-startup-files nil
        exec-path-from-shell-arguments '("-l"))
  :config (exec-path-from-shell-initialize))

;; add binding for toggling fullscreen
(global-set-key (kbd "M-RET") 'toggle-frame-fullscreen)

;; use gls if available (which supports --dired option)
(when (executable-find "gls")
  (setq insert-directory-program "gls"))

;; use gdf if available as it provides more correct output. When apfs is used,
;; the ifree column of bsd df seems to be incorrect
(when (executable-find "gdf")
  (setq dired-free-space-program "gdf"))

(provide 'init-mac)
