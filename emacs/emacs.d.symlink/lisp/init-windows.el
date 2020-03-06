;; Directional window-selection routines
;; Move between windows with Shift-left/right/up/down
(use-package windmove
  :ensure nil
  :init
  (setq-default windmove-wrap-around t)
  :hook (after-init . windmove-default-keybindings))

;; Restore old window configurations
;; Default is C-c <left|right>
(use-package winner
  :ensure nil
  :commands (winner-undo winner-redo)
  :hook (after-init . winner-mode)
  :init (setq winner-boring-buffers '("*Completions*"
                                      "*Compile-Log*"
                                      "*inferior-lisp*"
                                      "*Fuzzy Completions*"
                                      "*Apropos*"
                                      "*Help*"
                                      "*cvs*"
                                      "*Buffer List*"
                                      "*Ibuffer*"
                                      "*esh command on file*")))

(provide 'init-windows)
