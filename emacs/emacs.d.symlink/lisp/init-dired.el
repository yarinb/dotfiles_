(use-package dired
  :ensure nil
  :init
  ;; show human readable sizes in dired
  ;;(setq dired-listing-switches "-alh")

  ;; show human readable free space
  (setq directory-free-space-args "-h")

  ;; group directories first in dired if supported
  (when (eq 0 (call-process insert-directory-program
                            nil nil nil "--group-directories-first"))
    (setq dired-listing-switches (concat dired-listing-switches
                                         " --group-directories-first")))
  :bind (("M-<up>" . dired-up-directory)
        :map dired-mode-map ("<return>" . dired-find-alternate-file))
  :custom
  ;; Always delete and copy recursively
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  ;; Auto refresh Dired, but be quiet about it
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  ;; Quickly copy/move file in Dired
  (dired-dwim-target t)
  ;; Move files to trash when deleting
  (delete-by-moving-to-trash t)
  :config
  ;; Reuse same dired buffer, to prevent numerous buffers while navigating in dired
  (put 'dired-find-alternate-file 'disabled nil))

(provide 'init-dired)
