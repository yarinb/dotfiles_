;; Load `custom-file'
;; If it doesn't exist, copy from the template, then load it.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(let ((custom-example-file
       (expand-file-name "custom-example.el" user-emacs-directory)))
  (if (and (file-exists-p custom-example-file)
           (not (file-exists-p custom-file)))
      (copy-file custom-example-file custom-file)))

(if (file-exists-p custom-file)
    (load custom-file))
    
(provide 'init-custom)