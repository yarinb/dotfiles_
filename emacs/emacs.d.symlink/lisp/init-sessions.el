;;; init-sessions.el --- Save and restore editor sessions between restarts -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; save a list of open files in ~/.emacs.d/.emacs.desktop
(setq desktop-path (list user-emacs-directory)
      desktop-auto-save-timeout 600
      desktop-restore-frames nil)
(desktop-save-mode 1)

(defun my/desktop-time-restore (orig &rest args)
  (let ((start-time (current-time)))
    (prog1
        (apply orig args)
      (message "Desktop restored in %.2fms"
               (my/time-subtract-millis (current-time)
                                               start-time)))))
(advice-add 'desktop-read :around 'my/desktop-time-restore)

(defun my/desktop-time-buffer-create (orig ver filename &rest args)
  (let ((start-time (current-time)))
    (prog1
        (apply orig ver filename args)
      (message "Desktop: %.2fms to restore %s"
               (my/time-subtract-millis (current-time)
                                               start-time)
               (when filename
                 (abbreviate-file-name filename))))))
(advice-add 'desktop-create-buffer :around 'my/desktop-time-buffer-create)

(provide 'init-sessions)
