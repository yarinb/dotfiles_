(let ((font-family "Iosevka"))
  (when (and (display-graphic-p) (member font-family (font-family-list)))
    (set-face-attribute 'default nil :family font-family)
    (set-face-attribute 'default nil :height 160)))


(defun my/font-name-replace-size (font-name new-size)
  (let ((parts (split-string font-name "-")))
    (setcar (nthcdr 7 parts) (format "%d" new-size))
    (mapconcat 'identity parts "-")))

(defun my/increment-default-font-height (delta)
  "Adjust the default font height by DELTA on every frame.
   Emacs will keep the pixel size of the frame approximately the
   same.  DELTA should be a multiple of 10, to match the units used
   by the :height face attribute."
  (let* ((new-height (+ (face-attribute 'default :height) delta))
         (new-point-height (/ new-height 10)))
    (dolist (f (frame-list))
      (with-selected-frame f
        ;; Latest 'set-frame-font supports a "frames" arg, but
        ;; we cater to Emacs 23 by looping instead.
        (set-frame-font (my/font-name-replace-size
                         (face-font 'default)
                         new-point-height)
                        t)))
    (set-face-attribute 'default nil :height new-height)
    (message "Default font size is now %d" new-point-height)))

(defun my/increase-default-font-height ()
  (interactive)
  (my/increment-default-font-height 10))

(defun my/decrease-default-font-height ()
  (interactive)
  (my/increment-default-font-height -10))

(global-set-key (kbd "C-M-=") 'my/increase-default-font-height)
(global-set-key (kbd "C-M--") 'my/decrease-default-font-height)

(provide 'init-fonts)
