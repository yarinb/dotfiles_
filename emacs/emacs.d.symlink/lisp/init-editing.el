;; Word wrap in text
(add-hook 'text-mode-hook 'global-visual-line-mode)

;; use zap-up-to-char instead of zap-to-char
(use-package misc
  :ensure nil ;; package is bundled with emacs
  :bind ("M-z" . zap-up-to-char))

;; enable subword-mode in prog-mode
(use-package subword
  :ensure nil ;; package is bundled with emacs
  :diminish subword-mode
  :config
  (add-hook 'prog-mode-hook 'subword-mode))

;; Automatically insert matching braces and quotes
;; (electric-pair-mode 1)
(use-package smartparens
  :ensure t
  :diminish
  :hook
  ((prog-mode . smartparens-mode))
  :config
  (sp-pair "{" nil
           :post-handlers '(("||\n[i]" "RET") ("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p)
           :wrap "C-{")
  (sp-pair "(" nil
           :post-handlers '(("||\n[i]" "RET") ("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p)
           :wrap "C-(")
  (sp-pair "[" nil
           :post-handlers '(("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "\"" nil :unless '(sp-point-before-word-p sp-point-after-word-p sp-point-before-same-p))
  (sp-pair "'" nil :unless '(sp-point-before-word-p sp-point-after-word-p sp-point-before-same-p)))

;; Expand region with C-=
;; Contract region with C-- C-= (ctrl-minus key)
(use-package expand-region
   :commands er/expand-region
   :bind ("C-=" . er/expand-region))

;; enable electric-indent-mode
;; automatically indents the line after every <RET> you type
(use-package electric
  :ensure nil ;; package is bundled with emacs
  :config
  (electric-indent-mode 1))

;; use sh-mode for various zsh files
(use-package sh-script
  :ensure nil ;; package is bundled with emacs
  :mode ("z\\(sh[^/]*\\|login\\|logout\\|profile\\)\\'" . sh-mode))

;; move lines up/down
(use-package move-text
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

;; Show indent guides
(use-package highlight-indent-guides
  :init (setq-default highlight-indent-guides-method 'character)
  :config (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

(defun show-file-name ()
  "Show the full path file name in the minibuffer and add it to the kill ring."
  (interactive)
  (when buffer-file-name
    (message buffer-file-name)
    (kill-new (file-truename buffer-file-name))))

;; source:
;; http://emacsredux.com/blog/2013/05/04/rename-file-and-buffer/
;; https://github.com/bbatsov/prelude/blob/master/core/prelude-core.el
(defun rename-buffer-and-file ()
  "Rename current buffer and if the buffer is visiting a file, rename it too."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (rename-buffer (read-from-minibuffer "New name: " (buffer-name)))
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

;; swap RET and C-j
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-j") (lambda () (interactive) (insert "\n")))

;; C-x k kills current buffer
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; C-c f shows the path of the current file
(global-set-key (kbd "C-c f") 'show-file-name)

;; join line
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))

;; C-c n renames the current buffer and file
(global-set-key (kbd "C-c n") 'rename-buffer-and-file)

;; C-x g goes to line (as in mg)
(global-set-key (kbd "C-x g") 'goto-line)

(provide 'init-editing)
