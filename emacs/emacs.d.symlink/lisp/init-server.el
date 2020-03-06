;; start server for emacsclient
(use-package server
  :ensure nil ;; package is bundled with emacs

  :config
  (unless (server-running-p)
    (server-start))
  :hook (after-init . server-mode))

(provide 'init-server)
