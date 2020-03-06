; disable ido mode first
(add-hook 'after-init-hook
  (lambda ()
      (when (bound-and-true-p ido-ubiquitous-mode)
        (ido-ubiquitous-mode -1))
      (when (bound-and-true-p ido-mode)
        (ido-mode -1))))

(use-package counsel
  :diminish ivy-mode counsel-mode
  :bind (("C-s"   . swiper-isearch)
         ("C-r"   . swiper-isearch-backward)
         ("s-f"   . swiper)
         ("C-S-s" . swiper-all)

         ("C-c C-r" . ivy-resume)
         ("C-c v p" . ivy-push-view)
         ("C-c v o" . ivy-pop-view)
         ("C-c v ." . ivy-switch-view)

         :map counsel-mode-map
         ([remap swiper] . counsel-grep-or-swiper)
         ([remap swiper-backward] . counsel-grep-or-swiper-backward)
         ([remap dired] . counsel-dired)
         ([remap set-variable] . counsel-set-variable)
         ([remap insert-char] . counsel-unicode-char)

         ("C-x C-r" . counsel-buffer-or-recentf)
         ("C-x j"   . counsel-mark-ring)
         ("C-h F"   . counsel-faces)

         ("C-c B" . counsel-bookmarked-directory)
         ("C-c L" . counsel-load-library)
         ("C-c O" . counsel-find-file-extern)
         ("C-c P" . counsel-package)
         ("C-c R" . counsel-list-processes)
         ("C-c f" . counsel-find-library)
         ("C-c g" . counsel-grep)
         ("C-c h" . counsel-command-history)
         ("C-c t" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c l" . counsel-locate)
         ("C-c o" . counsel-outline)
         ("C-c r" . counsel-rg)
         ("C-c z" . counsel-fzf)

         ("C-c c B" . counsel-bookmarked-directory)
         ("C-c c F" . counsel-faces)
         ("C-c c L" . counsel-load-library)
         ("C-c c O" . counsel-find-file-extern)
         ("C-c c P" . counsel-package)
         ("C-c c R" . counsel-list-processes)
         ("C-c c a" . counsel-apropos)
         ("C-c c e" . counsel-colors-emacs)
         ("C-c c f" . counsel-find-library)
         ("C-c c g" . counsel-grep)
         ("C-c c h" . counsel-command-history)
         ("C-c c i" . counsel-git)
         ("C-c c j" . counsel-git-grep)
         ("C-c c l" . counsel-locate)
         ("C-c c m" . counsel-minibuffer-history)
         ("C-c c o" . counsel-outline)
         ("C-c c p" . counsel-pt)
         ("C-c c r" . counsel-rg)
         ("C-c c s" . counsel-ag)
         ("C-c c t" . counsel-load-theme)
         ("C-c c u" . counsel-unicode-char)
         ("C-c c w" . counsel-colors-web)
         ("C-c c v" . counsel-set-variable)
         ("C-c c z" . counsel-fzf)

         :map ivy-minibuffer-map
         ("C-w" . ivy-yank-word)

         :map counsel-find-file-map
         ("C-h" . counsel-up-directory)

         :map swiper-map
         ("M-s" . swiper-isearch-toggle)
         ("M-%" . swiper-query-replace)

         :map isearch-mode-map
         ("M-s" . swiper-isearch-toggle))
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))
  :init
  (setq ivy-use-selectable-prompt t
        ivy-use-virtual-buffers t    ; Enable bookmarks and recentf
        ivy-height 10
        ivy-fixed-height-minibuffer t
        ivy-count-format "(%d/%d) "
        ivy-on-del-error-function nil
        ivy-initial-inputs-alist nil)

  (setq swiper-action-recenter t)

  (setq counsel-find-file-at-point t
        counsel-yank-pop-separator "\n────────\n")

  ;; Use the faster search tool: ripgrep (`rg')
  (when (executable-find "rg")
    (setq counsel-grep-base-command "rg -S --no-heading --line-number --color never %s %s")
    (when (and sys/macp (executable-find "gls"))
      (setq counsel-find-file-occur-use-find nil
            counsel-find-file-occur-cmd
            "gls -a | grep -i -E '%s' | tr '\\n' '\\0' | xargs -0 gls -d --group-directories-first")))
  :config
  (with-no-warnings
    ;;
    ;; Improve search experience of `swiper' and `counsel'
    ;;
    (defun my-ivy-switch-to-swiper (&rest _)
      "Switch to `swiper' with the current input."
      (swiper ivy-text))

    (defun my-ivy-switch-to-swiper-isearch (&rest _)
      "Switch to `swiper-isearch' with the current input."
      (swiper-isearch ivy-text))

    (defun my-ivy-switch-to-swiper-all (&rest _)
      "Switch to `swiper-all' with the current input."
      (swiper-all ivy-text))

    (defun my-ivy-switch-to-rg-dwim (&rest _)
      "Switch to `rg-dwim' with the current input."
      (rg-dwim default-directory))

    (defun my-ivy-switch-to-counsel-rg (&rest _)
      "Switch to `counsel-rg' with the current input."
      (counsel-rg ivy-text default-directory))

    (defun my-ivy-switch-to-counsel-git-grep (&rest _)
      "Switch to `counsel-git-grep' with the current input."
      (counsel-git-grep ivy-text default-directory))

    (defun my-ivy-switch-to-counsel-find-file (&rest _)
      "Switch to `counsel-find-file' with the current input."
      (counsel-find-file ivy-text))

    (defun my-ivy-switch-to-counsel-fzf (&rest _)
      "Switch to `counsel-fzf' with the current input."
      (counsel-fzf ivy-text default-directory))

    (defun my-ivy-switch-to-counsel-git (&rest _)
      "Switch to `counsel-git' with the current input."
      (counsel-git ivy-text))

    ;; @see https://emacs-china.org/t/swiper-swiper-isearch/9007/12
    (defun my-swiper-toggle-counsel-rg ()
      "Toggle `counsel-rg' and `swiper'/`swiper-isearch' with the current input."
      (interactive)
      (ivy-quit-and-run
        (if (memq (ivy-state-caller ivy-last) '(swiper swiper-isearch))
            (my-ivy-switch-to-counsel-rg)
          (my-ivy-switch-to-swiper-isearch))))
    (bind-key "<C-return>" #'my-swiper-toggle-counsel-rg swiper-map)
    (bind-key "<C-return>" #'my-swiper-toggle-counsel-rg counsel-ag-map)

    (with-eval-after-load 'rg
      (defun my-swiper-toggle-rg-dwim ()
        "Toggle `rg-dwim' with the current input."
        (interactive)
        (ivy-quit-and-run
          (rg-dwim default-directory)))
      (bind-key "<M-return>" #'my-swiper-toggle-rg-dwim swiper-map)
      (bind-key "<M-return>" #'my-swiper-toggle-rg-dwim counsel-ag-map))

    (defun my-swiper-toggle-swiper-isearch ()
      "Toggle `swiper' and `swiper-isearch' with the current input."
      (interactive)
      (ivy-quit-and-run
        (if (eq (ivy-state-caller ivy-last) 'swiper-isearch)
            (swiper ivy-text)
          (swiper-isearch ivy-text))))
    (bind-key "<s-return>" #'my-swiper-toggle-swiper-isearch swiper-map)

    (defun my-counsel-find-file-toggle-fzf ()
      "Toggle `counsel-fzf' with the current `counsel-find-file' input."
      (interactive)
      (ivy-quit-and-run
        (counsel-fzf (or ivy-text "") default-directory)))
    (bind-key "<C-return>" #'my-counsel-find-file-toggle-fzf counsel-find-file-map)

    (defun my-swiper-toggle-rg-dwim ()
      "Toggle `rg-dwim' with the current input."
      (interactive)
      (ivy-quit-and-run (my-ivy-switch-to-rg-dwim)))
    (bind-key "<M-return>" #'my-swiper-toggle-rg-dwim swiper-map)
    (bind-key "<M-return>" #'my-swiper-toggle-rg-dwim counsel-ag-map)

    (defun my-swiper-toggle-swiper-isearch ()
      "Toggle `swiper' and `swiper-isearch' with the current input."
      (interactive)
      (ivy-quit-and-run
        (if (eq (ivy-state-caller ivy-last) 'swiper-isearch)
            (my-ivy-switch-to-swiper)
          (my-ivy-switch-to-swiper-isearch))))
    (bind-key "<s-return>" #'my-swiper-toggle-swiper-isearch swiper-map)

    ;; More actions
    (ivy-add-actions
     'swiper-isearch
     '(("r" my-ivy-switch-to-counsel-rg "rg")
       ("d" my-ivy-switch-to-rg-dwim "rg dwim")
       ("s" my-ivy-switch-to-swiper "swiper")
       ("a" my-ivy-switch-to-swiper-all "swiper all")))

    (ivy-add-actions
     'swiper
     '(("r" my-ivy-switch-to-counsel-rg "rg")
       ("d" my-ivy-switch-to-rg-dwim "rg dwim")
       ("s" my-ivy-switch-to-swiper-isearch "swiper isearch")
       ("a" my-ivy-switch-to-swiper-all "swiper all")))

    (ivy-add-actions
     'swiper-all
     '(("g" my-ivy-switch-to-counsel-git-grep "git grep")
       ("r" my-ivy-switch-to-counsel-rg "rg")
       ("d" my-ivy-switch-to-rg-dwim "rg dwim")
       ("s" my-swiper-toggle-swiper-isearch "swiper isearch")
       ("S" my-ivy-switch-to-swiper "swiper")))

    (ivy-add-actions
     'counsel-rg
     '(("s" my-ivy-switch-to-swiper-isearch "swiper isearch")
       ("S" my-ivy-switch-to-swiper "swiper")
       ("a" my-ivy-switch-to-swiper-all "swiper all")
       ("d" my-ivy-switch-to-rg-dwim "rg dwim")))

    (ivy-add-actions
     'counsel-git-grep
     '(("s" my-ivy-switch-to-swiper-isearch "swiper isearch")
       ("S" my-ivy-switch-to-swiper "swiper")
       ("r" my-ivy-switch-to-rg-dwim "rg")
       ("d" my-ivy-switch-to-rg-dwim "rg dwim")
       ("a" my-ivy-switch-to-swiper-all "swiper all")))

    (ivy-add-actions
     'counsel-find-file
     '(("g" my-ivy-switch-to-counsel-git "git")
       ("z" my-ivy-switch-to-counsel-fzf "fzf")))

    (ivy-add-actions
     'counsel-git
     '(("f" my-ivy-switch-to-counsel-find-file "find file")
       ("z" my-ivy-switch-to-counsel-fzf "fzf")))

    (ivy-add-actions
     'counsel-fzf
     '(("f" my-ivy-switch-to-counsel-find-file "find file")
       ("g" my-ivy-switch-to-counsel-git "git")))

    ;; Integration with `projectile'
    (with-eval-after-load 'projectile
      (setq projectile-completion-system 'ivy))

    ;; Integration with `magit'
    (with-eval-after-load 'magit
      (setq magit-completing-read-function 'ivy-completing-read)))

  ;; Enhance M-x
  (use-package amx
    :ensure t
    :init (setq amx-history-length 20))

  ;; Better sorting and filtering
  (use-package prescient
    :ensure t
    :commands prescient-persist-mode
    :init
    (setq prescient-filter-method '(literal regexp initialism fuzzy))
    (prescient-persist-mode 1))

  (use-package ivy-prescient
    :ensure t
    :commands ivy-prescient-re-builder
    :custom-face
    (ivy-minibuffer-match-face-1 ((t (:inherit font-lock-doc-face :foreground nil))))
    :init
    (defun ivy-prescient-non-fuzzy (str)
      "Generate an Ivy-formatted non-fuzzy regexp list for the given STR.
This is for use in `ivy-re-builders-alist'."
      (let ((prescient-filter-method '(literal regexp)))
        (ivy-prescient-re-builder str)))

    (setq ivy-prescient-retain-classic-highlighting t
          ivy-re-builders-alist
          '((counsel-ag . ivy-prescient-non-fuzzy)
            (counsel-rg . ivy-prescient-non-fuzzy)
            (counsel-pt . ivy-prescient-non-fuzzy)
            (counsel-grep . ivy-prescient-non-fuzzy)
            (counsel-imenu . ivy-prescient-non-fuzzy)
            (counsel-yank-pop . ivy-prescient-non-fuzzy)
            (swiper . ivy-prescient-non-fuzzy)
            (swiper-isearch . ivy-prescient-non-fuzzy)
            (swiper-all . ivy-prescient-non-fuzzy)
            (lsp-ivy-workspace-symbol . ivy-prescient-non-fuzzy)
            (lsp-ivy-global-workspace-symbol . ivy-prescient-non-fuzzy)
            (insert-char . ivy-prescient-non-fuzzy)
            (counsel-unicode-char . ivy-prescient-non-fuzzy)
            (t . ivy-prescient-re-builder))
          ivy-prescient-sort-commands
          '(:not swiper swiper-isearch ivy-switch-buffer
            counsel-grep counsel-git-grep counsel-ag counsel-imenu
            counsel-yank-pop counsel-recentf counsel-buffer-or-recentf))

    (ivy-prescient-mode 1))

  ;; Additional key bindings for Ivy
  (use-package ivy-hydra
    :ensure t
    :commands ivy-hydra-read-action
    :init (setq ivy-read-action-function #'ivy-hydra-read-action))

  ;; Ivy integration for Projectile
  (use-package counsel-projectile
    :ensure t
    :hook (counsel-mode . counsel-projectile-mode)
    :init (setq counsel-projectile-grep-initial-input '(ivy-thing-at-point)))

  ;; Integrate yasnippet
  (use-package ivy-yasnippet
    :ensure t
    :commands ivy-yasnippet--preview
    :bind ("C-c C-y" . ivy-yasnippet)
    :config (advice-add #'ivy-yasnippet--preview :override #'ignore))

  ;; Select from xref candidates with Ivy
  (use-package ivy-xref
    :ensure t
    :init
    (when (boundp 'xref-show-definitions-function)
      (setq xref-show-definitions-function #'ivy-xref-show-defs))
    (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

  ;; Correcting words with flyspell via Ivy
  (use-package flyspell-correct-ivy
    :ensure t
    :after flyspell
    :bind (:map flyspell-mode-map
           ([remap flyspell-correct-word-before-point] . flyspell-correct-wrapper))
    :init (setq flyspell-correct-interface #'flyspell-correct-ivy))

  ;; Quick launch apps
  (cond
   (sys/linux-x-p
    (bind-key "s-<f6>" #'counsel-linux-app counsel-mode-map))
   (sys/macp
    (use-package counsel-osx-app
      :ensure t
      :bind (:map counsel-mode-map
             ("s-<f6>" . counsel-osx-app)))))

  ;; Display world clock using Ivy
  (use-package counsel-world-clock
    :ensure t
    :bind (:map counsel-mode-map
           ("C-c c k" . counsel-world-clock)))

  ;; Tramp ivy interface
  (use-package counsel-tramp
    :ensure t
    :bind (:map counsel-mode-map
           ("C-c c T" . counsel-tramp))))

;; More friendly display transformer for Ivy

(provide 'init-ivy-old)
