;;; packages.el --- packages part of init.el.

;;; Commentary:

;;; Code:

(use-package delight)

(use-package helm
  :delight
  :init (require 'helm-config)
  :config (helm-mode 1)
  :bind
  (("M-x"     . helm-M-x)
   ("C-c M-x" . execute-extended-command)
   ("C-x C-f" . helm-find-files)))
(use-package helm-swoop
  :after projectile
  :bind
  (("C-c s"   . helm-swoop)
   ("C-c C-s" . helm-multi-swoop-projectile)))
(use-package helm-ag)
(use-package helm-xref)

(use-package avy
  :bind
  (("C-:" . avy-goto-line)
   ("C-;" . avy-goto-word-1)
   ("C-'" . avy-goto-char)))

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package company
  :delight
  :init (setq company-idle-delay nil))
(use-package company-c-headers
  :config (add-to-list 'company-backends 'company-c-headers))
(use-package gxref
  :init (add-to-list 'xref-backend-functions 'gxref-xref-backend))
(use-package helm-gtags
  :delight
  :init (setq-default helm-gtags-auto-update t
                      helm-gtags-ignore-case t)
  :config
  (helm-gtags-mode 1))
(use-package helm-company
  :bind
  (:map company-mode-map
        ("<M-tab>" . company-complete)
        ("<backtab>" . helm-company)
   :map company-active-map
        ("<backtab>" . helm-company)))

(use-package lsp-mode
  :delight lsp-mode lsp-lens-mode
  :commands lsp
  :init
  (setq lsp-log-io t)
  (setq lsp-auto-guess-root t)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-pyls-server-command '("~/.local/bin/pyls"))
  :bind
  (("C-c I" . lsp-organize-imports)
   ("C-c i" . lsp-goto-implementation)
   ("C-c D" . lsp-find-definition)
   ("C-c m" . helm-imenu)
   ("C-c r" . lsp-rename)
   ("C-c a" . lsp-avy-lens)))
(use-package ccls)
(use-package lsp-java
  :init (setq lsp-java-server-install-dir "~/.lsp/"))
(use-package company-lsp)
(use-package helm-lsp)
(use-package dap-mode
  :delight
  :init (require 'dap-python)
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  :bind
  (("C-c b" . dap-breakpoint-toggle)
   ("C-c n" . dap-next)
   ("C-c u" . dap-step-in)
   ("C-c o" . dap-step-out)))

(use-package flycheck
  :delight
  :init
  (setq lsp-prefer-flymake nil)
  (setq-default flycheck-disabled-checkers
                '(c/c++-clang
                  c/c++-cppcheck
                  c/c++-gcc
                  python-pylint
                  python-pycompile
                  python-mypy))
  :config (global-flycheck-mode))
(use-package lsp-ui
  :delight
  :init (setq lsp-ui-doc-enable nil))

(use-package yasnippet
  :delight yas-minor-mode
  :init
  (setq yas-triggers-in-field t)
  (setq yas-indent-line 'auto)
  :config (yas-global-mode 1))

(use-package projectile
  :delight '(:eval (concat " " (projectile-project-name)))
  :init
  (setq projectile-mode-line "Projectile")
  (setq projectile-project-search-path '("~/"))
  (setq projectile-globally-ignored-directories
        '(".git" ".hg" ".svn" "build" "target"))
  (projectile-add-known-project "/ssh:music@185.222.117.80:~/music_downloader")
  :config (projectile-mode 1)
  :bind-keymap ("C-c p" . projectile-command-map))
(use-package helm-projectile
  :config (helm-projectile-on))

(use-package treemacs
  :init
  (treemacs-git-mode 'deferred)
  (treemacs-filewatch-mode 1)
  :bind ("C-c t" . treemacs))
(use-package treemacs-projectile)
(use-package treemacs-magit)

(use-package vterm
  :disabled
  :init (setq vterm-kill-buffer-on-exit t)
  :bind ("C-x C-x" . vterm-send-C-x))

(use-package aggressive-indent)

(use-package markdown-mode)

(use-package lua-mode
  ;; Incompatible with Emacs 28 as of now.
  :disabled)

(use-package slime
  :init
  (let ((slime-helper "~/quicklisp/slime-helper.el")
        (inferior-lisp "/usr/local/bin/sbcl"))
    (when (file-exists-p slime-helper)
      (load slime-helper))
    (when (file-exists-p inferior-lisp)
      (setq inferior-lisp-program inferior-lisp)))
  (setq slime-contribs '(slime-fancy)))

(use-package sokoban
  :bind
  (:map sokoban-mode-map
        ("C-p" . sokoban-move-up)
        ("C-b" . sokoban-move-left)
        ("C-f" . sokoban-move-right)
        ("C-n" . sokoban-move-down)
        ("n" . nil)))

(use-package string-inflection)

;;; packages.el ends here
