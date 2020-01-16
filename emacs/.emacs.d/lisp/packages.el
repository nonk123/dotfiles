;;; packages.el --- packages part of init.el.

;;; Commentary:

;;; Code:

(use-package helm
  :init (require 'helm-config)
  :bind
  (("M-x" . helm-M-x)
   ("C-c M-x" . execute-extended-command)
   ("C-x C-f" . helm-find-files)))
(use-package helm-xref)

(use-package avy
  :bind
  (("C-;" . avy-goto-line)
   ("C-:" . avy-goto-word-1)))

(use-package company)
(use-package company-c-headers
  :config (add-to-list 'company-backends 'company-c-headers))
(use-package helm-gtags
  :init (setq-default helm-gtags-auto-update t
                      helm-gtags-ignore-case t))
(use-package helm-company
  :bind
  (:map company-mode-map
        ("<M-tab>" . company-complete)
        ("<backtab>" . helm-company)
   :map company-active-map
        ("<backtab>" . helm-company)))

(use-package lsp-mode
  :commands lsp
  :init
  (setq lsp-auto-guess-root t)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-enable-xref t)
  (setq lsp-enable-imenu t)
  :bind
  (("C-c I" . lsp-organize-imports)
   ("C-c i" . lsp-goto-implementation)
   ("C-c d" . lsp-find-definition)
   ("C-c m" . helm-imenu)
   ("C-c x" . xref-find-definitions)
   ("C-c r" . lsp-rename)
   ("C-c a" . lsp-avy-lens)))
(use-package ccls)
(use-package lsp-java)
(use-package company-lsp)
(use-package helm-lsp)

(use-package flycheck
  :init
  (setq lsp-prefer-flymake nil)
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  (global-flycheck-mode))
(use-package lsp-ui)

(use-package yasnippet
  :init
  (setq yas-triggers-in-field t)
  (setq yas-indent-line 'fixed)
  :config (yas-global-mode 1))

(use-package projectile
  :init (setq projectile-project-search-path '("~/Sources/" "~/"))
  :config (projectile-mode 1)
  :bind-keymap ("C-c p" . projectile-command-map))
(use-package helm-projectile
  :config (helm-projectile-on))

(use-package markdown-mode)

(use-package slime
  :init
  (setq inferior-lisp-program "/usr/local/bin/sbcl")
  (setq slime-contribs '(slime-fancy)))

(use-package sokoban
  :bind
  (:map sokoban-mode-map
        ("C-p" . sokoban-move-up)
        ("C-b" . sokoban-move-left)
        ("C-f" . sokoban-move-right)
        ("C-n" . sokoban-move-down)
        ("n" . nil)))

(use-package tetris
  :bind
  (:map tetris-mode-map
        ("C-p" . tetris-rotate-prev)
        ("C-b" . tetris-move-left)
        ("C-f" . tetris-move-right)
        ("C-n" . tetris-move-bottom)))

(use-package windmove
  :bind
  (("C-c h" . windmove-left)
   ("C-c j" . windmove-down)
   ("C-c k" . windmove-up)
   ("C-c l" . windmove-right)))

(use-package string-inflection)

;;; packages.el ends here
