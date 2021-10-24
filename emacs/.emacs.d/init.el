;;; init.el --- the NEO² init.el. -*- lexical-binding: t -*-

;;; Commentary:

;;; The 4th edition of my Emacs init-file.  Surely, it won't get worse, right?

;;; Code:

(defvar first-load t
  "If nil, the init file was fully loaded at least once.")

(defun load-init ()
  "Load the NEO init-file."
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory)))

(setenv "HOME" (expand-file-name "~/"))

;;;; Initialise custom file

(setq custom-file (locate-user-emacs-file "custom.el"))

;; Create the custom-file if it doesn't exist.
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

(load-file custom-file)

;;;; Bootstrap straight.el

(defvar bootstrap-version)

(when first-load
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))
  (straight-pull-recipe-repositories))

;;;; Prepare use-package

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(require 'use-package)

;;;; Install and configure packages

(use-package delight)

;;;;; Small utilities

;; Epic completions.
(use-package orderless
  :init (setq completion-styles '(orderless)))

;; Epic completion menu.
(use-package selectrum
  :commands selectrum-mode
  :init (selectrum-mode 1))

;; M-. in `emacs-lisp-mode'.
(use-package elisp-slime-nav
  :delight
  :hook (emacs-lisp-mode . elisp-slime-nav-mode))

;; Auto-detect indentation width.
(use-package dtrt-indent
  :delight
  :hook (prog-mode . dtrt-indent-mode))

;; Jump to headings like a boss.
(use-package outshine
  :delight outshine-mode
  :delight outline-minor-mode
  :preface
  (defun purge-outshine-keybindings ()
    "Prevent global-keymap pollution from `outshine-mode'."
    (assq-delete-all 'outshine-mode minor-mode-map-alist))
  :init (setq outline-minor-mode-prefix (kbd "C-c o"))
  :hook ((emacs-lisp-mode . outshine-mode)
         (outshine-mode . purge-outshine-keybindings))
  :bind (:map outline-minor-mode-map
              ("<C-tab>" . outline-cycle)
              ("M-N" . outline-next-visible-heading)
              ("M-P" . outline-previous-visible-heading)))

;;;;; IDE-like features.

;; Code snippets.
(use-package yasnippet
  :delight yas-minor-mode
  :commands yas-global-mode
  :init (yas-global-mode 1))

;; Syntax checking.
(use-package flycheck
  :delight
  :commands global-flycheck-mode
  :init
  ;; Proper syntax checking for Elisp files.
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (global-flycheck-mode 1)
  :config
  ;; Change Flycheck prefix to C-c f.
  (define-key flycheck-mode-map flycheck-keymap-prefix nil)
  (setq flycheck-keymap-prefix (kbd "C-c f"))
  (define-key flycheck-mode-map flycheck-keymap-prefix flycheck-command-map))

;; Language-server client.
(use-package lsp-mode
  :hook ((python-mode rust-mode js-mode c-mode c++-mode html-mode) . lsp)
  :defines lsp-completion-provider
  :init
  ;; Annoying as hell.
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-lens-enable t)
  (setq lsp-signature-auto-activate t)
  (setq lsp-signature-doc-lines 1)
  (setq lsp-completion-provider :none)
  :config (define-key lsp-mode-map (kbd lsp-keymap-prefix) nil)
  :bind (:map lsp-mode-map
              ("M-RET" . lsp-execute-code-action)
              ("M-." . lsp-find-declaration)
              ("M-:" . lsp-find-definition)
              ("M-r" . lsp-rename)))
(use-package lsp-ui)
(use-package ccls)
(use-package lsp-java)

;; Buffer formatting for all languages.
(use-package format-all
  :delight
  :hook ((prog-mode . format-all-mode)
         (prog-mode . format-all-ensure-formatter))
  :init (setq format-all-show-errors 'never))

;; Project manager.
(use-package projectile
  :delight
  :commands projectile-mode
  :init
  (setq projectile-completion-system 'default)
  (setq projectile-indexing-method 'alien)
  (setq projectile-project-search-path
        `((,(expand-file-name "Sources" "~/../..") . 1)))
  (defun projectile-magit ()
    "Open the Magit interface in the root of the current project."
    (interactive)
    (magit-status (projectile-acquire-root)))
  (projectile-mode 1)
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind (:map projectile-command-map
              ("g" . projectile-magit)))
(use-package flycheck-projectile
  :bind (:map projectile-command-map
              ("e" . flycheck-projectile-list-errors)))

;; File management within a project.
(use-package treemacs
  :commands (treemacs treemacs-show)
  :functions (treemacs--init treemacs-do-add-project-to-workspace
                             projectile-project-root treemacs-get-local-buffer
                             treemacs-get-local-window)
  :init
  (defun treemacs-but-stay ()
    "Toggle treemacs view without altering the current window."
    (interactive)
    (let ((current-window (selected-window)))
      (treemacs)
      (when (window-live-p current-window)
        (select-window current-window))))

  (defun treemacs-show ()
    "Show the treemacs window without losing focus."
    (interactive)
    (let ((previous-window (selected-window)))
      (when (eq (treemacs-current-visibility) 'none)
        (treemacs--init))
      (select-window previous-window)))

  (defun treemacs-add-current ()
    "Add open project to treemacs workspace."
    (interactive)
    (when-let ((path (projectile-project-root)))
      (treemacs-do-add-project-to-workspace path (treemacs--filename path)))
    (treemacs-show))
  :hook (projectile-after-switch-project . treemacs-add-current)
  :bind ("C-c t" . treemacs-but-stay))
(use-package treemacs-icons-dired
  :delight
  :commands treemacs-icons-dired-mode
  :init (treemacs-icons-dired-mode 1))
(use-package treemacs-projectile)
(use-package treemacs-magit)
(use-package lsp-treemacs)

;; Smart commands for manipulating parentheses.
(use-package smartparens
  :commands smartparens-global-strict-mode
  :init
  (require 'smartparens-config)
  (smartparens-global-strict-mode 1)
  :bind
  (:map smartparens-strict-mode-map
        ("C-M-e" . sp-end-of-sexp)
        ("C-M-a" . sp-beginning-of-sexp)
        ("C-M-n" . sp-down-sexp)
        ("C-M-p" . sp-backward-up-sexp)
        ("C-M-f" . sp-forward-sexp)
        ("C-M-b" . sp-backward-sexp)
        ("C-M-t" . sp-transpose-sexp)
        ("C-M-k" . sp-kill-sexp)
        ("C-M-<backspace>" . sp-backward-kill-sexp)
        ("M-[" . sp-backward-unwrap-sexp)
        ("M-]" . sp-unwrap-sexp)
        ("M-{" . sp-backward-slurp-sexp)
        ("M-}" . sp-forward-slurp-sexp)
        ("C-M-[" . sp-forward-barf-sexp)
        ("C-M-]" . sp-backward-barf-sexp)))

;; The silver searcher support for Projectile.
(use-package ag)

;; Git magic.
(use-package magit
  :demand
  :bind ("C-x g" . magit))

;;;;; Major modes

(use-package yaml-mode)

(use-package lua-mode
  :init (setq lua-indent-level 4))

(use-package markdown-mode)

(use-package rust-mode)

(use-package geiser-guile)
(use-package geiser-racket)

(use-package fish-mode)

;;;;; Configure built-ins

(use-package org
  :init (setq org-adapt-indentation nil))

;; A fix for `server-start'.
(use-package server
  :functions server-ensure-safe-dir
  :init (advice-add #'server-ensure-safe-dir :override (lambda (&rest ignored) t)))

(use-package whitespace
  :delight
  :init
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face trailing tab-mark lines-tail))
  :hook (prog-mode . whitespace-mode))

(use-package cc-mode
  :init
  (setq c-default-style
        '((java-mode . "java")
          (awk-mode . "awk")
          (other . "gnu"))))

(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode))

(use-package emacs
  :init
  ;; Configure the UI elements.
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (column-number-mode 1)
  (show-paren-mode 1)

  ;; Default to 4-space indentation.
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)

  ;; Enable all disabled commands.
  (setq disabled-command-function nil)

  ;; Always follow symlinks.
  (setq vc-follow-symlinks t)
  :hook (text-mode . auto-fill-mode)
  :bind (("<backtab>" . completion-at-point)
         ("M-SPC" . cycle-spacing)
         :map emacs-lisp-mode-map
         ("C-c C-b" . eval-buffer)))

;;;; Miscellany

(use-package wakatime-mode
  :delight
  :init
  ;; Wakatime requires Python 3.8 for whatever reason.
  (setq wakatime-cli-path
        (expand-file-name "~/../Local/Programs/Python/Python38/Scripts/wakatime.exe"))
  :hook ((geiser-mode lsp-managed-mode) . wakatime-mode))

;; Code formatting for org on export.
(use-package htmlize)

;; Fix the header snippet for `c-mode'.
(use-package string-inflection)

;; Don't ask if the buffer has a process running.
(setq kill-buffer-query-functions (delq #'process-kill-buffer-query-function
                                        kill-buffer-query-functions))

(defun scratch-kill-buffer-query-function ()
  "Ask for confirmation when killing *scratch*."
  (if (equal (buffer-name) "*scratch*") (yes-or-no-p "Are you sure? ") t))

(add-to-list 'kill-buffer-query-functions #'scratch-kill-buffer-query-function)

(setq inhibit-startup-screen t)

;;;; Look and Feel

(use-package color-theme-sanityinc-tomorrow
  :commands color-theme-sanityinc-tomorrow-eighties
  :init (when first-load
          (color-theme-sanityinc-tomorrow-eighties)))

(set-frame-font "Hack 9" nil t)

;; Disable GUI tweaks.
(scroll-bar-mode -1)
(fringe-mode (cons nil 1))
(blink-cursor-mode -1)
(setq visible-bell 1)

;;;; Finalise Everything

(unless (server-running-p)
  (server-start))

(setq first-load nil)

;;; init.el ends here
