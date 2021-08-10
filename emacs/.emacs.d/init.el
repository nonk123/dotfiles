;;; init.el --- the NEO² init.el -*- lexical-binding: t -*-

;;; Commentary:

;;; The 4th edition of my Emacs init-file.  Surely, it won't get worse, right?

;;; Code:

(defvar first-load t
  "If nil, the init file was fully loaded at least once.")

(defun load-init ()
  "Load the NEO init-file."
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory)))

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

;;;; Install and configure packages

(use-package delight)

;;;;; Small utilities

;; Epic completions.
(use-package orderless
  :init (setq completion-styles '(orderless)))

;; Epic completion menu.
(use-package selectrum
  :init (selectrum-mode 1))

;; M-. in `emacs-lisp-mode'.
(use-package elisp-slime-nav
  :delight
  :hook (emacs-lisp-mode . elisp-slime-nav-mode))

;; Auto-detect indentation width.
(use-package dtrt-indent
  :delight
  :hook (prog-mode . dtrt-indent-mode))

;; Do something with whitespace.
(use-package ws-butler
  :delight
  :init (setq ws-butler-keep-whitespace-before-point nil)
  :hook (prog-mode . ws-butler-mode))

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
              ("<backtab>" . outline-cycle)
              ("M-N" . outline-next-visible-heading)
              ("M-P" . outline-previous-visible-heading)))

;;;;; IDE-like features.

;; Code snippets.
(use-package yasnippet
  :delight yas-minor-mode
  :init (yas-global-mode 1))

;; Syntax checking.
(use-package flycheck
  :delight
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
  :init
  ;; Annoying as hell.
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-signature-auto-activate t)
  (setq lsp-signature-doc-lines 1)
  :config (define-key lsp-mode-map (kbd lsp-keymap-prefix) nil)
  :bind (:map lsp-mode-map
              ("M-l" . lsp-format-buffer)
              ("M-RET" . lsp-execute-code-action)
              ("M-." . lsp-find-declaration)
              ("M-:" . lsp-find-definition)))
(use-package lsp-ui)
(use-package ccls)
(use-package lsp-java)

;; Project manager.
(use-package projectile
  :delight
  :init
  (setq projectile-completion-system 'default)
  (setq projectile-project-search-path '(("~/Sources/" . 1)))
  (setq projectile-enable-caching t)
  (projectile-mode)
  :bind-keymap ("C-c p" . projectile-command-map))
(use-package flycheck-projectile
  :bind (:map projectile-command-map
              ("e" . flycheck-projectile-list-errors)))

;; File management within a project.
(use-package treemacs
  :init
  (defun treemacs-show ()
    "Show the treemacs window without switching to it."
    (interactive)
    (let ((previous-window (selected-window)))
      (when (eq (treemacs-current-visibility) 'none)
        (treemacs--init))
      (select-window previous-window)))
  (defun treemacs-but-stay ()
    "Toggle treemacs view without altering the current window."
    (interactive)
    (let ((current-window (selected-window)))
      (treemacs)
      (when (window-live-p current-window)
        (select-window current-window))))
  (defun treemacs-after-project-is-open ()
    "Automatically add an opened project to the treemacs workspace."
    (when-let ((path (projectile-project-root)))
      (treemacs-do-add-project-to-workspace path (treemacs--filename path)))
    (treemacs-show))
  :hook (projectile-after-switch-project . treemacs-after-project-is-open)
  :bind ("C-c t" . treemacs-but-stay))
(use-package treemacs-icons-dired
  :delight
  :init (treemacs-icons-dired-mode 1))
(use-package treemacs-projectile)
(use-package treemacs-magit)
(use-package lsp-treemacs)

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

;;;;; Org

(use-package org
  :hook (org-mode . org-mode-actions))

;; Code formatting on export.
(use-package htmlize)

;;;;; Configure built-ins

;; A fix for Magit.
(use-package server
  :config
  (defun true (&rest _args)
    "Ignore ARGS, return t."
    t)
  (advice-add #'server-ensure-safe-dir :override #'true))

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


(use-package time
  :init
  (setq display-time-default-load-average nil)
  (setq display-time-interval 1)
  (setq display-time-format "%H:%M:%S %a %d.%m.%y")
  (display-time-mode 1))

(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode))

(use-package emacs
  :init
  ;; Configure the UI elements.
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (column-number-mode 1)
  (show-paren-mode 1)
  (electric-pair-mode 1)

  ;; Default to 4-space indentation.
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)

  ;; Enable all disabled commands.
  (setq disabled-command-function nil)

  ;; Always follow symlinks.
  (setq vc-follow-symlinks t)

  (defun eval-region-or-buffer ()
    "If region is active, evaluate it.  Evaluate the current buffer otherwise."
    (interactive)
    (if (region-active-p)
        (eval-region (region-beginning) (region-end))
      (eval-buffer)))
  :hook (text-mode . auto-fill-mode)
  :bind (("M-SPC" . cycle-spacing)
         :map emacs-lisp-mode-map
         ("M-e" . eval-region-or-buffer)))

;;;; Miscellany

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

;;;; EXWM

(defun execute-command (command)
  "Execute COMMAND asynchronously using the default shell."
  (interactive (list (read-shell-command "$ ")))
  (start-process "*shell*" nil (getenv "SHELL") "-c" command))

(defun lambda-run (command)
  "Return a lambda calling (execute-command COMMAND)."
  (lambda () (interactive) (execute-command command)))

(use-package vterm
  :init (defun vterm-new-session ()
          (interactive) (vterm t)))

(use-package exwm
  :init
  (add-hook 'exwm-update-class-hook
          (lambda ()
            (exwm-workspace-rename-buffer exwm-class-name)))
  (add-hook 'exwm-update-title-hook
            (lambda ()
              (unless exwm-instance-name
                (exwm-workspace-rename-buffer exwm-title))))

  (setq exwm-manage-configurations '((t char-mode t)))

  (setq
   exwm-input-global-keys
   (mapcar
    (lambda (cell)
      (cons (kbd (car cell)) (cdr cell)))
    `(("s-h" . windmove-left)
      ("s-j" . windmove-down)
      ("s-k" . windmove-up)
      ("s-l" . windmove-right)
      ("s-n" . split-window-below)
      ("s-m" . split-window-right)
      ("s-x" . delete-window)
      ("s-q" . kill-current-buffer)
      ("s-z" . previous-buffer)
      ("s-b" . switch-to-buffer)
      ("s-d" . execute-command)
      ("s-w" . exwm-floating-toggle-floating)
      ("s-f" . exwm-layout-toggle-fullscreen)
      ("s-i" . load-init)
      ("s-<return>" . vterm-new-session)
      ("s-p" . ,(lambda-run "mpc toggle"))
      ,@(mapcar (lambda (i)
                  `(,(format "s-%d" i) .
                    (lambda ()
                      (interactive)
                      (exwm-workspace-switch-create ,i))))
                (number-sequence 0 9))
      ("<print>" . ,(lambda-run "screenshot region"))
      ("S-<print>" . ,(lambda-run "screenshot display")))))

  (require 'exwm-systemtray)
  (exwm-systemtray-enable)
  (exwm-enable)

  ;; Update EXWM keys if they are changed.
  (pcase-dolist (`(,key . ,command) exwm-input-global-keys)
    (exwm-input--set-key key command))
  (exwm-input--update-global-prefix-keys))

(set-frame-font "Hack 9" nil t)
(fringe-mode (cons nil 1))
(scroll-bar-mode -1)

(use-package color-theme-sanityinc-tomorrow
  :init (when first-load
          (color-theme-sanityinc-tomorrow-eighties)))

(when first-load
  (make-directory server-socket-dir t)
  (server-start))

(setq first-load nil)

;;; init.el ends here
