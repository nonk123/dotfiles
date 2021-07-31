;;; init.el --- the NEO² init.el -*- lexical-binding: t -*-

;;; Commentary:

;;; The 4th edition of my Emacs init-file.  Surely, it won't get worse, right?

;;; Code:

(setq inhibit-startup-screen t)

(setq package-user-dir (locate-user-emacs-file "elpa"))
(setq custom-file (locate-user-emacs-file "custom.el"))

(defvar first-load t
  "If nil, the init file was fully loaded at least once.")

(defun load-init ()
  "Load the NEO init-file."
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory)))

;; Create the custom-file if it doesn't exist.
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

(load-file custom-file)

;;;; Package initialization

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("ox-odt" . "https://kjambunathan.github.io/elpa/") 'append)

;; Workaround for Emacs 26; GNU package archive won't work otherwise.
(when (= emacs-major-version 26)
  (defvar gnutls-algorithm-priority)
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(when first-load
  (package-initialize)
  (package-refresh-contents))

;; Make sure `use-package' is always installed.
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Ensure we can actually load stuff.
(add-to-list 'load-path (locate-user-emacs-file "lisp/"))

(require 'use-package-ensure)

;;;; Homebrewn packages.

(setq use-package-always-ensure nil)
(use-package my-utils)
(use-package no-pop)

;;;; External packages

(setq use-package-always-ensure t)

;; Alter mode lighters at will.
(use-package delight)

;;;;; Small utilities

;; Epic completions.
(use-package selectrum
  :init
  (setq completion-styles '(flex))
  (setq selectrum-num-candidates-displayed 20)
  (selectrum-mode 1))

;; M-. in `emacs-lisp-mode'.
(use-package elisp-slime-nav
  :delight
  :hook (emacs-lisp-mode . elisp-slime-nav-mode))

;; Auto-detect indentation width.
(use-package dtrt-indent
  :delight
  :hook (prog-mode . dtrt-indent-mode))

;; Do something with whitespaces.
(use-package ws-butler
  :delight
  :hook (prog-mode . ws-butler-mode))

;;;; IDE-like features.

;; Code snippets.
(use-package yasnippet
  :delight yas-minor-mode
  :init (yas-global-mode 1))

;; Syntax checking.
(use-package flycheck
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
  :bind ("M-l" . lsp-format-buffer))

(use-package lsp-java)

;; Only useful for displaying docstrings.
(use-package lsp-ui)

;; Project manager.
(use-package projectile
  :delight
  :init
  (setq projectile-completion-system 'default)
  (setq projectile-project-search-path '(("~/Sources" . 1)))
  (setq projectile-enable-caching t)
  (projectile-mode)
  :bind-keymap ("C-c p" . projectile-command-map))

(use-package flycheck-projectile
  :bind (:map projectile-command-map
              ("e" . flycheck-projectile-list-errors)))

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

;;;; Org

(use-package org
  :hook (org-mode . org-mode-actions)
  :no-pop org-edit-special)

;; ODT exporter fork to make .doc trickery more manageable.
(use-package ox-odt)

;; Code formatting on export.
(use-package htmlize)

;;;; Configure built-ins

;; A fix for Magit.
(use-package server
  :config
  (defun true (&rest _args)
    "Ignore ARGS, return t."
    t)
  (advice-add #'server-ensure-safe-dir :override #'true))

(use-package proced
  :demand
  :no-pop)

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
  (setq display-time-day-and-date t)
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

;;;; GUI

(defun configure-font (&optional frame)
  (set-frame-font "Hack 9" nil t))

(add-hook 'after-make-frame-functions #'configure-font)
(add-hook 'server-after-make-frame-functions #'configure-font)

(scroll-bar-mode -1)

(use-package frames-only-mode
  :init (frames-only-mode 1))

(use-package vterm
  :init (defun vterm-new-session () (interactive) (vterm t))
  :bind ("C-c RET" . vterm-new-session))

(use-package color-theme-sanityinc-tomorrow
  :init (when first-load
          (color-theme-sanityinc-tomorrow-eighties)))

;; Finalize `no-pop'.
(use-package-commit-no-pop)

(setq first-load nil)

(make-directory server-socket-dir t)
(server-start)

;;; init.el ends here
