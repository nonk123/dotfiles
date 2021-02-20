;;; init.el --- the NEO init.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Yet another remake of init.el.  Third time's the charm, right?

;;; Code:

;; !!HACK WARNING!!
;;
;; $HOME will be overriden later in the file.
;; It is saved and restored at this point to ensure .emacs.d works.

(defvar real-home (expand-file-name "~"))

(setq user-emacs-directory (expand-file-name ".emacs.d" real-home))
(setq package-user-dir (locate-user-emacs-file "elpa"))

;; In case of an init-file reload.
(setenv "HOME" real-home)

(defvar first-load t
  "If nil, the init file was fully loaded at least once.")

(defun load-init ()
  "Load the NEO init-file."
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory)))

;; Since HOME is overriden, we have to use a dirty hack.
(setq custom-file (concat "C:/Users/" user-login-name "/AppData/Roaming/.emacs.d/custom.el"))

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
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

;; Make Menu key a prefix.
(define-prefix-command 'menu-prefix)
(global-set-key (kbd "<apps>") 'menu-prefix)

(require 'use-package-ensure)

;;;; $HOME hack.

;; !!VERY IMPORTANT!!
;;
;; Here it comes. Magit stuff won't work without this.
;;
;; "~" expands to %AppData%. Also not good.

(setenv "HOME" (concat "C:\\Users\\" user-login-name))

;;;; Homebrewn packages.

;; Disable :ensure for local packages.
(setq use-package-always-ensure nil)

(use-package my-utils)

;; The least epic GUI for `mpc'.
(use-package mpc-gui
  :config
  (defun mpc-gui-extra-actions ()
    "Set single and repeat to 1."
    (mpc-gui-run-mpc "repeat" 1)
    (mpc-gui-run-mpc "single" 1))
  (add-hook 'mpc-gui-mode-hook #'mpc-gui-extra-actions)
  :bind ("<apps> p" . mpc-gui))

;; A very stupid alarm clock.
(use-package alarm-clock
  :demand
  :config
  (let* ((file "Want to be Close - ATOLS Remix - Persona 3 Dancing Moon Night.mp4")
         (file (expand-file-name file "~/Music")))
    (when (file-exists-p file)
      (setq alarm-clock-default-media-file file)
      (alarm-clock-set-schedule
       '(["6:00" alarm-clock-all-days nil]
         ["6:30" alarm-clock-all-days nil]
         ["7:00" alarm-clock-all-days nil]))))
  :bind ("<apps> a" . alarm-clock-stop))

;; Epic theming stuff.
(use-package dark-light
  :demand
  :bind ("C-c l" . dl-switch))

;; A nice little hack.
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
  :config
  (define-key lsp-mode-map (kbd lsp-keymap-prefix) nil)
  (define-key lsp-mode-map (kbd "M-l") lsp-command-map))

(use-package lsp-python-ms
  :init (setq lsp-python-ms-auto-install-server t))

;; Only useful for displaying docstrings.
(use-package lsp-ui)

;; Project manager.
(use-package projectile
  :delight
  :init
  (defun add-project (path)
    "Call `projectile-add-known-project' on PATH if it exists."
    (predicate-pipe path #'file-exists-p #'projectile-add-known-project))
  (setq projectile-indexing-method 'alien)
  (setq projectile-completion-system 'default)
  (let ((dots-path "~/dotfiles")
        (sources-path "~/Sources")
        search-path)
    (when (file-exists-p dots-path)
      (push dots-path search-path))
    (when (file-exists-p sources-path)
      (push sources-path search-path))
    (add-project dots-path)
    (setq projectile-project-search-path search-path))
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

(use-package ahk-mode)

(use-package gdscript-mode)

;;;; Org

(use-package org
  :hook (org-mode . org-mode-actions)
  :no-pop org-edit-special
  :init
  (defun org-mode-actions ()
    "Set appropriate `fill-column' and disable `electric-indent-local-mode'."
    (electric-indent-local-mode -1)
    (set-fill-column 72))
  :config
  ;; TODO: none of the stuff below works on Windows.
  (setq org-confirm-babel-evaluate #'ignore)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ditaa . t)
     (plantuml . t)
     (asymptote .t)))
  (custom-set-variables
   '(org-odt-preferred-output-format "doc")
   '(org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar")
   '(org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar"))
  ;; A little hack to prevent "export failed" errors when LibreOffice is open.
  (defun org-export-to-odt-kill-libreoffice-hack (&rest _ignored)
    "Kill LibreOffice before exporting to ODT."
    (dolist (process (process-list))
      (when (string-prefix-p "soffice " (process-name process))
        (interrupt-process process))))
  (advice-add #'org-odt-export-to-odt :before #'org-export-to-odt-kill-libreoffice-hack))

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
  :no-pop
  :bind ("<apps> P" . proced))

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

(use-package sgml-mode
  :bind (:map sgml-mode-map ("§" . completion-at-point)))

(use-package emacs
  :init
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

  (defun eval-region-or-buffer ()
    "If region is active, evaluate it.  Evaluate the current buffer otherwise."
    (interactive)
    (if (region-active-p)
        (eval-region (region-beginning) (region-end))
      (eval-buffer)))
  :hook (text-mode . auto-fill-mode)
  :bind (("§" . completion-at-point)
         ("M-SPC" . cycle-spacing)
         :map emacs-lisp-mode-map
         ("M-e" . eval-region-or-buffer)))

;;;; Miscellany

;; Fixes the header snippet for c-mode.
(use-package string-inflection)

;; Don't ask if the buffer has a process running.
(setq kill-buffer-query-functions (delq #'process-kill-buffer-query-function
                                        kill-buffer-query-functions))

(defun scratch-kill-buffer-query-function ()
  "Ask for confirmation if killing *scratch*."
  (if (equal (buffer-name) "*scratch*")
      (yes-or-no-p "Are you sure? ")
    t))

(add-to-list 'kill-buffer-query-functions #'scratch-kill-buffer-query-function)

;;;; GUI

(use-package winner
  :init (winner-mode 1)
  :bind (("<apps> u" . winner-undo)
         ("<apps> U" . winner-redo)))

(defun on-gui-available ()
  "Code run when GUI (e.g., X) becomes available."
  (scroll-bar-mode -1)
  (set-frame-font "Go Mono for Powerline 10" nil t)
  (global-unset-key (kbd "C-z"))
  (use-package solarized-theme
    :init
    (setq solarized-use-less-bold t)
    (setq solarized-use-variable-pitch nil)
    (setq solarized-scale-org-headlines nil)
    (setq solarized-scale-outline-headlines nil)
    :config
    (dl-set-dark-theme 'solarized-dark)
    (dl-set-light-theme 'solarized-light)
    (when first-load
      (dl-go-light))))

;; Finalize `no-pop'.
(use-package-commit-no-pop)

(when (display-graphic-p)
  (on-gui-available))

(server-start)

(setq first-load nil)

;;; init.el ends here
