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

(package-initialize)
(package-refresh-contents)

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
;; Here it comes.
;;
;; Magit stuff won't work without this.
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
  (global-flycheck-mode 1)
  (setq flycheck-emacs-lisp-load-path 'inherit))

;; Language-server client.
(use-package lsp-mode
  :hook ((python-mode rust-mode) . lsp)
  :init ;; Annoying as hell.
  (setq lsp-headerline-breadcrumb-enable nil)
  :config
  (define-key lsp-mode-map (kbd lsp-keymap-prefix) nil)
  (define-key lsp-mode-map (kbd "M-l") lsp-command-map))

;; Only useful for displaying docstrings.
(use-package lsp-ui)

;; Project manager.
(use-package projectile
  :delight
  :init
  (setq projectile-indexing-method 'alien)
  (setq projectile-completion-system 'default)
  (predicate-set 'projectile-project-search-path #'file-exists-p "~/Sources" #'list)
  (predicate-pipe "~/dotfiles" #'file-exists-p #'projectile-add-known-project)
  (projectile-mode)
  :bind-keymap ("C-c p" . projectile-command-map))

;; The silver searcher support for Projectile.
(use-package ag)

;; Git magic.
(use-package magit
  :demand
  :bind ("C-x g" . magit))

;; More magic.
(use-package ssh-agency
  :demand)

;;;;; Major modes

(use-package yaml-mode)

(use-package lua-mode)

(use-package markdown-mode)

(use-package rust-mode)

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
  (set-frame-font "Hack 10" nil t)
  (use-package apropospriate-theme
    :init
    (setq dl-dark-theme 'apropospriate-dark)
    (setq dl-light-theme 'apropospriate-light)
    (when first-load
      (dl-switch))))

;; Finalize `no-pop'.
(use-package-commit-no-pop)

(when (display-graphic-p)
  (on-gui-available))

(server-start)

(setq first-load nil)

;;; init.el ends here
