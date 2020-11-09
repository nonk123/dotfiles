;;; neo-init.el --- the NEO init.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Yet another remake of init.el.  Third time's the charm, right?

;;; Code:

(defun load-init ()
  "Load the NEO init-file."
  (interactive)
  (load-file "~/.emacs.d/neo-init.el"))

(setq inhibit-startup-screen 'inhibit)

(setq custom-file "~/.emacs.d/custom.el")

;; Create custom file if it doesn't exist.
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

(load-file custom-file)

;;;; Package initialization

(require 'package)

(dolist (archive '(("melpa" . "https://melpa.org/packages/")
                   ("ox-odt" . "https://kjambunathan.github.io/elpa/")))
  (add-to-list 'package-archives archive 'append))

;; Workaround for Emacs 26; GNU package archive won't work otherwise.
(when (= emacs-major-version 26)
  (defvar gnutls-algorithm-priority)
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(package-initialize)

;; Make sure `use-package' is always installed.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Force :ensure t everywhere.
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;;;; Preface

;; These modes should be set even if init-file loading goes wrong.

;;;; Utilities

(defun concat-symbols (&rest symbols)
  "Concatenate SYMBOLS as if they were strings."
  (intern (apply #'concat (mapcar #'symbol-name symbols))))

;;;; External packages

;; Alter mode lighters at will.
(use-package delight)

;;;;; Small utilities

;; M-. in `emacs-lisp-mode'.
(use-package elisp-slime-nav
  :delight
  :hook (emacs-lisp-mode . elisp-slime-nav-mode))

;; Auto-detect indentation width.
(use-package dtrt-indent
  :delight
  :hook (prog-mode . dtrt-indent-mode))

;;;; IDE-like features.

(defvar lsp-supported-hook nil
  "Hook run when opening a file supported by `lsp-remote'.")

;; TODO: add more modes.
(dolist (mode '(python))
  (add-hook (concat-symbols mode '-mode-hook)
            (lambda ()
              (run-hooks 'lsp-supported-hook))))

(use-package eglot
  :config
  ;; Inject `lsp-remote' into all servers.
  (defun stringp-and-string= (x y)
    "Return non-nil if X and Y are equal strings."
    (and (stringp x) (stringp y) (string= x y)))
  (dolist (entry eglot-server-programs)
    (when (listp (cdr entry))
      (cl-pushnew "~/.local/bin/lsp-remote" (cdr entry)
                  :test #'stringp-and-string=)))
  ;; Override `eglot' path/URI functions to support `lsp-remote'.
  (defun eglot--uri-to-path (uri)
    "The modus operandi of this function has been lost to time.

It converts `lsp-remote' URIs starting with \"/tmp/project_root/\" into local
project paths."
    (expand-file-name
     (replace-regexp-in-string
      "^/tmp/"
      (concat (projectile-project-root) "../")
      (url-filename (url-generic-parse-url uri)))))
  (defun eglot--path-to-uri (path)
    "Convert local project path into \"/tmp/\"-based remote URI."
    (concat "file:///tmp/"
            (file-relative-name path (concat (projectile-project-root path) "../"))))
  :hook (lsp-supported . eglot-ensure))

(use-package projectile
  :delight
  :init
  (when (file-exists-p "~/Sources/")
    (setq projectile-project-search-path '("~/Sources/")))
  (when (file-exists-p "~/dotfiles/")
    (projectile-add-known-project "~/dotfiles/"))
  (projectile-mode)
  :bind-keymap ("C-c p" . projectile-command-map))

(use-package flymake
  :hook ((prog-mode eglot-managed-mode) . flymake-mode)
  :bind (:map flymake-mode-map
              (("M-n" . flymake-goto-next-error)
               ("M-p" . flymake-goto-prev-error))))

(use-package company
  :delight
  :bind ("<M-tab>" . completion-at-point))

(use-package company-c-headers)

(use-package magit
  :demand
  :bind ("C-x g" . magit))

;;;;; Major modes

;; Call `use-package' on each `-mode' beginning with LANG.
;; No specific configuration is required for them.
(dolist (lang '(rust lua yaml))
  (eval `(use-package ,(concat-symbols lang '-mode))))

;;;;; Fun

(use-package simple-mpc
  :config
  ;; Override seek functions to accept prefix argument.
  (declare-function simple-mpc-seek-internal "simple-mpc.el")
  (defun simple-mpc-seek-forward (&optional arg)
    (interactive "P")
    (simple-mpc-seek-internal (or arg simple-mpc-seek-time-in-s)))
  (defun simple-mpc-seek-backward (&optional arg)
    (interactive "P")
    (simple-mpc-seek-internal (- (or arg simple-mpc-seek-time-in-s)))))

;;;; Org

(use-package org
  :config
  (setq org-confirm-babel-evaluate #'ignore)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ditaa . t)))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((plantuml . t)))
  (custom-set-variables
   '(org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar")
   '(org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar")
   ;; TODO: use a system-wide install.
   '(org-latex-to-mathml-jar-file "~/Downloads/mathtoweb.jar")
   '(org-latex-to-mathml-convert-command
     "java -jar %j -unicode -force -df %o %I")))

(use-package ox-odt)

(use-package htmlize)

;;;; Configure built-ins

(use-package icomplete
  :delight
  :init (icomplete-mode 1))

(use-package whitespace
  :delight
  :init
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face trailing tab-mark lines-tail))
  :hook ((text-mode prog-mode) . whitespace-mode))

(use-package time
  :init
  (setq display-time-default-load-average nil)
  (setq display-time-day-and-date t)
  (display-time-mode 1))

(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode))

(defun eval-region-or-buffer ()
  "If region is active, evaluate it.  Evaluate the current buffer otherwise."
  (interactive)
  (if (region-active-p)
      (eval-region (region-beginning) (region-end))
    (eval-buffer)))

(use-package emacs
  :init
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (column-number-mode 1)
  (show-paren-mode 1)
  (electric-pair-mode 1)
  ;; Disable tab indentation.
  (setq-default indent-tabs-mode nil)
  ;; Enable all disabled commands.
  (setq disabled-command-function nil)
  :bind (:map emacs-lisp-mode-map
              ("M-e" . eval-region-or-buffer)))

;;;; Miscellany

;; Don't ask if the buffer has a process running.
(setq kill-buffer-query-functions (delq #'process-kill-buffer-query-function
                                        kill-buffer-query-functions))

(defun scratch-kill-buffer-query-function ()
  "Ask for confirmation if killing *scratch*."
  (if (string= (buffer-name) "*scratch*")
      (yes-or-no-p "Are you sure? ")
    t))

(add-to-list 'kill-buffer-query-functions #'scratch-kill-buffer-query-function)

;;;; GUI

(defun run-shell-command (command)
  "Run COMMAND using the default shell."
  (interactive (list (read-shell-command "Run: ")))
  (start-process "shell" nil "bash" "-c" command))

(defun screenshot ()
  "Run screenshot script."
  (interactive)
  (run-shell-command "~/.local/bin/screenshot"))

(defun spawn-or-switch (program regexp)
  "Switch to the buffer with name matching REGEXP.  Spawn PROGRAM if no match.

Actually returns a new _command_ to do that."
  (lambda ()
    (interactive)
    (let (result)
      (dolist (buffer (mapcar #'buffer-name (buffer-list)))
        (when (string-match regexp buffer)
          (setq result buffer)))
      (if result
          (switch-to-buffer result)
        (run-shell-command program)))))

(use-package exwm
  :init
  (require 'exwm-config)
  (setq exwm-workspace-show-all-buffers t)
  (setq exwm-layout-show-all-buffers t)
  ;; Prevent floating on all windows.
  (add-hook 'exwm-floating-setup-hook #'exwm-floating-toggle-floating)
  (custom-set-variables
   '(exwm-input-global-keys
     (mapcar (lambda (x)
               (cons (kbd (car x)) (cdr x)))
             `(("s-h" . windmove-left)
               ("s-j" . windmove-down)
               ("s-k" . windmove-up)
               ("s-l" . windmove-right)
               ("s-n" . split-window-below)
               ("s-m" . split-window-right)
               ("s-i" . load-init)
               ("s-f" . exwm-layout-toggle-fullscreen)
               ("s-w" . delete-window)
               ("s-q" . kill-current-buffer)
               ("s-b" . switch-to-buffer)
               ("s-v" . ,(spawn-or-switch "qutebrowser" ".*qutebrowser$"))
               ("s-d" . ,(spawn-or-switch "discord" ".*Discord$"))
               ("s-p" . simple-mpc)
               ("s-e" . run-shell-command)
               ("<s-return>" . ansi-term)
               ("<print>" . screenshot)))))
  :bind (:map exwm-mode-map ("C-c" . nil)))

;; Fix warnings.
(declare-function exwm-workspace-rename-buffer "exwm-workspace.el")

(defun on-gui-available ()
  "Code run when GUI (e.g. X) becomes available."
  (scroll-bar-mode -1)
  (set-frame-font "Hack 9" nil t)
  (global-set-key (kbd "C-z") nil))

;; Run again when the init-file is loaded.
(when (display-graphic-p)
  (on-gui-available))

(defun exwm-init-actions ()
  "Run `on-gui-available' when EXWM is initialized."
  (on-gui-available)
  ;; Split for two monitors.
  (split-window-right))
(add-hook 'exwm-init-hook #'exwm-init-actions)

(defun exwm-update-class-actions ()
  "Change buffer name to the window's class name if its title is unset."
  (unless exwm-title
    (exwm-workspace-rename-buffer exwm-class-name)))
(add-hook 'exwm-update-class-hook #'exwm-update-class-actions)

(defun exwm-update-title-actions ()
  "Change buffer name to the window's title."
  (exwm-workspace-rename-buffer exwm-title))
(add-hook 'exwm-update-title-hook #'exwm-update-title-actions)

;;; neo-init.el ends here
