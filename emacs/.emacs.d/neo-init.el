;;; neo-init.el --- the NEO init.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Yet another remake of init.el.  Third time's the charm, right?

;;; Code:

(defun load-init ()
  "Load the NEO init-file."
  (interactive)
  (load-file (expand-file-name "neo-init.el" user-emacs-directory)))

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

;; Ensure we can actually load stuff.
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

;;;; Utilities

(require 'subr-x)

(defun concat-symbols (&rest symbols)
  "Concatenate SYMBOLS as if they were strings."
  (intern (apply #'concat (mapcar #'symbol-name symbols))))

(defun assoc-update (place key value &optional testfn)
  "Destructively update the VALUE of KEY in alist PLACE.

PLACE is a symbol pointing to an alist.

KEY and VALUE correspond to the alist entry (KEY . VALUE).

If KEY is not present in PLACE, push (KEY . VALUE).
If KEY is present, update the VALUE.

TESTFN is passed to `assoc' call on PLACE."
  (if-let ((entry (assoc key (symbol-value place) testfn)))
      (setf (cdr entry) value)
    (add-to-list place (cons key value))))

(defmacro override-defun (symbol &rest defun-args)
  "Define a new function with DEFUN-ARGS.  Override SYMBOL with it.

DEFUN-ARGS takes the same arguments as `defun', without the function name."
  (declare (indent defun))
  (let* ((override (concat-symbols symbol '--override-hack)))
    `(progn
       (defun ,override ,@defun-args)
       (advice-add #',symbol :override #',override))))

(require 'use-package-ensure)

;;;; Homebrewn packages.

;; Disable :ensure for local packages.
(setq use-package-always-ensure nil)

;; The most epic keyword.
(use-package bind-exwm
  :config (use-package-bind-exwm-do-cleanup))

;; The least epic GUI for `mpc'.
(use-package mpc-gui
  :bind-exwm ("s-p" . mpc-gui))

;; A very stupid alarm clock.
(use-package alarm-clock
  :config
  (let ((file "~/Music/Want to be Close - ATOLS Remix - Persona 3 Dancing Moon Night.opus"))
    (when (file-exists-p file)
      (setq alarm-clock-default-media-file file)
      (alarm-clock-set-schedule
       '(["6:00" alarm-clock-all-days nil]
         ["6:30" alarm-clock-all-days nil]
         ["7:00" alarm-clock-all-days nil]))))
  :bind-exwm ("s-a" . alarm-clock-stop))

;; A nice little hack.
(use-package no-pop)

(use-package ff-extras
  :no-pop play-by-filename
  :init
  (defun play-by-filename (filename)
    "Play track FILENAME with mpv."
    (start-process "mpv" (pop-to-buffer "*mpv*")
                   "mpv" "--quiet" "--no-msg-color" filename))
  (let ((audio-extensions '("mp3" "ogg" "m4a" "mid" "midi" "opus")))
    (setq ff-extras-alist `((,audio-extensions . play-by-filename)
                            (("doc" "docx" "odt") "lowriter" :file)
                            (("xlsx" "xls" "gnumeric") "gnumeric" :file)
                            (("png" "jpeg") "sxiv" :file)
                            ("pptx" "loimpress" :file)
                            ("pdf" "zathura" :file)))))

;;;; External packages

;; And re-enable for external.
(setq use-package-always-ensure t)

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

;; Code snippets.
(use-package yasnippet
  :delight yas-minor-mode
  :init (yas-global-mode 1))

;; Syntax checking. TODO: ditch for `flycheck'?
(use-package flymake
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("M-p" . flymake-goto-prev-error)
              ("M-n" . flymake-goto-next-error)))

;; Language-server client. Heavily modified for use with `lsp-remote'.
(use-package eglot
  :config
  (assoc-update 'eglot-server-programs 'rust-mode '("~/.cargo/bin/rls"))
  ;; Inject `lsp-remote' into all servers.
  (dolist (entry eglot-server-programs)
    (when (listp (cdr entry))
      (cl-pushnew "~/.local/bin/lsp-remote" (cdr entry)
                  :test (lambda (x y)
                          (and (stringp x) (stringp y) (string= x y))))))
  ;; Override `eglot' path/URI functions to support `lsp-remote'.
  (override-defun eglot--uri-to-path (uri)
    "The modus operandi of this function has been lost to time.

It converts `lsp-remote' URIs starting with \"/tmp/project_root/\" into local
project paths."
    (expand-file-name
     (replace-regexp-in-string
      "^/tmp/"
      (concat (projectile-project-root) "../")
      (url-filename (url-generic-parse-url uri)))))
  (override-defun eglot--path-to-uri (path)
    "Convert local project path into \"/tmp/\"-based remote URI."
    (concat "file:///tmp/"
            (file-relative-name path (concat (projectile-project-root path) "../"))))
  :hook ((python-mode rust-mode) . eglot-ensure)
  :bind (:map eglot-mode-map
              ("C-c d" . eldoc-doc-buffer)))

;; Project manager.
(use-package projectile
  :delight
  :init
  (setq projectile-completion-system 'default)
  (when (file-exists-p "~/Sources/")
    (setq projectile-project-search-path '("~/Sources/")))
  (when (file-exists-p "~/dotfiles/")
    (projectile-add-known-project "~/dotfiles/"))
  (projectile-mode)
  :bind-keymap ("C-c p" . projectile-command-map))

;; The silver searcher support for Projectile.
(use-package ag)

;; Git magic.
(use-package magit
  :demand
  :bind ("C-x g" . magit))

;;;;; Major modes

(use-package yaml-mode)

(use-package lua-mode)

(use-package markdown-mode)

(use-package rust-mode
  :init
  ;; Format on save. It's too tedious to do manually.
  (when (file-exists-p "~/.cargo/")
    (setq rust-cargo-bin "~/.cargo/bin/cargo")
    (setq rust-rustfmt-bin "~/.cargo/bin/rustfmt")
    (setq rust-format-on-save t))
  (setq rust-format-show-buffer nil)
  :bind (:map rust-mode-map
              ("C-c r" . rust-run)))

(defvar asy-el-dir "/usr/share/emacs/site-lisp/")

(use-package asy-mode
  ;; Prevent loading if asymptote isn't installed.
  :when (file-exists-p (expand-file-name "asy-mode.el" asy-el-dir))
  :load-path asy-el-dir)

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
  (defun org-export-to-odt-kill-libreoffice-hack (&rest args)
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

;; TODO: use something more modern?
(use-package icomplete
  :delight
  :init
  (setq completion-styles '(basic partial-completion substring emacs22))
  (icomplete-mode 1))

(use-package proced
  :no-pop
  :bind-exwm ("s-P" . proced))

(use-package whitespace
  :delight
  :init
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face trailing tab-mark lines-tail))
  :hook (prog-mode . whitespace-mode))

(use-package time
  :init
  (setq display-time-default-load-average nil)
  (setq display-time-day-and-date t)
  (display-time-mode 1))

(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode))

(use-package woman
  :bind ("C-c w" . woman))

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
  ;; C-like styles.
  (c-add-style
   "nonk123"
   '("java"
     (c-basic-offset . 4)
     (c-offsets-alist
      (access-label . /)
      (case-label . +))))
  (setq c-default-style
        '((java-mode . "java")
          (awk-mode . "awk")
          (other . "nonk123")))
  ;; Default to 4-space indentation.
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  ;; Enable all disabled commands.
  (setq disabled-command-function nil)
  :hook (text-mode . auto-fill-mode)
  :bind (("M-SPC" . cycle-spacing)
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
  :bind (:map winner-mode-map
              ("C-c u" . winner-undo)
              ("C-c U" . winner-redo))
  :bind-exwm (("s-u" . winner-undo)
              ("s-U" . winner-redo)))

(use-package exwm
  :init
  (require 'exwm-config)

  (setq exwm-workspace-show-all-buffers t)
  (setq exwm-layout-show-all-buffers t)

  ;; Prevent floating on all windows.
  (add-hook 'exwm-floating-setup-hook #'exwm-floating-toggle-floating)

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

  (defun screenshot ()
    "Run screenshot script."
    (interactive)
    (call-process "~/.local/bin/screenshot" nil 0))

  (defun run-shell-command (command)
    (interactive (list (read-shell-command "Run: ")))
    (call-process "/bin/sh" nil 0 nil "-c" command))

  (setq exwm-input-global-keys
        (mapcar
         #'exwm-normalize-binding
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
           ("s-v" . ("qutebrowser" . ".*qutebrowser$"))
           ("s-d" . ("discord" . ".*Discord$"))
           ("s-s" . "*scratch*")
           ("s-e" . run-shell-command)
           ("<s-return>" . ansi-term)
           ("<print>" . screenshot)
           ,@(use-package-exwm-bindings--unwrap))))

  ;; Apply keymap changes.
  (pcase-dolist (`(,key . ,command) exwm-input-global-keys)
    (exwm-input--set-key key command))
  (exwm-input--update-global-prefix-keys)
  :bind (:map exwm-mode-map ("C-c" . nil)))

(defun on-gui-available ()
  "Code run when GUI (e.g. X) becomes available."
  (scroll-bar-mode -1)
  (set-frame-font "Hack 9" nil t)
  (global-set-key (kbd "C-z") nil))

;; Finalize `no-pop'.
(use-package-do-no-pop)

(when (display-graphic-p)
  (on-gui-available))

;;; neo-init.el ends here
