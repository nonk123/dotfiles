;;; packages.el --- packages part of init.el. -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package delight)

(use-package helm
  :delight
  :init (require 'helm-config)
  :config (helm-mode 1)
  :bind
  (("M-x"     . helm-M-x)
   ("C-x b"   . helm-buffers-list)
   ("C-c M-x" . execute-extended-command)
   ("C-x C-f" . helm-find-files)))
(use-package helm-swoop
  :after helm projectile
  :bind
  (("C-c s"   . helm-swoop)
   ("C-c C-s" . helm-multi-swoop-projectile)))
(use-package helm-ag
  :after helm)
(use-package helm-xref
  :after helm)

(use-package avy
  :bind
  (("C-:" . avy-goto-line)
   ("C-;" . avy-goto-word-1)
   ("C-'" . avy-goto-char)))

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package company
  :delight
  :init (setq company-idle-delay nil)
  :hook ((prog-mode sgml-mode) company-mode))
(use-package company-c-headers
  :after company
  :config (add-to-list 'company-backends 'company-c-headers))
(use-package gxref
  :config (add-to-list 'xref-backend-functions 'gxref-xref-backend))
(use-package helm-gtags
  :delight
  :after helm
  :init (setq-default helm-gtags-auto-update t
                      helm-gtags-ignore-case t)
  :config (helm-gtags-mode 1))
(use-package helm-company
  :after helm company
  :bind
  (:map company-mode-map
        ("<M-tab>" . company-complete)
        ("<backtab>" . helm-company)
        :map company-active-map
        ("<backtab>" . helm-company)))

(defun my-lsp-remote (server &rest args)
  (lsp-stdio-connection
   (lambda ()
     `("bash" "-lc" ,(format "lsp-remote %s %s %s"
                             server
                             (projectile-project-root)
                             (string-join args " "))))))

(defvar lsp-remote-dir "/tmp/"
  "Directory where local projects are stored on the remote server.")

(defun lsp-remote-uri->path (uri)
  (expand-file-name
   (replace-regexp-in-string
    (format "^%s" lsp-remote-dir)
    (concat (projectile-project-root) (file-name-as-directory ".."))
    (url-filename (url-generic-parse-url uri)))))

(defun lsp-remote-local->remote (path)
  (concat
   lsp-remote-dir
   (file-relative-name path (concat (projectile-project-root path) ".."))))

(defun lsp-remote-path->uri (path)
  (concat "file://" (lsp-remote-local->remote path)))

(defun make-lsp-remote-client (cmd &rest args)
  (let ((remote (temporary-file-directory)))
    (apply #'make-lsp-client
           :new-connection (apply 'my-lsp-remote (car cmd) (cdr cmd))
           :uri->path-fn 'lsp-remote-uri->path
           :path->uri-fn 'lsp-remote-path->uri
           :priority 10
           args)))

(use-package lsp-mode
  :delight lsp-lens-mode
  :init
  (setq lsp-auto-configure nil)
  (setq lsp-keymap-prefix nil)
  (setq lsp-log-io t)
  (setq lsp-lens-auto-enable t)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-server-install-dir "~/.lsp/")
  (setq lsp-pyls-server-command '("python3" "-m" "pyls"))
  (setq lsp-idle-delay 1)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-enable-folding nil)
  (defvar lsp-on-touch-time 0)
  (defadvice lsp-on-change (around lsp-on-change-hack activate)
    (when (> (- (float-time (current-time)) lsp-on-touch-time) 5)
      (setq lsp-on-touch-time (float-time (current-time)))
      ad-do-it))
  :config
  ;; JS.
  (lsp-register-client
   (make-lsp-remote-client '("javascript-typescript-stdio")
                           :major-modes '(javascript-mode js-mode)
                           :completion-in-comments? t
                           :server-id 'jsts-ssh))
  ;; HTML.
  (lsp-register-client
   (make-lsp-remote-client '("html-languageserver" "--stdio")
                           :major-modes '(html-mode sgml-mode)
                           :completion-in-comments? t
                           :server-id 'html-ls-ssh
                           :initialized-fn (lambda (w)
                                             (with-lsp-workspace w
                                               (lsp--set-configuration
                                                (lsp-configuration-section "html"))))))
  ;; Python.
  (lsp-register-client
   (make-lsp-remote-client lsp-pyls-server-command
                           :major-modes '(python-mode cython-mode)
                           :server-id 'pyls-ssh
                           :library-folders-fn (lambda (_workspace)
                                                 lsp-clients-python-library-directories)
                           :initialized-fn (lambda (workspace)
                                             (with-lsp-workspace workspace
                                               (lsp--set-configuration (lsp-configuration-section "pyls"))))))
  :hook ((prog-mode html-mode sgml-mode mhtml-mode web-mode) . lsp)
  :bind
  (("C-c r" . lsp-rename)
   ("C-c i" . lsp-organize-imports)
   ("C-c f" . lsp-execute-code-action)))
;; Needed just for docstring extraction.
(use-package lsp-ui)

(use-package company-lsp
  :after company lsp)
(use-package helm-lsp
  :after helm lsp)
(use-package ccls)
(use-package lsp-java
  :after lsp)

(use-package flycheck
  :after lsp
  :init (setq-default flycheck-disabled-checkers
                      '(c/c++-clang
                        c/c++-cppcheck
                        c/c++-gcc
                        python-pylint
                        python-pycompile
                        python-mypy))
  :config (global-flycheck-mode)
  :bind
  (("M-n" . flycheck-next-error)
   ("M-p" . flycheck-previous-error)))

(use-package yasnippet
  :delight yas-minor-mode
  :init
  (setq yas-triggers-in-field t)
  (setq yas-indent-line 'auto)
  :config (yas-global-mode 1))

(defun my-projectile-project-find-function (dir)
  "Bridge between projectile and project.el."
  (let ((root (projectile-project-root dir)))
    (and root (cons 'transient root))))

(use-package projectile
  :init
  (add-to-list 'project-find-functions 'my-projectile-project-find-function)
  (setq projectile-project-search-path '("~/Sources"))
  (projectile-add-known-project "~/dotfiles")
  (setq projectile-globally-ignored-directories
        '(".git" ".hg" ".svn" "build" "target"))
  :config (projectile-mode 1)
  :bind-keymap ("C-c p" . projectile-command-map))
(use-package helm-projectile
  :after helm projectile
  :config (helm-projectile-on))

(use-package vterm
  :init
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-shell "/bin/bash -l")
  :bind
  (("C-x C-x" . vterm-send-C-x)
   ("C-x x"   . exchange-point-and-mark)))

(use-package markdown-mode)

(use-package lua-mode
  ;; Incompatible with Emacs 28 as of now.
  :disabled)

(use-package slime
  :disabled
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

(use-package xref
  :hook (emacs-lisp-mode . xref-etags-mode))

(use-package whitespace
  :delight
  :init
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face trailing tab-mark lines-tail))
  :hook ((prog-mode sgml-mode) . whitespace-mode))

(use-package eldoc
  :delight)

(use-package display-line-numbers
  :delight
  :hook ((prog-mode sgml-mode) . display-line-numbers-mode))

(use-package vc
  :init (setq vc-handled-backends nil))

(use-package emacs
  :init (setq initial-major-mode 'fundamental-mode)
  :hook (text-mode . (lambda ()
                       (interactive)
                       (set-fill-column 80)
                       (auto-fill-mode))))

;;; packages.el ends here
