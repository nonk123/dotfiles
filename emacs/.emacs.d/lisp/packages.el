;;; packages.el --- packages part of init.el. -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package delight)

(use-package helm
  :delight
  :init (require 'helm-config)
  :config (helm-mode 1)
  :bind (("M-x"     . helm-M-x)
         ("C-x b"   . helm-buffers-list)
         ("C-c M-x" . execute-extended-command)
         ("C-x C-f" . helm-find-files)))
(use-package helm-swoop
  :after (helm projectile)
  :bind (("C-c s"   . helm-swoop)
         ("C-c C-s" . helm-multi-swoop-projectile)))
(use-package helm-ag
  :after helm)
(use-package helm-xref
  :after helm)

(use-package avy
  :bind (("C-:" . avy-goto-line)
         ("C-;" . avy-goto-word-1)
         ("C-'" . avy-goto-char)))

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package company
  :delight
  :init (setq company-idle-delay nil)
  :hook ((prog-mode sgml-mode xml-mode) . company-mode))
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
  :hook (company-mode . helm-gtags-mode))
(use-package helm-company
  :after (helm company)
  :bind
  (:map company-mode-map
        ("<M-tab>" . helm-company)))

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
  (defun lsp--auto-configure-after-disable-lsp-ui-hack ()
    (lsp-ui-mode 0))
  (advice-add 'lsp--auto-configure :after #'lsp--auto-configure-after-disable-lsp-ui-hack)
  (setq lsp-keymap-prefix nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-log-io t)
  (setq lsp-lens-auto-enable t)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-server-install-dir "~/.lsp/")
  (setq lsp-idle-delay 0.5)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-enable-folding nil)
  (setq lsp-pyls-server-command '("python3" "-m" "pyls"))
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
                           :major-modes '(html-mode mhtml-mode css-mode sgml-mode)
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
  :hook ((prog-mode sgml-mode xml-mode) . lsp)
  :bind
  (:map lsp-mode-map
        ("C-c r" . lsp-rename)
        ("C-c i" . lsp-organize-imports)
        ("C-c f" . lsp-execute-code-action)))
;; Needed just for docstring extraction.
(use-package lsp-ui)

(use-package company-lsp
  :after (company lsp-mode)
  :config (add-to-list 'company-backends 'company-lsp))
(use-package helm-lsp
  :after (helm lsp-mode))
(use-package ccls)
(use-package lsp-java
  :after lsp-mode)

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
  :after (helm projectile)
  :init (helm-projectile-on))

(use-package vterm
  :init
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-shell "/bin/bash -l")
  :bind
  (:map vterm-mode-map
        ("C-c C-x" . vterm-send-C-x)))

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

(use-package aggressive-indent
  :hook (((prog-mode html-mode mhtml-mode sgml-mode) . aggressive-indent-mode)
         (python-mode . (lambda () (interactive) (aggressive-indent-mode 0)))))

(use-package highlight-indent-guides
  :init (setq highlight-indent-guides-method 'bitmap)
  :hook ((prog-mode html-mode mhtml-mode smgl-mode) . highlight-indent-guides-mode))

(use-package smartparens
  :init
  (require 'smartparens-config)
  (setq sp-highlight-pair-overlay nil)
  :hook ((prog-mode html-mode mhtml-mode smgl-mode) . smartparens-mode))

(use-package elisp-slime-nav
  :delight
  :hook ((emacs-lisp-mode ielm-mode) . elisp-slime-nav-mode))

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

(use-package tetris
  :bind
  (:map tetris-mode-map
        ("C-p" . tetris-rotate-prev)
        ("C-b" . tetris-move-left)
        ("C-f" . tetris-move-right)
        ("C-n" . tetris-move-bottom)))

(use-package display-line-numbers
  :delight
  :hook ((prog-mode sgml-mode) . display-line-numbers-mode))

(use-package vc
  :init (setq vc-handled-backends nil))

(use-package flymake
  :hook ((prog-mode sgml-mode xml-mode markdown-mode) . flymake-mode)
  :bind
  (:map flymake-mode-map
        ("M-n" . flymake-goto-next-error)
        ("M-p" . flymake-goto-prev-error)))

(use-package emacs
  :mode (("\\.bash" . sh-mode)
         ("\\.gitignore" . prog-mode))
  :hook (text-mode . (lambda ()
                       (interactive)
                       (set-fill-column 80)
                       (auto-fill-mode)))
  :bind (("C-x C-b" . ibuffer)
         ("<S-tab>" . ff-find-other-file)
         ("s-i" . load-init)))

;;; packages.el ends here
