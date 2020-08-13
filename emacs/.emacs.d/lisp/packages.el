;;; packages.el --- packages part of init.el. -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package delight)

(use-package helm
  :demand
  :delight (helm-mode) (helm-ff-cache-mode)
  :init (require 'helm-config)
  :config (helm-mode 1)
  :bind (("M-x"     . helm-M-x)
         ("C-x b"   . helm-buffers-list)
         ("C-c M-x" . execute-extended-command)
         ("C-x C-f" . helm-find-files)))
(use-package helm-swoop
  :after (helm projectile))
(use-package helm-ag
  :after helm)
(use-package helm-xref
  :after helm)

(use-package avy
  :init (setq avy-keys '(?h ?j ?k ?l ?a ?s ?d ?f)))

(use-package magit)

(use-package company
  :delight
  :init (setq company-idle-delay nil)
  :config
  (dolist (disabled '(company-eclim company-clang company-xcode))
    (setq company-backends (delete disabled company-backends)))
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

(defun use-lsp-remote (con)
  (if (listp (cdr con))
      (let ((shell "bash")
            (server-command (cadr con))
            (server-args (string-join
                          (mapcar
                           (lambda (x)
                             (if (eq x :autoport) "1337" x))
                           (cddr con))
                          " ")))
        (if (and (stringp server-command)
                 (not (string-suffix-p "lsp-remote" server-command)))
            (cons (car con)
                  `("~/.local/bin/lsp-remote" ,server-command ,server-args))
          con))
    con))

(defconst level-up (file-name-as-directory ".."))

(use-package eglot
  :demand
  :commands (eglot eglot-ensure)
  :init
  (setq eglot-autoreconnect nil)
  (setq eglot-connect-timeout 25)
  (setq eglot-sync-connect t)
  (setq eglot-put-doc-in-help-buffer t)
  (setq jsonrpc-request-timeout 20)
  :config
  (setf (cdr (assoc 'python-mode eglot-server-programs)) '("python3" "-m" "pyls"))
  (setf (cdr (assoc 'rust-mode eglot-server-programs)) '("~/.cargo/bin/rls"))
  (setq eglot-server-programs (mapcar #'use-lsp-remote eglot-server-programs))
  (defun eglot--uri-to-path (uri)
    (expand-file-name
     (replace-regexp-in-string
      "^/tmp/"
      (concat (projectile-project-root) level-up)
      (url-filename (url-generic-parse-url uri)))))
  (defun eglot--path-to-uri (path)
    (concat "file:///tmp/"
            (file-relative-name path (concat (projectile-project-root path) level-up))))
  :hook ((python-mode js-mode typescript-mode sgml-mode xml-mode rust-mode) . eglot-ensure))

(use-package yasnippet
  :delight yas-minor-mode
  :init
  (setq yas-triggers-in-field t)
  (setq yas-indent-line 'auto)
  :config
  (define-key yas-minor-mode-map [(tab)] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (yas-global-mode 1))

(defun my-projectile-project-find-function (dir)
  "Bridge between projectile and project.el.  Used in eglot."
  (let ((root (projectile-project-root dir)))
    (and root (cons 'transient root))))

(use-package projectile
  :delight
  :init
  (add-to-list 'project-find-functions 'my-projectile-project-find-function)
  (setq projectile-project-search-path
        (when (file-exists-p "~/Sources/") '("~/Sources/")))
  (projectile-add-known-project "~/dotfiles")
  (setq projectile-globally-ignored-directories
        '(".git" ".hg" ".svn" "build" "target"))
  (projectile-mode)
  :bind-keymap ("C-c p" . projectile-command-map))
(use-package helm-projectile
  :after (helm projectile)
  :init (helm-projectile-on))

(use-package rust-mode)

(use-package web-mode
  :mode ("\\.html\\'" . web-mode))

(use-package markdown-mode)

(use-package typescript-mode)

(use-package lua-mode)

(use-package yaml-mode)

(use-package smartparens
  :delight
  :init
  (require 'smartparens-config)
  (setq sp-highlight-pair-overlay nil)
  :hook ((prog-mode html-mode mhtml-mode smgl-mode) . smartparens-mode))

(use-package elisp-slime-nav
  :delight
  :hook ((emacs-lisp-mode ielm-mode) . elisp-slime-nav-mode))

(use-package dtrt-indent
  :delight
  :hook (prog-mode . dtrt-indent-mode))

(use-package olivetti
  :delight
  :init (setq-default olivetti-body-width 72)
  :hook (Info-mode . olivetti-mode))

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
  :delight
  :hook (prog-mode . eldoc-mode)
  :init (setq eldoc-idle-delay 0))

(use-package tetris
  :bind
  (:map tetris-mode-map
        ("C-p" . tetris-rotate-prev)
        ("C-b" . tetris-move-left)
        ("C-f" . tetris-move-right)
        ("C-n" . tetris-move-bottom)
        ("k" . tetris-rotate-prev)
        ("h" . tetris-move-left)
        ("l" . tetris-move-right)
        ("j" . tetris-move-bottom)
        ("r" . tetris-reset-game)
        ("SPC" . tetris-pause-game)))

(use-package display-line-numbers
  :delight
  :hook ((prog-mode sgml-mode) . display-line-numbers-mode))

(use-package vc
  :init (setq vc-handled-backends nil))

(use-package flymake
  :hook ((prog-mode sgml-mode xml-mode markdown-mode) . flymake-mode))

(use-package emacs
  :delight auto-revert-mode
  :mode (("\\.bash.*" . sh-mode)
         ("\\.gitignore" . prog-mode))
  :hook (text-mode . (lambda ()
                       (interactive)
                       (set-fill-column 72)
                       (auto-fill-mode)))
  :bind (("C-x C-b" . ibuffer)
         ("<backtab>" . ff-find-other-file))
  :init
  (setq confirm-kill-emacs #'yes-or-no-p)
  (setq confirm-kill-processes nil))

;;; packages.el ends here
