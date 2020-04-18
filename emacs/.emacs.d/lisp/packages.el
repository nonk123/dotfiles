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
  :commands (eglot eglot-ensure)
  :init
  (defun eglot--uri-to-path (uri)
    (expand-file-name
     (replace-regexp-in-string
      "^/tmp/"
      (concat (projectile-project-root) level-up)
      (url-filename (url-generic-parse-url uri)))))
  (defun eglot--path-to-uri (path)
    (concat "file:///tmp/"
            (file-relative-name path (concat (projectile-project-root path) level-up))))
  (setq eglot-connect-timeout 10)
  (setq eglot-sync-connect t)
  (setq eglot-put-doc-in-help-buffer t)
  :config
  (setf (cdr (assoc 'python-mode eglot-server-programs)) '("python3" "-m" "pyls"))
  (setq eglot-server-programs (mapcar #'use-lsp-remote eglot-server-programs))
  :hook ((python-mode javascript-mode js-mode mhtml-mode sgml-mode xml-mode) . eglot-ensure)
  :bind
  (:map eglot-mode-map
        ("C-c r" . eglot-rename)
        ("C-c f" . eglot-code-actions)))

(use-package yasnippet
  :delight yas-minor-mode
  :init
  (setq yas-triggers-in-field t)
  (setq yas-indent-line 'auto)
  :config (yas-global-mode 1))

(defun my-projectile-project-find-function (dir)
  "Bridge between projectile and project.el.  Used in eglot."
  (let ((root (projectile-project-root dir)))
    (and root (cons 'transient root))))

(use-package projectile
  :init
  (add-to-list 'project-find-functions 'my-projectile-project-find-function)
  (setq projectile-project-search-path '("~/Sources"))
  (projectile-add-known-project "~/dotfiles")
  (setq projectile-globally-ignored-directories
        '(".git" ".hg" ".svn" "build" "target"))
  (projectile-mode)
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
  :delight
  :hook (((prog-mode html-mode mhtml-mode sgml-mode) . aggressive-indent-mode)
         ((python-mode sh-mode) . (lambda () (interactive) (aggressive-indent-mode 0)))))

(use-package highlight-indent-guides
  :delight
  :init (setq highlight-indent-guides-method 'bitmap)
  :hook ((prog-mode html-mode mhtml-mode smgl-mode) . highlight-indent-guides-mode))

(use-package smartparens
  :delight
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
