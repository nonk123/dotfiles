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
  (define-global-minor-mode company-global-mode
    company-mode company-mode)
  (defun company-flyspell (command &optional value &rest args)
    (pcase command
      ('prefix (when-let ((word (car (ispell-get-word nil)))) word))
      ('candidates
       (ispell-send-string "%\n")
       (ispell-send-string (concat "^" value "\n"))
       (while (progn
                (ispell-accept-output)
                (not (string= "" (car ispell-filter)))))
       (setq ispell-filter (cdr ispell-filter))
       (when (and ispell-filter (listp ispell-filter))
         (let ((result (ispell-parse-output (car ispell-filter))))
           (if (listp result)
               (append (caddr result) (caddr result))
             '()))))))
  (dolist (disabled '(company-eclim company-clang company-xcode company-dabbrev))
    (setq company-backends (delete disabled company-backends)))
  (add-to-list 'company-backends 'company-flyspell t)
  (company-global-mode))
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

(defconst level-up (file-name-as-directory ".."))

(defvar eglot-custom-server-programs
  '((python-mode "python3" "-m" "pyls")
    (rust-mode "~/.cargo/bin/rls"))
  "Drop-in replacements for eglot's default server-program commands.")

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
  ;; Replace eglot's unreasonable defaults.
  (pcase-dolist (`(,mode . ,command) eglot-custom-server-programs)
    (if-let ((entry (assoc mode eglot-server-programs)))
        (setf (cdr entry) command)
      (push (append (list mode) command) eglot-server-programs)))
  ;; Inject `lsp-remote' into all server commands.
  (dolist (cell eglot-server-programs)
    (when (listp (cdr cell))
      (unless (string-suffix-p "lsp-remote" (cadr cell))
        (push "~/.local/bin/lsp-remote" (cdr cell)))))
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
  :hook ((Info-mode text-mode org-mode markdown-mode) . olivetti-mode))

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

(use-package flyspell
  :delight
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

(use-package org
  :init
  (setq org-table-auto-blank-field nil)
  (setq org-entities-user
        '(("Emacr" "\\bar{E}" nil "&Emacr;" "E" "Ē" "Ē")
          ("emacr" "\\bar{e}" nil "&emacr;" "e" "ē" "ē")
          ("Ebreve" "\\u{E}" nil "&#276;" "E" "Ĕ" "Ĕ")
          ("ebreve" "\\u{e}" nil "&#277;" "e" "ĕ" "ĕ")
          ("Omacr" "\\bar{O}" nil "&Omacr;" "O" "Ō" "Ō")
          ("omacr" "\\bar{o}" nil "&omacr;" "o" "ō" "ō")
          ("Lacute" "\\'{L}" nil "&Lacute;" "L" "Ĺ" "Ĺ")
          ("lacute" "\\'{l}" nil "&lacute;" "l" "ĺ" "ĺ")
          ("Kacute" "\\'{K}" nil "&#7728;" "K" "Ḱ" "Ḱ")
          ("kacute" "\\'{k}" nil "&#7729;" "k" "ḱ" "ḱ")
          ("Kw" "K^{w}" nil "K&#695;" "Kw" "Kʷ" "Kʷ")
          ("kw" "k^{w}" nil "k&#695;" "kw" "kʷ" "kʷ")
          ("Hi" "H_{1}" nil "H&#8321;" "H1" "H₁" "H₁")
          ("hi" "h_{1}" nil "h&#8321;" "h1" "h₁" "h₁")
          ("hii" "h_{2}" nil "h&#8322;" "h2" "h₂" "h₂")
          ("hiii" "h_{3}" nil "h&#8323;" "h3" "h₃" "h₃")
          ("cbr" "_{o}" nil "&#805;" "." "̥" "̥")))
  :config
  (setq org-confirm-babel-evaluate (lambda (lang body)))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ditaa . t)))
  (setq org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar")
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((plantuml . t)))
  (setq org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar")
  (dolist (key (list [(tab)] (kbd "TAB") (kbd "<tab>")))
    (define-key org-mode-map key nil)))

(use-package org-preview-html)

(use-package emacs
  :delight (auto-revert-mode) (auto-fill-function)
  :mode (("\\.bash.*" . sh-mode)
         ("\\.gitignore" . prog-mode))
  :hook (text-mode . auto-fill-mode)
  :bind (("C-x C-b" . ibuffer)
         ("<backtab>" . ff-find-other-file))
  :init
  (setq-default fill-column 72)
  (setq confirm-kill-emacs #'yes-or-no-p)
  (setq confirm-kill-processes nil))

;;; packages.el ends here
