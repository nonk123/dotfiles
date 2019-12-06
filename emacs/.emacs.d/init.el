;;; init.el --- my init file.

;;; Commentary:

;;; Code:
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(setq inhibit-x-resources t)

(setq custom-file "~/.emacs.d/custom.el")
(load-file custom-file)

(use-package solarized-theme
  :init (load-theme 'solarized-dark t))

(use-package helm
  :init (require 'helm-config)
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)))

(use-package company)
(use-package company-c-headers
  :config (add-to-list 'company-backends 'company-c-headers))
(use-package helm-gtags
  :init (setq-default helm-gtags-auto-update t
                      helm-gtags-ignore-case t))
(use-package helm-company
  :bind
  (:map company-mode-map
        ("<M-tab>" . company-complete)
        ("<backtab>" . helm-company)
   :map company-active-map
        ("<backtab>" . helm-company)))

(use-package lsp-mode
  :commands lsp
  :init
  (setq lsp-auto-guess-root t)
  (setq lsp-enable-on-type-formatting nil))
(use-package ccls)
(use-package lsp-java)
(use-package company-lsp)
(use-package helm-lsp)

(use-package flycheck
  :init
  (setq lsp-prefer-flymake nil)
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  (global-flycheck-mode))
(use-package lsp-ui)

(use-package yasnippet
  :init
  (setq yas-triggers-in-field t)
  (setq yas-indent-line 'fixed)
  :config (yas-global-mode 1))

(use-package projectile
  :init (setq projectile-project-search-path '("~/Sources/" "~/"))
  :config (projectile-mode 1)
  :bind-keymap ("C-c p" . projectile-command-map))
(use-package helm-projectile
  :config (helm-projectile-on))

(use-package markdown-mode)

(use-package frames-only-mode
  :config (frames-only-mode 1)
  :bind ("s-o" . make-frame))

(use-package sokoban
  :bind
  (:map sokoban-mode-map
        ("C-p" . sokoban-move-up)
        ("C-b" . sokoban-move-left)
        ("C-f" . sokoban-move-right)
        ("C-n" . sokoban-move-down)
        ("n" . nil)))

(use-package string-inflection)

(defun after-make-frame-actions (frame)
  (interactive)
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face lines-tail))
  (whitespace-mode 1)
  (when window-system
    (let ((font "DejaVu Sans Mono-9"))
      (set-face-attribute 'default t :font font)
      (set-frame-font font 1 t))))

(add-to-list 'after-make-frame-functions 'after-make-frame-actions)

(add-to-list 'load-path "~/.emacs.d/lisp/")

(global-set-key (kbd "s-i") (lambda () (interactive)
  (load-file "~/.emacs.d/init.el")))

(global-set-key (kbd "s-SPC") 'rectangle-mark-mode)
(global-set-key (kbd "s-w") (lambda () (interactive)
  (kill-ring-save (line-beginning-position) (line-end-position))))

(global-set-key (kbd "M-p") 'scroll-down-line)
(global-set-key (kbd "M-n") 'scroll-up-line)

(defun tetris-actions ()
  (interactive)
  (define-key tetris-mode-map (kbd "C-p") 'tetris-rotate-prev)
  (define-key tetris-mode-map (kbd "C-b") 'tetris-move-left)
  (define-key tetris-mode-map (kbd "C-f") 'tetris-move-right)
  (define-key tetris-mode-map (kbd "C-n") 'tetris-rotate-next))

(add-hook 'tetris-mode-hook 'tetris-actions)

(defun term-actions ()
  (interactive)
  (setq-local bidi-paragraph-direction 'left-to-right))

(add-hook 'term-mode-hook 'term-actions)

(defun prog-actions ()
  (interactive)
  (display-line-numbers-mode 1)
  (company-mode 1)
  (helm-mode 1)
  (helm-gtags-mode 1)
  (lsp)
  (setq-local tab-width 4))

(add-hook 'prog-mode-hook 'prog-actions)

(defun emacs-lisp-actions ()
  (interactive)
  (setq-local tab-width 2))

(add-hook 'emacs-lisp-mode-hook 'emacs-lisp-actions)

(c-add-style "nonk123"
  '("java"
    (c-basic-offset . 4)
    (c-offsets-alist
     (access-label . /))))
(setq c-default-style "nonk123")

(defun text-actions ()
  (interactive)
  (set-fill-column 80)
  (auto-fill-mode 1))

(add-hook 'text-mode-hook 'text-actions)

(setq-default
  indent-tabs-mode nil
  tab-width 4
  tab-stop-list nil
  show-trailing-whitespace t
  epa-pinentry-mode 'loopback
  vc-follow-symlinks t)

(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)

(blink-cursor-mode 0)

(show-paren-mode 1)

(delete-selection-mode 1)
(electric-indent-mode 0)

(column-number-mode 1)

;;; init.el ends here
(put 'dired-find-alternate-file 'disabled nil)
