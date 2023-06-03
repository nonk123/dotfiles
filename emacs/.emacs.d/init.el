;;; init.el --- nonk123's ULTRA init.el

;;; Commentary:

;;; Code:

(defconst nonk/default-font "Iosevka Term SS03 Extended 9")

(defconst nonk/user-emacs-dir (expand-file-name user-emacs-directory))
(defconst nonk/emacs-custom-file (file-name-concat nonk/user-emacs-dir "custom.el"))
(defconst nonk/site-packages-dir (file-name-concat nonk/user-emacs-dir "site-packages/"))
(defconst nonk/sources-dir (expand-file-name "~/Sources"))

(setq custom-file nonk/emacs-custom-file
      auto-save-list-file-prefix (file-name-concat nonk/user-emacs-dir "auto-save-list" ".saves-"))

(setq inhibit-startup-message t)

(defvar straight-fix-flycheck t)
(defvar straight-use-package-by-default t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" nonk/user-emacs-dir))
      (bootstrap-version 6))
  (declare-function straight-use-package bootstrap-file)
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(when (file-exists-p custom-file)
  (load custom-file))

(use-package use-package
  :demand t
  :after diminish)

(use-package diminish)

(use-package emacs
  :demand t
  :diminish subword-mode eldoc-mode abbrev-mode
  :custom
  (show-paren-delay 0)
  (tab-always-indent t)
  (eldoc-idle-delay 0)
  :hook (prog-mode . display-line-numbers-mode)
  :config
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (blink-cursor-mode -1)
  (global-hl-line-mode 1)

  (global-eldoc-mode 1)
  (delete-selection-mode 1)
  (show-paren-mode 1)

  (column-number-mode 1)

  (electric-pair-mode 1)
  (electric-indent-mode 1)
  (global-subword-mode 1))

(use-package meow
  :demand t
  :functions
  meow-motion-overwrite-define-key
  meow-leader-define-key
  meow-normal-define-key
  meow-global-mode
  :config
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   '("j" . "H-j")
   '("k" . "H-k")
   '("s" . save-buffer)
   '("e" . eval-last-sexp)
   '("E" . eval-buffer)
   '("." . xref-find-definitions)
   '(">" . xref-find-apropos)
   '("," . xref-go-back)
   '("<" . xref-find-references)
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '(":" . "M-x")
   '("<escape>" . ignore)))

(use-package evil-nerd-commenter
  :autoload evilnc-comment-or-uncomment-lines
  :init (meow-leader-define-key '(";" . evilnc-comment-or-uncomment-lines)))

(use-package all-the-icons
  :if (display-graphic-p)
  :demand t)

(use-package catppuccin-theme
  :if (display-graphic-p)
  :defines catppuccin-flavor
  :init (setq catppuccin-flavor 'macchiato)
  :config
  (load-theme 'catppuccin t)
  (set-frame-font nonk/default-font nil t))

(use-package ligature
  :if (display-graphic-p)
  :functions ligature-set-ligatures global-ligature-mode
  :config
  (ligature-set-ligatures
   t
   '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
     "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
     "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
     ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++"))
  (global-ligature-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package treesit-auto
  :functions global-treesit-auto-mode
  :custom (treesit-auto-install nil)
  :config (global-treesit-auto-mode 1))

(use-package projectile
  :demand t
  :diminish
  :defines projectile-command-map
  :functions projectile-mode
  :custom (projectile-project-search-path (list (cons nonk/sources-dir 1)))
  :config
  (meow-leader-define-key (cons "p" projectile-command-map))
  (projectile-mode 1))

(use-package lsp-mode
  :after (flycheck yasnippet)
  :autoload lsp
  :defines lsp-mode lsp-command-map
  :functions lsp-format-buffer
  :custom
  (lsp-keymap-prefix nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-enable-suggest-server-download nil)
  (lsp-completion-provider :capf)
  :hook ((rust-ts-mode python-ts-mode go-ts-mode c-ts-mode c++-ts-mode csharp-ts-mode js-ts-mode typescript-ts-mode) . lsp)
  :config (meow-leader-define-key (cons "l" lsp-command-map)))

(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode))

(use-package flycheck
  :functions global-flycheck-mode
  :init
  (meow-leader-define-key
   '("f" . flycheck-list-errors)
   '("n" . flycheck-next-error)
   '("N" . flycheck-previous-error))
  :config (global-flycheck-mode 1))

(use-package yasnippet
  :demand t
  :diminish yas-minor-mode
  :functions yas-global-mode
  :config (yas-global-mode 1))

(use-package cape
  :demand t
  :functions cape-dabbrev cape-file
  :config
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package dap-mode)

(use-package treemacs
  :functions treemacs-load-theme treemacs-project-follow-mode
  :autoload treemacs-select-window
  :init (meow-leader-define-key '("v" . treemacs-select-window))
  :config (treemacs-project-follow-mode 1))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package lsp-treemacs
  :after (treemacs lsp-mode)
  :functions lsp-treemacs-sync-mode
  :config (lsp-treemacs-sync-mode 1))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-all-the-icons
  :after (treemacs all-the-icons)
  :config (treemacs-load-theme "all-the-icons"))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package magit
  :autoload magit-status
  :init (meow-leader-define-key '("G" . magit-status)))

(use-package forge
  :after magit)

(use-package ace-window
  :custom
  (aw-keys '(?a ?s ?d ?f ?h ?j ?k ?l))
  (aw-background nil)
  :init
  (meow-leader-define-key
   '("o" . ace-window)
   '("d" . delete-window)
   '("%" . split-window-right)
   '("\"" . split-window-below)))

(use-package vertico
  :demand t
  :straight (:files (:defaults "extensions/*"))
  :custom
  (vertico-count 10)
  (vertico-cycle t)
  (vertico-resize nil)
  :functions vertico-mode
  :config (vertico-mode 1))

(use-package consult
  :demand t
  :functions consult-completion-in-region consult-xref
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :init
  (meow-normal-define-key '("/" . consult-line))
  (meow-leader-define-key
   '("b" . consult-buffer)
   '("i" . consult-imenu))
  :config
  (setq completion-in-region-function #'consult-completion-in-region)
  (recentf-mode 1))

(use-package marginalia
  :demand t
  :after vertico
  :functions marginalia-mode
  :config (marginalia-mode 1))

(use-package all-the-icons-completion
  :demand t
  :after all-the-icons
  :functions all-the-icons-completion-mode
  :config (all-the-icons-completion-mode 1)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup))

(use-package orderless
  :demand t
  :custom (completion-styles '(substring orderless partial-completion basic)))

(use-package hl-todo
  :demand t
  :functions global-hl-todo-mode
  :config (global-hl-todo-mode 1))

(use-package savehist
  :demand t
  :init (setq enable-recursive-minibuffers t)
  :config (savehist-mode 1))

(use-package winner
  :demand t
  :init
  (setq winner-dont-bind-my-keys t)
  (meow-leader-define-key
   '("w" . winner-undo)
   '("W" . winner-redo))
  :config (winner-mode 1))

(use-package rust-mode)

(use-package lua-mode)

(use-package gdscript-mode)

(use-package alda-mode)

(use-package sxhkdrc-mode)

(defun format-buffer ()
  "Format the active buffer with whatever is available."
  (interactive)
  (cond
   (buffer-read-only (user-error "Cannot format a read-only buffer"))
   (lsp-mode (lsp-format-buffer))
   ((called-interactively-p 'interactive) (user-error "No formatter detected for current buffer"))))

(defun install-format-buffer-hook ()
  "Install `format-buffer' into the buffer-local `before-save-hook'."
  (add-hook 'before-save-hook #'format-buffer nil t))

(add-hook 'lsp-mode-hook #'install-format-buffer-hook)

(meow-leader-define-key '("F" . format-buffer))
(meow-global-mode 1)

;;; init.el ends here
