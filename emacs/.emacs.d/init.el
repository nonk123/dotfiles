;; -*- lexical-binding: t; -*-

(defvar bootstrap-version 6)

(let ((bootstrap-file
        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory)))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
      (url-retrieve-synchronously
        "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Annoying: collapsing the graphical window on a tiling WM freezes Emacs.
(unbind-key "C-z")

(defvar package-list nil)
(setq package-list
  '( ;; Theme.
     modus-themes

     ;; Cool libraries.
     dash
     ag

     ;; Customization.
     diminish

     ;; Mandatory fluff.
     vertico
     marginalia
     embark
     consult
     embark-consult
     cape
     savehist
     orderless

     ;; LSP support.
     flycheck
     lsp-mode
     lsp-ui
     company
     projectile
     yasnippet

     ;; Utilities.
     ace-window
     aggressive-indent-mode
     editorconfig
     wakatime-mode

     ;; It's Magit!
     magit

     ;; Various modes.
     markdown-mode
     cmake-mode))

(dolist (package package-list)
  (straight-use-package package))

(require 'dash)

(defun nonk/disable-clutter ()
  (interactive)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)

  (load-theme 'modus-vivendi t nil)
  (set-frame-font "LiterationMono Nerd Font:spacing=100:pixelsize=12" nil t))

(add-hook 'after-init-hook #'nonk/disable-clutter)

(setq eldoc-documentation-strategy #'eldoc-documentation-compose)
(global-eldoc-mode 1)

(line-number-mode 1)
(column-number-mode 1)

(electric-pair-mode 1)
(electric-indent-mode 1)

(setq completion-styles '(orderless partial-completion basic)
  completion-category-defaults nil)

(marginalia-mode 1)
(vertico-mode 1)
(savehist-mode 1)

(setq prefix-help-command #'embark-prefix-help-command)
(add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)

(bind-keys ("C-." . embark-act) ("M-." . embark-dwim))

(defvar consult-remap-alist nil)
(setq consult-remap-alist
  '((switch-to-buffer . consult-buffer)))

(pcase-dolist (`(,orig . ,new) consult-remap-alist)
  (bind-key (vector 'remap orig) new))

(bind-keys
  ("C-z" . consult-line)
	("C-c i" . consult-imenu-multi)
	("C-y" . consult-yank-from-kill-ring)
	("M-y" . yank))

(global-flycheck-mode 1)

(setq projectile-auto-discover t)
(setq projectile-file-exists-remote-cache-expire nil)
(setq projectile-indexing-method 'alien)
(setq projectile-project-search-path '(("~/Sources" . 1)))

(projectile-global-mode 1)

(bind-key "C-c p" projectile-command-map)

(global-company-mode 1)
(setq company-frontends nil)
(bind-key "M-TAB" #'completion-at-point)

;; Avoiding `company' at all cost and using it just for the backends.
(setq-default completion-at-point-functions
	(mapcar #'cape-company-to-capf
		(list #'company-files #'company-keywords #'company-dabbrev-code)))

(setq completion-in-region-function #'consult-completion-in-region)

(add-to-list 'auto-mode-alist '("\\.tpp\\'" . c++-mode))

(defvar nonk/aggressive-indent-modes '(lisp-data-mode))
(defvar nonk/ignore-lsp-modes '(sh-mode lisp-data-mode))

(defun nonk/wrap-elisp-capf (orig)
  (cape-wrap-nonexclusive orig))
(advice-add #'elisp-completion-at-point :around #'nonk/wrap-elisp-capf)

(defun nonk/start-coding ()
  (interactive)
  (when (-any-p #'derived-mode-p nonk/aggressive-indent-modes)
    (aggressive-indent-mode))
  (editorconfig-apply)
  (unless (-any-p #'derived-mode-p nonk/ignore-lsp-modes)
    (lsp nil)
    (lsp-ui-mode 1)))

(setq lsp-headerline-breadcrumb-enable nil
  lsp-ui-peek-enable nil
  lsp-ui-sideline-enable nil)

(require 'lsp-mode)
(bind-key "C-c l" lsp-command-map)

(dolist (mode '(prog-mode markdown-mode cmake-mode))
  (add-hook (intern (concat (symbol-name mode) "-hook"))
	  #'nonk/start-coding))

(bind-keys ("M-o" . ace-window)
	([remap other-window] . ace-window))

(setq vc-follow-symlinks t)

(defun nonk/magit-here ()
  (interactive)
  (if-let ((root (projectile-project-root)))
    (magit-status root)
    (user-error "You're not inside a project yet")))

(bind-keys ("C-c g" . nonk/magit-here))

(global-wakatime-mode 1)
(editorconfig-mode 1)

(diminish 'eldoc-mode)
(diminish 'auto-revert-mode)
(diminish 'projectile-mode)
(diminish 'abbrev-mode)
(diminish 'company-mode)
(diminish 'wakatime-mode)
(diminish 'editorconfig-mode)
