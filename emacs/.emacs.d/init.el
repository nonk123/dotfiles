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
     format-all
     editorconfig
     wakatime-mode

     ;; It's Magit!
     magit

     ;; Various modes.
     rust-mode
     zig-mode
     jinja2-mode
     markdown-mode
     cmake-mode
     dockerfile-mode
     yaml-mode))

(dolist (package package-list)
  (straight-use-package package))

(straight-use-package
  '(cmake-utils :type git :host github :repo "nonk123/emacs-cmake-utils"))

(require 'cmake-utils)
(setq cmake-utils-jobs-count 24)

(require 'dash)

(setq wdired-allow-to-change-permissions t)

(defun nonk/disable-clutter ()
  (interactive)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1))

(defvar nonk/theme-set-p nil)

(defun nonk/apply-theming (&optional force)
  (interactive "p")
  (when (or force (not nonk/theme-set-p))
    (set-face-font 'default "LiterationMono Nerd Font:spacing=100:pixelsize=12")
    (load-theme 'modus-vivendi t nil)
    (setq nonk/theme-set-p t)))

(add-hook 'after-init-hook #'nonk/disable-clutter)

;; Ensure the correct theme is set even in server mode.
(add-hook 'server-after-make-frame-hook #'nonk/apply-theming)
(add-hook 'after-make-frame-hook #'nonk/apply-theming)

(setq eldoc-documentation-strategy #'eldoc-documentation-compose)
(global-eldoc-mode 1)

(line-number-mode 1)
(column-number-mode 1)

(electric-pair-mode 1)
(electric-indent-mode 1)

(setq completion-styles '(orderless partial-completion basic))
(setq completion-category-defaults nil)

(marginalia-mode 1)
(vertico-mode 1)
(savehist-mode 1)

(setq prefix-help-command #'embark-prefix-help-command)
(add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)

(bind-keys ("C-." . embark-act) ("M-." . xref-find-definitions))

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

(setq ispell-alternate-dictionary "/usr/share/dict/words")

(require 'yasnippet)
(setq yas-snippet-dirs (list (concat user-emacs-directory "snippets")))
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(yas-global-mode 1)

(add-to-list 'auto-mode-alist '("LICENSE\\'" . text-mode))

(global-flycheck-mode 1)

(setq projectile-auto-discover t)
(setq projectile-file-exists-remote-cache-expire nil)
(setq projectile-indexing-method 'alien)
(setq projectile-project-search-path '(("~/Sources" . 1)))

(projectile-global-mode 1)

(bind-key "C-c p" projectile-command-map)

(global-company-mode 1)
(setq company-backends nil)
(setq company-frontends nil)

;; Avoiding `company' at all cost and using it just for the backends.
(setq-default completion-at-point-functions
	(append
    (mapcar (lambda (x) (cape-capf-nonexclusive (cape-company-to-capf x)))
		  (list #'company-files #'company-keywords))
    (list (cape-company-to-capf #'company-dabbrev))))

(setq yas-fallback-behavior 'return-nil)

(defun nonk/try-yas-then-cap ()
  (interactive)
  (unless (yas-expand)
    (completion-at-point)))

(global-set-key (kbd "<backtab>") #'nonk/try-yas-then-cap)
(define-key company-mode-map (kbd "<backtab>") #'nonk/try-yas-then-cap)

(setq completion-in-region-function #'consult-completion-in-region)

(add-to-list 'auto-mode-alist '("\\.tpp\\'" . c++-mode))

(require 'aggressive-indent)
(defvar nonk/aggressive-indent-modes '(lisp-data-mode))
(defvar nonk/ignore-lsp-modes '(sh-mode ld-script-mode lisp-data-mode))

(defun nonk/wrap-elisp-capf (orig)
  (cape-wrap-nonexclusive orig))
(advice-add #'elisp-completion-at-point :around #'nonk/wrap-elisp-capf)

(defun nonk/format-buffer (&optional arg)
  (interactive "p")
  (let ((fallback t))
    (when lsp-mode
      (when (or (lsp-feature? "textDocument/formatting")
              (lsp-feature? "textDocument/rangeFormatting"))
        (lsp-format-buffer)
        (setq fallback nil)))
    (when fallback
      ;; Ignore the error to prevent quirks while saving the buffer.
      (condition-case nil
        (format-all-buffer)
        (error nil)))))

(defvar nonk/mode-extras nil)

(require 'cmake-mode)
(setq nonk/mode-extras '((cmake-mode after-save-hook cmake-utils-configure)))

(defun nonk/start-coding ()
  (interactive)
  (diminish 'auto-revert-mode) ; something re-enables it unless run inside a hook???
  (when (-any-p #'derived-mode-p nonk/aggressive-indent-modes)
    (aggressive-indent-mode 1))
  (editorconfig-apply)
  (add-hook 'before-save-hook #'nonk/format-buffer 99 t)
  (let ((ptr nonk/mode-extras) (stop nil))
    (while (and ptr (not stop))
      (pcase-let ((`(,mode ,hook ,fn) (car ptr)))
        (when (derived-mode-p mode)
          (add-hook hook fn 99 t)
          (setq stop t)))
      (setq ptr (cdr ptr))))
  (unless (-any-p #'derived-mode-p nonk/ignore-lsp-modes)
    (lsp nil)
    (lsp-ui-mode 1)))

(defun nonk/disable-auto-format ()
  (interactive)
  (remove-hook 'before-save-hook #'nonk/format-buffer t))

(require 'lsp-mode)
(require 'lsp-ui)

(setq lsp-headerline-breadcrumb-enable nil
  lsp-ui-peek-enable nil
  lsp-ui-sideline-enable nil)

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

(defun nonk/diminish-things ()
  (diminish 'eldoc-mode)
  (diminish 'yas-minor-mode)
  (diminish 'projectile-mode)
  (diminish 'abbrev-mode)
  (diminish 'company-mode)
  (diminish 'aggressive-indent-mode)
  (diminish 'wakatime-mode)
  (diminish 'editorconfig-mode))

(add-hook 'after-init-hook #'nonk/diminish-things)
