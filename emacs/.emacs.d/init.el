;;; init.el --- nonk123's ULTRA init.el

;;; Commentary:

;;; Code:

;;;; Constants:

(defconst nonk/default-font "Iosevka Term SS03 Extended 9")

(defconst nonk/user-emacs-dir (expand-file-name user-emacs-directory))
(defconst nonk/emacs-custom-file (file-name-concat nonk/user-emacs-dir "custom.el"))
(defconst nonk/site-packages-dir (file-name-concat nonk/user-emacs-dir "site-packages/"))
(defconst nonk/sources-dir (expand-file-name "~/Sources"))
(defconst nonk/dotfiles-dir (expand-file-name "dotfiles" nonk/sources-dir))

;;;; Early setup:

(setq custom-file nonk/emacs-custom-file
      auto-save-list-file-prefix (file-name-concat nonk/user-emacs-dir "auto-save-list" ".saves-"))

(setq inhibit-startup-message t)

;;;; straight.el setup:

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

;;;; use-package setup:

;;;;; Package installation:

(straight-use-package 'use-package)

(when (file-exists-p custom-file)
  (load custom-file))

(use-package use-package
  :demand t
  :after diminish)

(use-package diminish)

(use-package dash
  :functions -let -let* -if-let -if-let* -when-let -when-let*)

;;;;; :meow keyword for use-package:

(defun nonk/use-package--meow-normalizer (_name _keyword args)
  "Normalize ARGS into an alist of (STATE . KEYBINDS)."
  (use-package-as-one (symbol-name _keyword) args
    (lambda (label arg)
      (unless (listp arg)
        (use-package-error
         (concat label
		 " a (<state> . <keybinds>)"
                 " or list of these")))
      (if (use-package-non-nil-symbolp (car arg))
          (list arg)
        arg))))

(defun nonk/use-package--meow-handler (_name _keyword alist _rest _state)
  "Turn ALIST into a bunch of `meow-define-keys' statements."
  (let ((body (use-package-process-keywords _name _rest _state)))
    (use-package-concat
     (mapcar
      (pcase-lambda (`(,state . ,keybinds))
	(setq keybinds (mapcar (lambda (x) `(backquote ,x)) keybinds))
	`(with-eval-after-load 'meow
	   ,(pcase state
	      ('motion `(meow-motion-overwrite-define-key ,@keybinds))
	      ('leader `(meow-leader-define-key ,@keybinds))
	      (_ `(meow-define-keys ',state ,@keybinds)))))
      alist)
     body)))

(defalias 'use-package-normalize/:meow #'nonk/use-package--meow-normalizer)
(defalias 'use-package-handler/:meow #'nonk/use-package--meow-handler)

(unless (memq :meow use-package-keywords)
  (push :meow (cdr (memq :config use-package-keywords))))

;;;; Package configuration:

(use-package meow
  :demand t
  :defines meow-char-thing-table
  :functions
  meow-motion-overwrite-define-key
  meow-leader-define-key
  meow-normal-define-key
  meow-global-mode
  meow-thing-register
  meow-define-keys
  :custom (meow-keypad-self-insert-undefined nil)
  :meow ((motion ("j" . meow-next)
		 ("k" . meow-prev)
		 ("<escape>" . ignore))
	 (leader ("j" . "H-j")
		 ("k" . "H-k")
		 ("s" . save-buffer)
		 ("." . xref-find-definitions)
		 (">" . xref-find-apropos)
		 ("," . xref-go-back)
		 ("<" . xref-find-references)
		 ("1" . meow-digit-argument)
		 ("2" . meow-digit-argument)
		 ("3" . meow-digit-argument)
		 ("4" . meow-digit-argument)
		 ("5" . meow-digit-argument)
		 ("6" . meow-digit-argument)
		 ("7" . meow-digit-argument)
		 ("8" . meow-digit-argument)
		 ("9" . meow-digit-argument)
		 ("0" . meow-digit-argument)
		 ("/" . meow-keypad-describe-key)
		 ("?" . meow-cheatsheet))
	 (normal ("0" . meow-expand-0)
		 ("9" . meow-expand-9)
		 ("8" . meow-expand-8)
		 ("7" . meow-expand-7)
		 ("6" . meow-expand-6)
		 ("5" . meow-expand-5)
		 ("4" . meow-expand-4)
		 ("3" . meow-expand-3)
		 ("2" . meow-expand-2)
		 ("1" . meow-expand-1)
		 ("-" . negative-argument)
		 (";" . meow-reverse)
		 ("," . meow-inner-of-thing)
		 ("." . meow-bounds-of-thing)
		 ("[" . meow-beginning-of-thing)
		 ("]" . meow-end-of-thing)
		 ("a" . meow-append)
		 ("A" . meow-open-below)
		 ("b" . meow-back-word)
		 ("B" . meow-back-symbol)
		 ("c" . meow-change)
		 ("d" . meow-delete)
		 ("D" . meow-backward-delete)
		 ("e" . meow-next-word)
		 ("E" . meow-next-symbol)
		 ("f" . meow-find)
		 ("g" . meow-cancel-selection)
		 ("G" . meow-grab)
		 ("h" . meow-left)
		 ("H" . meow-left-expand)
		 ("i" . meow-insert)
		 ("I" . meow-open-above)
		 ("j" . meow-next)
		 ("J" . meow-next-expand)
		 ("k" . meow-prev)
		 ("K" . meow-prev-expand)
		 ("l" . meow-right)
		 ("L" . meow-right-expand)
		 ("m" . meow-join)
		 ("n" . meow-search)
		 ("o" . meow-block)
		 ("O" . meow-to-block)
		 ("p" . meow-yank)
		 ("q" . meow-quit)
		 ("Q" . meow-goto-line)
		 ("r" . meow-replace)
		 ("R" . meow-swap-grab)
		 ("s" . meow-kill)
		 ("t" . meow-till)
		 ("u" . meow-undo)
		 ("U" . meow-undo-in-selection)
		 ("v" . meow-visit)
		 ("w" . meow-mark-word)
		 ("W" . meow-mark-symbol)
		 ("x" . meow-line)
		 ("X" . meow-goto-line)
		 ("y" . meow-save)
		 ("Y" . meow-sync-grab)
		 ("z" . meow-pop-selection)
		 ("'" . repeat)
		 (":" . "M-x")
		 ("<escape>" . ignore))))

(use-package emacs
  :demand t
  :diminish
  outline-minor-mode
  outline-mode
  subword-mode
  abbrev-mode
  auto-revert-mode
  visual-line-mode
  :custom
  (show-paren-delay 0)
  (tab-always-indent t)
  (switch-to-buffer-obey-display-actions t)
  :hook
  (prog-mode . display-line-numbers-mode)
  (emacs-lisp-mode . outline-minor-mode)
  :bind ("M-TAB" . completion-at-point)
  :preface
  (add-to-list 'display-buffer-alist
	       '("\\*Help\\*"
		 display-buffer-in-direction
		 (direction . rightmost)
		 (window-width . 80)
		 (dedicated . t)
		 (preserve-size . (t . nil))))
  (add-to-list 'display-buffer-alist
	       `(,(rx (| "*Warnings*" "*Backtrace*"))
		 display-buffer-same-window))
  :init
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (blink-cursor-mode -1)
  (global-hl-line-mode 1)

  (delete-selection-mode 1)
  (show-paren-mode 1)
  (global-visual-line-mode 1)

  (column-number-mode 1)
  (electric-pair-mode 1)
  (electric-indent-mode 1)
  (global-subword-mode 1))

(use-package eldoc
  :demand t
  :diminish
  :custom
  (eldoc-documentation-strategy #'eldoc-documentation-compose)
  (eldoc-idle-delay 0.18)
  :preface
  (defun nonk/display-eldoc-buffer (docs interactive)
    (when-let ((buffer (and (buffer-live-p eldoc--doc-buffer)
			    (eldoc-doc-buffer))))
      (if docs (display-buffer buffer)
	(when-let ((window (get-buffer-window buffer)))
	  (delete-window window)))))

  (add-to-list 'display-buffer-alist
	       `(,(rx (| "*eldoc*"
			 (: "*eldoc for " (+ anything) "*")))
		 display-buffer-in-side-window
		 (side . right)
		 (slot . -100)
		 (window-min-width . 60)
		 (dedicated . t)
		 (window-parameters . ((no-delete-other-windows . t)))))

  (defvar nonk/elisp-eldoc-string-len-until-wrap 72
    "Wrap value strings longer than this.")

  (defun nonk/elisp-eldoc-display-variable-value (callback &rest _ignored)
    (when-let* ((sym (elisp--current-symbol))
		(value (and (boundp sym)
			    (symbol-value sym)))
		(type (type-of value))
		(value (format "%S" value))
		(type (format "%S" type))
		(doc (concat "Type: "
			     type
			     "\nValue"
			     (if (> (length value) nonk/elisp-eldoc-string-len-until-wrap)
				 " (wrapped):\n\n"
			       ": ")
			     value)))
      (funcall callback doc)))

  (defun nonk/install-elisp-documentation-providers ()
    (add-hook 'eldoc-documentation-functions #'nonk/elisp-eldoc-display-variable-value 10 t))
  :hook (emacs-lisp-mode . nonk/install-elisp-documentation-providers)
  :init
  (setq eldoc-display-functions
	'(eldoc-display-in-buffer
	  nonk/display-eldoc-buffer))
  (global-eldoc-mode 1))

(use-package evil-nerd-commenter
  :meow (leader (";" . evilnc-comment-or-uncomment-lines)
		(":" . evilnc-comment-box)))

(use-package all-the-icons)

(use-package modus-themes
  :config
  (load-theme 'modus-vivendi t)
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
  :init (global-treesit-auto-mode 1))

(use-package projectile
  :demand t
  :diminish
  :defines projectile-command-map
  :functions projectile-mode
  :custom (projectile-project-search-path (list (cons nonk/sources-dir 1)))
  :meow (leader ("p" . ,projectile-command-map))
  :config (projectile-mode 1))

(use-package ag
  :after projectile)

(use-package rg
  :after projectile)

(use-package lsp-mode
  :demand t
  :defines lsp-mode lsp-command-map
  :functions lsp-format-buffer
  :custom
  (lsp-response-timeout nil)
  (lsp-keymap-prefix nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-enable-suggest-server-download nil)
  (lsp-completion-provider :none)
  (lsp-diagnostics-provider :flycheck)
  :preface
  (defun nonk/diminish-lsp-lens-mode ()
    "Diminish `lsp-lens-mode' forcefully because `:diminish' doesn't do it."
    (diminish 'lsp-lens-mode))

  ;; Diminish `flycheck-mode' in `lsp-mode' buffers because it doesn't provide any info.
  (diminish 'flycheck-mode
	    '((:eval (if lsp-mode ""
		      (flycheck-mode-line-status-text)))))
  :hook
  ((c-mode rust-ts-mode python-ts-mode go-ts-mode c-ts-mode c++-ts-mode csharp-ts-mode js-ts-mode typescript-ts-mode) . lsp)
  (lsp-lens-mode . nonk/diminish-lsp-lens-mode)
  :meow (leader ("l" . ,lsp-command-map)))

(use-package lsp-ui
  :demand t
  :after lsp-mode
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-peek-enable nil)
  (lsp-ui-sideline-enable nil)
  :hook (lsp-mode . lsp-ui-mode))

(use-package flycheck
  :demand t
  :defines
  flycheck-mode
  flycheck-current-errors
  flycheck-highlighting-mode
  :functions
  flycheck-list-errors
  global-flycheck-mode
  flycheck-next-error-pos
  flycheck-error-region-for-mode
  flycheck-highlighting-mode
  flycheck-error-level
  flycheck-error-message
  :custom
  (flycheck-display-errors-function nil)
  (flycheck-help-echo-function nil)
  (flycheck-mode-line-prefix "F")
  :hook
  (flycheck-mode . nonk/install-eldoc-flycheck-hook)
  ;; Other modes are handled by lsp-mode.
  (emacs-lisp-mode . flycheck-mode)
  :preface
  (defun nonk/format-flycheck-error (err)
    (format "%s: %s"
	    (pcase-let* ((level (flycheck-error-level err))
			 (`(,char . ,face)
			  (pcase level
			    ('info '("I" . flycheck-error-list-info))
			    ('warning '("W" . flycheck-error-list-warning))
			    ('error '("E" . flycheck-error-list-error))
			    (_ '("?" . nil)))))
	      (propertize char 'face face))
	    (flycheck-error-message err)))
  
  (defun nonk/display-flycheck-error-with-eldoc (callback &rest _ignored)
    (when-let ((flycheck-error (and flycheck-mode
				    (get-char-property (point) 'flycheck-error))))
      (funcall callback (nonk/format-flycheck-error flycheck-error))))

  (defun nonk/install-eldoc-flycheck-hook ()
    (add-hook 'eldoc-documentation-functions #'nonk/display-flycheck-error-with-eldoc -100 t))
  
  (defun nonk/meow-error-thing ()
    (when-let* ((err (and flycheck-mode (get-char-property (point) 'flycheck-error)))
		(region (flycheck-error-region-for-mode err flycheck-highlighting-mode)))
      region))
  :init
  (add-to-list 'meow-char-thing-table '(?e . error))
  (meow-thing-register 'error #'nonk/meow-error-thing #'nonk/meow-error-thing)
  :meow (leader ("f" . flycheck-list-errors)))

(use-package yasnippet
  :demand t
  :diminish yas-minor-mode
  :functions yas-global-mode
  :config (yas-global-mode 1))

(use-package cape
  :demand t
  :functions cape-company-to-capf)

(use-package company
  :demand t
  :after cape
  :autoload company-files company-ispell company-dabbrev
  :custom (company-backends nil)
  :init (setq-default
	 completion-at-point-functions
	 (mapcar #'cape-company-to-capf
		 (list #'company-files #'company-ispell #'company-dabbrev))))

(use-package dap-mode)

(use-package treemacs
  :demand t
  :functions treemacs-load-theme treemacs-project-follow-mode
  :config (treemacs-project-follow-mode 1)
  :meow (leader ("v" . treemacs-select-window)))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package lsp-treemacs
  :after (treemacs lsp-mode)
  :functions lsp-treemacs-sync-mode
  :config (lsp-treemacs-sync-mode 1))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-all-the-icons
  :functions treemacs-load-theme
  :after (treemacs all-the-icons)
  :config (treemacs-load-theme "all-the-icons"))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package magit
  :demand t
  :meow (leader ("G" . magit-status)))

(use-package forge
  :after magit)

(use-package ace-window
  :demand t
  :custom
  (aw-background nil)
  :meow (leader ("o" . ace-window)
		("d" . delete-window)
		("%" . split-window-right)
		("\"" . split-window-below)))

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
  (meow-goto-line-function #'consult-goto-line)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (completion-in-region-function #'consult-completion-in-region)
  :init (recentf-mode 1)
  :meow ((normal ("/" . consult-line)
		 ("?" . consult-line-multi)
		 ("P" . consult-yank-from-kill-ring))
	 (leader ("b" . consult-buffer)
		 ("i" . consult-imenu-multi)
		 ("I" . consult-imenu))))

(use-package marginalia
  :demand t
  :after vertico
  :functions marginalia-mode
  :config (marginalia-mode 1))

(use-package embark
  :demand t
  :after embark-consult vertico marginalia
  :functions embark-eldoc-first-target
  :custom (prefix-help-command #'embark-prefix-help-command)
  :bind (("C-." . embark-dwim)
	 ("C-," . embark-act)
	 ("C-h B" . embark-bindings))
  :init
  (add-to-list 'display-buffer-alist
	       `(,(rx (: "*Embark Collect ("
			 (| "Live" "Completions")
			 ")*"))
                 nil (window-parameters (mode-line-format . none))))
  :meow (normal ("<" . embark-act)
		(">" . embark-dwim)))

(use-package embark-consult
  :demand t
  :after consult)

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
  :init (setq winner-dont-bind-my-keys t)
  :meow (leader ("w" . winner-undo)
		("W" . winner-redo))
  :config (winner-mode 1))

(use-package rust-mode)

(use-package lua-mode)

(use-package gdscript-mode)

(use-package alda-mode)

(use-package sxhkdrc-mode)

(use-package glsl-mode)

;;;; Personal configuration:

;;;;; Mode-agnostic buffer formatting:

(defun nonk/format-buffer (&optional interactive)
  "Format the active buffer with whatever method is available.

If INTERACTIVE is non-nil and no formatters are available, the command
will signal a `user-error'."
  (interactive (list t))
  (cond
   (buffer-read-only (user-error "Cannot format a read-only buffer"))
   (lsp-mode (lsp-format-buffer))
   (interactive (user-error "No formatter detected for current buffer"))))

(defun nonk/install-format-buffer-before-save-hook ()
  "Install `format-buffer' into the buffer-local `before-save-hook'."
  (add-hook 'before-save-hook #'nonk/format-buffer nil t))

(add-hook 'lsp-mode-hook #'nonk/install-format-buffer-before-save-hook)

(meow-leader-define-key '("F" . nonk/format-buffer))
(meow-global-mode 1)

;;;;; Default file:

(defvar nonk/default-file (expand-file-name "emacs/.emacs.d/init.el" nonk/dotfiles-dir)
  "File to open on Emacs startup.")

(defun nonk/open-default-file ()
  "Open `nonk/default-file' on Emacs startup."
  (find-file nonk/default-file))

(add-hook 'emacs-startup-hook #'nonk/open-default-file)

;;; init.el ends here
