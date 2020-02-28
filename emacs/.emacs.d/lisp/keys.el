;;; keys.el --- my keybindings.

;;; Commentary:

;;; Code:

(defun unbind (keymap &rest keys)
  "Unbind KEYS from a KEYMAP."
  (dolist (key keys)
    (define-key keymap (kbd key) nil)))

(defmacro bind (keymap &rest key-cons)
  "Bind keys from a list of KEY-CONS onto a KEYMAP."
  (dolist (key-con key-cons)
    (let ((key (kbd (car key-con))) (def (cdr key-con)))
      (when (stringp def)
        (setq def (kbd def)))
      (define-key (eval keymap) key def))))

(defun save-whole-line ()
  "Save whole line to the kill ring."
  (interactive)
  (kill-ring-save (line-beginning-position) (line-end-position)))

(defun show-documentation (docstring)
  "Show a DOCSTRING in a temporary buffer."
  (unless (string-empty-p docstring)
    (with-output-to-temp-buffer "*Documentation*"
      (print docstring))))

(defun my-lsp-show-documentation ()
  "`lsp-mode' implementation of `show-documentation-at-point'."
  (interactive)
  (show-documentation
   (-some->>
    (gethash "contents" (lsp-request
                         "textDocument/hover"
                         (lsp--text-document-position-params)))
    lsp-ui-doc--extract
    (replace-regexp-in-string "\r" ""))))

(defun show-emacs-lisp-documentation-at-point ()
  "`emacs-lisp-mode' implementation of `show-documentation-at-point'."
  (let ((fun (function-called-at-point))
        (var (variable-at-point t)))
    (cond
     ((and var (boundp var) (not (functionp var)) (/= var 0))
      (describe-variable var))
     (fun
      (describe-function fun))
     (t
      (message "nil")))))

(defun show-documentation-at-point ()
  "Show documentation for symbol at point in a temporary buffer."
  (interactive)
  (cond
   ((equal major-mode 'emacs-lisp-mode)
    (show-emacs-lisp-documentation-at-point))
   ((bound-and-true-p slime-mode)
    (slime-documentation (slime-symbol-at-point)))
   ((bound-and-true-p lsp-mode)
    (my-lsp-show-documentation))
   (t
    (message "No documentation handler found"))))

(use-package tetris
  :bind
  (:map tetris-mode-map
        ("C-p" . tetris-rotate-prev)
        ("C-b" . tetris-move-left)
        ("C-f" . tetris-move-right)
        ("C-n" . tetris-move-bottom)))

(use-package windmove
  :bind
  (("C-c h" . windmove-left)
   ("C-c j" . windmove-down)
   ("C-c k" . windmove-up)
   ("C-c l" . windmove-right)))

(bind global-map
      ("C-c d" . show-documentation-at-point)
      ("s-i"   . load-init)
      ("s-o"   . make-frame)
      ("s-SPC" . rectangle-mark-mode)
      ("s-w"   . save-whole-line)
      ("C-c ;" . comment-or-uncomment-region)
      ("M-p"   . scroll-down-line)
      ("M-n"   . scroll-up-line))

;;; keys.el ends here
