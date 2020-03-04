;;; keys.el --- my keybindings.

;;; Commentary:

;;; Code:

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

(bind global-map
      ("C-c d" . show-documentation-at-point)
      ("s-i"   . load-init)
      ("s-o"   . make-frame)
      ("s-SPC" . rectangle-mark-mode)
      ("C-c ;" . comment-or-uncomment-region)
      ("M-p"   . scroll-down-line)
      ("M-n"   . scroll-up-line))

;;; keys.el ends here
