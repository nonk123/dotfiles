;;; keys.el --- my keybindings.

;;; Commentary:

;;; Code:

(defun show-lsp-documentation-at-point ()
  "`lsp-mode' implementation of `show-documentation-at-point'."
  (let ((doc
         (lsp-ui-doc--extract
          (gethash "contents"
                   (lsp-request "textDocument/hover"
                                (lsp--text-document-position-params))))))
    (if (string-empty-p doc)
        (message "No documentation found")
      (with-help-window (help-buffer)
        (princ doc)))))

(defun show-documentation-at-point ()
  "Show documentation for symbol at point in a temporary buffer."
  (interactive)
  (cond
   ((or (equal major-mode 'emacs-lisp-mode)
        (equal major-mode 'lisp-interaction-mode))
    (describe-symbol (symbol-at-point)))
   ((bound-and-true-p slime-mode)
    (slime-documentation (slime-symbol-at-point)))
   ((bound-and-true-p lsp-mode)
    (show-lsp-documentation-at-point))
   (t
    (message "No documentation handler found"))))

(use-package tetris
  :bind
  (:map tetris-mode-map
        ("C-p" . tetris-rotate-prev)
        ("C-b" . tetris-move-left)
        ("C-f" . tetris-move-right)
        ("C-n" . tetris-move-bottom)))

(defun my-kill-region (arg)
  "Kill ARG lines unless a region is selected."
  (interactive "P")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (kill-whole-line arg)))

(defun my-kill-ring-save ()
  "Save whole line to kill ring unless a region is selected."
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (kill-ring-save (line-beginning-position) (line-end-position))))

(bind global-map
      ("C-c C-k" . eval-buffer)
      ("C-c d" . show-documentation-at-point)
      ("C-c u" . list-packages)
      ("C-,"   . previous-buffer)
      ("C-."   . next-buffer)
      ("C-w"   . my-kill-region)
      ("M-w"   . my-kill-ring-save)
      ("s-i"   . load-init)
      ("s-SPC" . rectangle-mark-mode)
      ("C-c ;" . comment-or-uncomment-region))

;;; keys.el ends here
