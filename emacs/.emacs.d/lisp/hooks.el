;;; hooks.el --- hooks for my init.el.

;;; Commentary:

;;; Code:

(defun make-frame-actions (frame)
  "Actions to perform on a newly created FRAME."
  (interactive)
  (if (display-graphic-p frame)
      (progn
        (set-frame-font (x-get-resource "font" "emacs") nil t)
        (unbind global-map "C-z"))
    (bind global-map ("C-z" . suspend-frame))))
(add-to-list 'after-make-frame-functions 'make-frame-actions)

(defun prog-actions ()
  "Actions to perform upon entering `prog-mode'."
  (interactive)
  (display-line-numbers-mode 1)
  (company-mode 1)
  (lsp)
  (when (bound-and-true-p lsp-mode)
    (lsp-lens-mode 1))
  (setq-local tab-width 4))
(add-hook 'prog-mode-hook 'prog-actions)

(defun lisp-actions ()
  "Actions to perform upon entering `lisp-mode'."
  (interactive)
  (setq-local tab-width 2))
(add-hook 'lisp-mode-hook 'lisp-actions)

(defun text-actions ()
  "Actions to perform upon entering `text-mode'."
  (interactive)
  (set-fill-column 80)
  (auto-fill-mode 1))
(add-hook 'text-mode-hook 'text-actions)

;;; hooks.el ends here
