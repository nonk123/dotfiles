;;; hooks.el --- hooks for my init.el.

;;; Commentary:

;;; Code:

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

(defun xml-actions ()
  (interactive)
  (prog-actions)
  (aggressive-indent-mode 1))

(add-hook 'xml-mode-hook 'xml-actions)
(add-hook 'html-mode-hook 'xml-actions)

(defun lisp-actions ()
  "Actions to perform upon entering `lisp-mode'."
  (interactive)
  (setq-local tab-width 2)
  (aggressive-indent-mode 1))
(add-hook 'lisp-mode-hook 'lisp-actions)

(defun text-actions ()
  "Actions to perform upon entering `text-mode'."
  (interactive)
  (set-fill-column 80)
  (auto-fill-mode 1))
(add-hook 'text-mode-hook 'text-actions)

;;; hooks.el ends here
