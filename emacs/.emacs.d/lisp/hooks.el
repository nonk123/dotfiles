;;; hooks.el --- hooks for my init.el.

;;; Commentary:

;;; Code:

(defun prog-actions ()
  (interactive)
  (display-line-numbers-mode 1)
  (company-mode 1)
  (aggressive-indent-mode 1)
  (lsp)
  (when (bound-and-true-p lsp-mode)
    (lsp-lens-mode 1))
  (setq-local tab-width 4))

(add-hook 'prog-mode-hook 'prog-actions)
(add-hook 'xml-mode-hook 'prog-actions)
(add-hook 'html-mode-hook 'prog-actions)

(defun no-aggressive-indent ()
  (interactive)
  (aggressive-indent-mode 0))
(add-hook 'python-mode-hook 'no-aggressive-indent)
(add-hook 'sh-mode-hook 'no-aggressive-indent)

(defun lisp-actions ()
  (interactive)
  (setq-local tab-width 2)
  (aggressive-indent-mode 1))
(add-hook 'lisp-mode-hook 'lisp-actions)

(defun text-actions ()
  (interactive)
  (set-fill-column 80)
  (auto-fill-mode 1))
(add-hook 'text-mode-hook 'text-actions)

;;; hooks.el ends here
