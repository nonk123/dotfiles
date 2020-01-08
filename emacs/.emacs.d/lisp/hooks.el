;;; hooks.el --- hooks for my init.el.

;;; Commentary:

;;; Code:
(defun after-make-frame-actions (frame)
  (interactive)
  (use-package xresources-theme)
  ;; May be done by xresources-theme, not quite sure.
  (set-frame-font (x-get-resource "font" "emacs") nil t))

(add-to-list 'after-make-frame-functions 'after-make-frame-actions)

(defun tetris-actions ()
  (interactive)
  (define-key tetris-mode-map (kbd "C-p") 'tetris-rotate-prev)
  (define-key tetris-mode-map (kbd "C-b") 'tetris-move-left)
  (define-key tetris-mode-map (kbd "C-f") 'tetris-move-right)
  (define-key tetris-mode-map (kbd "C-n") 'tetris-rotate-next))
(add-hook 'tetris-mode-hook 'tetris-actions)

(defun prog-actions ()
  (interactive)
  (display-line-numbers-mode 1)
  (company-mode 1)
  (helm-mode 1)
  (helm-gtags-mode 1)
  (lsp)
  (setq-local tab-width 4))
(add-hook 'prog-mode-hook 'prog-actions)

(defun lisp-actions ()
  (interactive)
  (setq-local tab-width 2))
(add-hook 'lisp-mode-hook 'lisp-actions)

(defun text-actions ()
  (interactive)
  (set-fill-column 80)
  (auto-fill-mode 1))
(add-hook 'text-mode-hook 'text-actions)
;;; hooks.el ends here
