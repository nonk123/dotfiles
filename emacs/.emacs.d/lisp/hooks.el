;;; hooks.el --- hooks for my init.el.

;;; Commentary:

;;; Code:
(defun after-make-frame-actions (frame)
  (interactive)
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face lines-tail))
  (whitespace-mode 1)
  (when window-system
    (let ((font "DejaVu Sans Mono-9"))
      (set-face-attribute 'default t :font font)
      (set-frame-font font 1 t))))
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

(defun emacs-lisp-actions ()
  (interactive)
  (setq-local tab-width 2))
(add-hook 'emacs-lisp-mode-hook 'emacs-lisp-actions)

(defun text-actions ()
  (interactive)
  (set-fill-column 80)
  (auto-fill-mode 1))
(add-hook 'text-mode-hook 'text-actions)

(provide 'hooks)
;;; hooks.el ends here
