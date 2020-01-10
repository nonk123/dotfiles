;;; keys.el --- my keybindings.

;;; Commentary:

;;; Code:
(global-unset-key (kbd "C-z"))

(global-set-key (kbd "s-i") 'load-init)

(global-set-key (kbd "s-o") 'make-frame)

(global-set-key (kbd "s-SPC") 'rectangle-mark-mode)

(defun save-whole-line ()
  "Save whole line to the kill ring."
  (interactive)
  (kill-ring-save (line-beginning-position) (line-end-position)))

(global-set-key (kbd "s-w") 'save-whole-line)

(global-set-key (kbd "M-p") 'scroll-down-line)
(global-set-key (kbd "M-n") 'scroll-up-line)
;;; keys.el ends here
