;;; keys.el --- my keybindings.

;;; Commentary:

;;; Code:
(global-set-key (kbd "s-i") (lambda () (interactive)
  (load-file "~/.emacs.d/init.el")))

(global-set-key (kbd "s-SPC") 'rectangle-mark-mode)

(global-set-key (kbd "s-w") (lambda () (interactive)
  (kill-ring-save (line-beginning-position) (line-end-position))))

(global-set-key (kbd "M-p") 'scroll-down-line)
(global-set-key (kbd "M-n") 'scroll-up-line)

(provide 'keys)
;;; keys.el ends here
