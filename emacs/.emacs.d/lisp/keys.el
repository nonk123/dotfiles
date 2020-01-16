;;; keys.el --- my keybindings.

;;; Commentary:

;;; Code:
(defmacro unbind (&rest keys)
  "Unbind KEYS."
  (dolist (key keys)
    (global-unset-key (kbd key))))

(defmacro bind (&rest key-cons)
  "Bind keys using a list of KEY-CONS."
  (dolist (key-con key-cons)
    (global-set-key (kbd (car key-con)) (cdr key-con))))

(defun save-whole-line ()
  "Save whole line to the kill ring."
  (interactive)
  (kill-ring-save (line-beginning-position) (line-end-position)))

(unbind "C-z")

(bind ("s-i"   . load-init)
      ("s-o"   . make-frame)
      ("s-SPC" . rectangle-mark-mode)
      ("s-w"   . save-whole-line)
      ("C-c ;" . comment-region)
      ("M-p"   . scroll-down-line)
      ("M-n"   . scroll-up-line))
;;; keys.el ends here
