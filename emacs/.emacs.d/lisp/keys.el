;;; keys.el --- my keybindings.

;;; Commentary:

;;; Code:

(defun unbind (&rest keys)
  "Unbind KEYS."
  (dolist (key keys)
    (global-unset-key (kbd key))))

(defmacro bind (&rest key-cons)
  "Bind keys globally from a list of KEY-CONS."
  (dolist (key-con key-cons)
    (let ((key (kbd (car key-con))) (def (cdr key-con)))
      (when (stringp def)
        (setq def (kbd def)))
      (global-set-key key def))))

(defun save-whole-line ()
  "Save whole line to the kill ring."
  (interactive)
  (kill-ring-save (line-beginning-position) (line-end-position)))

(use-package tetris
  :bind
  (:map tetris-mode-map
        ("C-p" . tetris-rotate-prev)
        ("C-b" . tetris-move-left)
        ("C-f" . tetris-move-right)
        ("C-n" . tetris-move-bottom)))

(use-package windmove
  :bind
  (("C-c h" . windmove-left)
   ("C-c j" . windmove-down)
   ("C-c k" . windmove-up)
   ("C-c l" . windmove-right)))

(bind ("s-i"   . load-init)
      ("s-o"   . make-frame)
      ("s-SPC" . rectangle-mark-mode)
      ("s-w"   . save-whole-line)
      ("C-c ;" . comment-or-uncomment-region)
      ("M-p"   . scroll-down-line)
      ("M-n"   . scroll-up-line))

;;; keys.el ends here
