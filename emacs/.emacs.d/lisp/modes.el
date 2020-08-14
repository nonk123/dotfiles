;;; modes.el --- my weird minor modes.

;;; Commentary:

;;; Code:

(defvar extra-font-lock-mode-keywords
  '(("\\<\\(TODO\\|FIXME\\):" 1 font-lock-warning-face t)
    ("^;;; \\(.+?\\.el\\) --- .+" 1 font-lock-warning-face t)
    ("^;;;? \\(.+?\\):" 1 font-lock-warning-face t)
    ("^;;; \\(.+?\\.el ends here\\)" 1 font-lock-warning-face t)))

(define-minor-mode extra-font-lock-minor-mode
  "Toggle extra syntax highlighting in `font-lock-mode'."
  :init-value nil
  :lighter " EFL"
  (if extra-font-lock-minor-mode
      (font-lock-add-keywords nil extra-font-lock-mode-keywords)
    (font-lock-remove-keywords nil extra-font-lock-mode-keywords))
  (font-lock-flush))

(define-global-minor-mode extra-font-lock-global-mode
  extra-font-lock-minor-mode extra-font-lock-minor-mode)

(extra-font-lock-global-mode 1)

(defvar-local left-fringe-mode--is-managed nil
  "Non-nil if this buffer should have a fringe on the left.")

(define-minor-mode left-fringe-mode
  "Toggle a small fringe on the left of the selected window.
Currently used by Flymake."
  :init-value nil
  :lighter "φ"
  (setq left-fringe-mode--is-managed left-fringe-mode)
  (left-fringe-mode--set-fringe)
  (add-hook 'window-configuration-change-hook #'left-fringe-mode--set-fringe))

(defun left-fringe-mode--set-fringe ()
  (set-window-fringes
   nil
   (if (and left-fringe-mode left-fringe-mode--is-managed)
       (window-font-width)
     0)))

(add-hook 'prog-mode-hook #'left-fringe-mode)

;;; modes.el ends here
