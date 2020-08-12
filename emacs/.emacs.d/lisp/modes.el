;;; modes.el --- my weird minor modes.

;;; Commentary:

;;; Code:

(defvar extra-font-lock-mode-keywords
  '(("\\<\\(TODO\\|FIXME\\):" 1 font-lock-warning-face t)
    ("^;;; \\(.+?\\.el\\) --- .+" 1 font-lock-warning-face t)
    ("^;;; .+?\\.el --- \\(.+\\)" 1 font-lock-warning-face t)
    ("^;;;? \\(.+?\\):" 1 font-lock-warning-face t)
    ("^;;; \\(.+?\\.el ends here\\)" 1 font-lock-warning-face t)))

(define-minor-mode extra-font-lock-minor-mode
  "Toggle extra syntax highlighting in `font-lock-mode'."
  :init-value nil
  (if extra-font-lock-minor-mode
      (font-lock-add-keywords nil extra-font-lock-mode-keywords)
    (font-lock-remove-keywords nil extra-font-lock-mode-keywords))
  (font-lock-flush))

(define-global-minor-mode extra-font-lock-global-mode
  extra-font-lock-minor-mode extra-font-lock-minor-mode)

(extra-font-lock-global-mode 1)

;;; modes.el ends here
