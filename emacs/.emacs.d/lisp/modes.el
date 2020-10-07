;;; modes.el --- my weird minor modes.

;;; Commentary:

;;; Code:

(defvar-local left-fringe-mode--is-managed nil
  "Non-nil if this buffer should have a fringe on the left.

`left-fringe-mode' isn't available in `left-fringe-mode--set-fringe', and this
variable serves as a workaround: it is set internally.")

(define-minor-mode left-fringe-mode
  "Toggle a small fringe on the left of the selected window.

Currently used by Flymake."
  :init-value nil
  (setq left-fringe-mode--is-managed left-fringe-mode)
  (left-fringe-mode--set-fringe)
  (add-hook 'window-configuration-change-hook #'left-fringe-mode--set-fringe))

(defun left-fringe-mode--set-fringe ()
  (set-window-fringes
   (selected-window)
   (if left-fringe-mode--is-managed
       (window-font-width) ; just enough for a '!'
     0)))

(add-hook 'flymake-mode-hook #'left-fringe-mode)

(defvar column-width-alist
  '(("COMMIT_EDITMSG$" . 72)
    (".org$" . 72)
    ("\\*info\\*" . 74)
    (".*" . 80)))

(define-minor-mode auto-fill-column-mode
  "Automatically adjust `fill-column' and others, according to the buffer name."
  :init-value nil
  (pcase-let* ((buffer-name (or buffer-file-name (buffer-name (current-buffer))))
               (entry (assoc buffer-name column-width-alist #'string-match))
               (`(,pattern . ,column) entry))
    (if auto-fill-column-mode
        (progn
          (setq fill-column column)
          (setq olivetti-body-width (+ column 2)))
      (setq-to-default fill-column)
      (setq-to-default olivetti-body-width))))

(dolist (mode '(Info-mode-hook org-mode-hook text-mode-hook))
  (add-hook mode #'auto-fill-column-mode))

;;; modes.el ends here
