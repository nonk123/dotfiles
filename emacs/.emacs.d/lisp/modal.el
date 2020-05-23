;;; modal.el --- my modal keybindings.

;;; Commentary:

;;; Code:

(defun show-documentation-at-point ()
  "Show documentation for symbol at point in a temporary buffer."
  (interactive)
  (cond
   ((or (equal major-mode 'emacs-lisp-mode)
        (equal major-mode 'lisp-interaction-mode))
    (describe-symbol (symbol-at-point)))
   ((bound-and-true-p slime-mode)
    (slime-documentation (slime-symbol-at-point)))
   ((bound-and-true-p eglot--managed-mode)
    (eglot-help-at-point))
   (t
    (message "No documentation handler found"))))

(defvar-local modal-state 'normal)

(defun modal-toggle-state (&optional state)
  (setq modal-state
        (cond (state
               state)
              ((eq modal-state 'normal)
               'insert)
              ((eq modal-state 'insert)
               'normal)))
  (setf (cdr (assoc 'modal-mode minor-mode-map-alist))
        (cond
         ((eq modal-state 'insert)
          (make-keymap))
         ((eq modal-state 'normal)
          (bind (make-sparse-keymap) modal-bindings)))))

(defun modal-normal ()
  (interactive)
  (modal-toggle-state 'normal))

(defun modal-insert ()
  (interactive)
  (modal-toggle-state 'insert))

(defun modal-esc ()
  (interactive)
  (when (use-region-p)
    (keyboard-quit))
  (modal-normal))

(defvar modal-bindings '())

(setq modal-bindings
      `(("h" . backward-char)
        ("j" . next-line)
        ("k" . previous-line)
        ("l" . forward-char)
        ("J" . scroll-up-line)
        ("K" . scroll-down-line)
        ("a" . beginning-of-line)
        ("e" . end-of-line)
        ("G" . end-of-buffer-or-goto-line)
        ("H" . backward-sexp)
        ("L" . forward-sexp)
        ("w" . forward-word)
        ("b" . backward-word)
        ("(" . sp-backward-sexp)
        (")" . sp-forward-sexp)
        ("i" . modal-insert)
        ("m" . modal-newline)
        ("o" . modal-open-line)
        ("x" . delete-char)
        ("X" . delete-backward-char)
        ("s" . helm-swoop)
        ("S" . helm-multi-swoop-projectile)
        ("q" . kmacro-start-macro)
        ("Q" . kmacro-end-macro)
        ("@" . kmacro-end-and-call-macro)
        ("u" . undo)
        ("v" . set-mark-command)
        ("C-v" . rectangle-mark-mode)
        ("p" . yank)
        ("y" . kill-ring-save)
        (";" . comment-line)
        ("c" . recenter-top-bottom)
        ("," . previous-buffer)
        ("." . next-buffer)
        ("C-n" . modal-scroll-up)
        ("C-p" . modal-scroll-down)
        ("0" . "C-0")
        ("1" . "C-1")
        ("2" . "C-2")
        ("3" . "C-3")
        ("4" . "C-4")
        ("5" . "C-5")
        ("6" . "C-6")
        ("7" . "C-7")
        ("8" . "C-8")
        ("9" . "C-9")
        ("r" . (("r" . replace-character-or-region)
                ("w" . replace-word)
                ("b" . backward-replace-word)))
        ("d" . (("d" . kill-whole-line-or-region)
                ("w" . kill-word)
                ("b" . backward-kill-word)
                ("k" . backward-kill-sexp)
                ("j" . kill-sexp)
                ("h" . backward-kill-line)
                ("l" . kill-line)))
        ("g" . (("g" . beginning-of-buffer-or-goto-line)
                ("l" . avy-goto-line)
                ("w" . avy-goto-word-1)
                ("c" . avy-goto-char)))
        (":" . (("w" . save-buffer)
                ("q" . kill-current-buffer)))
        ("SPC" . (("g" . magit-status)
                  ("p" . ,projectile-command-map)
                  ("f" . eglot-code-actions)
                  ("r" . eglot-rename)
                  ("d" . show-documentation-at-point)
                  ("j" . flymake-goto-next-error)
                  ("k" . flymake-goto-prev-error)
                  ("e" . eval-buffer)))))

(defun modal-mode-lighter ()
  (format " M[%s]" (upcase (symbol-name modal-state))))

(define-minor-mode modal-mode
  "A minor mode that forces modal keybindings."
  :init-value nil
  :lighter (:eval (modal-mode-lighter))
  :keymap (make-sparse-keymap)
  (modal-toggle-state 'normal)
  (if modal-mode
      (local-set-key (kbd "<escape>") #'modal-esc)
    (local-set-key (kbd "<escape>") esc-map)))

(defun beginning-of-buffer-or-goto-line (&optional arg)
  (interactive "P")
  (call-interactively (if arg #'goto-line #'beginning-of-buffer)))

(defun end-of-buffer-or-goto-line (&optional arg)
  (interactive "P")
  (call-interactively (if arg #'goto-line #'end-of-buffer)))

(defun kill-whole-line-or-region (&optional arg)
  (interactive "P")
  (if (use-region-p)
      (call-interactively #'kill-region)
    (kill-whole-line arg)))

(defun modal-newline ()
  (interactive)
  (call-interactively #'newline)
  (modal-insert))

(defun modal-open-line ()
  (interactive)
  (call-interactively #'open-line)
  (modal-insert))

(defun modal-scroll-up ()
  (interactive)
  (let ((next-screen-context-lines 0))
    (call-interactively #'scroll-up)
    (recenter-top-bottom '(4))))

(defun modal-scroll-down ()
  (interactive)
  (let ((next-screen-context-lines 0))
    (call-interactively #'scroll-down)
    (recenter-top-bottom '(4))))

(defun replace-character-or-region (char)
  (interactive "creplace with: ")
  (if (use-region-p)
      (string-rectangle)
    (delete-char 1)
    (insert-char char)))

(defun replace-word (word)
  (interactive "sreplace with: ")
  (call-interactively #'kill-word)
  (insert word))

(defun backward-replace-word ()
  (interactive)
  (call-interactively #'backward-word)
  (call-interactively #'replace-word))

(defun backward-kill-line (&optional arg)
  (interactive "P")
  (kill-line (when arg (- arg))))

(add-hook 'prog-mode-hook #'modal-mode)
(add-hook 'text-mode-hook #'modal-mode)

;;; modal.el ends here
