;;; modal.el --- my modal keybindings. -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defvar modal-movement-keys '())
(defvar modal-bindings '())
(defvar modal-modes-alist '())

(defvar modal-mode-exit-key (kbd "TAB"))

(defvar-local modal-state 'normal)

(defun modal-toggle-state (&optional state)
  (setq modal-state
        (cond (state
               state)
              ((eq modal-state 'normal)
               'insert)
              (t
               'normal)))
  (setf (cdr (assoc 'modal-mode minor-mode-map-alist))
        (cond
         ((eq modal-state 'insert)
          (make-keymap))
         ((eq modal-state 'normal)
          (bind (make-sparse-keymap)
                (append
                 modal-bindings
                 (if-let ((mode (assoc major-mode modal-mode-specifics-alist)))
                     (cdr mode)
                   (list)))))))
  (local-set-key modal-mode-exit-key #'modal-exit))

(defun modal-normal ()
  (interactive)
  (modal-toggle-state 'normal))

(defun modal-insert ()
  (interactive)
  (modal-toggle-state 'insert))

(defun modal-exit ()
  (interactive)
  (modal-normal)
  (keyboard-quit))

(setq modal-movement-keys
      '(("h" . backward-char)
        ("j" . next-line)
        ("k" . previous-line)
        ("l" . forward-char)
        ("J" . scroll-up-line)
        ("K" . scroll-down-line)
        ("a" . beginning-of-line)
        ("e" . end-of-line)
        ("H" . backward-sexp)
        ("L" . forward-sexp)
        ("w" . forward-word)
        ("b" . backward-word)
        ("(" . sp-backward-sexp)
        (")" . sp-forward-sexp)
        ("G" . end-of-buffer-or-goto-line)
        ("g" . (("g" . beginning-of-buffer-or-goto-line)
                ("l" . avy-goto-line)
                ("w" . avy-goto-word-1)
                ("c" . avy-goto-char)))))

(defun modal-take-movement-commands (key fun &optional dwim-fun keys-alist)
  (let ((bindings
         (mapcar
          (lambda (con)
            (cons
             (car con)
             (if (functionp (cdr con))
                 (lambda ()
                   (interactive)
                   (funcall fun (cdr con)))
               ;; Prevent setting DWIM bindings in child keymaps.
               (modal-take-movement-commands fun nil nil (cdr con)))))
          (or keys-alist modal-movement-keys))))
    ;; If `keys-alist' is supplied, it probably is a child keymap.
    (if keys-alist
        bindings
      (cons key
            (if dwim-fun
                (append bindings (list (cons key dwim-fun)))
              bindings)))))

(define-minor-mode modal-mode
  "A minor mode that forces modal keybindings."
  :init-value nil
  :lighter " Modal"
  :keymap (make-sparse-keymap)
  (if modal-mode
      (modal-toggle-state modal-state)
    (setf (cdr (assoc 'modal-mode minor-mode-map-alist)) nil)))

(defvar line-mark-mode-map
  (let ((map (make-sparse-keymap)))
    (dolist (command '(next-line previous-line right-char left-char forward-char
                                 backward-char))
      (define-key map (vector 'remap command)
        (lambda ()
          (interactive)
          (call-interactively command)
          (line-mark-fix-point)
          (line-mark-fix-mark))))
    map))

(define-minor-mode line-mark-mode
  "Minor mode for selecting whole lines using `set-mark'."
  nil nil nil
  (if (not line-mark-mode)
      (deactivate-mark)
    (add-hook 'deactivate-mark-hook (lambda () (line-mark-mode -1)))
    (unless (region-active-p)
      (push-mark (line-end-position) t t)
      (line-mark-fix-point)
      (message "Mark set (line mode)"))))

(defun line-mark-fix-mark ()
  (let* ((n (- (line-number-at-pos (mark)) (current-line)))
         (beginning (line-beginning-position n))
         (end (line-end-position n)))
    (cond
     ((> (point) beginning)
      (push-mark beginning t t))
     ((< (point) end)
      (push-mark end t t)))))

(defun line-mark-fix-point ()
  (cond
   ((>= (point) (mark))
    (move-end-of-line nil))
   ((<= (point) (mark))
    (move-beginning-of-line nil))))

(defun eval-region-or-buffer ()
  (interactive)
  (call-interactively (if (use-region-p)
                          (prog1 #'eval-region
                            (deactivate-mark))
                        #'eval-buffer)))

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

(defun open-line-and-insert ()
  (interactive)
  (call-interactively #'open-line)
  (modal-insert))

(defun newline-and-insert ()
  (interactive)
  (call-interactively #'newline)
  (modal-insert))

(defun start-macro-or-quit-window ()
  (interactive)
  (cond
   ((derived-mode-p 'help-mode 'eww-mode)
    (quit-window t))
   ((bound-and-true-p vterm-copy-mode)
    (vterm-copy-mode -1))
   (t
    (call-interactively #'kmacro-start-macro-or-insert-counter))))

(defvar-local modal-search-query nil)

(defun modal-can-search-p ()
  (and modal-search-query (not (string-empty-p modal-search-query))))

(defun modal-search (query direction)
  "Search for QUERY and move to the next match according to DIRECTION.
DIRECTION is a string `prev' or `next', or nil to just set the query."
  (interactive "s/\ni")
  (setq modal-search-query query)
  (when (modal-can-search-p)
    (funcall (intern (concat "modal-search-" (or direction "next"))))))

(defun modal-search-backwards (query)
  "Call `modal-search' with QUERY and string `prev' as DIRECTION."
  (interactive "s/")
  (modal-search query "prev"))

(defun modal-search-next ()
  (interactive)
  (if (modal-can-search-p)
      (search-forward modal-search-query)
    (call-interactively #'modal-search)))

(defun modal-search-prev ()
  (interactive)
  (if (modal-can-search-p)
      (search-backward modal-search-query)
    (call-interactively #'modal-search)))

(defun better-rename-file (newname)
  (interactive "FNew name: ")
  (rename-file (buffer-file-name) newname)
  (kill-buffer)
  (find-file newname))

(defun delete-current-file ()
  (interactive)
  (delete-file (buffer-file-name) t)
  (kill-buffer))

(defun insert-command-output (&optional command)
  (interactive)
  (unless command
    (setq command (read-shell-command "!")))
  (insert
   (with-temp-buffer
     (sh command t (current-buffer))
     (buffer-string))))

(defun modal-scroll-up (&optional arg)
  (interactive "P")
  (let ((lines (/ (window-text-height) 2)))
    (recenter (if (and arg (< arg 0)) -1 0))
    (next-line (if arg (* lines arg) lines))))

(defun modal-scroll-down (&optional arg)
  (interactive "P")
  (modal-scroll-up (if arg (- arg) -1)))

(defun modal-save-buffer (&optional arg)
  (interactive "p")
  (if (or (eq arg 4) (not (derived-mode-p 'special-mode)))
      (save-buffer 0)
    (message (format "Run with prefix arg to %s save the buffer."
                     (propertize "really" 'face 'italic)))))

(defun replace-character-or-region ()
  (interactive)
  (if (use-region-p)
      (if (bound-and-true-p rectangle-mark-mode)
          (call-interactively #'string-rectangle)
        (call-interactively #'kill-region)
        (modal-insert))
    (when-let ((char (read-char "replace with: ")))
      (delete-char 1)
      (insert-char char))))

(defun end-of-line-and-insert ()
  (interactive)
  (call-interactively #'end-of-line)
  (modal-insert))

(defun beginning-of-line-and-insert ()
  (interactive)
  (call-interactively #'beginning-of-line)
  (modal-insert))

(defun move-and-kill (move-fun)
  (let ((start (point)))
    (call-interactively move-fun)
    (kill-region start (point))))

(defun move-and-replace (move-fun)
  (move-and-kill move-fun)
  (modal-insert))

(defun move-and-yank (move-fun)
  (let ((start (point)))
    (call-interactively move-fun)
    (kill-ring-save start (point))))

(defun kill-ring-save-region-or-line ()
  (interactive)
  (if (use-region-p)
      (call-interactively #'kill-ring-save)
    (kill-ring-save (line-beginning-position) (line-end-position))))

(defun s/ (pattern with)
  (interactive "ss/\nss/%s/")
  (let ((old-point (point)))
    (goto-char (point-min))
    (while (re-search-forward pattern nil t)
      (replace-match with))
    (goto-char old-point)))

(setq modal-bindings
      `(,@modal-movement-keys
        ("i" . modal-insert)
        ("A" . beginning-of-line-and-insert)
        ("E" . end-of-line-and-insert)
        ("o" . open-line-and-insert)
        ("m" . newline-and-insert)
        ("x" . delete-char)
        ("X" . delete-backward-char)
        ("f" . delete-indentation)
        ("s" . helm-swoop)
        ("S" . helm-multi-swoop-projectile)
        ("q" . start-macro-or-quit-window)
        ("Q" . kmacro-end-macro)
        ("@" . kmacro-end-and-call-macro)
        ("u" . undo)
        ("v" . set-mark-command)
        ("V" . line-mark-mode)
        ("C-v" . rectangle-mark-mode)
        ("p" . yank)
        (";" . comment-line)
        ("t" . indent-for-tab-command)
        ("c" . recenter-top-bottom)
        ("/" . modal-search)
        ("?" . modal-search-backwards)
        ("n" . modal-search-next)
        ("N" . modal-search-prev)
        ("!" . insert-command-output)
        ("#" . insert-file)
        ("," . previous-buffer)
        ("." . next-buffer)
        ("C-n" . modal-scroll-up)
        ("C-p" . modal-scroll-down)
        ("+" . text-scale-increase)
        ("-" . text-scale-decrease)
        ("Z" . (("Z" . save-buffers-kill-terminal)))
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
        ,(modal-take-movement-commands "d" #'move-and-kill #'kill-whole-line-or-region)
        ,(modal-take-movement-commands "r" #'move-and-replace #'replace-character-or-region)
        ,(modal-take-movement-commands "y" #'move-and-yank #'kill-ring-save-region-or-line)
        (":" . (("w" . modal-save-buffer)
                ("q" . kill-current-buffer)
                ("s" . s/)
                ("x" . helm-M-x)))
        ("SPC" . (("g" . magit-status)
                  ("u" . list-packages)
                  ("p" . ,projectile-command-map)
                  ("f" . helm-find-files)
                  ("b" . helm-buffers-list)
                  ("h" . ,help-map)
                  ("F" . eglot-code-actions)
                  ("r" . eglot-rename)
                  ("d" . eldoc-doc-buffer)
                  ("w" . eww)
                  ("j" . flymake-goto-next-error)
                  ("k" . flymake-goto-prev-error)
                  ("e" . eval-region-or-buffer)
                  ("/" . dabbrev-expand)
                  ("C-r" . better-rename-file)
                  ("C-d" . delete-current-file)))))

(setq modal-mode-specifics-alist
      '((eww-mode . (("H" . eww-back-url)
                     ("L" . eww-forward-url)
                     ("t" . eww)
                     ("z" . eww-copy-page-url)
                     ("<M-return>" . eww-open-in-new-buffer)))))

(add-hook 'prog-mode-hook #'modal-mode)
(add-hook 'text-mode-hook #'modal-mode)
(add-hook 'conf-mode-hook #'modal-mode)

(add-hook 'help-mode-hook #'modal-mode)
(add-hook 'eww-mode-hook #'modal-mode)

(defun vterm-copy-mode-modal-mode-hack (_arg)
  "An advice hack to use `modal-mode' alongside `vterm-copy-mode'."
  (if vterm-copy-mode
      (modal-mode 1)
    (modal-mode -1)))

(advice-add #'vterm-copy-mode :after #'vterm-copy-mode-modal-mode-hack)

;;; modal.el ends here
