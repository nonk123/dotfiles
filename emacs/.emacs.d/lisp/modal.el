;;; modal.el --- my modal keybindings. -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;; Remove warnings.

(declare-function bind "util")
(declare-function current-line "array")
(declare-function eglot-managed-p "eglot")
(declare-function eglot-rename "eglot")
(declare-function s-concat "utils")

(defvar projectile-command-map)

;;;; Variables.

(defvar modal-bindings nil
  "A keys-alist describing all keybinding available in `normal' state.")

(defvar modal-mode-specifics-alist nil
  "Keys-alists specific to certain major modes.

CAR is the major mode symbol; CDR is a keys-alist.

CDR is overlayed onto `modal-bindings', replacing keys already bound.")

(defvar-local modal-state 'normal
  "Current state: either `normal' or `insert'.

`modal-toggle-state' alters the keybindings according to this variable.")

(defvar modal-mode-exit-key (kbd "TAB")
  "Key bound to `modal-exit'.

It is set in the local keymap irreversibly.")

;;;; Basic functions.

(defun modal--flip-state ()
  "Flip `modal-state' from `insert' to `normal' and vice versa.

This _only_ sets `modal-state', without changing the keymap."
  (modal-toggle-state
   (if (eq modal-state 'normal)
       'insert
     'normal)))

(defun modal--get-specifics ()
  "Return the relevant keys-alist(s) from `modal-mode-specifics-alist'.

The return value is always a list."
  (let ((valid (list)))
    (pcase-dolist (`(,mode . ,specifics) modal-mode-specifics-alist valid)
      (when (derived-mode-p mode)
        (push specifics valid)))))

(defun modal-toggle-state (&optional state)
  "Set `modal-state' and alter the local keymap.

If STATE is non-nil, set `modal-state' to that; flip it otherwise."
  (if state
      (setq modal-state state)
    (modal--flip-state))
  ;; `local-set-key' is irreversible, so we modify the minor mode map directly.
  (setf (cdr (assoc 'modal-mode minor-mode-map-alist))
        (if (eq modal-state 'insert)
            (make-keymap) ; default keymap with self-inserts
          (bind (make-sparse-keymap)
                (append-nested (list modal-bindings) (modal--get-specifics)))))
  (local-set-key modal-mode-exit-key #'modal-exit))

(defun modal-insert (&optional arg)
  "Enter `insert' state from `normal'.

When ARG is non-nil, ask for a string to repeat ARG times.  In this case, stay
in `normal' state."
  (interactive "P")
  (modal-toggle-state 'insert)
  ;; Emulate Vi's insert mode with count.
  (when-let ((arg)
             (string (read-string (format "Repeat x%d: " arg)))
             (starting-size (buffer-size)))
    (dotimes (_ arg)
      (insert string))
    (unless (or (= (buffer-size) starting-size)
                (string-match "^[[:space:]]+$" string))
      (delete-horizontal-space t))
    (modal-toggle-state 'normal)))

(defun modal-exit ()
  "Return to `normal' state from `insert'.

When already `normal', deactivate the mark and stop macro definition."
  (interactive)
  (when (eq modal-state 'normal)
    ;; Copied from `keyboard-quit':
    (deactivate-mark)
    (kmacro-keyboard-quit)
    (when defining-kbd-macro
      (force-mode-line-update t))
    (setq defining-kbd-macro nil))
  (modal-toggle-state 'normal))

;;;; Related modes.

;;;;; Modal.

(define-minor-mode modal-mode
  "A minor mode that forces modal keybindings."
  :init-value nil
  :lighter " μ"
  :keymap (make-sparse-keymap)
  (if modal-mode
      (modal-toggle-state modal-state)
    (setf (cdr (assoc 'modal-mode minor-mode-map-alist)) nil)))

;;;;; Line-mark.

(defvar line-mark-mode-map
  (let ((map (make-sparse-keymap)))
    (dolist (command '(next-line previous-line right-char
                                 left-char forward-char backward-char))
      (define-key map (vector 'remap command)
        (lambda ()
          (interactive)
          (call-interactively command)
          (line-mark--fix-point-and-mark))))
    map))

(define-minor-mode line-mark-mode
  "Minor mode for selecting whole lines using `set-mark'."
  :init-value nil
  ;; Quite a bit of code was copied over from `rectangle-mark-mode'.
  (if (not line-mark-mode)
      (deactivate-mark)
    (add-hook 'deactivate-mark-hook (lambda () (line-mark-mode -1)))
    (unless (region-active-p)
      (push-mark (line-end-position) t t)
      (line-mark--fix-point-and-mark)
      (message "Mark set (line mode)"))))

(defun line-mark--fix-point-and-mark ()
  "Make sure the mark and the point are in the right positions.

There are two such 'positions': mark is in the beginning
of a line, and the point is at the end and vice versa."
  (let* ((line (- (line-number-at-pos (mark)) (current-line)))
         (beginning (line-beginning-position line))
         (end (line-end-position line)))
    (cond
     ((> (point) beginning)
      (push-mark beginning t t)
      (goto-char (line-end-position)))
     ((< (point) end)
      (push-mark end t t)
      (goto-char (line-beginning-position))))))

;;;; Various commands.

;;;;; Context-sensitive (supplied prefix arg, or region is active).

(defun eval-region-or-buffer ()
  "Evaluate region if it is active; evaluate whole buffer otherwise."
  (interactive)
  (if (use-region-p)
      (progn
        (eval-region (region-beginning) (region-end))
        (deactivate-mark))
    (eval-buffer)))

(defun beginning-of-buffer-or-goto-line (&optional arg)
  "If ARG is supplied, go to that line.
Go to the beginning of the buffer otherwise.

Emulation of Vi's 'gg' command"
  (interactive "P")
  (if arg
      (goto-char (line-beginning-position arg))
    (goto-char (point-min))))

(defun end-of-buffer-or-goto-line (&optional arg)
    "If ARG is supplied, go to that line.
Go to the end of the buffer otherwise.

Emulation of Vi's 'G' command"
  (interactive "P")
  (if arg
      (goto-char (line-beginning-position arg))
    (goto-char (point-max))))

(defun kill-whole-line-or-region (&optional arg)
  "If region is active, kill it.  Otherwise, kill ARG (or the current) line(s)."
  (interactive "P")
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (kill-whole-line arg)))

;;;;; Combined.

(defun open-line-and-insert ()
  "Call `open-line' and enter `insert' state."
  (interactive)
  (call-interactively #'open-line)
  (modal-insert))

(defun newline-and-insert ()
  "Call `newline' and enter `insert' state."
  (interactive)
  (call-interactively #'newline)
  (modal-insert))

(defun end-of-line-and-insert ()
  "Enter `insert' state after calling `end-of-line'."
  (interactive)
  (call-interactively #'end-of-line)
  (modal-insert))

(defun beginning-of-line-and-insert ()
  "Enter `insert' state after calling `beginning-of-line'."
  (interactive)
  (call-interactively #'beginning-of-line)
  (modal-insert))

;;;;; Vi-like '/' search.

(defvar-local modal-search-query nil)

(defun modal-can-search-p ()
  "Return nil if user should be prompted for a search query."
  (and modal-search-query
       (not (string-empty-p modal-search-query))))

(defun modal-search (&optional backwards)
  "Ask for a search query and move to the next match forwards.

If BACKWARDS is non-nil, move to the next match backwards instead."
  (interactive "i")
  (when-let ((query (read-string "/")))
    (setq modal-search-query query)
    (if backwards
        (modal-search-prev)
      (modal-search-next))))

(defun modal-search-next ()
  "Go to next match, prompting for a query if there is none."
  (interactive)
  (if (modal-can-search-p)
      (search-forward-regexp modal-search-query)
    (modal-search nil)))

(defun modal-search-prev ()
  "Go to previous match, prompting for a query if there is none."
  (interactive)
  (if (modal-can-search-p)
      (search-backward-regexp modal-search-query)
    (modal-search t)))

;;;;; File operations.

(defun rename-buffer-file (buffer new-name)
  "Rename BUFFER's file to NEW-NAME and re-open it."
  (interactive (list (current-buffer) (read-file-name "New name: ")))
  (let ((file (buffer-file-name buffer)))
    (unless file
      (user-error "Buffer is not assigned to a file"))
    (unless (file-exists-p file)
      (user-error "Buffer file doesn't exist.  Did you forget to save it?"))
    (rename-file file new-name t)
    (let ((kill-buffer-query-functions (list)))
      (kill-buffer buffer))
    (find-file new-name)))

(defun delete-buffer-file (buffer)
  "Kill BUFFER (or the current buffer) after deleting its file."
  (interactive (list (current-buffer)))
  (let ((file (buffer-file-name buffer)))
    (unless file
      (user-error "Buffer is not assigned to a file"))
    (when (file-exists-p file)
      (delete-file file t))
    (kill-buffer buffer)))

(defun modal--special-buffer-p ()
  "Return non-nil if the current buffer is considered 'special'."
  (or (derived-mode-p 'special-mode 'Info-mode)
      (string= (buffer-name) "*scratch*")))

(defun modal-save-buffer (&optional arg)
  "Like `save-buffer', but doesn't save special buffers.

Run with prefix arg ARG to force saving a buffer in this case.

See `modal--special-buffer-p'."
  (interactive "P")
  (if (or (equal arg '(4)) (not (modal--special-buffer-p)))
      (save-buffer 0)
    (message (concat "Run with prefix arg to "
                     (propertize "really" 'face 'italic)
                     " save the buffer"))))

;;;;; Misc.

(defun indent-right (count)
  "Indent COUNT level right."
  (interactive "p")
  (setq count (* count standard-indent))
  (if (use-region-p)
      (indent-rigidly (region-beginning) (region-end) count)
    (indent-rigidly (line-beginning-position) (line-end-position) count)))

(defun indent-left (count)
  "Indent COUNT levels left."
  (interactive "p")
  (indent-right (- count)))

(defun repeat-region (arg start end)
  "Repeat the text between START and END ARG times."
  (interactive "p\nr")
  (dotimes (_ arg)
    (insert (buffer-substring start end))))

(defun modal-scroll-up (&optional arg)
  "Scroll up half a screenful ARG times."
  (interactive "p")
  (let ((lines (/ (window-text-height) 2)))
    (recenter (if (< arg 0) -1 0))
    (forward-line (if arg (* lines arg) lines))))

(defun modal-scroll-down (&optional arg)
  "Scroll down half a screenful ARG times."
  (interactive "p")
  (modal-scroll-up (- arg)))

(defun s/ (pattern new-name &optional search-fun)
  "Replace PATTERN with NEW-NAME, using SEARCH-FUN to match.

SEARCH-FUN defaults to `re-search-forward', mostly for interactive calls.  It
must always search forwards.

Suitable for non-interactive use."
  (interactive "ss/\nss/%s/")
  (unless search-fun
    (setq search-fun #'re-search-forward))
  (let ((old-point (point)))
    (goto-char (point-min))
    (while (funcall search-fun pattern nil t)
      (replace-match new-name))
    (goto-char old-point)))

(defun modal-rename-symbol ()
  "Rename symbol at point.

If `eglot' is active, rename using the language server.
Otherwise, replace text blindly."
  (interactive)
  (if-let ((symbol (symbol-at-point)))
      (if (eglot-managed-p)
          (call-interactively #'eglot-rename)
        (when-let* ((string (symbol-name symbol))
                    (prompt (format "Rename %s to: " string))
                    (new-name (read-string prompt)))
          (s/ string new-name #'search-forward)))
    (user-error "No symbol at point")))

;;;;; Movement-key combos.

(defun replace-character-or-region ()
  "If region is set, kill it and enter `insert' state.

Otherwise, prompt for a character and replace with it the one at point."
  (interactive)
  (if (use-region-p)
      (if (bound-and-true-p rectangle-mark-mode)
          (call-interactively #'string-rectangle)
        (call-interactively #'kill-region)
        (modal-insert))
    (when-let ((char (read-char "Replace with: ")))
      (replace-region-contents
       (point) (1+ (point))
       (lambda ()
         (char-to-string char))))))

(defun move-and-kill (old-point)
  "Kill everything between OLD-POINT and point."
  (kill-region old-point (point)))

(defun move-and-replace (old-point)
  "Kill everything between OLD-POINT and point, and enter `insert' state."
  (move-and-kill old-point)
  (modal-insert))

(defun move-and-yank (old-point)
  "Save to kill ring everything between OLD-POINT and point."
  (kill-ring-save old-point (point)))

(defun kill-ring-save-region-or-line ()
  "If region is active, save it to the kill ring.  Save current line otherwise."
  (interactive)
  (if (use-region-p)
      (call-interactively #'kill-ring-save)
    (kill-ring-save (line-beginning-position) (line-end-position))))

;;;; Key definitions.

;;;;; Internals.

(defun modal--echo (format-string &rest args)
  "Display a message in echo area, without logging it to messages buffer.

Used in `modal-combo' to display pressed keys.

FORMAT-STRING and ARGS are used just like in `message'."
  (let ((message-log-max nil))
    (apply #'message format-string args)))

(defun modal-combo (key move-fun &optional dwim-fun)
  "Return a keys-alist entry which expects a key combo.

The CAR is KEY.  The CDR is a lambda which:
 * Calls DWIM-FUN if DWIM-FUN is supplied and KEY is pressed twice.
 * Calls MOVE-FUN if the point value has changed after a key combo.

DWIM-FUN is a command.  MOVE-FUN is a command which
takes one argument: the previous point value."
  (cons key
        (lambda ()
          (interactive)
          (let ((old-point (point))
                (sequence
                 (progn
                   (modal--echo "%s-" key)
                   (read-key-sequence nil))))
            (modal--echo "%s-%s" key (key-description sequence))
            (when-let ((command (key-binding sequence)))
              (if (and dwim-fun (eq this-command command))
                  (call-interactively dwim-fun)
                (call-interactively command)
                (when (/= (point) old-point)
                  (funcall move-fun old-point))))))))

;;;;; Variables.

(setq modal-bindings
      `(("h" . backward-char)
        ("j" . next-line)
        ("k" . previous-line)
        ("l" . forward-char)
        ("J" . scroll-up-line)
        ("K" . scroll-down-line)
        ("a" . beginning-of-line)
        ("e" . end-of-line)
        ("T" . back-to-indentation)
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
                ("c" . avy-goto-char)))
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
        ("q" . kmacro-start-macro-or-insert-counter)
        ("Q" . kmacro-end-macro)
        ("@" . kmacro-end-and-call-macro)
        ("Z" . yas-expand)
        ("u" . undo)
        ("v" . set-mark-command)
        ("V" . line-mark-mode)
        ("C-v" . rectangle-mark-mode)
        ("D" . kill-line)
        ("p" . yank)
        (";" . comment-line)
        ("t" . indent-for-tab-command)
        ("F" . fill-paragraph)
        ("R" . repeat-region)
        ("c" . recenter-top-bottom)
        ("z" . cycle-spacing)
        ("/" . modal-search)
        ("n" . modal-search-next)
        ("N" . modal-search-prev)
        ("#" . insert-file)
        ("," . indent-left)
        ("." . indent-right)
        ("C-n" . modal-scroll-up)
        ("C-p" . modal-scroll-down)
        ("+" . text-scale-increase)
        ("-" . text-scale-decrease)
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
        ,(modal-combo "d" #'move-and-kill #'kill-whole-line-or-region)
        ,(modal-combo "r" #'move-and-replace #'replace-character-or-region)
        ,(modal-combo "y" #'move-and-yank #'kill-ring-save-region-or-line)
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
                  ("r" . modal-rename-symbol)
                  ("d" . eldoc-doc-buffer)
                  ("w" . eww)
                  ("v" . reset-variable)
                  ("j" . flymake-goto-next-error)
                  ("k" . flymake-goto-prev-error)
                  ("e" . eval-region-or-buffer)
                  ("E" . eval-defun)
                  ("/" . dabbrev-expand)
                  ("o" . org-preview-html-mode)
                  ("x" . org-html-export-to-html)
                  ("C-r" . rename-buffer-file)
                  ("C-d" . delete-buffer-file)))))

(setq modal-mode-specifics-alist
      '((eww-mode . (("H" . eww-back-url)
                     ("L" . eww-forward-url)
                     ("t" . eww)
                     ("z" . eww-copy-page-url)
                     ("<M-return>" . eww-open-in-new-buffer)))
        (special-mode . (("q" . quit-window)))
        (help-mode . (("<" . help-go-back)
                      (">" . help-go-forward)))
        (Info-mode . (("q" . quit-window)
                      ("H" . Info-prev)
                      ("L" . Info-next)
                      ("<" . Info-history-back)
                      (">" . Info-history-forward)))
        (org-mode . (("t" . org-cycle)))))

;;;; Hooks.

(dolist (mode '(prog text conf help apropos Info eww))
  (add-hook (s-concat mode "-mode-hook") #'modal-mode))

;;; modal.el ends here
