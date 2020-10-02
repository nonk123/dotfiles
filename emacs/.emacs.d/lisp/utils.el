;;; utils.el --- utils part of init.el. -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun unbind (keymap &rest keys)
  "Unbind KEYS from a KEYMAP."
  (dolist (key keys)
    (define-key keymap (kbd key) nil)))

(defun bind (keymap keys-alist)
  "Bind keys from KEYS-ALIST onto KEYMAP, and return the modified keymap.

KEYS-ALIST contains `(key . binding)', where `key' is a string passed to `kbd',
and `binding' is either a symbol, string, keymap, or another keys-alist."
  (pcase-dolist (`(,key . ,def) keys-alist keymap)
    (define-key keymap (kbd key) ; `key' comes in string form
      (cl-typecase def
       (string ; key alias
        (kbd def))
       ((or symbol function keymap)
        def)
       (t
        (bind (make-sparse-keymap) def))))))

(defun sh (cmd &optional destination pwd)
  "Run CMD using the default shell.

DESTINATION is passed to `call-process'.

If PWD is specified, use that as the `default-directory', instead of `~'."
  (interactive)
  (let ((default-directory (or pwd "~")))
    (call-process (getenv "SHELL") nil destination nil "-l" "-c" cmd)))

(defun sh-string (cmd)
  "Call `sh' and return the output of CMD as string."
  (with-temp-buffer
    (sh cmd (current-buffer))
    (buffer-string)))

(defun sh-lines (cmd)
  "Return the result of `sh-string' called with CMD as a list of lines."
  (split-string (sh-string cmd) "\n" t))

(defun fetch (action url &rest args)
  "Run function ACTION after successfully fetching URL formatted with ARGS.

ACTION takes one parameter: the response string without headers."
  (with-temp-buffer
    (url-retrieve
     (url-encode-url (apply #'format url args))
     (lambda (_status)
       (forward-paragraph) ; skip stupid HTTP headers.
       (funcall action (buffer-substring (point) (point-max)))))))

(defun prompt--action (_candidates)
  "Output marked candidates, each on its own line.

Used internally in function `prompt'."
  (with-output-to-string
    (mapc #'princ (helm-marked-candidates))
    (princ "\n")))

(defun mktemp (filename &optional contents)
  "Create FILENAME in temporary directory, suffixed with random garbage.

Insert CONTENTS if non-nil.

Return the created file's name like `make-temp-file' (which see)."
  (make-temp-file filename nil nil contents))

(defun prompt (msg file)
  "Show a prompt, using candidates from FILE and MSG as the prompt message.

Used internally in `prompt' script; avoid calling this, at all cost."
  (interactive)
  (helm :prompt msg
        :candidate-number-limit 250
        :buffer (format "*%s*" msg)
        :sources (helm-build-sync-source msg
                   :action #'prompt--action
                   :candidates
                   (if (file-exists-p file)
                       (with-temp-buffer
                         (insert-file-contents file)
                         ;; Each candidate on a separate line.
                         (split-string (buffer-string) "\n" t))
                     (error (format "File doesn't exist: %s" file))))))

(defun set-to-default (variable)
  "Reset VARIABLE (a symbol) to its default value."
  (set variable (default-value variable)))

(defmacro setq-to-default (variable)
  "Call `set-to-default' with VARIABLE, in macro form."
  (set-to-default variable))

(defun reset-variable ()
  "Interactive version of `set-to-default' (which see)."
  (interactive)
  (when-let* ((symbol (completing-read
                       "Reset value of: "
                       ;; Taken straight from help-fns.el:
                       #'help--symbol-completion-table
                       (lambda (var)
                         (or (get var 'variable-documentation)
                             (and (boundp var) (not (keywordp var))))))))
    (set-to-default (intern symbol)))) ; returns a string

;;; utils.el ends here
