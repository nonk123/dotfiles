;;; utils.el --- utils part of init.el. -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun temp-path (name)
  (concat (temporary-file-directory) name))

(defun unbind (keymap &rest keys)
  "Unbind KEYS from a KEYMAP."
  (dolist (key keys)
    (define-key keymap (kbd key) nil)))

(defun bind (keymap keys-alist)
  "Bind keys from KEYS-ALIST onto KEYMAP and return it."
  (dolist (key-con keys-alist)
    (let ((key (kbd (car key-con)))
          (def (cdr key-con)))
      (define-key keymap key
        (cond
         ((stringp def)
          (kbd def))
         ((and (sequencep def) (not (keymapp def)) (not (functionp def)))
          (bind (make-sparse-keymap) def))
         (t
          def)))))
  keymap)

(defun sh (cmd &optional destination)
  "Run CMD using the default shell.  DESTINATION is passed to `call-process'."
  (interactive)
  (let ((default-directory "~"))
    (call-process (getenv "SHELL") nil destination nil "-l" "-c" cmd)))

(defun sh-output (cmd)
  "Call `sh' and return the output of CMD as string."
  (with-temp-buffer
    (sh cmd (current-buffer))
    (buffer-string)))

(defun sh-output-lines (cmd)
  "Return the result of `sh' called with CMD as a vector of lines."
  (split-string (sh-output cmd) "\n" t))

(defun fetch (action url &rest args)
  "Run function ACTION after successfully fetching URL formatted with ARGS.
ACTION takes one parameter - the response string without headers."
  (with-temp-buffer
    (url-retrieve
     (url-encode-url (apply #'format url args))
     (lambda (_status)
       (forward-paragraph) ;; skip stupid HTTP headers.
       (funcall action (buffer-substring (point) (point-max)))))))

(defun prompt-actions (candidates)
  (with-temp-buffer
    (dolist (candidate (helm-marked-candidates))
      (insert candidate)
      (newline))
    (buffer-string)))

(defun prompt (msg file)
  "Show a `helm' prompt, using candidates from FILE, and MSG as the prompt message."
  (interactive)
  (let ((candidates
         (if (file-exists-p file)
             (with-temp-buffer
               (insert-file-contents file)
               (split-string (buffer-string) "\n" t))
           (error (format "File doesn't exist: %s" file)))))
    (helm :prompt msg
          :candidate-number-limit 250
          :buffer "*prompt*"
          :sources (helm-build-sync-source msg
                     :action 'prompt-actions
                     :candidates candidates))))

(defun set-to-default (variable)
  "Reset VARIABLE (a symbol) to its default value."
  (set variable (default-value variable)))

(defmacro setq-to-default (variable)
  "Call `set-to-default' with VARIABLE, but in `setq'-like macro form."
  (set-to-default variable))

(defun reset-variable ()
  "Interactive version of `set-to-default' (which see)."
  (interactive)
  (when-let* ((symbol (completing-read
                       "Rest value of: "
                       #'help--symbol-completion-table
                       ;; Taken straight from help-fns.el:
                       (lambda (var)
                         (or (get var 'variable-documentation)
                             (and (boundp var) (not (keywordp var))))))))
    (set-to-default (intern symbol))))

;;; utils.el ends here
