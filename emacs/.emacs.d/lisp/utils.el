;;; utils.el --- utils part of init.el.

;;; Commentary:

;;; Code:

(defun unbind (keymap &rest keys)
  "Unbind KEYS from a KEYMAP."
  (dolist (key keys)
    (define-key keymap (kbd key) nil)))

(defmacro bind (keymap &rest key-cons)
  "Bind keys from a list of KEY-CONS onto a KEYMAP."
  (dolist (key-con key-cons)
    (let ((key (kbd (car key-con))) (def (cdr key-con)))
      (when (stringp def)
        (setq def (kbd def)))
      (define-key (eval keymap) key def))))

(defun sh (cmd &optional sync buf)
  "Run CMD using the default shell.
Asynchronous if SYNC is nil.  May output to BUF if it is set."
  (interactive)
  (let ((default-directory "~"))
    (call-process (getenv "SHELL")
                  nil
                  (cond (buf buf)
                        (sync nil)
                        (t 0))
                  nil
                  "-l" "-c" cmd)))

(defun sh-output (cmd)
  "Call `sh' and return the output of CMD as string."
  (let (output)
    (with-temp-buffer
      (sh cmd t (current-buffer))
      (setq output (buffer-string)))
    output))

(defun sh-output-lines (cmd)
  "Return the result of `sh' called with CMD as a vector of lines."
  (split-string (sh-output cmd) "\n" t))

(defun prompt (msg file)
  "Show a `helm' prompt, using candidates from FILE, and MSG as the prompt message."
  (let ((candidates
         (if (file-exists-p file)
             (with-temp-buffer
               (insert-file-contents file)
               (split-string (buffer-string) "\n" t))
           (error (format "File doesn't exist: %s" file)))))
    (helm :prompt msg
          :buffer "*prompt*"
          :sources (helm-build-sync-source "prompt"
                     :candidates candidates))))

;;; utils.el ends here
