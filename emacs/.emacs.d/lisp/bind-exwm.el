;;; bind-exwm.el --- a use-package keyword -*- lexical-binding: t -*-

;;; Commentary:

;; A `use-package' keyword which allows creating EXWM keybindings.

;;; Code:

(require 'use-package)

(defvar use-package-exwm-bindings '()
  "EXWM keybindings created with `:bind-exwm' keyword.")

(defun exwm-binding-p (entry)
  "Return non-nil if ENTRY is recognized by `exwm-normalize-binding'."
  (and (consp entry)
       (or (stringp (car entry)) (vectorp (car entry)))
       (or (symbolp (cdr entry)) (stringp (cdr entry))
           (and (consp (cdr entry)) ; (PROGRAM . BUFFER-NAME)
                (stringp (cadr entry))
                (stringp (cddr entry))))))

(defun exwm-normalize-binding (entry)
  "Normalize ENTRY into `exwm-input-global-keys'-compatible format.

ENTRY is a cons cell of (BINDING . COMMAND).

BINDING is either a string to pass to `kbd', or a vector of keys.

COMMAND can be:
- A symbol (command name).
- A buffer name to switch to.
- A cons cell of (PROGRAM . BUFFER-NAME).  Switch to BUFFER-NAME, matched with
  regex.  If no match was found, spawn PROGRAM."
  (cons
   (let ((key (car entry)))
     (if (stringp key) ; convert string to vector
         (kbd key)
       key))
   (let ((command (cdr entry)))
     (cl-typecase command
       (symbol command)
       (string
        (lambda ()
          (interactive)
          (switch-to-buffer command)))
       (cons
        (lambda ()
          (interactive)
          (let ((list (mapcar #'buffer-name (buffer-list)))
                (program (car command))
                buffer)
            (while (and list (null buffer))
              (setq buffer (car list))
              (unless (string-match (cdr command) buffer)
                (setq buffer nil)
                (setq list (cdr list))))
            (if buffer
                (switch-to-buffer buffer)
              (call-process program nil 0)))))))))

(defun use-package-exwm-bindings--unwrap ()
  "Return `use-package-exwm-bindings' in unwrapped form.

That is, you can plug it into `exwm-input-global-keys' after normalizing with
\\(mapcar #'exwm-normalize-keybinding ...\\)"
  (let (result)
    ;; CAR is the package name, CDR is the keybinding list.
    (dolist (nested (mapcar #'cdr use-package-exwm-bindings) result)
      ;; Flatten the list.
      (setq result (nconc result nested)))))

(defun use-package-normalize/:bind-exwm (name keyword args)
  "Normalize :bind-exwm arguments.

NAME, KEYWORD, and ARGS, so that `flymake' doesn't complain."
  (let ((arg args)
        args*)
    (while arg
      (let ((x (car arg)))
        (cond
         ((exwm-binding-p x)
          (setq args* (nconc args* (list x))))
         ((listp x)
          (let ((x (use-package-normalize/:bind-exwm name keyword x)))
            (setq args* (nconc args* x)))
          (setq arg (cdr arg)))
         (t
          (use-package-error ":bind-exwm takes a list of (BINDING . COMMAND)")))
        (setq arg (cdr arg))))
    args*))

(defun use-package-handler/:bind-exwm (name _keyword args rest state)
  "Update `use-packge-exwm-bindings' with (NAME . ARGS).

REST and STATE so that `flymake' doesn't complain."
  (use-package-concat
   (use-package-process-keywords name rest state)
   `((assoc-update 'use-package-exwm-bindings ',name ',args))))

;; Add `:bind-exwm' after `:bind'.
(cl-pushnew
 :bind-exwm
 (let ((place nil) ; TODO: find a better way?
       (list use-package-keywords))
   (while (and list (not place))
     (if (eq (car list) :bind)
         (setq place list)
       (setq list (cdr list))))
   (cdr place)))

(defun use-package-bind-exwm-do-cleanup ()
  "Make sure to call this function before reloading your init file.

It cleans up `use-package-exwm-bindings' in case any packages were removed
from the configuration."
  (setq use-package-exwm-bindings '()))

(provide 'bind-exwm)

;;; bind-exwm.el ends here
