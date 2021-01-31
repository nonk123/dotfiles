;;; my-utils.el --- various utilities shared across custom packages.

;;; Commentary:

;;; Code:

(require 'subr-x)

;;;; Lisp utils

(defun concat-symbols (&rest symbols)
  "Concatenate SYMBOLS as if they were strings."
  (intern (apply #'concat (mapcar #'symbol-name symbols))))

(defun assoc-update (place key value &optional testfn)
  "Destructively update the VALUE of KEY in alist PLACE.

PLACE is a symbol pointing to an alist.

KEY and VALUE correspond to the alist entry (KEY . VALUE).

If KEY is not present in PLACE, push (KEY . VALUE).
If KEY is present, update the VALUE.

TESTFN is passed to `assoc' call on PLACE."
  (if-let ((entry (assoc key (symbol-value place) testfn)))
      (setf (cdr entry) value)
    (add-to-list place (cons key value))))

(defun pipe (value &rest functions)
  "While VALUE is non-nil, pipe it into the next element of FUNCTIONS."
  (while (and value functions)
    (setq value (funcall (car functions) value))
    (setq functions (cdr functions)))
  value)

(defun predicate-set (symbol predicate argument &optional wrapper)
  "Set SYMBOL's value to ARGUMENT if PREDICATE with ARGUMENT returns non-nil.

Otherwise, return nil.

If WRAPPER is non-nil, call it with ARGUMENT before setting the value."
  (when (funcall predicate argument)
    (set symbol (if wrapper (funcall wrapper argument) argument))))

(defun predicate-pipe (value predicate function)
  "Pipe VALUE into FUNCTION if PREDICATE with VALUE returns non-nil.

Otherwise, return nil."
  (and (funcall predicate value) (funcall function value)))

;;;; Media

(defvar mpv-executable "C:\\Program Files\\mpv\\mpv.exe")

(defvar last-mpv-process nil
  "Set when `play-media' runs mpv.")

(defun play-media (file &optional interactive)
  "Play FILE with mpv.  Return the mpv process.

If INTERACTIVE is nil, start mpv silently."
  (interactive "fPlay file: ")
  (pipe last-mpv-process 'kill-process)
  (setq last-mpv-process nil)
  (let* ((file (expand-file-name file))
         (args (if interactive (list file) (list "--no-video" file)))
         (process (apply #'start-process "Playback" nil mpv-executable args)))
    (setq last-mpv-process process)))

(provide 'my-utils)

;;; my-utils.el ends here
