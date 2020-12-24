;;; ff-extras.el --- `find-file' additions. -*- lexical-binding: t -*-

;;; Commentary:

;; A small package that allows starting external utilities from `find-file'.
;;
;; WARNING: calling `find-file' twice will start a program twice!

;;; Code:

(require 'subr-x)

(defvar ff-extras-alist '()
  "An alist of form (EXTENSIONS . ACTION).

EXTENSIONS can be a string or a list of strings.

If a file is opened with `find-file', and its extension matches, perform ACTION.

If ACTION is a function, call it with one argument: expanded filename.

If ACTION is a list, its CAR is a program to start, and CDR is its args.

Special argument :file is substituted with the found file, expanded.")

(defun ff-extras--normalize-args (args filename)
  "Replace :file entries in ARGS with FILENAME."
  (mapcar (lambda (x)
            (if (eq x :file)
                (expand-file-name filename)
              x))
          args))

(defun ff-extras--find-file-hack (original-function filename &optional wildcards)
  "A `find-file' advice to start external utilities for FILENAME's extension.

WILDCARDS is used in ORIGINAL-FUNCTION."
  (if-let* ((extension (file-name-extension filename))
            (entry (assoc extension ff-extras-alist
                          (lambda (extensions key)
                            (if (listp extensions)
                                (member key extensions)
                              (equal key extensions)))))
            (action (cdr entry)))
      (if (or (symbolp action) (functionp action))
          (funcall action (expand-file-name filename))
        (apply #'start-process "ff-extras" nil (car action)
               (ff-extras--normalize-args (cdr action) filename)))
    (funcall original-function filename wildcards)))

(advice-add #'find-file :around #'ff-extras--find-file-hack)

(provide 'ff-extras)

;;; ff-extras.el ends here
