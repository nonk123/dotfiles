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

(provide 'my-utils)

;;; my-utils.el ends here
