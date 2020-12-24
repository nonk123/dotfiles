;;; no-pop.el --- prevent calls to `pop-to-buffer'

;;; Commentary:

;; `pop-to-buffer' has been quite annoying under EXWM.
;;
;; This package allows replacing that functionality with `switch-to-buffer',
;; for specified commands.

;;; Code:

;;;; Advising

(defun no-pop--pop-to-buffer-hack (buffer-or-name &optional action norecord)
  "Call `switch-to-buffer' with BUFFER-OR-NAME and NORECORD.

ACTION from `pop-to-buffer' is left unused."
  (switch-to-buffer buffer-or-name norecord))

(defun no-pop--display-buffer-hack (original-function buffer-or-name &optional action frame)
  "Display BUFFER-OR-NAME in the current window.

FRAME is passed to the ORIGINAL-FUNCTION, `display-buffer'.
Its ACTION is overriden."
  (let ((action (cons #'display-buffer-same-window '())))
    (funcall original-function buffer-or-name action frame)))

(defun no-pop--main-hack (original-function &rest args)
  "Override `pop-to-buffer' with `switch-to-buffer' just for this call.

ORIGINAL-FUNCTION is called with ARGS."
  (advice-add #'pop-to-buffer :override #'no-pop--pop-to-buffer-hack)
  (advice-add #'display-buffer :around #'no-pop--display-buffer-hack)
  (apply original-function args)
  (advice-remove #'pop-to-buffer #'no-pop--pop-to-buffer-hack)
  (advice-remove #'display-buffer #'no-pop--display-buffer-hack))

;;;; Main

(defvar no-pop--advised-functions '()
  "Previously `no-pop'ped functions to be un-advised.")

(defun no-pop (functions)
  "Prevent calling `pop-to-buffer' in FUNCTIONS.

FUNCTIONS is either a symbol or a list of symbols.

Previously `no-pop'ped functions are reset after each call."
  (dolist (fun no-pop--advised-functions)
    (advice-remove fun #'no-pop--main-hack))
  (setq no-pop--advised-functions '())
  (unless (listp functions)
    (setq functions (list functions)))
  (dolist (fun functions)
    (advice-add fun :around #'no-pop--main-hack))
  (setq no-pop--advised-functions functions))

;;;; `use-package' keyword

(require 'use-package)

(defvar use-package-no-pop-functions '())

(defun use-package-normalize/:no-pop (name-symbol _keyword args)
  "Normalize ARGS into a list of symbol names.

NAME-SYMBOL is the default symbol when no ARGS are specified."
  (cl-typecase args
    (null (list (symbol-name name-symbol)))
    (symbol (list (symbol-name args)))
    (list (mapcar #'symbol-name args))
    (t (use-package-error ":no-pop takes a symbol, a list of symbols, or nothing"))))

(defun use-package-handler/:no-pop (name-symbol keyword functions rest state)
  "Join FUNCTIONS into `use-package-no-pop-functions'.

NAME-SYMBOL, KEYWORD, REST, and STATE do magic."
  (use-package-concat
   (use-package-process-keywords name-symbol rest state)
   `(,@(mapcar
        (lambda (x)
          `(push ',(intern x) use-package-no-pop-functions))
        functions))))

(defun use-package-do-no-pop ()
  "Call this function at the END of your init-file.

It will finalize the `no-pop' changes."
  (no-pop (append no-pop--advised-functions use-package-no-pop-functions))
  ;; Reset for subsequent init-file reloads.
  (setq use-package-no-pop-functions '()))

(add-to-list 'use-package-keywords :no-pop 'append #'eq)

(provide 'no-pop)

;;; no-pop.el ends here
