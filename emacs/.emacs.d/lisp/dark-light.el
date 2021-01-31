;;; dark-light.el --- easily switch between dark and light themes.

;;; Commentary:

;;; Code:

(defun dl--set-theme (symbol value)
  "Set SYMBOL to theme VALUE, loading it if needed."
  (load-theme value 'no-confirm 'no-enable)
  (set symbol value))

(defcustom dl-dark-theme nil
  "The dark theme."
  :group 'dark-light
  :type 'symbol
  :set #'dl--set-theme)

(defcustom dl-light-theme nil
  "The light theme."
  :group 'dark-light
  :type 'symbol
  :set #'dl--set-theme)

(defun dl-switch ()
  "Switch between the dark and light themes.

If the dark theme is active, switch to the light theme.
Switch to the dark theme otherwise."
  (interactive)
  (if (memq dl-dark-theme custom-enabled-themes)
      (progn
        (disable-theme dl-dark-theme)
        (enable-theme dl-light-theme))
    (disable-theme dl-light-theme)
    (enable-theme dl-dark-theme)))

(provide 'dark-light)

;;; dark-light.el ends here
