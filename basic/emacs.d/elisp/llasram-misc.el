;;; llasram-misc -- miscellaneous functions

;;; Commentary:

;;; Code:

(defun my/preserve-selected-window (f &rest args)
  "Function version of `save-selected-window'.
Argument F is a function to invoke and optional ARGS any
arguments to `apply' that function to."
  (save-selected-window (apply f args)))

(defun my/describe-symbol ()
  "Personal variant of `describe-symbol'."
  (interactive)
  (let ((sym (or (symbol-at-point) (function-called-at-point))))
    (if sym
      (describe-symbol sym)
      (command-execute 'describe-symbol))))

(provide 'llasram-misc)

;;; llasram-misc.el ends here
