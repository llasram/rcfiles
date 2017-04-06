;;; llasram-misc -- miscellaneous functions

;;; Commentary:

;;; Code:

(require 'cl-lib)

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

(defconst my/css-font-sizes
  '("xx-large" "x-large" "large" "medium" "small" "x-small" "xx-small")
  "CSS named font sizes, from largest to smallest.")

(defun my/shift-css-font-size-up ()
  "Shift CSS font-size to be larger."
  (interactive)
  (save-excursion
    (cl-mapcar #'(lambda (size1 size)
                   (goto-char (point-min))
                   (let ((regexp (concat "\\(font-size *: *\\)" size))
                         (newtext (concat "\\1" size1)))
                     (while (re-search-forward regexp nil t)
                       (replace-match newtext))))
               my/css-font-sizes (cdr my/css-font-sizes))))

(provide 'llasram-misc)

;;; llasram-misc.el ends here
