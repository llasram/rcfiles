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

(defun TeX-insert-dollar--skip-close-math (f &rest args)
  "If point is before the end of a math section, skip it.
In that case applies F to ARGS."
  (if (cond ((not (texmathp))
             t)
            ((and (eq (preceding-char) ?\$)
                  (eq (following-char) ?\$))
             (backward-char)
             (delete-char 2)
             (insert "\\[\\]")
             (backward-char 2)
             nil)
            ((and (not (eq (preceding-char) ?\$))
                  (eq (following-char) ?\$))
             (forward-char)
             nil)
            ((< (point) (+ 2 (point-min)))
             t)
            ((or (and (not (string= (buffer-substring (- (point) 2) (point)) "\\["))
                      (string= (buffer-substring (point) (+ (point) 2)) "\\]"))
                 (and (not (string= (buffer-substring (- (point) 2) (point)) "\\("))
                      (string= (buffer-substring (point) (+ (point) 2)) "\\)")))
             (forward-char 2)
             nil)
            (t t))
      (apply f args)))

(provide 'llasram-misc)

;;; llasram-misc.el ends here
