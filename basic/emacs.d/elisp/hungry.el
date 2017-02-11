;;; hungry -- generic hungry space deletion

;;; Commentary:

;;; Code:

(defun generic-hungry-code-at-point-p ()
  "True iff the text at point appears to be code."
  (let* ((properties (text-properties-at (point))))
    (null (or (memq 'font-lock-string-face properties)
              (memq 'font-lock-comment-face properties)))))

(defun generic-hungry-backspace (&optional arg)
  "Delete any amount of backspace backwards while in code.
Delete even if in a comment/string when ARG is non-nil."
  (interactive "*P")
  (if (or arg (not (generic-hungry-code-at-point-p)))
      (backward-delete-char-untabify (prefix-numeric-value arg))
    (let ((here (point)))
      (skip-chars-backward " \t\n")
      (if (/= (point) here)
          (delete-region (point) here)
        (backward-delete-char-untabify 1)))))

(defun generic-hungry-delete (&optional arg)
  "Delete any amount of backspace forwards while in code.
Delete even if in a comment/string when ARG is non-nil."
  (interactive "*P")
  (if (or arg (not (generic-hungry-code-at-point-p)))
      (backward-delete-char-untabify (- (prefix-numeric-value arg)))
    (let ((here (point)))
      (skip-chars-forward " \t\n")
      (if (/= (point) here)
          (delete-region (point) here)
        (backward-delete-char-untabify -1)))))

(defmacro generic-hungry-delete-advice (function skip-fn)
  "Define hungry deletion advice.
Add advice for FUNCTION in terms of SKIP-FN."
  (let ((name (intern (concat (symbol-name function) "--hungry"))))
    `(progn
       (defun ,name (f &optional arg &rest args)
         (let ((here (point))
               (there (save-excursion
                        (,skip-fn " \t\n")
                        (point))))
           (if (or arg (not (generic-hungry-code-at-point-p)) (= there here))
               (apply f arg args)
             (delete-region there here))))
       (advice-add ',function :around #',name))))

(provide 'hungry)

;;; hungry.el ends here
