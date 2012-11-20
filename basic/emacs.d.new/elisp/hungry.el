(defun generic-hungry-code-at-point-p ()
  (let* ((properties (text-properties-at (point))))
    (null (or (memq 'font-lock-string-face properties)
              (memq 'font-lock-comment-face properties)))))

(defun generic-hungry-backspace (&optional arg)
  (interactive "*P")
  (if (or arg (not (generic-hungry-code-at-point-p)))
      (backward-delete-char-untabify (prefix-numeric-value arg))
    (let ((here (point)))
      (skip-chars-backward " \t\n")
      (if (/= (point) here)
          (delete-region (point) here)
        (backward-delete-char-untabify 1)))))

(defun generic-hungry-delete (&optional arg)
  (interactive "*P")
  (if (or arg (not (generic-hungry-code-at-point-p)))
      (backward-delete-char-untabify (- (prefix-numeric-value arg)))
    (let ((here (point)))
      (skip-chars-forward " \t\n")
      (if (/= (point) here)
          (delete-region (point) here)
        (backward-delete-char-untabify -1)))))

(defmacro generic-hungry-delete-advice (function name skip-fn)
  `(defadvice ,function
     (around ,name activate)
     (let ((arg (ad-get-arg 0)) (here (point))
           (there (save-excursion
                    (,skip-fn " \t\n")
                    (point))))
       (if (or arg (not (generic-hungry-code-at-point-p)) (= there here))
           ad-do-it
         (delete-region there here)))))

(generic-hungry-delete-advice
  paredit-backward-delete
  my/paredit-hungry-backward-delete
  skip-chars-backward)

(generic-hungry-delete-advice
  paredit-forward-delete
  my/paredit-hungry-forward-delete
  skip-chars-forward)

(provide 'hungry)
