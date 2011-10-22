(require 'ruby-mode)

(defun test-ruby-parse-partial ()
  (let ((end (point-max)) state)
    (goto-char (point-min))
    (while (and (< (point) end)
                (setq state (apply 'ruby-parse-partial end state)))
      (princ (cons (point) state)) (princ "\n"))))

(provide 'ruby-mode-test)
