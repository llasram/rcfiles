(defun tight-fit-window-to-buffer (&optional window)
  "Apply temp-buffer-max height to window."
  (let ((window (or window (get-buffer-window))))
    (fit-window-to-buffer window
       (funcall temp-buffer-max-height (window-buffer window)))))

(provide 'tight-fit)
