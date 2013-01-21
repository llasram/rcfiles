(require 'ess)

(defun ess-truncate-buffer ()
  (interactive)
  (let ((cbms comint-buffer-maximum-size))
    (unwind-protect
        (progn
          (setq comint-buffer-maximum-size 0)
          (comint-truncate-buffer))
      (setq comint-buffer-maximum-size cbms))))

(provide 'llasram-ess)
