;; 03paredit.el -- paredit customization's

;; Switch the default/primary backwards-delete key prior to loading
(require 'paredit)

(define-key paredit-mode-map "\C-h" 'paredit-backward-delete)
(define-key paredit-mode-map "\M-h" 'paredit-backward-kill-word)

(defun llasram/paredit-mode ()
  (setq autopair-dont-activate t)
  (autopair-mode 0)
  (paredit-mode 1))

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
  llasram/paredit-hungry-backward-delete
  skip-chars-backward)

(generic-hungry-delete-advice
  paredit-forward-delete
  llasram/paredit-hungry-forward-delete
  skip-chars-forward)
