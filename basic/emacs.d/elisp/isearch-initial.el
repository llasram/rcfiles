;;; isearch-initial -- Begin isearch with the contents at point

;;; Commentary:

;;; Code:

;; I-search with initial contents
(defvar isearch-initial-string nil)

(defun isearch-set-initial-string ()
  "Set the initial string for isearch."
  (remove-hook 'isearch-mode-hook 'isearch-set-initial-string)
  (setq isearch-string isearch-initial-string)
  (isearch-search-and-update))

(defun isearch-forward-at-point (&optional regexp-p no-recursive-edit)
  "Interactive search forward for the symbol at point.
Will pass REGEXP-P and NO-RECURSIVE-EDIT to the original
function `isearch-forward'."
  (interactive "P\np")
  (if regexp-p (isearch-forward regexp-p no-recursive-edit)
    (let* ((end (progn (skip-syntax-forward "w_") (point)))
           (begin (progn (skip-syntax-backward "w_") (point))))
      (if (eq begin end)
          (isearch-forward regexp-p no-recursive-edit)
        (setq isearch-initial-string (buffer-substring begin end))
        (add-hook 'isearch-mode-hook 'isearch-set-initial-string)
        (isearch-forward regexp-p no-recursive-edit)))))

(provide 'isearch-initial)

;;; isearch-initial.el ends here
