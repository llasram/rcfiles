;;; llasram-tex -- personal functions etc for working with TeX

;;; Commentary:

;;; Code:

(require 'tex)
(require 'flycheck)

(defun typopunct-insert-single-quotation-mark--texmathp (f &rest args)
  "Around-style advice function.
Applies function F to list ARGS."
  (if (and (or (eq major-mode 'latex-mode)
               (eq major-mode 'tex-mode))
           (texmathp))
      (insert ?\')
    (apply f args)))
(advice-add 'typopunct-insert-single-quotation-mark :around
            #'typopunct-insert-single-quotation-mark--texmathp)

(defun LaTeX-common-initialization--electric-pair ()
  "Re-enable electric-pair-mode."
  (setq-local electric-pair-mode t))
(advice-add 'LaTeX-common-initialization :after
            #'LaTeX-common-initialization--electric-pair)

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
(advice-add 'TeX-insert-dollar :around #'TeX-insert-dollar--skip-close-math)

(flycheck-define-checker tex-chkweb
  "A TeX and LaTeX syntax and style checker using chkweb.

See URL `http://www.nongnu.org/chktex/'."
  :command ("chkweb"
            (config-file "--localrc" flycheck-chktexrc)
            ;; Compact error messages, and no version information, and execute
            ;; \input statements
            "--verbosity=0" "--quiet" "--inputfiles")
  :standard-input t
  :error-patterns
  ((warning line-start "stdin:" line ":" column ":"
            (id (one-or-more digit)) ":" (message) line-end))
  :error-filter
  (lambda (errors)
    (flycheck-sanitize-errors (flycheck-increment-error-columns errors)))
  :modes (latex-mode plain-tex-mode))
(add-to-list 'flycheck-checkers 'tex-chkweb)

(provide 'llasram-tex)

;;; llasram-tex.el ends here
