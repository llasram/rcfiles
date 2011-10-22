;;;; `Pyrex' mode.

(add-to-list 'auto-mode-alist '("\\.pyx\\'" . pyrex-mode))

(define-derived-mode pyrex-mode python-mode "Pyrex"
  (font-lock-add-keywords
   nil
   `((,(concat "\\<\\(NULL"
	       "\\|c\\(def\\|har\\|typedef\\)"
	       "\\|e\\(num\\|xtern\\)"
	       "\\|float"
	       "\\|in\\(clude\\|t\\)"
	       "\\|object\\|public\\|struct\\|type\\|union\\|void"
	       "\\)\\>")
      1 font-lock-keyword-face t))))

(provide 'pyrex)