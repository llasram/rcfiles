(defadvice typopunct-insert-single-quotation-mark (around smarterquote activate)
  (if (texmathp)
      (insert ?\')
    ad-do-it))

(provide 'smarterquote)
