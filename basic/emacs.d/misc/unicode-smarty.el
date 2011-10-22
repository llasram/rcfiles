;;; unicode-smarty.el --- toggle Unicode smart keys

;;; Code:
(provide 'unicode-smarty)

(defvar unicode-smarty-mode-map 
  (easy-mmode-define-keymap
   '(("\"" . unicode-smart-double-quote)
     ("'"  . unicode-smart-single-quote)
     ("-"  . unicode-smart-hyphen))))
  
(define-minor-mode unicode-smarty-mode
  "Toggle the unicode smart keys (unicode-smarty) minor mode.
With prefix ARG, turn unicode-smarty minor mode on iff ARG is
positive."
  nil " smarty" nil)
