(defun my/indent-only-inner-classes (langelem)
  (let ((start (line-number-at-pos)))
    (save-excursion
      (beginning-of-line)
      (if (and (search-backward-regexp "^\\s *class" 0 t 2)
               (search-forward-regexp "^\\s *class" (point-max) t 2)
               (search-forward "{" (point-max) t)
               (goto-char (- (point) 1))
               (or (condition-case _
                       (prog1 nil (forward-sexp))
                     ('error t))
                   (<= start (line-number-at-pos))))
          '+
        0))))

(c-add-style
 "llasram/java"
 '((c-basic-offset . 4)
   (fill-column . 80)
   (c-hanging-braces-alist . ((defun-open after)
                              (defun-close before after)
                              (class-open after)
                              (class-close before after)
                              (namespace-open after)
                              (inline-open after)
                              (inline-close before after)
                              (block-open after)
                              (block-close . c-snug-do-while)
                              (extern-lang-open after)
                              (extern-lang-close after)
                              (statement-case-open after)
                              (substatement-open after)))
   (c-offsets-alist . ((inline-open . +)
                       (knr-argdecl-intro . +)
                       (substatement-open . +)
                       (substatement-label . 2)
                       (label . 2)
                       (statement-case-open . 0)
                       (statement-cont . +)
                       (arglist-intro . +)
                       (access-label . -)
                       (func-decl-cont . +)
                       (defun-open . 0)
                       (defun-close . 0)
                       (class-open . 0)
                       (knr-argdecl . 0)
                       (annotation-top-cont . 0)
                       (annotation-var-cont . +)
                       (member-init-intro . +)
                       (member-init-cont . c-lineup-multi-inher)
                       (block-open . 0)
                       (brace-list-open . 0)
                       (brace-list-close . 0)
                       (brace-list-intro . +)
                       (brace-list-entry . 0)
                       (brace-entry-open . 0)
                       (statement-case-intro . +)
                       (substatement . +)
                       (case-label . 0)
                       (do-while-closure . 0)
                       (else-clause . 0)
                       (catch-clause . 0)
                       (arglist-cont c-lineup-gcc-asm-reg 0)
                       (stream-op . c-lineup-streamop)
                       (cpp-macro-cont . +)
                       (cpp-define-intro c-lineup-cpp-define +)
                       (friend . 0)
                       (extern-lang-open . 0)
                       (namespace-open . 0)
                       (module-open . 0)
                       (composition-open . 0)
                       (extern-lang-close . 0)
                       (namespace-close . 0)
                       (module-close . 0)
                       (composition-close . 0)
                       (inextern-lang . +)
                       (innamespace . +)
                       (inmodule . +)
                       (incomposition . +)
                       (template-args-cont c-lineup-template-args +)
                       (inlambda . c-lineup-inexpr-block)
                       (lambda-intro-cont . +)
                       (inexpr-statement . +)
                       (inexpr-class . +)
                       (topmost-intro . 0)
                       (inher-intro . +)
                       (inclass . my/indent-only-inner-classes)
                       (defun-block-intro . +)
                       (inline-close . 0)
                       (topmost-intro-cont . 0)
                       (statement-block-intro . +)
                       (statement . 0)
                       (block-close . 0)
                       (class-close . 0)
                       (c . c-lineup-C-comments)
                       (inher-cont . c-lineup-multi-inher)
                       (string . -1000)
                       (comment-intro . c-lineup-comment)
                       (arglist-cont-nonempty . c-lineup-arglist)
                       (arglist-close . c-lineup-close-paren)
                       (cpp-macro . -1000)))))


(c-add-style
 "rhickey/java"
 '((c-basic-offset . 4)
   (tab-width . 4)
   (c-offsets-alist (inline-open . +)
                    (topmost-intro-cont . 0)
                    (knr-argdecl-intro . +)
                    (substatement-label . 2)
                    (label . 2)
                    (statement-case-open . 0)
                    (arglist-intro . +)
                    (access-label . -)
                    (func-decl-cont . +)
                    (defun-open . 0)
                    (defun-close . 0)
                    (class-open . 0)
                    (knr-argdecl . 0)
                    (annotation-top-cont . 0)
                    (annotation-var-cont . +)
                    (member-init-intro . +)
                    (member-init-cont . c-lineup-multi-inher)
                    (inher-intro . +)
                    (block-open . 0)
                    (brace-list-open . 0)
                    (brace-entry-open . 0)
                    (statement-case-intro . +)
                    (case-label . 0)
                    (do-while-closure . 0)
                    (stream-op . c-lineup-streamop)
                    (cpp-macro-cont . +)
                    (cpp-define-intro c-lineup-cpp-define +)
                    (friend . 0)
                    (extern-lang-open . 0)
                    (namespace-open . 0)
                    (module-open . 0)
                    (composition-open . 0)
                    (extern-lang-close . 0)
                    (namespace-close . 0)
                    (module-close . 0)
                    (composition-close . 0)
                    (inextern-lang . +)
                    (innamespace . +)
                    (inmodule . +)
                    (incomposition . +)
                    (template-args-cont c-lineup-template-args +)
                    (inlambda . c-lineup-inexpr-block)
                    (lambda-intro-cont . +)
                    (inexpr-statement . +)
                    (inexpr-class . +)
                    (topmost-intro . 0)
                    (inclass . 0)
                    (arglist-cont . 0)
                    (defun-block-intro . +)
                    (statement . 0)
                    (substatement-open . +)
                    (statement-block-intro . 0)
                    (substatement . +)
                    (block-close . 0)
                    (inline-close . 0)
                    (brace-list-intro . *)
                    (brace-list-entry . 0)
                    (brace-list-close . -)
                    (class-close . 0)
                    (else-clause . 0)
                    (catch-clause . 0)
                    (statement-cont . -2)
                    (c . c-lineup-C-comments)
                    (inher-cont . c-lineup-multi-inher)
                    (string . -1000)
                    (comment-intro . c-lineup-comment)
                    (arglist-cont-nonempty . c-lineup-arglist)
                    (arglist-close . c-lineup-close-paren)
                    (cpp-macro . -1000))))

(provide 'llasram-c-style)
