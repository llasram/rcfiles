(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(abbrev-mode t t)
 '(ac-ignore-case nil)
 '(ac-use-fuzzy nil)
 '(ac-use-menu-map t)
 '(backup-directory-alist (quote ((".*" . "~/.backup"))))
 '(browse-url-browser-function (quote browse-url-xdg-open))
 '(canlock-password "30a32630624cdac50341667c89e7e01a9713a094")
 '(cider-history-file "~/.cidr-history")
 '(cider-repl-history-file "/home/llasram/.cider-history")
 '(cider-repl-pop-to-buffer-on-connect nil)
 '(cider-server-command "lein trampoline repl :headless")
 '(column-number-mode t)
 '(erc-auto-query (quote bury))
 '(erc-fill-column 45)
 '(erc-hide-list (quote ("JOIN" "PART" "QUIT")))
 '(erc-prompt ">")
 '(erc-query-display (quote buffer))
 '(erc-spelling-mode t)
 '(fill-column 80)
 '(flyspell-abbrev-p t)
 '(flyspell-issue-message-flag nil)
 '(flyspell-use-global-abbrev-table-p t)
 '(gnus-message-archive-group "nnml:sent")
 '(ido-default-buffer-method (quote selected-window))
 '(ido-default-file-method (quote selected-window))
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(ido-use-filename-at-point (quote guess))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(kill-whole-line t)
 '(mail-user-agent (quote gnus-user-agent))
 '(max-specpdl-size 2600)
 '(menu-bar-mode nil)
 '(message-send-mail-function (quote message-smtpmail-send-it))
 '(mouse-yank-at-point t)
 '(nrepl-history-file "~/.nrepl-history")
 '(nrepl-popup-stacktraces nil)
 '(nrepl-server-command "lein trampoline repl :headless")
 '(org-adapt-indentation nil)
 '(org-babel-load-languages (quote ((R . t) (emacs-lisp . t) (clojure . t) (sh . t))))
 '(org-confirm-babel-evaluate nil)
 '(org-export-babel-evaluate nil)
 '(org-structure-template-alist (quote (("s" "#+begin_src ?

#+end_src" "<src lang=\"?\">

</src>") ("e" "#+begin_example
?
#+end_example" "<example>
?
</example>") ("q" "#+begin_quote
?
#+end_quote" "<quote>
?
</quote>") ("v" "#+begin_verse
?
#+end_verse" "<verse>
?
</verse>") ("v" "#+begin_verbatim
?
#+end_verbatim" "<verbatim>
?
</verbatim>") ("c" "#+begin_center
?
#+end_center" "<center>
?
</center>") ("l" "#+begin_latex
?
#+end_latex" "<literal style=\"latex\">
?
</literal>") ("l" "#+latex: " "<literal style=\"latex\">?</literal>") ("h" "#+begin_html
?
#+end_html" "<literal style=\"html\">
?
</literal>") ("h" "#+html: " "<literal style=\"html\">?</literal>") ("a" "#+begin_ascii
?
#+end_ascii") ("a" "#+ascii: ") ("i" "#+index: ?" "#+index: ?") ("i" "#+include: %file ?" "<include file=%file markup=\"?\">"))))
 '(package-archive-enable-alist nil)
 '(package-archive-exclude-alist nil)
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("marmalade" . "http://marmalade-repo.org/packages/"))))
 '(ruby-electric-expand-delimiters-list (quote (124)))
 '(safe-local-variable-values (quote ((eval put-clojure-indent (quote c-for) (quote defun)) (eval\. (whitespace-mode -1)) (encoding . utf-8))))
 '(save-abbrevs (quote silently))
 '(save-place t nil (saveplace))
 '(scroll-bar-mode nil)
 '(send-mail-function (quote smtpmail-send-it))
 '(show-paren-mode t)
 '(tab-width 8)
 '(temp-buffer-max-height (lambda (buffer) (/ (- (frame-height) 2) 3)))
 '(temp-buffer-resize-mode t)
 '(tool-bar-mode nil)
 '(typopunct-buffer-language (quote english))
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(user-full-name "Marshall Bockrath-Vandegrift")
 '(user-mail-address "llasram@damballa.com")
 '(whitespace-action (quote (auto-cleanup)))
 '(whitespace-style (quote (face trailing lines-tail space-before-tab indentation space-after-tab tab-mark)))
 '(x-gtk-use-system-tooltips nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-warning-highlight-face ((t (:inherit nil :underline (:color "dark orange" :style wave))))))
