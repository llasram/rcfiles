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
 '(bbdb-message-all-addresses t)
 '(bbdb-mua-pop-up-window-size 2)
 '(bbdb-pop-up-layout (quote one-line))
 '(bbdb-pop-up-window-size 2)
 '(browse-url-browser-function (quote browse-url-xdg-open))
 '(canlock-password "30a32630624cdac50341667c89e7e01a9713a094")
 '(cider-auto-jump-to-error t)
 '(cider-auto-select-error-buffer nil)
 '(cider-history-file "~/.cidr-history")
 '(cider-lein-parameters "trampoline repl :headless")
 '(cider-prompt-for-symbol nil)
 '(cider-repl-history-file "/home/llasram/.cider-history")
 '(cider-repl-pop-to-buffer-on-connect nil)
 '(cider-repl-use-clojure-font-lock t)
 '(cider-server-command "lein trampoline repl :headless")
 '(column-number-mode t)
 '(comint-move-point-for-output (quote this))
 '(comint-scroll-to-bottom-on-input (quote this))
 '(company-backends
   (quote
    (company-nxml company-css company-eclim company-semantic company-clang company-xcode company-ropemacs company-cmake company-capf
                  (company-dabbrev-code company-gtags company-etags company-keywords)
                  company-oddmuse company-files)))
 '(company-minimum-prefix-length 2)
 '(company-tooltip-align-annotations t)
 '(eldoc-idle-delay 0)
 '(electric-indent-mode t)
 '(electric-layout-mode t)
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-pyvenv elpy-module-highlight-indentation elpy-module-sane-defaults)))
 '(elpy-rpc-backend "jedi")
 '(ensime-implicit-gutter-icons nil)
 '(erc-auto-query (quote bury))
 '(erc-fill-column 43)
 '(erc-hide-list nil)
 '(erc-insert-post-hook
   (quote
    (erc-truncate-buffer erc-make-read-only erc-track-modified-channels)))
 '(erc-lurker-hide-list (quote ("JOIN" "NICK" "PART" "QUIT")))
 '(erc-modules
   (quote
    (completion hl-nicks netsplit fill button match track readonly networks ring autojoin noncommands irccontrols move-to-prompt stamp menu list)))
 '(erc-prompt ">")
 '(erc-query-display (quote buffer))
 '(erc-spelling-mode t)
 '(ess-default-style (quote RStudio))
 '(fill-column 80)
 '(flyspell-abbrev-p t)
 '(flyspell-issue-message-flag nil)
 '(flyspell-use-global-abbrev-table-p t)
 '(fringe-mode (quote (2 . 8)) nil (fringe))
 '(git-gutter:hide-gutter t)
 '(global-company-mode t)
 '(global-git-gutter-mode t)
 '(gnus-gcc-mark-as-read t)
 '(gnus-message-archive-group "nnml:sent")
 '(ido-default-buffer-method (quote selected-window))
 '(ido-default-file-method (quote selected-window))
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(ido-use-filename-at-point (quote guess))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(ispell-program-name "aspell")
 '(j-console-cmd "ijconsole")
 '(kill-whole-line t)
 '(magit-diff-options (quote ("--patience")))
 '(magit-push-always-verify nil)
 '(mail-user-agent (quote gnus-user-agent))
 '(matlab-functions-have-end t)
 '(matlab-shell-command "/opt/mathworks/matlab/bin/matlab")
 '(matlab-shell-command-switches (quote ("-nodesktop" "-nosplash")))
 '(max-specpdl-size 2600)
 '(menu-bar-mode nil)
 '(message-alternative-emails nil)
 '(message-dont-reply-to-names "\\(llasram\\|mbockrath\\)@")
 '(message-send-mail-function (quote message-smtpmail-send-it))
 '(mml2015-encrypt-to-self t)
 '(mml2015-signers (quote ("69ABB985")))
 '(mouse-yank-at-point t)
 '(nrepl-history-file "~/.nrepl-history")
 '(nrepl-popup-stacktraces nil)
 '(nrepl-server-command "lein trampoline repl :headless")
 '(org-adapt-indentation nil)
 '(org-babel-clojure-backend (quote cider))
 '(org-babel-load-languages
   (quote
    ((R . t)
     (emacs-lisp . t)
     (clojure . t)
     (sh . t)
     (python . t)
     (octave . t)
     (ipython . t))))
 '(org-confirm-babel-evaluate nil)
 '(org-export-babel-evaluate nil)
 '(org-format-latex-options
   (quote
    (:foreground default :background default :scale 1.5 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\["))))
 '(org-latex-classes
   (quote
    (("article" "\\documentclass[11pt]{article}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("report" "\\documentclass[11pt]{report}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("book" "\\documentclass[11pt]{book}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("letter" "\\documentclass[11pt]{letter}"))))
 '(org-latex-default-packages-alist
   (quote
    (("T1" "fontenc" t)
     ("AUTO" "inputenc" t)
     ("" "fixltx2e" nil)
     ("" "graphicx" t)
     ("" "grffile" t)
     ("" "longtable" nil)
     ("" "wrapfig" nil)
     ("" "rotating" nil)
     ("normalem" "ulem" t)
     ("" "amsmath" t)
     ("" "textcomp" t)
     ("" "amssymb" t)
     ("" "capt-of" nil)
     ("" "hyperref" nil))))
 '(org-latex-listings (quote minted))
 '(org-latex-minted-langs
   (quote
    ((emacs-lisp "common-lisp")
     (cc "c++")
     (cperl "perl")
     (shell-script "bash")
     (caml "ocaml")
     (ess-jags "jags")
     (ess-bugs "bugs")
     (ipython "python"))))
 '(org-latex-packages-alist (quote (("" "lmodern" nil) ("" "mathtools" nil))))
 '(org-latex-pdf-process
   (quote
    ("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f" "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f" "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")))
 '(org-latex-prefer-user-labels t)
 '(org-log-done (quote time))
 '(org-src-fontify-natively t)
 '(org-startup-with-inline-images t)
 '(org-startup-with-latex-preview t)
 '(org-structure-template-alist
   (quote
    (("s" "#+begin_src ?

#+end_src" "<src lang=\"?\">

</src>")
     ("e" "#+begin_example
?
#+end_example" "<example>
?
</example>")
     ("q" "#+begin_quote
?
#+end_quote" "<quote>
?
</quote>")
     ("v" "#+begin_verse
?
#+end_verse" "<verse>
?
</verse>")
     ("v" "#+begin_verbatim
?
#+end_verbatim" "<verbatim>
?
</verbatim>")
     ("c" "#+begin_center
?
#+end_center" "<center>
?
</center>")
     ("l" "#+begin_latex
?
#+end_latex" "<literal style=\"latex\">
?
</literal>")
     ("l" "#+latex: " "<literal style=\"latex\">?</literal>")
     ("h" "#+begin_html
?
#+end_html" "<literal style=\"html\">
?
</literal>")
     ("h" "#+html: " "<literal style=\"html\">?</literal>")
     ("a" "#+begin_ascii
?
#+end_ascii")
     ("a" "#+ascii: ")
     ("i" "#+index: ?" "#+index: ?")
     ("i" "#+include: %file ?" "<include file=%file markup=\"?\">"))))
 '(package-archive-enable-alist nil)
 '(package-archive-exclude-alist nil)
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa-stable" . "https://stable.melpa.org/packages/")
     ("marmalade" . "http://marmalade-repo.org/packages/")
     ("org" . "http://orgmode.org/elpa/"))))
 '(racer-cmd "~/.cargo/bin/racer")
 '(racer-rust-src-path "/home/llasram/ws/rust/src/")
 '(ruby-electric-expand-delimiters-list (quote (124)))
 '(safe-local-variable-values
   (quote
    ((org-latex-prefer-user-labels . t)
     (whitespace-style face tabs spaces trailing lines space-before-tab::space newline indentation::space empty space-after-tab::space space-mark tab-mark newline-mark)
     (ess-bugs-chains . 1)
     (whitespace-mode)
     (whitespace-line-column . 99)
     (eval put-clojure-indent
           (quote c-for)
           (quote defun))
     (eval\.
      (whitespace-mode -1))
     (encoding . utf-8))))
 '(save-abbrevs (quote silently))
 '(save-place t nil (saveplace))
 '(scroll-bar-mode nil)
 '(send-mail-function (quote smtpmail-send-it))
 '(server-raise-frame nil)
 '(show-paren-mode t)
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 587)
 '(tab-width 8)
 '(temp-buffer-max-height (lambda (buffer) (/ (- (frame-height) 2) 3)))
 '(temp-buffer-resize-mode t)
 '(tool-bar-mode nil)
 '(typopunct-buffer-language (quote english))
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(user-full-name "Marshall Bockrath-Vandegrift")
 '(user-mail-address "llasram@gmail.com")
 '(warning-suppress-types (quote ((undo discard-info))))
 '(whitespace-action (quote (auto-cleanup)))
 '(whitespace-style
   (quote
    (face trailing lines-tail space-before-tab indentation space-after-tab tab-mark)))
 '(x-gtk-use-system-tooltips nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-warning-highlight-face ((t (:inherit nil :underline (:color "dark orange" :style wave)))))
 '(git-gutter:added ((t (:foreground "green" :inverse-video t :weight bold))))
 '(git-gutter:deleted ((t (:foreground "red" :inverse-video t :weight bold))))
 '(git-gutter:modified ((t (:foreground "magenta" :inverse-video t :weight bold))))
 '(gnu-apl-default ((t (:family "APL385 Unicode Regular"))) t)
 '(j-adverb-face ((t (:foreground "forest green"))))
 '(j-conjunction-face ((t (:foreground "dark cyan"))))
 '(j-verb-face ((t (:foreground "Blue1"))))
 '(preview-reference-face ((t (:height 112)))))
