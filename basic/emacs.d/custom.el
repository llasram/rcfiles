(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-electric-left-right-brace t)
 '(TeX-command-list
   (quote
    (("TeX" "%(PDF)%(tex) %(file-line-error) %(extraopts) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
      (plain-tex-mode texinfo-mode ams-tex-mode)
      :help "Run plain TeX")
     ("LaTeX" "%`%l%(mode)%' %t" TeX-run-TeX nil
      (latex-mode doctex-mode)
      :help "Run LaTeX")
     ("Makeinfo" "makeinfo %(extraopts) %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with Info output")
     ("Makeinfo HTML" "makeinfo %(extraopts) --html %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with HTML output")
     ("AmSTeX" "amstex %(PDFout) %(extraopts) %`%S%(mode)%' %t" TeX-run-TeX nil
      (ams-tex-mode)
      :help "Run AMSTeX")
     ("ConTeXt" "%(cntxcom) --once --texutil %(extraopts) %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt once")
     ("ConTeXt Full" "%(cntxcom) %(extraopts) %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt until completion")
     ("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX")
     ("Biber" "biber %s" TeX-run-Biber nil t :help "Run Biber")
     ("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer")
     ("Print" "%p" TeX-run-command t t :help "Print the file")
     ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command)
     ("File" "%(o?)dvips %d -o %f " TeX-run-dvips t t :help "Generate PostScript file")
     ("Dvips" "%(o?)dvips %d -o %f " TeX-run-dvips nil t :help "Convert DVI file to PostScript")
     ("Dvipdfmx" "dvipdfmx %d" TeX-run-dvipdfmx nil t :help "Convert DVI file to PDF with dvipdfmx")
     ("Ps2pdf" "ps2pdf %f" TeX-run-ps2pdf nil t :help "Convert PostScript file to PDF")
     ("Index" "makeindex %s" TeX-run-index nil t :help "Run makeindex to create index file")
     ("Xindy" "texindy %s" TeX-run-command nil t :help "Run xindy to create index file")
     ("Check" "lacheck %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for correctness")
     ("ChkTeX" "chktex -v6 %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for common mistakes")
     ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document")
     ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files")
     ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files")
     ("Other" "" TeX-run-command t t :help "Run an arbitrary command")
     ("Compile" "make" TeX-run-compile nil t))))
 '(TeX-electric-math (quote ("$" . "$")))
 '(abbrev-mode t t)
 '(ac-ignore-case nil)
 '(ac-use-fuzzy nil)
 '(ac-use-menu-map t)
 '(backup-directory-alist (quote ((".*" . "~/.backup"))))
 '(bbdb-message-all-addresses t)
 '(bbdb-mua-pop-up-window-size 2)
 '(bbdb-pop-up-layout (quote one-line))
 '(bbdb-pop-up-window-size 2)
 '(blink-matching-paren nil)
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
    (company-bbdb company-nxml company-css company-eclim company-semantic company-clang company-xcode company-cmake company-capf company-files
                  (company-dabbrev-code company-gtags company-etags company-keywords)
                  company-oddmuse)))
 '(company-minimum-prefix-length 2)
 '(company-tooltip-align-annotations t)
 '(compilation-window-height 8)
 '(eldoc-idle-delay 0)
 '(electric-indent-mode t)
 '(electric-layout-mode t)
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-pyvenv elpy-module-highlight-indentation elpy-module-sane-defaults)))
 '(elpy-rpc-backend "jedi")
 '(ensime-default-java-flags (quote ("-Xms1024m" "-Xmx1024m")))
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
 '(ess-swv-plug-into-AUCTeX-p t)
 '(ess-swv-processor (quote knitr))
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
 '(mml-secure-openpgp-encrypt-to-self t)
 '(mml-secure-openpgp-signers (quote ("69ABB985")))
 '(mml2015-encrypt-to-self t)
 '(mml2015-signers (quote ("69ABB985")))
 '(mouse-yank-at-point t)
 '(nrepl-history-file "~/.nrepl-history")
 '(nrepl-popup-stacktraces nil)
 '(nrepl-server-command "lein trampoline repl :headless")
 '(ob-ipython-command "jupyter")
 '(org-adapt-indentation nil)
 '(org-agenda-files (quote ("~/doc/space/music.org")))
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
 '(package-archive-priorities
   (quote
    (("melpa-stable" . 21)
     ("marmalade" . 20)
     ("gnu" . 10)
     ("melpa" . 0))))
 '(package-archives
   (quote
    (("gnu" . "https://elpa.gnu.org/packages/")
     ("marmalade" . "https://marmalade-repo.org/packages/")
     ("melpa-stable" . "https://stable.melpa.org/packages/")
     ("melpa" . "https://melpa.org/packages/"))))
 '(package-selected-packages
   (quote
    (toml-mode racer rust-mode bind-key use-package ruby-electric yaml-mode typopunct swiper stan-mode puppet-mode polymode paredit ob-ipython muse multiple-cursors monroe mmm-mode markdown-mode magit link j-mode hy-mode htmlize git-gutter flycheck-rust find-file-in-repository ess erc-hl-nicks ensime elpy edit-server dictionary connection company-math cargo browse-kill-ring bbdb auctex ag ace-jump-mode)))
 '(racer-cmd "~/.cargo/bin/racer")
 '(racer-rust-src-path "/home/llasram/ws/rust/src/")
 '(ruby-electric-expand-delimiters-list (quote (124)))
 '(safe-local-variable-values
   (quote
    ((org-babel-inline-result-wrap . "$%s$")
     (org-latex-prefer-user-labels . t)
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
 '(use-package-always-ensure t)
 '(user-full-name "Marshall Bockrath-Vandegrift")
 '(user-mail-address "llasram@gmail.com")
 '(warning-suppress-types (quote ((undo discard-info))))
 '(whitespace-action (quote (auto-cleanup)))
 '(whitespace-style
   (quote
    (face trailing lines-tail space-before-tab indentation space-after-tab tab-mark)))
 '(woman-fill-column 80)
 '(woman-fill-frame t)
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
