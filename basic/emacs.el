;;; .emacs.el --- Marshall T. Vandegrift's personal Emacs startup script

;; Copyright (c) 2006 Marshall T. Vandegrift

;;; Commentary:

;; This file contains startup code needed to load the rest of the
;; Emacs configuration from the .emacs.el.d directory, plus the Emacs
;; "customization" system preferences.

;; Customize my world!
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(abbrev-mode t)
 '(asm-comment-char 35)
 '(backup-directory-alist (quote (("." . "~/.backup"))))
 '(blink-cursor-mode nil)
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-generic-args nil)
 '(browse-url-generic-program "conkeror")
 '(browse-url-netscape-program "netscape")
 '(c++-font-lock-extra-types (quote ("\\sw+_t" "\\([iof]\\|str\\)+stream\\(buf\\)?" "ios" "string" "rope" "list" "slist" "deque" "vector" "bit_vector" "set" "multiset" "map" "multimap" "hash\\(_\\(m\\(ap\\|ulti\\(map\\|set\\)\\)\\|set\\)\\)?" "stack" "queue" "priority_queue" "type_info" "iterator" "const_iterator" "reverse_iterator" "const_reverse_iterator" "reference" "const_reference" "[A-Z]\\sw*[a-z]\\sw+")))
 '(c-backslash-column 32)
 '(c-font-lock-extra-types (quote ("FILE" "bfd" "\\sw+_t" "Lisp_Object" "KG[^_]\\sw+" "[A-Z][a-z0-9]*\\(?:_[A-Z][a-z0-9]*\\)*" "\\(?:\\(?:[A-Z]\\|\\(?:[A-Z]+[a-z0-9]+I?\\)+\\)_\\)*\\(?:[A-Z][a-z0-9]*\\)+[a-z0-9]\\(?:[A-Z][a-z0-9]*\\)*\\(?:_\\(?:[euft_]\\|[0-9]+\\)\\)?")))
 '(canlock-password "912d8ae2b64e51f9bfc6d51834a5f74a08aae3d6")
 '(case-fold-search t)
 '(column-number-mode t)
 '(compilation-error-screen-columns t)
 '(compilation-scroll-output nil)
 '(compilation-window-height nil)
 '(compile-auto-highlight nil)
 '(compile-command "make")
 '(cperl-electric-parens t)
 '(cperl-extra-perl-args "-w")
 '(cperl-font-lock t)
 '(cperl-hairy t)
 '(cperl-indent-level 4)
 '(cperl-invalid-face (quote none))
 '(current-language-environment "English")
 '(default-input-method nil)
 '(default-mime-charset (quote utf-8))
 '(delete-selection-mode nil nil (delsel))
 '(dictionary-default-popup-strategy "soundex")
 '(display-buffer-reuse-frames nil)
 '(ebrowse--indentation 2)
 '(ecb-compilation-buffer-names (quote (("*Kill Ring*") ("*Calculator*") ("*vc*") ("*vc-diff*") ("*Apropos*") ("*Backtrace*") ("*Help*") ("*shell*") ("*bsh*") ("*Messages*") ("*compilation*") ("*grep*") ("*Completions*"))))
 '(ecb-compile-window-enlarge-by-select t)
 '(ecb-compile-window-height 5)
 '(ecb-compile-window-temporally-enlarge (quote both))
 '(ecb-compile-window-width (quote edit-window))
 '(ecb-enlarged-compilation-window-max-height (quote best))
 '(ecb-eshell-auto-activate nil)
 '(ecb-layout-name "left7")
 '(ecb-layout-nr 9)
 '(ecb-non-semantic-parsing-function nil)
 '(ecb-options-version "2.32")
 '(ecb-other-window-behavior (quote edit-and-compile))
 '(ecb-other-window-jump-behavior (quote edit-and-compile))
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--C-mouse-1))
 '(ecb-select-edit-window-on-redraw t)
 '(ecb-show-sources-in-directories-buffer (quote ("left7" "left13" "left14" "left15" "right7")))
 '(ecb-source-path (quote (("~/ws/TASMANIA/tasmania" "TASMANIA") ("~/ws/GALAPAGOS/Products" "GALAPAGOS") ("~/ws/PLATYPOPE/platyblog" "platypope.org") "~/.emacs.d" "~" "/")))
 '(ecb-tip-of-the-day nil)
 '(ecb-tree-buffer-style (quote image))
 '(ecb-tree-image-icons-directories (quote ("/usr/share/emacs/site-lisp/ecb/ecb-images/default/height-17" (ecb-directories-buffer-name . "/usr/share/emacs/site-lisp/ecb/ecb-images/directories/height-17") (ecb-sources-buffer-name . "/usr/share/emacs/site-lisp/ecb/ecb-images/sources/height-14_to_21") (ecb-methods-buffer-name . "/usr/share/emacs/site-lisp/ecb/ecb-images/methods/height-14_to_21"))))
 '(ecb-wget-setup (cons "wget" (quote cygwin)))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(erc-fill-column 45)
 '(erc-hide-list (quote ("JOIN" "PART" "QUIT")))
 '(erc-nick "llasram")
 '(erc-prompt ">")
 '(erc-user-full-name "Marshall T. Vandegrift")
 '(eudc-protocol (quote ldap))
 '(eudc-server "atlmaiexcp01.iss.local")
 '(fast-lock-cache-directories (quote ("~/.emacs-flc")))
 '(fast-lock-save-events (quote (kill-buffer kill-emacs)))
 '(fill-column 79)
 '(flyspell-abbrev-p t)
 '(flyspell-use-global-abbrev-table-p t)
 '(flyspell-use-meta-tab nil)
 '(font-lock-support-mode (quote jit-lock-mode))
 '(footnote-section-tag-regexp "Footnotes\\(\\[.\\]\\)?: *")
 '(footnote-use-message-mode nil)
 '(fortune-always-compile t)
 '(fortune-author-line-prefix "        -- ")
 '(fortune-dir "~/fortunes/")
 '(fortune-file "/home/llasram/fortunes/general")
 '(fortune-fill-column 51)
 '(fortune-sigstart "Marshall T. Vandegrift <mvandegrift@iss.net>

")
 '(global-font-lock-mode t nil (font-lock))
 '(gnus-add-to-list t)
 '(gnus-agent nil)
 '(gnus-agent-cache nil)
 '(gnus-article-encrypt-protocol "PGP")
 '(gnus-article-sort-functions (quote (gnus-article-sort-by-date)))
 '(gnus-audio-au-player (executable-find "esdplay"))
 '(gnus-audio-directory nil)
 '(gnus-audio-wav-player (executable-find "esdplay"))
 '(gnus-buttonized-mime-types (quote ("multipart/signed" "multipart/encrypted")))
 '(gnus-convert-image-to-face-command "pngtopnm %s | ppmnorm | pnmscale -width 48 -height 48 | ppmquant %d | pnmtopng")
 '(gnus-gcc-mark-as-read t)
 '(gnus-group-line-format "%M%S%p%P%5y: %(%G%)%l %O
")
 '(gnus-group-mode-hook (quote (gnus-topic-mode)))
 '(gnus-ham-process-destinations (quote (("^[^:]*:mail\\..*$" "nnml:mail.other"))))
 '(gnus-large-newsgroup 500)
 '(gnus-mailing-list-groups "mail\\\\.list\\\\..*")
 '(gnus-message-archive-group "mail.sent")
 '(gnus-message-replyencrypt t)
 '(gnus-message-replysign t)
 '(gnus-picon-databases (quote ("/home/llasram/media/images/picons/" "/usr/share/picons/local/" "/usr/share/picons/")))
 '(gnus-picon-file-types (quote ("xpm" "gif" "xbm" "png")))
 '(gnus-select-article-hook nil)
 '(gnus-spam-newsgroup-contents (quote (("\\(^\\|:\\)mail\\.\\(other$\\|person\\|list\\)" gnus-group-spam-classification-ham) ("\\(^\\|:\\)mail\\.spam$" gnus-group-spam-classification-spam))))
 '(gnus-spam-process-destinations (quote (("^[^:]*:mail\\..*$" "nnml:mail.spam"))))
 '(gnus-spam-process-newsgroups (quote (("\\(^\\|:\\)mail\\.\\(other$\\|person\\|list\\)" (gnus-group-spam-exit-processor-bogofilter)) ("\\(^\\|:\\)mail\\.spam$" (gnus-group-ham-exit-processor-bogofilter)))))
 '(gnus-startup-hook (quote (bbdb-insinuate-gnus)))
 '(gnus-thread-sort-functions (quote (gnus-thread-sort-by-date)))
 '(gnus-treat-body-boundary nil)
 '(gnus-treat-fill-article nil)
 '(gnus-treat-fill-long-lines nil)
 '(gnus-treat-from-picon nil)
 '(gnus-treat-mail-picon nil)
 '(gnus-treat-newsgroups-picon nil)
 '(gnus-treat-unsplit-urls t)
 '(gnus-treat-x-pgp-sig nil)
 '(gnuserv-frame t)
 '(grep-command "grep -EHn -e ")
 '(grep-find-command "find . -type f -print0 | xargs -0 -e grep -EHn -e ")
 '(help-char 26)
 '(help-event-list (quote (help f1)))
 '(icicle-completing-mustmatch-prompt-prefix "=")
 '(icicle-completing-prompt-prefix " ")
 '(icicle-reminder-prompt-flag nil)
 '(ido-decorations (quote ("{" "}" " | " " | ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]")))
 '(ido-default-buffer-method (quote selected-window))
 '(ido-default-file-method (quote selected-window))
 '(ido-enable-tramp-completion t)
 '(ido-enabled (quote (quote both)) t)
 '(ido-everywhere t)
 '(ido-ignore-files (quote ("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "\\`\\.svn/")))
 '(ido-mode (quote both) nil (ido))
 '(ido-show-dot-for-dired nil)
 '(ido-use-filename-at-point (quote guess))
 '(indent-tabs-mode nil)
 '(iswitchb-case t)
 '(iswitchb-default-method (quote samewindow))
 '(iswitchb-define-mode-map-hook (quote (llasram/iswitchb-extra-keys)))
 '(iswitchb-mode nil nil (iswitchb))
 '(kill-whole-line t)
 '(lazy-lock-defer-on-the-fly t)
 '(lcomp-enable t nil (lcomp))
 '(ldap-default-host "atlmaiexcp01.iss.local")
 '(ldap-ldapsearch-args (quote ("")))
 '(lj-music-external-program "xmmscurrent -n")
 '(lj-update-mode-hook (quote (force-flyspell-mode)))
 '(lpr-switches nil)
 '(ls-lisp-dirs-first t)
 '(mail-default-reply-to "mvandegrift@iss.net")
 '(mail-envelope-from nil)
 '(mail-from-style (quote angles))
 '(mail-host-address "iss.net")
 '(mail-self-blind nil)
 '(mail-signature "None")
 '(mail-signature-file "~/.sig")
 '(mail-source-delete-incoming t)
 '(mail-specify-envelope-from nil)
 '(mail-user-agent (quote gnus-user-agent))
 '(makefile-mode-hook (quote (semantic-default-make-setup (lambda nil (setq tab-width 4)))) t)
 '(menu-bar-mode nil)
 '(message-default-headers "")
 '(message-default-mail-headers "")
 '(message-from-style (quote angles))
 '(message-required-headers (quote ((X-Draft-From lambda nil (gnus-inews-make-draft-meta-information "nnfolder+archive:mail.sent" (quote 338))) (optional . References) From)))
 '(message-send-mail-function (quote smtpmail-send-it))
 '(message-sendmail-envelope-from (quote header))
 '(message-sendmail-f-is-evil t)
 '(message-setup-hook (quote (bbdb-insinuate-message)))
 '(message-signature "-Marshall")
 '(message-signature-file nil)
 '(message-signature-setup-hook nil)
 '(message-user-fqdn "epictetus")
 '(mime-charset-coding-system-alist (quote ((x-unknown . undecided) (unknown . undecided) (cp874 . tis-620) (windows-874 . tis-620) (iso-2022-jp-3 . iso-2022-7bit-ss2) (us-ascii . raw-text))))
 '(mm-body-charset-encoding-alist (quote ((utf-16 . base64) (utf-16be . base64) (utf-16le . base64) (utf-8 . quoted-printable) (iso-8859-1 . quoted-printable))))
 '(mm-coding-system-priorities (quote (utf-8 iso-8859-1)))
 '(mm-decrypt-option (quote always))
 '(mm-discouraged-alternatives (quote ("text/html")))
 '(mm-text-html-renderer (quote html2text))
 '(mm-verify-option (quote always))
 '(mml-signencrypt-style-alist (quote (("smime" separate) ("pgp" combined) ("pgpauto" combined) ("pgpmime" combined))))
 '(mmm-submode-decoration-level 2)
 '(mouse-avoidance-mode nil nil (avoid))
 '(mouse-wheel-mode t nil (mwheel))
 '(mouse-yank-at-point t)
 '(muse-html-encoding-default (quote utf-8))
 '(muse-implicit-link-regexp "\\([^[:blank:]
]+\\(?: [^[:blank:]
]+\\)?\\)")
 '(muse-latex-header "\\documentclass[12pt]{article}

\\usepackage[english]{babel}
\\usepackage[latin1]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{hyperref}
\\usepackage[pdftex]{graphicx}

\\topmargin=-0.5in
\\oddsidemargin=0.0in
\\textwidth=6.5in
\\textheight=9.0in

\\newcommand{\\comment}[1]{}

\\begin{document}

\\title{<lisp>(muse-publishing-directive \"title\")</lisp>}
\\date{<lisp>(muse-publishing-directive \"date\")</lisp>}

\\maketitle

<lisp>(and muse-publish-generate-contents
           \"\\\\tableofcontents
\\\\newpage\")</lisp>

")
 '(muse-latex-markup-specials-entire-document (quote ((92 . "\\textbackslash{}") (95 . "\\textunderscore{}") (60 . "\\textless{}") (62 . "\\textgreater{}") (36 . "\\$") (37 . "\\%") (123 . "\\{") (125 . "\\}") (38 . "\\&") (35 . "\\#"))))
 '(muse-latex-markup-strings (quote ((image-with-desc . "\\begin{figure}[htb]
\\centering
\\includegraphics[width=\\textwidth]{%s}
\\end{figure}") (image-link . "\\begin{figure}[htb]
\\centering
\\includegraphics[width=\\textwidth]{%s}
\\end{figure}") (url-with-image . "%% %s
\\includegraphics[width=\\textwidth]{%s}") (url-link . "\\href{%s}{%s}") (internal-link . "\\hyperlink{%s}{%s}") (email-addr . "\\verb|%s|") (emdash . "---") (comment-begin . "\\comment{") (comment-end . "}") (rule . "\\bigskip") (no-break-space . "~") (enddots . "\\ldots{}") (dots . "\\dots{}") (part . "\\part{") (part-end . "}") (chapter . "\\chapter{") (chapter-end . "}") (section . "\\section{") (section-end . "}") (subsection . "\\subsection{") (subsection-end . "}") (subsubsection . "\\subsubsection{") (subsubsection-end . "}") (section-other . "\\paragraph{") (section-other-end . "}") (footnote . "\\footnote{") (footnote-end . "}") (footnotetext . "\\footnotetext[%d]{") (begin-underline . "\\underline{") (end-underline . "}") (begin-literal . "\\texttt{") (end-literal . "}") (begin-emph . "\\emph{") (end-emph . "}") (begin-more-emph . "\\textbf{") (end-more-emph . "}") (begin-most-emph . "\\textbf{\\emph{") (end-most-emph . "}}") (begin-verse . "\\begin{verse}
") (end-verse-line . " \\\\") (verse-space . "~~~~") (end-verse . "
\\end{verse}") (begin-example . "\\begin{quote}
\\begin{verbatim}") (end-example . "\\end{verbatim}
\\end{quote}") (begin-center . "\\begin{center}
") (end-center . "
\\end{center}") (begin-quote . "\\begin{quote}
") (end-quote . "
\\end{quote}") (begin-uli . "\\begin{itemize}
\\item ") (end-uli . "
\\end{itemize}") (begin-oli . "\\begin{enumerate}
\\item ") (end-oli . "
\\end{enumerate}") (begin-ddt . "\\begin{description}
\\item[") (start-dde . "] ") (end-ddt . "\\end{description}"))))
 '(muse-mode-auto-p t)
 '(muse-project-alist (quote (("WikiPlanner" ("~/Plans" :default "TaskPool.muse" :major-mode planner-mode :visit-link planner-visit-link)))))
 '(nnimap-debug t)
 '(nnimap-retrieve-groups-asynchronous nil)
 '(nnimap-split-download-body nil)
 '(nnimap-split-inbox (quote ("INBOX")))
 '(nnmail-cache-accepted-message-ids t)
 '(nnmail-cache-ignore-groups "^mail\\.sent$")
 '(nnmail-crosspost nil)
 '(nnmail-expiry-wait 5)
 '(nnmail-large-newsgroup 100)
 '(nnmail-list-identifiers "\\[.*?\\]")
 '(nnmail-message-id-cache-length 5000)
 '(nnmail-split-methods (quote nnmail-split-fancy))
 '(nnmail-treat-duplicates (quote warn))
 '(nxml-slash-auto-complete-flag t)
 '(pc-select-selection-keys-only t)
 '(pc-selection-mode t nil (pc-select))
 '(pgg-cache-passphrase t)
 '(pgg-default-user-id "795715C4")
 '(pgg-encrypt-for-me t)
 '(pgg-gpg-extra-args nil)
 '(pgg-query-keyserver nil)
 '(planner-annotation-functions (quote (planner-gnus-annotation planner-bbdb-annotation-from-bbdb planner-annotation-from-info planner-annotation-from-file-with-position planner-annotation-from-planner-note planner-annotation-from-planner planner-annotation-from-wiki planner-annotation-from-dired)))
 '(planner-annotation-strip-directory t)
 '(planner-carry-tasks-forward 7)
 '(planner-plan-page-template "* Tasks

* Plan

* Notes


")
 '(planner-show-only-existing nil)
 '(planner-use-other-window nil)
 '(ps-run-x (quote ("gs" "-r72" "-sPAPERSIZE=a4")))
 '(psgml-html-build-new-buffer nil)
 '(read-mail-command (quote gnus))
 '(read-quoted-char-radix 16)
 '(ruby-electric-expand-delimiters-list nil)
 '(safe-local-variable-values (quote ((encoding . utf-8))))
 '(save-place t nil (saveplace))
 '(scroll-bar-mode nil)
 '(scroll-conservatively 0)
 '(scroll-down-aggressively t)
 '(scroll-step 0)
 '(search-exit-option t)
 '(semanticdb-default-save-directory "/home/llasram/.semanticdb")
 '(semanticdb-persistent-path (quote (always)))
 '(semanticdb-system-database-warn-level t)
 '(send-mail-function (quote sendmail-send-it))
 '(show-paren-mode t nil (paren))
 '(smiley-regexp-alist (quote (("\\(:-?)\\)\\W" 1 "smile") ("\\(;-?)\\)\\W" 1 "blink") ("\\(:-]\\)\\W" 1 "forced") ("\\(8-)\\)\\W" 1 "braindamaged") ("\\(:-|\\)\\W" 1 "indifferent") ("\\(:-[/\\]\\)\\W" 1 "wry") ("\\(:-(\\)\\W" 1 "sad") ("\\(:-{\\)\\W" 1 "frown") ("\\((-?:\\)\\W" 0 "reverse-smile"))))
 '(smtp-default-server "atlmaiexcp02.iss.local")
 '(smtp-server "atlmaiexcp02.iss.local")
 '(smtpmail-debug-info nil)
 '(smtpmail-default-smtp-server "atlmaiexcp02.iss.local")
 '(smtpmail-smtp-server "atlmaiexcp02.iss.local")
 '(spam-bogofilter-ham-switch "-Sn")
 '(spam-bogofilter-header "X-Bogosity")
 '(spam-bogofilter-spam-switch "-Ns")
 '(spam-junk-mailgroups (quote ("mail.spam" "spam" "mail.junk" "poste.pourriel")))
 '(spam-mark-ham-unread-before-move-from-spam-group t)
 '(spam-regex-headers-ham (quote ("^X-Spam-Flag: NO" "^X-Bogosity: No")))
 '(spam-regex-headers-spam (quote ("^X-Spam-Flag: YES" "^X-Bogosity: \\(Yes\\|Spam\\)")))
 '(spam-split-group "mail.spam")
 '(spam-use-regex-headers t)
 '(tab-width 8)
 '(temp-buffer-max-height (lambda (buffer) (/ (- (frame-height) 2) 3)))
 '(temp-buffer-resize-mode t nil (help))
 '(text-mode-hook (quote (turn-on-flyspell text-mode-hook-identify)))
 '(tool-bar-mode nil nil (tool-bar))
 '(tooltip-mode nil nil (tooltip))
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(user-full-name "Marshall T. Vandegrift")
 '(user-mail-address "llasram@gmail.com")
 '(vc-cvs-diff-switches nil)
 '(vc-diff-switches nil)
 '(vc-rcs-diff-switches nil)
 '(vc-sccs-diff-switches nil)
 '(whitespace-action (quote (auto-cleanup)))
 '(whitespace-auto-cleanup t)
 '(whitespace-check-indent-whitespace nil)
 '(whitespace-global-mode t)
 '(whitespace-style (quote (tabs trailing space-before-tab space-after-tab tab-mark lines-tail face)))
 '(woman-use-own-frame nil)
 '(x-select-enable-clipboard t))


;; Customize my face!
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width semi-condensed :foundry "misc" :family "fixed"))))
 '(completions-common-part ((t (:inherit default))))
 '(font-lock-comment-face ((((class color) (background light)) (:foreground "Firebrick"))))
 '(highlight-beyond-fill-column-face ((t (:background "red"))))
 '(ido-first-match ((t (:foreground "blue"))))
 '(ido-only-match ((((class color)) (:foreground "firebrick"))))
 '(ido-subdir ((((min-colors 88) (class color)) (:foreground "darkgreen"))))
 '(info-header-node ((t (:inherit info-node))))
 '(info-header-xref ((t (:inherit info-xref))))
 '(info-menu-star ((((class color)) (:foreground "red1"))))
 '(info-node ((t (:foreground "brown" :slant italic :weight bold))))
 '(info-xref ((t (:foreground "magenta4" :weight bold))))
 '(mmm-code-submode-face ((t (:box (:line-width 1 :color "Purple")))))
 '(mmm-comment-submode-face ((t (:foreground "Firebrick" :box (:line-width 1 :color "Firebrick")))))
 '(mmm-default-submode-face ((t (:background "#d0d0ff"))))
 '(mmm-output-submode-face ((t (:box (:line-width 1 :color "#0000a0")))))
 '(muse-link-face ((t (:foreground "blue" :underline "blue" :weight bold))))
 '(planner-id-face ((((class color) (background light)) (:foreground "lightgray"))))
 '(rainbow-delimiters-depth-1-face ((((background light)) (:foreground "dark gray"))))
 '(rainbow-delimiters-depth-2-face ((((background light)) (:foreground "green"))))
 '(rainbow-delimiters-depth-3-face ((((background light)) (:foreground "gold"))))
 '(rainbow-delimiters-depth-4-face ((((background light)) (:foreground "turquoise"))))
 '(rainbow-delimiters-depth-5-face ((((background light)) (:foreground "orange"))))
 '(rainbow-delimiters-depth-6-face ((((background light)) (:foreground "slate blue"))))
 '(rainbow-delimiters-depth-7-face ((((background light)) (:foreground "yellow"))))
 '(rainbow-delimiters-depth-8-face ((((background light)) (:foreground "brown"))))
 '(rainbow-delimiters-depth-9-face ((((background light)) (:foreground "light gray"))))
 '(yaml-tab-face ((((class color)) (:background "red" :inverse-video unspecified)))))

;; Pull in split configuration
(debian-run-directories "~/.emacs.d")
