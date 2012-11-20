;;; muse-platyblog.el --- publish a document for serving by platyblog

(require 'outline)
(require 'muse)
(require 'muse-colors)
(require 'muse-mode)
(require 'muse-blosxom)
(require 'muse-docbook)
(require 'muse-html)
(require 'muse-texinfo)
(require 'muse-wiki)
(require 'muse-xml)
(require 'muse-project)
(require 'muse-publish)
(require 'muse-html)

;; Notice explicitly-marked Muse file.
(add-to-list 'auto-mode-alist '("\\.muse$" . muse-mode))

;; Escape characters slightly more properly
(defvar llasram/muse-html-extra-markup-regexps
  '((100010 "\\(^\\|\\s-+\\)&\\($\\|\\s-+\\)" 0 "\\1&amp;\\2")
    (100020 "\\(^\\|\\s-+\\)<\\($\\|\\s-+\\)" 0 "\\1&lt;\\2")))
(dolist (item llasram/muse-html-extra-markup-regexps)
  (add-to-list 'muse-html-markup-regexps item))

;; Get footnotes right in HTML
(assq-delete-all 'footnote muse-html-markup-functions)
(add-to-list 'muse-html-markup-functions
             '(footnote . llasram/muse-html-markup-footnote))

(defun llasram/muse-html-markup-footnote ()
  (cond
   ((get-text-property (match-beginning 0) 'muse-link)
    nil)
   ((= (muse-line-beginning-position) (match-beginning 0))
    (prog1
        (let ((text (match-string 1)))
          (muse-insert-markup
           (concat "<p class=\"footnote\">"
                   "<a class=\"footnum\" name=\"fn." text
                   "\" id=\"fn." text "\">" text "</a>")))
      (save-excursion
        (save-match-data
          (let* ((beg (goto-char (match-end 0)))
                 (end (and (search-forward "\n\n" nil t)
                           (prog1
                               (copy-marker (match-beginning 0))
                             (goto-char beg)))))
            (while (re-search-forward (concat "^["
                                              muse-regexp-blank
                                              "]+\\([^\n]\\)")
                                      end t)
              (replace-match "\\1" t)))))
      (replace-match "")))
   (t (let ((text (match-string 1)))
        (muse-insert-markup
         (concat "<a class=\"footref\" href=\"#fn." text
                 "\">" text "</a>")))
      (replace-match ""))))


(assq-delete-all 'fn-sep muse-html-markup-strings)
(assq-delete-all 'fn-sep muse-xhtml-markup-strings)

;; Crazy example highlighting...
(require 'htmlize)

(add-to-list 'muse-html-markup-tags
             '("example" t t t llasram/muse-html-example-tag))

(defun llasram/muse-html-example-tag (beg end attrs)
  (let ((mode (intern (or (cdr (assoc "mode" attrs)) "fundamental-mode")))
        (muse-buffer (current-buffer))
        (example-buffer (generate-new-buffer "*muse-example-temp*"))
        (html-buffer nil))
    (unwind-protect
        (let (html-string)
          (copy-to-buffer example-buffer beg end)
          (with-current-buffer example-buffer
            (funcall mode)
            ;;(font-lock-fontify-buffer)
            (setq html-buffer (htmlize-buffer)))
          (with-current-buffer html-buffer
            (setq html-string
                  (buffer-substring-no-properties
                   (progn (goto-char (point-min))
                          (re-search-forward "<pre>\\s-*" nil t))
                   (progn (goto-char (point-max))
                          (re-search-backward "\\s-*</pre>" nil t)))))
          (goto-char beg)
          (delete-region (point) end)
          (insert (muse-markup-text 'begin-example)
                  html-string
                  (muse-markup-text 'end-example))
          (muse-publish-mark-read-only beg (point)))
      (when html-buffer (kill-buffer html-buffer))
      (kill-buffer example-buffer))))

;; RFCs links
(defconst llasram/muse-rfc-link-re "\\[?\\<RFC\\s-*\\([[:digit:]]+\\)\\>\\]?"
  "Regular expression identifying a reference to an RFC.")

(defconst llasram/muse-rfc-highlight-link-re
  "\\<RFC\\s-*\\([[:digit:]]+\\)\\>"
  "Regular expression identifying the link portion of a reference to an RFC.")

(add-to-list 'muse-implicit-link-functions 'llasram/muse-handle-rfc-link)
(defun llasram/muse-handle-rfc-link (&optional string)
  (when (if string
            (string-match llasram/muse-rfc-link-re string)
          (looking-at llasram/muse-rfc-link-re))
    (concat "rfc://" (match-string 1 string))))

(add-to-list 'muse-url-protocols '("rfc://" llasram/muse-browse-rfc-url nil))
(defun llasram/muse-browse-rfc-url (url)
  (when (string-match "\\`rfc://\\([[:digit:]]+\\)" url)
    (fetch-rfc (match-string 1 url))))

(add-to-list 'muse-colors-markup
             `(,llasram/muse-rfc-highlight-link-re
               t muse-colors-implicit-link)
             t)

;; Time for key bindings!
(add-hook 'muse-mode-hook 'llasram/muse-extra-keys)
(defun llasram/muse-extra-keys ()
  (define-key muse-mode-map [mouse-1] 'muse-follow-name-at-mouse)
  (define-key muse-mode-map [mouse-2] 'muse-follow-name-at-mouse-other-window))

;; Time for minor modes!
(add-hook 'muse-mode-hook 'llasram/muse-minor-modes)
(defun llasram/muse-minor-modes ()
  (unicode-smarty-mode 1)
  (footnote-mode 1))

(unless (assoc "platyblog-jekyll" muse-publishing-styles)
  (muse-derive-style "platyblog-jekyll" "xhtml"
                     :link-suffix 'muse-xhtml-extension
                     :header      ""
                     :footer      ""))

(provide 'muse-platyblog)

;;; muse-platyblog.el ends here
