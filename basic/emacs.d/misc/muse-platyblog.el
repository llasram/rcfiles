;;; muse-platyblog.el --- publish a document for serving by platyblog

;; Copyright (C) 2006  Marshall T. Vandegrift

;; Author: Marshall T. Vandegrift <llasram@gmail.com>

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:


;;; Contributors:

;; Based largely on `muse-blosxom' by Michael Olson (mwolson AT gnu DOT org).

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Muse Platyblog Publishing
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'muse-project)
(require 'muse-publish)
(require 'muse-html)
(require 'hmac-sha1)
(require 'url)

(defgroup muse-platyblog nil
  "Options controlling the behavior of Muse Platyblog publishing.
See `muse-platyblog' for more information."
  :group 'muse-publish)

(defcustom muse-platyblog-header
  (concat
   "post:\n"
   "  title: <lisp>(muse-publishing-directive \"title\")</lisp>\n"
   "  content: |\n")
  "Header used for publishing Platyblog files.
This may be text or a filename."
  :type 'string
  :group 'muse-platyblog)

(defcustom muse-platyblog-footer
  "  source: |\n"
  "Footer used for publishing Platyblog files.
This may be text or a filename."
  :type 'string
  :group 'muse-platyblog)

(defvar muse-platyblog-password nil
  "Password to use when posting to the platypope.org blog.")

(defvar muse-platyblog-url "http://platypope.org/blog/posts"
  "URI to use when accessing the platypope.org blog")

(defvar muse-platyblog-post-last-id -1
  "ID number of the last created blog post.")

(defvar muse-platyblog-saved-id nil
  "Saved post ID number pulled from muse source.")

;; Fixup hooks

(defun muse-platyblog-indent-content ()
  (goto-char (point-min))
  (insert "    ")
  (while (not (eobp))
    (when (bolp) (insert "    "))
    (forward-line 1)))

(defun muse-platyblog-save-source ()
  (goto-char (point-min))
  (setq muse-platyblog-saved-id
        (when (re-search-forward "^#id +" (point-max) t)
          (prog1 (string-to-number (buffer-substring (point) (point-at-eol)))
            (goto-char (point-at-bol))
            (delete-char (1+ (- (point-at-eol) (point)))))))
  (setq muse-platyblog-saved-source (buffer-string)))

(defun muse-platyblog-insert-source ()
  (goto-char (point-max))
  (save-excursion (insert muse-platyblog-saved-source))
  (setq muse-platyblog-saved-source nil)
  (while (not (eobp))
    (when (bolp) (insert "    "))
    (forward-line 1)))

(defun muse-platyblog-insert-auth ()
  (let ((time (format-time-string "%Y-%m-%dT%H:%M:%S%z")))
    (goto-char (point-min))
    (insert "auth:\n  name: llasram\n  time: '" time "'\n  token: '"
            (encode-hex-string (hmac-sha1 time muse-platyblog-password))
            "'\n")))

(defun muse-platyblog-publish-after ()
  (muse-platyblog-insert-source)
  (muse-platyblog-insert-auth))

;; Enter a new blog entry

;;;###autoload
(defun muse-platyblog-post-new ()
  (interactive)
  (switch-to-buffer (generate-new-buffer "*blog post*"))
  (muse-mode)
  (footnote-mode 1)
  (local-set-key "\C-cpp" 'muse-platyblog-post-publish)
  (local-set-key "\C-c\C-c" 'muse-platyblog-post-publish)
  (goto-char (point-min))
  (insert "#title "))

(defun muse-platyblog-post-publish ()
  (interactive)
  (let ((muse-publishing-current-file "*blog entry*")
        (source-buffer (current-buffer))
        (publish-buffer (generate-new-buffer "*platyblog-temp*"))
        (result-buffer nil))
    (unwind-protect
        (progn
          (copy-to-buffer publish-buffer (point-min) (point-max))
          (with-current-buffer publish-buffer
            (muse-publish-markup-buffer "*blog entry*" "platyblog")
            (let ((url-package-name "muse-platyblog")
                  (url-package-version "1.0")
                  (url-request-method
                   (if muse-platyblog-saved-id "PUT" "POST"))
                  (url-mime-charset-string
                   "utf-8;q=1, iso-8859-1;q=0.5, us-ascii;q=0.5")
                  (url-request-data (buffer-string))
                  (url-request-extra-headers
                   (cons (cons "Content-Type" "application/x-yaml")
                         url-request-extra-headers))
                  (publish-request-url
                   (if muse-platyblog-saved-id
                       (concat muse-platyblog-url "/"
                               (number-to-string muse-platyblog-saved-id))
                     muse-platyblog-url)))
              (setq result-buffer
                    (url-retrieve-synchronously publish-request-url))))
          (with-current-buffer result-buffer
            (goto-char (point-min))
            (unless (re-search-forward "^id: *" (point-max) t)
              (error "URL did not retrieve the entire response"))
            (setq muse-platyblog-post-last-id
                  (string-to-number
                   (buffer-substring-no-properties (point) (point-at-eol)))))
          (kill-buffer source-buffer))
      (when result-buffer (kill-buffer result-buffer))
      (kill-buffer publish-buffer))))

(defun muse-platyblog-post-delete ()
  (interactive)
  (let ((result-buffer nil))
    (unwind-protect
        (save-excursion
          (goto-char (point-min))
          (unless (re-search-forward "^#id *" (point-max) t)
            (error "Could not determine post ID to delete."))
          (let ((post-id
                 (buffer-substring-no-properties (point) (point-at-eol))))
            (when (yes-or-no-p (format "Really delete post #%s? " post-id))
              (let ((source-buffer (current-buffer))
                    (url-package-name "muse-platyblog")
                    (url-package-version "1.0")
                    (url-request-method "DELETE")
                    (url-mime-charset-string
                     "utf-8;q=1, iso-8859-1;q=0.5, us-ascii;q=0.5")
                    (delete-request-url
                     (concat muse-platyblog-url "/" post-id)))
                (setq result-buffer
                (url-retrieve-synchronously delete-request-url))
                (with-current-buffer result-buffer
                (goto-char (point-min))
                (unless (re-search-forward "status: *success" (point-max) t)
                  (error "An error occurred.  Entry may not be deleted."))
                (kill-buffer source-buffer))))))
      (when result-buffer (kill-buffer result-buffer)))))

(defun muse-platyblog-post-get (arg)
  (interactive "P")
  (let ((post-buffer (generate-new-buffer "*blog post*"))
        (result-buffer nil)
        (post-id (if arg (prefix-numeric-value arg)
                   muse-platyblog-post-last-id)))
    (unwind-protect
        (progn
          (let ((url-package-name "muse-platyblog")
                (url-package-version "1.0")
                (url-mime-charset-string
                 "utf-8;q=1, iso-8859-1;q=0.5, us-ascii;q=0.5")
                (post-source-url (concat muse-platyblog-url "/"
                                         (number-to-string post-id)
                                         "?format=source")))
            (setq result-buffer (url-retrieve-synchronously post-source-url)))
          (with-current-buffer result-buffer
            (goto-char (point-min))
            (unless (or (re-search-forward "\r\n\r\n" (point-max) t)
                        (re-search-forward "\n\n" (point-max) t))
              (error "URL did not retrieve the entire response"))
            (copy-to-buffer post-buffer (point) (point-max)))
          (with-current-buffer post-buffer
            (goto-char (point-min))
            (when (re-search-forward "^#id *" (point-max) t)
              (setq muse-platyblog-post-last-id
                  (string-to-number
                   (buffer-substring (point) (point-at-eol)))))
            (re-search-forward "\n\n" (point-max) t)))
      (when result-buffer (kill-buffer result-buffer))
      (switch-to-buffer post-buffer)
      (muse-mode)
      (footnote-mode 1)
      (local-set-key "\C-cpp" 'muse-platyblog-post-publish)
      (local-set-key "\C-cpd" 'muse-platyblog-post-delete)
      (local-set-key "\C-c\C-c" 'muse-platyblog-post-publish))))

;; Key bindings

(global-set-key "\C-cp" nil)
(global-set-key "\C-cpn" 'muse-platyblog-post-new)
(global-set-key "\C-cpg" 'muse-platyblog-post-get)

;; Register the Platyblog Publisher

;;;(setq muse-publishing-styles (cdr muse-publishing-styles))
(unless (assoc "platyblog" muse-publishing-styles)
  (muse-derive-style "platyblog" "xhtml"
                     :link-suffix 'muse-xhtml-extension
                     :header      'muse-platyblog-header
                     :footer      'muse-platyblog-footer
                     :before      'muse-platyblog-save-source
                     :before-end  'muse-platyblog-indent-content
                     :after       'muse-platyblog-publish-after))

(unless (assoc "platyblog-jekyll" muse-publishing-styles)
  (muse-derive-style "platyblog-jekyll" "xhtml"
                     :link-suffix 'muse-xhtml-extension
                     :header      ""
                     :footer      ""))

(provide 'muse-platyblog)

;;; muse-platyblog.el ends here
