;; Gnus configuration

(setq gnus-select-method
      '(nnfolder ""))

(setq gnus-secondary-select-methods
      '((nntp "news.gmane.org")
        (nnimap "imap.gmail.com")))

(setq gnus-message-archive-method
      '(nnfolder ""))

(setq mail-sources
      '((file)))

(setq nnmail-split-methods
      '(("inbox" "")))
