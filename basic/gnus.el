;; Gnus configuration

(setq gnus-select-method
      '(nnfolder ""))

(setq gnus-secondary-select-methods
      '((nntp "news.gmane.org")))

(setq gnus-message-archive-method
      '(nnfolder ""))

(setq mail-sources
      '((file)))

(setq nnmail-split-methods
      '(("inbox" "")))
