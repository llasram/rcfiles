;; Gnus configuration

(setq gnus-select-method
      '(nnml ""))

(setq gnus-secondary-select-methods
      '(;;(nntp "news.gmane.org")
        (nnimap "imap.gmail.com")
        (nnimap "cybraics"
                (nnimap-address "mex06.emailsrvr.com")
                (nnimap-stream starttls))))

(setq gnus-message-archive-method
      '(nnml ""))

(setq mail-sources
      '((file)))

(setq nnmail-split-methods
      '(("inbox" "")))

(setq gnus-posting-styles
      '((".*")
        ("nnimap\\+cybraics:.*"
         (from "Marshall Bockrath-Vandegrift <llasram@cybraics.com>")
         (gcc "nnimap+cybraics:INBOX \"nnimap+cybraics:Sent Items\"")
         ("X-Message-SMTP-Method" "smtp smtp.emailsrvr.com 587")
         (signature-file "~/.signature.cybraics"))))

(setq gnus-message-archive-group
      '(("nnimap\\+cybraics:.*"
         "nnimap+cybraics:INBOX \"nnimap+cybraics:Sent Items\"")
        (".*" "nnml:sent")))
