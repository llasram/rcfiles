;;; 10gnus.el --- Personal initial Gnus setup code

;; Copyright (c) 2006 Marshall T. Vandegrift

;;; Commentary:

;; Loads and configures Gnus and intimately related packages.

(require 'gnus)
(require 'message)
(require 'bbdb)
(bbdb-initialize 'gnus 'message)
(bbdb-insinuate-gnus)
(bbdb-insinuate-message)

(add-hook 'message-mode-hook 'llasram/message-mode-hook)
(defun llasram/message-mode-hook ()
  (setq fill-column 72))
