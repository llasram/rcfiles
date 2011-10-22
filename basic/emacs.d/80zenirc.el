;; 80zenirc.el -- Custom ZenIRC configuration

(require 'zenirc)
(require 'zenirc-complete)
(require 'zenirc-fill)
(require 'zenirc-history)

;; List of IRC servers to use, consisting of servername, portnumber,
;; password, nickname, username
(setq zenirc-server-alist 
      '(("irc.freenode.net" 6667 nil "llasram" "llasram")))

;; This is how you want ZenIRC to send confirmations
;; "nil" is no confirmation
;; "t" is confirmation in buffer
;; "'message" is confirmation in echo area
(setq zenirc-send-confirmation t)

;; setting this variable to t will make ZenIRC remove preceding
;; whitespaces before a command
(setq zenirc-delete-preceding-whitespaces t)

;; commandkey in ZenIRC
(setq zenirc-command-char ?/)


;; if ZenIRC should fill incoming lines
(setq zenirc-fill-mode t)

;; if ZenIRC should fill outgoing lines
(setq zenirc-fill-outgoing-mode t)

;; zenirc-wrap-region-static uses zenirc-fill-static columns to the
;; left to display <nick#channel> and the rest to actual messages.
(setq zenirc-fill-region-function 'zenirc-wrap-region-static)
(setq zenirc-fill-static 10)

