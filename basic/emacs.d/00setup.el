;;; 00setup.el --- Personal initial setup code

;; Copyright (c) 2006 Marshall T. Vandegrift

;;; Commentary:

;; This file contains miscellaneous startup code which needs to / can
;; run early-on

;; TODO: make this generic
(add-to-list 'load-path "~/.emacs.d/ruby")
(add-to-list 'load-path "~/.emacs.d/elpa")
(add-to-list 'load-path "~/.emacs.d/misc")

;; Typeface
;;(set-face-attribute 'default nil :font "Inconsolata:pixelsize=12")
;;(set-face-attribute 'default nil :font "Inconsolata:pixelsize=18")
;;(set-face-attribute 'default nil :font "Anonymous Pro:pixelsize=11")
;;(set-face-attribute 'default nil :font "Anonymous Pro:pixelsize=12")
;;(set-face-attribute 'default nil :font "Anonymous Pro:pixelsize=18")
;;(set-face-attribute 'default nil :font "ProggySquareTTSZ:pixelsize=16")
;;(set-face-attribute 'default nil :font "ProFontWindows:pixelsize=12")
(set-face-attribute 'default nil :font "-Misc-Fixed-Medium-R-SemiCondensed--13-120-75-75-C-60-ISO10646-1")


;; Packages!  Oh my!
(require 'warnings)
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; Make sure we have font-lock to start with
(require 'font-lock)

;; Just say no to splash screens
(setq inhibit-startup-message t)

;; Abbrevs
(quietly-read-abbrev-file)

;; I like UTF-8
(prefer-coding-system 'utf-8)

;; Disable advanced featres? Bah.
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Some minor modes I usually enjoy
(column-number-mode t)

;; Start a server
;;(require 'gnuserv-compat)
(server-start)

;; Make sure we have TRAMP
(require 'tramp)

;; Load ffap's bindings
(require 'ffap)
(ffap-bindings)

;; Global MMM mode settings
;(require 'mmm-mode)
;(setq mmm-global-mode 'maybe)
;(require 'mmm-sample)

;; Should perhaps have a file for misc auto-mode-alist foo?
(require 'gnuplot)
(add-to-list 'auto-mode-alist '("\\.plot$" . gnuplot-mode))

;; URL is one useful library
(require 'url)

;; RELAX-NG editing
(require 'rnc-mode)
(add-to-list 'auto-mode-alist '("\\.rnc$" . rnc-mode))

;; SVN
(require 'vc-svn)

;; Setup to get sane flyspell everywhere
(require 'flyspell)

(setq flyspell-mouse-map
      (let ((map (make-sparse-keymap)))
        (define-key map [down-mouse-3] #'flyspell-correct-word)
        map))

(defvar flyspell-maybe-prog-mode-disable-modes
  '(fundamental-mode text-mode latex-mode tex-mode muse-mode planner-mode)
  "Modes in which `flyspell-prog-mode' is less than useful")

(defun flyspell-maybe-prog-mode ()
  "Turn on `flyspell-mode' for desirable text."
  (interactive)
  (if (memq major-mode flyspell-maybe-prog-mode-disable-modes)
      (flyspell-mode 1)
    (flyspell-prog-mode)))

(add-hook 'font-lock-mode-hook 'flyspell-maybe-prog-mode)

;; Snippets for you!
(require 'snippet)

(add-hook 'snippet-cleanup-hook 'llasram/mmm-parse-buffer-maybe)
(defun llasram/mmm-parse-buffer-maybe ()
  (when mmm-mode
    (mmm-parse-buffer)))

(require 'whitespace)
(set-default 'whitespace-check-buffer-indent nil)
(defun llasram/whitespace-mode ()
  (font-lock-mode 1)
  (whitespace-mode 1))

(require 'align)
(require 'uniquify)

(require 'autopair)
(autopair-global-mode t)
(setq autopair-blink nil)

(require 'pos-tip)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
             "~/.emacs.d/elpa/auto-complete-1.4.20110207/dict")
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
(ac-flyspell-workaround)

;; Misc modes
(add-to-list 'auto-mode-alist '("\\.fs$" . forth-mode))
(add-to-list 'auto-mode-alist '("\\.4th$" . forth-mode))
(add-to-list 'auto-mode-alist '("\\.fth$" . forth-mode))
(add-to-list 'auto-mode-alist '("\\.fr$" . forth-mode))
(add-to-list 'auto-mode-alist '("\\.f$" . forth-mode))

;; end 00setup.el
