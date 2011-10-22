;; 35nxml.el -- configuration for nxml-mode

(require 'xml)
(require 'nxml-mode)
(require 'xml-fragment)
(load-library "unichars")
(load-library "xmlunicode")

;; There can be only one... XML mode
(defalias 'xml-mode 'nxml-mode)

(defvar nxml-mode-abbrev-table
  (let (table)
    (define-abbrev-table 'table ())
    table)
  "Abbrev table in use in ruby-mode buffers.")

(add-hook 'nxml-mode-hook 'llasram/nxml-set-abbrev-table)
(defun llasram/nxml-set-abbrev-table ()
  (setq local-abbrev-table nxml-mode-abbrev-table))

(add-hook 'nxml-mode-hook 'xml-fragment-mode-on-maybe)

(add-to-list 'flyspell-prog-text-faces 'nxml-text-face)

(defun nxml-fontify-mode ()
  (nxml-mode)
  (mmm-mode-on)
  (font-lock-fontify-buffer)
  (nxml-fontify-buffer))

;; Destroy!
(setq magic-mode-alist
      '(("%![^V]" . ps-mode)
        ("# xmcd " . conf-unix-mode)))

;; Key bindings
(add-hook 'nxml-mode-hook 'llasram/nxml-extra-keys)
(defun llasram/nxml-extra-keys ()
  (define-key nxml-mode-map "\M-h" 'backward-kill-word)
  (define-key nxml-mode-map "\C-m" 'newline-and-indent))

;; 35nxml.el ends
