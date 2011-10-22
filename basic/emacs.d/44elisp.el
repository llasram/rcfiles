;;; 47elisp.el --- Custom emacs-lisp-mode configuration

(require 'lisp-mode)
(require 'nukneval)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

(add-hook 'emacs-lisp-mode-hook 'llasram/elisp-extra-keys)
(defun llasram/elisp-extra-keys ()
  (define-key emacs-lisp-mode-map "\C-c\C-c" 'comment-region)
  (define-key emacs-lisp-mode-map "\C-m" 'newline-and-indent)
  (define-key emacs-lisp-mode-map "RET" 'newline-and-indent)
  (define-key emacs-lisp-mode-map [backspace] 'generic-hungry-backspace)
  (define-key emacs-lisp-mode-map "\C-h" 'generic-hungry-backspace)
  (define-key emacs-lisp-mode-map [delete] 'generic-hungry-delete)
  (define-key emacs-lisp-mode-map "\C-d" 'generic-hungry-delete)
  (define-key emacs-lisp-mode-map "\C-cc" 'nuke-and-eval)
  (define-key emacs-lisp-mode-map "\C-c\C-d\C-d" 'describe-function))

(add-hook 'emacs-lisp-mode-hook 'llasram/paredit-mode t)
(add-hook 'emacs-lisp-mode-hook 'llasram/whitespace-mode t)
