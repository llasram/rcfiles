;;; 46python.el --- Custom python-mode configuration

(require 'python)
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(add-to-list 'auto-mode-alist '("\\(^\\|/\\)SCons" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(require 'pyrex)
(add-to-list 'auto-mode-alist '("\\.pyx$" . pyrex-mode))
(add-to-list 'auto-mode-alist '("\\.pxd$" . pyrex-mode))
(add-to-list 'auto-mode-alist '("\\.pxi$" . pyrex-mode))

(define-key python-mode-map [backspace] 'python-backspace)
(define-key python-mode-map "\C-h" 'python-backspace)
(define-key python-mode-map "\C-m" 'newline-and-indent)

(add-hook 'python-mode-hook 'llasram/whitespace-mode t)
