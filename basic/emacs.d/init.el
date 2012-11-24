(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(require 'package)
(package-initialize)

(defun my/package-install-maybe (package)
  (when (not (package-installed-p package))
    (package-install package)))

(mapc 'my/package-install-maybe
      '(ac-nrepl autopair clojure-mode find-file-in-repository magit
        markdown-mode muse nrepl paredit pos-tip puppet-mode gnus
        typopunct))

(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(server-start)

;;
;; Upstream extensions

;; Misc
(require 'saveplace)
(require 'uniquify)

;; Text editing
(require 'flyspell)
(require 'typopunct)
(add-hook 'markdown-mode-hook 'my/text-editing-setup)
(add-hook 'text-mode-hook 'my/text-editing-setup)
(add-hook 'muse-mode-hook 'my/text-editing-setup)
(defun my/text-editing-setup ()
  (typopunct-mode 1))

;; Autocomplete
(require 'pos-tip)
(require 'auto-complete-config)
(ac-config-default)
(ac-flyspell-workaround)

;; Autopair
(require 'autopair)
(setq autopair-blink nil)
(defun my/autopair-extra-newlines (action pair pos-before)
  (when (not (eq (point) (1+ pos-before)))
    (cond ((eq 'opening action)
           (save-excursion
             (insert-char ?\n 1)
             (indent-according-to-mode)))
          ((and (eq 'closing action)
                (looking-at (format "\n\\s *%c" last-input-event)))
           (replace-match "")))))

;; browse-kill-ring
(require 'browse-kill-ring)
(defadvice yank-pop (around kill-ring-browse-maybe (arg) activate)
  "If last action was not a yank, run `browse-kill-ring' instead."
  (if (not (eq last-command 'yank))
      (browse-kill-ring)
    ad-do-it))

;; gnus
(require 'gnus)
(require 'message)
(require 'bbdb)
(bbdb-initialize 'gnus 'message)
(bbdb-insinuate-gnus)
(bbdb-insinuate-message)
(add-hook 'message-mode-hook 'llasram/message-mode-hook)
(defun llasram/message-mode-hook ()
  (setq fill-column 72))

;;
;; Local custom extensions

(add-to-list 'load-path "~/.emacs.d/elisp")

(require 'hungry)
(require 'isearch-initial)
(require 'tight-fit)
(require 'llasram-c-style)
(require 'muse-platyblog)
(require 'llasram-clojure-indent)
(require 'flyspell-everywhere)

;; Mode mapping

(add-to-list 'auto-mode-alist '("\\(?:^\\|/\\)Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\(?:^\\|/\\)Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;;
;; General configuration

(global-set-key "\C-ch" help-map)

(global-set-key [backspace] 'generic-hungry-backspace)
(global-set-key "\C-h" 'generic-hungry-backspace)
(global-set-key [delete] 'generic-hungry-delete)
(global-set-key "\C-d" 'generic-hungry-delete)
(defadvice autopair-mode
  (after my/autopair-ctr-h activate)
  (let ((map (aget autopair-emulation-alist t)))
    (when map (define-key map (kbd "C-h") 'autopair-backspace))))

(global-set-key "\M-h" 'backward-kill-word)
(global-unset-key "\C-z")
(global-unset-key [insert])
(global-set-key [M-insert] 'overwrite-mode)
(global-set-key "\C-x\C-b" 'switch-to-buffer)

(global-set-key "\C-t" nil)
(global-set-key "\C-tt" 'transpose-chars)
(global-set-key "\C-t\C-t" 'transpose-chars)
(global-set-key (kbd "C-t u") 'ucs-insert)

(global-unset-key "\M-%")
(global-unset-key (kbd "C-M-%"))
(global-set-key (kbd "C-t r") 'query-replace)
(global-set-key (kbd "C-t C-r") 'query-replace-regexp)
(define-key isearch-mode-map "\C-h" 'isearch-delete-char)
(define-key isearch-mode-map [backspace] 'isearch-delete-char)
(global-set-key "\C-ts" 'isearch-forward-at-point)
(global-set-key "\C-t\C-s" 'isearch-forward-at-point)

(global-set-key (kbd "C-x C-f") 'find-file-in-repository)
(global-set-key (kbd "C-x f") 'find-file)

(global-set-key (kbd "C-c g") 'goto-line)
(global-set-key (kbd "C-c w") 'woman)
(global-set-key (kbd "C-c m") 'gnus)

(defun my/describe-function ()
  (interactive)
  (let ((fn (function-called-at-point)))
    (if fn
      (describe-function fn)
      (command-execute 'describe-function))))

(define-key emacs-lisp-mode-map (kbd "C-c C-k") 'eval-buffer)
(define-key emacs-lisp-mode-map (kbd "C-c C-d") 'my/describe-function)
(define-key emacs-lisp-mode-map "\C-m" 'newline-and-indent)

(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map "\C-h" 'paredit-backward-delete)
     (define-key paredit-mode-map "\M-h" 'paredit-backward-kill-word)))

(defun my/paredit-mode-on () (paredit-mode 1))
(add-hook 'emacs-lisp-mode-hook 'my/paredit-mode-on)
(add-hook 'clojure-mode-hook 'my/paredit-mode-on)
(add-hook 'lisp-mode-hook 'my/paredit-mode-on)
(add-hook 'lisp-interaction-mode-hook 'my/paredit-mode-on)
(add-hook 'nrepl-mode-hook 'my/paredit-mode-on)

(add-hook 'ido-setup-hook 'my/ido-extra-keys)
(defun my/ido-extra-keys ()
  "Add personal keybindings for ido."
  (define-key ido-completion-map "\C-h" 'ido-delete-backward-updir)
  (define-key ido-completion-map "\C-n" 'ido-next-match)
  (define-key ido-completion-map "\C-f" 'ido-next-match)
  (define-key ido-completion-map "\C-p" 'ido-prev-match)
  (define-key ido-completion-map "\C-b" 'ido-prev-match)
  (define-key ido-completion-map " "    'ido-exit-minibuffer))

(eval-after-load 'eldoc
  '(eldoc-add-command
    'paredit-backward-delete
    'paredit-close-round))
(defun my/eldoc-mode-on () (eldoc-mode 1))
(add-hook 'emacs-lisp-mode-hook 'my/eldoc-mode-on)
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)

(defadvice nrepl-emit-into-color-buffer
  (after my/nrepl-fit-stacktrace (buffer value) activate)
  (tight-fit-window-to-buffer (get-buffer-window buffer)))

(defadvice nrepl-emit-into-popup-buffer
  (around my/nrepl-fit-docs-etc (buffer value) activate)
  (with-current-buffer buffer
    (goto-char (point-max))
    ad-do-it
    (tight-fit-window-to-buffer)))

(defadvice nrepl-doc
  (around my/nrepl-doc-other-window activate)
  (save-selected-window ad-do-it))

(defadvice nrepl-load-file
  (before my/nrepl-load-success-cleanup activate)
  (let ((window (get-buffer-window nrepl-error-buffer)))
    (when window (delete-window window))))

(defun my/whitespace-mode-on () (font-lock-mode 1) (whitespace-mode 1))
(add-hook 'emacs-lisp-mode-hook 'my/whitespace-mode-on)
(add-hook 'clojure-mode-hook 'my/whitespace-mode-on)
(add-hook 'c-mode-common-hook 'my/whitespace-mode-on)

(add-hook 'c-mode-common-hook 'my/c-common-sane-defaults)
(defun my/c-common-sane-defaults ()
  (c-toggle-auto-hungry-state t)
  (define-key c-mode-base-map "\C-m" 'c-context-line-break)
  (setq autopair-handle-action-fns
        (list #'autopair-default-handle-action
              #'my/autopair-extra-newlines))
  (autopair-on))

(add-hook 'java-mode-hook 'my/java-mode-style)
(defun my/java-mode-style ()
  (c-set-style "llasram/java"))
