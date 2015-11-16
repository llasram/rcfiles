(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(require 'package)
(package-initialize)
(unless (file-exists-p package-user-dir)
  (package-refresh-contents))
(dolist (package '(ace-jump-mode ag autopair browse-kill-ring cider clojure-mode
                   clojure-mode-extra-font-locking company diminish ess
                   find-file-in-repository git-gutter htmlize julia-mode
                   magit markdown-mode mmm-mode muse org paredit puppet-mode
                   scala-mode2 typopunct yasnippet))
  (unless (package-installed-p package)
    (package-install package)))

(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(server-start)

;;
;; Upstream extensions

;; Misc
(require 'assoc)
(require 'saveplace)
(require 'uniquify)

;; Text editing
(require 'flyspell)
(define-key flyspell-mode-map (kbd "M-TAB") nil)
(require 'typopunct)
(add-hook 'markdown-mode-hook 'my/text-editing-setup)
(add-hook 'text-mode-hook 'my/text-editing-setup)
(add-hook 'muse-mode-hook 'my/text-editing-setup)
(defun my/text-editing-setup ()
  (typopunct-mode 1))

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
;; (defadvice yank-pop (around kill-ring-browse-maybe (arg) activate)
;;   "If last action was not a yank, run `browse-kill-ring' instead."
;;   (if (not (eq last-command 'yank))
;;       (browse-kill-ring)
;;     ad-do-it))

;; gnus
(require 'gnus)
(require 'message)
(add-hook 'message-mode-hook 'llasram/message-mode-hook)
(defun llasram/message-mode-hook ()
  (setq fill-column 72))

;; git-gutter
(require 'git-gutter)
(global-git-gutter-mode)

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

;; Diminish after everything else is loaded
(require 'yasnippet)
(require 'paredit)
(require 'eldoc)
(require 'whitespace)
(require 'hideshow)
(require 'magit)

(require 'diminish)
(mapc 'diminish
      '(hs-minor-mode
        abbrev-mode
        eldoc-mode
        paredit-mode
        autopair-mode
        typopunct-mode
        flyspell-mode
        yas-minor-mode
        whitespace-mode
        git-gutter-mode
        ))

;; Mode mapping

(add-to-list 'auto-mode-alist '("\\(?:^\\|/\\)Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\(?:^\\|/\\)Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\(?:^\\|/\\)Guardfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.R$" . ess-mode))
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.rs$" . rust-mode))

;;
;; General configuration

(global-set-key "\C-ch" help-map)
(global-set-key (kbd "C-x C-n") nil)

(global-set-key [backspace] 'generic-hungry-backspace)
(global-set-key "\C-h" 'generic-hungry-backspace)
(global-set-key [delete] 'generic-hungry-delete)
(global-set-key "\C-d" 'generic-hungry-delete)
;; (defadvice autopair-mode
;;   (after my/autopair-ctr-h activate)
;;   (let ((map (aget autopair-emulation-alist t)))
;;     (when map (define-key map (kbd "C-h") 'autopair-backspace))))

(global-set-key "\M-h" 'backward-kill-word)
(global-unset-key "\C-z")
(global-unset-key [insert])
(global-set-key [M-insert] 'overwrite-mode)
(global-set-key "\C-x\C-b" 'switch-to-buffer)

(global-set-key (kbd "C-t") nil)
(global-set-key (kbd "C-t t") 'transpose-chars)
(global-set-key (kbd "C-t C-t") 'transpose-chars)
(global-set-key (kbd "C-t M-t") 'transpose-words)
(global-set-key (kbd "C-t u") 'insert-char)

(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

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

(global-set-key (kbd "C-c g") nil)
(global-set-key (kbd "C-c g g") 'magit-status)
(global-set-key (kbd "C-c g d") 'git-gutter:popup-hunk)
(global-set-key (kbd "C-c g n") 'git-gutter:next-hunk)
(global-set-key (kbd "C-c g p") 'git-gutter:previous-hunk)
(global-set-key (kbd "C-c g r") 'git-gutter:revert-hunk)
(global-set-key (kbd "C-c g s") 'git-gutter:stage-hunk)

(global-set-key (kbd "C-c w") 'woman)
(global-set-key (kbd "C-c m") 'gnus)

(global-set-key (kbd "C-c f") 'font-lock-fontify-buffer)

;; Extra xterm bindings
(eval-after-load "xterm"
  '(progn
     (define-key xterm-function-map "\e[1;9A" [M-up])
     (define-key xterm-function-map "\e[1;9B" [M-down])
     (define-key xterm-function-map "\e[1;9C" [M-right])
     (define-key xterm-function-map "\e[1;9D" [M-left])
     (define-key xterm-function-map "\e[1;9F" [M-end])
     (define-key xterm-function-map "\e[1;9H" [M-home])))

(defun my/coding-on ()
  (font-lock-mode 1)
  (whitespace-mode 1))
(add-hook 'emacs-lisp-mode-hook 'my/coding-on)
(add-hook 'clojure-mode-hook 'my/coding-on)
(add-hook 'c-mode-common-hook 'my/coding-on)
(add-hook 'puppet-mode-hook 'my/coding-on)
(add-hook 'org-mode-hook 'my/coding-on)
(add-hook 'python-mode-hook 'my/coding-on)

(defun my/preserve-selected-window (f &rest args)
  (save-selected-window (apply f args)))

(eval-after-load 'clojure-mode
  '(progn
     (require 'cider)
     (define-key clojure-mode-map "\C-m" 'paredit-newline)))

(defun my/describe-function ()
  (interactive)
  (let ((fn (function-called-at-point)))
    (if fn
      (describe-function fn)
      (command-execute 'describe-function))))

(define-key emacs-lisp-mode-map (kbd "C-c C-k") 'eval-buffer)
(define-key emacs-lisp-mode-map (kbd "C-c C-d C-d") 'my/describe-function)
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
(add-hook 'cider-repl-mode-hook 'my/paredit-mode-on)

;; For... lesser modes
(add-hook 'puppet-mode-hook 'autopair-on)

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
(add-hook 'cidr-repl-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-mode-hook #'eldoc-mode)

(defun my/cider-fit-docs (&rest args)
  (when (get-buffer-window cider-doc-buffer)
    (with-current-buffer cider-doc-buffer
      (tight-fit-window-to-buffer)
      (goto-char (point-min)))))
(advice-add 'cider-doc :after #'my/cider-fit-docs)

(defun my/cider-doc-other-window (f &rest args)
  (save-selected-window (apply f args)))
(advice-add 'cider-doc :around #'my/cider-doc-other-window)

(defun my/cider-load-success-cleanup (&rest args)
  (let ((window (get-buffer-window cider-error-buffer)))
    (when window (delete-window window))))
(advice-add 'cider-load-file :before #'my/cider-load-success-cleanup)

(defun my/cider-restore-git-gutter (&rest args)
  (git-gutter:update-all-windows))
(advice-add 'cider-test-clear-highlights :after #'my/cider-restore-git-gutter)
(advice-add 'cider-test-render-report :after #'my/cider-restore-git-gutter)

(add-hook 'c-mode-common-hook 'my/c-common-sane-defaults)
(defun my/c-common-sane-defaults ()
  (c-toggle-auto-hungry-state t)
  (define-key c-mode-base-map "\C-m" 'c-context-line-break)
  (setq autopair-handle-action-fns
        (list #'autopair-default-handle-action
              #'my/autopair-extra-newlines))
  (autopair-on))

(eval-after-load 'ruby-mode
  '(progn
     (require 'ruby-electric)
     (define-key ruby-mode-map "\C-m" 'ruby-electric-return)))
(defun my/ruby-electric-on () (ruby-electric-mode 1))
(add-hook 'ruby-mode-hook 'my/ruby-electric-on)
(add-hook 'ruby-mode-hook 'my/coding-on)
(add-hook 'ruby-mode-hook 'autopair-on)
;; (defadvice ruby-electric-bar
;;   (around my/ruby-electric-rebar activate)
;;   (if (looking-at (string last-command-event))
;;       (forward-char 1)
;;     ad-do-it))

(eval-after-load 'python
  '(progn
     (define-key python-mode-map "\C-m" 'newline-and-indent)))

(eval-after-load "ess-site"
  '(progn
     (require 'ess-inf)
     (require 'ess-mode)
     (require 'llasram-ess)))
(eval-after-load 'llasram-ess
  '(progn
     (define-key inferior-ess-mode-map (kbd "C-c M-o") 'ess-truncate-buffer)
     (define-key inferior-ess-mode-map (kbd "C-c C-d") 'ess-help)
     (define-key ess-mode-map (kbd "C-c C-d") 'ess-help)
     (define-key ess-mode-map (kbd "C-c C-k") 'ess-load-file)
     (define-key ess-mode-map (kbd "M-TAB") 'ess-complete-object-name)))
(add-hook 'ess-mode-hook 'my/coding-on)
(add-hook 'ess-mode-hook 'autopair-on)
(advice-add 'ess-load-file :around #'my/preserve-selected-window)

(eval-after-load 'org
  '(progn
     (define-key org-mode-map (kbd "M-h") 'backward-kill-word)
     (define-key org-mode-map (kbd "RET") 'org-return-indent)))
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)

(eval-after-load 'octave-mod
  '(progn
     (define-key octave-mode-map (kbd "RET") 'newline-and-indent)))
(add-hook 'octave-mode-hook 'my/coding-on)
(add-hook 'octave-mode-hook 'autopair-on)

(eval-after-load 'rust-mode
  '(progn
     (define-key rust-mode-map (kbd "RET") 'newline-and-indent)))
(add-hook 'rust-mode-hook 'my/coding-on)
(add-hook 'rust-mode-hook 'autopair-on)

(add-hook 'j-mode 'my/coding-on)
(advice-add 'j-console-execute-region :around #'my/preserve-selected-window)
