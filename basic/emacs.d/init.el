;;; init -- Personal initialization

;;; Commentary:

;;; Code:

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(require 'package)
(package-initialize)
(unless (file-exists-p package-user-dir)
  (package-refresh-contents))
(dolist (package '(ace-jump-mode ag browse-kill-ring cider clojure-mode
                   clojure-mode-extra-font-locking company diminish ess
                   find-file-in-repository git-gutter htmlize julia-mode
                   magit markdown-mode mmm-mode muse paredit puppet-mode
                   scala-mode2 typopunct flycheck erc-hl-nicks yaml-mode
                   toml-mode company-math org-plus-contrib racer rust-mode
                   stan-mode elpy))
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
  "Enable minor modes etc for text-editing."
  (typopunct-mode 1))

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
(add-hook 'message-mode-hook 'llasram/message-mode-hook)
(defun llasram/message-mode-hook ()
  "Setup buffer for mode."
  (setq fill-column 72))

;; git-gutter
(require 'git-gutter)
(global-git-gutter-mode)
(add-hook 'git-gutter:update-hooks 'magit-revert-buffer-hook)

;;
;; Local custom extensions

(add-to-list 'load-path "~/.emacs.d/elisp")

(require 'llasram-autoloads)
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
(require 'company)

(require 'diminish)
(mapc 'diminish
      '(hs-minor-mode
        abbrev-mode
        eldoc-mode
        paredit-mode
        typopunct-mode
        flyspell-mode
        yas-minor-mode
        whitespace-mode
        git-gutter-mode
        company-mode
        global-company-mode
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
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))

;;
;; General configuration

(global-set-key "\C-ch" help-map)
(global-set-key (kbd "C-x C-n") nil)

(global-set-key [backspace] 'generic-hungry-backspace)
(global-set-key "\C-h" 'generic-hungry-backspace)
(global-set-key [delete] 'generic-hungry-delete)
(global-set-key "\C-d" 'generic-hungry-delete)

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
  "Enable minor modes etc for all code-editing buffers."
  (font-lock-mode 1)
  (whitespace-mode 1))
(add-hook 'c-mode-common-hook 'my/coding-on)
(add-hook 'puppet-mode-hook 'my/coding-on)
(add-hook 'org-mode-hook 'my/coding-on)
(add-hook 'python-mode-hook 'my/coding-on)

(defun my/paredit-mode-on ()
  "Force-enable `paredit-mode'."
  (paredit-mode 1))
(add-hook 'emacs-lisp-mode-hook 'my/paredit-mode-on)
(add-hook 'lisp-mode-hook 'my/paredit-mode-on)
(add-hook 'lisp-interaction-mode-hook 'my/paredit-mode-on)

(eval-after-load 'eldoc
  '(eldoc-add-command
    'paredit-backward-delete
    'paredit-close-round
    'electric-pair-delete-pair))
(defun my/eldoc-mode-on ()
  "Force-enable `eldoc-mode'."
  (eldoc-mode 1))

(defun my/preserve-selected-window (f &rest args)
  "Function version of `save-selected-window'.
Argument F is a function to invoke and optional ARGS any
arguments to `apply' that function to."
  (save-selected-window (apply f args)))

(defun my/comint-empty-buffer ()
  "Truncate a comint buffer to empty."
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(eval-after-load 'comint
  '(progn
     (define-key comint-mode-map (kbd "C-c o") 'my/comint-empty-buffer)
     (define-key comint-mode-map (kbd "C-c r")
       'comint-history-isearch-backward)))

(eval-after-load 'clojure-mode
  '(progn
     (require 'cider)
     (define-key clojure-mode-map "\C-m" 'paredit-newline)
     (define-key cider-repl-mode-map (kbd "C-c o") 'cider-repl-clear-buffer)))
(add-hook 'clojure-mode-hook 'my/coding-on)
(add-hook 'clojure-mode-hook 'my/paredit-mode-on)
(add-hook 'cider-repl-mode-hook 'my/paredit-mode-on)
(add-hook 'cidr-repl-mode-hook 'my/eldoc-mode-on)
(add-hook 'cider-mode-hook 'my/eldoc-mode-on)

(defun my/describe-function ()
  "Personal variant of `describe-function'."
  (interactive)
  (let ((fn (function-called-at-point)))
    (if fn
      (describe-function fn)
      (command-execute 'describe-function))))

(define-key emacs-lisp-mode-map (kbd "C-c C-k") 'eval-buffer)
(define-key emacs-lisp-mode-map (kbd "C-c C-d C-d") 'my/describe-function)
(define-key emacs-lisp-mode-map "\C-m" 'newline-and-indent)
(add-hook 'emacs-lisp-mode-hook 'my/coding-on)
(add-hook 'emacs-lisp-mode-hook 'my/eldoc-mode-on)

(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map "\C-h" 'paredit-backward-delete)
     (define-key paredit-mode-map "\M-h" 'paredit-backward-kill-word)
     (define-key paredit-mode-map "\M-{" 'paredit-wrap-curly)
     (define-key paredit-mode-map "\M-[" 'paredit-wrap-square)))

;; For... lesser modes
(add-hook 'ido-setup-hook 'my/ido-extra-keys)
(defun my/ido-extra-keys ()
  "Add personal keybindings for ido."
  (define-key ido-completion-map "\C-h" 'ido-delete-backward-updir)
  (define-key ido-completion-map "\C-n" 'ido-next-match)
  (define-key ido-completion-map "\C-f" 'ido-next-match)
  (define-key ido-completion-map "\C-p" 'ido-prev-match)
  (define-key ido-completion-map "\C-b" 'ido-prev-match)
  (define-key ido-completion-map " "    'ido-exit-minibuffer))

(defun my/cider-fit-docs (&rest args)
  "Advice function for `cider-doc' to fit window to documentation.
ARGS are as for `cider-doc'."
  (when (get-buffer-window cider-doc-buffer)
    (with-current-buffer cider-doc-buffer
      (tight-fit-window-to-buffer)
      (goto-char (point-min)))))

(defun my/cider-load-success-cleanup (&rest args)
  "Advice function for `cider-load-file' to clear errors on successful load.
ARGS are as for `cider-load-file'."
  (let ((window (get-buffer-window cider-error-buffer)))
    (when window (delete-window window))))

(advice-add 'cider-load-file :before #'my/cider-load-success-cleanup)
(advice-add 'cider-doc :after #'my/cider-fit-docs)
(advice-add 'cider-doc :around #'my/preserve-selected-window)

(defun my/cider-restore-git-gutter (&rest args)
  "Advice function for updating the git gutter.
ARGS are as per the arguments to the advised functions."
  (git-gutter:update-all-windows))
(advice-add 'cider-test-clear-highlights :after #'my/cider-restore-git-gutter)
(advice-add 'cider-test-render-report :after #'my/cider-restore-git-gutter)

(add-hook 'c-mode-common-hook 'my/c-common-sane-defaults)
(defun my/c-common-sane-defaults ()
  "Common mode hook for C modes."
  (c-toggle-auto-hungry-state t)
  (define-key c-mode-base-map "\C-m" 'c-context-line-break))

(eval-after-load 'ruby-mode
  '(progn
     (require 'ruby-electric)
     (define-key ruby-mode-map "\C-m" 'ruby-electric-return)))
(defun my/ruby-electric-on ()
  "Force on Ruby electric key bindings."
  (ruby-electric-mode 1))
(add-hook 'ruby-mode-hook 'my/ruby-electric-on)
(add-hook 'ruby-mode-hook 'my/coding-on)

(eval-after-load 'python
  '(progn
     (define-key python-mode-map "\C-m" 'newline-and-indent)))
(elpy-enable)

(eval-after-load "ess-site"
  '(progn
     (require 'ess-inf)
     (require 'ess-mode)
     (require 'ess-bugs-d)
     (require 'ess-jags-d)
     (require 'llasram-ess)))
(eval-after-load 'llasram-ess
  '(progn
     (define-key inferior-ess-mode-map (kbd "C-c C-d") 'ess-help)
     (define-key inferior-ess-mode-map (kbd "_") 'self-insert-command)
     (define-key ess-mode-map (kbd "C-c C-d") 'ess-help)
     (define-key ess-mode-map (kbd "C-c C-k") 'ess-load-file)
     (define-key ess-mode-map (kbd "M-TAB") 'ess-complete-object-name)
     (define-key ess-mode-map (kbd "_") 'self-insert-command)
     (define-key ess-bugs-mode-map (kbd "_") 'self-insert-command)))
(add-hook 'ess-mode-hook 'my/coding-on)
(advice-add 'ess-load-file :around #'my/preserve-selected-window)
(advice-add 'ess-help :around #'my/preserve-selected-window)
(require 'ess-site) ;; hacky, but easy

(eval-after-load 'org
  '(progn
     (define-key org-mode-map (kbd "M-h") 'backward-kill-word)
     (define-key org-mode-map (kbd "RET") 'org-return-indent)))
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
(defun my/run-write-file-functions (&rest args)
  "Run the `write-file-functions' hook, ignoring ARGS."
  (run-hooks 'write-file-functions))
(advice-add 'org-edit-src-save :before #'my/run-write-file-functions)

(defun my/matlab-electric ()
  "Setup electric rules for MATLAB code."
  (setq-local electric-indent-chars (cons ?\; electric-indent-chars))
  (setq-local electric-layout-rules '((?\; . after)))
  (setq-local electric-pair-pairs '((?\; . after))))

(eval-after-load 'matlab-mode
  '(progn
     (define-key matlab-mode-map (kbd "C-h") nil)
     (define-key matlab-mode-map (kbd "C-c h") nil)
     (define-key matlab-mode-map
       (kbd "C-c C-d") 'matlab-view-current-word-doc-in-another-buffer)
     (define-key matlab-mode-map
       (kbd "M-.") 'matlab-jump-to-definition-of-word-at-cursor)
     (define-key matlab-shell-mode-map (kbd "C-h") nil)
     (define-key matlab-shell-mode-map (kbd "C-c h") nil)
     (define-key matlab-shell-mode-map (kbd "TAB") 'company-complete)
     (define-key matlab-shell-mode-map
       (kbd "C-c C-d") 'matlab-view-current-word-doc-in-another-buffer)
     (define-key matlab-shell-mode-map
       (kbd "M-.") 'matlab-jump-to-definition-of-word-at-cursor)
     (defun matlab-do-functions-have-end-p () t)))
(add-hook 'matlab-mode-hook 'my/coding-on)
(add-hook 'matlab-mode-hook 'my/eldoc-mode-on)
(add-hook 'matlab-mode-hook 'my/matlab-electric)
(add-hook 'matlab-shell-mode-hook 'my/eldoc-mode-on)
(advice-add 'matlab-view-current-word-doc-in-another-buffer
            :around #'my/preserve-selected-window)

(eval-after-load 'octave
  '(progn
     (define-key octave-mode-map (kbd "RET") 'newline-and-indent)
     (define-key octave-mode-map (kbd "C-h") nil)
     (define-key octave-mode-map (kbd "C-c C-d") 'octave-help)
     (define-key inferior-octave-mode-map (kbd "C-h") nil)
     (define-key inferior-octave-mode-map (kbd "C-c C-d") 'octave-help)))
(add-hook 'octave-mode-hook 'my/coding-on)
(add-hook 'octave-mode-hook 'my/eldoc-mode-on)

(if (file-accessible-directory-p "~/ws/matlab-mode")
    (progn
      (add-to-list 'load-path "~/ws/matlab-mode")
      (add-to-list 'auto-mode-alist '("\\.m$" . matlab-mode))
      (autoload 'matlab-mode "matlab-mode" "" t)
      (autoload 'matlab-shell "matlab-mode" "" t))
  (add-to-list 'auto-mode-alist '("\\.m$" . octave-mode)))

(eval-after-load 'rust-mode
  '(progn
     (require 'flycheck-rust)
     (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)
     (define-key rust-mode-map (kbd "RET") 'newline-and-indent)))
(add-hook 'rust-mode-hook 'my/coding-on)
(defun my/rust-whitespace ()
  "Set whitespace to Rust style maximum 99 columns."
  (setq-local whitespace-line-column 99))
(add-hook 'rust-mode-hook 'my/rust-whitespace)
(add-hook 'rust-mode-hook 'racer-mode)
(add-hook 'racer-mode-hook 'my/eldoc-mode-on)

(add-hook 'j-mode 'my/coding-on)
(advice-add 'j-console-execute-region :around #'my/preserve-selected-window)

(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "C-h") nil)
     (define-key company-active-map (kbd "C-d") 'company-show-doc-buffer)))
(add-hook 'after-init-hook #'global-company-mode)

(eval-after-load 'elec-pair
  '(progn
     (let ((item (cdadr electric-pair-mode-map)))
       (define-key electric-pair-mode-map (kbd "C-h") item)
       (define-key electric-pair-mode-map (kbd "C-d") item))))
(add-hook 'after-init-hook #'electric-pair-mode)

(add-hook 'after-init-hook #'global-flycheck-mode)

;; erc (non-customizable)
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#clojure" "#leiningen")
        ("mozilla.org" "#rust")
        ("damballa" "#rnd" "#bugfarmers" "#cspfarmers" "#itops"
                    "#threatresearch" "#research" "#watercooler")))

(provide 'init)
;;; init.el ends here
