;;; init -- Personal initialization

;;; Commentary:

;;; Code:

(eval-and-compile
  (setq custom-file "~/.emacs.d/custom.el")
  (load custom-file))

(require 'package)
(package-initialize)
(let ((have-use-package (package-installed-p 'use-package))
      (have-bind-key (package-installed-p 'bind-key)))
  (unless (and have-use-package have-bind-key)
    (package-refresh-contents)
    (unless have-use-package (package-install 'use-package))
    (unless have-bind-key (package-install 'bind-key))))
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(use-package server
  :if window-system
  :commands server-start
  :init (add-hook 'after-init-hook 'server-start t))

(use-package flyspell
  :commands flyspell-mode flyspell-prog-mode
            turn-on-flyspell turn-off-flyspell
  :diminish flyspell-mode
  :bind (:map flyspell-mode-map
              ("M-TAB" . nil)))

(use-package flyspell-everywhere
  :ensure nil
  :load-path "elisp")

(use-package typopunct
  :commands typopunct-mode turn-on-typopunct
  :functions typopunct-insert-single-quotation-mark
  :diminish typopunct-mode
  :init (add-hook 'text-mode-hook 'turn-on-typopunct)
  :config (defun turn-on-typopunct ()
            (typopunct-mode 1)))

(use-package simple
  :ensure nil
  :commands auto-fill-mode turn-on-auto-fill
  :diminish auto-fill-function
  :init (add-hook 'text-mode-hook 'turn-on-auto-fill))

(use-package markdown-mode
  :mode "\\.md\\'" "\\.markdown\\'"
  :init
  (add-hook 'markdown-mode-hook 'turn-on-auto-fill)
  (add-hook 'markdown-mode-hook 'turn-on-typopunct))

(use-package browse-kill-ring
  :commands browse-kill-ring
  :functions yank-pop--browse-kill-ring
  :init
  (defun yank-pop--browse-kill-ring (f &rest args)
    "If last action was not a yank, run `browse-kill-ring' instead."
    (if (not (eq last-command 'yank))
        (browse-kill-ring)
      (apply f args)))
  (advice-add 'yank-pop :around #'yank-pop--browse-kill-ring))

(use-package gnus
  :bind (("C-c m" . gnus)
         :map gnus-summary-mode-map
         (";" . bbdb-mua-edit-field))
  :functions gnus-topic-mode gnus-group-list-all-groups
  :config
  (bbdb-initialize 'gnus 'message)
  (bbdb-mua-auto-update-init 'gnus 'message)

  (defun my/gnus-started-hook ()
    (gnus-topic-mode 1)
    (gnus-group-list-all-groups))
  (add-hook 'gnus-started-hook 'my/gnus-started-hook)

  (defun my/message-mode-hook ()
    "Setup buffer for mode."
    (setq fill-column 72))
  (add-hook 'message-mode-hook 'my/message-mode-hook))

(use-package bbdb
  :pin melpa
  :commands bbdb-initialize bbdb-mua-auto-update-init)

(use-package git-gutter
  :demand t
  :diminish git-gutter-mode
  :bind (("C-c g d" . git-gutter:popup-hunk)
         ("C-c g n" . git-gutter:next-hunk)
         ("C-c g p" . git-gutter:previous-hunk)
         ("C-c g r" . git-gutter:revert-hunk)
         ("C-c g s" . git-gutter:stage-hunk))
  :functions global-git-gutter-mode
  :config
  (add-hook 'git-gutter:update-hooks 'magit-revert-buffer-hook)
  (global-git-gutter-mode))

(use-package magit
  :bind (("C-c g g" . magit-status)))

(use-package hungry
  :ensure nil
  :load-path "elisp"
  :commands generic-hungry-delete-advice
  :functions generic-hungry-code-at-point-p
  :bind (([backspace] . generic-hungry-backspace)
         ("C-h" . generic-hungry-backspace)
         ([delete] . generic-hungry-delete)
         ("C-d" . generic-hungry-delete)))

(use-package paredit
  :commands paredit-mode turn-on-paredit
  :functions paredit-backward-delete--hungry paredit-forward-delete--hungry
  :diminish paredit-mode
  :bind (:map paredit-mode-map
         ("C-h" . paredit-backward-delete)
         ("M-h" . paredit-backward-kill-word)
         ("M-{" . paredit-wrap-curly)
         ("M-[" . paredit-wrap-square))
  :config
  (defun turn-on-paredit () (paredit-mode 1))
  (generic-hungry-delete-advice paredit-backward-delete skip-chars-backward)
  (generic-hungry-delete-advice paredit-forward-delete skip-chars-forward))

(use-package font-lock
  :ensure nil
  :commands turn-on-font-lock)

(use-package whitespace
  :diminish whitespace-mode
  :commands turn-on-whitespace-mode
  :config (defun turn-on-whitespace-mode () (whitespace-mode 1)))

(use-package tight-fit
  :ensure nil
  :load-path "elisp"
  :commands tight-fit-window-to-buffer)

(use-package isearch-initial :ensure nil :load-path "elisp")
(use-package llasram-c-style :ensure nil :load-path "elisp")
(use-package llasram-misc :ensure nil :load-path "elisp")

(use-package elec-pair
  :commands electric-pair-mode
  :functions electric-pair-post-self-insert-function--single
  :init (add-hook 'after-init-hook 'electric-pair-mode)
  :config
  (let ((item (cdadr electric-pair-mode-map)))
    (define-key electric-pair-mode-map (kbd "C-h") item)
    (define-key electric-pair-mode-map (kbd "C-d") item))

  (defun electric-pair-post-self-insert-function--single (f &rest args)
    "If current command is an electric brace command, do nothing."
    (unless (eq this-command 'LaTeX-insert-left-brace)
      (apply f args)))
  (advice-add 'electric-pair-post-self-insert-function :around
              #'electric-pair-post-self-insert-function--single))

(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :functions typopunct-insert-single-quotation-mark--texmathp
             TeX-command-default--maybe-compile
             LaTeX-common-initialization--electric-pair
  :config
  (defun typopunct-insert-single-quotation-mark--texmathp (f &rest args)
    (if (and (or (eq major-mode 'latex-mode)
                 (eq major-mode 'tex-mode))
             (texmathp))
        (insert ?\')
      (apply f args)))
  (advice-add 'typopunct-insert-single-quotation-mark :around
              #'typopunct-insert-single-quotation-mark--texmathp)

  (defun TeX-command-default--maybe-compile (f &rest args)
    (if (file-exists-p "Makefile")
        "Compile"
      (apply f args)))
  (advice-add 'TeX-command-default :around
              #'TeX-command-default--maybe-compile)

  (defun LaTeX-common-initialization--electric-pair ()
    "Re-enable electric-pair-mode."
    (setq-local electric-pair-mode t))
  (advice-add 'LaTeX-common-initialization :after
              #'LaTeX-common-initialization--electric-pair)

  (add-hook 'TeX-mode-hook 'turn-on-font-lock)
  (add-hook 'TeX-mode-hook 'turn-on-whitespace-mode))

(use-package abbrev
  :ensure nil
  :diminish abbrev-mode
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))

(use-package hideshow
  :diminish hs-minor-mode)

(use-package company
  :diminish company-mode
  :commands global-company-mode
  :init (add-hook 'after-init-hook 'global-company-mode)
  :bind (:map company-active-map
         ("C-h" . nil)
         ("C-d" . company-show-doc-buffer)))

;;
;; Local custom extensions

(add-to-list 'load-path "~/.emacs.d/elisp")

;; Diminish after everything else is loaded
(require 'eldoc)
(require 'hideshow)

(mapc 'diminish
      '(eldoc-mode
        ))

;; Mode mapping

(add-to-list 'auto-mode-alist '("\\(?:^\\|/\\)Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\(?:^\\|/\\)Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\(?:^\\|/\\)Guardfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))
(add-to-list 'auto-mode-alist '("\\.R$" . ess-mode))


;;
;; General configuration

(global-set-key "\C-ch" help-map)
(global-set-key (kbd "C-x C-n") nil)

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

(global-set-key (kbd "C-c w") 'woman)

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

(add-hook 'after-init-hook #'global-flycheck-mode)
(defun my/disable-flycheck-mode ()
  "Disable flycheck mode."
  (flycheck-mode -1))

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

(defun my/describe-function ()
  "Personal variant of `describe-function'."
  (interactive)
  (let ((fn (function-called-at-point)))
    (if fn
      (describe-function fn)
      (command-execute 'describe-function))))

(define-key emacs-lisp-mode-map (kbd "C-c C-k") 'eval-buffer)
(define-key emacs-lisp-mode-map (kbd "C-c C-d") 'my/describe-function)
(define-key emacs-lisp-mode-map "\C-m" 'newline-and-indent)
(add-hook 'emacs-lisp-mode-hook 'my/coding-on)
(add-hook 'emacs-lisp-mode-hook 'my/eldoc-mode-on)

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
     (define-key ess-mode-map (kbd "C-c h") nil)
     (define-key ess-help-mode-map (kbd "C-c h") nil)
     (define-key ess-noweb-minor-mode-map (kbd "C-c h") nil)
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

(eval-after-load 'ensime-mode
  '(progn
     (define-key ensime-mode-map (kbd "M-n") nil)
     (define-key ensime-mode-map (kbd "C-c ! n") 'ensime-forward-note)
     (define-key ensime-mode-map (kbd "M-p") nil)
     (define-key ensime-mode-map (kbd "C-c ! p") 'ensime-backward-note)))
(eval-after-load 'cc-mode
  '(progn
     (define-key java-mode-map (kbd "M-,") 'ensime-pop-find-definition-stack)))
(add-hook 'scala-mode-hook 'my/coding-on)
(add-hook 'scala-mode-hook 'ensime-mode)
(add-hook 'scala-mode-hook 'my/disable-flycheck-mode)
(defun my/scala-whitespace ()
  "Set whitespace to Scala style maximum 99 columns."
  (setq-local whitespace-line-column 99))
(add-hook 'scala-mode-hook 'my/scala-whitespace)
(add-hook 'c-mode-common-hook 'my/scala-whitespace)



(defadvice TeX-insert-dollar
    (around skip-close-math activate)
  "If point is before the end of a math section, skip it."
  (if (cond ((not (texmathp))
             t)
            ((and (eq (preceding-char) ?\$)
                  (eq (following-char) ?\$))
             (backward-char)
             (delete-char 2)
             (insert "\\[\\]")
             (backward-char 2)
             nil)
            ((and (not (eq (preceding-char) ?\$))
                  (eq (following-char) ?\$))
             (forward-char)
             nil)
            ((< (point) (+ 2 (point-min)))
             t)
            ((or (and (not (string= (buffer-substring (- (point) 2) (point)) "\\["))
                      (string= (buffer-substring (point) (+ (point) 2)) "\\]"))
                 (and (not (string= (buffer-substring (- (point) 2) (point)) "\\("))
                      (string= (buffer-substring (point) (+ (point) 2)) "\\)")))
             (forward-char 2)
             nil)
            (t t))
      ad-do-it))

(provide 'init)

;;; init.el ends here
