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

(unbind-key "C-x C-n")
(unbind-key "C-z")
(unbind-key "C-t")
(unbind-key "M-%")
(unbind-key "C-M-%")
(unbind-key [insert])

(bind-key "C-c h" help-map)

(bind-keys ("M-h" . backward-kill-word)
           ([M-insert] . overwrite-mode)
           ("C-x C-b" . switch-to-buffer))

(bind-keys ("C-t t" . transpose-chars)
           ("C-t C-t" . transpose-chars)
           ("C-t M-t" . transpose-words)
           ("C-t u" . insert-char)
           ("C-t r" . query-replace)
           ("C-t C-r" . query-replace-regexp))

(bind-keys :map isearch-mode-map
           ("C-h" . isearch-delete-char)
           ([backspace] . isearch-delete-char))

(use-package server
  :ensure nil
  :if window-system
  :commands server-start
  :init (add-hook 'after-init-hook 'server-start t))

(use-package saveplace
  :ensure nil
  :commands save-place-mode
  :init (add-hook 'after-init-hook 'save-place-mode))

(use-package savehist
  :commands savehist-mode
  :init (add-hook 'after-init-hook 'savehist-mode))

(use-package flyspell
  :ensure nil
  :commands flyspell-mode flyspell-prog-mode
            turn-on-flyspell turn-off-flyspell
  :diminish flyspell-mode
  :bind (:map flyspell-mode-map
         ("M-TAB" . nil)
         ("C-." . nil)))

(use-package flyspell-everywhere
  :ensure nil
  :load-path "elisp"
  :commands turn-on-flyspell-everywhere
  :init (add-hook 'after-init-hook 'turn-on-flyspell-everywhere))

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
  :commands paredit-mode
  :functions paredit-backward-delete--hungry paredit-forward-delete--hungry
  :diminish paredit-mode
  :bind (:map paredit-mode-map
         ("C-h" . paredit-backward-delete)
         ("M-h" . paredit-backward-kill-word)
         ("M-{" . paredit-wrap-curly)
         ("M-[" . paredit-wrap-square))
  :config
  (generic-hungry-delete-advice paredit-backward-delete skip-chars-backward)
  (generic-hungry-delete-advice paredit-forward-delete skip-chars-forward))

(use-package font-lock
  :ensure nil
  :commands turn-on-font-lock
  :bind ("C-c f" . font-lock-fontify-buffer))

(use-package whitespace
  :diminish whitespace-mode
  :commands turn-on-whitespace-mode my/wide-columns
  :config
  (defun turn-on-whitespace-mode () (whitespace-mode 1))
  (defun my/wide-columns () (setq-local whitespace-line-column 99)))

(use-package tight-fit
  :ensure nil
  :load-path "elisp"
  :commands tight-fit-window-to-buffer)

(use-package isearch-initial :ensure nil :load-path "elisp")
(use-package llasram-c-style :ensure nil :load-path "elisp")

(use-package llasram-misc
  :demand t
  :ensure nil
  :load-path "elisp"
  :commands my/preserve-selected-window
            TeX-insert-dollar--skip-close-math)

(use-package elec-pair
  :ensure nil
  :commands electric-pair-mode
  :functions electric-pair-post-self-insert-function--single
  :init (add-hook 'after-init-hook 'electric-pair-mode)
  :config
  (let ((item (cl-cdadr electric-pair-mode-map)))
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
  (advice-add 'TeX-insert-dollar :around #'TeX-insert-dollar--skip-close-math)

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
  :ensure nil
  :diminish hs-minor-mode)

(use-package company
  :diminish company-mode
  :commands global-company-mode
  :init (add-hook 'after-init-hook 'global-company-mode)
  :bind (:map company-active-map
         ("C-h" . nil)
         ("C-d" . company-show-doc-buffer)))

(use-package eldoc
  :ensure nil
  :commands eldoc-mode
  :diminish eldoc-mode
  :config
  (eldoc-add-command
   'paredit-backward-delete
   'paredit-close-round
   'electric-pair-delete-pair))

(use-package ruby-electric
  :commands ruby-electric-mode
  :diminish ruby-electric-mode)

(use-package ruby-mode
  :mode "\\.rb\\'" "\\.gemspec\\'" "\\.rake\\'"
        "\\(?:^\\|/\\)Rakefile\\'"
        "\\(?:^\\|/\\)Gemfile\\'"
        "\\(?:^\\|/\\)Guardfile\\'"
  :interpreter "ruby"
  :config
  (add-hook 'ruby-mode-hook 'ruby-electric-mode)
  (add-hook 'ruby-mode-hook 'turn-on-font-lock)
  (add-hook 'ruby-mode-hook 'turn-on-whitespace-mode))

(use-package elpy
  :commands elpy-enable)

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :bind (:map python-mode-map
         ("RET" . newline-and-indent))
  :config
  (add-hook 'python-mode-hook 'turn-on-font-lock)
  (add-hook 'python-mode-hook 'turn-on-whitespace-mode)
  (elpy-enable))

(use-package puppet-mode
  :pin melpa
  :mode "\\.pp\'"
  :config
  (add-hook 'puppet-mode-hook 'turn-on-font-lock)
  (add-hook 'puppet-mode-hook 'turn-on-whitespace-mode))

(use-package flycheck
  :commands global-flycheck-mode flycheck-mode
  :functions turn-off-flycheck
  :init (add-hook 'after-init-hook 'global-flycheck-mode)
  :config
  (defun turn-off-flycheck () (flycheck-mode -1)))

(use-package toml-mode
  :pin melpa
  :mode "\\.toml\'")

(use-package cargo
  :pin melpa
  :commands cargo-minor-mode
  :diminish cargo-minor-mode)

(use-package flycheck-rust
  :pin melpa
  :commands flycheck-rust-setup)

(use-package racer
  :pin melpa
  :commands racer-mode
  :diminish racer-mode
  :config (add-hook 'racer-mode-hook 'eldoc-mode))

(use-package rust-mode
  :pin melpa
  :mode "\\.rs\'"
  :bind (:map rust-mode-map
         ("RET" . newline-and-indent))
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)
  (add-hook 'rust-mode-hook 'turn-on-font-lock)
  (add-hook 'rust-mode-hook 'my/wide-columns)
  (add-hook 'rust-mode-hook 'turn-on-whitespace-mode)
  (add-hook 'rust-mode-hook 'cargo-minor-mode)
  (add-hook 'rust-mode-hook 'racer-mode))

(use-package elisp-mode
  :ensure nil
  :mode ("\\.el\'" . emacs-lisp-mode)
  :interpreter ("emacs" . emacs-lisp-mode)
  :bind (:map emacs-lisp-mode-map
         ("RET" . newline-and-indent)
         ("C-c C-k" . eval-buffer)
         ("C-c C-d" . my/describe-function))
  :config
  (add-hook 'emacs-lisp-mode-hook 'turn-on-font-lock)
  (add-hook 'emacs-lisp-mode-hook 'turn-on-whitespace-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode))

(use-package cc-mode
  :ensure nil
  :mode ("\\.c" . c-mode) ("\\.h" . c-mode)
        ("\\.java" . java-mode)
  :bind (:map c-mode-base-map
         ("RET" . c-context-line-break)
         :map java-mode-map
         ("M-," . ensime-pop-find-definition-stack))
  :functions turn-on-c-auto-hungry c-toggle-auto-hungry-state
  :config
  (defun turn-on-c-auto-hungry () (c-toggle-auto-hungry-state t))
  (add-hook 'c-mode-common-hook 'turn-on-font-lock)
  (add-hook 'c-mode-common-hook 'turn-on-whitespace-mode)
  (add-hook 'c-mode-common-hook 'turn-on-c-auto-hungry)
  (add-hook 'java-mode-hook 'my/wide-columns))

(use-package scala-mode
  :mode "\\.scala\'"
  :config
  (add-hook 'scala-mode-hook 'turn-on-font-lock)
  (add-hook 'scala-mode-hook 'turn-on-whitespace-mode)
  (add-hook 'scala-mode-hook 'my/wide-columns)
  (add-hook 'scala-mode-hook 'ensime-mode)
  (add-hook 'scala-mode-hook 'turn-off-flycheck))

(use-package yasnippet
  :ensure nil
  :commands yas-minor-mode
  :diminish yas-minor-mode)

(use-package ensime
  :commands ensime ensime-mode ensime-pop-find-definition-stack
  :diminish yas-minor-mode
  :bind (:map ensime-mode-map
         ("M-n" . nil)
         ("C-c ! n" . ensime-forward-note)
         ("M-p" . nil)
         ("C-c ! p" . ensime-backward-note)))

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         :map org-mode-map
         ("M-h" . backward-kill-word)
         ("RET" . org-return-indent))
  :functions my/run-write-file-functions
  :config
  (defun my/run-write-file-functions (&rest args)
    (run-hooks 'write-file-functions))
  (advice-add 'org-edit-src-save :before #'my/run-write-file-functions)
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images t)
  (add-hook 'org-mode-hook 'turn-on-font-lock)
  (add-hook 'org-mode-hook 'turn-on-whitespace-mode))

(use-package ido
  :ensure nil
  :config
  (defun my/ido-extra-keys ()
    (bind-keys :map ido-completion-map
               ("C-h" . ido-delete-backward-updir)
               ("C-n" . ido-next-match)
               ("C-f" . ido-next-match)
               ("C-p" . ido-prev-match)
               ("C-b" . ido-prev-match)
               ("SPC" . ido-exit-minibuffer)))
  (add-hook 'ido-setup-hook 'my/ido-extra-keys))

(use-package find-file-in-repository
  :bind ("C-x C-f" . find-file-in-repository)
  :init (bind-key "C-x f" 'find-file))

(use-package ace-jump-mode
  :bind ("C-." . ace-jump-mode)
  :config (require 'cl))

(use-package woman
  :bind ("C-c w" . woman))

(use-package comint
  :ensure nil
  :defer t
  :bind (:map comint-mode-map
         ("C-c o" . my/comint-empty-buffer)
         ("C-c r" . comint-history-isearch-backward))
  :functions comint-truncate-buffer
  :config
  (defun my/comint-empty-buffer ()
    "Truncate a comint buffer to empty."
    (interactive)
    (let ((comint-buffer-maximum-size 0))
      (comint-truncate-buffer))))

(use-package ess-site
  :ensure ess
  :mode ("/R/.*\\.q\\'" . R-mode)
        ;; Having "\\.[rR]\\'" in the list prevents ESS from adding its own
        ;; entries, which is bizarre but in this case helpful
        ("\\.[rR]\\'" . R-mode)
        ("\\.[rR]profile\\'" . R-mode)
        ("NAMESPACE\\'" . R-mode)
        ("CITATION\\'" . R-mode)
        ("\\.[Rr]out" . R-transcript-mode)
        ("\\.Rd\\'" . Rd-mode)
        ("\\.[Bb][Uu][Gg]\\'" . ess-bugs-mode)
        ("\\.[Bb][Oo][Gg]\\'" . ess-bugs-mode)
        ("\\.[Bb][Mm][Dd]\\'" . ess-bugs-mode)
        ("\\.[Jj][Aa][Gg]\\'" . ess-jags-mode)
        ("\\.[Jj][Oo][Gg]\\'" . ess-jags-mode)
        ("\\.[Jj][Mm][Dd]\\'" . ess-jags-mode)
  :interpreter ("Rscript" . R-mode)
  :bind (:map inferior-ess-mode-map
         ("_" . self-insert-command)
         ("C-c C-d" . ess-help)
         :map ess-mode-map
         ("C-c C-d" . ess-help)
         ("C-c C-k" . ess-load-file)
         ("M-TAB" . ess-complete-object-name)
         ("_" . self-insert-command)
         ("C-c h" . nil)
         :map ess-help-mode-map
         ("C-c h" . nil)
         :map ess-noweb-minor-mode-map
         ("C-c h" . nil)
         :map ess-bugs-mode-map
         ("_" . self-insert-command))
  :config
  (require 'ess-inf)
  (require 'ess-mode)
  (require 'ess-bugs-d)
  (require 'ess-jags-d)
  (add-hook 'ess-mode-hook 'turn-on-font-lock)
  (add-hook 'ess-mode-hook 'turn-on-whitespace-mode)
  (advice-add 'ess-load-file :around #'my/preserve-selected-window)
  (advice-add 'ess-help :around #'my/preserve-selected-window))

(use-package poly-R
  :ensure polymode
  :mode ("\\.Snw" . poly-noweb+r-mode)
        ("\\.Rnw" . poly-noweb+r-mode)
        ("\\.Rmd" . poly-markdown+r-mode))

(use-package isearch-initial
  :ensure nil
  :load-path "elisp"
  :bind ("C-t s" . isearch-forward-at-point)
        ("C-t C-s" . isearch-forward-at-point))

(provide 'init)

;;; init.el ends here
