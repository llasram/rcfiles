;;; 90misc.el --- Miscellaneous definitions 

;; Function of fun
(defun force-flyspell-mode ()
  "Activates flyspell-mode, whether already active or not."
  (flyspell-mode 1))

;; Some custom commands
(defun other-window-backwards (&optional n)
  "Select Nth previous window"
  (interactive "P")
  (other-window (- (prefix-numeric-value n))))

(defun scroll-n-lines-ahead (&optional n)
  "Scroll ahead N lines (1 by default)."
  (interactive "P")
  (scroll-ahead (prefix-numeric-value n)))

(defun scroll-n-lines-behind (&optional n)
  "Scroll behind N lines (1 by default)."
  (interactive "P")
  (scroll-behind (prefix-numeric-value n)))

(defun set-tab-width (width)
  "Set tab-width to 4"
  (interactive "P")
  (set-variable 'tab-width width))

(defun generic-hungry-code-at-point-p ()
  (let* ((properties (text-properties-at (point))))
    (null (or (memq 'font-lock-string-face properties)
              (memq 'font-lock-comment-face properties)))))

(defun generic-hungry-backspace (arg)
  (interactive "*P")
  (if (or arg (not (generic-hungry-code-at-point-p)))
      (backward-delete-char-untabify (prefix-numeric-value arg))
    (let ((here (point)))
      (skip-chars-backward " \t\n")
      (if (/= (point) here)
          (delete-region (point) here)
        (backward-delete-char-untabify 1)))))

(defun generic-hungry-delete (arg)
  (interactive "*P")
  (if (or arg (not (generic-hungry-code-at-point-p)))
      (backward-delete-char-untabify (- (prefix-numeric-value arg)))
    (let ((here (point)))
      (skip-chars-forward " \t\n")
      (if (/= (point) here)
          (delete-region (point) here)
        (backward-delete-char-untabify -1)))))

;; RFC fetching
(defun fetch-rfc (arg)
  (interactive "MRFC number: ")
  (let ((name (format "*rfc%s*" arg))
        (url (format "http://www.ietf.org/rfc/rfc%s.txt" arg)))
    (if (get-buffer name)
        (switch-to-buffer name)
      (switch-to-buffer (url-retrieve-synchronously url))
      (rename-buffer name)
      (goto-char (point-min))
      (while (and (not (looking-at "^$"))
                  (not (eobp)))
        (forward-line 1))
      (forward-line 1)
      (delete-char (- (1- (point))))
      (setq buffer-read-only t)
      (rfcview-mode)
      (not-modified))))

(add-hook 'rfcview-mode-hook 'llasram/rfcview-extra-keys)
(defun llasram/rfcview-extra-keys ()
  (define-key rfcview-mode-map [mouse-1] 'muse-follow-name-at-mouse)
  (define-key rfcview-mode-map [mouse-2]
    'muse-follow-name-at-mouse-other-window))

;; Browse kill-ring
(require 'browse-kill-ring)
(defadvice yank-pop (around kill-ring-browse-maybe (arg) activate)
  "If last action was not a yank, run `browse-kill-ring' instead."
  (if (not (eq last-command 'yank))
      (browse-kill-ring)
    ad-do-it))

;; My own magic modes
(require 'unicode-smarty)
(require 'hbfc)
(hbfc-mode 1)

;; Can't `diminish' until after the modes are loaded...
(require 'diminish)
(diminish 'abbrev-mode)
(diminish 'hs-minor-mode)
(diminish 'hbfc-mode)
(diminish 'flyspell-mode)
(diminish 'unicode-smarty-mode)

;; Replace Unicode XML entities with the appropriate characters
(defun entity-to-char-buffer ()
  (interactive)
  (let ((entities-replaced 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "&#\\(....\\);" (point-max) t)
        (replace-match
         (char-to-string
          (decode-char 'ucs (string-to-number (match-string 1)))))
        (setq entities-replaced (1+ entities-replaced))))
    (message "Replaced %d %s" entities-replaced
             (if (= entities-replaced 1) "entity" "entities"))))

;; I-search with initial contents
(defvar isearch-initial-string nil)

(defun isearch-set-initial-string ()
  (remove-hook 'isearch-mode-hook 'isearch-set-initial-string)
  (setq isearch-string isearch-initial-string)
  (isearch-search-and-update))

(defun isearch-forward-at-point (&optional regexp-p no-recursive-edit)
  "Interactive search forward for the symbol at point."
  (interactive "P\np")
  (if regexp-p (isearch-forward regexp-p no-recursive-edit)
    (let* ((end (progn (skip-syntax-forward "w_") (point)))
           (begin (progn (skip-syntax-backward "w_") (point))))
      (if (eq begin end)
          (isearch-forward regexp-p no-recursive-edit)
        (setq isearch-initial-string (buffer-substring begin end))
        (add-hook 'isearch-mode-hook 'isearch-set-initial-string)
        (isearch-forward regexp-p no-recursive-edit)))))
