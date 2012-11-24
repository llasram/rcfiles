;; Flyspell (almost) everywhere
(require 'flyspell)

(defvar flyspell-maybe-prog-mode-disable-modes
  '(fundamental-mode text-mode latex-mode tex-mode muse-mode planner-mode
    markdown-mode message-mode)
  "Modes in which `flyspell-prog-mode' is less than useful")

(defun flyspell-maybe-prog-mode ()
  "Turn on `flyspell-mode' for desirable text."
  (interactive)
  (cond
   ((minibufferp) nil)
   ((memq major-mode flyspell-maybe-prog-mode-disable-modes)
    (flyspell-mode 1))
   ((flyspell-prog-mode)) nil))

(add-hook 'font-lock-mode-hook 'flyspell-maybe-prog-mode)

(provide 'flyspell-everywhere)
