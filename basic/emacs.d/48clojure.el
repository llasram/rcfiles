;;; 48clojure.el --- Custom Clojure configuration

(require 'clojure-mode)
(add-hook 'clojure-mode-hook 'llasram/whitespace-mode t)
(add-hook 'clojure-mode-hook 'llasram/paredit-mode t)

(define-key slime-repl-mode-map "{" 'paredit-open-curly)
(define-key slime-repl-mode-map "}" 'paredit-close-curly)

(defun llasram/sldb-mode-hook ()
  (setq autopair-dont-activate t))
(add-hook 'sldb-mode-hook 'llasram/sldb-mode-hook)

(defun llasram/slime-repl-mode-hook ()
  (set-syntax-table clojure-mode-syntax-table)
  (clojure-mode-font-lock-setup)
  (llasram/paredit-mode))
(add-hook 'slime-repl-mode-hook 'llasram/slime-repl-mode-hook)

(defun llasram/fit-window-to-buffer (&optional window)
  (let ((window (or window (get-buffer-window))))
    (fit-window-to-buffer window
       (funcall temp-buffer-max-height (window-buffer window)))))

(defadvice slime-display-popup-buffer
  (after llasram/slime-fit-popup activate)
  "Fit SLIME most popup windows to necessary size."
  (llasram/fit-window-to-buffer))

(defadvice slime-initialize-macroexpansion-buffer
  (around llasram/slime-macroexpansion-other-window activate)
  "Prevent SLIME from switching windows for macroexpansion."
  (save-selected-window
    ad-do-it
    (llasram/fit-window-to-buffer)))

(defadvice slime-maybe-show-compilation-log
  (after llasram/slime-fit-popup-compilation activate)
  "Fit SLIME compilation windows to necessary size."
  (let ((window (get-buffer-window "*SLIME Compilation*")))
    (when window
      (with-struct (slime-compilation-result. successp)
          slime-last-compilation-result
        (if successp
            (delete-window window)
          (llasram/fit-window-to-buffer window))))))

(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(add-to-list 'ac-modes 'slime-repl-mode)

(define-clojure-indent
  (doto-let 1)
  (assert-args 1)
  (case-expr 1)
  (ignore-errors 0)
  (returning 1)
  (l/receive-all 1)
  (receive-all 1)
  (l/receive-in-order 1)
  (receive-in-order 1)
  (l/run-pipeline 1)
  (run-pipeline 1)
  (on-error 1)
  (l/on-error 1)
  (run 2)
  (l/run 2)
  (fresh 1)
  (l/fresh 1)
  (project 1)
  (l/project 1)
  (run* 1)
  (l/run* 1)
  (behavior 1)
  (fact?- 1)
  (<- 1))

(put 'defupdaters 'clojure-backtracking-indent '((2)))
(put 'defmulti-group 'clojure-backtracking-indent '((2)))
(put 'defmethod-group 'clojure-backtracking-indent '(4 (2)))
(put 'b/extend-byteable 'clojure-backtracking-indent '((2)))
(put 'b/extend-byteable 'clojure-indent-function 'defun)
