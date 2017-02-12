;;; llasram-misc -- miscellaneous functions

;;; Commentary:

;;; Code:

(defun rename-this-buffer-and-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (message "File '%s' successfully renamed to '%s'" name
                        (file-name-nondirectory new-name))))))))

(defun my/preserve-selected-window (f &rest args)
  "Function version of `save-selected-window'.
Argument F is a function to invoke and optional ARGS any
arguments to `apply' that function to."
  (save-selected-window (apply f args)))

(defun my/describe-function ()
  "Personal variant of `describe-function'."
  (interactive)
  (let ((fn (function-called-at-point)))
    (if fn
      (describe-function fn)
      (command-execute 'describe-function))))

(defun TeX-insert-dollar--skip-close-math (f &rest args)
  "If point is before the end of a math section, skip it.
In that case applies F to ARGS."
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
      (apply f args)))

(provide 'llasram-misc)

;;; llasram-misc.el ends here
