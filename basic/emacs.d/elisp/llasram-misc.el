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

(provide 'llasram-misc)

;;; llasram-misc.el ends here
