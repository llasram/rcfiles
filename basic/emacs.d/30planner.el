;;; 30planner.el --- Initial configuration for planner-mode

;; Planner
(require 'planner)
(require 'planner-id)
(require 'planner-diary)
(require 'planner-appt)
(require 'planner-calendar)
(require 'planner-bbdb)
(require 'planner-gnus)
(require 'remember)
(require 'remember-bbdb)
(require 'remember-planner)
(require 'remember-diary)
(add-hook 'diary-display-hook 'fancy-diary-display)
(planner-insinuate-diary)
(planner-gnus-insinuate)
(planner-appt-use-schedule)
(planner-appt-insinuate)
(planner-insinuate-calendar)
(setq remember-handler-functions '(remember-planner-append))
(setq remember-annotation-functions planner-annotation-functions)

;; I don't know where it comes from, but I don't like it
(display-time-mode -1)

(defun planner-diary-add-entry (date time text)
  "Prompt for a diary entry to add to `diary-file' on DATE.
Uses `planner-annotation-functions' to make hyperlinks.
TIME and TEXT are used in the description."
  (interactive (list (planner-read-date)
                     (read-string "Time: ")
                     (read-string "Diary entry: ")))
  (save-window-excursion
    (make-diary-entry
     (concat
      (let ((cal-date (planner-filename-to-calendar-date date)))
        (calendar-date-string cal-date t t))
      "  "
      (planner-appt-format-appt-section-line
       (concat time " " text " "
               (run-hook-with-args-until-success
                'planner-annotation-functions))))
     nil planner-diary-file))
  (planner-diary-insert-all-diaries-maybe)
  (planner-appt-schedule-sort))

;; Duplicated from planner-id.el and modified
(defun llasram/planner-id-markup (beg end &optional verbose)
  "Highlight IDs as invisible text from BEG to END.
VERBOSE is ignored."
  (goto-char beg)
  (while (re-search-forward "{{[^}\n]+}} *" end t)
    (planner-highlight-region
     (match-beginning 0)
     (match-end 0)
     'planner-id 60
     (list
      'face 'planner-id-face
      'invisible 'muse
      'intangible t))))

;; Duplicated from planner-id.el and modified
(defun llasram/planner-id-setup ()
  "Hook into `planner-mode'."
  (add-hook 'muse-colors-buffer-hook
            'llasram/planner-id-markup t t)
  (add-hook
   (if (and (boundp 'write-file-functions)
            (not (featurep 'xemacs)))
       'write-file-functions
     'write-file-hooks)
   'planner-id-update-tasks-maybe nil t))

(remove-hook 'planner-mode-hook 'planner-id-setup)
(add-hook 'planner-mode-hook 'llasram/planner-id-setup)
