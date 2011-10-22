;;; 45c.el --- Custom cc-mode configuration

;; Mmm... cc-mode
(require 'cc-mode)

;; Setup a new "C" style to support this C-Language Coding Standard
(c-add-style
 "clcs"
 ;; Expand all tabs with appropriate number of spaces
 '((indent-tabs-mode         . nil)
   (tab-width                . 4)
   (c-basic-offset           . 4)
   (fill-column              . 79)
   ;;
   ;; Set up when NOT to auto fill
   ;;   string -- in text string declarations
   ;;   cpp    -- Preprocessor Macros and directives
   ;;   c      -- C comments
   ;;   c++    -- C++ comments
   ;;   code   -- in C or C++ code blocks
   ;; Normal default is '(string cpp code) below line
   ;; only disables auto-fill in string and cpp
   (c-ignore-auto-fill       . '(string cpp))
   (c-hanging-braces-alist   . ((defun-open        before after)
                                (defun-close       before after)
                                (brace-list-open   before after)
                                (brace-list-close  before after)
                                (brace-entry-open  before after)
                                (statement         before after)
                                (substatement-open before after)
                                (block-open        before after)
                                (block-close       . c-snug-do-while)
                                (else-clause       before after)))
   (c-offsets-alist          . ((statement-block-intro   . +)
                                (knr-argdecl-intro       . 0)
                                (substatement-open       . 0)
                                (label                   . 0)
                                (case-label              . 0)
                                (brace-list-open         . 0)
                                (statement-cont          . +)
                                (inline-open             . 0)
                                (inexpr-class            . 0)
                                (arglist-intro           . +)
                                (arglist-cont-nonempty   . +)
                                (arglist-cont            . 0)))))

(defun custom-c-mode-common-hook ()
  (c-toggle-auto-hungry-state t)
  (c-set-style "clcs")
  (setq tab-width 8 indent-tabs-mode nil)
  (define-key c-mode-base-map "\C-m" 'c-context-line-break))

(add-hook 'c-mode-common-hook 'custom-c-mode-common-hook)
(add-hook 'c-mode-common-hook 'llasram/whitespace-mode t)

(require 'tempo)
(tempo-define-template
  "c-function"
  '(&
    "{" n
    "#   undef CDL__func__" n
    "#   define CDL__func__ \"" p "\"" n
    "    Cdl_Result result = CDL_SUCCESS;" n
    "    " p n
    " finally:" n
    "    return result;" n
    "}" n)
  "function"
  "Insert a C function")
