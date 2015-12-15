;;; llasram-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "flycheck-rust" "flycheck-rust.el" (22120 661
;;;;;;  100908 542000))
;;; Generated autoloads from flycheck-rust.el

(autoload 'flycheck-rust-setup "flycheck-rust" "\
Setup Rust in Flycheck.

If the current file is part of a Cargo project, configure
Flycheck according to the Cargo project layout.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "gforth" "gforth.el" (22095 36154 871768 255000))
;;; Generated autoloads from gforth.el

(autoload 'forth-mode "gforth" "\

Major mode for editing Forth code. Tab indents for Forth code. Comments
are delimited with \\ and newline. Paragraphs are separated by blank lines
only. Block files are autodetected, when read, and converted to normal
stream source format. See also `forth-block-mode'.
\\{forth-mode-map}

Variables controlling syntax hilighting/recognition of parsed text:
 `forth-words'
    List of words that have a special parsing behaviour and/or should be
    hilighted. Add custom words by setting forth-custom-words in your
    .emacs, or by setting forth-local-words, in source-files' local
    variables lists.
 forth-use-objects
    Set this variable to non-nil in your .emacs, or in a local variables
    list, to hilight and recognize the words from the \"Objects\" package
    for object-oriented programming.
 forth-use-oof
    Same as above, just for the \"OOF\" package.
 forth-custom-words
    List of custom Forth words to prepend to `forth-words'. Should be set
    in your .emacs.
 forth-local-words
    List of words to prepend to `forth-words', whenever a forth-mode
    buffer is created. That variable should be set by Forth sources, using
    a local variables list at the end of file, to get file-specific
    hilighting.
    0 [IF]
       Local Variables: ...
       forth-local-words: ...
       End:
    [THEN]
 forth-hilight-level
    Controls how much syntax hilighting is done. Should be in the range
    0..3

Variables controlling indentation style:
 `forth-indent-words'
    List of words that influence indentation.
 forth-local-indent-words
    List of words to prepend to `forth-indent-words', similar to
    forth-local-words. Should be used for specifying file-specific
    indentation, using a local variables list.
 forth-custom-indent-words
    List of words to prepend to `forth-indent-words'. Should be set in your
    .emacs.
 forth-indent-level
    Indentation increment/decrement of Forth statements.
 forth-minor-indent-level
    Minor indentation increment/decrement of Forth statemens.

Variables controlling block-file editing:
 forth-show-screen
    Non-nil means, that the start of the current screen is marked by an
    overlay arrow, and screen numbers are displayed in the mode line.
    This variable is by default nil for `forth-mode' and t for
    `forth-block-mode'.
 forth-overlay-arrow-string
    String to display as the overlay arrow, when `forth-show-screen' is t.
    Setting this variable to nil disables the overlay arrow.
 forth-block-base
    Screen number of the first block in a block file. Defaults to 1.
 forth-warn-long-lines
    Non-nil means that a warning message is displayed whenever you edit or
    move over a line that is longer than 64 characters (the maximum line
    length that can be stored into a block file). This variable defaults to
    t for `forth-block-mode' and to nil for `forth-mode'.

Variables controlling interaction with the Forth-process (also see
`run-forth'):
  forth-program-name
    Program invoked by the `run-forth' command (including arguments).
  inferior-forth-mode-hook
    Hook for customising inferior-forth-mode.
  forth-compile-command
    Default command to execute on `compile'.

\(fn)" t nil)

(autoload 'forth-block-mode "gforth" "\
Major mode for editing Forth block source files, derived from
`forth-mode'. Differences to `forth-mode' are:
 * files are converted to block format, when written (`buffer-file-format'
   is set to `(forth-blocks)')
 * `forth-show-screen' and `forth-warn-long-lines' are t by default

Note that the length of lines in block files is limited to 64 characters.
When writing longer lines to a block file, a warning is displayed in the
echo area and the line is truncated.

Another problem is imposed by block files that contain newline or tab
characters. When Emacs converts such files back to block file format,
it'll translate those characters to a number of spaces. However, when
you read such a file, a warning message is displayed in the echo area,
including a line number that may help you to locate and fix the problem.

So have a look at the *Messages* buffer, whenever you hear (or see) Emacs'
bell during block file read/write operations.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "ruby-electric" "ruby-electric.el" (22095 36154
;;;;;;  871768 255000))
;;; Generated autoloads from ruby-electric.el

(autoload 'ruby-electric-mode "ruby-electric" "\
Toggle Ruby Electric minor mode.
With no argument, this command toggles the mode.  Non-null prefix
argument turns on the mode.  Null prefix argument turns off the
mode.

When Ruby Electric mode is enabled, an indented 'end' is
heuristicaly inserted whenever typing a word like 'module',
'class', 'def', 'if', 'unless', 'case', 'until', 'for', 'begin',
'do'. Simple, double and back quotes as well as braces are paired
auto-magically. Expansion does not occur inside comments and
strings. Note that you must have Font Lock enabled.

\(fn &optional ARG)" t nil)

(eval-after-load 'ruby-mode '(add-hook 'ruby-mode-hook 'ruby-electric-mode))

;;;***

;;;### (autoloads nil "rust-mode" "rust-mode.el" (22120 764 697620
;;;;;;  286000))
;;; Generated autoloads from rust-mode.el

(autoload 'rust-mode "rust-mode" "\
Major mode for Rust code.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;;;***

;;;### (autoloads nil nil ("flyspell-everywhere.el" "hungry.el" "isearch-initial.el"
;;;;;;  "llasram-c-style.el" "llasram-clojure-indent.el" "llasram-ess.el"
;;;;;;  "muse-platyblog.el" "tight-fit.el") (22124 14957 732822 914000))

;;;***

(provide 'llasram-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; llasram-autoloads.el ends here
