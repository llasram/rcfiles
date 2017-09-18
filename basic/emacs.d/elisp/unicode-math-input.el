;;; unicode-math-input.el --- Input unicode math symbols with LaTeX macros
;;
;; Copyright (C) 2016 Tamas K. Papp <tkpapp@gmail.com>
;; Author: Tamas K. Papp <tkpapp@gmail.com>
;; URL: https://github.com/tpapp/unicode-math-input
;; Keywords: Unicode, symbols, input
;; Version: 0.1
;; Package-Requires: ((math-symbol-lists "1.0"))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'math-symbol-lists)
(require 'quail)

(quail-define-package "unicode-math-input" "unicode" "α" t
                      "Input unicode math symbols with LaTeX macros.")

(defun unicode-math-input--define-rules (package alist)
  (mapc (lambda (elt)
          (let* ((macro (nth 1 elt))
                 (code (nth 2 elt))
                 (translation (and code (decode-char 'ucs code))))
            (when (and macro translation)
              (quail-defrule macro translation package))))
        alist))

(unicode-math-input--define-rules "unicode-math-input" math-symbol-list-basic)
(unicode-math-input--define-rules "unicode-math-input" math-symbol-list-extended)

(provide 'unicode-math-input)
