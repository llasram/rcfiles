;;; llasram-poly-python -- Polymodes for Python

;;; Commentary:

;;; Code:

(require 'polymode)
(require 'poly-noweb)

(defcustom pm-poly/noweb+python
  (clone pm-poly/noweb :innermode 'pm-inner/noweb+python)
  "Noweb for Python configuration"
  :group 'polymodes
  :type 'object)

(defcustom pm-inner/noweb+python
  (clone pm-inner/noweb
         :mode 'python-mode)
  "Noweb for Python"
  :group 'innermodes
  :type 'object)

(define-polymode poly-noweb+python-mode pm-poly/noweb+python :lighter " PM-Pnw")

(provide 'llasram-poly-python)

;;; llasram-poly-python.el ends here
