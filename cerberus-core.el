(require 'cerberus-vars)
(require 'cerberus-languages)

(defgroup cerberus ()
  "Cerberus custom group")

(define-minor-mode cerberus-mode
  "Structural editing"
  :init nil
  :group cerberus
  (if cerberus-mode
      (cerberus--stop)
    (cerberus--start))
  )


(define-globalized-minor-mode cerberus-global-mode cerberus-mode  cerberus--start)

(defun cerberus--start ()
  (let ((parsers (treesit-parser-list)))
    (if (eq 1 (length parsers))
	(cerberus--lang-init (treesit-parser-language (car parsers)))
      (funcall cerberus-fallback))))

(defun cerberus--stop ()
  (message "TODO"))

(provide 'cerberus-core)
