;;; cerberus-core.el --- Combobulate mode definition -*- lexical-binding: t; -*-

;; Author: Karl Ragnar Giese
;; Created: 26. December 2024
;; Package-Requires: ((emacs "30"))
;; Version: 0.1
;;
;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'cerberus-vars)
(require 'cerberus-languages)

(defgroup cerberus ()
  "Cerberus custom group")

(define-minor-mode cerberus-mode
  "Structural editing"
  :init nil
  :group cerberus
  (if cerberus-mode
      (cerberus--start)
    (cerberus--stop))
  )


(define-globalized-minor-mode cerberus-global-mode cerberus-mode (lambda () (cerberus-mode 1))
  :group 'cerberus
  (if cerberus-mode (cerberus--start)
    (cerberus--stop))
  )

(defun cerberus--start ()
  (let ((parsers (treesit-parser-list)))
    (if (eq 1 (length parsers))
	(progn (cerberus--lang-init (treesit-parser-language (car parsers)))
	       (ryo-modal-mode 1))
      (funcall cerberus-fallback))))

(defun cerberus--stop ()
  (ryo-modal-mode -1))

(provide 'cerberus-core)
