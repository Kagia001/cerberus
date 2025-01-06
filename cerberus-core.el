;; cerberus-core.el --- Combobulate mode definition -*- lexical-binding: t; -*-

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
    (cerberus--stop)))

(define-globalized-minor-mode cerberus-global-mode cerberus-mode (lambda () (unless (minibufferp) (cerberus-mode 1)))
  :group 'cerberus
  (if cerberus-mode (cerberus--start)
    (cerberus--stop)))

(define-minor-mode cerberus-normal-mode
  "Cerberus' normal mode"
  :group cerberus
  :lighter "[N]"
  (when cerberus-normal-mode
    (dolist (mode '(cerberus-insert-mode cerberus-motion-mode))
      (funcall mode -1))))

(define-minor-mode cerberus-insert-mode
  "Cerberus' insert mode"
  :group cerberus
  :lighter "[I]"
  (when cerberus-insert-mode
    (dolist (mode '(cerberus-normal-mode cerberus-motion-mode))
      (funcall mode -1))))

(define-minor-mode cerberus-motion-mode
  "Cerberus' motion mode"
  :group cerberus
  :lighter "[M]"
  (when cerberus-motion-mode
    (dolist (mode '(cerberus-normal-mode cerberus-insert-mode))
      (funcall mode -1))))

(defun cerberus--start ()
  (dolist (element '(cerberus--default-keymaps
		     cerberus--treesit-override-keymaps
		     cerberus--user-override-keymaps))
    (add-to-list 'emulation-mode-map-alists element))
  
  (if (derived-mode-p 'special-mode)
      (cerberus-motion-mode)
    (cerberus-normal-mode))
  
  (let ((parsers (treesit-parser-list)))
    (if (eq 1 (length parsers))
	(progn (cerberus--treesit-init (treesit-parser-language (car parsers)))))))

(defun cerberus--stop ()
  (dolist (element '(cerberus--default-keymaps cerberus--treesit-override-keymaps cerberus--user-override-keymaps))
    (setq emulation-mode-map-alists (delete element emulation-mode-map-alists))))

(provide 'cerberus-core)
