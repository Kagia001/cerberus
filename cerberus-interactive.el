;;; cerberus-interactive.el --- Interactive functions for Cerberus -*- lexical-binding: t; -*-

;; Author: Karl Ragnar Giese
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



(require 'treesit)
(require 'cerberus-treesit)
(require 'cerberus-util)
(require 'cerberus-edit)

(defun cerberus-up ()
  (interactive)
  (if (use-region-p)
      (cerberus--mark-node (cerberus--node-larger-parent (cerberus--node-at-point)))
    (cerberus--mark-node (cerberus--node-at-point))))

(defun cerberus-down ()
  (interactive)
  (if (use-region-p)
      (cerberus--mark-node (cerberus--node-smaller-child (cerberus--node-at-point) 0 nil))
    (cerberus--mark-node (cerberus--node-at-point)))
  )

(defun cerberus-next ()
  (interactive)
  (if (use-region-p)
      (cerberus--mark-node (treesit-node-next-sibling (cerberus--node-at-point)))
    (cerberus--mark-node (cerberus--node-at-point)))
  )

(defun cerberus-prev ()
  (interactive)
  (if (use-region-p)
      (cerberus--mark-node (treesit-node-prev-sibling (cerberus--node-at-point)))
    (cerberus--mark-node (cerberus--node-at-point)))
  )

(defun cerburus-test-next-sentence ()
  (interactive)
  (cerberus--mark-node (treesit-navigate-thing (point) 1 'end 'sentence )))

(defun cerberus-select-next-leaf ()
  (interactive)
  (cerberus--mark-node (cerberus--next-leaf (cerberus--node-at-point))))

(defun cerberus-select-prev-leaf ()
  (interactive)
  (cerberus--mark-node (cerberus--prev-leaf (cerberus--node-at-point))))

(defun cerberus-select-sexp-forward ()
  (interactive)
  (cerberus--mark-node (cerberus--bottom-level-thing-next (cerberus--node-at-point) 'sexp)))

(defun cerberus-select-sexp-backward ()
  (interactive)
  (cerberus--mark-node (cerberus--bottom-level-thing-prev (cerberus--node-at-point) 'sexp)))

(defun cerberus-select-statement-forward ()
  (interactive)
  ;; (cerberus--mark-node (cerberus--bottom-level-thing-next (cerberus--node-at-point) 'cerberus-sentence))
  (cerberus--mark-node (cerberus--bottom-level-thing-next (cerberus--node-at-point) 'cerberus-sentence))
  )

(defun cerberus-select-statement-backward ()
  (interactive)
  (cerberus--mark-node (cerberus--bottom-level-thing-prev (cerberus--node-at-point) 'cerberus-sentence)))

(defun cerberus-delete ()
  (interactive)
  (if-let ((node (cerberus--node-in-region)))
      (let ((next-selection (cond ((cerberus--only-child-p node t)
				   nil)
				  ((cerberus--last-child-p node t)
				   (treesit-node-prev-sibling node t))
				  (t
				   (treesit-node-next-sibling node t)))))
	(message "%s" next-selection)
	(cerberus--mark-node (cerberus--delete-node node next-selection)))))
(provide 'cerberus-interactive)
