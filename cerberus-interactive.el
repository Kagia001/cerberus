;;; cerberus-interactive.el --- Interactive functions for Cerberus -*- lexical-binding: t; -*-

;; Author: Karl Ragnar Giese
;; Created: 23. December 2024
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

(defun cerberus-sentence-next ()
  (interactive)
  (cerberus--mark-node (cerberus--node-navigate (cerberus--node-at-point) 1 'cerberus-sentence-or-condition)))

(defun cerberus-sentence-prev ()
  (interactive)
  (cerberus--mark-node (cerberus--node-navigate (cerberus--node-at-point) -1 'cerberus-sentence-or-condition)))

(defun cerberus-word-next ()
  (interactive)
  (cerberus--mark-node (cerberus--node-navigate (cerberus--node-at-point) 1 'cerberus-word)))

(defun cerberus-word-prev ()
  (interactive)
  (cerberus--mark-node (cerberus--node-navigate (cerberus--node-at-point) -1 'cerberus-word)))

(defun cerberus-move-node-up ()
  (interactive)
  (cerberus--mark-node (cerberus--swap-nodes (cerberus--node-at-point) (treesit-node-prev-sibling (cerberus--node-at-point) t))))

(defun cerberus-move-node-down ()
  (interactive)
  (cerberus--mark-node (cerberus--swap-nodes (cerberus--node-at-point) (treesit-node-next-sibling (cerberus--node-at-point) t))))

(defun cerberus-delete ()
  (interactive)
  (if-let ((node (cerberus--node-in-region)))
      (let ((next-selection (cond ((or (cerberus--node-only-child-p node t)
				       (treesit-node-field-name node))
				   nil)
				  ((cerberus--node-last-child-p node t)
				   (treesit-node-prev-sibling node t))
				  (t
				   (treesit-node-next-sibling node t)))))
	(message "%s" next-selection)
	(cerberus--mark-node (cerberus--delete-node node next-selection)))))
(provide 'cerberus-interactive)
