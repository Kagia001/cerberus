;;; cerberus-edit.el --- Editing functions for Cerberus -*- lexical-binding: t; -*-

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



;; (require 'combobulate)
(require 'cerberus-util)
(require 'cerberus-treesit)

(defun cerberus--swap-nodes (a b)
  "Swap nodes a and b. Return new a."
  (when (and a b)
    (let ((atext (treesit-node-text a t))
	  (btext (treesit-node-text b t))
	  (astart (treesit-node-start a)))
      (setq b (cerberus--rm-track (treesit-node-start a)
			  (treesit-node-end a)
			  b))
      (setq b (cerberus--insert-track astart btext b))
      (cerberus--replace-track (treesit-node-start b) (treesit-node-end b) atext)
      (treesit-node-on (treesit-node-start b) (+ (treesit-node-start b) (length atext))))))

(defun cerberus--replace-track (start end text &optional track)
  (setq track (cerberus--rm-track start end track))
  (setq track (cerberus--insert-track start text track))
  track)

(defun cerberus--insert-track (position text &optional track)
  (save-mark-and-excursion
    (let ((diff (length text))
	  (tstart (treesit-node-start track))
	  (tend (treesit-node-end track)))
      (goto-char position)
      (insert text)
      (when track
	(when (>= tstart position) (setq tstart (+ tstart diff)))
	(when (>= tend position) (setq tend (+ tend diff)))
	(treesit-node-on tstart tend)))))

(defun cerberus--rm-track (start end &optional track)
  (let ((rmdiff (- end start))
	(tstart (treesit-node-start track))
	(tend (treesit-node-end track)))
    (delete-region start end)
    (when track
      (when (>= tstart end) (setq tstart (- tstart rmdiff)))
      (when (>= tend end) (setq tend (- tend rmdiff)))
      (treesit-node-on tstart tend))))

(defun cerberus--delete-node (node track)
  (cond ((cerberus--node-is-thing-p node 'cerberus-trailing-list-element)
	 (message "TODO trailing list"))
	(t
	 (cond ((or (cerberus--node-only-child-p node t)
		    (treesit-node-field-name node))
		(cerberus--rm-track (seq-find #'identity (list (treesit-node-end (treesit-node-prev-sibling node nil))
						       (treesit-node-start node)))
			    (treesit-node-end node)
			    track))
	       ((cerberus--node-last-child-p node t)
		(cerberus--rm-track (treesit-node-end (treesit-node-prev-sibling node t))
			    (treesit-node-end node)
			    track))
	       (t
		(cerberus--rm-track (treesit-node-start node)
			    (treesit-node-start (treesit-node-next-sibling node t))
			    track))))))

(defun cerberus--list-element-with-separator (node)
  (cond ((cerberus--node-is-thing-p node 'cerberus-nontrailing-list-element)
	 (list node (cond ((cerberus--node-first-child-p node t) (treesit-node-next-sibling node nil))
			  (t (treesit-node-prev-sibling node nil)))))
	
	((cerberus--node-is-thing-p node 'cerberus-trailing-list-element)
	 (list node (treesit-node-next-sibling node nil)))
	(t nil)))

(defun cerberus--node-with-optional-separator (node)
  (if (or (cerberus--node-is-thing-p node 'cerberus-trailing-list-element)
	  (cerberus--node-is-thing-p node 'cerberus-nontrailing-list-element))
      (cerberus--list-element-with-separator node)
    (node)))

(provide 'cerberus-edit)
