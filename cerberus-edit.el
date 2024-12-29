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

;; (push '(c-ts-mode . c) tree-edit-language-alist)

(defun cerberus--rm-track (start end track)
  (let ((rmdiff (- end start))
	(tstart (treesit-node-start track))
	(tend (treesit-node-end track)))
    (delete-region start end)
    (when track
      (when (>= tstart end) (setq tstart (- tstart rmdiff)))
      (when (>= tend end) (setq tend (- tend rmdiff)))
      (treesit-node-on tstart tend))))

;; (defun cerberus--delete-node (node track)
;;   (let ((track (cond ((cerberus--node-is-thing-p node 'cerberus-trailing-list-element)
;; 		      ;; (list (treesit-node-start node)
;; 		      ;;       (if (cerberus--node-last-child-p)))
;; 		      (message "TODO trailing list"))
;; 		     ((cerberus--node-is-thing-p node 'cerberus-nontrailing-list-element)
;; 		      (cond ((cerberus--node-only-child-p node t)
;; 			     (cerberus--rm-track (treesit-node-start node)
;; 						 (treesit-node-end node)
;; 						 track))
;; 			    ((cerberus--node-last-child-p node t)
;; 			     (cerberus--rm-track (treesit-node-start (treesit-node-prev-sibling node nil))
;; 						 (treesit-node-end node)
;; 						 track))
;; 			    (t
;; 			     (cerberus--rm-track (treesit-node-start node)
;; 						 (treesit-node-start (treesit-node-next-sibling node t))
;; 						 track))))

;; 		     (t
;; 		      (cerberus--rm-track (treesit-node-start node)
;; 					  (treesit-node-end node)
;; 					  track)))))

;;     (if (s-match "^\s*$" (buffer-substring (point-at-bol) (point-at-eol)))
;; 	(cerberus--rm-track (point-at-bol) (1+ (point-at-eol)) track)
;;       track)))


(defun cerberus--delete-node (node track)
  (cond ((cerberus--node-is-thing-p node 'cerberus-trailing-list-element)
	 (message "TODO trailing list"))
	(t
	 (cond ((cerberus--node-only-child-p node t)
		(cerberus--rm-track (treesit-node-end (treesit-node-prev-sibling node nil))
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

(defun cerberus--delete-nodes (nodes)
  (save-excursion
    (delete-region (apply #'min (mapcar #'treesit-node-start nodes))
		   (apply #'max (mapcar #'treesit-node-end nodes)))))

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
