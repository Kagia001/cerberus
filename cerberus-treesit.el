;;; cerberus-treesit.el --- Treesit utility functions for Cerberus -*- lexical-binding: t; -*-

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
(require 'seq)
(require 'cerberus-util)

(defun cerberus--node-at-point ()
  (cerberus--same-size-parent (if (use-region-p)
			  (treesit-node-on (region-beginning) (region-end))
			(treesit-node-at (point)))))

(defun cerberus--node-in-region ()
  (when (use-region-p) (cerberus--same-size-parent (treesit-node-on (region-beginning) (region-end)))))

(defun cerberus--mark-node (node)
  (when node (setq deactivate-mark nil)
	(let ((outer (cerberus--same-size-parent node)))
	  (push-mark (treesit-node-end outer))
	  (goto-char (treesit-node-start outer))
	  (activate-mark))))

(defun cerberus--node-match-p (a b)
  "return matching nodes if nodes or same-size children are of the same type"
  (cl-intersection (mapcar #'treesit-node-type (cerberus--node-same-size-subtree a))
		   (mapcar #'treesit-node-type (cerberus--node-same-size-subtree b))))

(defun cerberus--node-swappable-p (a b)
  "Return t if treesit nodes A and B may be swapped.
This is determined to be possible if
they or same-size children either are of the same type or statements,
or if they are siblings and one is a comment"
  (or (cerberus--node-match-p a b)
      (cl-every
       (lambda (n) (cl-some
	       (lambda (nn) (cerberus--node-is-thing-p nn 'cerberus-statement))
	       (cerberus--node-same-size-subtree n)))
       (list a b))
      (and (cerberus--node-eq-p (treesit-node-parent a) (treesit-node-parent b))
	   (cl-some (lambda (n) (cerberus--node-is-thing-p n 'cerberus-comment))
		    (list a b)))))

(defun cerberus--add-to-treesit-thing-settings (lang &optional default)
  (let* ((new-settings (cerberus--alist-get treesit-thing-settings lang)))
    (dolist (settings (list default (cerberus--alist-get cerberus--thing-definitions lang)))
      (dolist (setting settings)
	(setq new-settings (cerberus--alist-put new-settings (car setting) (cdr setting)))))
    
    (setq treesit-thing-settings
	  (cerberus--alist-put treesit-thing-settings lang new-settings))))

(defun cerberus--search-subtree-without-self (node predicate &optional backward all depth)
  (let ((children (cerberus--node-smaller-children node (not all)))
	(return nil))
    (dolist (child (if backward (reverse children) children) return)
      (if (not return) (setq return (treesit-search-subtree child predicate backward all depth))))))

(defun cerberus--node-eq-p (node1 node2)
  (and (eq (treesit-node-start node1) (treesit-node-start node2))
       (eq (treesit-node-end node1) (treesit-node-end node2))))


(defun cerberus--node-is-thing-p (node thing)
  "Check if NODE is thing THING. THING may also be a thing predicate."
  ;; This was faster than (cerberus--node-eq-p node (treesit-search-subtree node thing))
  (let ((eq-flag nil))
    (dolist (node (cerberus--node-same-size-subtree node))
      (let* ((lang (treesit-node-language node))
	     (thing-definition (if (treesit-thing-defined-p thing lang) (treesit-thing-definition thing lang) thing)))
	(setq eq-flag
	      (or eq-flag
		  (cond ((functionp thing-definition)
			 (funcall thing-definition node))
			
			((stringp thing-definition)
			 (string-match-p thing-definition (treesit-node-type node)))
			
			((ignore-errors (and (stringp (car thing-definition))
					     (functionp (cdr thing-definition))))
			 (and (cerberus--node-is-thing-p node (car thing-definition))
			      (cerberus--node-is-thing-p node (cdr thing-definition))))
			
			((ignore-errors (eq (car thing-definition) 'or))
			 (cl-some (lambda (pred) (cerberus--node-is-thing-p node pred)) (cdr thing-definition)))
			
			((ignore-errors (eq (car thing-definition) 'not))
			 (not (cerberus--node-is-thing-p node (cdr thing-definition)))))))))
    eq-flag))


(defun cerberus--thing-no-subthing-p (node thing)
  (and (cerberus--node-is-thing-p node thing)
       (not (cerberus--search-subtree-without-self node thing))))

(defun cerberus--same-size-parent (node)
  (let ((start (treesit-node-start node))
	(end (treesit-node-end node))
	(parent (treesit-node-parent node)))
    
    (while (and (eq start (treesit-node-start parent))
		(eq end (treesit-node-end parent))
		(treesit-node-parent parent))
      (setq node parent)
      (setq parent (treesit-node-parent parent)))
    
    node))

(defun cerberus--node-larger-parent (node)
  (let ((start (treesit-node-start node))
	(end (treesit-node-end node)))
    
    (while (and (eq start (treesit-node-start node))
		(eq end (treesit-node-end node))
		(treesit-node-parent node))
      (setq node (treesit-node-parent node)))
    
    (cerberus--same-size-parent node)))

(defun cerberus--node-same-size-subtree (node)
  (when node
    (let ((child (treesit-node-child node 0)))
      (cons node (when (cerberus--node-eq-p child node) (cerberus--node-same-size-subtree child))))))

(defun cerberus--node-smaller-children (node &optional named)
  (if-let ((child (treesit-node-child node 0 named)))
      (if (not (cerberus--node-eq-p node child))
	  (treesit-node-children node named)
	(cerberus--node-smaller-children child named))))

(defun cerberus--node-smaller-child-count (node &optional named)
  (length (cerberus--node-smaller-children node named)))

(defun cerberus--node-smaller-child (node n &optional named)
  (nth n (cerberus--node-smaller-children node named)))

;; (defun cerberus--node-next-peer (node)
;;   (let ((parent (treesit-node-parent node))
;; 	(i 0))
;;     (while (eq (treesit-node-index node) (treesit-node-child-count parent))
;;       (setq node parent)
;;       (setq parent (treesit-node-parent parent))
;;       (setq i (+ i 1)))

;;     (treesit-node-child parent (1+ (treesit-node-index node)))))

(defun cerberus--node-search-fwd (node thing)
  (let ((new-node node)
	(stop-flag nil))
    (while (not (or (null new-node)
		    (and (cerberus--node-is-thing-p new-node thing)
			 (not (cerberus--node-eq-p new-node node)))))
      (setq new-node
	    (seq-find
	     #'identity
	     (list (treesit-node-child new-node 0)
		   (treesit-node-next-sibling new-node)
		   (treesit-node-next-sibling (treesit-parent-until new-node (lambda (n) (when (treesit-node-next-sibling n)))))))))
    new-node))

(defun cerberus--node-navigate (node arg thing &optional tactic)
  "Navigate ARG THINGs using TACTIC
TODO implement tactics
we go back from starting point or fwd from end point and find the next matching thing"
  (if node
      (pcase arg
	('0 node)
	('1
	 (cerberus--node-search-fwd node thing))
	('-1
	 (treesit-search-forward node thing t t))
	(_
	 (cerberus--node-navigate ((cerberus--node-navigate node (cl-signum arg) thing tactic)
			   (- arg (cl-signum arg))
			   thing
			   tactic))))))


(defun cerberus--node-last-child-p (node &optional named)
  (eq (1+ (treesit-node-index node named)) (treesit-node-child-count (treesit-node-parent node) named)))
(provide 'cerberus-tree-util)

(defun cerberus--node-first-child-p (node &optional named)
  (eq 0 (treesit-node-index node named)))

(defun cerberus--node-only-child-p (node &optional named)
  (eq 1 (treesit-node-child-count (treesit-node-parent node) named)))

(provide 'cerberus-treesit)
