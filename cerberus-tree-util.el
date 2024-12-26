;;; cerberus-tree-util.el --- Syntax tree utility functions for Cerberus -*- lexical-binding: t; -*-

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

(defun cerberus--search-subtree-without-self (node predicate &optional backward all depth)
  (let ((children (cerberus--node-smaller-children node (not all)))
	(return nil))
    (dolist (child (if backward (reverse children) children) return)
      (if (not return) (setq return (treesit-search-subtree child predicate backward all depth))))))

(defun cerberus--node-eq (node1 node2)
  (and (eq (treesit-node-start node1) (treesit-node-start node2))
       (eq (treesit-node-end node1) (treesit-node-end node2))))

(defun cerberus--node-is-thing-p (node thing)
  (treesit-node-eq node (treesit-search-subtree node thing nil t)))

(defun cerberus--thing-no-subthing-p (node thing)
  (and (cerberus--node-is-thing-p node thing)
       (not (cerberus--search-subtree-without-self node thing))))

(defun cerberus--is-leaf-p (node)
  (pcase (treesit-node-child-count node)
    (0 t)
    (1 (cerberus--is-leaf-p (treesit-node-child node 0)))))


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

(defun cerberus--larger-parent (node)
  (let ((start (treesit-node-start node))
	(end (treesit-node-end node)))
    
    (while (and (eq start (treesit-node-start node))
		(eq end (treesit-node-end node))
		(treesit-node-parent node))
      (setq node (treesit-node-parent node)))
    
    (cerberus--same-size-parent node)
    ))

(defun cerberus--smaller-child (node n &optional named)
  (nth n (cerberus--node-smaller-children node named)))

(defun cerberus--node-smaller-children (node &optional named)
  (if-let ((child (treesit-node-child node 0 named)))
      (if (not (cerberus--node-eq node child))
	  (treesit-node-children node named)
	(cerberus--node-smaller-children child named))))

;; (defun cerberus--node-next-peer (node)
;;   (let ((parent (treesit-node-parent node))
;; 	(i 0))
;;     (while (eq (treesit-node-index node) (treesit-node-child-count parent))
;;       (setq node parent)
;;       (setq parent (treesit-node-parent parent))
;;       (setq i (+ i 1)))

;;     (treesit-node-child parent (1+ (treesit-node-index node)))))


(defun cerberus--next-leaf (node)
  (if (cerberus--is-leaf-p node)
      (treesit-search-forward node #'cerberus--is-leaf-p nil)
    (treesit-search-subtree node #'cerberus--is-leaf-p nil)))

(defun cerberus--prev-leaf (node)
  (if (cerberus--is-leaf-p node)
      (treesit-search-forward node #'cerberus--is-leaf-p t)
    (treesit-search-subtree node #'cerberus--is-leaf-p t)))

(defun cerberus--bottom-level-thing-next (node thing)
  (let ((search-p (lambda (n) (cerberus--thing-no-subthing-p n thing))))
    (seq-find #'identity `(,(cerberus--search-subtree-without-self node search-p nil t)
			   ,(treesit-search-forward node search-p nil t)))))

(defun cerberus--bottom-level-thing-prev (node thing)
  (treesit-search-forward node (lambda (n) (cerberus--thing-no-subthing-p n thing)) t t))

(defun cerberus--thing-next (node thing)
  (let ((search-p (lambda (n) (cerberus--thing-no-subthing-p n thing))))
    (seq-find #'identity `(,(cerberus--search-subtree-without-self node search-p nil t)
			   ,(treesit-search-forward node search-p nil t)))))


(defun cerberus--last-child-p (node &optional named)
  (eq (1+ (treesit-node-index node named)) (treesit-node-child-count (treesit-node-parent node) named)))
(provide 'cerberus-tree-util)

(defun cerberus--first-child-p (node &optional named)
  (eq 0 (treesit-node-index node named)))

(defun cerberus--only-child-p (node &optional named)
  (eq 1 (treesit-node-child-count (treesit-node-parent node) named)))

(provide 'cerberus-tree-util)
