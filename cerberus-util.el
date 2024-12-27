;;; cerberus-util.el --- Utility functions for Cerberus -*- lexical-binding: t; -*-

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



(require 'cerberus-tree-util)

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

(defun cerberus-lang-def-things (lang definitions)
  (let ((lang-settings (assq lang cerberus--thing-settings))
	(other-settings (assq-delete-all lang cerberus--thing-settings)))
    (dolist (definition definitions)
      (setq lang-settings (cons definition (assq-delete-all (car definition) lang-settings))))
    (setq cerberus--thing-settings (cons `(,lang . ,lang-settings) other-settings))))


(defun cerberus--add-to-treesit-thing-settings (lang &optional default)
  (let* ((new-settings (seq-find #'identity (list (cdr (assq lang cerberus--thing-settings)) default)))
	 (merged-settings (append new-settings
				(seq-remove (lambda (s) (member (car s) (mapcar #'car new-settings)))
					    (cdr (assq lang treesit-thing-settings))))))
    (setq treesit-thing-settings
	  (cons (cons lang merged-settings)
		(assq-delete-all lang treesit-thing-settings)))))

(provide 'cerberus-util)
