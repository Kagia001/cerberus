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

(defun cerberus--append-treesit-thing-settings (lang settings)
  (if-let (old-settings (assq lang treesit-thing-settings))
      (dolist (setting settings)
	(unless (assq (car setting) old-settings)
	  (push setting (cdr (assq lang treesit-thing-settings)))))
    ;; (setcdr settings (cdr (assq lang treesit-thing-settings)))
    (push `(,lang . ,settings) treesit-thing-settings)))
;; (if (assq lang treesit-thing-settings)
;;     (push settings (cdr (assq lang treesit-thing-settings)))
;;   (push `(,lang . ,settings) treesit-thing-settings)))

;; (setq-local treesit-thing-settings `((,lang . ,settings))))

(provide 'cerberus-util)
