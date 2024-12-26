;;; cerberus-python.el --- Python support for Cerberus -*- lexical-binding: t; -*-

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

(require 'cerberus-vars)
(require 'cerberus-util)

(setq cerberus--python-thing-settings
      `((cerberus-statement
	 ,(regexp-opt '("statement"
			"comment")))
	
	(cerberus-nontrailing-list
	 ,(regexp-opt '("argument_list"
			"parameters"
			"list"
			"dictionary")))
	
	(cerberus-nontrailing-list-element
	 ,(lambda (node) (cerberus--node-is-thing-p (treesit-node-parent node) 'cerberus-nontrailing-list)))))


(defun cerberus--python-init ()
  (defvar treesit-thing-settings nil)	; python-ts-mode doesn't define this yet
  (cerberus--append-treesit-thing-settings 'python cerberus--python-thing-settings))

(add-hook 'python-ts-mode-hook #'cerberus--python-init)

(provide 'cerberus-python)

