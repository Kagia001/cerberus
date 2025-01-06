;;; cerberus-languages.el --- Language support for Combobulate -*- lexical-binding: t; -*-

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

;; Languages with specific definitions
(require 'cerberus-c)
(require 'cerberus-python)
(require 'cerberus-treesit)


;; Defaults for other languages
(setq cerberus-default-thing-settings
      `((cerberus-statement ,regexp-unmatchable) ; Statements must be swappable with eachother
	
	(cerberus-condition
	 ,(lambda (node) (and (treesit-node-check node 'named)
			 (equal "condition" (treesit-node-field-name node)))))
	
	(cerberus-comment ,(regexp-opt '("comment")))

	(cerberus-other-sentence ,regexp-unmatchable)
	
	(cerberus-sentence
	 (or
	  cerberus-comment
	  cerberus-condition
	  cerberus-statement
	  cerberus-other-sentence
	  (not (or (not cerberus--node-spans-line-p) ; !(!a v !b) = ab
		   (not ,(lambda (n) (treesit-node-check n 'named)))))))
	
	(cerberus-word
	 ,(lambda (node) (and (zerop (cerberus--node-smaller-child-count node t))
			 (treesit-node-check node 'named))))
	
	(cerberus-nontrailing-list-element
	 ,(lambda (node) (cerberus--node-is-thing-p (treesit-node-parent node) 'cerberus-nontrailing-list)))))

(defun cerberus--treesit-init (lang)
  (defvar treesit-thing-settings nil)
  (setq cerberus--treesit-override-keymaps `((cerberus-normal-mode . ,cerberus-ts-normal-keymap)
				     (cerberus-insert-mode . ,cerberus-default-insert-keymap)))
  (cerberus--add-to-treesit-thing-settings lang cerberus-default-thing-settings))


(provide 'cerberus-languages)
