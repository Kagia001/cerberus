;;; cerberus-c.el --- C support for Combobulate -*- lexical-binding: t; -*-

;; Author: Karl Ragnar Giese
;; Created: 22. December 2024
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

(require 'cerberus-util)

(cerberus-lang-def-things
 'c
 `((cerberus-condition
    ,(lambda (node) (and (treesit-node-check node 'named)
		    (equal "condition" (treesit-node-field-name (treesit-node-parent node))))))
   (cerberus-sentence
    (,(regexp-opt '("attributed_statement"
		    "break_statement"
		    "case_statement"
		    "comment"
		    "continue_statement"
		    "declaration"
		    "do_statement"
		    "else_clause"
		    "expression_statement"
		    "for_statement"
		    "function_definition"
		    "goto_statement"
		    "if_statement"
		    "labeled_statement"
		    "preproc_def"
		    "preproc_include"	; Mostly yanked from c-ts-mode
		    "return_statement"
		    "switch_statement"
		    "while_statement")
		  'symbols)
     .
     ;; Dont include [else if] if-statements
     ,(lambda (node) (not (and (cerberus--node-is-thing-p (treesit-node-parent node) "^else_clause$")
			  (cerberus--node-is-thing-p node "^if_statement$"))))))

   (cerberus-word
    ,(lambda (node)
       (and
	;; Must not have (smaller) child nodes
	(zerop (cerberus--node-smaller-child-count node t))
	(or
	 ;; And either be named,
	 (treesit-node-check node 'named)
	 ;; an operator,
	 (equal "operator" (treesit-node-field-name node))
	 ;; or one of theese:
	 (string-match (concat "\\`"
			       (regexp-opt '("=" "*" "&" "struct"))
			       "\\'")
		       (treesit-node-text node t))))))
   (cerberus-nontrailing-list
    ,(regexp-opt '("argument_list"
		   "parameter_list"
		   "initializer_list")))

   (cerberus-nontrailing-list-element
    ,(lambda (node) (cerberus--node-is-thing-p (treesit-node-parent node) 'cerberus-nontrailing-list)))))

(provide 'cerberus-c)

