;;; cerberus-python.el --- Python support for Cerberus -*- lexical-binding: t; -*-

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
 'python
 `((cerberus-statement
    ,(regexp-opt '("statement")))

   (cerberus-other-sentence ,(regexp-opt '("else_clause"
		       "except_clause"
		       "definition")))
   
   (cerberus-nontrailing-list
    ,(regexp-opt '("argument_list"
		   "parameters"
		   "list"
		   "dictionary")))
   (cerberus-word
    ,(lambda (node)
       (and
	;; Must not have (smaller) child nodes
	(zerop (cerberus--node-smaller-child-count node t))
	(or
	 ;; And either be named (but not string start/end)
	 (and (treesit-node-check node 'named)
	      (not (treesit-query-capture node '([(string_start) (string_end)] @ignore))))
	 ;; Or an operator (having to specifically exclude "in" in for-statements)
	 (and (string-match (regexp-opt '("=" "+=" "-=" "*=" "/=" "%=" "//=" "**=" "&=" "|=" "^=" ">>=" "<<=" ":="
					  "and" "not" "or" "is" "is not" "in" "not in"
					  "&" "|" "^" "~" "<<" ">>"
					  "==" ">=" "<=" "!=" ">" "<"
					  "+" "-" "*" "/" "%" "**" "//")
					'symbols)
			    (treesit-node-text node t))
	      (not (cerberus--node-is-thing-p (treesit-node-parent node) "^for_statement$")))))))))



(provide 'cerberus-python)

