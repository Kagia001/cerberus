;;; cerberus-c.el --- C support for Combobulate -*- lexical-binding: t; -*-

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

(require 'cerberus-util)

(cerberus-lang-def-things
 'c
 `((cerberus-sentence
    ,(regexp-opt '("preproc"
		   "declaration"
		   ;; "specifier"
		   "attributed_statement"
		   "labeled_statement"
		   "expression_statement"
		   "if_statement"
		   "switch_statement"
		   "do_statement"
		   "while_statement"
		   "for_statement"
		   "return_statement"
		   "break_statement"
		   "continue_statement"
		   "goto_statement"
		   "case_statement")))
   
   (cerberus-nontrailing-list
    ,(regexp-opt '("argument_list"
		   "parameter_list"
		   "initializer_list")))

   (cerberus-nontrailing-list-element
    ,(lambda (node) (cerberus--node-is-thing-p (treesit-node-parent node) 'cerberus-nontrailing-list)))))

(provide 'cerberus-c)

