;;; cerberus-languages.el --- Language support for Combobulate -*- lexical-binding: t; -*-

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

;; Languages with specific definitions
(require 'cerberus-c)
(require 'cerberus-python)
(require 'cerberus-treesit)


;; Defaults for other languages
(setq cerberus-default-thing-settings
      `((cerberus-sentence
	 ,(lambda (node) (save-mark-and-excursion
		      (and (progn (goto-char (treesit-node-start node))
				  (eq (+ (current-indentation) (line-beginning-position))
				      (treesit-node-start node)))
			   (progn (goto-char (treesit-node-end node))
				  (eq (line-end-position) (treesit-node-end node)))))))))

(defun cerberus--lang-init (lang)
  (defvar treesit-thing-settings nil)
  (cerberus--add-to-treesit-thing-settings lang cerberus-default-thing-settings))


(provide 'cerberus-languages)
