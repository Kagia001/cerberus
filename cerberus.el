;;; cerberus.el --- Structural editing for EMACS -*- lexical-binding: t; -*-

;; Author: Karl Ragnar Giese
;; Created: 21. December 2024
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
(straight-use-package 'meow)
(straight-use-package 'combobulate)

(require 'combobulate)
(require 'meow)

(require 'cerberus-vars)
(require 'cerberus-core)
(require 'cerberus-languages)
(require 'cerberus-edit)
(require 'cerberus-treesit)
(require 'cerberus-util)
(require 'cerberus-interactive)



(provide 'cerberus)
