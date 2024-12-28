;;; cerberus.el --- Structural editing for EMACS -*- lexical-binding: t; -*-

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
(require 'cerberus-core)
(require 'cerberus-languages)
(require 'cerberus-edit)
(require 'cerberus-treesit)
(require 'cerberus-util)
(require 'cerberus-interactive)

(straight-use-package 'combobulate)
(straight-use-package 'ryo-modal)

(defun cerberus-normal-state ()
  (interactive)
  (ryo-modal-keys
   ("m" cerberus-up)
   ("n" cerberus-select-statement-forward)
   ("e" cerberus-select-statement-backward)
   ("i" cerberus-down)

   ("f" cerberus-select-next-leaf)
   ("b" cerberus-select-prev-leaf)
   ("l" cerberus-next)
   ("u" cerberus-prev)

   ("d" cerberus-delete)

   ("M-i" combobulate-envelop)
   ("M-n" combobulate-drag-down)
   ("M-e" combobulate-drag-up)
   ("M-m" combobulate-splice-parent)
   ))
(cerberus-normal-state)
(defun cerberus-tree-nav-state ()
  (interactive)
  )
(provide 'cerberus)
