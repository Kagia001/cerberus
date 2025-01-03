;;; cerberus-vars.el --- Variable definitions for Cerberus -*- lexical-binding: t; -*-

;; Author: Karl Ragnar Giese
;; Created: 27. December 2024
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

(defvar cerberus--thing-definitions nil "Thing settings for treesit things. See `treesit-thing-settings' for format")

(defvar-keymap cerberus-default-normal-keymap
  :doc "Keymap for Cerberus' normal state"

  "q" #'ignore
  "w" #'meow-mark-symbol
  "f" #'meow-next-symbol
  "p" #'yank
  "b" #'meow-prev-symbol
  "j" #'meow-join
  "l" #'meow-line
  "u" #'undo
  "y" #'kill-ring-save
  ";" #'execute-extended-command
  "'" #'ignore
  "~" #'ignore
  
  "a" #'ignore
  "r" #'meow-replace
  "s" #'ignore
  "t" #'cerberus-insert-mode
  "g" #'ignore
  "m" #'backward-char
  "n" #'next-line
  "e" #'previous-line
  "i" #'forward-char
  "o" #'meow-block
  "(" #'ignore
  ")" #'ignore
  
  "z" #'ignore
  "x" #'delete-char
  "c" #'cerberus-change
  "d" #'kill-region
  "v" #'ignore
  "/" #'ignore
  "k" #'ignore
  "h" #'ignore
  "," #'ignore
  "." #'ignore
  "-" #'ignore

  "TAB" #'ignore
  "RET" #'ignore
  "ESC" #'keyboard-quit
  "SPC" #'hydra-leader/body)

(defvar-keymap cerberus-default-insert-keymap
  :doc "Keymap for Cerberus' insert mode"
  "TAB" #'completion-at-point
  "ESC" #'cerberus-normal-mode)

(defvar-keymap cerberus-ts-normal-keymap
  :doc "Keymap for Cerberus' normal mode when treesit is active"
  :parent cerberus-default-normal-keymap
  "m" #'cerberus-up
  "n" #'cerberus-sentence-next
  "e" #'cerberus-sentence-prev
  "i" #'cerberus-down

  "f" #'cerberus-word-next
  "b" #'cerberus-word-prev)


(defvar cerberus--keymap-alist `((cerberus-normal-mode . ,cerberus-default-normal-keymap)
			(cerberus-insert-mode . ,cerberus-default-insert-keymap))
  "Keymap alist for Cerberus which is added to emulation-mode-map-alists.")
(make-variable-buffer-local 'cerberus--keymap-alist)

(provide 'cerberus-vars)
