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

(defvar cerberus--major-mode-keymaps nil
  "Keymaps overriding default keymaps ")

(defvar-keymap cerberus-default-normal-keymap
  :doc "Keymap for Cerberus' normal state"

  "q" #'ignore
  "w" #'meow-mark-symbol
  "f" #'meow-next-symbol
  "p" #'yank
  "b" #'meow-back-symbol
  "j" #'meow-join
  "l" #'meow-line
  "u" #'undo
  "y" #'kill-ring-save
  ";" #'execute-extended-command
  "'" #'ignore
  "~" #'ignore
  
  "a" #'cerberus-insert
  "A" #'cerberus-open-above
  "r" #'meow-replace
  "s" #'ignore
  "t" #'cerberus-append
  "T" #'cerberus-open-below
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
  "/" #'consult-line
  "k" #'ignore
  "h" #'ignore
  "," #'ignore
  "." #'ignore
  "-" #'ignore

  "<tab>" #'ignore
  "<return>" #'ignore
  "<escape>" #'keyboard-quit
  "SPC" #'hydra-leader/body)

(defvar-keymap cerberus-default-insert-keymap
  :doc "Keymap for Cerberus' insert mode"
  ;; "<tab>" #'completion-at-point
  "<escape>" #'cerberus-normal-mode)

(defvar-keymap cerberus-ts-normal-keymap
  :doc "Keymap for Cerberus' normal mode when treesit is active"
  :parent cerberus-default-normal-keymap
  
  "m" #'cerberus-up
  "n" #'cerberus-sentence-next
  "e" #'cerberus-sentence-prev
  "i" #'cerberus-down

  "o" #'cerberus-up

  "f" #'cerberus-word-next
  "b" #'cerberus-word-prev

  "k" #'cerberus-next
  "h" #'cerberus-prev

  "d" #'cerberus-delete

  "<up>" #'cerberus-move-node-up
  "<down>" #'cerberus-move-node-down)


(defvar cerberus--default-keymaps `((cerberus-normal-mode . ,cerberus-default-normal-keymap)
			(cerberus-insert-mode . ,cerberus-default-insert-keymap))
  "Keymap alist for Cerberus which is added to emulation-mode-map-alists.")

(defvar cerberus--treesit-override-keymaps nil)
(make-variable-buffer-local 'cerberus--treesit-override-keymaps)

(defvar cerberus--user-override-keymaps nil)
(make-variable-buffer-local 'cerberus--user-override-keymaps)

(defvar cerberus-use-motion-modes '(special-mode dired-mode compilation-mode)
  "List of major-modes where Cerberus defaults to the motion state")

(provide 'cerberus-vars)
