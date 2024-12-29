;;; cerberus-util.el --- Utility functions for Cerberus -*- lexical-binding: t; -*-

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

(defun cerberus--alist-put (alist key value)
  ())

(defun cerberus--one-closer-to-0 (n)
  (- n (cl-signum n)))

(defun cerberus--alist-put (alist key value)
  (cons (cons key value) (assq-delete-all key (copy-alist alist))))

(defun cerberus--alist-get (alist key)
  (cdr (assq key alist)))

(defun cerberus--alist-member (alist key)
  (member key (mapcar #'car alist)))

(defun cerberus-lang-def-things (lang definitions)
  (let ((new-defs (cerberus--alist-get cerberus--thing-definitions lang)))
    (dolist (def definitions)
      (setq new-defs (cerberus--alist-put new-defs (car def) (cdr def))))
  (setq cerberus--thing-definitions
	(cerberus--alist-put cerberus--thing-definitions lang new-defs))))

;; (let ((lang-settings (cdr (assq lang cerberus--thing-definitions)))
;;       (other-settings (assq-delete-all lang cerberus--thing-definitions)))
;;   (dolist (definition definitions)
;;     (setq lang-settings (cons definition (assq-delete-all (car definition) lang-settings))))
;;   (setq cerberus--thing-definitions (cons `(,lang . ,lang-settings) other-settings))))

(provide 'cerberus-util)

