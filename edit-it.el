;;; edit-it.el --- interactive value editing         -*- lexical-binding: t; -*-

;; Copyright (C) 2016  

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Version: 0.1
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; edit-it provides an interactive UI for manipulating lists.

;;; Code:

(require 's)

(defun edit-it--variables ()
  "Return a list of all symbol names (strings) that are variables."
  (let ((symbols))
    (mapatoms (lambda (symbol)
                (when (boundp symbol)
                  (push symbol symbols))))
    symbols))

(defun edit-it--pretty-print (value)
  "Pretty print VALUE as a string."
  (with-temp-buffer
    (cl-prettyprint value)
    (s-trim (buffer-string))))

(defun edit-it--update (buffer symbol value)
  "Update BUFFER with the VALUE of SYMBOL."
  (with-current-buffer buffer
    (erase-buffer)
    (insert (edit-it--pretty-print value))))

(defun edit-it ()
  "Interactively edit the value of a symbol \(usually a list\)."
  (interactive)
  (let* ((symbol-name (completing-read "Variable: " (edit-it--variables)))
         (symbol-value (eval (read symbol-name) t))
         (buf (get-buffer-create (format "*edit-it: %s*" symbol-name))))
    (edit-it--update buf symbol-name symbol-value)
    (switch-to-buffer buf)
    (edit-it-mode)))

(defvar edit-it-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" table)
    table))

(define-derived-mode edit-it-mode fundamental-mode "Edit-It"
  "A major mode for interactively editing elisp values."
  :syntax-table edit-it-mode-syntax-table
  (font-lock-fontify-buffer))

(define-key edit-it-mode-map (kbd "q") #'kill-this-buffer)

(provide 'edit-it)
;;; edit-it.el ends here
