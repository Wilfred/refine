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

(defun edit-it--update (buffer symbol)
  "Update BUFFER with the current value of SYMBOL."
  (let ((value (eval symbol t)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "%s is %s\n\n" symbol
                      (edit-it--describe value)))
      ;; TODO: Handle empty lists and non-lists, we shouldn't
      ;; propertize anything in those cases.
      (--map-indexed
       (insert
        (propertize it 'edit-it-index it-index)
        "\n")
       (s-lines (edit-it--pretty-print value)))
      ;; TODO: preserve point position
      (goto-char (point-min)))))

(defun edit-it-update ()
  "Update the current edit-it buffer."
  (interactive)
  ;; TODO: assert we're in edit-it mode.
  (edit-it--update (current-buffer) edit-it--symbol))

(defun edit-it--pop (symbol index)
  "Remove the item at INDEX from list variable SYMBOL.
This mutates the list.

If the list only has one element, assign nil to SYMBOL instead."
  ;; TODO: factor out an eval wrapper function.
  (let* ((list (eval symbol t))
         (length (safe-length list)))
    ;; `symbol' must be a list that's long enough.
    (assert (and (consp list) (> length index)))

    (cond
     ((= length 0) (user-error "Can't pop from an empty list"))
     ((= length 1) (set symbol nil))
     (t
      ;; Walk down the list until we're one element away from our
      ;; target.
      (while (>= index 1)
        (setq list (cdr list))
        (setq index (1- index)))
      (-let [(_ new-car . new-cdr) list]
        (setcar list new-car)
        (setcdr list new-cdr))))))

(defun edit-it-delete ()
  "Remove the current list item at point."
  (interactive)
  (-when-let (list-index (get-text-property (point) 'edit-it-index))
    (edit-it--pop edit-it--symbol list-index)
    (edit-it-update)))

(defvar-local edit-it--symbol nil
  "The symbol being inspected in the current buffer.")

(defun edit-it--buffer (symbol)
  "Get or create an edit-it buffer for SYMBOL."
  (assert (symbolp symbol))
  (let ((buffer (get-buffer-create (format "*edit-it: %s*" symbol))))
    (with-current-buffer buffer
      ;; Need to set the major mode before we local variables.
      (edit-it-mode)
      (setq edit-it--symbol symbol))
    buffer))

(defun edit-it--describe (value)
  "Return a human-readable description for VALUE."
  (cond
   ((stringp value) "a string")
   ((consp value) (if (list-utils-cyclic-p value)
                      "an improper list"
                    (format "a list containing %d values"
                            (length value))))
   ((vectorp value) "a vector")
   ((null value) "nil")
   (:else "an unsupported type")))

(defun edit-it ()
  "Interactively edit the value of a symbol \(usually a list\)."
  (interactive)
  (let* ((symbol (read (completing-read "Variable: " (edit-it--variables))))
         (buf (edit-it--buffer symbol)))
    (edit-it--update buf symbol)
    (switch-to-buffer buf)))

(defvar edit-it-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" table)
    table))

(define-derived-mode edit-it-mode fundamental-mode "Edit-It"
  "A major mode for interactively editing elisp values."
  :syntax-table edit-it-mode-syntax-table
  (font-lock-fontify-buffer))

(define-key edit-it-mode-map (kbd "q") #'kill-this-buffer)
(define-key edit-it-mode-map (kbd "g") #'edit-it-update)
(define-key edit-it-mode-map (kbd "d") #'edit-it-delete)

(provide 'edit-it)
;;; edit-it.el ends here
