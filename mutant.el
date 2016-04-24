;;; mutant.el --- interactive value editing         -*- lexical-binding: t; -*-

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

;; mutant provides an interactive UI for manipulating lists.

;;; Code:

(require 's)
(require 'dash)
(require 'list-utils)
(eval-when-compile
  (require 'cl)
  (require 'magit-popup))

(defun mutant--variables ()
  "Return a list of all symbol names (strings) that are variables."
  (let ((symbols))
    (mapatoms (lambda (symbol)
                (when (boundp symbol)
                  (push symbol symbols))))
    symbols))

(defun mutant--pretty-print (value)
  "Pretty print VALUE as a string."
  (with-temp-buffer
    (cl-prettyprint value)
    (s-trim (buffer-string))))

(defun mutant--update (buffer symbol)
  "Update BUFFER with the current value of SYMBOL."
  (let ((value (eval symbol t)))
    (with-current-buffer buffer
      (let (buffer-read-only)
        (erase-buffer)
        (insert (format "%s is %s\n\n" symbol
                        (mutant--describe value)))
        ;; TODO: Handle empty lists and non-lists, we shouldn't
        ;; propertize anything in those cases.
        (--map-indexed
         (insert
          (propertize it 'mutant-index it-index)
          "\n")
         (s-lines (mutant--pretty-print value)))
        ;; TODO: preserve point position
        (goto-char (point-min))))))

(defvar-local mutant--symbol nil
  "The symbol being inspected in the current buffer.")

(defun mutant-update ()
  "Update the current mutant buffer."
  (interactive)
  ;; TODO: assert we're in mutant mode.
  (mutant--update (current-buffer) mutant--symbol))

(defun mutant--insert (symbol index value)
  "Insert VALUE at INDEX in list variable SYMBOL.
This mutates the list.

If SYMBOL is nil, assigns to SYMBOL instead."
  (interactive)
  (assert (symbolp symbol))
  (let* ((list (eval symbol t))
         (length (safe-length list)))
    ;; `symbol' must be a list that's long enough.
    (assert (and (consp list) (>= length index)))

    (cond
     ;; If list is nil, we can't modify destructively.
     ((= length 0) (set symbol (list value)))

     ;; Append to the list.
     ((= index length)
      ;; Find the last cons cell.
      (while (cdr list)
        (setq list (cdr list)))
      ;; Append the value requested.
      (setcdr list (cons value nil)))

     ;; Insert inplace.
     (t
      ;; Walk down the list until we're one element away from our
      ;; target.
      (while (>= index 1)
        (setq list (cdr list))
        (setq index (1- index)))
      ;; Mutate this cons cell.
      (-let [(old-car . old-cdr) list]
        (setcar list value)
        (setcdr list (cons old-car old-cdr)))))))

(defun mutant--pop (symbol index)
  "Remove the item at INDEX from list variable SYMBOL.
This mutates the list.

If the list only has one element, assign nil to SYMBOL instead."
  ;; TODO: factor out an eval wrapper function.
  (assert (symbolp symbol))
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

(defun mutant-delete ()
  "Remove the current list item at point."
  (interactive)
  (-when-let (list-index (get-text-property (point) 'mutant-index))
    (mutant--pop mutant--symbol list-index)
    (mutant-update)))

(defun mutant-insert-before (value)
  "Insert a new item before the list item at point."
  (interactive "XValue to insert before this: ")
  (-when-let (list-index (get-text-property (point) 'mutant-index))
    (mutant--insert mutant--symbol list-index value)
    (mutant-update)))

(defun mutant-insert-after (value)
  "Insert a new item before the list item at point."
  (interactive "XValue to insert after this: ")
  (-when-let (list-index (get-text-property (point) 'mutant-index))
    (mutant--insert mutant--symbol (1+ list-index) value)
    (mutant-update)))

(defun mutant--buffer (symbol)
  "Get or create an mutant buffer for SYMBOL."
  (assert (symbolp symbol))
  (let ((buffer (get-buffer-create (format "*mutant: %s*" symbol))))
    (with-current-buffer buffer
      ;; Need to set the major mode before we local variables.
      (mutant-mode)
      (setq mutant--symbol symbol))
    buffer))

(defun mutant--describe (value)
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

;;;###autoload
(defun mutant ()
  "Interactively edit the value of a symbol \(usually a list\)."
  (interactive)
  (let* ((symbol (read (completing-read "Variable: " (mutant--variables))))
         (buf (mutant--buffer symbol)))
    (mutant--update buf symbol)
    (switch-to-buffer buf)))

(defvar mutant-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" table)
    table))

(define-derived-mode mutant-mode fundamental-mode "Mutant"
  "A major mode for interactively editing elisp values."
  :syntax-table mutant-mode-syntax-table
  (font-lock-fontify-buffer)
  (setq buffer-read-only t))

(magit-define-popup mutant-popup
  "Popup console for changing items in a list."
  :actions '((?g "Reload" mutant-update)
             (?d "Delete" mutant-delete)))

(define-key mutant-mode-map (kbd "q") #'kill-this-buffer)
(define-key mutant-mode-map (kbd "?") #'mutant-popup)
(define-key mutant-mode-map (kbd "g") #'mutant-update)
(define-key mutant-mode-map (kbd "d") #'mutant-delete)
(define-key mutant-mode-map (kbd "b") #'mutant-insert-before)
(define-key mutant-mode-map (kbd "a") #'mutant-insert-after)

(provide 'mutant)
;;; mutant.el ends here
