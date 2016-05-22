;;; mutant.el --- interactive value editing         -*- lexical-binding: t; -*-

;; TODO: global variables
;; TODO: hook into customize somehow

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
  "Return a list of all symbols that are variables."
  (let ((symbols))
    (mapatoms (lambda (symbol)
                (when (boundp symbol)
                  (push symbol symbols))))
    symbols))

(defun mutant--pretty-format (value)
  "Pretty print VALUE as a string."
  (let ((cl-formatted (with-temp-buffer
                        (cl-prettyprint value)
                        (s-trim (buffer-string)))))
    (cond ((stringp value)
           ;; Don't use cl-prettyprint for strings, as we want to see
           ;; fontified text, not escaped literals like
           ;; #("f" 0 1 (face font-lock-keyword-face))
           (format "\"%s\"" value)) ;; todo: escape " inside the
          ;; string Display symbols and lists with a quote, so we show
          ;; usable syntax.
          ((or (symbolp value) (consp value))
           (format "'%s" cl-formatted))
          (t
           cl-formatted))))

(defun mutant--eval (symbol)
  "Return the value of SYMBOL."
  ;; TODO: handle buffer-local variables
  (eval symbol t))

(defface mutant-dim-face
  '((((class color) (background light))
     :foreground "grey50")
    (((class color) (background dark))
     :foreground "grey50"))
  "Face for metadata in ag results buffers."
  :group 'mutant)

(defun mutant--vector->list (vector)
  "Shallow conversion from a vector to a list."
  (mapcar #'identity vector))

(defun mutant--format-value (value)
  "Given a list or vector VALUE, return a pretty propertized
string listing the elements."
  (when (vectorp value)
    (setq value (mutant--vector->list value)))

  (let* ((length (length value))
         (index-digits-required (ceiling (log length 10)))
         ;; If there are 10 or more items, make sure we print the
         ;; index with a width of 2, and so on.
         (index-format-string (format "%%%dd" index-digits-required)))
    (s-join
     "\n"
     (--map-indexed
      (let* ((index (propertize
                     (format index-format-string it-index)
                     'face 'mutant-dim-face))
             (raw-line (format
                        "%s %s"
                        index
                        (mutant--pretty-format it))))
        (propertize raw-line 'mutant-index it-index))
      value))))

(defun mutant--update (buffer symbol)
  "Update BUFFER with the current value of SYMBOL."
  (with-current-buffer buffer
    (let* ((value (mutant--eval symbol))
           (pos (point))
           buffer-read-only)
      (erase-buffer)
      (insert (format "%s is %s:\n\n" symbol
                      (mutant--describe value)))
      (insert (mutant--format-value value))
      (goto-char pos))))

(defvar-local mutant--symbol nil
  "The symbol being inspected in the current buffer.")

(defun mutant-update ()
  "Update the current mutant buffer."
  (interactive)
  (unless (eq major-mode #'mutant-mode)
    (user-error "mutant-update must be run in a mutant buffer"))
  (mutant--update (current-buffer) mutant--symbol))

(defun mutant--insert (symbol index value)
  "Insert VALUE at INDEX in list variable SYMBOL.
This mutates the list.

If SYMBOL is nil, assigns to SYMBOL instead."
  (interactive)
  (assert (symbolp symbol))
  (let* ((list (mutant--eval symbol))
         (length (safe-length list)))
    ;; `symbol' must be a list that's long enough.
    (assert (and (consp list) (>= length index)))

    (cond
     ;; If list is nil, we can't modify destructively.
     ((= length 0) (set symbol (list value)))

     ;; Append to the list.
     ((= index length)
      ;; Find the last cons cell.
      (setq list (last list))
      ;; Append the value requested.
      (setcdr list (cons value nil)))

     ;; Insert inplace.
     (t
      ;; Walk down the list until we're one element away from our
      ;; target.
      (setq list (nthcdr index list))
      ;; Mutate this cons cell.
      (-let [(old-car . old-cdr) list]
        (setcar list value)
        (setcdr list (cons old-car old-cdr)))))))

(defun mutant--vector-pop (symbol index)
  "Remove the item at INDEX from vector variable SYMBOL."
  ;; TODO: this isn't in-place. Can we make it in-place?
  (let* ((vector (mutant--eval symbol))
         (length (length vector)))
    (assert (and (vectorp vector) (< index length)))

    (let* ((all-items (mutant--vector->list vector))
           (items (--reject (= it-index index) all-items)))
      (set symbol (apply #'vector items)))))


(defun mutant--list-pop (symbol index)
  "Remove the item at INDEX from list variable SYMBOL.
This mutates the list.

If the list only has one element, assign nil to SYMBOL instead."
  (assert (symbolp symbol))
  (let* ((list (mutant--eval symbol))
         (length (safe-length list)))
    ;; `symbol' must be a list that's long enough.
    (assert (and (consp list) (< index length)))

    (cond
     ((= length 0) (user-error "Can't pop from an empty list"))
     ((= length 1) (set symbol nil))
     (t
      ;; Walk down the list until we're one element away from our
      ;; target.
      (setq list (nthcdr index list))
      ;; Mutate this cons cell.
      (-let [(_ new-car . new-cdr) list]
        (setcar list new-car)
        (setcdr list new-cdr))))))

(defun mutant--pop (symbol index)
  "Remote the item at INDEX in vectory/list variable SYMBOL.
Mutates the value where possible."
  (if (vectorp (mutant--eval symbol))
      (mutant--vector-pop symbol index)
    (mutant--list-pop symbol index)))

(defun mutant-delete ()
  "Remove the current list item at point."
  (interactive)
  (-when-let (list-index (get-text-property (point) 'mutant-index))
    (mutant--pop mutant--symbol list-index)
    (mutant-update)))

;; TODO: inserts should support vectors too.
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
  "Get or create a mutant buffer for SYMBOL."
  (assert (symbolp symbol))
  (let ((buffer (get-buffer-create (format "*mutant: %s*" symbol))))
    (with-current-buffer buffer
      ;; Need to set the major mode before we local variables.
      (mutant-mode)
      (setq mutant--symbol symbol))
    buffer))

;; TODO: support hash maps
(defun mutant--describe (value)
  "Return a human-readable description for VALUE."
  (cond
   ((stringp value) "a string")
   ((and (consp value) (list-utils-cyclic-p value))
    "an improper list")
   ((sequencep value)
    (let* ((type (if (vectorp value) "vector" "list"))
           (length (length value))
           (units (if (= length 1) "value" "values")))
      (format "a %s containing %d %s"
              type length units)))
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
  (setq buffer-read-only t))

(magit-define-popup mutant-popup
  "Popup console for changing items in a list."
  :actions '((?g "Reload" mutant-update)
             (?d "Delete" mutant-delete)))

;; TODO: n and p navigation
(define-key mutant-mode-map (kbd "q") #'kill-this-buffer)
(define-key mutant-mode-map (kbd "?") #'mutant-popup)
(define-key mutant-mode-map (kbd "g") #'mutant-update)
(define-key mutant-mode-map (kbd "d") #'mutant-delete)
(define-key mutant-mode-map (kbd "a") #'mutant-insert-after)
(define-key mutant-mode-map (kbd "i") #'mutant-insert-before)

(provide 'mutant)
;;; mutant.el ends here
