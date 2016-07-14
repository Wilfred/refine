;;; mutant.el --- interactive value editing         -*- lexical-binding: t; -*-

;; TODO: prompt the user to choose between local and global variables
;; TODO: hook into customize when users are choosing new values.

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
  (let (symbols)
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
           (format "\"%s\"" (s-replace "\"" "\\\"" value)))
          ;; Print nil and t as-is.'
          ((or (eq t value) (eq nil value))
           (format "%s" value))
          ;; Display other symbols, and lists, with a quote, so we
          ;; show usable syntax.
          ((or (symbolp value) (consp value))
           (format "'%s" cl-formatted))
          (t
           cl-formatted))))

(defun mutant--eval (symbol)
  "Return the value of SYMBOL."
  ;; TODO: handle buffer-local variables
  (eval symbol t))

;; TODO: name for index, i.e. purpose, not colour.
(defface mutant-dim-face
  '((((class color) (background light))
     :foreground "grey50")
    (((class color) (background dark))
     :foreground "grey50"))
  "Face for metadata in mutant results buffers."
  :group 'mutant)

(defun mutant--vector->list (vector)
  "Shallow conversion from a vector to a list."
  (mapcar #'identity vector))

(defun mutant--prefix-lines (prefix string)
  "Return STRING with PREFIX prepended on the first line.
If STRING contains multiple lines, indent subsequent lines
to preserve vertical indentation."
  (let* ((raw-lines (s-lines string))
         ;; Whitespace of the same length as PREFIX.
         (leading-whitespace (make-string (length prefix) ?\ ))
         ;; Append a prefix to each line.
         (prefixes
          (cons prefix
                (-repeat (1- (length raw-lines)) leading-whitespace)))
         (lines (--zip-with (concat it other) prefixes raw-lines)))
    (s-join "\n" lines)))

(defun mutant--format-element (element index-string)
  "Given ELEMENT, an item from a list, and INDEX-STRING,
a string marking our position in the containing list/vector,
return a pretty, propertized string."
  (let* (;; Pretty print ELEMENT.
         (formatted-element (mutant--pretty-format element))
         ;; Style the index.
         (propertized-index (propertize index-string 'face 'mutant-dim-face)))
    (mutant--prefix-lines
     (concat propertized-index " ") formatted-element)))

(defun mutant--format-value (value)
  "Given a list or vector VALUE, return a pretty propertized
string listing the elements."
  (cond
   ((vectorp value)
    (mutant--format-value (mutant--vector->list value)))

   ((null value) "nil")

   ((mutant--dotted-pair-p value)
    (format "%s\n%s"
            (mutant--format-element (car value) "CAR")
            (mutant--format-element (cdr value) "CDR")))

   (t
    (let* ((index-digits-required
            (if (null value) 0 (ceiling (log (safe-length value) 10))))
           ;; If there are 10 or more items, make sure we print the
           ;; index with a width of 2, and so on.
           (index-format-string (format "%%%dd " index-digits-required))
           ;; Pretty-print each element, along with an index.
           (formatted-elements
            (--map-indexed
             (mutant--format-element it (format index-format-string it-index))
             value))
           ;; Propertize each element, so we can work out which element
           ;; point is on.
           (propertized-elements
            (--map-indexed (propertize it 'mutant-index it-index)
                           formatted-elements)))
      (s-join "\n" propertized-elements)))))

(defun mutant--update (buffer symbol)
  "Update BUFFER with the current value of SYMBOL."
  (with-current-buffer buffer
    (let* ((value (mutant--eval symbol))
           (pos (point))
           buffer-read-only)
      (erase-buffer)
      (insert (format "%s:\n\n" (mutant--describe symbol value)))
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

(defun mutant--insert-list (list index value)
  "Insert VALUE at INDEX in LIST.
This mutates the list."
  ;; We can't handle empty lists: there's no cons cell to mutate.
  (assert (consp list))

  (if (= index (length list))
      ;; We're appending to the list.
      (progn
        ;; Find the last cons cell.
        (setq list (last list))
        ;; Append the value requested.
        (setcdr list (cons value nil)))
    ;; Otherwise, insert in-place.
    (progn
      ;; Walk down the list until we're one element away from our
      ;; target.
      (setq list (nthcdr index list))
      ;; Mutate this cons cell.
      (-let [(old-car . old-cdr) list]
        (setcar list value)
        (setcdr list (cons old-car old-cdr))))))

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

     (t (mutant--insert-list list index value)))))

(defun mutant--vector-pop (symbol index)
  "Remove the item at INDEX from vector variable SYMBOL.

This creates a new vector and assigns it to SYMBOL. Vectors have
fixed length, see *info* (elisp) Arrays."
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

(defun mutant--index-at-point ()
  (get-text-property (point) 'mutant-index))

(defun mutant-delete ()
  "Remove the current list item at point."
  (interactive)
  (-when-let (list-index (mutant--index-at-point))
    (mutant--pop mutant--symbol list-index)
    (mutant-update)))

;; TODO: inserts should support vectors too.
(defun mutant-insert (value)
  "Insert a new item before the list item at point."
  (interactive "XValue to insert before this: ")
  (-when-let (list-index (mutant--index-at-point))
    (mutant--insert mutant--symbol list-index value)
    (mutant-update)))

(defun mutant-insert-after (value)
  "Insert a new item before the list item at point."
  (interactive "XValue to insert after this: ")
  (-when-let (list-index (mutant--index-at-point))
    (mutant--insert mutant--symbol (1+ list-index) value)
    (mutant-update)))

(defun mutant--swap (index1 index2)
  "Switch the items at INDEX1 and INDEX2 in the current list."
  (let* ((value (mutant--eval mutant--symbol))
         (index1-element (nth index1 value))
         (index2-element (nth index2 value)))
    (setf (nth index2 value) index1-element)
    (setf (nth index1 value) index2-element)))

(defun mutant-move-forward (arg)
  "Move the current item one position forward.
When called with a prefix, move that many positions."
  (interactive "p")
  ;; Move the element.
  (mutant--move-element (mutant--index-at-point) arg)
  (mutant-update)
  ;; Move point to match.
  (mutant-next arg))

(defun mutant-move-backward (arg)
  "Move the current item one position forward.
When called with a prefix, move that many positions."
  (interactive "p")
  (mutant-move-forward (- arg)))

;; TODO: extract all these internal manipulation functions to a
;; separate package. Each function should take a symbol rather than
;; implicitly using `mutant--symbol'.
(defun mutant--move-element (index distance)
  "Move the element at INDEX by DISTANCE positions.
If DISTANCE is too big, move it as far as possible."
  (let* ((value (mutant--eval mutant--symbol))
         (target-index-raw (+ index distance))
         ;; Ensure 0 <= target-index <= length - 1
         (target-index (max (min target-index-raw (1- (length value))) 0)))
    (while (not (equal index target-index))
      (if (> distance 0)
          ;; Moving forwards
          (progn
            (mutant--swap index (1+ index))
            (incf index))
        ;; Moving backwards
        (progn
          (mutant--swap index (1- index))
          (decf index))))))

(defun mutant--move (distance)
  "Move point DISTANCE items forward.
If DISTANCE is negative, move backwards."
  (let* ( ;; Work out which list index to go to.
         (current-index (mutant--index-at-point))
         (requested-index (+ current-index distance))
         ;; Ensure we don't try to go outside the range allowed for
         ;; this list.
         (value (mutant--eval mutant--symbol))
         (target-index (max 0 (min requested-index (safe-length value)))))
    (beginning-of-line)
    (if (> distance 0)
        ;; Go forwards until we're on the first line of the requested value.
        (while (not (equal (mutant--index-at-point) target-index))
          (forward-line 1))
      ;; Go backwards until we're on the first line of the requested
      ;; value, even if it has multiple lines.
      (progn
        ;; Go to last line of the target value.
        (while (not (equal (mutant--index-at-point) target-index))
          (forward-line -1))
        ;; Go past the target value.
        (while (equal (mutant--index-at-point) target-index)
          (forward-line -1))
        ;; Move back to the first line of this value.
        (forward-line 1)))))

(defun mutant-edit (new-value)
  "Edit the current item in the list."
  (interactive
   (let* ((lst (mutant--eval mutant--symbol))
          (current-value (nth (mutant--index-at-point) lst)))
     (list (read--expression "New value: " (mutant--pretty-format current-value)))))
  (eval
   `(setf (nth ,(mutant--index-at-point) ,mutant--symbol) ,new-value))
  (mutant-update))

(defun mutant-next (arg)
  "Move point to the next item.
With a numeric prefix, move that many items."
  (interactive "p")
  (mutant--move arg))

(defun mutant-previous (arg)
  "Move point to the previous item.
With a numeric prefix, move that many items."
  (interactive "p")
  (mutant--move (- arg)))

(defun mutant--buffer (symbol)
  "Get or create a mutant buffer for SYMBOL."
  (assert (symbolp symbol))
  (let ((buffer (get-buffer-create (format "*mutant: %s*" symbol))))
    (with-current-buffer buffer
      ;; Need to set the major mode before we local variables.
      (mutant-mode)
      (setq-local mutant--symbol symbol))
    buffer))

;; TODO: replace calls with just list-utils-improper-p
(defun mutant--dotted-pair-p (value)
  "Return t if VALUE is a dotted pair."
  (and (consp value)
       (not (consp (cdr value))) (not (null (cdr value)))))

;; TODO: support hash maps
(defun mutant--describe (symbol value)
  "Return a human-readable description for SYMBOL set to VALUE."
  (let ((pretty-symbol
         (propertize (format "%s" symbol)
                     'face 'font-lock-variable-name-face))
        (symbol-descripton
         (if (local-variable-p symbol)
             (format "a local variable in buffer %s" (current-buffer))
           "a global variable"))
        (type-description
         (cond
          ((stringp value) "a string")
          ((null value) "nil")
          ((and (consp value) (not (consp (cdr value))) (not (null (cdr value))))
           "a pair")
          ((and (consp value) (list-utils-cyclic-p value))
           "an improper list")
          ((sequencep value)
           (let* ((type (if (vectorp value) "vector" "list"))
                  (length (length value))
                  (units (if (= length 1) "value" "values")))
             (format "a %s containing %d %s"
                     type length units)))
          (:else "an unsupported type"))))
    (s-word-wrap 60
                 (format "%s is %s. Its current value is %s"
                         pretty-symbol symbol-descripton
                         type-description))))

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

;; TODO: get this popup working.
;; (define-key mutant-mode-map (kbd "?") #'mutant-popup)

;; Buffer-level operations.
(define-key mutant-mode-map (kbd "q") #'kill-this-buffer)
(define-key mutant-mode-map (kbd "g") #'mutant-update)

;; Modifying the list.
(define-key mutant-mode-map (kbd "e") #'mutant-edit)
(define-key mutant-mode-map (kbd "d") #'mutant-delete)
(define-key mutant-mode-map (kbd "a") #'mutant-insert-after)
(define-key mutant-mode-map (kbd "i") #'mutant-insert)
;; Provide keybindings familiar to lispy users, as well as to move-dup users.
(define-key mutant-mode-map (kbd "<M-down>") #'mutant-move-forward)
(define-key mutant-mode-map (kbd "s") #'mutant-move-forward)
(define-key mutant-mode-map (kbd "<M-up>") #'mutant-move-backward)
(define-key mutant-mode-map (kbd "w") #'mutant-move-backward)

;; Moving around.
(define-key mutant-mode-map (kbd "n") #'mutant-next)
(define-key mutant-mode-map (kbd "p") #'mutant-previous)

(provide 'mutant)
;;; mutant.el ends here
