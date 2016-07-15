;;; refine.el --- interactive value editing         -*- lexical-binding: t; -*-

;; TODO: prompt the user to choose between local and global variables
;; TODO: hook into customize when users are choosing new values.

;; Copyright (C) 2016  

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Version: 0.2
;; Keywords: convenience
;; Package-Requires: ((s "1.11.0") (dash "2.12.0") (list-utils "0.4.4") (magit-popup "2.7.0"))

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

;; refine provides an interactive UI for manipulating lists.

;;; Code:

(require 's)
(require 'dash)
(require 'list-utils)
(eval-when-compile
  (require 'cl)
  (require 'magit-popup))

(defun refine--variables ()
  "Return a list of all symbols that are variables."
  (let (symbols)
    (mapatoms (lambda (symbol)
                (when (boundp symbol)
                  (push symbol symbols))))
    symbols))

(defun refine--pretty-format (value)
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

(defun refine--eval (symbol)
  "Return the value of SYMBOL."
  (eval symbol t))

(defface refine-index-face
  '((((class color) (background light))
     :foreground "grey50")
    (((class color) (background dark))
     :foreground "grey50"))
  "Face for metadata in refine results buffers."
  :group 'refine)

(defun refine--vector->list (vector)
  "Shallow conversion from a vector to a list."
  (mapcar #'identity vector))

(defun refine--prefix-lines (prefix string)
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

(defun refine--format-element (element index-string)
  "Given ELEMENT, an item from a list, and INDEX-STRING,
a string marking our position in the containing list/vector,
return a pretty, propertized string."
  (let* (;; Pretty print ELEMENT.
         (formatted-element (refine--pretty-format element))
         ;; Style the index.
         (propertized-index (propertize index-string 'face 'refine-index-face)))
    (refine--prefix-lines
     (concat propertized-index " ") formatted-element)))

(defun refine--format-value (value)
  "Given a list or vector VALUE, return a pretty propertized
string listing the elements."
  (cond
   ((vectorp value)
    (refine--format-value (refine--vector->list value)))

   ((null value) "nil")

   ((refine--dotted-pair-p value)
    (format "%s\n%s"
            (refine--format-element (car value) "CAR")
            (refine--format-element (cdr value) "CDR")))

   (t
    (let* ((index-digits-required
            (if (null value) 0 (ceiling (log (safe-length value) 10))))
           ;; If there are 10 or more items, make sure we print the
           ;; index with a width of 2, and so on.
           (index-format-string (format "%%%dd " index-digits-required))
           ;; Pretty-print each element, along with an index.
           (formatted-elements
            (--map-indexed
             (refine--format-element it (format index-format-string it-index))
             value))
           ;; Propertize each element, so we can work out which element
           ;; point is on.
           (propertized-elements
            (--map-indexed (propertize it 'refine-index it-index)
                           formatted-elements)))
      (s-join "\n" propertized-elements)))))

(defun refine--update (buffer symbol)
  "Update BUFFER with the current value of SYMBOL."
  (with-current-buffer buffer
    (let* ((value (refine--eval symbol))
           (pos (point))
           buffer-read-only)
      (erase-buffer)
      (insert (format "%s:\n\n" (refine--describe symbol value)))
      (insert (refine--format-value value))
      (goto-char pos))))

(defvar-local refine--symbol nil
  "The symbol being inspected in the current buffer.")

(defun refine-update ()
  "Update the current refine buffer."
  (interactive)
  (unless (eq major-mode #'refine-mode)
    (user-error "refine-update must be run in a refine buffer"))
  (refine--update (current-buffer) refine--symbol))

(defun refine--insert-list (list index value)
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

(defun refine--insert (symbol index value)
  "Insert VALUE at INDEX in list variable SYMBOL.
This mutates the list.

If SYMBOL is nil, assigns to SYMBOL instead."
  (interactive)
  (assert (symbolp symbol))
  (let* ((list (refine--eval symbol))
         (length (safe-length list)))
    ;; `symbol' must be a list that's long enough.
    (assert (and (consp list) (>= length index)))

    (cond
     ;; If list is nil, we can't modify destructively.
     ((= length 0) (set symbol (list value)))

     (t (refine--insert-list list index value)))))

(defun refine--vector-pop (symbol index)
  "Remove the item at INDEX from vector variable SYMBOL.

This creates a new vector and assigns it to SYMBOL. Vectors have
fixed length, see *info* (elisp) Arrays."
  (let* ((vector (refine--eval symbol))
         (length (length vector)))
    (assert (and (vectorp vector) (< index length)))

    (let* ((all-items (refine--vector->list vector))
           (items (--reject (= it-index index) all-items)))
      (set symbol (apply #'vector items)))))


(defun refine--list-pop (list index)
  "Remove the item at INDEX from LIST.
This mutates the list."
  (let* ((length (safe-length list)))
    (assert (and (consp list) (> length 1) (< index length)))

    (if (equal index (1- length))
        ;; We're popping the very last cons cell.
        (progn
          (setq list (nthcdr (1- index) list))
          (setcdr list nil))
      (progn
        ;; Walk down the list until we're one element away from our
        ;; target.
        (setq list (nthcdr index list))
        
        ;; Mutate this cons cell to skip over the popped item and point
        ;; directly at the next cell.
        (-let [(_ new-car . new-cdr) list]
          (setcar list new-car)
          (setcdr list new-cdr))))))

(defun refine--pop (symbol index)
  "Remote the item at INDEX in vectory/list variable SYMBOL.
Mutates the value where possible."
  (let ((value (refine--eval symbol)))
    (cond ((vectorp value)
           (refine--vector-pop symbol index))
          ((equal (length value) 1)
           ;; We can't pop from a one-element list in-place, because a
           ;; cons cell requires a non-empty list.
           (set symbol nil))
          (t 
           (refine--list-pop value index)))))

(defun refine--index-at-point ()
  "Get the index of the list item at point."
  (save-excursion
    (when (eolp)
      (backward-char 1))
    (get-text-property (point) 'refine-index)))

(defun refine--read-eval-expr (prompt &optional initial-contents)
  "Read a lisp expression from the minibuffer and evaluate it.
Equivalent to interactive \"X\"."
  (eval (read--expression prompt initial-contents)))

(defun refine-delete ()
  "Remove the current list item at point."
  (interactive)
  (-when-let (list-index (refine--index-at-point))
    (refine--pop refine--symbol list-index)
    (refine-update)))

;; TODO: inserts should support vectors too.
(defun refine-insert (value)
  "Insert a new item before the list item at point."
  (interactive
   (let ((index (refine--index-at-point)))
     (if index
         (list (refine--read-eval-expr
                (format "Value to insert at %s: " (refine--index-at-point))))
       (user-error "No value here"))))
  (-when-let (list-index (refine--index-at-point))
    (refine--insert refine--symbol list-index value)
    (refine-update)))

(defun refine-insert-after (value)
  "Insert a new item before the list item at point."
  (interactive
   (let ((index (refine--index-at-point)))
     (if index
         (list (refine--read-eval-expr
                (format "Value to insert at %s: " (1+ (refine--index-at-point)))))
       (user-error "No value here"))))
  (-when-let (list-index (refine--index-at-point))
    (refine--insert refine--symbol (1+ list-index) value)
    (refine-update))
  (refine-next 1))

(defun refine--swap (index1 index2)
  "Switch the items at INDEX1 and INDEX2 in the current list."
  (let* ((value (refine--eval refine--symbol))
         (index1-element (nth index1 value))
         (index2-element (nth index2 value)))
    (setf (nth index2 value) index1-element)
    (setf (nth index1 value) index2-element)))

(defun refine-move-forward (arg)
  "Move the current item one position forward.
When called with a prefix, move that many positions."
  (interactive "p")
  ;; Move the element.
  (refine--move-element (refine--index-at-point) arg)
  (refine-update)
  ;; Move point to match.
  (refine-next arg))

(defun refine-move-backward (arg)
  "Move the current item one position forward.
When called with a prefix, move that many positions."
  (interactive "p")
  (refine-move-forward (- arg)))

;; TODO: extract all these internal manipulation functions to a
;; separate package. Each function should take a symbol rather than
;; implicitly using `refine--symbol'.
(defun refine--move-element (index distance)
  "Move the element at INDEX by DISTANCE positions.
If DISTANCE is too big, move it as far as possible."
  (let* ((value (refine--eval refine--symbol))
         (target-index-raw (+ index distance))
         ;; Ensure 0 <= target-index <= length - 1
         (target-index (max (min target-index-raw (1- (length value))) 0)))
    (while (not (equal index target-index))
      (if (> distance 0)
          ;; Moving forwards
          (progn
            (refine--swap index (1+ index))
            (incf index))
        ;; Moving backwards
        (progn
          (refine--swap index (1- index))
          (decf index))))))

(defun refine--move (distance)
  "Move point DISTANCE items forward.
If DISTANCE is negative, move backwards."
  (let* ( ;; Work out which list index to go to.
         (current-index (or (refine--index-at-point) -1))
         (requested-index (+ current-index distance))
         ;; Ensure we don't try to go outside the range allowed for
         ;; this list.
         (value (refine--eval refine--symbol))
         (target-index (max 0 (min requested-index (1- (safe-length value))))))
    (beginning-of-line)
    (if (> distance 0)
        ;; Go forwards until we're on the first line of the requested value.
        (while (or (null (refine--index-at-point))
                   (< (refine--index-at-point) target-index))
          (forward-line 1))
      ;; Go backwards until we're on the first line of the requested
      ;; value, even if it has multiple lines.
      (progn
        ;; Go to last line of the target value.
        (while (not (equal (refine--index-at-point) target-index))
          (forward-line -1))
        ;; Go past the target value.
        (while (equal (refine--index-at-point) target-index)
          (forward-line -1))
        ;; Move back to the first line of this value.
        (forward-line 1)))))

(defun refine-edit (new-value)
  "Edit the current item in the list."
  (interactive
   (let* ((lst (refine--eval refine--symbol))
          (current-value (nth (refine--index-at-point) lst)))
     (list (read--expression "New value: " (refine--pretty-format current-value)))))
  (eval
   `(setf (nth ,(refine--index-at-point) ,refine--symbol) ,new-value))
  (refine-update))

(defun refine-next (arg)
  "Move point to the next item.
With a numeric prefix, move that many items."
  (interactive "p")
  (refine--move arg))

(defun refine-previous (arg)
  "Move point to the previous item.
With a numeric prefix, move that many items."
  (interactive "p")
  (refine--move (- arg)))

(defun refine--buffer (symbol)
  "Get or create a refine buffer for SYMBOL."
  (assert (symbolp symbol))
  (let ((buffer (get-buffer-create (format "*refine: %s*" symbol))))
    (with-current-buffer buffer
      ;; Need to set the major mode before we local variables.
      (refine-mode)
      (setq-local refine--symbol symbol))
    buffer))

;; TODO: replace calls with just list-utils-improper-p
(defun refine--dotted-pair-p (value)
  "Return t if VALUE is a dotted pair."
  (and (consp value)
       (not (consp (cdr value))) (not (null (cdr value)))))

;; TODO: support hash maps
(defun refine--describe (symbol value)
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
(defun refine ()
  "Interactively edit the value of a symbol \(usually a list\)."
  (interactive)
  (let* ((symbol (read (completing-read "Variable: " (refine--variables))))
         (buf (refine--buffer symbol)))
    (refine--update buf symbol)
    (switch-to-buffer buf)))

(defvar refine-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" table)
    table))

(define-derived-mode refine-mode fundamental-mode "Refine"
  "A major mode for interactively editing elisp values."
  :syntax-table refine-mode-syntax-table
  (setq buffer-read-only t))

(magit-define-popup refine-popup
  "Popup console for changing items in a list."
  :actions '((?g "Reload" refine-update)
             (?d "Delete" refine-delete)))

;; TODO: get this popup working.
;; (define-key refine-mode-map (kbd "?") #'refine-popup)

;; Buffer-level operations.
(define-key refine-mode-map (kbd "q") #'kill-this-buffer)
(define-key refine-mode-map (kbd "g") #'refine-update)

;; Modifying the list.
(define-key refine-mode-map (kbd "e") #'refine-edit)
(define-key refine-mode-map (kbd "k") #'refine-delete)
(define-key refine-mode-map (kbd "a") #'refine-insert-after)
(define-key refine-mode-map (kbd "i") #'refine-insert)
;; Provide keybindings familiar to lispy users, as well as to move-dup users.
(define-key refine-mode-map (kbd "<M-down>") #'refine-move-forward)
(define-key refine-mode-map (kbd "s") #'refine-move-forward)
(define-key refine-mode-map (kbd "<M-up>") #'refine-move-backward)
(define-key refine-mode-map (kbd "w") #'refine-move-backward)

;; Moving around.
(define-key refine-mode-map (kbd "n") #'refine-next)
(define-key refine-mode-map (kbd "p") #'refine-previous)

(provide 'refine)
;;; refine.el ends here
