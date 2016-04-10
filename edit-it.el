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

(defun edit-it ()
  "Interactively edit the value of a symbol \(usually a list\)."
  (interactive)
  ;; TODO: show the symbol name in the buffer name
  (let* ((buf (get-buffer-create "*edit-it*"))
         (symbol-name 'auto-mode-alist))
    (switch-to-buffer buf)
    ;; TODO: is there a better way of getting a symbol value?
    (cl-prettyprint (eval symbol-name t))))


(provide 'edit-it)
;;; edit-it.el ends here
