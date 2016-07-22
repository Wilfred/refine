(require 'ert)
(require 'refine)

;; It's much easier to have a global variable to test against, sadly.
(defvar test-list nil)

(ert-deftest refine-insert ()
  (setq test-list (list 1 2 3))
  (let ((original test-list))
    (refine--insert 'test-list 1 "foo")
    ;; We should have inserted the value.
    (should (equal test-list (list 1 "foo" 2 3)))
    ;; We should have mutated in place, so we still have the same cons
    ;; cell.
    (should (eq test-list original))
    ;; The tails should also be the same cons cell.
    (should (eq (last test-list) (last original)))))

(ert-deftest refine-list-insert-at-end ()
  "Test we handle the append case correctly."
  (setq test-list (list 1 2 3))
  (let ((original test-list))
    (refine--insert 'test-list 3 "foo")
    (should (equal test-list (list 1 2 3 "foo")))))

(ert-deftest refine-pop ()
  (setq test-list (list 1 2 3))
  (let ((original test-list))
    (refine--pop 'test-list 1)
    ;; We should have popped the value
    (should (equal test-list (list 1 3)))
    ;; We should have mutated in place, so we still have the same cons
    ;; cell.
    (should (eq test-list original))
    ;; The tails should also be the same cons cell.
    (should (eq (last test-list) (last original)))))

(ert-deftest refine-move-element ()
  (let ((my-list '(a b c d e)))
    ;; Move 'b two positions forward.
    (refine--move-element my-list 1 2)
    (should
     (equal my-list '(a c d b e)))))
