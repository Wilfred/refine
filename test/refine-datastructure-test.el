(require 'ert)
(require 'mutant)

;; It's much easier to have a global variable to test against, sadly.
(defvar test-list nil)

(ert-deftest mutant-insert ()
  (setq test-list (list 1 2 3))
  (let ((original test-list))
    (mutant--insert 'test-list 1 "foo")
    ;; We should have inserted the value.
    (should (equal test-list (list 1 "foo" 2 3)))
    ;; We should have mutated in place, so we still have the same cons
    ;; cell.
    (should (eq test-list original))
    ;; The tails should also be the same cons cell.
    (should (eq (last test-list) (last original)))))

(ert-deftest mutant-list-insert-at-end ()
  "Test we handle the append case correctly."
  (setq test-list (list 1 2 3))
  (let ((original test-list))
    (mutant--insert 'test-list 3 "foo")
    (should (equal test-list (list 1 2 3 "foo")))))

(ert-deftest mutant-pop ()
  (setq test-list (list 1 2 3))
  (let ((original test-list))
    (mutant--pop 'test-list 1)
    ;; We should have popped the value
    (should (equal test-list (list 1 3)))
    ;; We should have mutated in place, so we still have the same cons
    ;; cell.
    (should (eq test-list original))
    ;; The tails should also be the same cons cell.
    (should (eq (last test-list) (last original)))))
