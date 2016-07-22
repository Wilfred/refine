(require 'ert)
(require 'refine)

(ert-deftest refine-smoke-test ()
  "Smoke test to ensure that we can show a complex list."
  (refine 'auto-mode-alist))

(defvar refine--test-var)

(ert-deftest refine-update-preserves-position ()
  "`refine-update' should not move point, even if the value
description changes."
  ;; First, show a refine buffer on a nil value.
  (setq refine--test-var nil)
  (refine 'refine--test-var)
  ;; Move point to the value.
  (refine-next 1)
  (let ((start-line (line-number-at-pos)))
    ;; Set the variable to a list, and update.
    (setq refine--test-var '(1 2 3 4))
    (refine-update)
    ;; Point should still be on the same line.
    (should (equal start-line (line-number-at-pos)))))

(ert-deftest refine-next-symbol-value ()
  "`refine-update' should not move point, even if the value
description changes."
  (setq refine--test-var 'foo)
  (refine 'refine--test-var)
  ;; Move point to the value.
  ;; Warning: when this is broken, it's often an infinite loop!
  (refine-next 1))

(ert-deftest refine-insert-empty-list ()
  "Smoke test to ensure that we can insert into an empty list."
  ;; Open refine on an empty list.
  (setq refine--test-var nil)
  (refine 'refine--test-var)
  ;; Move to the list itself.
  (refine-next 1)
  (refine-insert 'a)
  ;; Verify that we inserted the value we expected.
  (should (equal refine--test-var
                 (list 'a)))
  ;; We should have point positioned on the newly inserted item.
  (should (looking-at "0 'a")))

(defcustom refine--test-var-no-choices '(x y)
  "Apparently cask insists on a docstring here."
  :type '(repeat (choice symbol)))

(ert-deftest refine-cycle-but-symbols-without-choices ()
  "We should call user-error if defcustom does not specify
specific possibilities."
  (refine 'refine--test-var-no-choices)
  ;; Move to the first item.
  (refine-next 1)
  (let ((user-error-called nil))
    (condition-case err
        (refine-cycle)
      (user-error (setq user-error-called t)))
    (should user-error-called)))

(defcustom refine--test-var-choices-set '(baz)
  "Apparently cask insists on a docstring here."
  :type '(set
          (const :tag "Foo" foo)
          (const :tag "Bar" bar)
          (const :tag "Baz" baz)))

;; TODO: should we be smarter and prevent users inserting duplicates?
(ert-deftest refine-cycle-choices-set ()
  "If our options are a set, we should still cycle."
  (refine 'refine--test-var-choices-set)
  ;; Move point to the first value.
  (refine-next 1)
  ;; Cycle once, which should wrap around.
  (refine-cycle)
  (should (equal refine--test-var-choices-set '(foo))))

;; TODO: move to a better file.
(ert-deftest refine-variables-not-functions ()
  (let ((vars (refine--variables)))
    (should (-contains-p vars 'kill-ring))
    (should (not (-contains-p vars 'message)))))
