(require 'ert)
(require 'refine)

(ert-deftest refine-smoke-test ()
  "Smoke test to ensure that we can show a complex list."
  (refine 'auto-mode-alist)
  (refine-update)
  (refine-next 1)
  (refine-previous 1))

(defvar refine--test-var)

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

;; TODO: move to a better file.
(ert-deftest refine-variables-not-functions ()
  (let ((vars (refine--variables)))
    (should (-contains-p vars 'kill-ring))
    (should (not (-contains-p vars 'message)))))
