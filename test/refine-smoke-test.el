(require 'ert)
(require 'refine)

(ert-deftest refine-smoke-test ()
  "Smoke test to ensure that we can show a complex list."
  (refine 'auto-mode-alist)
  (refine-update)
  (refine-next 1)
  (refine-previous 1))

(ert-deftest refine-variables-not-functions ()
  (let ((vars (refine--variables)))
    (should (-contains-p vars 'kill-ring))
    (should (not (-contains-p vars 'message)))))
