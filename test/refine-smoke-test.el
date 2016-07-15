(require 'ert)
(require 'refine)

(ert-deftest refine-auto-mode-alist ()
  "Smoke test to ensure that we can show a complex list."
  (refine 'auto-mode-alist))

(ert-deftest refine-variables-not-functions ()
  (let ((vars (refine--variables)))
    (should (-contains-p vars 'kill-ring))
    (should (not (-contains-p vars 'message)))))
