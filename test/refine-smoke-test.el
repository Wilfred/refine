(require 'ert)
(require 'refine)

(ert-deftest refine-auto-mode-alist ()
  "Smoke test to ensure that we can show a complex list."
  (refine 'auto-mode-alist))
