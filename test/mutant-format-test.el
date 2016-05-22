(require 'ert)
(require 'mutant)

(ert-deftest mutant-format-symbol ()
  (should (equal (mutant--pretty-format 'x) "'x")))

(ert-deftest mutant-format-dotted-list ()
  (should (equal (mutant--pretty-format (cons 1 2)) "'(1 . 2)")))

(ert-deftest mutant-describe-dotted-list ()
  (should (equal (mutant--describe (cons 1 2))
                 "a pair")))
