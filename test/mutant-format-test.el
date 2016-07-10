(require 'ert)
(require 'mutant)

(ert-deftest mutant-format-symbol ()
  (should (equal (mutant--pretty-format 'x) "'x")))

(ert-deftest mutant-format-builtin-symbol ()
  (should (equal (mutant--pretty-format nil) "nil"))
  (should (equal (mutant--pretty-format t) "t")))

(ert-deftest mutant-format-string ()
  (should (equal (mutant--pretty-format "abc\"def") "\"abc\\\"def\"")))

(ert-deftest mutant-format-dotted-list ()
  (should (equal (mutant--pretty-format (cons 1 2)) "'(1 . 2)")))

(ert-deftest mutant-describe-dotted-list ()
  (should (equal (mutant--describe 'x (cons 1 2))
                 "x is a global variable. Its current value is a pair")))

(ert-deftest mutant-describe-single-element-list ()
  (should
   (equal (mutant--describe 'x (list 'foo))
          "x is a global variable. Its current value is a list\ncontaining 1 value")))
