(require 'ert)
(require 'refine)

(defcustom refine--possible-sexp '(x y)
  "Apparently cask insists on a docstring here."
  :type 'sexp)

(ert-deftest refine--possible-elements-sexp ()
  "There are no obvious possibilities for a `defcustom' that allows
any sexp."
  (should
   (null (refine--possible-elements 'refine--possible-sexp))))

(defcustom refine--possible-set '(baz)
  "Apparently cask insists on a docstring here."
  :type '(set
          (const :tag "Foo" foo)
          (const :tag "Bar" bar)
          (const :tag "Baz" baz)))

(ert-deftest refine--possible-elements-set ()
  "Ensure we offer the correct possibilities for a `defcustom'
that only allows values from a set."
  (should
   (equal
    (refine--possible-elements 'refine--possible-set)
    '(foo bar baz))))

(defcustom refine--possible-choice '(baz)
  "Dummy to make Cask happy."
  :type '(repeat (choice
                  (const :tag "Foo" foo)
                  (const :tag "Bar" bar)
                  (const :tag "Baz" baz))))

(ert-deftest refine--possible-elements-choice ()
  "Ensure we offer the correct possibilities for a `defcustom'
that allows a repeated choice."
  (should
   (equal
    (refine--possible-elements 'refine--possible-choice)
    '(foo bar baz))))

(defcustom refine--possible-unhandled '(1 2 3)
  "Dummy to make Cask happy."
  :type '(repeat number))

(ert-deftest refine--possible-elements-unhandled ()
  "Ensure that unhandled `defcustom' types do not error out."
  (refine--possible-elements 'refine--possible-unhandled))
