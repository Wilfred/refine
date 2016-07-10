# Mutant
[![Build Status](https://travis-ci.org/Wilfred/mutant.svg?branch=master)](https://travis-ci.org/Wilfred/mutant)

***finally, an editor for Emacs!***

Mutant provides a convenient UI for editing lists and vectors in
Emacs.

It's particularly useful for modifying large lists, such as mode
hooks, `auto-mode-alist` or even the `kill-ring`.

Mutant deliberately **modifies values in place**. This is useful if
you're working with a list that's shared between multiple variables.
