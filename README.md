# Mutant
[![Build Status](https://travis-ci.org/Wilfred/mutant.svg?branch=master)](https://travis-ci.org/Wilfred/mutant)

***finally, an editor for Emacs!***

Mutant provides a convenient UI for editing lists.

![edit_hook](edit_hook.gif)

Mutant is great for editing large lists, such as hooks. In the above
example, I insert and edit values in `prog-mode-hook`.

![kill_ring](kill_ring.gif)

Mutant is also a valuable debugging tool. In this example, I reorder
items in the `kill-ring` so I paste the value I want.

## Limitations

Mutant deliberately **modifies values in place**. This is useful if
you're working with a list that's shared between multiple variables,
such as `font-lock-defaults`.

In some cases, this isn't possible (e.g. inserting into an empty
list). In these cases, mutant will overwrite your variable.

## License

GPLv3.
