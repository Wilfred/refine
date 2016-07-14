# Refine
[![Build Status](https://travis-ci.org/Wilfred/refine.svg?branch=master)](https://travis-ci.org/Wilfred/refine)

***finally, an editor for Emacs!***

Refine provides a convenient UI for editing lists.

![edit_hook](edit_hook.gif)

Refine is great for editing large lists, such as hooks. In the above
example, I insert and edit values in `prog-mode-hook`.

![kill_ring](kill_ring.gif)

Refine is also a valuable debugging tool. In this example, I reorder
items in the `kill-ring` so I paste the value I want.

## Limitations

Refine deliberately **modifies values in place**. This is useful if
you're working with a list that's shared between multiple variables,
such as `font-lock-defaults`.

In some cases, this isn't possible (e.g. inserting into an empty
list). In these cases, refine will overwrite your variable.

## License

GPLv3.

## Installation

Add refine.el to `load-path`. MELPA in progress.
