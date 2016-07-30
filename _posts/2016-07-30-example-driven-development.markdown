--- 
layout: post
title: "Example Driven Development"
tags:
 - emacs
---

"Hey $COLLEAGUE, is there any function that takes this list and returns a
list like this?"

Some functions are hard to Google. It's often easier to speak about
concrete examples: I have a list `'(x y z)`, and an index `1`,
and I want a list without the element at that position: `'(x z)`.

<img src="/assets/suggest_el.png">

I was blown away when I saw
[this feature implemented in Smalltalk](https://www.youtube.com/watch?v=HOuZyOKa91o#t=5m05s). I
decided to
[build an Emacs implementation](https://github.com/Wilfred/suggest.el)
and it's super helpful when you need it. The screenshot above shows it
in action.

## Lisp Wins

This project is much easier in a language with little syntax. Suppose
we're looking for arithmetic functions:

<img src="/assets/suggest_numeric.png">

suggest.el simply has a list of functions, and tries each one in
turn. The fact that `+` is syntactically the same as any other
function is a big help here. We don't need to do any special
formatting for infix functions.

## Brute-Force Performance

suggest.el brute-force searches its list of functions. It ignores
anything that throws an error, or returns a value different to the
output requested.

We currently have a list of 140 functions. I was expecting this to be
slow, but it's instant.

We also need to try every permutation of arguments, which is
computationally expensive. We want users to discover functions like
`nth`, even if they get arguments in the wrong order. There are a lot
of permutations, but typically users only use 1-3 arguments, so it's
not a problem.

If suggest.el find a match, then it doesn't try any other
orderings. This prevents us suggesting both `(+ 2 3)` and `(+ 3 2)` --
the user would probably prefer the order they've proposed anyway.

## Eval All The Way

suggest.el evals all the expressions given, allowing users to enter
literals, variables, or even function expressions. This is convenient
when developing new packages and you want to use values from your
package.

<img src="/assets/suggest_eval.png">

In the above screenshot, note how the suggested expression `(-sum
(list 2 3 4))` uses exactly the expression given, so it can be
copy-pasted. However, we show the final value `=> 9`, so it's clear
what the function is doing.

## Exploiting The Emacs UI

To take best advantage of Emacs, the UI needs to work like a text
editor. `suggest-mode` derives from `emacs-lisp-mode`, so your
favourite lisp packages just work. The entire buffer is a valid lisp
file and we get highlighting for free.

suggest.el adds a few extra touches on top. We use
[overlays](https://www.gnu.org/software/emacs/manual/html_node/elisp/Overlays.html)
to highlight the headings, which overrides the comment
highlighting. Headings and output also use the
[special property `read-only`](https://www.gnu.org/software/emacs/manual/html_node/elisp/Special-Properties.html#Special-Properties)
to guide users into typing in the correct places.

<img src="/assets/suggest_hook.gif">

suggest.el also uses
[change hooks](https://www.gnu.org/software/emacs/manual/html_node/elisp/Change-Hooks.html)
(pictured above) to inform the user that they need to re-run
`suggest-update`. This shows `C-c C-c` (the default keybinding) by
default. Just like the Emacs tutorial, suggest.el will always use the
current keybindings.

## Choosing Functions

suggest.el has a whitelist of functions that are safe to run. Which
functions should we include? suggest.el is in a position of
responsibility, as it affects the code that users write.

Functions absolutely cannot have side-effects. Calling `delete-file`
could be catastrophic. If a function only mutated its input arguments,
that would be safe, but it's not necessarily what the user expected.

Strictly requiring pure functions means that many built-in Emacs
functions aren't offered. Wherever possible, we suggest built-in
functions, and show them first.

<img src="/assets/suggest_aliases.png">

suggest.el has to decide which function aliases to show. I've been
[convincingly persuaded that predicate functions should use `-p`](https://github.com/bbatsov/projectile/pull/291#issuecomment-38379199),
so suggest.el prefers those aliases. suggest.el also favours `cl-lib`
over the popular, but deprecated `cl` (e.g. `cl-first` not `first`).

We also have to decide which packages to include. suggest.el includes
third-party packages that I consider important (dash.el, s.el and
f.el). Long term, I hope packages will add to `suggest-functions`
using `eval-after-load`.

Third-party packages often have a higher proportion of pure
functions. In the case of `f.el`, I've included a few impure functions
that read the filesystem, as they're safe.

It's also tricky to pick *which* functions belong in the
whitelist. suggest.el tries to offer functions that you're likely to
'stumble across'.

I found that the best functions take 1-3 arguments of simple
types. You're unlikely to find a higher-order function from a random
example. We also want functions that are characterised by a single
example. Single-argument predicates aren't much use, since they need
several examples to demonstrate their behaviour.

We currently have functions for lists, alists, plists, hash tables,
numbers, strings, symbols and paths. This covers much of my day-to-day
development, and I've already learnt some new functions!

## Give It A Try!

suggest.el is [on GitHub](https://github.com/Wilfred/suggest.el),
[on MELPA](http://melpa.org/#/suggest), and I would love to hear your
feedback. You can
[tweet at me](https://twitter.com/_wilfredh) or
[file an issue](https://github.com/Wilfred/suggest.el/issues/new).
