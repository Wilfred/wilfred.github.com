--- 
layout: post
title: "Essential Elisp Libraries"
---

Recently, I've been writing a lot of Emacs Lisp. I've been trying to write
as many of my own tools as possible in elisp, to see what the language
works well at and what it doesn't do well.

Turns out, elisp does many things well. However, there are several
libraries that make life *much* easier.

(Disclaimer: I've written some of them.)

## Lists

Firstly, [dash.el](https://github.com/magnars/dash.el), which
describes itself as "a modern list library for Emacs". Dash.el offers
`-map`, `-reduce`, `-count`, `-slice`, `-group-by`, and many other
list operations that are very common in other languages.

Dash.el has modernised elisp. To my knowledge, stock Emacs doesn't
even include a traditional `(map 'some-function some-list)` operation
-- there's only `cl-map`, but it requires another argument and using
`'cl` at runtime is frowned upon.

**Edit**: Several people have pointed out that stock Emacs has
`mapcar`, which does exactly this.

The documentation is excellent (it's generated automatically from the
test suite), but my favourite feature is the anaphoric macros. Rather
than writing:

    (-filter (lambda (x) (> x 10)) (list 8 9 10 11 12))
    
You can simply write an s-expression that is evaluated for each item:

    (--filter (> it 10) (list 8 9 10 11 12))
    
## Strings

[s.el](https://github.com/magnars/s.el), "the long lost Emacs string
manipulation library", is another library that is too valuable to
miss. Emacs functions often revolve around text editing (surprise!)
and often need a versatile set of string manipulation functions.

As with dash.el, s.el introduces a more contemporary style to elisp. All
s.el functions are pure, returning a new string, despite elisp
supporting mutable strings. Some common string operations include
`s-trim`, `s-replace` and `s-join`. It's not that these operations are
impossible in elisp -- you can replace literal strings using
`replace-regexp-in-string` and `regexp-quote`, but s.el makes this
much easier.

## Hash tables

Hash tables are a rarely used datastructure in elisp. It's more common
to see lists of pairs used, despite their slower performance. This is
mostly because lists are easier to work with: creating a list is
trivial, but `make-hash-table` has an intimidating docstring with five
different keyword arguments. Common operations (can you see a theme
here?) for hash tables, such as iterating over keys, are not as easy
as they should be.

[ht.el](https://github.com/Wilfred/ht.el) solves this. `ht-create` has
only one argument, and it can be omitted. ht.el also provides common
operations like `ht-keys`, `ht-values` as well as convenience
functions for converting from and to alists or plists.

## Loops

With a good list package like dash.el, I find that an explicit loop is
rarely necessary. However, sometimes you're iterating for side effects
and you need something more powerful. Elisp itself only includes
`while`. `'cl` has `dolist` and dash.el has `-each`, but they're all
low-level.

[loop.el](https://github.com/Wilfred/loop.el) "friendly imperative
loop structures" offers a larger set of loop structures, including:
`loop-until`, `loop-do-while` and `loop-for-each`. More interestingly,
you can call `loop-break` or `loop-continue` in any loop.

## Why now?

Now that Marmalade and MELPA are maturing, it's easy to depend on
these libraries. Previously, elisp package authors would try to make
their code self-contained. This is no longer necessary, you can simply
put the following in the header of your package:

    ;; Package-Requires: ((dash "1.1.0") (loop "1.1"))

(Note that the version specifier is simply the minimum version.)

Since these packages are available on both Marmalade and MELPA, people
who install your package will get these utility libraries
automatically, if they don't have them already.

All these utility libraries demonstrate the remarkable flexibility of
elisp. You can take familiar ideas from other, newer languages and use
them in elisp. I have deliberately not described any of the functions
I've mentioned here (they have good docstrings anyway) since their
names will be familiar to programmers with mainstream programming
language experience.

These libraries also help with a classic problem in the Emacs world:
discoverability. Their purposes are well-defined, and each library
mentioned has a README that lists all the functionality available to
the elisp programmer.

## The future

I don't believe this is the last word in utility libraries. There are
still several areas of elisp that would benefit from a new
API. Regular expressions in elisp modify global state rather than
returning multiple values (see for example `match-beginning` and
`save-match-data`). Platform-independent path manipulation is awkward
([use `concat` for joining paths](http://stackoverflow.com/a/3964815)),
and I'm not aware of
[any elegant way](https://github.com/Wilfred/.emacs.d/blob/feedd50fc3f5bf890f268e987e22876872a0ce47/user-lisp/file-utils.el#L61)
of ignoring `.` and `..` in directory listings.

So there's plenty of room for new interesting solutions. Some will
even (or at least should) get pushed into Emacs proper. Happy hacking!
