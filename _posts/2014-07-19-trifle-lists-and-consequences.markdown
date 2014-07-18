--- 
layout: post
title: "Trifle: Lists and CONSequences"
---

Lists are a spectacularly important data type. Even more so for lisp
languages, as a lisp program is just a text file with some list
literals.

In Trifle, we want our list structure to be easy to reason about,
fast, and consistent. After exploring a range of possibilities, we
settled on using dynamic arrays.

The major possibilities we considered were:

**Sparse arrays**. These are often harder to reason about. JavaScript
is the most widely used language with this feature, yet many popular
list libraries (such as underscore.js) [do not support sparse
arrays](http://underscorejs.org/#arrays). Sparse arrays are also slower, leading to
[silly situations where user functions can be faster than interpreter built-ins](https://github.com/codemix/fast.js#how).

**Cons cells**. This is the classic lisp datastructure. It's easy as an
experienced lisper to forget how awkward cons cells are.

Lists made out of cons cells have become similar to sparse arrays
in that users make simplifying assumptions about their structure. For
example, [dash.el](https://github.com/magnars/dash.el) assumes that
all lists are proper lists. Try to write a `map` function that accepts
arbitrary cons cell structures and throws an error if the 'list' is
improper or cyclic. It's hard, and it's inefficient.

Cons cells also introduce infix syntax `(1 . nil)` (bizarre in a lisp)
and have strange naming conventions. A cons cell is really just a
2-tuple with the first element named 'car' and the second element
'cdr'. This leads to ugly function names like `caddr` for getting the
third element of a list.

To make matters worse, cons cells are as slow as linked lists but less
versatile. Some lisp machines improve performance with
[cdr-coding](http://en.wikipedia.org/wiki/CDR_coding) but this just
allows cons cell lists to approximate dynamic arrays. Whilst some list
operations benefit from using linked lists (e.g. `insert!`), it's not
actually possible to implement `push!` or `pop!` as functions when
using cons cells.

**Linked lists**. Linked lists are a more popular option and fix several of
our objections to cons cells. Statically typed languages often default
to linked lists (e.g. Haskell) and offer stronger guarantees about the
well-formedness of the lists.

Proper linked lists do provide `push!` and `pop!`, and immutable
singly-linked lists allow you to share their tails, reducing memory
usage. However, the performance overhead of traversing the list with
poor cache locality means linked list performance is frequently
outperformed by dynamic arrays.

**Dynamic arrays**. Dynamic arrays are fast, since you're accessing
contiguous memory. Finding their length is free and they are always
well-formed. Writing a `map` function is now trivial.

In Trifle, lists are mutable, zero-indexed, and accessing values
out-of-bounds is an immediate error. Parsing s-expressions returns
lists. We also benefit from using a datastructure that will be
immediately familiar to users of many other languages.

This is a fundamental design choice that can only really be changed in
the earliest stages of a language's development. Trifle lists offer a
nice abstraction in that we can expose the same API for lists, strings
and bytestrings. Only time and many lines of code will reveal if our
decisions were the best, but we are excited to see what form Trifle
programs will take.
