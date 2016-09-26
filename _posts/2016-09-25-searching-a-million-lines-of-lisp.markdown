--- 
layout: post
title: "Searching a million lines of lisp"
---

Time for another Emacs adventure!

I got Smalltalk envy again recently. In Smalltalk, I can use the
finder to find all direct callers of a method ('message sends' in
Smalltalk jargon).

In Emacs, however, you're outta luck. There's no way to find all the
callers of a function or macro. Most Emacsers just grab their
favourite text search tool.

We can do better. It just wouldn't be Emacs without a litle fanatical
tool building. How hard can it be?

## Parsing

Everyone know how to parse lisp, right? It's just `read`.

It turns out that a homoiconic language is hard to parse when you want
to know *where you found the code in the first place*. In a language
with a separate AST, the AST type is tagged with file positions. In
lisp, you just have... a list. Unadorned.

I briefly explored writing my own parser before coming to my
senses. Inspired by similar work in
[el-search](https://elpa.gnu.org/packages/el-search.html), 

## Performance

After about a week of figuring out how to parse code, I realised that
Emacs has a **lot** of elisp. My current instance has 750KLOC loaded,
and since Emacs lazily loads files, that's only the subset of
functionality that I use!

I wanted to parse everything in the current Emacs instance, just like
Smalltalk. With ~800 files on my `load-path`, my 10 second parser
wouldn't cut it.

To write fast elisp, you need to do as little as possible. I
refactored my parsing to only calculate positions when the current
form might contain a match for the current search.

How do we know a form might match? Emacs has a little-known variable
called `read-with-symbol-positions` that will report all the symbols
read during parsing. If we're looking for function calls to
`some-func`, and there's no reference to the symbol `some-func`, we
can skip that form entirely.

This brought parsing down to <1 second per file, and I wrote a proper
benchmark script. I then managed to get another 3-4x speedup with
careful profiling. 

C functions are fast, elisp functions are slow. So
much for CS algorithms: `assoc` with alists was faster than hash
maps.

## Display


