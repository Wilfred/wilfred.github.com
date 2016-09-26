--- 
layout: post
title: "Searching A Million Lines Of Lisp"
tags:
 - emacs
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
with a separate AST, the AST type includes file positions. In
lisp, you just have... a list. Unadorned.

I briefly explored writing my own parser before coming to my
senses. Did you know the following is legal elisp?

```
;; Variables can start with numbers:
(let ((0x0 1))
  ;; And a backquote does not have to immediately precede the
  ;; expression it's quoting:
  `
  ;; foo
  (+ ,0x0))
```

Cripes.

Anyway, I ~~totally ripped off~~ was inspired by similar work in
[el-search](https://elpa.gnu.org/packages/el-search.html). `read`
moves point to the end of the expression read, and you can use
`scan-sexps` to find the start of the expression.

## Performance

After about a week of figuring out how to parse code, I realised that
Emacs has a **ton** of elisp. My current instance has 750 KLOC loaded,
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
maps. `save-excursion` is expensive, so we just create temporary
buffers and dirty them.

Emacs is incredibly customisable: you can even adjust garbage
collection behaviour within your code! I reduced runtime by 30% by
setting `gc-cons-percentage` to a higher value, so GC runs less
frequently. I don't know of many languages that let you
(non-invasively) tune this.

Finally, we give the user feedback on progress, so they know Emacs
hasn't hung.

## Display

We have something that works, and we can search in under 10
seconds. How do we display results?

I experimented with color, and with underlines, but it quickly became
noisy.

The best way to answer UI questions in Emacs is to ask 'what would
magit do?'. Magit, I thought, would use a small number of colours,
with bold text to highlight headings.

I settled on syntax highlighting the result, but treating the rest of
the search context as a comment. I think this is intuitive, and fits
whatever funky colour scheme you're using.

<img src="/assets/refs_screenshot.png">
