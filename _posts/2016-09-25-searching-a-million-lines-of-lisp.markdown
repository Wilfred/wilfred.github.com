--- 
layout: post
title: "Searching A Million Lines Of Lisp"
tags:
 - emacs
---

Time for another Emacs adventure!

In Emacs today, there's no way to find all the callers of a function
or macro. Most Emacsers just grab their favourite text search tool.

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

{% highlight common-lisp %}
;; Variables can start with numbers:
(let ((0x0 1))
  ;; And a backquote does not have to immediately precede the
  ;; expression it's quoting:
  `
  ;; foo
  (+ ,0x0))
{% endhighlight %}

Cripes.

Anyway, I ~~totally ripped off~~ was inspired by similar work in
[el-search](https://elpa.gnu.org/packages/el-search.html). `read`
moves point to the end of the expression read, and you can use
`scan-sexps` to find the start of the expression.

## Analysing

OK, we've parsed our code, preserving positions. Which forms actually
look like function calls?

This requires a little thought. Here are some tricky examples:

{% highlight common-lisp %}
;; Not references to `foo' as a function.
(defun some-func (foo))
(lambda (foo))
(let (foo))
(let ((foo)))

;; Calls to `foo'.
(foo)
(lambda (x) (foo))
(let (x) (foo))
(let ((x (foo))) (foo))
(funcall 'foo)
;; Not necessarily a call, but definitely a reference to 
;; the function `foo'.
(a-func #'foo)
{% endhighlight %}

We can't simply walk the list: `(foo)` may or may not be a function
call, depending on context. We calculate the path taken to get to a
form, so we have context.

For example, given the code `(let (x) (bar) (setq x (foo)))`, we build a
path `((setq . 2) (let . 3))` when looking at the `(foo)`. This gives
us enough context to recognise function calls.

"Aha!", says the experienced lisper. "What about macros?"

refs.el understands a few common macros, and *most* macros just
evaluate *most* of their arguments. This generally works, but refs.el
can't understand arbitrary forms. We do provide a `refs-symbol` to
find all references, regardless of their positions in the form.

## Performance

It turns out that Emacs has a **ton** of elisp. My current instance
has loaded *three quarters of a million lines of code*. Emacs actually
lazily loads files, so that's only the functionality that I use!

So, uh, a little optimisation was needed. I wrote a benchmark script
and learnt how to make elisp fast.

refs.el needs to know where it found matches, so users can jump to the
file at the right position. However, if a form doesn't contain any
matches, we don't need to do this expensive calculation.

It turns out we can do even better. Emacs has a little-known variable
called `read-with-symbol-positions` that will report all the symbols
read when parsing a form. If we're looking for function calls to
`some-func`, and there's no reference to the symbol `some-func`, we
can skip that form entirely.

When you have unavoidable calculations, you need to use C functions
wherever possible. So much for CS algorithms: `assoc` with small
alists was faster than building a hash map with elisp.

Elisp provides various ways to avoid changing the state of the current
buffer, particularly `save-excursion` and `with-current-buffer`. This
bookkeeping is expensive, and refs.el just creates its own temporary
buffers and dirties them.

Finally, we do the traditional software performance cheat: we show a
progress bar.

## Display

We have something that works, and we can search in under 10
seconds. How do we display results?

<img src="/assets/refs_proto.png">

Initially, we just dumped the form in the results buffer. This loses
context and puts everything on one line.

I experimented with colour, and with underlines, but it quickly became
noisy.

The best way to answer UI questions in Emacs is to ask 'what would
magit do?'. Magit, I thought, would use a small number of colours,
with bold text to highlight headings.

I settled on syntax highlighting the result, but treating the rest of
the search context as a comment. I think this is intuitive, and fits
whatever funky colour scheme you're using.

<img src="/assets/refs_screenshot.png">
