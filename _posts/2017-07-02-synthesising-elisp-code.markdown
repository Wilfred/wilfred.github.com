--- 
layout: post
title: "Synthesising Elisp Code"
---

<blockquote class="twitter-tweet" data-lang="en"><p lang="en" dir="ltr">Just look at this awesome Emacs mode.<br><br>(It&#39;s <a href="https://t.co/wBOvlM0ixK">https://t.co/wBOvlM0ixK</a> and <a href="https://twitter.com/_wilfredh">@_wilfredh</a> did it) <a href="https://t.co/5SV1MgnHJP">pic.twitter.com/5SV1MgnHJP</a></p>&mdash; ☭🚀🐕 Bodil 🐕🚀☭ (@bodil) <a href="https://twitter.com/bodil/status/762770893298950146">August 8, 2016</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

I've just released v0.3
of [suggest.el](https://github.com/Wilfred/suggest.el), an Emacs
package for discovering elisp functions. You supply an example input
and output, and it makes suggestions.

The initial release was already useful, but v0.3 is now much smarter.

## what is this, anyway?

Communicating what suggest.el does is difficult. Bodil's tweet above
did a much better job of communicating usage, so I've overhauled the
README. It's now full of examples like this:

<blockquote>
<p>suggest.el can also help you find tricky dash.el functions:</p>
<pre><code>;; Inputs (one per line):
(list 'a 'b 'c 'd)
'c

;; Desired output:
2

;; Suggestions:
(-elem-index 'c (list 'a 'b 'c 'd)) ;=> 2
</code></pre>
</blockquote>

This helps users get an idea of what they can do with suggest.el.

## let's not segfault Emacs

It turns out that brute-forcing elisp primitives can expose
some nasty Emacs bugs. suggest.el will no longer make your Emacs
crash, and
[upstream have fixed the underlying issue](https://debbugs.gnu.org/cgi/bugreport.cgi?bug=25684).

## isn't there a function for that?

Since the initial release, suggest.el can now suggest 38 additional
functions. suggest.el is now able to help you find values in a vector,
convert symbols to strings, and much more besides.

## any number of arguments

If your input is a list, you might be able to call a function with
`apply` to get your desired result. Given an input `'(2 3)` and an
output of `5`, suggest.el can now propose `(apply #'+ '(2 3))`!

## slightly magical

One user commented that suggest.el was very limited because it can't go
from `'(a b c d)` to `'(a b)`.

Thinking about the implementation, this seemed like a silly
request. Why would you expect a *single function* to drop exactly two
elements?

However, if we search for *sequences* of function calls, we can make this
user happy!

```emacs-lisp
;; Inputs (one per line):
'(a b c d)

;; Desired output:
'(a b)

;; Suggestions:
(butlast (butlast '(a b c d))) ;=> '(a b)
```

This proved to be a really fun feature that sometimes produces
surprising results:

```emacs-lisp
;; Inputs (one per line):
0.0

;; Desired output:
3.0

;; Suggestions:
(1+ (1+ (1+ 0.0))) ;=> 3.0
(float (length (number-to-string 0.0))) ;=> 3.0
```

Converting to a string, then taking the length, does indeed produce
3.0!

```emacs-lisp
> (number-to-string 0.0)
"0.0"
> (length "0.0")
3
> (float 3)
3.0
```

suggest.el is doing a breadth-first search of function calls. I've written
[several search heuristics](https://github.com/Wilfred/suggest.el/blob/b543b15cbff0d5dfeaebff1f4c9aacab7412a40f/suggest.el#L492-L512)
and found that a depth of three function calls still gives acceptable
performance.

Sorting results is still an open problem: suggest.el prefers fewer
function calls with a crude notion of 'simple' functions.

```emacs-lisp
;; Inputs (one per line):
'(a b c)

;; Desired output:
'c

;; Suggestions:
(cl-third '(a b c)) ;=> 'c
(-last-item '(a b c)) ;=> 'c
(cadr (cdr '(a b c))) ;=> 'c
(car (last '(a b c))) ;=> 'c
(apply #'last (last '(a b c))) ;=> 'c
```

`cl-third` vs `-last-item` vs `(car (last ...))` is largely a matter
of taste. The last result is entirely silly.

## testing a search

I've also written a series of tests to verify that my search returns
the intended result (in addition to any others). This is really
helpful when tweaking search heuristics.

I wrapped this up in a pretty macro:

```emacs-lisp
(ert-deftest suggest-possibilities ()
  ;; A particularly deep search.
  (should-suggest 0 => 3
                  (1+ (1+ (1+ _))))
  ;; Ensure we offer built-in list functions.
  (should-suggest '(a b c d) => '(c d)
                  (cdr (cdr _))))
```

However, the implementation of the `should-suggest` macro is not
pretty at all. Macros are great for finding better ways of expressing
ideas, but I often find they produce insight that leads to refactoring
them away entirely. Time will tell if this macro is worth the
maintenance.

## outro

suggest.el v0.3 is [available on MELPA](https://melpa.org/#/suggest) and I
hope it's useful for you.

If you like suggest.el, you'll be blown away
by [barliman](https://github.com/webyrd/Barliman/). This takes the
idea of synthesising much much further, and can even generate quines!
See [this video](https://www.youtube.com/watch?v=er_lLvkklsk) for a
great demonstration.

