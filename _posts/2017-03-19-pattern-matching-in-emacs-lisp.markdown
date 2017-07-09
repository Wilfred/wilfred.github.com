--- 
layout: post
title: "Pattern Matching in Emacs Lisp"
tags:
 - emacs
---

Pattern matching is invaluable in elisp. Lists are ubiquitous, and a
small amount of pattern matching can often replace a ton of verbose
list fiddling.

Since this is Lisp, we have lots of choices! In this post, we'll
compare
[cl.el](https://www.gnu.org/software/emacs/manual/cl.html),
[pcase.el](https://www.gnu.org/software/emacs/manual/html_node/elisp/Pattern-matching-case-statement.html),
[dash.el](https://github.com/magnars/dash.el),
and [shadchen](https://github.com/VincentToups/shadchen-el), so you
can choose the best fit for your project. We'll look at the most
common use cases, and end with some recommendations.

For the sake of this post, we'll consider both pattern matching and
destructuring, as they're closely related concepts.

## A Simple List

Let's get started with a list that always has three elements. All four
of our libraries work here:

{% highlight common-lisp %}
(cl-destructuring-bind (a b c) (list 1 2 3)
  (+ a b c))
{% endhighlight %}

{% highlight common-lisp %}
(pcase (list 1 2 3)
  (`(,a ,b ,c) (+ a b c)))
;; Or:
(pcase-let ((`(,a ,b ,c) (list 1 2 3)))
  (+ a c b))
{% endhighlight %}

{% highlight common-lisp %}
(require 'dash)
;; Using `-let' with a single value
(-let [(a b c) (list 1 2 3)]
  (+ a b c))

;; We can also use `-let' like the built-in `let',
;; which enables us to add other bindings (`pcase-let'
;; can do this too).
(-let (((a b c) (list 1 2 3)))
  (+ a b c))
{% endhighlight %}

{% highlight common-lisp %}
(require 'shadchen)
(match (list 1 2 3)
  ((list a b c) (+ a b c)))
;; Shadchen also provides a `pcase-let' equivalent:
(match-let (((list a b c) (list 1 2 3)))
  (+ a b c))
{% endhighlight %}

Already, we can see some major syntactic
differences. `cl-destructuring-bind` has a very lightweight syntax.
`-let` is also lightweight, and has a definite Clojure
influence. `pcase` uses a backquote syntax that's familiar if you've
written macros, and shadchen's `match` uses patterns based
on
[Racket's pattern matcher](http://docs.racket-lang.org/reference/match.html).

Users often want to use patterns inside `let` bindings, so three of
our libraries provide this too. It's a little paren-heavy, but useful
when you need it.

## Lists of Different Lengths

It's very common to have lists of different lengths, and pattern
matching is a great fit here.

We can't use `cl-destructuring-bind`, `pcase-let` or `-let` here, as
they require a single matching pattern.

{% highlight common-lisp %}
(pcase (list 1 2)
  (`() 0)
  (`(,a) a)
  (`(,a ,b) (+ a b)))
{% endhighlight %}

{% highlight common-lisp %}
(require 'shadchen)
(match (list 1 2)
  ((list) 0)
  ((list a) a)
  ((list a b) (+ a b)))
{% endhighlight %}

`pcase` and `match` take different attitudes to a failing pattern
match. If we pass in `(list 1 2 3)` to the code above, `pcase` just
returns `nil` whereas `match` signals an error. If we want to ignore
other cases with `match`, we have to handle it explicitly:

{% highlight common-lisp %}
(match (list 1 2)
  ((list) 0)
  ((list a) a)
  ((list a b) (+ a b))
  (_ nil))
{% endhighlight %}

## Skipping Elements

A full-featured pattern matching library should include the ability to
skip elements, so naturally this is possible too:

{% highlight common-lisp %}
(cl-destructuring-bind (a _ _ d) (list 1 2 3 4)
  (+ a d))
{% endhighlight %}

{% highlight common-lisp %}
(pcase (list 1 2 3 4)
  (`(,a ,_ ,_ ,d) (+ a d)))
{% endhighlight %}

{% highlight common-lisp %}
(require 'dash)
(-let [(a _ _ d) (list 1 2 3 4)]
  (+ a d))
{% endhighlight %}

{% highlight common-lisp %}
(require 'shadchen)
(match (list 1 2 3 4)
  ((list a _ _ d) (+ a d)))
{% endhighlight %}

`pcase`, `-let` and `match` explicitly use `_` for
placeholders. `cl-destructuring-bind` doesn't care and just binds `_`
like any other variable.

## Values in Patterns

Pattern matching can really help when you're expecting explicit values
in certain places.

For literals, it's straightforward:

{% highlight common-lisp %}
(pcase (list 1 2)
  (`(0 ,b) b)
  (`(,a ,b) (+ a b)))
{% endhighlight %}

{% highlight common-lisp %}
(require 'shadchen)
(match (list 0 0)
  ((list 0 b) b)
  ((list a b) (+ a b)))
{% endhighlight %}

If we want symbols, it takes a little more care with our quoting:

{% highlight common-lisp %}
(pcase (list 'x 'y)
  (`(x ,b) "Starts with 'x")
  (`(y ,b) "Starts with 'y")
  (`(,a ,b) "Starts with something else"))
{% endhighlight %}

{% highlight common-lisp %}
(require 'shadchen)
(match (list 'x 'y)
  ((list 'x b) "Starts with 'x")
  ((list 'y b) "Starts with 'y")
  ((list a b) "Starts with something else"))
{% endhighlight %}

## Repeated Values

This is sometimes called 'non-linear patterns', and is less common in
languages with pattern matching built-in (e.g. Haskell doesn't support
it). Since pattern matching is just a macro library, we can pick the
library that provides this:

{% highlight common-lisp %}
(pcase (list 0 1)
  (`(,a ,a) "Elements are equal according to eq")
  (`(,a ,b) "Elements are different"))
{% endhighlight %}

## Cons Cells

If you have an explicit cons pair, it may not be a proper list. You
can match on this instead:

{% highlight common-lisp %}
;; The docstring for `cl-destructuring-bind' doesn't say
;; what's possible, but Info node `(cl) Macros' informs
;; us that the syntax is the same as `cl-defmacro'.
(cl-destructuring-bind (a . b) (cons 1 2)
  (+ a b))
{% endhighlight %}

{% highlight common-lisp %}
(pcase (cons 1 2)
  (`(,a . ,b) (+ a b)))
{% endhighlight %}

{% highlight common-lisp %}
(require 'dash)
(-let [(a . b) (cons 1 2)]
  (+ a b))
{% endhighlight %}

{% highlight common-lisp %}
(require 'shadchen)
(match (cons 1 2)
  ((cons a b) (+ a b)))
{% endhighlight %}

## Where Clauses

Testing values against a predicate isn't really pattern matching, but
it's a popular feature in languages with built-in pattern matching. We
can do this too:

{% highlight common-lisp %}
(pcase (list 1 2)
  ((and `(,a ,b) (guard (oddp a)))
   "Two element list starting with an odd number")
  (`(,a ,b)
   "Other two element list."))
{% endhighlight %}

{% highlight common-lisp %}
(match (list 1 2)
  ((list (? #'oddp a) b)
   "Two element list starting with an odd number")
  ((list a b)
   "Other two element list."))
{% endhighlight %}

## Recommendations

Shadchen is very powerful, and even allows recursing with `match-let`
or function declaration with `defun-match`. However, it's not
namespaced and the documentation isn't as good.

dash.el doesn't let you use multiple patterns, nor can it match on
literal values. The syntax is very readable though and it has
excellent docstrings. You can also use its patterns with the other
dash binding macros, such as `-if-let`.

pcase.el has both excellent docstrings and examples in the info
manual, which makes up for slightly noisier syntax. It's also widely
used in Emacs core and some popular Emacs packages, such as magit and
use-package.

Now we've seen some examples, which should you pick? If you're already
using dash.el in your projects, it's a no-brainer: use `-let`. If not,
give `pcase` a try: it's built-in, it's used extensively in core, and
it's the only library that worked in every scenario we looked at.
