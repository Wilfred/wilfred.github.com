--- 
layout: post
title: "Suggest.el: Synthesising Constants"
tags:
 - emacs
---

[Suggest.el](https://github.com/Wilfred/suggest.el) v0.4 is now out,
and it offers some really interesting new ways of making suggestions.

## Supplying Constants

Suppose the user gives us the input `'(a b c d)` and desired output
`'a`. We would already suggest `car`, but that only gets the first
element of the list. They may have wanted `elt` or `nth`, which get
elements at a specific position.

We now try adding constants to the user's inputs, specifically `nil`,
`t`, `-1`, `0`, `1` and `2`. This makes suggest.el much more
effective.

Here's the example we mentioned:

{% highlight common-lisp %}
;; Inputs (one per line):
'(a b c d)

;; Desired output:
'a

;; Suggestions:
(car '(a b c d)) 
(elt '(a b c d) 0) ; <- new
(nth 0 '(a b c d)) ; <- new
{% endhighlight %}

We can now suggest grouping items in a list pairwise:

{% highlight common-lisp %}
;; Inputs (one per line):
'(a b c d e f)

;; Desired output:
'((a b) (c d) (e f))

;; Suggestions:
(-partition 2 '(a b c d e f)) ; <- new
{% endhighlight %}

Converting a vector to a list:

{% highlight common-lisp %}
;; Inputs (one per line):
(vector 1 2 3)

;; Desired output:
(list 1 2 3)

;; Suggestions:
(string-to-list (vector 1 2 3))
(append (vector 1 2 3) nil) ; <- new
(-rotate 0 (vector 1 2 3))  ; <- new
(-concat (vector 1 2 3) nil)  ; <- new
{% endhighlight %}

Truncating lists:

{% highlight common-lisp %}
;; Inputs (one per line):
'(a b c d e)

;; Desired output:
'(c d e)

;; Suggestions:
(-drop 2 '(a b c d e)) ; <- new
(-slice '(a b c d e) 2) ; <- new
(cdr (cdr '(a b c d e)))
{% endhighlight %}

Choosing good values for constants is difficult, but the current set
seems to be a good tradeoff between performance, the likelihood of
finding a result, and the number of useful results.

## Ranking Suggestions

Now we have more possibilities, ordering our suggestions is more
complex. The first prototype didn't always get the ordering correct:

{% highlight common-lisp %}
;; Inputs (one per line):
0

;; Desired output:
1

;; Suggestions:
(+ 0 1) ; <- new
(- 1 0) ; <- new
(1+ 0)
{% endhighlight %}

The user is probably looking for the increment function, `1+`. `(+ 0
1)` feels like stating the obvious.

Suggest.el prefers function calls that don't require extra arguments,
giving us a better order:

{% highlight common-lisp %}
;; Inputs (one per line):
0

;; Desired output:
1

;; Suggestions:
(1+ 0)
(+ 1 0) ; <- new
(- 1 0) ; <- new
{% endhighlight %}

## Smarter Search

If suggest.el tries a function and it returns, we save it. If it isn't
the value we're looking for, it's saved to a list of 'intermediate'
values. This is important for finding nested function calls.

Some intermediate values are very common, especially when exploring
arithmetic. There are many ways you can convert an integer to the same
float value, for example:

{% highlight common-lisp %}
(sqrt 0) ;=> 0.0
(float 0) ;=> 0.0
(fround 0) ;=> 0.0
(ffloor 0) ;=> 0.0
(fceiling 0) ;=> 0.0
(ftruncate 0) ;=> 0.0
{% endhighlight %}

Suggest.el previously considered every single way of generating the
same value. v0.4 only tries each unique value 3 times. This allows us
to explore more unique possibilities, increasing the likelihood of
finding a result before giving up.

## Literature Review

Finally, I've spent some time researching similar tools, and
documented them in
[the related projects section](https://github.com/Wilfred/suggest.el#related-projects).

Not only has it been interesting to see other approaches, suggest.el
has benefited from the comparisons. The Smalltalk Finder, for example,
also explores bitwise operations, so suggest.el does too!

If there are simple code snippets that you think suggest.el should
find, please
[file a bug](https://github.com/Wilfred/suggest.el/issues/new). I'm
routinely surprised by the results it finds, but I'm sure it could be
smarter still.
