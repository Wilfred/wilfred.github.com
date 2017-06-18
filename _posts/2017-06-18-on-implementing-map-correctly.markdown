--- 
layout: post
title: "On Implementing Map Correctly"
---

It's surprisingly difficult to implement a robust `map` function a
lisp. Let's take a look at how easy it is to go wrong.

## The Textbook Implementation

Many textbooks have an elegant implementation that looks something
like this:

{% highlight lisp %}
(define (map1 fn items)
  (if (pair? items)
      (cons (fn (car items))
            (map1 fn (cdr items)))
      '()))
{% endhighlight %}

For pedagogical purposes, this is great. It shows recursion on a cons
cell and is great for new lispers.

## Recursing

Our first problem is that we're consuming stack space proportionate to
the size of our list:

{% highlight lisp %}
(define (list-of-length n value)
  (if (zero? n)
      '()
      (cons value (list-of-length (- n 1) value))))
      
;; Stack overflow in some Scheme implementations.
;; Note that Racket and recent Guile will allocate larger
;; stacks until you run out of memory.
(map1 (lambda (x) x) (list-of-length 10000 0)
{% endhighlight %}

How do we fix this? Let's make it tail-recursive! We define a helper
function `iter` that calls itself in the tail position:

{% highlight lisp %}
(define (map2 fn items)
  (define (iter items accum)
    (if (pair? items)
        ;; We put new items at the head of the accumulator
        ;; (reversing the list) to ensure iter is tail-recursive.
        (iter (cdr items) (cons (fn (car items)) accum))
        accum))
  (reverse (iter items '())))
{% endhighlight %}

## Improper Lists

Not every cons cell is a proper list. An improper list is a cons cell
where the second element (the cdr) is neither a cons cell nor the
empty list: `(cons 1 2)`.

Our current implementations silently ignore improper lists:

{% highlight lisp %}
>>> (map1 (lambda (x) x) '(cons 1 2))
(1)
{% endhighlight %}

This is surprising, and a standard library implementation should be
more helpful. We could solve this by applying the function to the tail
of the list too:

{% highlight lisp %}
(define (map3 fn items)
  (cond
    ((null? items)
     '())
    ((pair? items)
     (cons (fn (car items)) (map3 fn (cdr items))))
    (#t
     (fn items))))
{% endhighlight %}

This is a little bit dirty. If the user has created an improper list
by accident, they may be surprised by the values that `fn` is called
with. If we really want to recurse on an arbitrary cons cell
structure, a seasoned lisper would suggest `tree-map` instead.

Since we can detect this case, a friendlier approach is to inform the
user what happened. We expected a proper list, but didn't get one:

{% highlight lisp %}
(define (map4 fn items)
  (cond
    ((null? items)
     '())
    ((pair? items)
     (cons (fn (car items)) (map4 fn (cdr items))))
    (#t
     (error "Improper list"))))
{% endhighlight %}

## Circular Lists

use `list?`

rabbit and tortoise

## Closing Thoughts

Implementing `map` forces us to think about what sort of cons cells we
expect. Many newer languages sidestep this issue altogether. Racket
has immutable cons cells, so you cannot build circular
lists. Statically typed languages like Haskell have a cons cell
equivalent, but the type checker enforces that all lists are
well-formed.
