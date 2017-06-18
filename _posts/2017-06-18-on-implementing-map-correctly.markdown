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

