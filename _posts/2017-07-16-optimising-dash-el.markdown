--- 
layout: post
title: "Optimising Dash.el"
tags:
 - emacs
---

Dash.el is a lovely library, and one of the most popular on MELPA. If
we can squeeze every last drop of performance out of it, everyone
benefits.

Let's take a look at the black art of making elisp faster.

## Measure First!

Chris Wellons has a
[great optimisation blog post](http://nullprogram.com/blog/2017/01/30/) that
discusses the performance overhead of creating lambdas with `mapcar`.

If we look at `--map`, it does indeed create anonymous functions:

{% highlight common-lisp %}
(defmacro --map (form list)
  "Anaphoric form of `-map'."
  `(mapcar (lambda (it) ,form) ,list))
{% endhighlight %}


Creating anonymous functions instantiates a closure, which isn't
free. Let's write an iterative equivalent:

{% highlight common-lisp %}
(defmacro --map-loop (form list)
  (declare (debug (form form)))
  (let ((result-sym (make-symbol "result")))
    `(let (,result-sym)
       (dolist (it ,list)
         (push ,form ,result-sym))
       (nreverse ,result-sym))))
{% endhighlight %}

| List Length | mapcar (seconds) | dolist (seconds) |
| :---------- |         -------: |           -----: |
| 1           |         0.000010 |         0.000028 |
| 1,000       |           0.0027 |           0.0079 |
| 100,000     |             0.74 |             1.24 |

(Times are in seconds, and the
[full source is here](https://gist.github.com/Wilfred/d51db0a1433ec4abdbca58a0dec039a5) if
you want to reproduce my results.)

Other Emacsers have [observed `dolist` outperforming `mapcar`](https://gist.github.com/Wilfred/d51db0a1433ec4abdbca58a0dec039a5#gistcomment-2019226) for
short lists. It's clearly not a speedup in all situations. `mapcar` is
primitive, and primitives tend to be fast.

## Matching Primitive Performance

Some dash.el functions are equivalent to primitive functions. For
example, `-first-item` is equivalent to `car`, `-drop` is equivalent
to `nthcdr`.

We could write `-first-item` like this:

{% highlight common-lisp %}
(defun -first-item (lst)
  (car lst))
{% endhighlight %}

However, this adds the overhead of an extra function call compared
with calling `car` directly. Instead, dash.el does this:

{% highlight common-lisp %}
(defalias '-first-item 'car)
{% endhighlight %}

Let's do a small benchmark, to ensure that `defalias` giving us the
peformance we want:

| Approach         | time (seconds) |
| :----------      |       -------: |
| custom function  |         0.1399 |
| alias            |         0.0055 |
| use car directly |         0.0050 |

([Full source code here](https://gist.github.com/Wilfred/3ee025116cff82169435262f44bffb0c).)

For shame! Our alias still isn't as fast as using the primitive. Let's
compare the disassembly using `M-x disasemble`.

{% highlight common-lisp %}
(defalias 'car-alias 'car)

(defun use-car-alias (x)
  (car-alias x))
;; byte code for use-car-alias:
;;   args: (x)
;; 0       constant  car-alias
;; 1       varref    x
;; 2       call      1
;; 3       return    

(defun use-car-directly (x)
  (car x))
;; byte code for use-car-directly:
;;   args: (x)
;; 0       varref    x
;; 1       car       
;; 2       return    

{% endhighlight %}

These are not the same! There's a `car` bytecode when we
use the `car` function directly.

With a little
[help from the Emacs Stack Exchange](https://emacs.stackexchange.com/q/30064/304),
we can see that byte-opt.el looks for `'byte-opt` properties on
functions. If a function symbol has this property, the byte compiler
will replace the function with custom bytecode.

{% highlight common-lisp %}
;; Ensure that calls to `-first-item' are compiled to a single opcode,
;; just like `car'.
(put '-first-item 'byte-opcode 'byte-car)
(put '-first-item 'byte-compile 'byte-compile-one-arg)
{% endhighlight %}

This makes performance of `-first-item` indistinguishable from `car`!
We do lose the ability to advise `-first-item`, but that's not
possible with `car` either.

## Sufficiently Smart Compilers

What about functions that aren't just aliases? Can the byte
