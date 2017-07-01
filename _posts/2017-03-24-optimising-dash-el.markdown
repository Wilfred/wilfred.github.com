--- 
layout: post
title: "Optimising Dash.el"
---

Dash.el is a lovely library, and one of the most popular on MELPA. If
we can squeeze every last drop of performance out of it, everyone
benefits.

Let's take a look at the black art of making elisp faster.

## Primitives Are Your Friend

Chris Wellons has pointed out that `--map` creates an anonymous
function:

{% highlight common-lisp %}
(defmacro --map (form list)
  "Anaphoric form of `-map'."
  `(mapcar (lambda (it) ,form) ,list))
{% endhighlight %}

Creating anonymous functions can be slow -- we're instantiating a
closure. Can we do better?

CODE IN dashtest.el

Rather than calling `mapcar`, we can use an explicit loop and avoid
creating a closure. Does this actually make it faster?

PLOT

Foiled! Why is this?

The advantage of using `mapcar` is that it's a primitive, and
primitive are *fast*. 

## Matching Primitive Performance

Since primitives are faster, how can we ensure dash.el uses them to
the fullest?

To avoid the overhead of a function call, dash.el uses `defalias`
where possible. For example, `-first-item` is `car` and `-drop` is
`nthcdr`.

Let's do a small benchmark, to ensure that `defalias` giving us the
peformance we want:

;; Question: use a function call too?

(defun use-car-aliases (x)
  (-first-item x))

(defun use-car-directly (x)
  (car x))

PLOT

For shame! Our alias still isn't as fast as using the primitive. Let's
compare the disassembly using `M-x disasemble`:

byte code for use-car-directly:
  args: (x)
0       varref    x
1       car       
2       return    

byte code for use-car-alias:
  args: (x)
0       constant  -first-item
1       varref    x
2       call      1
3       return    

These are not the same! We're compiling to the `car` bytecode when we
use the `car` function directly.

Looking at byte-opt.el, we can see see byte-cop

## Sufficiently Smart Compilers

What about functions that aren't just aliases? Can the byte
