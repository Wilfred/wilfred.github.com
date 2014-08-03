--- 
layout: post
title: "Recursion, TCO, and You"
---

Recursion is an excellent tool in a developer's toolbox, yet it is
frequently misunderstood. Used correctly, it can give programs that
are fast, readable and succinct. Used incorrectly, it can produce some
tricky bugs.

Let's look at an example. Suppose we write a function that calculates
x to the power of y for integers. In early programming languages,
[recursion was a rather controversial feature](http://vanemden.wordpress.com/2014/06/18/how-recursion-got-into-programming-a-comedy-of-errors-3/),
so you'd just use a loop:

{% highlight python %}
def to_the_power_of(base, exponent):
    result = 1

    # This is O(n), see
    # http://mitpress.mit.edu/sicp/full-text/sicp/book/node18.html
    # for faster approaches.
    while exponent > 0:
        result = result * base
        exponent -= 1

    return result
{% endhighlight %}

By the mid-60s, we had languages that supported recursion. You could
write this power function recursively:

{% highlight python %}
def to_the_power_of(base, exponent):
    if exponent == 0:
        return 1
    else:
        return base * to_the_power_of(base, exponent - 1)
{% endhighlight %}

However, this innocent looking code can fail us if `exponent` is
greater than the maximum depth of the stack. In Python, this is
the stack depth is 1,000, so our function doesn't support values of
`exponent` greater than 1,000.

This risk of stack overflow makes recursion much less
useful. Even though recursion is ideally suited for some class of
programs (e.g. depth-first search is much more hassle when written
iteratively), we cannot use it for arbitrary data with a fixed size
stack.

In the 70s, Scheme popularised tail-call optimisation. The Scheme
standard requires implementations to optimise function calls (if they
are 'in the [tail position](https://en.wikipedia.org/wiki/Tail_call)',
i.e. the caller has nothing left to do) such that they use constant
stack space. With care, we can now write a recursive power function
that works for arbitrary values.

{% highlight python %}
# Scheme's syntax is a little different, but the function
# would be exactly equivalent.
def to_the_power_of(base, exponent, accum=1):
    if exponent == 0:
        return accum
    else:
        return to_the_power_of(base, exponent - 1, accum * base)
{% endhighlight %}

This is a big improvement, and enables us to write many more functions
recursively. However, the programmer must know exactly what forms of
recursion will be optimised. It's easy to carelessly refactor a
function such that it is no longer tail-recursive. This is especially
risky when running unit tests, as tests often use small datasets,
hiding the stack overflow you've accidentally introduced.

However, what if we want to write recursive functions that aren't
strictly tail-recursive? For example, we might want to save the value of our
recursive call before returning it.

{% highlight python %}
def to_the_power_of(base, exponent, accum=1):
    if exponent == 0:
        result = accum
    else:
        result = to_the_power_of(base, exponent - 1, accum * base)

    return result
{% endhighlight %}

[The scheme standard defines tail-calls](http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-14.html#node_sec_11.20),
but also allows implementations to recognise other equivalent
recursive forms (see the note at the end of the section). However,
we're now in an awkward situation where changing Scheme interpreter
could break our program if we depend on that behaviour.

In an ideal world, we'd be able to write this function without
an accumulator, as in our original recursive version.

{% highlight python %}
def to_the_power_of(base, exponent):
    if exponent == 0:
        return 1
    else:
        return base * to_the_power_of(base, exponent - 1)
{% endhighlight %}

We're in luck -- some languages do provide this! For example, Haskell,
Oz, and [even some C compilers](http://ridiculousfish.com/blog/posts/will-it-optimize.html),
are smart enough to optimise this without us providing an accumulator.

This is called 'tail recursion modulo cons', which is a tail-recursive
function that also applies a 'constructor' function to the result. A
constructor function is a function that is both commutative and
associative. Many useful functions, such as `*` and
`cons` (a lisp function for building a list), meet these criteria.

Where does this leave us? We've seen that different programming
languages support writing robust recursive functions in various forms,
but you need to be aware of which functions will be optimised. To make
matters worse, it's not possible to provide a full stack trace for
functions that have been optimised this
way. [Guido cites this as a reason for not providing TCO in Python](http://neopythonic.blogspot.co.uk/2009/04/tail-recursion-elimination.html).

Can we do better?

In [Trifle lisp](https://github.com/Wilfred/trifle), we plan to take
an explicit approach. We plan to make TCO opt-in, so our power
function is labelled as requiring TCO.

{% highlight lisp %}
(function to-the-power-of (base exponent accum)
  (if (zero? exponent)
    accum
    (to-the-power-of base (dec exponent) (* accum base))
  )
)

; Not yet available in Trifle.
(set-tco! to-the-power-of)
{% endhighlight %}

Using `set-tco!` documents that the programmer expects tail-call
optimisation, and acts as an assertion. If the function is refactored
to a form that cannot be optimised, `set-tco!` will throw an
error. This allows programmers to depend on this behaviour.

Opting-in also has the nice property that programmers don't have to
use tail-call optimisation. If they're developing or debugging and
want full stack traces, we can provide them. If we add optimisation
for tail-call modulo cons, programmers can depend on that optimisation
too.

The obvious disadvantage of opting-in is the loss in performance for
functions that we could optimise but don't. gcc compromises by
providing tail-call optimisation at its higher optimisation levels. We
could do exactly the same -- the only loss is the completeness of
stack traces.

In conclusion: Recursion is a great tool in your toolbox. If it makes
your code clearer, absolutely consider using it. Make sure you are
aware of what guarantees your language provides, how big your stack
is, and how big your input data will be.
