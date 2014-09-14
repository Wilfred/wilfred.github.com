--- 
layout: post
title: "Comparative Macrology"
---

Macros are an extraordinary way to add expressive power to a
language. They also excel at preventing boilerplate.

I used to think that macros were just a feature that a language
included or lacked. In practice, there's a huge design space of how
macro systems can be implemented.

I thought it would be interesting to take a look at implementing the
same two macros in different languages. We'll look at both established
macro systems and newer languages. We'll compare readability,
robustness and safety in each language.

There are other problems macro systems must solve that we won't look at
here. We won't look at macro-expansion performance, how informative
error messages are, or the readability of backtraces in macro-expanded
code. We also won't look at ensuring our editor correctly highlights
and indents code using our macros.

## The Macros

We'll look at two macros, one that requires hygiene, and one that
requires variable capture.

Hygiene is where we want to introduce a variable name that is not used
anywhere in the program. Here's a `swap` macro, and we will need to ensure
that `tmp` is unused elsewhere:

{% highlight js %}
// Using the macro:
swap(x, y);

// Should be equivalent to this:
var tmp = x;
x = y;
y = tmp;
{% endhighlight %}

For variable capture, we'll create a `each-it` macro that sets `it` to
the current value of the item in our array/list. This is an
'anaphoric' macro ('anaphora' just means 'it').

{% highlight js %}
// Using the macro:
var myArray = [1, 2, 3];
eachIt(myArray) {
    console.log(it);
}

// Should be equivalent to this:
var myArray = [1, 2, 3];
for (var i=0; i<myArray.length; i++) {
    var it = myArray[i];
    console.log(it);
}
{% endhighlight %}

Note that our macros are being treated as first-class syntax. We want
to use `eachIt` just the same way we would use a `for` loop.

If you're interested in running this code, it's
[all available in a git repo](https://github.com/Wilfred/comparative_macrology).

## C (1972)

First, let's look at C's macro preprocessor. C's macros are textual
(rather than manipulating a parse tree), and the preprocessor only
does one pass through the file. As a result, we can't recurse, there
are many things we can't express, and they're error prone.

Whilst there's no way to write `each-it`, we can implement `swap`:

{% highlight c %}
#define SWAP(x, y) {        \
        typeof (x) tmp = x; \
        x = y;              \
        y = tmp;            \
    }
{% endhighlight %}

By using a nested scope, our macro is hygienic. We're also able to use
`typeof` to ensure our `SWAP` macro works with any type.

Other languages make use of similar preprocessors (C++, Haskell,
Ocaml), but these are similarly limited. Let's move on to some
full-power macro systems.

## Common Lisp (1984)

Common Lisp provides extensive macro facilities, and idiomatic code
often uses macros extensively.

Macros in CL are just functions that run at compile time, which makes
their execution easy to reason about. CL also has a lightweight
backquote syntax for building expressions. However, we have to be
careful about our output, and manually generate symbols to avoid
accidental variable capture.

{% highlight lisp %}
(defmacro swap (x y)
  (let ((tmp-sym (gensym)))
    `(let (,tmp-sym)
       (setf ,tmp-sym ,x)
       (setf ,x ,y)
       (setf ,y ,tmp-sym))))
{% endhighlight %}

This implementation has the nice property that `setf` understands
place structures. This means we can write things like
`(swap (first my-list) (second my-list))`.

However, we could be more defensive. Using `gensym` protects us from
accidentally shadowing a local `tmp` variable, but we're still
assuming that the user hasn't shadowed `setf`.

By contrast, the lack of hygiene works really well when we want to
deliberately capture variables. `each-it` is very readable:

{% highlight lisp %}
(defmacro each-it (list &rest body)
  `(loop for it in ,list
      do ,@body))
{% endhighlight %}

## newLisp (1991)

newLisp supports f-expressions. These are similar to macros, but
they're evaluated at runtime. A macro is a function that takes an
abstract syntax tree (AST) and returns another AST. An f-expression is
like a function except it can choose which of its arguments will be
evaluated and when.

F-expressions have largely fallen out of favour today. With
conventional macros, you can expand all your code and do static
analysis, e.g. check for undefined variables. With f-expressions, you
lose this ability (though it is an active research topic).

TODO

## R5RS Scheme (1998)

A major selling point of Scheme's macro system, in contrast to earlier
systems, is that it's hygienic by default. Instead of a function with
quasiquotes, `syntax-rules` is a 'pattern language'. This is
declarative and slightly magical.

{% highlight scheme %}
(define-syntax swap!
  (syntax-rules ()
    ((swap x y)
     (let ((tmp x))
       (set! x y)
       (set! y tmp)))))
{% endhighlight %}

In this example, `syntax-rules` is able to work out that `tmp` is
only used within the macro, so it should be renamed, but `x` and `y`
should be substituted in.

I don't believe it's possible to write unhygienic macros in R5RS
Scheme. However, R6RS introduced `syntax-case`, which lets us write
`each-it`. Unfortunately, R7RS does not include `syntax-case`, so we
can't guarantee that all conforming implementations will support our
macro. In practice, `syntax-case` is widely implemented.

{% highlight scheme %}
(define-syntax each-it
  (lambda (x)
    (syntax-case x ()
      ((_ lst body ...)
       (with-syntax ((it (datum->syntax x 'it)))
         #'(for-each (lambda (it) body ...) lst))))))
{% endhighlight %}

This has become significantly more complex than our Common Lisp
implementation. We have quasiquotes, explicit 'syntax' object
wrapping, and generally more code.

R6RS has been criticised for providing multiple macro systems without
clear guidance on which to use. `syntax-case` is more powerful, and we
can write `syntax-rules` in terms of it. Nonetheless, Scheme
implementations are still experimenting. For example, Racket
[introduces the notion of a 'syntax parameter'](http://www.schemeworkshop.org/2011/papers/Barzilay2011.pdf),
which gives users fine grained control of where a parameter may be
used. Syntax parameters have the amazing feature that you can write
unhygienic macros that compose safely.

## Clojure (2007)

Clojure's macro system is closest in spirit to Common Lisp's. However,
we can't write a direct equivalent to `swap!` as Clojure has strict
limits on mutability. Instead, we'll mutate refs, which are threadsafe
shared mutable storage and can only be modified inside a transaction.

{% highlight clojure %}
(defmacro my-swap! [x y]
  `(dosync
    (let [tmp# (deref ~x)]
      (ref-set ~x (deref ~y))
      (ref-set ~y tmp#))))
{% endhighlight %}

Working with the immutability has made this example a little more
verbose, forcing us to add `dosync` and `deref`. Clojure's surface
syntax also differs from CL's, which can make this macro confusing
initially. Note that Clojure uses commas as whitespace, so users can
write `(1, 2, 3)` or `{1 2, 3 4}`. This means commas cannot be used as
to splice values into our quasiquote, so CL's `,x` is written `~x`
in Clojure.

## sweet.js (2012)

sweet.js is a tool for writing macros in JavaScript. Whilst JS is
interpreted, many JS projects have a 'compilation' workflow including
concatenation and minification. This enables you to simply add
sweet.js as another build step.

{% highlight js %}
macro swap {
    rule { ($x, $y) } => {
        var tmp = $x;
        $x = $y;
        $y = tmp;
    }
}
{% endhighlight %}

sweet.js macros are hygienic and reminiscent of `syntax-case`. The
hygiene means we can just use a symbol called `tmp` safely. By
default, it's over-aggressive about variable renaming (which makes
the compiled output less readable). This is rarely an issue in
practice as it supports source maps and has a `--readable-names` flag
for ES5 implementations.

When using `--readable-names`, sweet.js produces impressively readable
code. In the expansion of `swap` example, our variable `tmp` is not renamed
unless the surrounding code defines a `tmp` variable. This is nicer
than explicitly calling `gensym`, as that will always give us a random
symbol name. It even preserves some comments in our macro template.

sweet.js also explicitly supports breaking hygiene, and the
documentation includes examples. `eachIt` is written with `case`,
which is very similar to Scheme's `syntax-case`.

{% highlight js %}
macro eachIt {
    case { $eachIt_name ($x) {$y...} } => {
        // Create an `it` variable using the lexical context
        // of `eachIt`.
        var it = makeIdent("it", #{$eachIt_name});
        letstx $it = [it];
        return #{
            for (var i=0; i<$x.length; i++) {
                (function($it) {
                    $y...
                })($x[i]);
            }
        }
    }
}
{% endhighlight %}

As with our Scheme macros, it's more complex when we want to break
hygiene. It's also slightly more verbose because we have to use an
anonymous function to introduce a new scope for `it`.

## Rust (2012)

## Julia (2012)

Julia has a lot of Lisp influences, offering a hygienic macro system
that allows you to define syntax that is indistinguishable from the
built-in syntax. However, macro calls have a @ prefix (e.g.
`@swap(x, y)`) so readers can distinguish between user-defined macros
and built-in functions/keywords.

Whilst this makes code more explicit, it does force some keywords into
the Julia compiler that could otherwsie live as macros in the standard
library.

{% highlight julia %}
macro swap!(x, y)
    quote
        tmp = $(esc(x))
        $(esc(x)) = $(esc(y))
        $(esc(y)) = tmp
    end
end
{% endhighlight %}

Julia uses `$foo` as variable interpolation (i.e. equivalent to CL's
`,foo`). This is elegant as it is consistent with other parts of the
language: string interpolation (`"the value of x is $x"`) and command
interpolation use the same syntax.

Since we're accessing variables in the scope of macro call
environment, we're forced to use `esc()` to escape these
variables. There are
[plans to improve this](https://github.com/JuliaLang/julia/pull/6910)
which will mean that we can just write `$x` and `$y`.

The hygiene still helps us with our `tmp` variable, as Julia sees
we're assigning to `tmp` and ensures it is renamed to be unique.

{% highlight julia %}
macro each_it(arr, body)
    quote
        for it in $arr
            $body
        end
    end
end
{% endhighlight %}

Surprisingly, Julia's hygiene cheerfully allows variable capture. It
renames `it` in both the loop header and in `$body`. It also saves us
splicing in the `$body`, because we're passing in a `begin...end`
block when we use it.

{% highlight julia %}
@each_it my_array begin
    println("it: $it")
end
{% endhighlight %}

## Conclusion

Adding any macro system is a wise thing to do when designing a language,
as it allows users to improve the language itself. Designing a good
macro system is hard.

Whilst it's tempting to see a parse tree as simply nested lists, it's
insufficient in practice. A parse tree node should have line numbers
(Clojure's macros can access this), and may have a type associated
(Julia's macros can access this).

A language must expost a set of tools that empower the user to write,
document and debug macros with minimal friction. If they're too hard
to use, users won't use them. I had to cut a number of languages
from early drafts of this post because their docs were so poor or I
I needed extensive knowledge of the grammar or compiler itself!

A language designer must decide what kind of datatype to expose to


