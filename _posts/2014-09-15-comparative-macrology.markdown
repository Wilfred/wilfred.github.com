--- 
layout: post
title: "Comparative Macrology"
---

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
code. We also won't consider ensuring our editor correctly highlights
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
does one pass through the file. As a result, we can't recurse
([though co-recursion is possible](https://github.com/pfultz2/Cloak/wiki/C-Preprocessor-tricks,-tips,-and-idioms)),
there are many things we can't express, and they're error prone.

{% highlight c %}
#define SWAP(x, y) {                            \
        typeof (x) x##y = x;                    \
            x = y;                              \
            y = x##y;                           \
    }
{% endhighlight %}

By using a nested scope, our macro is hygienic. To ensure our
temporary variable is fresh, we concatenate the variable names.  We're
also able to use `typeof` operator (a GCC extension) to ensure our
`SWAP` macro works with any type.

{% highlight c %}
#define EACH_IT(array)                                  \
    for (typeof(*array)* it = array;                    \
       it != array + sizeof(array)/sizeof(array[0]);    \
       it++)
{% endhighlight %}

`EACH_IT` has no issues with being unhygienic, as the C preprocessor
imposes no such restrictions on our output.

Other languages make use of similar preprocessors (C++, Haskell,
OCaml), but those preprocessors are similarly limited. Let's move on to some
full-power macro systems.

## Common Lisp (1984)

Common Lisp (CL) provides extensive macro facilities, and idiomatic code
often uses macros.

Macros in CL are just functions that run at compile time, which makes
their execution easy to reason about. CL also has a lightweight
backquote syntax for building expressions. However, we have to be
careful about our output, and manually generate fresh, uninterned symbols to avoid
accidental variable capture.

{% highlight common-lisp %}
(defmacro swap (x y)
  (let ((tmp-sym (gensym)))
    `(let ((,tmp-sym ,x))
       (setf ,x ,y)
       (setf ,y ,tmp-sym))))
{% endhighlight %}

This implementation has the nice property that `setf` understands
place structures. This means we can write things like
`(swap (first my-list) (second my-list))`.

However, we could be more defensive. Using `gensym` protects us from
accidentally shadowing a local `tmp` variable, but we're still
assuming that the user hasn't shadowed `setf`. Such code would be
invalid according to the CL specification (and SBCL gives an error),
but it still may occur in user code in macro expansions.

By contrast, the lack of hygiene works really well when we want to
deliberately capture variables. `each-it` is very readable:

{% highlight common-lisp %}
(defmacro each-it (list &rest body)
  `(mapc (lambda (it) ,@body) ,list))
{% endhighlight %}

## newLisp (1991)

newLisp supports f-expressions. These are similar to macros, but
they're evaluated at runtime. A macro is a function that takes an
abstract syntax tree (AST) and returns another AST. An f-expression is
a runtime expression that can choose which of its arguments will be
evaluated and when.

F-expressions have largely fallen out of favour today (the definitive
criticism is [this paper](http://www.nhplace.com/kent/Papers/Special-Forms.html)). With
conventional macros, you can expand all your code and do static
analysis, e.g. check for undefined variables. With f-expressions, you
lose this ability (though it is an active research topic).

Recent versions of newLisp
[also support expansion macros](http://www.newlisp.org/downloads/newlisp_manual.html#macro),
but let's explore what f-expressions would look like.

{% highlight common-lisp %}
;; swap is already defined in newlisp
(context 'my-swap)
(define-macro (my-swap:my-swap x y)
    (set 'tmp (eval x))
    (set x (eval y))
    (set y tmp))
(context MAIN)
{% endhighlight %}

newLisp's f-expressions are challenging to write if you've only
written normal macros. There's no `macroexpand`, no quasiquotes and
scoping is dynamic. We don't have the same separation between runtime
and compiletime, so we can just call `set` directly. 

{% highlight common-lisp %}
(define-macro (each-it lst)
    (let ((template (list 'dolist (list 'it lst))))
      ;; args holds all the arguments that we
      ;; haven't bound in our parameter list
      (extend template (args))
      (eval template)))
{% endhighlight %}

`each-it` also suffers from a lack of quasiquotes. newLisp is
unhygienic, so we don't need to do any extra work to capture `it`.

## R5RS Scheme (1998)

A major selling point of Scheme's macro system, in contrast to earlier
systems, is that it's hygienic by default. Instead of a function with
quasiquotes, `syntax-rules` is a 'pattern language'. This is
declarative and slightly magical when you first encounter it.

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
should be substituted in. Unlike our CL macro, we're even safe from
the user adding a macro or function called `set!`.

R5RS provides no facilities to break hygiene. However, R6RS introduced
`syntax-case`, which lets us write `each-it`. Unfortunately, R7RS does
not include `syntax-case`, so we can't guarantee that all conforming
implementations will support our macro. In practice, `syntax-case` is
widely implemented.

{% highlight scheme %}
(define-syntax each-it
  (lambda (x)
    (syntax-case x ()
      ((_ lst body ...)
       (with-syntax ((it (datum->syntax x 'it)))
         #'(for-each (lambda (it) body ...) lst))))))
{% endhighlight %}

This has become significantly more complex than our Common Lisp
implementation. We have explicit 'syntax' object wrapping, compile-time
function calls and generally more code.

Scheme implementations are still experimenting with providing safe,
readable macro systems. The most exciting system is Racket's notion of
a 'syntax parameter'
([great paper here](http://www.schemeworkshop.org/2011/papers/Barzilay2011.pdf)),
which gives users fine grained control of where a parameter may be
used. Syntax parameters have the amazing feature that you can write
unhygienic macros that compose safely. That said, the linked paper
gives a few examples where even syntax parameters aren't enough, and you
need to be fully unhygienic.

## Clojure (2007)

Clojure's macro system is closest in spirit to Common Lisp's. However,
we can't write a direct equivalent to `swap!` as Clojure has strict
limits on mutability. Instead, we'll mutate refs, which are threadsafe
shared mutable storage that can only be modified inside a transaction.

{% highlight clojure %}
;; swap! already exists in Clojure.
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
in Clojure. Finally, Clojure allows us to write `tmp#` to instruct the
compiler to call `gensym("tmp")` and substitute it in.

{% highlight clojure %}
(defmacro each-it [lst & body]
  `(doseq [~'it ~lst]
     ~@body))
{% endhighlight %}

Clojure has a really interesting notion of hygiene. During macro
expansion, all symbols are written in their fully qualified form
(e.g. `user.core/tmp`). This makes accidental variable capture much
harder. Furthermore, the language is compiled, and any references
to undefined variables is a compile-time error. As a result, Clojure
macro problems tend to be caught early.

For `each-it`, we have to unquote a quoted symbol to break
this hygiene. Our resulting macro is still very readable.

## sweet.js (2012)

sweet.js is a tool for writing macros in JavaScript (allowing
[some astounding syntax abuse](https://github.com/mozilla/sweet.js/issues/385)). Whilst
JS is interpreted, many JS projects have a 'compilation' workflow
including concatenation and minification. This enables you to simply
add sweet.js as another build step.

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
hygiene again means we can just use a symbol called `tmp` safely. By
default, it's over-aggressive about variable renaming, making the
compiled output less readable. This is rarely an issue in practice as
it supports source maps and has a `--readable-names` flag for ES5
implementations.

When using `--readable-names`, sweet.js produces impressively readable
code. In the expansion of `swap` example, our variable `tmp` is not renamed
unless the surrounding code defines a `tmp` variable! This is nicer
than explicitly calling `gensym`, as that will always give us a random
symbol name. sweet.js even preserves some comments in our macro template.

sweet.js also explicitly supports breaking hygiene, and the
documentation helpfully includes examples. `eachIt` is written with `case`,
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

> `macro_rules` is very experimental with a few annoying bugs and
> definitely scope for change (possibly arbitrarily large).
> 
> -- Huon Wilson (Rust core dev) [[source](http://www.reddit.com/r/rust/comments/2gcjhr/is_rust_more_concise_than_c/ckhvta3)]

Much of Rust is still in flux, but these samples have all been tested
against Rust 0.11.0. Since macros are still experimental, we have to
explicitly enable the feature.

{% highlight rust %}
#![feature(macro_rules)]
{% endhighlight %}

Rust's macro system is again similar to Scheme's `syntax-rules`. It
has the additional constraint that you must declare what they're
expecting as each macro argument (identifier, expression, etc). This
makes macros more explicit and enables better reporting when
misused.

{% highlight rust %}
macro_rules! swap {
    ($x:expr, $y:expr) => {
        {
            let tmp = $x;
            $x = $y;
            $y = tmp;
        }
    }
}
{% endhighlight %}

In this case, we've allowed `$x` and `$y` to be any expression, so we
can use l-values (e.g. `swap!(foo[0], foo[1]);`). If want to limit our
swap macro to simple variables, we would write `($x:ident, $y:ident)`
instead.

At time of writing, it's not possible to break hygiene without writing
a compiler plugin. At this point it's not really a macro, and it's
beyond my Rust knowledge.

## Julia (2012)

Julia has a lot of Lisp influences, offering a hygienic macro system
that allows you to define syntax that is indistinguishable from the
built-in syntax. However, macro calls have a @ prefix (e.g.
`@swap(x, y)`) so readers can distinguish between user-defined macros
and built-in functions/keywords.

Whilst this makes code more explicit, it does force some keywords into
the Julia compiler that could otherwise live as macros in the standard
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

Designing a good macro system is really hard. It should allow templates that
are visually similar to the expanded code, and provide convenient ways
to substitute values in. The designer must strike a balance between
hygiene that guarantees safety and hygiene that can be broken without
hurting readability.

I think the lowest ceremony, quasiquote based macros are the most
readable versions that I've shown here. The further your syntax is
from s-expressions, the harder this is to achieve.

To make matters worse, a parse tree is never actually a nested list,
making quasiquotes a convenient lie. A compiler's parse tree will
include metadata such as line numbers. Scheme actually exposes a
separate 'syntax' datatype, and Clojure exposes `&env` and `&form`
with metadata.

Finally, the ultimate measure of a macro system is its usage. How
often do users write macros, and how robust is the resulting code? I
had to cut a number of languages from early drafts of this post
because their docs were so poor or I needed extensive knowledge of
the grammar or even compiler itself!

If a language has a macro system, and if users use it, that's a
success in my book. I am always glad of the facility, even if I'm only
using macros written by others.
