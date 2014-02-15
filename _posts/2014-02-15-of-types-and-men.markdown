--- 
layout: post
title: "Of Types and Men"
---

A type system is a wonderful thing. I find them very helpful in
weeding out bugs early on in the development process. However, I am
convinced that mainstream statically typed languages are less
expressive and less ergonomic than they could be. Here's what I'd like
to see in a strongly-typed programming language.

## 1. Invalid types as warnings

You're a capable man or woman, and you've written a wonderful
program. You've written the code, satisfied the type checker, and
ironed out the bugs.

Perhaps it's a game where characters can be armed or not, so you use
`Maybe Weapon` (Haskell) or a nullable `Weapon` (Java).

Then, requirements change. You will have characters with multiple
weapons! Your type is now `[Weapon]` (Haskell) or `List<Weapon>`
(Java). You are forced to go through your entire codebase making
changes: some trivial, some logic changes.

At no point in this process can you run the program to see if any of
the updated code is correct. Perhaps you'd like to try to spawn an
unarmed character from the shell with your updated logic. This isn't
really possible in today's statically typed languages.

If you're lucky, your changes might be isolated in certain files which
can be run independently. If you're unlucky, you'll be forced to try
ad-hoc commenting out to placate the type checker.

### Solution 1a: Not yet typed

Haskell, interestingly, has a notion of 'not yet typed'. If you're
building a new program and don't want to implement everything at once,
you can use `undefined` which will pass type checking for anything.

For example, if you're building a restaurant simulator, you might
write:

{% highlight haskell %}
chefPrepare :: [Ingredient] -> Dessert
chefPrepare = undefined
{% endhighlight %}

You can then happily write other functions that call `chefPrepare` and
the type checker won't get in your way.

Haskell can even give you hints as to what you need, when you're
halfway through implementing a program. With the TypeHoles extension,
you can leave placeholders in your functions and Haskell will tell you
what type value or function you need to use.

This is helpful during the initial implementation, but doesn't help us
in our refactoring situation above.

### Solution 1b: Optional typing

Some lisps, including Scheme and Clojure, provide type checkers as a
library. These aren't part of the core language, and the
compiler/interpreter does not enforce type correctness. Users can use
these 'optional type checkers' to verify some or all of their code, as
they please.

This is a wonderfully pragmatic approach, and works well. However, the
type systems supported by optional type checkers is not always as
sophisticated as a Haskell user would be accustomed to.

There are other interesting advantages of type checking being a
library. It can evolve separately to the language itself, giving users
more opportunities to experiment. It also makes it much easier to call
the type checker programmatically, a feature that some statically
typed language do not offer at all.

### Solution 1c: Dynamically typed dialects

RPython is a quirky little language. It's a proper subset of Python,
so you can run your RPython programs on the standard Python
interpreter (called CPython).

This allows you to run your program at any point, regardless of how
invalid the types are. You can invoke the RPython compiler and have it
check your types. However, if you find the errors confusing, or type
checking is simply interrupting your flow, you can just run the
program anyway.

Sometimes, you may find it helpful to deal with concrete errors with
concrete values. By running your RPython program in CPython, you can
slap print statements everywhere until your hacky prototype is
working. Afterwards, you can appease the type checker.

Pypy, an interpreter written RPython, is developed something like
this. The developers typically ensure their unit tests pass first,
then fix any remaining type issues. It's a great workflow, and few
languages let you do this.

## 2. A Universal Type

Sometimes you need a universal type. There are programs that simply
cannot be written without it. Java has this, and calls it
`Object`. Haskell has `Any` and `Dynamic`, but it's not quite as
expressive or concise.

Some notable use cases include:

I. Flattening an arbitrarily nested list. This is easily done with
recursion and inspecting the type of a value. That said, a determined
Haskell hacker can flatten a nested list with
[typeclass abuse](http://stackoverflow.com/a/5994717).

II. Applying an arbitrary length list of functions
`[a -> b, b -> c, ...]` to a value of type `a`.

III. Serialising or deserialising arbitrary values.

IV. `eval`.

## 3. The Environment as an effect

Pure language languages, such as Haskell, force you to be explicit
about effects that your functions may cause. However, the programming
environment itself is a piece of state, and Haskell does not allow us
to declare that we will interact with it.

There are many (extremely useful) functions whose behaviour we could
model if we treat the environment as an effect. Haskell provides the
`State` monad; let's suppose we have an `Environment` type and a
`WithEnvironment` monad.

### Reading Types In The Environment

Python has a `pickle` module that allows you to convert arbitrary
types to strings. This is a very powerful concept. For example, it
makes memoisation for arbitrary functions straightforward (I
understand it's possible in Haskell, but it's extremely non-trivial).

If we model the environment as an effect, dumping and loading
serialisations is simply:

{% highlight haskell %}
serialise :: Any -> WithEnvironment String
-- Assume we return Nothing if we try to deserialise
-- a type that isn't present in our current environment.
deserialise :: String -> WithEnvironment (Maybe Any)
{% endhighlight %}

### Writing Types In the Environment

We gain even more expressive power if we looking at mutating the
environment. Two excellent examples of this are mocking out functions
for testing, and `eval`.

Now, a purist might ask why you'd even want `eval`, since it can cause
some spectacular errors. One big advantage of providing `eval` is that
it allows you to write an interpreter. In many scripting languages,
the users have developed interpreters that are much friendlier and
more capable than the built-in one.

For example, Python has bpython (which provides pop-up help using a
curses interface) and ipython (which provides helper syntax, shell
conveniences, history and sophisticated tab completion). Ruby has pry
(which provides debugging and live editing), and Common Lisp's slime
paired with swank is extraordinary for interactive development.

Given this, an `eval` function would have the following type:

{% highlight haskell %}
-- You'd probably want eval to throw various exceptions
-- in the case of bad input.
eval :: String -> IO (WithEnvironment Any)
{% endhighlight %}

## Conclusion

There are still many forms of type systems that are little explored in
programming languages. When evaluating a type system, ask yourself:

I. What bugs can this type system catch?

II. What proportion of those bugs are difficult to catch otherwise?

III. What functions are difficult to express in this type system?

IV. What functions cannot be expressed in this type system?

V. What, if any, escape hatches does type system provide?

VI. Is this type system sound?

VII. How comprehensible are its type errors? How helpful are its
suggested fixes?

With those questions in mind, it's much easier to answer that nebulous
question: _do I want to program with this type system?_
