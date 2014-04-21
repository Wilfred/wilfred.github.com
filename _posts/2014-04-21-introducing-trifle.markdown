--- 
layout: post
title: "Introducing Trifle"
---

I've started developing a programming language, called Trifle. My day
job is writing Python, but I find it doesn't quite meet my needs in a
scripting language. Trifle was born out of this desire for a scripting
language that made me happy.

I'd love to share it with you. Here's the feature set we're working
towards.

## Expressive

Suppose you are writing a program that would benefit from a
`do-while` or `until` loop. In Python, you're out of luck. You
could write a PEP, but it may not be accepted if this language feature
isn't useful in the general case. If your PEP is accepted, you still
need to wait until a new version of Python is released with this
feature, and all your users have upgraded to that version.

Macros solve this. They give the user more freedom to find what works
best for the problem at hand. Trifle will offer macros.

## Optional Verification

Sometimes, you want to just mutate some lists and you don't want to
worry about types.  For prototyping, that's great. As your program
grows and matures, you'll want more confidence in the code.

You may want to make your datatypes immutable, use a linter or to use
a type checker. Trifle will offer both mutable and immutable
containers, plus optional static verification tools.

## Mathematically Sound

We want maths operations to default to well-behaved datatypes. We want
arbitrarily sized integers and exact fractions. When dealing with
inexact numbers, we want decimals rather than floats. This fixes many
mathematical gotchas that programmers have to learn.

Whilst this can be slower, we believe fixed-size integers and floats
should be used an optimisation, not the default.

## Interactive

Writing a program with a good REPL is great for learning the language,
incrementally writing code and testing it. You can send functions to
the interpreter individually, you can call them with test data, or
inspect their results.

Python doesn't have this. It's awkward to impossible to reload code
that you've imported. It's also awkward to mock functions as it
depends on how the original code imported it!

The better solution is allowing the user to change their current
namespace. Trifle will have this feature, enabling us to build a great
REPL on top.

## Fast Enough

Trifle is being written in RPython, which means we get a JIT for free.

## Researched

Finally, Trifle lisp design decisions will be made by comparing
different languages. We'll produce a document (per language feature)
listing the different possible approaches with their pros and cons. We
can't guarantee that every user will like every decision, but there
will be a justification for every design choice.

## Current Status

There's a crude interpreter available, the most exciting programs
being
[fizzbuzz](https://github.com/Wilfred/trifle/blob/c2a2e83b1fec1045126beead68a1fde00c7a6114/sample_programs/fizzbuzz.tfl)
and
[quicksort](https://github.com/Wilfred/trifle/blob/c2a2e83b1fec1045126beead68a1fde00c7a6114/src/prelude.tfl#L313). You
can follow
[Trifle's progress on GitHub](https://github.com/Wilfred/trifle), or
I'd love to have any feedback.
