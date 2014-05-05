--- 
layout: post
title: "Choosing A Host Language"
---

If you're writing an interpreter, what language do you choose? What
effect does your decision have?

I've noticed that many languages inherit features from the host
implementation language. Haskell code uses the C preprocessor. PHP
standard library functions sometimes have `out` parameters. Python's
exceptions expose error numbers from C headers.

If you write an interpreter for a new wonderful programming language
in C, you will end up writing a lot of C. The majority of your time
will be taken up by implementation details in C. It's hard to avoid
picking up features of the host language without weighing their
merits.

I only know two ways to avoid this. One approach is to use a
high-level language with semantics close to your target language. This
helps you avoid regularly using host language features that are
inappropriate for your target language. Low-level details can be also
be distraction, slowing you down, since your semantics will change
significantly early on.

The other approach is to **avoid implementation-specific
thinking**. This is my favourite way of building the right
features. This separates the design process into two steps.

First, you consider your goals and what features would meet those
goals. You write them down, giving examples of what usage looks
like. Once you've settled on what the feature should look like, only
then start worrying about implementation.

It's too easy to think about implementation too early in the design
process. Many GUI apps have been written that expose too much database
structure. Don't think about implementation prematurely.

Where does this leave
[Trifle lisp](https://github.com/Wilfred/trifle)? Trifle is written in
[RPython](http://pypy.readthedocs.org/en/latest/getting-started-dev.html).

RPython has garbage collection and higher-order functions, which suits
Trifle. It
[helps write fast interpreters quickly](http://tratt.net/laurie/blog/entries/fast_enough_vms_in_fast_enough_time). However,
unlike Trifle, it's statically typed, lacks closures and it's
(essentially) compiled.

This is a major motivation behind Trifle Lisp Design documents. We
need to consider the range of possible design decisions, and make
decisions _only_ according to Trifle's goals.

This involves comparing many other programming languages design
choices, and iterating the interpreter design. It's also a whole lot
of fun!
