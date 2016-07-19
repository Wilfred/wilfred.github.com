--- 
layout: post
title: "Quantitative Debugging"
---

Can you systematically improve your debugging techniques?

Some of the best typists I know will practice words that they
habitually type incorrectly. Can we apply this idea to debugging?

To investigate my debugging effectiveness, I took notes on every time
I got stuck. Every time my programs threw an error, or did the wrong
thing: if the fix wasn't immediately obvious, I would log it.

## Data collected

For every time I got stuck, I recorded:

* A description of the problem
* The solution I eventually found
* The root cause of the issue
* Potential changes that would have sped up debugging

Halfway through data collection, I realised that time spent debugging
would also have been interesting. Even without this, the dataset is
still full of insights.

## What can we learn?

Once you have a log of all your debugging problems, you can ask
lots of interesting questions.

* What are the primary causes of my bugs?
* Am I using appropriate tools?
* Could I refine my debugging technique to fix bugs faster?
* Would I benefit from more static analysis?

## Limitations of the dataset

My bug log is very specific to me and may not apply to you. I might
have an effective coping mechanism for a class of bugs that you don't
have (or vice versa).

The data is also massively biased by the languages and tools that I
use. I have been primarily working with Python, Rust and Emacs Lisp
whilst I collected data. When using safe languages, I won't see memory
bugs. When using statically-typed languaes, I won't see type bugs.

Still, quantitative data can give new insights on how to improve my
debugging. Let's look at the results!

## Overview of results

Over the course of ? months, I logged 59 instances of getting stuck on
a bug.

-- plot of bugs over time

I manually categorised these into:

* Misleading error messages
* Misunderstandings of the programming language
* Programming language implementation bugs
* Lack of a type checker
* Needed static analysis
* Confusing APIs
* Misconfigured tools
* Tool bugs

-- numeric table by category

We can also look at bug breakdown by programming language used. This
is heavily skewed by my tool choice, so there's no obvious takeaway here.

-- numeric table by language

## Most common bug category

I was surprised to learn that my most common bug source was actually
**misleading error messages**. Poorly worded or vague messages would
lead me on a wild goose chase.

For example:

A better wording would be:

Of course, it's easy to suggest improvements with
hindsight. Nonetheless, for tools that make an explicit effort to
provide helpful errors (e.g. the Rust compiler), I never saw this
particular issue.

## Longest diagnosis time

Whilst I didn't record debugging time in general, I did notice that
language implementation bugs took conspicuously more time than any
others.

The single bug with the longest diagnosis time was a nasty
optimisation [bug in my BF compiler](). This bug only manifested in
one of my largest integration tests, with full optimisations, on
certain versions of LLVM.

After examining the data, stepping through with a debugger and failed
attempts to reduce the failing example, I had to find a smarter
approach. I ended up writing a BF interpreter, running it in lockstep
with my compiler, and seeing where the interpreted behaviour and the
compiled behaviour diverged.

(The root cause was an out-of-bounds array access due to a faulty
assumption in one of my optimisation passes -- see the commit message
for more details.)

The primary problem with language bugs is that you can't use many of
your usual debugging tools. If your compiler is producing broken
output (e.g. [Rust]()) or your interpreter is doing the wrong thing
(e.g. [Emacs]()) then a source-level debugger won't help you. Print
debugging or re-reading the code won't help either.

Fortunately, these bugs were few and far between.

## Avoidable bugs

The whole purpose of this exercise was to find ways of avoiding bugs
entirely, or at least diagnosing them more quickly.

Some bug categories are entirely fixable.

Other bugs would go away with practice.

Badly designed tools/APIs can be replaced.
