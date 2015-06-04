--- 
layout: post
title: "Exploring Rust"
---

Rust recently released 1.0, and it's a perfect time to see what the
language has to offer.

I decided to implement a link checker that would crawl a website and
report any broken URLs. You can find the
[final project on GitHub](https://github.com/Wilfred/linkdoc).

## Cargo

The first striking feature of Rust is Cargo. Cargo is a package
manager for Rust, and also generates project skeletons to get you started.

Cargo works amazingly well. It's a stark contrast to programming in C
and C++, providing canonical answers to questions like:

* How do I depend on external libraries?
* How do I upgrade libraries those dependencies?
* How do I discover libraries that can help me with my
project?
* How do I work on two projects which depend on different versions of
the same library?
* How do I distinguish between direct dependencies I need and indirect
dependencies I happen to use?

If Rust had no other interesting features, the usefulness of Cargo
makes Rust very attractive for new systems projects
today. [crates.io](https://crates.io/), the online Rust package
repository, is now
[growing faster than Haskell's hackage](http://www.modulecounts.com/)!

## Rust is amazingly polished

rust play can run from any example in the docs

docs are generally good

## The community is brilliant

Merged a fix for my bug.

## Rust is still immature

Needed to use nightly

majority of stack overflow answers no longer compile

no easy to way to do timeouts is causing problems

several of my dependencies, even though they were mozilla/servo deps,
were not yet on crates.io.

## There's a huge learning curve

Writing a Unique iterator took a lot of head scratching, two IRC
questions and one SO question.

## Resulting programs are excellent

Fast, robust.

Rust is way past critical mass. The future looks extremely bright.
