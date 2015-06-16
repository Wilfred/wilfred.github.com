--- 
layout: post
title: "Exploring Rust"
---

Rust recently released 1.0, so it's a perfect time to see what the
language has to offer.

To get to know the language, I wrote a
[multi-threaded web crawler](https://github.com/Wilfred/linkdoc) that
finds any broken links on a website. In this blog post, I explore the
Rust programming experience.

## The dependency tooling

The first striking feature of Rust is Cargo. Cargo is a package
manager for Rust, and also generates project skeletons to get you started.

Cargo works amazingly well. It's a stark contrast to programming in C
and C++, providing canonical answers to questions like:

* How do I depend on external libraries?
* How do I upgrade those dependencies when new versions are released?
* How do I discover libraries that could help me with my
current project?
* How do I work on two projects which depend on different versions of
the same library?
* How do I distinguish between direct dependencies I need and indirect
dependencies that I happen to use?

If Rust had no other interesting features, the usefulness of Cargo
makes Rust very attractive. The closest alternative is
[biicode](https://www.biicode.com/), a C/C++ dependencies manager that
hasn't gained much traction. By contrast,
the [Rust package repository](https://crates.io/) is now
[growing faster than Haskell's hackage](http://www.modulecounts.com/)!

## The documentation

The Rust docs are excellent. They're cross-referenced, searchable and
include direct links to the relevant source code. I'm left wondering
why more languages aren't like this.

Generating docs from source code is hard to get right. I've worked
with documentation tools generate elaborate diagrams from inheritance,
without asking whether it's the best way to present your library to a
user. At the other extreme, some doc tools require you to include
elaborately formatted comments that distract from the code
itself. Rustdoc strikes a good balance, using a lightweight markdown
syntax.

Virtually every feature in the standard library includes an example in
the docs, which is delightful. With one click, you can even compile
and run the example in the Rust playground!

## The community

The Rust community goes to great lengths to be welcoming and
inclusive. The code of conduct is applied everywhere, and it
shows. For example,
[here's a user being asked not to make unconstructive criticisms of Go](http://article.gmane.org/gmane.comp.lang.rust.devel/4767/). This
creates a community that I want to be part of.

I've also been struck as to how active the community is, especially
for a young language. I made a
[suggestion for improving a compiler error message](https://github.com/rust-lang/rust/issues/25468)
and a pull request was merged in under a day! There's also ample help
available with knowledgeable people on IRC and Stack Overflow who are
very amenable.

## The learning curve

There's a lot to learn with any new language, but particularly so with
Rust. Developers without systems programming experience will have to
understand the distinction between the stack and the heap. There's
also the trait system, which is different enough to traditional OO
that you'll need a little thought.

Rust's biggest conceptual hurdle is the ownership system. Fortunately,
the [official Rust book](https://doc.rust-lang.org/stable/book/) is
extremely approachable and broken up into small, digestible chapters.

## Rust's readiness

Rust has a reasonable range of libraries and I had no problem finding
what I needed. However, several of those libraries required me to use
a nightly release of Rust rather than v1.0. This is a pain because at
any point in time an unstable API can change and suddenly you have to
find a combination of nightly release and library commit that actually
compiles.

Rust compile times aren't great either (though
[very competitive with C++](https://ruudvanasseldonk.com/2014/10/20/writing-a-path-tracer-in-rust-part-7-conclusion#performance)). This
is an active area of development and it should improve. In the
meantime, [rest_easy](https://github.com/cmr/rest_easy) will notify
you as soon as the compiler is happy with your code, so you don't need
to wait for code generation.

## The payoff

Rust programs are fast. The abstractions are pleasant to work with,
but don't cost performance.

The resulting programs tend to be very robust, much like working with
Haskell.

Rust is way past critical mass. The future looks extremely bright.
