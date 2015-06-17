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
with documentation tools that generate elaborate inheritance diagrams,
without asking whether it's the best way to explain your library to a
user. At the other extreme, some doc tools require you to write
elaborately formatted comments that distract from the code
itself. Rustdoc strikes a good balance, using a lightweight markdown
syntax.

Virtually every feature in the standard library includes an example in
the docs, which is delightful. With one click, you can immediately run
the example in the [Rust playground](https://play.rust-lang.org/)!

## The community

The Rust community goes to great lengths to be welcoming and
inclusive. The code of conduct is applied everywhere, and it
shows. For example,
[here's a user being asked not to make unconstructive criticisms of Go](http://article.gmane.org/gmane.comp.lang.rust.devel/4767/). This
creates a community that I want to be part of.

I was also amazed by how active the community is, especially for such
a young language. I made a
[suggestion for improving a compiler error message](https://github.com/rust-lang/rust/issues/25468)
and a pull request was merged in under a day! There's also ample help
available with knowledgeable people on IRC and Stack Overflow who are
very amenable.

## The learning curve

There's a lot to learn with any new language, but particularly so with
Rust. Developers without systems programming experience will be
exposed to the distinction between the stack and the heap. There's
also the trait system, which provides object-oriented style
encapsulation but without traditional inheritance. It works well, but
it's a novel approach for many developers.

Rust's biggest conceptual hurdle is the ownership system. This
formalises a concept that C/C++ developers already needed to worry
about, but takes time to grok. Fortunately, the
[official Rust book](https://doc.rust-lang.org/stable/book/) is
extremely approachable and broken up into small, digestible
chapters. It's not always easy to make the compiler happy, but the
compiler tries hard to make useful suggestions. I was particularly
impressed with rustc's suggestion in this example:

    foo.rs:16:24: 16:33 error: attempted to take value of method `get_score` on type `Player`
    foo.rs:16     let score = player.get_score + 1;
                                     ^~~~~~~~~
    foo.rs:16:24: 16:33 help: maybe a `()` to call it is missing? If not, try an anonymous function


## Rust's readiness

Rust has a reasonable range of libraries and I had no problems finding
what I needed. However, several libraries required me to use a
bleeding-edge, nightly build of Rust rather than v1.0. This can be
inconvenient because nightly may change an API and you need to
downgrade your compiler until the libraries have been updated to
match.

In both cases ([1](https://github.com/Manishearth/rust-clippy),
[2](https://github.com/servo/html5ever)) these libraries depend on
compiler plugins. This is fairly rare in practice, but it would be
nice to see compiler plugins supported in stable Rust.

Rust compile times aren't great either (though
[very competitive with C++](https://ruudvanasseldonk.com/2014/10/20/writing-a-path-tracer-in-rust-part-7-conclusion#performance)). This
is an active area of development and it should improve. In the
meantime, [rest_easy](https://github.com/cmr/rest_easy) will notify
you as soon as the compiler is happy with your code, so you don't need
to wait for code generation.

## The end result

Once you've finished coding a project, what have you gained? You're
left with something remarkable: robust code (like Haskell) and great
performance (like C++). I've greatly enjoyed hacking on a Rust project
and wouldn't hesitate to recommend it.

Rust is way past critical mass. The future looks extremely bright.
