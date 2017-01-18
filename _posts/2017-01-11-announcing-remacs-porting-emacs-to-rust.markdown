--- 
layout: post
title: "Announcing Remacs: Porting Emacs to Rust"
---

I am delighted to announce
[Remacs](https://github.com/Wilfred/remacs), a project to **port Emacs
to Rust**!

Emacs, at its heart, is a lisp interpreter written in C. In Remacs,
we're replacing this C code with Rust, and all the elisp you know and
love will *just work*.

If you've ever fancied contributing to core Emacs, this is a great
opportunity to learn the internals. There's tons of low hanging fruit,
we have [a list of good first bugs](https://github.com/Wilfred/remacs#help-needed) and even a
walkthrough of [writing your first elisp function using Rust](https://github.com/Wilfred/remacs#porting-elisp-primitive-functions-walkthrough).

Rust is perfect for this because we can port incrementally. If you
want to replace the entire regular expression engine, you can do
that. If you just want to replace *this* function *here*, you can do
that and the C code won't even notice. You will have a full-blown
Emacs every step of the way.

Rust is also a terrific language to work with. The compiler, the
autoformatter, the safety checks and the community all make for a
great developer experience. Emacs support is pretty good too.

**Why port to Rust?** Porting to Rust gives us lots of
opportunities. We can leverage the rapidly-growing crate ecosystem. We
can drop support legacy compilers and platforms (looking at you,
MS-DOS). We can add docstrings and unit tests to core functions that
aren't exposed to elisp. It's also a ton of fun.

Remacs is based on Emacs 25.2. We've got enough type definitions that
you can write interesting built-in functions, but the project is still
at a very early stage. Using these, we've got a few built-in elisp
functions written entirely in Rust: some arithmetic,
some type checks, and even some basic list functionality.

If you'd like to join us, there's plenty to do. You could:

* Port a small C function in lisp.h to lisp.rs.
* Port your favourite built-in elisp function to Rust.

If you're feeling ambitious, we eventually want to explore:

* Porting Remacs to the regex crate for a major performance speedup.
* Using the existing GUI bindings crates.

We have a [full list of project ideas here](https://github.com/Wilfred/remacs#help-needed).

Naturally, we also provide
[Emacs propaganda](https://github.com/Wilfred/remacs#why-emacs),
[Rust propaganda](https://github.com/Wilfred/remacs#why-rust), and a
Travis-based workflow on GitHub. What are you waiting for? If you've
ever wanted to hack on the world's most powerful editor, we'd love you
to [join us](https://github.com/Wilfred/remacs)!
