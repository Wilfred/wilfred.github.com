--- 
layout: post
title: "Rustdoc Meets The Self-Documenting Editor"
tags:
 - emacs
---

Emacs Lisp has a delightful help system. You can view the docstring
for any function under the cursor, making it easy to learn
functionality.

Rust goes a step further. All the standard library documentation is
written with the source code. This means we can find docs
programmatically!

When I learnt that
[racer recently added support for rustdoc](https://github.com/phildawes/racer/pull/535),
I couldn't resist adding support to racer.el.

<img src="/assets/racer_help.png">

The new `racer-describe` command actually **renders the markdown** in
rustdoc comments. Since we're showing a separate buffer, we can render
the docs and throw away the markdown syntax. We can even convert
external hyperlinks to clickable links!

This is a really nice example of composing Emacs functionality. Since
we can easily highlight code snippets (it's an editor!), we actually
apply syntax highlighting to inline code! Note how `Vec` and `T` are
highlighted as types in the above screenshot.

Whilst we don't use `*Help*` buffers, we extend the same keymaps, so
all the relevant help shortcuts just work too.

We have hit a few teething issues in racer (namely
[#594](https://github.com/phildawes/racer/issues/594) and
[#597](https://github.com/phildawes/racer/issues/597)) but it's
changed the way I explore Rust APIs. It's particularly useful for
learning functionality via examples, without worrying about
implementation:

<img src="/assets/racer_help_refcell.png">

I hope it will also encourage users to write great docstrings for
their own projects.

Love it? Hate it? Let me know what you think in the
[/r/rust discussion](#todo).

(It's hot off the press, so there will be bugs. If you find one, please
[file it on GitHub](https://github.com/racer-rust/emacs-racer).)
