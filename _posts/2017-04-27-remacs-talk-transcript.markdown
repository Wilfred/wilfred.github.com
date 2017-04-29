--- 
layout: post
title: "Remacs Talk Transcript"
---

*This is a transcript of my Remacs talk at the last Emacs meetup. This
is based on my notes, so it may not match exactly what I said on the
night.*

## Intro
I'm excited to be here to talk about Remacs!

Remacs is a project to build a Rust version of Emacs. Today, I'm going
to talk about what Remacs is, what it isn't, and our goals.

I will use the terms **Remacs** or **GNU Emacs** when I need to be
specific, and use the term **Emacs** as a generic term. Please shout
if I'm unclear.

## About Me

I'm a Python programmer by day, and I write crazy Emacs things by
night. I've contributed to many elisp projects, so hopefully I've
benefited your Emacs experience in some small way.

I've committed patches to dash.el, counsel, magit and I even have
commit rights to GNU Emacs. I believe in Emacs as the most powerful
practical programmer tool available today.

## The Structure of Emacs

Emacs is really just a lisp interpreter called `temacs`. The Emacs
functionality that we know and love is really the standard library
that ships with `temacs`. The `temacs` binary itself is basically just
I/O and GUI glue code on top a bytecode interpreter.

## Zooming In

Let's take a closer look at Emacs' code structure.

```
~/src/emacs $ loc
--------------------------------------------------------------------------------
 Language             Files        Lines        Blank      Comment         Code
--------------------------------------------------------------------------------
 Lisp                  1700      1619767       172079       219650      1228038
 C                      282       404244        57153        69636       277455
 Plain Text              31       238671         4587            0       234084
 C/C++ Header           221        85153        10800        17448        56905
 TeX                     28        28245         3983         6425        17837
 Objective-C             11        23744         3662         2377        17705
 Makefile                59        21294         3302         4577        13415
 Autoconf                14         8521         1641         1612         5268
```

Emacs is largely written in Emacs lisp. That's great, because elisp is
really expressive and any user can extend it. In fact, the core Emacs
team is pushing more functionality into elisp over time. For example,
undo functionality moved from C to elisp in 2012.

We can see that there's a lot more elisp than C in Emacs. Since elisp
requires fewer lines of code per feature, the vast majority of Emacs
functionality is written in elisp.

There's still a lof of C here. There's the C code itself, the header
files, and build scripts supporting it.

This view is slightly misleading, however. C code doesn't have a
package manager, so there are C libraries in Emacs that have been
copy-pasted into the source tree. For example, the MD5 implementation.

This means that some of the C code is less maintained and not all of
the functionality at the C level is used anywhere inside Emacs.

Being Emacsy is about providing an incredible mutable programming
environment, and things like portably accessing a user's home
directory are not really what Emacs is about.

## Remacs!

So, how is Remacs different?

Remacs is an incremental port of that C code to Rust. Since it's an
incremental port, we have a working Emacs instance at every stage of
the project.

Note that Remacs is based on the master branch of GNU Emacs, so
sometimes we get compatibility bug reports due to GNU Emacs changing
something on master.

We aim to be a drop-in replacement with bug-for-bug compatibility.

We're also seeking to match or beat GNU Emacs performance. We've
discussed replacing the `unexec` support, but we've been unwilling to
move to anything that hurts performance.

I have done a few preliminary benchmarks of Remacs' primitives, and
performance is definitely no worse. It's possible that it's slightly
faster, but I would need to measure more and be very careful to do a
fair comparison.

## Non-Goals

I also want to talk about what Remacs isn't.

Remacs aims to be friendly fork. I have commit privileges on GNU
Emacs: we haven't fallen out with the core team.

On the contrary, we have hit bugs in in the test suite (#25534) and
interpreter segfaults (#25684) and we've reported them upstream. We
want to be positive contributors to the Emacs ecosystem.

Remacs is also not a radical overhaul of the Emacs design. From a
user's perspective, they might notice that Remacs is slightly faster,
or more robust (although GNU Emacs very rarely crashes). They might
also notice that the contribution workflow is different (pull requests on
GitHub).

Finally, Remacs is not planning to replace Emacs Lisp. We've discussed
possible changes to the bytecode format, but Remacs should be a
drop-in alternative implementation of emacs lisp.

## The C Codebase

GNU Emacs is a large and old C codebase. It has code to support DOS,
SunOS, and even broken malloc implementations.

Remacs will not support these platforms, and that's OK. GNU Emacs
still exists for your DOS computing needs :)

The C codebase predates `clang-format`, so the code style is not
always consistent. The GNU Emacs core team is understandably reluctant
to add `clang-format` as it makes tools like `git-blame` less
effective.

Remacs uses `rustfmt` for all contributions, because we have that
luxury.

Contributing to GNU Emacs follows a traditional workflow: you send
your patches to mailing list. There is CI
([emacs-trunk on Hydra](http://hydra.nixos.org/jobset/gnu/emacs-trunk))
but it's Linux only and after-the-fact (it runs once your changes have
been accepted). GNU Emacs' tests are written in elisp and don't have
unit tests at the C level.

To contribute to Remacs, we use GitHub with pull requests. We use
Travis for PRs, so every PR is checked for Rust compiler warnings, we
run the Rust unit tests and the whole Emacs test suite on both Linux
and OS X.

## Why Rust?


