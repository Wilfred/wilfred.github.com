--- 
layout: post
title: "These Weeks in Remacs II"
tags:
 - emacs
---

Lots has happened since the last Remacs update!

## Community

We now have
a [Gitter chat room](https://gitter.im/remacs-discuss/Lobby)! Do drop
by if you have any questions or wish to discuss Remacs. There's a
low traffic [Remacs Subreddit](https://www.reddit.com/r/remacs/) too.

We've added @jeandudey and @birkenfeld to the GitHub collaborators,
bringing us to five fine people who can approve your pull requests.

## Elisp Features

We're still tracking upstream GNU Emacs master, so new features there
are landing in Remacs
([1](https://github.com/Wilfred/remacs/pull/158),
[2](https://github.com/Wilfred/remacs/pull/227)).

We've added a lot new elisp primitive functions:

Strings: [characterp](https://github.com/Wilfred/remacs/pull/170),
multibyte conversions
([1](https://github.com/Wilfred/remacs/pull/210),
[2](https://github.com/Wilfred/remacs/pull/211),
[3](https://github.com/Wilfred/remacs/pull/218)), and [comparisons](https://github.com/Wilfred/remacs/pull/217)

Vectors:
[basic support](https://github.com/Wilfred/remacs/pull/202/commits/24248b43295f47f32fe6ba0c74cc60c9c18747f9),
[functions](https://github.com/Wilfred/remacs/pull/213/)

Buffers:
[basic support](https://github.com/Wilfred/remacs/pull/202/commits/c7f81453a47ae8ebfd9d7e45bb8909b73e87d886), [functions](https://github.com/Wilfred/remacs/pull/215)

Symbols: [various functions](https://github.com/Wilfred/remacs/pull/224)

A much requested feature, adding Rust support to `find-function`, [has
been added](https://github.com/Wilfred/remacs/pull/203). This was an
unusual PR as it includes some elisp changes in Remacs.

We now have
[documentation on our compatibility with GNU Emacs](https://github.com/Wilfred/remacs/blob/master/REMACS_COMPATIBILITY.md). This
covers all known implementation differences, platform support
differences, and describes how to detect Remacs in elisp code.

## Cleanup

Platforms: We've
[dropped MS-DOS support](https://github.com/Wilfred/remacs/pull/140). Remacs
now runs on 32-bit Linux and OS X.

The codebase has been split out:

- remacs-lib (Rust equivalents of gnulib)
- remacs-sys (type definitions of Emacs types and C functions)
- remacs-macros (procedural macros supporting elisp primitive
  functions in Rust)
- src (Rust implementation code of elisp)

We now
run [rustfmt on every PR](https://github.com/Wilfred/remacs/pull/151).

Signal name mapping is pure-Rust:
https://github.com/Wilfred/remacs/pull/165/files

If you fancy building Remacs without installing a dev toolchain
(compilers, C libraries etc), there's now
a [docker-compose.yml](https://github.com/Wilfred/remacs/pull/205) to
make your life easy.

## Macros

It wouldn't be a proper lisp project without some macro magic. 

https://github.com/Wilfred/remacs/pull/192 follows various PRs and
discussion about how to represent elisp primitive functions


## Leveraging Rust

Using Rust crates for SHA-1 and SHA-2:
https://github.com/Wilfred/remacs/pull/162/files

base64 encoding: https://github.com/Wilfred/remacs/pull/136/files

Counting bits was C code, now just the rust stdlib with a wrapper: https://github.com/Wilfred/remacs/pull/195

Upstream transparent RFC has approved:
https://github.com/rust-lang/rfcs/pull/1758

Upstream alloc work has been approved:
link
https://github.com/rust-lang/rfcs/pull/1398
and we've updated:
https://github.com/Wilfred/remacs/pull/221

xsignal is marked as never returning: https://github.com/Wilfred/remacs/pull/182/files

## Closing Thoughts

There's still lots to do on Remacs: many small elisp functions to port, or
larger projects to sink your teeth into. We also welcome incomplete
pull requests: many of the PRs shown here have been built on top of
initial implementations written by other contributors.

Join the fun at: https://github.com/Wilfred/remacs
