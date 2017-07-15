--- 
layout: post
title: "These Weeks in Remacs II"
tags:
 - emacs
---

It's been six months since the last Remacs update, and many new
features have landed!

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
[type definitions](https://github.com/Wilfred/remacs/pull/202/commits/24248b43295f47f32fe6ba0c74cc60c9c18747f9),
[functions](https://github.com/Wilfred/remacs/pull/213/)

Buffers:
[type definitions](https://github.com/Wilfred/remacs/pull/202/commits/c7f81453a47ae8ebfd9d7e45bb8909b73e87d886), [functions](https://github.com/Wilfred/remacs/pull/215)

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
[dropped MS-DOS support](https://github.com/Wilfred/remacs/pull/140). The
Remacs build has been fixed on 32-bit Linux and 32-bit macOS.

The codebase has been split out:

- remacs-lib (Rust equivalents of gnulib)
- remacs-sys (type definitions of Emacs types and C functions)
- remacs-macros (procedural macros supporting elisp primitive
  functions in Rust)
- src (Rust implementation code of elisp)

Signal name mapping is [pure Rust code](https://github.com/Wilfred/remacs/pull/165).

We now run [rustfmt on every PR](https://github.com/Wilfred/remacs/pull/151).

If you fancy building Remacs without installing a dev toolchain
(compilers, C libraries etc), there's now
a [docker-compose.yml](https://github.com/Wilfred/remacs/pull/205) to
make your life easy.

## Macros

It wouldn't be a proper lisp project without some macro magic.

After several PRs and discussions, Remacs now includes
a [procedural macro](https://github.com/Wilfred/remacs/pull/192) to
simplify defining elisp functions in Rust.

For example, here's `vectorp`:

{% highlight rust %}
/// Return t if OBJECT is a vector.
#[lisp_fn]
fn vectorp(object: LispObject) -> LispObject {
    LispObject::from_bool(object.is_vector())
}
{% endhighlight %}

## Leveraging Rust

Remacs now
[uses Rust crates for SHA-1 and SHA-2](https://github.com/Wilfred/remacs/pull/162/files),
and for
[base64 encoding](https://github.com/Wilfred/remacs/pull/136/files). We've
even
[replaced some C bit-counting functions](https://github.com/Wilfred/remacs/pull/195)
with functions from the Rust stdlib.

Rust has also enabled us to
[mark Emacs functions with the ! type](https://github.com/Wilfred/remacs/pull/182),
a neat Rust feature that marks functions as not returning.

The [#[repr(transparent)] Rust RFC](https://github.com/rust-lang/rfcs/pull/1758)
has been approved, so we're looking forward to using that in Remacs. In the 
meantime, Remacs has a `LispObject` type for use in Rust, and a `CLisp_Object` 
type for FFI compatibility.

Remacs also takes advantage of the
[user-defined allocators RFC](https://github.com/rust-lang/rfcs/pull/1398),
which has been approved too. We're now [up-to-date with the new API](https://github.com/Wilfred/remacs/pull/221).

## Phew!

There's still lots to do on Remacs: many small elisp functions to port, or
larger projects to sink your teeth into. We also welcome incomplete
pull requests: many of the PRs shown here have been built on top of
initial implementations written by other contributors.

Join the fun at: [https://github.com/Wilfred/remacs](https://github.com/Wilfred/remacs)
