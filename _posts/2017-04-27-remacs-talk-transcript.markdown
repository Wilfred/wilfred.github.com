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
directory are not reallly what Emacs is about.
