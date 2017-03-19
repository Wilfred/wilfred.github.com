--- 
layout: post
title: "The Dirty Secret of Emacs Code Completion"
---

A little static analysis is all you need for a respectable code
completion feature. IDE users, quite reasonably, expect this.

PICTURE

These same users are often surprised that **Emacs doesn't have this by
default**. They're further surprised that **many Emacsers don't
configure a static analysis tool for completion**. Why is this?

Emacs is an editor: it defaults to dirty text-based tricks. Working
with text is easy, so these tricks have a headstart in the plugin arms
race.

When you teach Emacs about a new programming language, you define a
syntax table. This enables Emacs to highlight strings and
comments, but also lets Emacs see symbol boundaries. `x-y` is `x - y`
in JS, but it's a single value `x-y` in CSS.

You can then use a command `dabbrev-expand` ('dynamic abbreviations')
to find all variable names (including method names) that start with
the current prefix.

GIF

`dabbrev-expand` will try /desperately hard/ to make you happy. It
will offer up:

* symbols before the cursor
* symbols after the cursor
* symbols in other open files in the same programming language
* symbols in any open file
* symbols in the clipboard

This works astoundingly well in practice. If dabbrev is finding too
many irrelevant results, just type a little more of the prefix. By
cycling through results, we don't have to populate a dropdown list.

There are other perks, too:

Is your file unfinished and syntactically invalid? dabbrev doesn't
mind.

Are you writing a function with similarly named internal variables?
dabbrev saw them coming.

Are you working on 
