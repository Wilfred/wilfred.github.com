--- 
layout: post
title: "Difftastic, The Fantastic Diff"
---

Writing a diff is one of the most fascinating and frustrating programs
I've ever written.

## The Concept

Diffing lisp should be trivial. A few other lispers have had a go at
it. Autochrome is by far the best today.

Some academics have explore this space too. They've taken a different
approach: focusing on performance and 'optimality'.

Turns out that being optimal isn't sufficient. More on that later.

## Hard Problems

* Parsing the code
* Calculating changes
* Displaying the changes

## Choosing Tools

I decided to use Rust. I eventually needed a language that I could
optimise heavily, but more on that later.

## Parsing

Wrote some lexers with minimal parsers based on Comby. Writing a
correct lexer alone is a ton of work. tree-sitter was great.

tree-sitter paresrs are imprecise, but they're brilliant. There's a
ton of languages available, and someone else (often a neovim
enthsusiast or GH employee) has done the hard work already.

## Diffing Ideas

Let's talk about things that don't work.

Lexical diffing: doesn't guarantee that your code is well balanced.

Top-down diffing.

## Diffing Performance

Autochrome is awesome but it's slow. I'm using Dijkstra too.

Difftastic is still fairly slow. Scales poorly. Added limits to avoid
it overwhelming the system.

Tons of micro optimisations, shaving bytes off of hashmap. Wasn't
clear I'd ever make something sufficiently fast.

Unchanged pass beforehand was very powerful. GNU diff does something
similar with leading/trailing lines. Content hashing was important
too.

Tried several other things (e.g. A* search) to no avail. `perf stat`
was very important.

## Resolving Ambiguous Diffs

Sliders are valid solutions where humans have strong intuitions. Done
as diffing afterwards, but path-dependent behaviour is expensive.

## Weaknesses of Syntactic Diffs

sometimes you care about whitespace. Accidental reformatting. Large
multiline strings. Incomprehensible diffs.

## Display

Side-by-side is more popular and easier to reason about.

24-bit colours is desirable but less portable. Invites more
customisation.

Very hard to reason about in general. Eventually realised that a diff
is about what hasn't change, not what's changed.

Hard to align blank lines, as they have no information.

## Release

Concerned about everyone using it. Eventually hit HN. Eventually
provided binaries.

Super positive reception. Many people get it. Not easy to hack on.
