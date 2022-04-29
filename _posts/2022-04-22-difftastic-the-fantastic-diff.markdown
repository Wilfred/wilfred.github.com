--- 
layout: post
title: "Difftastic: The Fantastic Diff"
---

Diff tools are dumb. They have no understanding of the syntax they're
comparing. I set out to write a syntactic diff tool, which I called
[difftastic](https://github.com/wilfred/difftastic).

It proved to be the most fascinating and most frustrating program that
I've ever written.

## The Lisp Perspective

Writing code in Lisp is a lot like writing code in JSON. Every program
is just a list of stuff.

There's already a tool called
[json-diff](https://github.com/andreyvit/json-diff), which is
excellent. I really wanted something similar for at least my Lisp
programs.

How hard could it be? Why doesn't this already exist?

Turns out it's really hard.

## The Challenges

I started playing with some code in Christmas 2018. After a ton of
expreriments and dead ends, I realised there were three hard problems
I needed to solve.

**Parsing.** I needed the ability to recognise the programming
language and parse it. I needed at least an accurate lexer and matched
delimiters, as well as preserving omments.

**Tree Diffing.** I tried really hard to make a lexical diff tool
work. It didn't.

**Display.** It's amazingly hard to display diff results in a
comprehensible way. This is just as complex as tree diffing.

In the rest of this post, I will talk about the high level details of
difftastic. I will then devote a separate blog post discussing each of
these challenges in more depth.

## Choosing Tools

I decided to use Rust. Cyclic graphs take a little fiddling with
lifetimes, but it turned out to be a good decision overall. 

I eventually needed a language that I could optimise heavily. More
on that later.

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
