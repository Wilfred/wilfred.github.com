--- 
layout: post
title: "Difftastic, the Fantastic Diff"
---

I've always wanted a structural diff tool, so I built
[difftastic](https://github.com/wilfred/difftastic).

This has been the most fascinating, most frustrating, and most
challenging program I've ever written.

## How Hard Could It Be?

If you write Lisp code for a while, you start to see code like
JSON. Everything is basically a list.

[json-diff](https://github.com/andreyvit/json-diff) already exists,
and it's pretty darn good. I wanted something similar for programming
languages.

After an absolute ton of experimentation, I have something that
works. In this post, I'll show you how it works.

I won't show the many, many dead ends and failed designs along the
way. We can pretend I got it right first time.

## Parsing The Code

If I want to compare two programs, I need a parse tree for each
program. I need an accurate lexer, a basic parser, and I need to
preserve comments.

[tree-sitter](https://tree-sitter.github.io/tree-sitter/) was a great
fit here. You define a grammar in JSON or JS, and it generates a C
library that anyone can use.

```
list: ($) => seq("(", repeat($._sexp), ")"),
vector: ($) => seq("[", repeat($._sexp), "]"),
```

Here's an excerpt from my [Emacs Lisp
grammar](https://github.com/Wilfred/tree-sitter-elisp). There's a ton
of tree-sitter parsers available too. Difftastic supports 44 different
syntaxes, and adding new ones is straightforward.

After parsing, difftastic converts the tree-sitter parse tree to an
s-expression. Everything is a list or an atom. This uniform
representation enables the diffing logic to work on any language I can
parse.

For example, given a JavaScript program like this:

```
foo(1, 2)
```

tree-sitter parses it to this parse tree:

```
expression_statement
  call_expression
    identifier "foo"
    arguments
      (
      number "1"
      ,
      number "2"
      )
```

difftastic then converts the tree to this s-expression representation:

```
List {
    open_content: "",
    children: [
        Atom "foo",
        List {
            open_content: "(",
            children: [
                Atom "1",
                Atom ",",
                Atom "2",
            ],
            close_content: ")",
        },
    ],
    close_content: "",
}
```

## Calculating The Diff

Fun fact: I think of diffing programs as working out what has
changed. **The goal of diffing is actually to work out what hasn't
changed!** The more stuff you can match up, the smaller and more
readable your diff.

How do I work out what's changed between two parse trees? There are a
bunch of different design ideas out there, but
[autochrome](https://fazzone.github.io/autochrome.html) is the most
effective approach I found, and it includes an incredibly helpful
worked example.

Autochrome and difftastic represent diffing as a **shortest path
problem** on a directed acyclic graph. A vertex represents a pair of
positions: the position in the left-hand side s-expression (before), and the position in
the right-hand side s-expression (after).

The goal is to find the shortest route from the start (where both
positions are before the first item in the programs) to the end (where
both positions are after the last item in the program).

For example, suppose you're comparing the program `A` with `X A`. I
use `^` to mark the position on each side.

```
START
+---------------------+
| Left: A  Right: X A |
|      ^         ^    |
+---------------------+

END
+---------------------+
| Left: A  Right: X A |
|        ^           ^|
+---------------------+
```

The edges in the graph are the diffing decisions available. 

Difftastic could consider the left-hand side to be novel and increment
that position, or it could consider the right-hand side to be novel
and increment that position.

A novel item on the left-hand side is a <span style="color: red">removal</span>, and a novel item on
the right-hand side is an <span style="color: green">addition</span>.

```
            START
            +---------------------+
            | Left: A  Right: X A |
            |      ^         ^    |
            +---------------------+
                   /       \
     Novel atom L /         \ Novel atom R
1                v       2   v
+---------------------+  +---------------------+
| Left: A  Right: X A |  | Left: A  Right: X A |
|        ^       ^    |  |      ^           ^  |
+---------------------+  +---------------------+
```

When both positions are pointing to an identical s-expression, I can
use an unchanged edge in the graph. I give unchanged edges a lower
cost than novel edge. The routing problem is then a matter of finding
the route with the most unchanged edges.

```
            2
            +---------------------+
            | Left: A  Right: X A |
            |      ^           ^  |
            +---------------------+
                   /    |   \
     Novel atom L /     |    \ Novel atom R
3                v      | 4   v
+---------------------+ | +---------------------+
| Left: A  Right: X A | | | Left: A  Right: X A |
|        ^         ^  | | |      ^             ^|
+---------------------+ | +---------------------+
  |                     |                    |
  | Novel atom R        | Nodes match        | Novel atom L
  |                     |                    |
  |         END         v                    |
  |         +---------------------+          |
  +-------->| Left: A  Right: X A |<---------+
            |        ^           ^|
            +---------------------+
```

The shortest route with programs `A` and `X A` is `[Novel Right,
Unchanged]`.

## Finding The Shortest Route

Difftastic and Autochrome both use Dijkstra's algorithm. Unfortunately
the size of the graph is quadratic: it's proportional to the number of
items in the left-hand side s-expression times the number of nodes on
the right-hand side expression.




## What About Nesting?

## But It's Slow!

## Dogfooding

## Adding An Interface
