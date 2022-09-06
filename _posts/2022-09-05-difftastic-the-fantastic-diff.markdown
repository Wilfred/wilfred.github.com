--- 
layout: post
title: "Difftastic, the Fantastic Diff"
---

I've always wanted a structural diff tool, so I built
[difftastic](https://github.com/wilfred/difftastic).

<img src="/assets/difftastic_demo.png">

This has been the most fascinating, most frustrating, and most
challenging program I've ever written.

## How Hard Could It Be?

If you write Lisp code for a while, you start to see code like
JSON. Everything is basically a list.

<figure>
    <img src="/assets/json_diff.png">
    <figcaption>json-diff example</figcaption>
</figure>

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
library that anyone can use. It's not 100% accurate (e.g. the C++
parser doesn't have preprocessor data) but it's more than good enough.

```
list: ($) => seq("(", repeat($._sexp), ")"),
vector: ($) => seq("[", repeat($._sexp), "]"),
```

Here's an excerpt from my [Emacs Lisp
grammar](https://github.com/Wilfred/tree-sitter-elisp). There's a ton
of tree-sitter parsers available too. Difftastic supports 44 different
syntaxes, and adding new ones is straightforward.

<figure>
    <img src="/assets/difftastic_elisp.png">
    <figcaption>Using difftastic with Emacs Lisp</figcaption>
</figure>

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

<figure>
    <img src="/assets/autochrome.png">
    <figcaption>Autochrome example</figcaption>
</figure>

Autochrome and difftastic represent diffing as a **shortest path
problem** on a directed acyclic graph. A vertex represents a pair of
positions: the position in the left-hand side s-expression (before), and the position in
the right-hand side s-expression (after).

The goal is to find the shortest route from the start (where both
positions are before the first item in the programs) to the end (where
both positions are after the last item in the program).

For example, suppose you're comparing the program `A` with `X A`.

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
the size of the graph is quadratic: it's `O(Left * Right)`, the number
of items in the left-hand side s-expression multiplied by the number
of nodes on the right-hand side expression.

I made performance bearable by aggressively discarded obviously
unchanged s-expressions at the beginning, middle and end of the
file. If you've only changed the last function in a file, difftastic
shouldn't consider the other functions at all.

<figure>
    <img src="/assets/difftastic_unchanged.png">
    <figcaption>Everything above the line is identical</figcaption>
</figure>

(GNU Diff has a similar feature with [its horizon-lines
option](https://www.gnu.org/software/diffutils/manual/html_node/diff-Performance.html),
although its performance is much better in general.)

I also generate the graph lazily. The primary performance bottleneck
is vertex construction, so smarter algorithms like A* didn't offer a
speedup.

Even with this, difftastic sometimes struggles with performance. I've
profiled and optimised everything I can think of (using Rust helped
here). If the graph is just too big, difftastic falls back to a
conventional line-oriented diff.

## What About Nesting?



## Building The Interface

Phew! After wrestling with diffing algorithms for some time, I thought
building the UI would be straightforward. I was wrong.

<figure>
    <img src="/assets/difftastic_json.png">
    <figcaption>There are almost no lines with the same text content here!</figcaption>
</figure>

Difftastic knows which s-expression nodes are unchanged. It completely
ignores whitespace.

This means there is no guarantee there are any lines in common between
the two files. It's also possible that a line in the first file might
have matches in zero, one or many lines in the second file.

The display logic iterates through all the paired lines, and tries to
align as many as possible. It uses a two-column display, so you could
reformat the entire file and it will still produce a sensible
alignment.

<figure>
    <img src="/assets/difftastic_align.png">
    <figcaption>Difftastic has aligned both format!() expressions here.</figcaption>
</figure>

Working on diff UIs has made me realise how bad the traditional diff
display is.

<figure>
    <img src="/assets/git_diff.png">
    <figcaption>Conventional git diff</figcaption>
</figure>

The header `@@ -392,7 +392,12 @@` means that the diff starts at
line 392. The user is forced to count lines to work out that the change
itself is on line 395!

## Dogfooding

Eventually I added the ability to use difftastic with git or
mercurial. This was an exceptional way to find bugs and see how well
the design works in practice.

This exposed a bunch of subtle issues with structural diffing.

```
;; Before
(foo (bar))

;; After
(foo (novel) (bar))
```

Diff algorithms are described in the literature as "finding a minimal
edit script", where the edit script is the adding/removals required to
turn the input file into the output file.

In this example, <code>(foo (<span style="background-color:PaleGreen">novel</span>)
<span style="background-color:PaleGreen">(</span>bar<span
style="background-color:PaleGreen">)</span>)</code> would be a totally
valid, minimal diff. We're adding the symbol `novel` and adding one
set of parentheses.

This isn't what the user wants though. They'd rather see <code>(foo <span style="background-color:PaleGreen">(novel)</span> (bar))</code>
even though it's equally minimal.

I solved this by adjusting the graph edge cost model to produce nicer
results. I also added a secondary pass on the diff result to choose a more
aesthetically pleasing result with the same edge cost.

(I encountered a bunch more of challenging issues, see the [Tricky
Cases chapter in the
manual](https://difftastic.wilfred.me.uk/tricky_cases.html) for more details.)


## Future Work

I use difftastic daily, but it's still not perfect. Performance
remains a challenge. It still fails in interesting ways.

pic: String Literals

pic: A little bit in common.
