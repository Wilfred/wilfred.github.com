--- 
layout: post
title: "Difftastic: The Syntactic Diff"
---

## Parsing

Wrote some lexers with minimal parsers based on Comby. Writing a
correct lexer alone is a ton of work. tree-sitter was great.

tree-sitter parsers are imprecise, but they're brilliant. There's a
ton of languages available, and someone else (often a neovim
enthsusiast or GH employee) has done the hard work already.

## Invalid Parsing

tree-sitter is good at this

## Precise Parsing

tree-sitter is good enough

## Performant Parsing

Good enough. tree-sitter is half the speed of rustc.

Incremental parsing not useful. You need a diff in advance, which
defeats the object for my purposes.

Can even run in parallel.

## Available Parsing

Lots of languages supported.

PLs get the most attention. SQL is limited, markdown is unmaintained,
no XML.

## Concrete Syntax Trees

Need comments and positions.
