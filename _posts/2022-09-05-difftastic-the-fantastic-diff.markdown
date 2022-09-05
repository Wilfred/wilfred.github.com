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

