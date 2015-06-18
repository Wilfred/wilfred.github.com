--- 
layout: post
title: "The Unreasonable Effectiveness of Dumb Text Search"
---

[ag](http://geoff.greer.fm/ag/) has permanently
changed the way I write code. It's a blisteringly fast grep
alternative, using
[sophisticated algorithms](https://en.wikipedia.org/wiki/Boyer%E2%80%93Moore_string_search_algorithm)
and sensible heuristics to help you find things quickly.

<img src="/assets/ag_screenshot.png" alt="screenshot of ag">

So, it has pretty output by default and it's quick. Grep's been around
a long time. What's the big deal?

Dumb text search finds **everything**. Relevant documentation. Duplicated
code. Commented-out hacks in dusty corners.

Dumb text search is also superb for when you're working with multiple
languages. Want to find all the HTML and CSS that has a `user-info`
class? No problem.

There's still a place, of course, for smart code analysis. Good
editors index your source code and let you find the definition or all
callers. It's just that you only find what you were looking or.

Sometimes I even find myself doing a text search for `def foo_bar` rather
than using jump-to-definition. When you have a fast, generic tool
that's at your finger tips, why not? There'll probably never be a
syntax-aware search tool for that documentation format you're using
anyway.

<img src="/assets/ag_in_emacs.png" alt="screenshot of ag in Emacs">

To make the most of `ag`, you need editor integration. I wrote
[emacs integration](https://github.com/Wilfred/ag.el) in January 2013,
making search easy from within Emacs.

Note that the above screenshot shows ag.el offering the current
variable name as the search term. In just two keystrokes you can
search for the current thing you're looking at, and it's amazing.

Powerful, effective, generic tools are worth your time. It's the very
best programming has to offer.
