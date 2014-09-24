--- 
layout: post
title: "The Definitive Guide To Syntax Highlighting"
---

What do you expect your editor to highlight? What are the different
ways that we can highlight code without calling external tools? There
are some common solutions, but there are a variety of options.

The limitation of highlighting tools is that you can't use all of them
at the same time. We'll explore what's available to help you choose.

I'm taking these examples from Emacs, but many of these are available
on other editors too. We'll limit ourselves to programming language
highlighting that the editor itself can do, ignoring lint tools and
VCS integrations.

## Lexical Highlighting

<img src="/assets/lexical_fibs_js.png" class="screenshot">

A programmer typically expects syntax highlighting to look like
this. Different lexical categories -- function names, keywords,
comments and so on -- are each shown in a different colour. Virtually
all editors provide this, with the notable exception of
[Acme](https://en.wikipedia.org/wiki/Acme_%28text_editor%29). In
Emacs, this is largely `font-lock-syntax-table`, though
`font-lock-keywords` is usually used too.

Simple lexical highlighting is already useful. Syntactic mistakes,
such as typos in keywords or unclosed string or comments, become
obvious. The screenshot above is the default colour scheme in
Emacs. It's interesting to note that Emacs does not choose a washed
out grey for comments, preferring to
[make comments prominent](https://medium.com/@MrJamesFisher/your-syntax-highlighter-is-wrong-6f83add748c9).

Note that these code samples aren't particularly idiomatic or elegant, I've simply
chosen them to show off relevant parts of the syntax.

## Extended Lexical Highlighting

Depending on your taste for 'angry fruit salad' highlighting, you
might choose to distinguish more lexical classes. Emacs has
`font-lock-maximum-decoration` to adjust how many distinct things are
highlighted, but it's rarely used in practice.

There are variety of minor modes that offer additional highlighting of
specific syntactic features. What's great about these minor-modes is
that they compose nicely, allowing them to be reused for highlighting
many different languages.

<img src="/assets/highlight_numbers.png" class="screenshot">

This is
[highlight-numbers](https://github.com/Fanael/highlight-numbers). It's
a simple, non-intrusive extension to highlighting that makes sense in
pretty much every language.

enh-ruby highlighting = and >>

<img src="/assets/highlight_quotes.png" class="screenshot">

Fun fact: Vim has better highlighting of Emacs lisp than Emacs itself!
[Here's a screenshot](http://i.imgur.com/WtmoEbz.png). One great
feature of Vim's stock highlighting is highlighting quoted
values. This is available with
[highlight-quoted](https://github.com/Fanael/highlight-quoted).

<img src="/assets/highlight_quotes.png" class="screenshot">

[highlight-escape-sequences](https://github.com/dgutov/highlight-escape-sequences)
is another minor mode that does exactly what its name suggests. It
only supports Ruby and JavaScript currently.

All these minor modes are matters of preference. If a major mode
developer likes this extended highlighting, they tend to include it in
their major mode anyway.

## Semantic Highlighting

Some modes include a full parser rather than just a lexer. This
enables more sophisticated highlighting techniques.

<img src="/assets/semantic_fibs_js.png" class="screenshot">

In this example, [js2-mode](https://github.com/mooz/js2-mode) has
highlighted `n` differently as it can distinguish between parameters
and variable declarations. More impressively, it has highlighted
`console` because it can see that `console` is not defined within this
snippet of code.

Highlighting globals is useful because use of globals is not an error,
but it's useful information about the code. js2-mode can also be
configured to highlight globals specific to your current project or JS
platform (see `js2-additional-externs`).

## S-expression Highlighting

Emacs also offers a number of specialist highlighting modes for s-expressions.

<img src="/assets/paren_face.png" class="screenshot">

[paren-face](https://github.com/tarsius/paren-face) is a simple minor
mode that assigns an additional face to parentheses, enabling you to
style parens separately. It's intended to fade out the parens, so you
can focus on the rest of your code.

<img src="/assets/rainbow_delimeters.png" class="screenshot">

[rainbow-delimeters](https://github.com/jlr/rainbow-delimiters) takes
the opposite approach. Each level of parens is assigned a unique face,
enabling you give each one a different colour. This works particularly
well when using `cond`, as it's easy to spot the different boolean
expressions.

By default it allows nine levels of nesting before cycling colours
(see `rainbow-delimeters-max-face-count`) but you will have to choose
a tradeoff between more levels and contrast between the colours of the
different levels. I settled for six levels that are very distinct (the
defaults are rather subtle).

If you like this mode,
[rainbow-blocks](https://github.com/istib/rainbow-blocks) colours all
the symbols the same colours as the parens.

highlight-stages.

## Standard Library Highlighting

Another school of thought is that you should highlight all functions
from the language standard or standard library. Xah Lee subscribes to
this philosophy, and has released
[JS](http://ergoemacs.org/emacs/xah-js-mode.html) and
[elisp](http://ergoemacs.org/emacs/xah-elisp-mode.html) modes that
provide this.

<img src="/assets/highlight_elisp_builtins.png" class="screenshot">

This is difficult to do in elisp as it's a lisp-2, and this mode can
confuse variables and functions slots (so `list` is highlighted even
when used as a variable).

<img src="/assets/python_builtins.png" class="screenshot">

The default Python mode in Emacs takes a similar approach,
highlighting the 80 built-in functions and some methods on built-in
types. This is hard to do in general, and `python-mode` will
incorrectly highlight similarly-named methods on other types or
methods whose name match built-in functions.

## Docstring Highlighting

<img src="/assets/elisp_docstring.png" class="screenshot">

Note that we have separate highlighting for comments, strings and
docstrings (`font-lock-comment-face`, `font-lock-string-face` and
`font-lock-doc-face` respectively).

Elisp / clojure as grey, highlighting inline symbols

javascript doxygen syntax

## Contextual Highlighting

<img src="/assets/highlight_sexp.png" class="screenshot">

Current s-expression.

<img src="/assets/highlight_nested_parens.png" class="screenshot">

Nested s-expression, heat from the cursor ('point' in Emacs
terminology).

<img src="/assets/highlight_current_symbol.png" class="screenshot">

This is highlight-symbol-mode. Change the timeout.

## Explicit Highlighting

## Substitutions

pretty symbols

glasses mode

## Hashed Highlighting

Symbol based (called 'semantic highlighting' in kdevelop)

https://github.com/ankurdave/color-identifiers-mode

## Defined Highlighting

highlight-defined
