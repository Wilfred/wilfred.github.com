--- 
layout: post
title: "The Different Flavours of Syntax Highlighting"
---

What do you expect your editor to highlight? What are the different
ways that we can highlight code? There are some common solutions, but
there are a variety of options.

The limitation of highlighting tools is that you can't use all of them
at the same time. We'll explore what's available to help you choose.

I'm taking these examples from Emacs, but many of these are available
on other editors too.

## Lexical Highlighting

<img src="/assets/lexical_fibs_js.png" class="screenshot">

A programmer typically expects syntax highlighting to look like
this. Different syntactic categories -- function names, keywords,
comments and so on -- are each shown in a different colour. Virtually
all editors provide this, with the notable exception of
[Acme](https://en.wikipedia.org/wiki/Acme_%28text_editor%29).

This is already useful. Syntactic mistakes, such as typos in keywords or
unclosed string or comments, become obvious. The screenshot above is
the default colour scheme in Emacs. It's interesting to note that
Emacs does not choose a washed out grey for comments, preferring to
[make comments prominent](https://medium.com/@MrJamesFisher/your-syntax-highlighter-is-wrong-6f83add748c9).

Note that these code samples aren't particularly idiomatic or elegant, I've simply
chosen them to show off relevant parts of the syntax.

## Extended Lexical Highlighting

Numbers

Quoted values.

## Semantic Highlighting

Some modes include a full parser rather than just a lexer. This
enables more sophisticated highlighting tricks.

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

There are a number of improvements.

<img src="/assets/highlight_sexp.png" class="screenshot">

Current s-expression.

<img src="/assets/highlight_nested_parens.png" class="screenshot">

Nested s-expression, heat from the cursor ('point' in Emacs
terminology).

## Standard Library Highlighting

Xah Lee's JS mode.

## Docstring Highlighting

Elisp / clojure as grey, highlighting inline symbols

javascript doxygen syntax

## Symbol Highlighting

symbol at point

<img src="/assets/highlight_current_symbol.png" class="screenshot">

This is highlight-symbol-mode. Change the timeout.

## Uniqueness Highlighting

Symbol based (called 'semantic highlighting' in kdevelop)

https://github.com/ankurdave/color-identifiers-mode

paren based.

<img src="/assets/rainbow_delimeters.png" class="screenshot">

Rainbow delimeters uses a unique colour for depth. Need lots of
colours before cycling. The default colours are too subtle.
