--- 
layout: post
title: "The Different Flavours of Syntax Highlighting"
---

What do you expect your editor to highlight? What are the different
ways that we can highlight code without calling external tools? There
are some common solutions, but there are a variety of options.

The limitation of highlighting tools is that you can't use all of them
at the same time. We'll explore what's available to help you choose.

I'm taking these examples from Emacs, but many of these are available
on other editors too. We'll limit ourselves to highlighting that the
editor itself can do, ignoring lint tools and VCS integrations.

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

Depending on your taste for 'angry fruit salad' highlighting, you
might choose to distinguish more lexical classes. Emacs also has
`font-lock-maximum-decoration` to adjust how many distinct things are
highlighted, but it's rarely used in practice.

What's great about these minor-modes is that they compose nicely,
allowing them to be reused for highlighting many different languages.

<img src="/assets/highlight_numbers.png" class="screenshot">

Numbers

<img src="/assets/highlight_quotes.png" class="screenshot">

Quoted values.

Escape sequences.

Whilst these modes are great, in practice some major modes will bundle
this behaviour anyway.

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

<img src="/assets/paren_face.png" class="screenshot">

Fade out.

<img src="/assets/rainbow_delimeters.png" class="screenshot">

Rainbow delimeters uses a unique colour for depth. Need lots of
colours before cycling. The default colours are too subtle.

## Standard Library Highlighting

Xah Lee's JS mode.

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
