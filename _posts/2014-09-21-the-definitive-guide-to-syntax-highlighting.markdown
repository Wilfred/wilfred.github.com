--- 
layout: post
title: "The Definitive Guide To Syntax Highlighting"
---

What do you expect your editor to highlight? What are the different
ways that we can highlight code without calling external tools? Whilst
most editors have converged on a common set of base functionality,
there's still innovation occurring in this field.

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
Emacs, this is largely done with `font-lock-syntax-table`, though
`font-lock-keywords` is usually used too.

Simple lexical highlighting is already useful. Syntactic mistakes,
such as typos in keywords or unclosed string or comments, become
obvious.

A note on screenshots: The above image is the default colour scheme in
Emacs. In other images I've customised the styling to only show the
highlighting that's related to the feature mentioned. The code samples
aren't particularly idiomatic or elegant, I've simply chosen them to
show off relevant parts of the syntax.

It's interesting to see that default Emacs colour scheme does not
choose a washed out grey for comments, preferring to
[make comments prominent](https://medium.com/@MrJamesFisher/your-syntax-highlighter-is-wrong-6f83add748c9).

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

<img src="/assets/highlight_quotes.png" class="screenshot">

Fun fact: Vim has better highlighting of Emacs lisp than Emacs itself!
[Here's a screenshot](http://i.imgur.com/WtmoEbz.png). One great
feature of Vim's stock highlighting is highlighting quoted
values. This is available with
[highlight-quoted](https://github.com/Fanael/highlight-quoted).

<img src="/assets/highlight_escapes.png" class="screenshot">

[highlight-escape-sequences](https://github.com/dgutov/highlight-escape-sequences)
is another minor mode with a simple goal. In this case, it highlights
escape sequences inside strings. It currently only supports Ruby and JavaScript.

<img src="/assets/enh_ruby_mode.png" class="screenshot">

All these minor modes are matters of preference. If a major mode
developer likes this extended highlighting, they tend to include it in
their major mode anyway. In the above example, `enh-ruby-mode`
highlights all infix operators (in addition the standard Ruby highlighting).

## Semantic Highlighting

Some modes include a full parser rather than just a lexer. This
enables more sophisticated highlighting techniques.

<img src="/assets/js2_mode.png" class="screenshot">

[js2-mode](https://github.com/mooz/js2-mode) is the best example of
this. js2-mode includes a full-blown recursive-descent ECMAScript
parser, including a number of common extensions. This enables js2-mode
to distinguish more syntax types. For example, it can distinguish
parameters and global variables.

This is an amazing achievement and even allows the editor to do many
checks that are traditionally done by lint tools. Highlighting globals
is particularly useful because use of globals is not an error, but
it's useful information about the code. js2-mode can also be
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

<img src="/assets/highlight_stages.png" class="screenshot">

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

Docstrings are conceptually between strings and comments: they're for
the reader (like comments), but they're available at runtime (like
strings). Emacs exposes separate faces for  comments, strings and
docstrings (`font-lock-comment-face`, `font-lock-string-face` and
`font-lock-doc-face` respectively).

Docstrings may also contain additional syntax for
cross-references. Emacs will highlight these differently too (though
their main use is cross-references in `*Help*` buffers).

<img src="/assets/jsdoc_highlighting.png" class="screenshot">

Some languages support elaborate syntax in their comments, both to
help the reader and to aid automatic documentation tools. In this
example, js2-mode offers additional highlighting of JSDoc comments.

## Contextual Highlighting

Another important area of highlighting is to highlight elements based
on where the cursor ('point' in Emacs terminology) is currently
located.

<img src="/assets/highlight_matching.png" class="screenshot">

The most basic contextual highlighting is showing the matching bracket
to the bracket currently at the cursor. This is part of Emacs, but off
by default: `show-paren-mode` will switch it on.

<img src="/assets/hl_line.png" class="screenshot">

Highlighting the current line is a very common feature of editor
highlighting, and Emacs provides `hl-line-mode` for this. This works
well for line-oriented programming languages. 

<img src="/assets/highlight_sexp.png" class="screenshot">

For lisps, you can take this a step further with
[hl-sexp](https://github.com/emacsmirror/hl-sexp). This shows the
entire s-expression under point, showing where you are in the code.

<img src="/assets/highlight_nested_parens.png" class="screenshot">

[highlight-parentheses](https://github.com/nschum/highlight-parentheses.el)
takes a more subtle approach. It highlights the current paren as
'hot', and highlight outer parens in progressively 'cooler' colours.

<img src="/assets/highlight_current_symbol.png" class="screenshot">

The last example in this section is the superb
[highlight-symbol](https://github.com/nschum/highlight-symbol.el). This
is invaluable for showing you where else the current symbol is being
used. highlight-symbol is conservative and only does when the point
isn't moving, but set `highlight-symbol-idle-delay` to 0 to override
this.

`highlight-symbol-mode` is particularly clever in that it's able to
inspect the current syntax table. This prevents it from becoming
confused with strings like `x-1` which is usually a single symbol in
lisps but equivalent to `x - 1` in many other languages.

## Explicit Highlighting

<img src="/assets/hi_lock_mode.png" class="screenshot">

There comes a point where automatic highlighting isn't sufficient, and
you want to explicitly highlight something. Emacs provides
`hi-lock-mode` for this, and support a special comment syntax that
allows other readers to see the same highlighting.

## Substitutions

<img src="/assets/pretty_symbols.png" class="screenshot">

pretty symbols

<img src="/assets/glasses_mode.png" class="screenshot">

glasses mode

## Hashed Highlighting

<img src="/assets/color_identifiers.png" class="screenshot">

Symbol based (called 'semantic highlighting' in kdevelop)

https://github.com/ankurdave/color-identifiers-mode

## Defined Highlighting

<img src="/assets/highlight_defined.png" class="screenshot">

highlight-defined

## Conclusions

Composition is hard

if you highlight everything, you end up highlighting nothing

use lots of faces with inheritance.
