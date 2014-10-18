--- 
layout: post
title: "The Definitive Guide To Syntax Highlighting"
tags:
 - emacs
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

<figure>
    <img src="/assets/lexical_fibs_js.png">
    <figcaption>js-mode in the standard Emacs theme</figcaption>
</figure>

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

<figure>
    <img src="/assets/highlight_numbers.png">
    <figcaption>highlight-numbers</figcaption>
</figure>

This is
[highlight-numbers](https://github.com/Fanael/highlight-numbers). It's
a simple, non-intrusive extension to highlighting that makes sense in
pretty much every language.

<figure>
    <img src="/assets/highlight_quotes.png">
    <figcaption>highlight-quoted</figcaption>
</figure>

Fun fact: Vim has a Common Lisp highlighting mode that highlights more
syntax classes that Emacs does!
[Here's a screenshot](http://i.imgur.com/WtmoEbz.png). One great
feature of Vim's mode is highlighting quoted values. This is available
with
[highlight-quoted](https://github.com/Fanael/highlight-quoted). As
pictured above, it highlights quotes, backticks, and quoted symbols.

<figure>
    <img src="/assets/highlight_escapes.png">
    <figcaption>highlight-escapes</figcaption>
</figure>

[highlight-escape-sequences](https://github.com/dgutov/highlight-escape-sequences)
is another minor mode with a simple goal. In this case, it highlights
escape sequences inside strings. It currently only supports Ruby and JavaScript.

<figure>
    <img src="/assets/enh_ruby_mode.png">
    <figcaption>enh-ruby-mode</figcaption>
</figure>

All these minor modes are matters of preference. If a major mode
developer likes this extended highlighting, they tend to include it in
their major mode anyway. In the above example, `enh-ruby-mode`
highlights all infix operators (in addition the standard Ruby highlighting).

## Semantic Highlighting

Some modes include a full parser rather than just a lexer. This
enables more sophisticated highlighting techniques.

<figure>
    <img src="/assets/js2_mode.png">
    <figcaption>js2-mode</figcaption>
</figure>

[js2-mode](https://github.com/mooz/js2-mode) is the best example of
this. js2-mode includes a full-blown recursive-descent ECMAScript
parser, plus a number of common JS extensions. This enables js2-mode
to distinguish more syntax types. For example, it can distinguish
parameters and global variables (pictured above).

This is an amazing achievement and even allows the editor to do many
checks that are traditionally done by lint tools. Highlighting globals
is particularly useful because use of a global is not necessarily an
error, but it's useful information about the code. js2-mode can also
be configured to highlight globals specific to your current project or
JS platform (see `js2-additional-externs`).

## S-expression Highlighting

Emacs also offers a number of specialist highlighting modes for s-expressions.

<figure>
    <img src="/assets/paren_face.png">
    <figcaption>paren-face</figcaption>
</figure>

[paren-face](https://github.com/tarsius/paren-face) is a simple minor
mode that assigns an additional face to brackets, enabling you to
style brackets separately. It's intended to fade out the brackets, so
you can focus on the rest of your code.

<figure>
    <img src="/assets/rainbow_delimeters.png">
    <figcaption>rainbow-delimeters</figcaption>
</figure>

[rainbow-delimeters](https://github.com/jlr/rainbow-delimiters) takes
the opposite approach. Each level of brackets is assigned a unique face,
enabling you give each one a different colour. This works particularly
well when using `cond`, as it's easy to spot the different boolean
expressions.

By default it allows nine levels of nesting before cycling colours
(see `rainbow-delimeters-max-face-count`) but you will have to choose
a tradeoff between more levels and contrast between the colours of the
different levels. I settled for six levels that are very distinct (the
defaults are rather subtle).

<figure>
    <img src="/assets/rainbow_blocks.png">
    <figcaption>rainbow-blocks</figcaption>
</figure>

If you like rainbow-delimeters,
[rainbow-blocks](https://github.com/istib/rainbow-blocks) applies the
same technique, but colours everything according to the nesting
depth. It's fantastic for seeing nesting, but it does limit how much
else you can highlight.

<figure>
    <img src="/assets/highlight_stages.png">
    <figcaption>highlight-stages</figcaption>
</figure>

[highlight-stages](https://github.com/zk-phi/highlight-stages/)
specifically targets quoted and quasi-quoted s-expressions. It enables
the reader to easily spot unquoted parts of a quasi-quote, and is
particularly useful if you're nesting quasi-quotes.

## Standard Library Highlighting

Another school of thought is that you should highlight all functions
from the language standard or standard library. Xah Lee subscribes to
this philosophy, and has released
[JS](http://ergoemacs.org/emacs/xah-js-mode.html) and
[elisp](http://ergoemacs.org/emacs/xah-elisp-mode.html) modes that
provide this.

<figure>
    <img src="/assets/highlight_elisp_builtins.png">
    <figcaption>xah-elisp-mode</figcaption>
</figure>

This is difficult to do in elisp as it's a lisp-2, and this mode can
confuse variables and functions slots (so `list` is highlighted even
when used as a variable).

<figure>
    <img src="/assets/python_builtins.png">
    <figcaption>python-mode</figcaption>
</figure>

The default Python mode in Emacs takes a similar approach,
highlighting the 80 built-in functions and some methods on built-in
types. This is hard to do in general, and `python-mode` will
incorrectly highlight similarly-named methods on other types or
methods whose name matches built-in functions.

## Docstring Highlighting

<figure>
    <img src="/assets/elisp_docstring.png">
    <figcaption>docstrings in elisp</figcaption>
</figure>

Docstrings are conceptually between strings and comments: they're for
the reader (like comments), but they're available at runtime (like
strings). Emacs exposes separate faces for  comments, strings and
docstrings (`font-lock-comment-face`, `font-lock-string-face` and
`font-lock-doc-face` respectively).

Elisp docstrings may also contain additional syntax for
cross-references. Emacs will highlight these differently too (though
their primary purpose is linking cross-references in `*Help*` buffers).

<figure>
    <img src="/assets/jsdoc_highlighting.png">
    <figcaption>js2-mode with JSDoc</figcaption>
</figure>

Some languages support elaborate syntax in their comments, both to
help the reader and to aid automatic documentation tools. In this
example, js2-mode offers additional highlighting of JSDoc comments.

## Contextual Highlighting

Another important area of highlighting is to highlight elements based
on where the cursor ('point' in Emacs terminology) is currently
located.

<figure>
    <img src="/assets/highlight_matching.png">
    <figcaption>show-paren-mode</figcaption>
</figure>

The most basic contextual highlighting is showing the matching bracket
to the bracket currently at the cursor. This is part of Emacs, but off
by default: `show-paren-mode` will switch it on.

<figure>
    <img src="/assets/hl_line.png">
    <figcaption>hl-line-mode</figcaption>
</figure>

Highlighting the current line is a very common feature of editor
highlighting, and Emacs provides `hl-line-mode` for this. This works
well for line-oriented programming languages. 

<figure>
    <img src="/assets/highlight_sexp.png">
    <figcaption>hl-sexp-mode</figcaption>
</figure>

When dealing with s-expressions, you can take this a step further with
[hl-sexp](https://github.com/emacsmirror/hl-sexp). This shows the
entire s-expression under point, avoiding confusion when editing
deeply nested expressions.

<figure>
    <img src="/assets/highlight_nested_parens.png">
    <figcaption>highlight-parentheses-mode</figcaption>
</figure>

[highlight-parentheses](https://github.com/nschum/highlight-parentheses.el)
takes a more subtle approach. It highlights the current bracket as
'hot', and highlight outer brackets in progressively 'cooler' colours.

<figure>
    <img src="/assets/highlight_current_symbol.png">
    <figcaption>highlight-symbol-mode</figcaption>
</figure>

The last example in this section is the superb
[highlight-symbol](https://github.com/nschum/highlight-symbol.el). This
is invaluable for showing you where else the current symbol is being
used. highlight-symbol is conservative and only does when the point
isn't moving, but set `highlight-symbol-idle-delay` to 0 to override
this.

`highlight-symbol-mode` is particularly clever in that it's able to
inspect the current syntax table. This prevents it from becoming
confused with strings like `x-1`, which is usually a single symbol in
lisps, but equivalent to `x - 1` in many other languages.

## Explicit Highlighting

<figure>
    <img src="/assets/hi_lock_mode.png">
    <figcaption>hi-lock-mode</figcaption>
</figure>

There comes a point where automatic highlighting isn't sufficient, and
you want to explicitly highlight something. Emacs provides
`hi-lock-mode` for this, and supports a special comment syntax that
allows other readers to see the same highlighting.

## Substitutions

It's also possible to configure Emacs to change how it display the
text itself.

<figure>
    <img src="/assets/pretty_symbols.png">
    <figcaption>pretty-symbols-mode</figcaption>
</figure>

There are several modes in Emacs for substituting strings like
`lambda` or `<=` with their mathematical counterparts. Emacs 24.4 will
also include a `prettify-symbols-mode` that provides this.

This works very well when editing LaTeX documents, but can be tricky
with code. In cases like `lambda` you're replacing with a shorter
string, which means you end up indenting differently depending on
whether you have substitutions switched on.

<figure>
    <img src="/assets/glasses_mode.png">
    <figcaption>glasses-mode</figcaption>
</figure>

`glasses-mode` is a fun minor mode for users who don't like
CamelCase. It displays camel case symbols with underscores, so
`FooBar` becomes `Foo_Bar`, without changing the underlying text.

## Hashed Highlighting

<figure>
    <img src="/assets/color_identifiers.png">
    <figcaption>color-identifiers-mode</figcaption>
</figure>

One novel approach to highlighting code is to give each symbol a
different colour. You simply hash each string and assign a colour
accordingly. This means that variables with similar spellings get
completely different colours.

This was popularised recently by
[an article by Evan Brooks](https://medium.com/@evnbr/coding-in-color-3a6db2743a1e)
and
[color-identifiers-mode](https://github.com/ankurdave/color-identifiers-mode)
was released as a result (pictured above). KDevelop has had this feature for
some time, calling it 'semantic highlighting'. IRC clients often use
this technique for nickname highlighting.

Whilst powerful, it's tricky to get right. Too few colours, and
different symbols end up the same colour. Too many colours, and it's
hard to visually distinguish some pairs of colours. In the above
image, `url` and `encodeURIComponent` are quite
similar. This small code snippet does not really take full advantage
of hashed-based highlighting: it's most effective when you have a
larger piece of code with more distinct symbols.

## Introspective Highlighting

<figure>
    <img src="/assets/highlight_defined.png">
    <figcaption>highlight-defined-mode</figcaption>
</figure>

Finally, self-hosting environments, such as Emacs or Smalltalk, can
offer additional highlighting
possibilities. [highlight-defined](https://github.com/Fanael/highlight-defined)
enables you to highlight functions, variables or macros that are
currently defined.

This works well for spotting typos in variable names, but it's a
little more sophisticated. In the above image, we can see that
`fibonacci` has been evaluated, so the recursive calls are
highlighted. We can even see whether we've forgotten to evaluate any
library imports!

<figure>
    <img src="/assets/pharo_selectors.png">
    <figcaption>Pharo Workspace highlighting selectors</figcaption>
</figure>

[Pharo](http://pharo.org/) (a Smalltalk implementation) is also able
to do this. The methods of classes (called 'selectors') may be changed
at any point, but the environment can introspect to see if the current
selector is appropriate for the value it is being called on.

This is quite different from the traditional Java-style IDE
integration, as it's based on runtime information in the current
process, instead of static analysis.

In practice however, many of the benefits of introspective
highlighting are provided by calling an external language-specific lint
tool from the editor.

## Conclusions

It's really hard to compose syntax highlighting tools. Some of the
examples here are very intrusive (particularly rainbow-blocks
color-identifiers-mode), preventing you from using them in addition to
other tools. The contextual highlighting tools are the best in this
regard.

There's a lot of information that could be displayed by the editor,
but relatively little can be shown at once. The primary options for
highlighting are only text colour, background colour, weight,
lines (underline, overline, strikethrough) and fringes (colours shown
at the left edge of the editor window).

If you're writing a highlighting tool in Emacs, try to define your own
faces wherever possible. For example, highlight-stages doesn't provide
a face, so it can only highlight quasi-quotes by changing the
background colour. If you're already using the background colour to
highlight something else, you cannot make highlight-stages use
underlines instead. I had similar problems with modes that dynamically
define faces, as you can't customise them in the normal way.

When you release a highlighting tool, please include
screenshots. It's amazing how many tools that I've listed have no
screenshots on their GitHub pages.

Personally, I like angry fruit salad. Lots of contrasting colours for
different lexical classes, plus tons of contextual highlighting, is
the sweet spot for me. Experiment, and see what suits you.
