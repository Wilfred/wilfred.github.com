--- 
layout: post
title: "Helpful: One Year On"
tags:
 - emacs
---

It's been a year since the first release of
[Helpful](/blog/2017/08/30/helpful-adding-contextual-help-to-emacs/)!
It's gained a ton of new features, and I'd love to share the
highlights with you.

## Tool Integration

<img src="/assets/helpful_more_debugging.png">

Emacs has some excellent built-in debugging tools that I wanted to
expose within Helpful. These tools missed the first release, but I've now
had the chance to build them.

From a Helpful buffer, you can now toggle edebug of a
function. This allows you to easily step through some code.

You can also toggle tracing. Tracing is an underrated built-in feature
of Emacs. You can use it to confirm functions have the inputs and
outputs you're expecting. It's also really useful for exploring
unfamiliar code.

<img src="/assets/emacs_trace.png">

In this example, I've enabled tracing on `projectile-project-root` to
see when it's called, and what values it's returning.

## Summaries

After some great user feedback, Helpful buffers now start with a
summary of what you're looking at.

<img src="/assets/projectile_intro.png">

Users often want a direct link to the source code, so this is included
in the summary. Helpful also mentions if a function is interactive or
autoloaded, just like `describe-function`. If a user doesn't know what
that means, those words now link to the relevant part of the Emacs
manual!

## Aliases

Helpful tries to show all relevant information for the current
thing. I've overhauled aliases with this in mind.

<img src="/assets/obsolete_alias.png">

For example, if you view `make-hash-table`, you can now see that there
is another alias of this function, but it's now deprecated.

## Modifying Variables

<img src="/assets/helpful_set_variable.png">

It's now possible to set variables directly from Helpful buffers. This
was inspired by `counsel-set-variable`, which has an excellent similar
feature.

If a variable is a `defcustom`, then Helpful also includes a link to
the relevant part of Customize.

## Even Better Docstrings

Helpful now handles all Emacs docstring syntax. It handles references
to keybindings, keymaps, and even supports the obscure features like
`\<foo-map>` and `\='`.

<img src="/assets/fortran_mode.png">

`fortran-mode` is a great example of a docstring that uses a lot of
Emacs docstring features.

Recent versions of Helpful try even harder to save you
keystrokes. URLs are now automatically converted to links.

Finally, my favourite new feature is Info manual links. If a docstring
mentions a section of the manual, Helpful converts it to a link.

<img src="/assets/helpful_info_reference.png">

Even better, if a symbol is documented in the Emacs manual, Helpful
automatically adds a link at the bottom of the docstring! This hugely
helps discoverability.

<img src="/assets/helpful_view_in_manual.png">

## Installing It

[Helpful](https://github.com/Wilfred/helpful) is available on
[MELPA](http://www.melpa.org/#/helpful) and [MELPA
stable](https://stable.melpa.org/#/helpful). It's become an
indispensable part of my Emacs workflow, and I'm sure it will benefit
you too.
