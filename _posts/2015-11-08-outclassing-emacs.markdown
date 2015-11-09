--- 
layout: post
title: "Outclassing Emacs"
tags:
 - emacs
---

Software comes and goes, platforms change, use cases are adapted for
contemporary usage.

Somehow, Emacs has outlived everything. In an industry that's
continually reinventing itself, it's incredible that a 1976 text
editor is still in active use.

What can Emacs teach us about the design of programming tools? What
tools will surpass Emacs in the very areas it excels in?

In this blog post, I will explore Emacs' strengths, weaknesses, and
then look at newer, more potent competitors.

## Inspect Editor At Point

<figure>
    <img src="/assets/inspect_element_chromium.png">
    <figcaption>Inspecting HTML in Chromium</figcaption>
</figure>

Every browser today allows you inspect the HTML of the webpage you're
looking at. If you see an interesting web page design, you can see
what elements are used, and how they're styled.

Emacs provides this for the whole editing experience.

<figure>
    <img src="/assets/emacs_describe_tab.png">
    <figcaption>What code runs when we press tab?</figcaption>
</figure>

This is immensely powerful. We can ask profound questions of the
functionality we're using:

Q: What command is run when I press this key?
A: `c-indent-line-or-region`

Q: What is this command supposed to do?
A: It indents the currently selected text ('region'), the current
line, or the current block.

Q: What code is actualy executed when I press this key?
A: Emacs gives us a link to the code it's using.

This has a profound effect on *how* we develop code in Emacs. Suppose we
want to add new functionality. If we know of any similar commands, we
can effortless find their source and see how they achieve their
behaviour.

This isn't limited to functions. Emacs settings are simply variables
that we can set

Emacs also encourages docstrings for everything. Settings in Emacs are
just variables. W

## Just an Interpreter!

There's no difference between changing a setting and setting a
variable in Emacs[1].

<figure>
    <img src="/assets/python_settings.gif">
    <figcaption>Modifying settings / setting variables</figcaption>
</figure>


## Emacs Ecosystem

Huge melting pot of experiments to see what sticks.

## Emacs Limitations

## Victory of the commons: Atom

Amazing ecosystem, proper package manager. GitHub team know how to
build community projects.

Leveraging the HTML/CSS/JS developer community: many devs, but
sometimes performance suffers. Very cutting edge too: used react, then
native, trying to use the Electron tooling to best effect. Interesting
experiment, exploring the limits of the web platform.

Not just text: great colour pickers, markdown renderers etc.

Not very introspectable, but latest release adds good descriptions of
settings: http://blog.atom.io/2015/10/29/atom-1-1-is-out.html
('Settings Have Nice Descriptions')

Requires a separate master/slave editor rather than interactive
inplace changes.

## Meta Editing: Pharo

Very introspectable.

Maturing: moving away from Scratch's UI, better software engineering
(cf scratch being removed by gentoo for libjpeg vulnerabilities).

## Closing Thoughts

## Footnotes:

1: I'm glossing over the difference between `defcustom` and `defvar`
here. You can still use `setq` on `defcustom` variables.
