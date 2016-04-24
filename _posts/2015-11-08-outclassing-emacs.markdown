--- 
layout: post
title: "Outclassing Emacs"
tags:
 - emacs
---

It's bizarre that programmers still use Emacs. In an industry that's
continually reinventing itself, it's incredible that a 1976 text
editor is still in active use.

What can we learn from the design of Emacs? What tools will surpass
Emacs in the very areas it excels in?

Let's explore what makes Emacs special, and we'll try to predict which
tools will eventually beat Emacs at its own game.

## The Hackable Ecosystem

**Property** The Emacs ecosystem is huge and thriving.

Magit, smartparens, undo-tree, skewer-mode/cider, evil, aggressive-indent: people experiment and produce
incredible projects. Crazy projects: pschoanalyze, nyan progress, 

Huge melting pot of experiments to see what sticks.

Really easy to depend on 

**Competition** Most editors have package managers now (vim, sublime,
eclipse) and that's a good thing.

However, Atom is something special. Not only is HTML/JS a lingua
franca for many developers, the package manager is not an
afterthought. It's well-engineered, modelled on npm, and really easy
to share projects.

## Inspect Editor At Point

Have you ever clicked 'Inspect Element' in a web browser? This makes
it easy to find out what HTML a website uses in its design.

<figure>
    <img src="/assets/inspect_element_chromium.png">
    <figcaption>Inspecting HTML in Chromium</figcaption>
</figure>

Emacs provides this for the whole editing experience.

<figure>
    <img src="/assets/emacs_describe_tab.png">
    <figcaption>What code runs when we press tab?</figcaption>
</figure>

This is shockingly powerful. We can ask profound questions of the
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

describe-variable, apropos-value

Emacs also encourages docstrings for everything. Settings in Emacs are
just variables.

**Competition** Pharo

## Just an Interpreter!

There's no difference between changing a setting and setting a
variable in Emacs[1].

<figure>
    <img src="/assets/python_settings.gif">
    <figcaption>Modifying settings / setting variables</figcaption>
</figure>

**Competition** Pharo

## A UI that scales

Atom has arbitrary command names and a M-x equivalent.

## Emacs Limitations

No namespaces.

No GUI: not customisable.

Not pretty (cf atom/light table/sublime)

Learning curve (terminology, lisp syntax, keybindings, concepts).

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

## Low cognitive load: Zed

Nice simple UI.

## Closing Thoughts

## Footnotes:

1: I'm glossing over the difference between `defcustom` and `defvar`
here. You can still use `setq` on `defcustom` variables.
