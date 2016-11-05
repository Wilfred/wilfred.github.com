--- 
layout: post
title: "Introspecting Glue Code"
tags:
 - emacs
---

Writing glue code is easy. Writing it *well* is harder.

Glue code is particularly common when developing editor plugins: you
want to leverage the existing tools, often via a command line
interface.

When bugs occur, it's hard to know where the fault lies. Is this an
issue with your glue code, or the underlying tool? We frequently face
this problem with
[emacs-racer](https://github.com/racer-rust/emacs-racer). Users feel
powerless: they don't know if they've misconfigured something, if the
glue code is wrong, or if they've hit an genuine racer bug.

It turns out that [flycheck](http://www.flycheck.org/en/latest/)
already has a great solution to this. Flycheck can actually tell you
why it couldn't run a checker!

<figure>
    <img src="/assets/flycheck_check.png">
    <figcaption>
    <pre>M-x flycheck-select-checker</pre>
    </figcaption>
</figure>

Looking at flycheck, I realised this was exactly what emacs-racer
needed. I built a `M-x racer-debug` command that tells you exactly
what happened, and how to reproduce the issue outside of Emacs.

<figure>
    <img src="/assets/racer_debug.png">
    <figcaption>
    <pre>M-x racer-debug</pre>
    </figcaption>
</figure>

This makes it much easier to see what racer is doing. Not only is it
great for bug reports, but it helps us develop new features
too. Developers can see exactly what data we receive from racer.

I'm still surprised by the versatility of introspective tools. When
building functionality, it's incredibly useful to be able to ask 'what
just happened?'. Debuggable debugging tools are a superpower.
