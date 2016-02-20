--- 
layout: post
title: "Coming in Emacs 25.1: Even Better Introspection!"
tags:
 - emacs
---

Emacs 25 is now
[on its second pretest release](https://lists.gnu.org/archive/html/emacs-devel/2016-02/msg00787.html),
so now is a great time to play with it. You can check for bugs in
core, make sure your favourite packages work, and get a sneak preview
of upcoming features!

Emacs is already a self-documenting editor, but 25.1 will include
several improvements to discovering how your editor is configured.

`C-h k` is a really handy command for finding out what command runs
when you press a key. Emacs 25 improves on this, telling you which
keymap it found the keybinding in.

If you've ever had a major mode or minor mode override some global
keybinding you've set up, it's now easy to find the cause.

<img src="/assets/emacs_describe_key.png">

What if you've already typed something and you want to know what
happened? `M-x view-lossage` will show you the last keys you pressed,
so you can work out what happened. This is much improved in Emacs 25,
where you can directly see which commands were called by each
keypress:

<img src="/assets/emacs_view_lossage.png">

Emacs: now more Emacsy than ever.

*If you liked this, [Artur Malabarba has a great series](http://endlessparentheses.com/new-in-emacs-25-1-map-el-library.html) on other
features upcoming in 25.1.*
