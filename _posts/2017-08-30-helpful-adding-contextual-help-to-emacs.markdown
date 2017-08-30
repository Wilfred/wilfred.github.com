--- 
layout: post
title: "Helpful: Adding Contextual Help to Emacs"
tags:
 - emacs
---

I've just released [Helpful](https://github.com/Wilfred/helpful), a
new way of getting help in Emacs!

<img src="/assets/helpful.png">

The `*Help*` built-in to Emacs is already pretty good. Helpful
goes a step further and includes lots of contextual info. Let's take a
look.

Have you ever wondered which major modes have a keybinding for a
function? Helpful reports keybindings in all keymaps!

<img src="/assets/helpful_bindings.png">

When you're hacking on some new code, you might end up with old
function aliases after renaming a function. Helpful provides
discoverable debug buttons, so you don't need to remember
`fmakunbound`.

<img src="/assets/helpful_tools.png">

Helpful also has strong opinions on viewing docstrings. Summaries
are given focus, and text is fontified. We solve the
`text-quoting-style` debate by removing superfluous puncuation
entirely.

<img src="/assets/helpful_docstring.png">

Helpful will even show all the references to the symbol you're
looking at,
using [elisp-refs](https://github.com/Wilfred/elisp-refs). This is
great for understanding how and where a function is used.

<img src="/assets/helpful_refs.png">

Finally, Helpful will rifle through your Emacs instance to find
source code to functions:

* If you've defined a function interactively, Helpful will
use edebug properties to find the source code. 

* If Emacs can only find the raw `closure`, helpful will convert it
back to an equivalent `defun`.

* If Emacs can only find the byte-compiled files, helpful will just
  pretty-print that.
  
<img src="/assets/helpful_interactive_defun.png">

<img src="/assets/helpful_closure.png">

I've just released v0.1, so there will be bugs. Please give it a try,
and let me know what you think, or how we can make it even more, well,
helpful!
