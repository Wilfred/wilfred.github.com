--- 
layout: post
title: "Helpful: Contextual Help for Emacs"
tags:
 - emacs
---

I am delighted to
announce [Helpful](https://github.com/Wilfred/helpful), an alternative
to the built-in `*Help*` buffer!

<img src="/assets/helpful.png">

Emacs' built-in help is very useful. Virtually every function has
a docstring with cross-references to other functions, and `*Help*`
displays this.

There's a wealth of additional information that Emacs knows but
doesn't display in `*Help*`. Helpful is a project to fix that.

Helpful will report keybindings in all keymaps, not just the global
keymap.

<img src="/assets/helpful_bindings.png">

When you're developing new functions, you might want to disassemble
them or unbind old names. Helpful provides buttons for this
functionality.

<img src="/assets/helpful_tools.png">

Helpful includes an opinionated docstring formatter. The function
summary is put on a separate line, superfluous punctuation is removed,
and everything is cross-referenced.

<img src="/assets/helpful_docstring.png">

I've also integrated 
[elisp-refs](https://github.com/Wilfred/elisp-refs) into Helpful. When
viewing a symbol, helpful shows all the references in its defining
file. This is great for understanding where a function is used.

<img src="/assets/helpful_refs.png">

Finally, Helpful tries really hard to find source code for
your functions: 

* If you've defined a function interactively, Helpful will
use edebug properties to find the source code. 

* If Emacs can only find the raw `closure`, helpful will convert it
back to an equivalent `defun`.

* If Emacs can only find the byte-compiled files, helpful will just
  pretty-print that.
  
<img src="/assets/helpful_interactive_defun.png">

<img src="/assets/helpful_closure.png">

I've just released v0.1, so there will be bugs. Please give it a try,
and let me know what you think, or how we can make it even more
helpful!
