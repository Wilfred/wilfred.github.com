--- 
layout: post
title: "Minimalism: Keeping code to a minimum"
---

There is only one program that is completely bug free, well documented, reliable and perfectly commented. Let me quote it in full: "". Now, some naysayers may claim this isn't a valid program -- yet this program has won <a href="http://www.ioccc.org/1994/smr.hint">a programming contest</a>!

Sadly, the empty program is probably lacking a few features you require. Although any additions we implement will reduce performance and readability, keeping this ideal in mind is worthwhile. We therefore conclude that great code is minimalist.

Before continuing, it is worth considering what this doesn't mean. Minimalism is a target we consider for clarity. Reducing the number of functions (where sensible) is helpful; making variable names shorter is not. <a href="http://codegolf.com/">Code golf</a> is an entertaining practice, not a good habit.

<h3>Your code: A biography</h3>

Production code will spend most of its life in maintenance. The source will be read at a later date to try to make sense of its workings. But even in the initial development stages you will write and rewrite sections as you tackle problems. Clear, readable code makes this initial writing easier. Even if you write throwaway code you may wish to reuse its ideas quickly in the future. Here again, readability counts.

<h3>Blinding clarity</h3>

<blockquote>When the light went on it almost blinded me.

<a href="http://catb.org/jargon/html/story-of-mel.html">The Story of Mel</a></blockquote>

Being in the dark is no fun. Black boxes are great -- when they work, and you don't need to examine them. Minimal code turns reading into a stroll down a garden path, rather than a mountain trek. The sooner you can reach the "oh, I get it" moment, the better. It's at this point that you can begin to understand what is going on, and the fun begins.

Clear code is a pleasure to work with. The results are easy to work with, (almost) painless to debug and leave the programmer satisfied.

<h3>Cutting to the chase</h3>

So, how do we reach this point? Make every word count. Ensure every function has a name so clear it is almost defined by the name alone. Consider each variable name carefully, then consider them again when refactoring. Aim to write readable code first and foremost, and if (and only if) your profiler shows this simple approach is to slow, add the minimal possible complexity to it, and comment it appropriately.

It's amazing how much code runs routinely in major production roles, yet the moment the source is marked as being released to the public, the coders say "wait, that needs cleaning up". If you wouldn't be happy showing your code to someone else today, perhaps you need to make it more minimal.

Re-read, refactor, reduce.
