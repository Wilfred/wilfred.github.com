--- 
layout: post
title: "Best Practices When Developing Major Modes"
---

Major modes are at the heart of Emacs functionality. I recently
[wrote a mode for Cask files](https://github.com/Wilfred/cask-mode)
and explored what's the correct way to write a major mode
today. What's best practice?

## Highlighting

Highlighting hasn't changed significantly. You define a syntax table
to get string and comment highlighting. Make sure you also do symbol
constituents, so dabbrev and navigation works.

You'll also want to highlight keywords and special values. This is a great way of seeing
what values are available. For Cask, we highlight all the fetchers.

PICTURE WITH INVALID FETCHER

Also, make sure you use `regexp-opt` with symbols (as we said symbols
before). You don't want substrings highlighted:

BAD HIGHLIGHTING OF FETCHER SUBSTRING

Highlighting faces should inherit, but be separate.

## Indentation

This is very language specific. For s-expression languages it's usually
straightforward to work out what you're looking at: just count the
parens and check for any keywords.

You *can* do the same thing for other languages. Where possible, Emacs
has the
[Simple Minded Indentation Engine](https://www.gnu.org/software/emacs/manual/html_node/elisp/SMIE.html)
where you define a language grammar and get indentation functionality
for free.

SMIE is fairly new, only added to Emacs in 2010. Emacs core is
gradually migrating, with SQL, sh, Ruby and Modula 2 ported already.

## Code Completion

Use capf, not company.

## Testing

The most recent addition to major mode tooling is
[assess](https://github.com/phillord/assess). This adds some brilliant
assertion functions for writing major modes.

You can test highlighting:

PICTURE

And indentation:

PICTURE

## MELPA

Finally, your package should be uploaded to
[MELPA](http://melpa.org/). This makes it generally available and
allows others to depend on it.

In my experience, as soon as you upload to MELPA, you get bug reports
and pull requests.
