--- 
layout: post
title: "Tips For Developing Major Modes"
---

Major modes are at the heart of Emacs functionality. I recently
[wrote a mode for Cask files](https://github.com/Wilfred/cask-mode)
and explored what's the correct way to write a major mode
today. What's best practice?

## Highlighting

Every major mode starts with highlighting. First, define a syntax
table to highlight strings and comments.

Next, you'll want to highlight keywords. This is a fantastic visual
cue for users to whether their code is legal.

<img src="/assets/cask_incorrect_keyword.png">

Wherever possible, whitelist syntax. Cask accepts various different
fetchers, `:git`, `:github`, `:hg` and so on. A lazy solution would be
to highlight anything starting with `:`. By whitelisting, users can
see whether or not they've used a legal value.

Make sure you define symbol constituents in your syntax table:

{% highlight common-lisp %}
(defvar cask-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; ...
    ;; Treat : as a symbol character
    (modify-syntax-entry ?: "_" table)
    table))
{% endhighlight %}

This enables commands like `forward-symbol` to do the right thing.

Also, make sure you use `regexp-opt` with symbols (as we said symbols
before). You don't want substrings highlighted:

<img src="/assets/cask_highlight_substring.png">

There will probably be syntax in your target language that doesn't
have an obvious suitable face in Emacs. Define your own:

{% highlight common-lisp %}
(defface cask-mode-source-face
  '((t :inherit font-lock-variable-name-face))
  "Face for known cask sources."
  :group 'cask-mode)
{% endhighlight %}

Note that we've inherited from `font-lock-variable-name-face`, so we
get a colour by default, but users can still override this colour
specifically.

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

SAMPLE

If it fails, you get a useful assertion:



And indentation:

SAMPLE

## MELPA

Finally, your package should be uploaded to
[MELPA](http://melpa.org/). This makes it generally available and
allows others to depend on it.

In my experience, as soon as you upload to MELPA, you get bug reports
and pull requests.
