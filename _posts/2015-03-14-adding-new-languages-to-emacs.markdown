--- 
layout: post
title: "Adding New Languages to Emacs"
tags:
 - emacs
---

Writing a major mode is a rite of passage for elisp hackers. Sooner or
later, you will find a programming language or configuration format
that is too new or obscure to have Emacs support.

You decide to roll up your sleeves and plug this hole in the Emacs
ecosystem. How do you write a major mode? What will make your major mode
great?

## 1: A basic syntax table

The bare minimum for a major mode is a syntax table. If you can
highlight comments and strings, your mode is useful.

Here's how we'd write a minimal JS mode:

{% highlight common-lisp %}
(defconst my-js-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; ' is a string delimiter
    (modify-syntax-entry ?' "\"" table)
    ;; " is a string delimiter too
    (modify-syntax-entry ?\" "\"" table)

    ;; / is punctuation, but // is a comment starter
    (modify-syntax-entry ?/ ". 12" table)
    ;; \n is a comment ender
    (modify-syntax-entry ?\n ">" table)
    table))

(define-derived-mode my-js-mode prog-mode "Simple JS Mode"
  :syntax-table my-js-mode-syntax-table
  (font-lock-fontify-buffer))
{% endhighlight %}

Here's the result:

<img src="/assets/simple_js_mode.png">

This might not seem like much, but it's often sufficient for config
file formats.

Congratulations, you're an elisp hacker! Add your major mode to
[MELPA](http://melpa.org/) so others can use and contribute to your
new mode.

## 2: Full syntax highlighting

From here, there's huge scope to expand. You'll want to look at
[sophisticated syntax highlighting](/blog/2014/09/27/the-definitive-guide-to-syntax-highlighting/)
to cover all the features of the language.

As you make your major mode more sophisticated, you should worry about
writing tests for it. Many major modes have no tests, but a
self-respecting hacker like you likes bugs to stay fixed.

The first step is to create a sample file of syntax corner-cases and
just open it. This doesn't scale, so you will eventually want
programmatic tests. Fortunately,
[puppet-mode has some great examples](https://github.com/lunaryorn/puppet-mode/blob/1813c7bc46f178aeab5d78d5268dda0dd756c305/test/puppet-mode-test.el#L107).

## 3: Indentation

Next, you'll want to tackle indentation. Users expect Emacs to indent
code correctly, which requires calculating how deeply nested the
current code is.

This is essentially a matter of searching the buffer backwards from
point, counting `{` (or equivalent). You then adjust the current line
to be indented `(* count my-indent-width)`.  Provided you're careful
with `{` in strings and comments, this works.

Alternatively, Emacs provides
[SMIE](https://www.gnu.org/software/emacs/manual/html_node/elisp/SMIE.html),
the Simple Minded Indentation Engine. You write a BNF grammar and you
get basic indentation and movement commands for free.

> You could be a total lunatic, and *Emacs has to make you happy*.
>
> -- [Steve Yegge on indentation](http://steve-yegge.blogspot.com/2008_03_01_archive.html)

In practise, users will disagree on what the 'correct' indentation is,
so you will ultimately need to provide settings for different
styles. If you get it right, you should be able to open a large file
from an existing project, run `(indent-region (point-min)
(point-max))` and nothing should change.

Indentation logic is very easy to test, and you can see some
[examples in julia-mode](https://github.com/JuliaLang/julia/blob/76df7f48b3956de7d2eb07a15c995c9304d5361f/contrib/julia-mode.el#L441).

Your indentation code needs to handle invalid syntax, which is
common when the user is typing.

Indentation also needs to be fast. Make sure you test your indenting
logic on large files, as indentation performance often needs careful
optimising.

## 4: Flycheck

You'll want probably want to set up a linter with
[flycheck](http://www.flycheck.org/en/latest/). Even if there aren't
any sophisticated lint tools available, highlighting syntax errors
as-you-type is very helpful.

<figure>
    <img src="/assets/flycheck_python.png">
    <figcaption>
    <a
    href="https://github.com/Wilfred/flycheck-pyflakes">flycheck-pyflakes</a>
    showing an unused variable
    </figcaption>
</figure>

## 5: Completion

You should also look at providing a [company](http://company-mode.github.io/) backend for
completion. Here are some examples to inspire you:

<figure>
    <img src="/assets/c_member_completion.png">
    <figcaption>
    company-clang (part of company) uses Clang to discover struct members
    </figcaption>
</figure>

<figure>
    <img src="/assets/c_header_completion.png">
    <figcaption>
    <a
    href="https://github.com/randomphrase/company-c-headers">company-c-headers</a>
    inspects the local filesystem to suggest C headers
    </figcaption>
</figure>

<figure>
    <img src="/assets/pip_library_completion.png">
    <figcaption>
    <a
    href="https://github.com/Wilfred/pip-requirements.el">pip-requirements</a>
    accesses the internet to find out what packages are available
    </figcaption>
</figure>

## 6: Eldoc

[Eldoc](http://www.gnu.org/software/emacs/manual/html_node/emacs/Lisp-Doc.html)
is an elisp 

eldoc

<figure>
    <img src="/assets/elisp_eldoc.png">
    <figcaption>
    eldoc showing docstrings in elisp
    </figcaption>
</figure>

<figure>
    <img src="/assets/c_eldoc.png">
    <figcaption>
    <a
    href="https://github.com/nflath/c-eldoc">c-eldoc</a>
    showing the function prototype for the function at point
    </figcaption>
</figure>

## 7: REPL integration

comint vs cider/slime

<figure>
    <img src="/assets/cider.png">
    <figcaption>
    alternatively, you may want to use eldoc to show
    docstrings
    </figcaption>
</figure>

## It's a labour of love

It doesn't take much effort to write a basic major mode. However,
there is infinite scope for polish. Whilst Emacs has supported
programming in C since its inception, even
[in 2015 it has been improved](https://github.com/emacs-mirror/emacs/commits/master/lisp/progmodes/cc-mode.el)!
