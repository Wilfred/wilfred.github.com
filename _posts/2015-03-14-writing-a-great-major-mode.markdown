--- 
layout: post
title: "Writing a Great Major Mode"
tags:
 - emacs
---

Writing a major mode is a rite of passage for elisp hackers. Sooner or
later, you will find a programming language or configuration format
that is too new or obscure to have Emacs support.

You decide to roll up your sleeves and plug this hole in the Emacs
ecosystem. At this point, you may start to wonder, **what makes a great
major mode**?

## It's a labour of love

It doesn't take much effort to write a basic major mode. However,
there is infinite scope for polish. Whilst Emacs has supported
programming in C since its inception, even
[in 2015 it has been improved](https://github.com/emacs-mirror/emacs/commits/master/lisp/progmodes/cc-mode.el)!

## 1: A basic syntax table

The bare minimum for a major mode is a syntax table. If you can
highlight comments and strings, your mode is useful.

Here's how we'd write a minimal JS mode:

{% highlight common-lisp %}
(defconst my-js-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; ' is a string delimeter
    (modify-syntax-entry ?' "\"" table)
    ;; " is a string delimeter too
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

Congratulations, you're an elisp hacker! Add your major mode to MELPA
so others can benefit from your major mode.

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

## 3: Navigation

## 4: Indentation

Next, you'll want to tackle indentation. Users expect Emacs to indent
code correctly, which requires calculating how deeply nested the
current code is.

This is essentially a matter of searching the buffer backwards, counting
`{` (or equivalent), then your indent should be `(* count my-indent-width)`.
Provided you ignore `{` in strings and comments, this basically works.

> You could be a total lunatic, and *Emacs has to make you happy*.
>
> -- [Steve Yegge on indentation](http://steve-yegge.blogspot.com/2008_03_01_archive.html)

The 'correct' indentation is a highly contentious issue, so you will
eventually need to support different styles. If you get it right, you
should be able to open a large file from an existing project, run
`(indent-region (point-min) (point-max)` and nothing should change.

Indentation logic is very easy to test, and you can see some
[examples in julia-mode](https://github.com/JuliaLang/julia/blob/76df7f48b3956de7d2eb07a15c995c9304d5361f/contrib/julia-mode.el#L441).

It's worth noting that Emacs provides [SMIE](https://www.gnu.org/software/emacs/manual/html_node/elisp/SMIE.html), the Simple Minded
Indentation Engine. You write a BNF grammar and you get basic
indentation and movement commands for free.

You might be wondering why major modes don't just use the official
language grammar, which is often already available in BNF
form. Your indentation code needs to handle invalid syntax, which is
common when the user is typing.

Indentation also needs to be fast. Make sure you test your indenting
logic on large files, as indentation performance often needs careful
optimising.

## 5: The sky's the limit

Editing is only the beginning, there are many other nice features you
can add to make your users happy.

You'll want probably want to set up a linter with flycheck. Good
flycheck integration will help users spot syntax errors (saving a
compiler invocation) and many classes of bugs.

<figure>
    <img src="/assets/flycheck_python.png">
    <figcaption>Spotting bugs is a lot easier with flycheck</figcaption>
</figure>

You should also look at providing a company backend for
completion. Any data that's available with static analysis, examining
the filesystem, or querying the internet can be used.

<figure>
    <img src="/assets/c_member_completion.png">
    <figcaption>Top: member completion from compiler<br>
    </figcaption>
</figure>

<figure>
    <img src="/assets/c_header_completion.png">
    <figcaption>Library completion from filesystem
    </figcaption>
</figure>

<figure>
    <img src="/assets/pip_library_completion.png">
    <figcaption>Package completion from internet
    </figcaption>
</figure>

## 7: Interpreter integration

Basic to Cider/Slime.

## 8: Inline docs

eldoc, docstrings and parameters (cf pydoc)

## 9: Tooling integration

django commands, install library

## 9: Refactoring
