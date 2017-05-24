--- 
layout: post
title: "The Astounding Variety of Groovy Strings"
---

I've been writing Groovy recently. Groovy has a *shockingly* flexible
string syntax, as I discovered when trying to improve
the
[Emacs mode](https://github.com/Groovy-Emacs-Modes/groovy-emacs-modes).

Groovy tries really hard to make you happy. It provides normal
strings:

{% highlight groovy %}
"hello world"
{% endhighlight %}

and since it's a recent scripting language, it provides string
interpolation too!

{% highlight groovy %}
def name = "wilfred"
"hello $name"
{% endhighlight %}

Groovy doesn't stop there, however. Oh no.

The string interpolation is very flexible. You can access object
attributes, or even write arbitrary Groovy expressions.

{% highlight groovy %}
"hello $user.name"
// Interpolations are challenging to highlight,
// because { } can nest!
"hello ${if (true) { 1 } else { 2 }}"
{% endhighlight %}

Naturally, you might not always want interpolation. Groovy provides an
escaping mechanism, and an additional syntax for this:

{% highlight groovy %}
// Escaping the $.
"the cost is \$10"
// No interpolation with single-quoted strings.
'the cost is $10'
{% endhighlight %}

You want multiline strings? Groovy has those too, in both interpolated
and raw flavours:

{% highlight groovy %}
"""hello
world"""
'''hello
world'''
{% endhighlight %}

These are useful, but sometimes you want to start a line with
`"""`. Groovy lets you escape that:

{% highlight groovy %}
def foo = """\
line 1 (no newline before this line)
line 2"""
{% endhighlight %}

Other scripting languages also support a `/foo/` syntax. Groovy users
might want this as well, so *of course* they're available:

{% highlight groovy %}
// This is tricky to highlight correctly, because "" is 
// an empty string but // begins a comment.
/fo+b{ar,az}/
{% endhighlight %}

For the brave of heart, you can interpolate into your regular
expressions:

{% highlight groovy %}
// Simple interpolation.
def name = "wilfred"
/user $name/

// However, what if we want to use $ in a pattern?
// This works because $ is not a legal interpolation,
// so it magically falls back to a literal pattern.
/user$/
{% endhighlight %}

Groovy doesn't even stop here. What if you have a string with many
embedded single-quotes and double-quotes? The interpolated, multiline,
dollar-slashy-string is available!

The dollar-slashy-string is so awkward to highlight that even [the
official docs don't try to highlight it](http://groovy-lang.org/syntax.html#_dollar_slashy_string)!

{% highlight groovy %}
def message = $/Dear ${user.capitalize()},
Thank you for signing up to "Acme's Mailing list".

Kind Regards./$

// This string syntax has its own escaping mechanism:
"foo / /$ bar" == $/foo $/ $/$ bar/$
{% endhighlight %}

To stake its claim as an incredibly versatile syntax, Groovy strings
have one last trick up their sleeves. Strings can be **lazily
evaluated**!

{% highlight groovy %}
def number = 1
def eagerString = "${number}"
number = 2
assert eagerString == "1"

// However:
def number = 1
def lazyString = "${ -> number}"
assert lazyString == "1"
number = 2
assert lazyString == "2"
{% endhighlight %}

Needless to say, only a total madman would try to write a syntax
highlighter for this:

<img src="/assets/groovy_mode_strings.png">

Groovy is a really neat language that is excellent for glue code. I've
had a whole lot of fun
[making it work in Emacs](https://github.com/Groovy-Emacs-Modes/groovy-emacs-modes).
