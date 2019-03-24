--- 
layout: post
title: "The Siren Song of Little Languages"
---

Some programming languages languish due to obscurity. They lack
breathless blog posts exclaiming how much nicer they are to use.

Other languages are too ambitious. They aspire to support so many
features that the original implementers struggle to get a first
version working. For example, the type system in Fortress
[required constraint solving which took exponential time](https://youtu.be/EZD3Scuv02g?t=50m10s).

Sometimes a usable language struggles simply because **it's too much
fun to write your own**. Developers end up building their own implementation
rather than actually *using* the language.

The most obvious implementation-focused language
is [BF](https://en.wikipedia.org/wiki/Brainfuck). Despite
having
[many implementations](https://esolangs.org/wiki/Brainfuck_implementations),
BF programmers have to [encourage the implementers to actually try using the language](http://www.hevanet.com/cristofd/brainfuck/epistle.html)!

Scheme is also susceptible to this. Wikipedia
lists
[31 different Scheme implementations](https://en.wikipedia.org/wiki/Category:Scheme_(programming_language)_implementations),
not to mention the many toy implementations. Writing a Scheme is a
great introduction to interpreters,
especially
[once you get beyond the minimal lisp featureset](http://axisofeval.blogspot.co.uk/2011/01/more-fully-featured-modern-lisps.html).
It's a little harder to find actively developed Scheme applications
(though they certainly exist).

The problem seems to be languages with a small, well written
specification. [Shen](http://www.shenlanguage.org/) is a
multiparadigm lisp defined in terms of an
[elegant base language](http://www.shenlanguage.org/learn-shen/shendoc.htm#Kl) with
only 46 system functions. This has resulted in a remarkable
[15 third-party implementations](https://github.com/Shen-Language/wiki/wiki#ports),
but
[only a small number of libraries implemented in the language](http://www.shenlanguage.org/library.html).

This phenomenon is not limited to lisps. Forth is also a language
that developers often prefer to implement rather than
use. [Jones Forth](https://github.com/AlexandreAbreu/jonesforth/blob/master/jonesforth.S) is
both a Forth tutorial and a discussion of how to build a Forth
compiler. There are even
[stories of people spending years working on implementations without learning much of the language](http://yosefk.com/blog/my-history-with-forth-stack-machines.html).

Designing a language with a straightforward implementation is not a
bad thing. It's just a pitfall that language designers need to be
aware of. [Some crypto systems have this problem
too](http://www.moserware.com/2009/09/stick-figure-guide-to-advanced.html#act-3-details).

It seems that we need languages to be big enough that new users write
hello world *in* the language, not write a tool for *others* to write
hello world. In the Lisp family, Clojure and Racket seem to have
reached a size threshold where newcomers are happy downloading the
canonical implementation.

This doesn't mean multiple implementations are bad. It's a great sign
of language health. We just don't need these until there's a community of
*users* and there is a need for implementations with different
qualities.

A language can be too small and elegant.
