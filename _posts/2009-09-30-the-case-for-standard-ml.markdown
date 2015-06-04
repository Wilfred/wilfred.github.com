--- 
layout: post
title: The Case For Standard ML
tags: 
- Essays
---

If you do a computer science degree at Cambridge, your very first
taste of programming will be in Standard ML. For quite some time this
seemed strange to me, as the merits were never clearly explained. Most
students had not even heard of the language, let alone written any
code in it. At the time, I assumed that its purpose was to give all
the students a level playing field, since no-one seemed to have any
SML experience.

However, an interesting discussion ensued when MIT changed from
teaching Scheme to Python as a first language for its computer
scientists. All of a sudden people were arguing over whether we should
teach computer scientists functional programming and the importance of
being mainstream. To see the various sides being argued read the
discussion
on [Hacker News](http://news.ycombinator.com/item?id=602307), [reddit](http://www.reddit.com/r/programming/comments/8mfw8/chiming_in_on_the_mit_scheme_to_python_switch/)
and [Lambda the Ultimate](http://lambda-the-ultimate.org/node/1840). This prompted me to re-evaluate SML to decide how much
value there is in learning the language.

### The strengths of Standard ML

No language is ideally suited to every task. Since SML is used as a
starting language, you may not have many other tools that you are
familiar with. This sadly highlights the weaknesses more than SML's
strengths. Let us consider situations where SML is a great choice.

SML has a fantastic pattern matching approach to function
definitions. Rather than using switch case statements the syntax is
minimal and elegant. The result of this is that
SML [lends itself to compiler writing](http://flint.cs.yale.edu/cs421/case-for-ml.html). When studying semantics in the second
year of the
course, [code for an interpreter](http://www.cl.cam.ac.uk/teaching/0809/Semantics/) of a toy language is presented in SML and
Java. The SML version includes a pretty print function not in the
version and is still half the total lines of code.

SML is fundamentally a functional language.  First time programmers
will be exposed to (amongst other things) currying, higher order
functions and immutable data structures. This is increasingly relevant
as functional language are often ideal for writing multithreaded
code. A number of popular languages in this area such as F#, OCaml and
Clojure have all been influenced by SML, so an SML programmer will
find these languages much easier to pick up at a later date.

SML is also unusual in having a complete formal specification. For
safety-critical systems or other areas that need formal verification
of code, SML allows developers to prove properties of their
system. This is still a major area of computer science research. A
common alternative to SML is to use a subset of a more mainstream
language's features. The advantage SML has here is that its libraries
are also valid SML and so verifying the library functions are valid
and ensuring the compiler is reliable is a lot easier.

### The importance of evangelism

SML is sorely lacking people who declare its virtues. There are very
few people who blog about the language and very few projects out there
for people to play with. Bloggers offer tips, ideas and inspiration to
fellow programmers of a language. A
single [blog post on SML](http://talideon.com/weblog/2008/03/mosml-pt1.cfm) can be full of helpful information. Only through
reading material produced by curious programmers was I exposed
to [using rlwrap to save history](http://talideon.com/weblog/2008/03/mosml-pt1.cfm#cmt23664) and
the [emacs modes available for SML](http://mlton.org/Emacs).

As for projects, I was informed that SML is used by the military
(presumably due to its formal specification), but this doesn't exactly
produce public-facing projects. The only two applications written in
SML that I am aware of
are [fxp (an XML parser)](http://web.archive.org/web/20141021075111/http://atseidl2.informatik.tu-muenchen.de/~berlea/Fxp/) and [Swerve (a HTTP server)](http://mlton.org/Swerve). Exposure to these projects is a huge encouragement
to students.

Whilst SML is failing to evangelise, Objective Caml (OCaml) is much
more effective at this. It is a derivative language of SML and so the
syntactic differences are
slight [[1]](http://www.mpi-sws.org/~rossberg/sml-vs-ocaml.html)[[2]](http://hyperpolyglot.org/ml). One
consequence of this is that there are plenty of interesting OCaml
projects to experiment with, such
as [P2P clients](http://en.wikipedia.org/wiki/Mldonkey), [internet radio](http://savonet.sourceforge.net/) and even [web frameworks](http://ocsigen.org/). With such a thriving community I would encourage any
SML programmer to explore OCaml.

### Should SML be taught?

Functional programming is important today and won't be disappearing
anytime soon. SML does level the playing field between those who have
programmed before and those who haven't. Sadly many of its tools are
no longer maintained. There is also very little in the way of a
community around the language. However, a good programmer is fluent in
a number of languages with different paradigms and at Cambridge he or
she will be exposed to several. SML programmers can move to OCaml or
other functional programming languages without too much difficulty.

That being said, if Cambridge moves to another functional language
tomorrow I wouldn't consider it a great loss.
