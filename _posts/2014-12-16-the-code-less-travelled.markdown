--- 
layout: post
title: "The Code Less Travelled"
---

Consider the technologies that you haven't used: programming
languages, libraries, frameworks and tools. You've probably formed an
opinion on some of these already. Which of these technologies would
you least like to use? Why?

I made a list recently, and I was horrified by the length and variety
of items on there. It included languages (Scala, Perl, Go), web
frameworks (Spring, Yii), source control systems (CVS, Darcs), and
more besides.

One common reason for me avoiding a technology is **what others say
about it**. For example, some programming language implementors I
respect are critical of Go's design. However, my sphere of influence
is heavily skewed: many of the bloggers/twitter users I read have
similar technology backgrounds to me. It's unlikely that they'll be
the ones articulating what I'm missing out on. In this situation, I
learn the weaknesses before the strengths, and risk never learning the
strengths!

A related issue is **reputation**. Isn't Spring just a framework for huge
ugly enterprise codebases? So I'm told. However, there's a tooling
ecosystem around Spring and maybe the benefits of the framework
outweigh the alleged bad parts. Today, I cannot even describe what Spring
does well, yet it's used by lots of people. It's ridiculous.

It's also easy to dislike technologies that are **similar to those I'm
very familiar with**. Why would you want a new web framework when Django
exists already? I've written a lot of code using Django and I'm very
productive with it. This is a sunk time cost, and it's easy to resent
the additional time required to reach the same level of productivity
with another framework.

I've ended up working on projects that used other web frameworks, and
discovered approaches I would have never seen otherwise. The Pyramid
debug toolbar
[opens an interpreter if you have an unhandled exception](https://twitter.com/davidemoro/status/500413935255060481). Node.js
frameworks often have a superb asset pipeline with auto-reloading that
Django doesn't provide either (by default).

On the other hand, sometimes I dislike technologies because they're
**too different to what I'm familiar with**! For example, Darcs is a VCS
that treats a repository as a partially ordered set of patches. This,
again, is a mental shortcut: sometimes I have to learn a whole new
conceptual model. Until I understand the basic concepts and
terminology, I cannot do anything with the technology.

I'm **not saying that all languages are created equal** or using
badly-designed tools is 'character-building'. If a technology has
actual people doing real projects with it, we must understand the
advantages, preferably from modest first-hand experience, before
drawing conclusions about it. We should be intellectually honest.

I'm also **not saying that technology is a treadmill** where we must keep
learning new tools or risk an obsolete skillset. You can pick
technologies that are sufficiently entrenched (e.g. C, JavaScript)
so experience would still be useful in five years time.

Instead, keep exploring, and be mindful of your blind spots. Somewhere
on the web, there will always be an article saying $LANGUAGE is bad,
saving you from learning it and expanding your horzons. I write a lot
of Python, a language which has limited multi-threading support. I
recently realised that I didn't appreciate multi-threading because
it's uncommon in my go-to language!

Stay open minded. No language has all the good ideas. Don't miss out
by only being a `$LANGUAGE` user. For 2015, I'm resolving to do some
side projects using the tools in my list.
