--- 
layout: post
title: "The Code Less Travelled"
---

Consider all the technologies that you haven't used. Of those, which would
you really rather not use? Why?

I made a list recently, and it had a *lot* of items. There were
languages (Scala, Perl, Go), web frameworks (Spring, Yii), source
control systems (CVS, Darcs), and more besides.

I'm actually concerned by this. It's very hard to form a well-informed
view if you've never even played with something. Once I had my list, I
grouped the items according to the reason I didn't want to use them.

My most common reason was **what others say
about it**. For example, some programming language implementers that I
respect are critical of Go's design. However, my sphere of influence
is heavily skewed: many of the bloggers/twitter users I read have
similar technology backgrounds. It's unlikely that they'll be
the ones able to say what I'm missing out on! I spent some time
looking at Go, and it does some very impressive tooling (I'm not aware
of any equivalent of `go fix` anywhere else).

A related reason is **reputation**. Isn't Spring just a framework for huge
ugly enterprise codebases? So I'm told. However, many people use it,
many projects have been created with it, and I am totally unable to
say what Spring's advantages are. It's a blind spot.

I find it easy to dislike technologies that are **similar to those I'm
very familiar with**. Why would I want a new web framework when Django
exists already? I've written a lot of code using Django and I'm very
productive with it. This is a sunk time cost, and it's easy to resent
the additional time required to reach the same skill level with
something else. As a result, it's hard to be open-minded and fair to
new tools.

I've been forced to use other web frameworks when working on
established projects. I've often found things that my Django projects
lacked. For example, the Pyramid debug toolbar
[opens an interpreter if you have an unhandled exception](https://twitter.com/davidemoro/status/500413935255060481). JavaScript
frameworks often have great asset pipelines with auto-reloading
(which Django doesn't provide by default).

On the other hand, sometimes I dislike technologies because they're
**too different to what I'm familiar with**! For example, Darcs is a VCS
that treats a repository as a partially ordered set of patches. I'm
forced to learn the new conceptual models before I can understand the
documentation and get things done. This effort requirement often feeds
into an ill-informed dislike.

Note that I'm **not saying that all languages are created equal** or
using badly-designed tools is 'character-building'. If a technology
has actual people doing real projects with it, we must *understand the
advantages*, preferably from modest first-hand experience, before
drawing conclusions. We must strive to be intellectually honest.

I'm also **not saying that technology is a treadmill** where we must keep
learning new tools or risk an obsolete skillset. You can pick
technologies that are sufficiently entrenched (e.g. C, JavaScript)
so your experience will still be useful in five years time.

Instead, we should keep exploring, and be mindful of our blind
spots. Somewhere on the web, there will always be an article saying
$LANGUAGE is bad, giving us an excuse to miss that learning
opportunity.

Stay open minded. No language has all the good ideas. Don't miss out
by only being a $LANGUAGE person. For 2015, I'm resolving to do some
side projects using the tools in my dislike list.
