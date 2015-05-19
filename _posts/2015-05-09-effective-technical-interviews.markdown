--- 
layout: post
title: "Effective Developer Interviews"
---

How do you interview developers effectively? How do you get to know
them, assess their skill level, and make it a positive experience?

I've interviewed over 50 people for different developer roles. Here's
what I've learnt.

## Side with the candidate

The candidate has agreed to meet unfamiliar people, in an unfamiliar
place, and answer questions. You want to do your best to put them at
ease.

Don't jump straight into questions: introduce yourself (and any other
interviewers) and chat a little. Set expectations about what will
happen during the interview (especially important in phone
interviews).  Not only is this the right thing to do, relaxed
candidates are smarter and more fun to spend time with.

You shouldn't have more than two interviewers with a candidate at any
one point.

An interview is a two-way process: candidates are also considering
whether they want to work with you. The best candidates often have
multiple job offers, so it's important that they like the idea of role
you're offering.

## Avoid 'Big Bang' Questions

It's easy to look for a single great interview question. This 'all or
nothing' approach leads to questions that are too big, and candidates
are likely to get stuck and stress.

Instead, break questions into smaller parts. Candidates enter an
interview with no idea what to expect, so warmup questions set
expectations.

Having more questions gives you flexibility with candidates. You might
have some SQL questions, but meet a candidate who clearly hasn't
written much SQL. You've learnt something about their ability, and you
can move on and explore other topics.

Suppose we're interviewing a candidate for a frontend web development
role. We could start with some CSS selector questions:

> Consider the following HTML:

{% highlight html %}
<div class="article">
    <p>Lorem ipsum <a href="/dolor">dolor</a> sit amet.</p>

    <p>Ut enim ad minim veniam.</p>

    <a href="/lorem-ipsum">Read more</a>
</div>
{% endhighlight %}

> 1: What does the CSS selector `.article` match?

> 2: What's the difference between `.article a` and `.article > a`?

> 3: Write a CSS selector that only matches the first paragraph
element in an article div.

These questions are incremental, but there's opportunities for great
candidates to shine. In addition to just answering the question,
experienced CSS developers will use exact terminology (child vs
descendant in question 2) and comment on the bigger picture
(`:first-child` browser support, CSS level 2 vs level 3).

It's also important to have a standard set of questions that you ask
candidates. 

Standardise questions as far as possible. We still see many different
answers to the same question. Be aware that you will find the
questions progressively easier after you've seen them many times over.

If they give an answer that you think is wrong, explore their
reasoning. Step through the code together on a carefully chosen
counterexample.

Standard questions are a blessing and a curse: you have a standard bar
to measure ability. You are very comfortable with the question if the
candidate come up with a novel approach (I'm amazed how often still
this happens). However, beware that you will perceive the question
difficulty to decrease over time.

## Write some code

Discussing code is great, but it's important you have some actual
coding questions. You learn a lot about how a candidate thinks and how
comfortable they are with different technologies.

## Write feedback immediately afterwards

If you're interviewing several people, it's very important to have
records of how the interview went. Keep this feedback in a standard
place.

Be specific. It's more useful to say 'John Doe didn't show much
experience with JS because his code used reserved words as
variables'. This is concrete, and saying *why* is especially helpful
when interviewers disagree on the suitability of a candidate.
