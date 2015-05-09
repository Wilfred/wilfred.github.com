--- 
layout: post
title: "Effective Technical Interviews"
---

## Avoid 'The Interview Question'

It's easy to look for a single great interview question. This 'all or
nothing' approach leads to questions that are too big, and candidates
are likely to get struck and stress.

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

## Write some code

Discussing code is great, but it's important you have some actual
coding questions. You learn a lot about how a candidate thinks and how
comfortable they are with different technologies.

## Side with the candidate

Learn about them, find out a little bit about them. You want them at
their best. You want them to desire to work with you (the best
candidates often have multiple job offers).

Help them relax: don't jump in to questions. Set expectations,
especially with phone interviews (time taken, what you'll chat
about). Don't have more than two people interviewing a candidate at
once. 

## Write feedback immediately afterwards

If you're interviewing several people, it's very important to have
records of how the interview went. Keep this feedback in a standard
place.

Be specific. It's more useful to say 'John Doe didn't show much
experience with JS because his code used reserved words as
variables'. This is concrete, and saying *why* is especially helpful
when interviewers disagree on the suitability of a candidate.
