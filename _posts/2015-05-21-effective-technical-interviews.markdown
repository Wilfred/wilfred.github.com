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
multiple job offers, so it's important that they like the role and the
team.

## Avoid 'Big Bang' Questions

A common interview antipattern is to look for a single 'great interview
question'. This all-or-nothing approach leads to questions that are
too big, and candidates are more likely to get stuck or stress.

Instead, break questions into smaller parts. Candidates enter an
interview with no idea what to expect. Warmup questions set
expectations and let you discuss a broader ranger of technologies.

For example, suppose you're interviewing candidates for a frontend web
developer role. You could start with some CSS questions:

> Consider the following HTML:

{% highlight html %}
<div id="article">
    <p>Lorem ipsum <a href="/dolor">dolor</a> sit amet.</p>

    <img src="cat.jpg">

    <p>Ut enim ad minim veniam.</p>

    <a href="/lorem-ipsum">Read more</a>
</div>
{% endhighlight %}

> 1: What does the CSS selector `#article` match?

> 2: What's the difference between `#article a` and `#article > a`?

> 3: Write a CSS selector that only matches the first paragraph
element in a div with an article ID.

These questions are incremental, but there are also opportunities for
experienced candidates to show-off. You can also add open-ended
questions like 'is this good HTML?' to see how candidates think.

(Topics you might discuss here include: using the `alt` attribute on
`img` tags, using classes versus IDs, or the HTML 5 `<article>` tag).

Once you're done with CSS questions, you can move on to other
topics. Having more questions gives you much more flexibility. You
want to distinguish strong developers who are weak in one area from
weak developers.

## Use Standard Questions

Once you have a set of interview questions, evolve them slowly. It's
easy to conclude your questions are too easy if you have some really
great candidates (or vice versa). If you have a standard set of
questions, you have a standard benchmark to measure candidates'
abilities.

Seeing multiple candidates answer the same question can also highlight
poorly worded questions. On several occasions we've changed examples
in our questions to prevent common misunderstandings.

You should be aware that asking the same questions repeatedly will
make them seem easy to you. Make sure, however, that you humour
candidates if they propose weird or novel approaches. We still see new
ways of answering old questions, and it's easy to follow along when
you're very familiar with the problem.

If a candidate does give you a bizarre answer, take the time to
explore their reasoning. Stepping through the code together is often
worthwhile, especially if you suggest an input that exposes some bug
in their implementation.

## Write Actual Code

It is crucial that you ask developer candidates to actually write some
code. They won't have an IDE in the interview, and may not remember
exact arguments for obscure standard library functions, but they
should be able to write working code.

Continuing with the frontend developer example, you'll want to ask
some JavaScript questions.

{% highlight js %}
function throttle(func, waitMs) {
    // ...
}
{% endhighlight %}

> Implement `throttle` so it returns a function that only calls the
> original `func` at most every `waitMs` milliseconds, even if called more
> frequently.

This question also has scope for broader discussions: How do we write
variadic functions in JS? Are there libraries that include `throttle`
functionality? How would you test code like this?

You shouldn't be trying to catch the candidate out: the goal is to
understand how deep their knowledge is in different areas.

## Write Feedback Promptly

Once the interview is over, it's important to have written records of
how it went. Write up some notes soon after the interview.

Be specific. Good feedback says *why* you came to a conclusion about a
candidate. This is especially helpful if interviewers disagree on the
suitability of a candidate.

> John Doe comfortably managed our CSS questions, demonstrating
> knowledge of specificity, the box model and
> pseudo-elements. However, his JS is much weaker: he needed
> significant help to solve our first JS question. He created
> a variable called `default` (a syntax error) and didn't know about
> variable hoisting.
>
> He'd work well alongside our designers, but I don't think he'd be
> a good fit on our developer team.

## Final Impressions

Finally, be welcoming, be supportive and make the most of meeting new
people. Everyone involved should leave the interview having learnt
something.
