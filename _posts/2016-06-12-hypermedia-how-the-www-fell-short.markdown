--- 
layout: post
title: "Hypermedia: How the WWW fell short"
---

Whenever there's a revolution computing, there's a period of wild
experimentation. Ideas are explored, prototypes are built, and
manifestos are written.

The final outcome does not always achieve the great ambition of the
first prototypes. Not everything works well in practice. Some features
prove difficult to build, are punted to v2, and never get shipped.

The World Wide Web, with its **HyperText** Markup Language, was
heavily influenced by the ideas of hypermedia. The idea of hyperlinks,
cross-referencing documents, was radical.

Is it copyright infringement to link to someone else's web page? The
practice is widely accepted now, but only
[after a series of court cases contesting this](https://en.wikipedia.org/wiki/Copyright_aspects_of_hyperlinking_and_framing#History_of_copyright_litigation_in_field). It
was not until 2014 that the European Court of Justice made a binding
ruling on linking being acceptable.

The original concept of
[hypertext](https://en.wikipedia.org/wiki/Hypertext), particularly the
[memex](https://en.wikipedia.org/wiki/Memex) and
[Project Xanadu](https://en.wikipedia.org/wiki/Project_Xanadu), were
much more ambitious.

Here's the web they envisaged.

## Back links

Today's hyperlinks are one-way: when you visit a webpage, you do not
know what links point to it. You can only see what links start from
it.

The basic principle of
[Google's PageRank](https://en.wikipedia.org/wiki/PageRank) is founded
on crawling the entire web to find back links.

Bidirectional links are alive and well in Wikipedia, which provides a
'What links here' feature to discover related topics.

<figure>
<img src="/assets/what_links_here_small.png">
<figcaption>
Wikipedia: <a
href="https://en.wikipedia.org/wiki/Special:WhatLinksHere/PageRank">Pages
that link to "PageRank"</a>
</figcaption>
</figure>

## Transclusion

It's not possible to include one web page in another. You could
manaully scrape the HTML and paste it in, but then you miss out any
future updates.

HTML does provide the `<iframe>` tag, but the embedded content does
not flow into the original page. It's clunky and rarely used in
practice.

Twitter, with its many constraints, does provide a transclusion
facility.

<blockquote class="twitter-tweet" data-lang="en"><p lang="en" dir="ltr">Blogged: Effortless Major Mode Development <a href="https://t.co/e7Wm4Im0SL">https://t.co/e7Wm4Im0SL</a> <a href="https://twitter.com/hashtag/emacs?src=hash">#emacs</a></p>&mdash; Wilfred Hughes (@_wilfredh) <a href="https://twitter.com/_wilfredh/status/725808793309831168">April 28, 2016</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

The above embedded tweet contains the current retweet and like counts,
without me updating my blog post.

Twitter takes this idea further in its clients. If a tweet contains a
hyperlink, it includes a preview -- another form of transclusion!

<figure>
<img src="/assets/tweet_embed_url.png">
<figcaption>
<a
href="https://twitter.com/_wilfredh/status/738107117140344832">Tweet</a>
with embedded webpage
</figcaption>
</figure>

Twitter calls this functionality
[Twitter Cards](https://dev.twitter.com/cards/overview). For short
content, such as other tweets, the preview includes the entire content.

<a name="unique-ids"></a>

## Unique IDs

Web pages are identified by URLs, which are usually unique. Despite
W3C
[pleas not to break URLs](https://www.w3.org/Provider/Style/URI.html),
link rot is [a very common problem](https://weblock.io/report?id=all).

Link rot can prevented by adding another level of
indirection. [PURLs](https://purl.oclc.org/docs/) (Persistent URLs)
allow users to refer to an URL which they can update if the underlying
URL changes.

For example, Keith Shafer (one of the original PURL developers), has a
personal web page at
[purl.oclc.org/keith/home](http://purl.oclc.org/keith/home). This
originally linked to www.oclc.org:5046/~shafer, which no longer
resolves. However, the PURL has been updated, so visitors are now
redirected to
[https://docs.google.com/document/d/1tnDck.../](https://docs.google.com/document/d/1tnDck5nVkk6vTVayobIO9cpvyEh6lfNTIcBp8_uDEmg/).

A common form of link rot is page
renaming. [Stack Overflow](http://stackoverflow.com/), a Q&A site,
includes question titles in URLs. These are often edited to clarify
the question. Stack Overflow embeds unique question IDs in its URLs to ensure
old links work.

Thus
http://stackoverflow.com/questions/37089768/does-pharo-provide-tail-call-optimisation
is the canonical URL, but
http://stackoverflow.com/questions/37089768/foobar still works.

## Section links

URLs in today's web can also reference named HTML tags. For
example,
[http://wilfred.me.uk/blog/2016/06/12/hypermedia-how-the-www-fell-short/#unique-ids](/blog/2016/06/12/hypermedia-how-the-www-fell-short/#unique-ids)
directly links to this section of this blog post. However, this
requires co-operation from the author: they must provide named
anchors, which cannot change in future.

<figure>
<img src="/assets/github_anchor.png">
<figcaption>
<a
href="https://github.com/wilfred/bfc#usage">GitHub
project README</a>
with discoverable section links
</figcaption>
</figure>

A common UI pattern is provide discoverable links on headings in web
pages. This assumes that headings never change or repeat in a
document, but fits the common use case.

<figure>
<img src="/assets/genius_highlight.png">
<figcaption>
<a
href="http://genius.com/">Genius</a>
highlights <a href="http://genius.com/511238">can link to ranges of text</a>
</figcaption>
</figure>

The other major limitation of section links is that they cannot
reference a range of tags on a page. [Genius](http://genius.com/) (a
website offering song lyric interpretations) is one of very few
websites that allow users to link to arbitrary sections of a page.

## History

Finally, early hypermedia designs kept historical versions of
content. You could link to old versions of content if desired.

We don't have this in today's web. There's the
[Wayback Machine](https://archive.org/web/), which periodically
snapshots many websites. For high-profile online news,
[NewsDiffs](http://newsdiffs.org/) regularly snapshots stories to see
how articles are edited over time.

Again, this is an example where wikis come closer to the traditional
idea of hypermedia. 
