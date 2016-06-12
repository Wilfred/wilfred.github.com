--- 
layout: post
title: "Hypermedia: How the WWW fell short"
---

Whenever there's a revolution in computing, there's a period of wild
experimentation. Ideas are explored, prototypes are built, and
manifestos are written.

The final outcome does not always achieve the great ambition of the
first prototypes. Not everything works well in practice. Some features
prove difficult to build, are punted to v2, and never get shipped.

The World Wide Web, with its **HyperText** Markup Language, was
heavily influenced by the ideas of hypermedia. The idea of hyperlinks &mdash;
cross-referencing documents &mdash; was radical. There were even
[court cases disputing the right to link to content](https://en.wikipedia.org/wiki/Copyright_aspects_of_hyperlinking_and_framing#History_of_copyright_litigation_in_field).

The original concept of
[hypertext](https://en.wikipedia.org/wiki/Hypertext) was much more
ambitious. Early designs, such as the
[memex](https://en.wikipedia.org/wiki/Memex) and
[Project Xanadu](https://en.wikipedia.org/wiki/Project_Xanadu), had a
richer feature set.

Here's the web they envisaged.

## Back links

Today's hyperlinks are one-way: when you visit a webpage, you do not
know what links point to it. You can only see what links start from
it.

It's useful information, enabling the reader to discover related
pages. Indeed, the basic principle of
[Google's PageRank](https://en.wikipedia.org/wiki/PageRank) is founded
on crawling the web to find back links. 

<figure>
<img src="/assets/what_links_here.png">
<figcaption>
Wikipedia: <a
href="https://en.wikipedia.org/wiki/Special:WhatLinksHere/PageRank">Pages
that link to "PageRank"</a>
</figcaption>
</figure>

Bidirectional links are alive and well in Wikipedia, which provides a
'What links here' feature to discover related topics.

## Transclusion

Today's web cannot include one page in another. You could
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
highlights can <a href="http://genius.com/511238">link to ranges of text</a>
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

This is another example where wikis come closer to the traditional
idea of
hypermedia. [https://en.wikipedia.org/wiki/Hypertext](https://en.wikipedia.org/wiki/Hypertext)
links to the current version of the hypertext article on Wikipedia, whereas
[https://en.wikipedia.org/w/index.php?title=Hypertext&oldid=722248276](https://en.wikipedia.org/w/index.php?title=Hypertext&oldid=722248276)
explicitly links to the version at time of writing, regardless of
future changes.

## Looking Forward

HTML is over 20 years old. We author web pages in this medium, but its
abilities and limitations
[shape our content](https://en.wikipedia.org/wiki/The_medium_is_the_message). We
need to consider what hypermedia can be and design our platforms to
make the best use of our content.
