--- 
layout: post
---

Blogging is a form of writing for the web. So if you're not writing at
all, then you're doing something wrong.

I grew steadily more frustrated with WordPress. Wrestling with the
editor to produce the HTML I wanted. Fiddling with server permissions
so that I just upload one simple image. The worst bit, though, was the
security updates. When I had a desire to write, I would be bothered by
a "please update now" message. For a sporadic blogger, this updating
process took up a significant percentage of total blogging time.

So, I've moved to [Jekyll](https://github.com/mojombo/jekyll), a
static site generator that suits blogging well. I can write posts in
the text editor of my choice, undisturbed. Deploying is trivial: `$
git push`. It even imported all the old WordPress posts.

Jekyll isn't perfect, by any means: there's
[no basic example to get started with](https://github.com/mojombo/jekyll/issues/194),
and
[rendering can silently fail](https://github.com/mojombo/jekyll/issues/353). Then
there's the issue that you can't provide comments on a purely static site.

This brings us to the question of the value of comments. Over the
lifetime of this blog, there have been 3 comments total. That's not
enough to justify the maintenance burden of WordPress. Granted, there
are services like Disqus which offer third-party commenting systems,
but they are inflexible and not open source.

I believe that the solution here is just to build a comments system, and pull in
comments using JavaScript. After all,
[80% of Django developers are writing a blogging engine](http://www.youtube.com/watch?v=i6Fr65PFqfk&t=5m51s),
so a simple comments system will hopefully be more than sufficient.

Indeed, the existence of this post suggests this approach works better
for this lone writer. Stay tuned.
