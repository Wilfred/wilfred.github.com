--- 
layout: post
title: "LLVM Developer Meeting 2016"
---

I had the pleasure of attending the LLVM Developer Meeting this
year. Too much happened to describe it all, so I'll just give you the
highlights.

There were many great talks on a range of topics: tools that were
ready for primetime, practical techniques, blue sky research, and more
besides. Some of my favourites were:

There was an Intel talk on optimising for code size on x86. This was a
classic juicy details talk, showing how different instruction
encodings could save precious bytes. Every technique was clearly
described along with measurements of space savings.

Tobias Grosser (the lead developer of Polly) and Johannes Doerfert
gave a fantastic tutorial on Polly. 'Polyhedral loop modelling' is an
intimidating term when you first encounter it, but their explanation
was clear and effective. They also showed how to derive simple loop
optimisations like loop interchange in this system and the more
sophisticated optimisations they've developed on top.

Rui Ueyama gave a talk on LLD, which has recently added ELF
support. He gave a very accessible overview of linking algorithms and
their semantics/performance tradeoffs. Linkers are often black boxes
ignored by developers, so I enjoyed learning the implementation
details of the LLVM linker.

If you missed any of the talks, they were recorded. They should be
uploaded to the
[LLVM youtube account](https://www.youtube.com/channel/UCv2_41bSAa5Y_8BacJUZfjQ)
(uploads are usually announced on the
[llvm-devmeeting mailing list](http://lists.llvm.org/mailman/listinfo/llvm-devmeeting)).

In addition to the talks, there were plenty of opportunities to mingle
with people from different parts of the LLVM ecosystem. I met
academics, compiler engineers from companies large and small, and even
had dinner with the PS4 compiler team.

It was well worth attending, and I'd recommend it to anyone who is
interested in compiler development. I hope to see you there next year!
