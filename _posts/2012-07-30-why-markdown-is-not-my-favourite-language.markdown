--- 
layout: post
title: "Why Markdown Is Not My Favourite Language"
---

Markdown is fast approaching ubiquity. In addition to massive sites
such as GitHub and Stack Overflow using it, it's a default choice for
many developers for everything from comments to wikis.

I've built two bespoke wiki sites using Markdown, run a blog in
Markdown, and used a variety of Markdown sites. I've concluded that
Markdown is ill-suited for wikis, and often a poor choice in
general. Let me count the ways.

### You don't want normal markdown

Out of the box, a standards-compliant markdown parser is confusing,
insecure and limited in functionality. The largest users of markdown,
such as GitHub, all end up providing their own modified version. This
produces fragmentation and prevents reliable, bug-free implementations
gaining traction.

### Security

Markdown allows embedded HTML. Since this is part of the markdown
standard, simply enabling markdown rendering on your site will
introduce XSS bugs. Some implementations provide a flag to disable
embedded arbitrary HTML, whereas others require you to use a separate
HTML sanitisation library.

Since Markdown does not require implementations to offer this flag,
too many developers are not even aware there is an issue. Using a
templating language with XSS protection does not help, since the
rendered Markdown must be marked as not needing escaping.

### Gotchas

I built a wiki for a software development company where even the
developers would get caught out by the syntax. The Markdown syntax is
surprising in a number of ways.

One of the most common issues is mixing bulleted lists (which can
start with `*`) and code snippets (which start with four spaces). A
user might write:

    * A bullet
    * Another bullet
    
        System.out.println("hello world!");
        
To their surprise, the code snippet would get treated as normal text,
part of the previous bullet point. This is an unfortunate consequence
of Markdown supporting multiple paragraphs in a single bullet:

    * First bullet
    
        Another paragraph in the first bullet
        
    * Second bullet
    
        Another paragraph in the second bullet
        
[The standard is actually ambiguous on this point](http://meta.stackoverflow.com/a/99637),
although in practice all implementation behave as I describe. The user
is forced to either indent by eight spaces, producing code that is
part of the preceding bullet:

    * Bullet point
    
            foo(); // indented by eight spaces
            
Or forced to use an invisible marker, such as `&nbsp;` or a comment:

    * Bullet point
    
    <!-- don't remove this comment -->
    
        foo();
        
More invisible syntax is required if the user wants a single line
break. A simple newline is ignored by Markdown:

    This is all on
    a single line.
    
The only solutions within standard Markdown is to insert a literal
`<br>`, or to use two trailing spaces:

    This is one line ending with two spaces  
    So this is on another line.
    
This makes editing Markdown files unreasonably difficult. Many
well-known sites using Markdown convert a single newline to a
`<br>`. This fixes the immediate problem, but the resulting
fragmentation means that users' experience with Markdown does not
necessarily carry over to other sites.

My favourite gotcha with Markdown is the numbering syntax. In many
other text markup formats, a specific character is used for numbering,
such as `#`. With plaintext, the user must manually increment numbers
themselves. Markdown supports the worst of both worlds, by recognising
and renumbering lists. I saw one user write the following:

    1. This is my first point.
    
    Since I have a lot to say about my first point, I wrote some more
    text here.
    
    2. This is my second point.
    
    I have a lot to say about my second point too, which I am putting
    here.
    
    3. This is my third point.

Markdown renumbers this, so every point is number one. This is not
what a simple reading of the input would lead you to expect.

### Limited features

Despite Markdown's ubiquity, it is missing several features that my
users frequently wanted. It does not support tables, unless the user
writes raw embedded HTML. You cannot automatically generate a table of
contents based on the headings on piece of Markdown source. Text that
is obviously a link is not automatically converted to a link, forcing
a rather heavyweight syntax:

    Our primary landing page is [http://www.example.com](http://www.example.com)
    
Stack Overflow supports an alternative syntax for this use case:

    Our primary landing page is <http://www.example.com>
    
Again, this is not knowledge that the user can take with them to other
Markdown based sites.

There's also no support for labelling the language used in code
snippets, making highlighting less convenient. There are good tools
(for example [this](http://softwaremaniacs.org/soft/highlight/en/))
which autodetect the language, but they are unreliable for short code
snippets.

Several Markdown implementations offer plugins for some of these
missing features. Sadly, some implementations do not support plugins
at all, and most do not have plugins for all the features I have
listed here. I have also been disappointed with the bugginess of
several plugins I tried, as they hadn't though about nesting
correctly. For example, a user wants their code snippets to be rendered
as written, even if the syntax overlaps with table plugin's syntax.

### Evolution

The Markdown specification is essentially unchanged from 2004. There's
no standard test suite, no updates to resolve ambiguities, and no work
being done to standardise the syntax for these language
extensions. This would greatly benefit the language today, and make it
easier to use in the future.

### Alternative 1: ReStructuredText

ReStructuredText (ReST) is a popular text markup developed by the
Python community. It provides many more features than Markdown, in a
well specified language. ReST replaced an earlier language called
StructuredText, but suffers somewhat from the 'second-system
effect'. Whilst it learnt many lessons from its predecessor, it is
a big, complex language with few implementations.

ReST requires more learning, a monospaced font, and 'recompiling'. The
Python community replaced LaTeX with ReST for their documentation, so
these requirements seem straightforward next to learning TeX and
LaTeX. However, ReST is less suited for a simple website where the user is
writing in a text box. For example, the following syntax will give an error:

    Heading
    ------
    
This is because a heading must have an underline that is at least as
long as the text. This is fine when using a dedicated text editor, but
a pain in a simple browser text editor.

When the syntax is incorrect, ReST shows an error. This is reminiscent
of LaTeX where you must fix all your syntax errors before it will
render your document. With ReST, these errors are often just shown
inline with the rendered HTML. For websites such as wikis,
[Postel's law](http://en.wikipedia.org/wiki/Robustness_principle) ('be
permissive in what you accept') is much more suitable. Rather than an
'edit, view errors, edit, save' paradigm, a better approach is to use
a JavaScript renderer so the user can see a live preview of the
output.

Sadly, ReST has no JavaScript implementation. Integrating the major
implementation, docutils, is a little more involved than most Markdown
implementations ("no, I don't want the first two headings to have
their level ignored and used as page title/page subtitle").

Whilst ReST has far fewer gotchas, the syntax is more heavyweight and
complex. For example, the syntax for headings:

    Main heading
    ============
    
    A subheading
    ------------
    
This appears to be using `=` to mark top-level headings, but ReST
treats the first heading as top-level *regardless of which underline
character you use*. Furthermore, ReST often requires two characters
where Markdown would require only one -- for example two backticks for inline
code, two colons for code blocks:

    Call the ``foo`` function::
    
        foo(1, 2);
        
Finally, ReST syntax for creating links is very awkward. Inline links
require a heavyweight syntax:

    See our `home page <http://www.example.com>`_
    
For many uses of online text markup, this is too common a use case to
require so many characters. Links are a fundamental part of HTML.

### Alternative 2: Creole (recommended)

None of the largest wiki engines (MediaWiki, MoinMoin, etc) use
Markdown. Most provide their own markup language, but the syntax
between them is usually very similar. However, their syntax is not
well specified, and the only existing renderer is an inseparable part
of the engine. (This is a common problem for people seeking to do
natural language processing of Wikipedia.)

[Creole](http://wikicreole.org/) is a project to find a common syntax
between these different wiki engines. They have produced a
comprehensive specification, and many implementations
exist. Importantly, there are JavaScript implementations, making it
straightforward to build a real-time preview. This is difficult in
Markdown because any plugins or non-standard modifications must be
replicated in both the server-side code and client-side preview
rendering.

The Creole specification is evolving, adding new features and fixing
problems as they arrive. The syntax is lightweight, tried and tested
in a large number of environments. Common use cases are simple. The
syntax is a best-of-breed result, avoiding the mistakes made in the
engines which first implemented a wiki syntax.

Whilst the syntax supports fewer features than ReST, you'd be hard
pressed to find things you will miss in practice: you get basic
formatting, links, headings, code snippets (though no way to specify
language), tables, and images. This more than enough for the vast
majority of use cases, without plugins.

Next time you're looking for a simple markup to HTML solution, try Creole.
