--- 
layout: post
title: "This Week in Remacs"
tags:
 - emacs
---

Wow, what a start to the
[Remacs project](https://github.com/Wilfred/remacs)! We've had a ton
of media attention:
[Soylent News](https://soylentnews.org/article.pl?sid=17/01/13/0119220)
[Heise](https://www.heise.de/developer/meldung/Remacs-eine-Emacs-Implementierung-mit-Rust-3594327.html),
[Golem.de](http://www.golem.de/news/remacs-rust-basis-fuer-editor-emacs-vorgestellt-1701-125544.html),
[Linux Magazin](http://www.linux-magazin.de/NEWS/Remacs-Rust-Basis-fuer-Emacs),
[Phoronix](https://www.phoronix.com/scan.php?page=news_item&px=Remacs-Rust-Emacs)
and [Hacker News](https://news.ycombinator.com/item?id=13378247).

This has brought a number of new contributors on board, and we've
landed lots of features this week:

We've **added 28 primitive elisp functions** written in Rust. These
included floating point arithmetic, type checks, basic list
manipulation and even some string creation (which requires
allocation).

The code is now much **more idiomatic Rust**, and you can write things
like `LispObject::from_bool(true)` and it does exactly what you'd
expect.

Our **Makefile has vastly improved**. Cargo is now driven from Make, and
we have users on both Linux and BSD. Windows and OS X are both being
actively worked on.

Remacs is now **based on Emacs' master branch**. All the latest goodies in
upstream Emacs are available in Remacs too.

Finally, our **branding has improved**. The welcome screen now
explicitly says Remacs, and the build process produces a `remacs`
binary. This means you can **install Remacs alongside your current
Emacs instance**!

Huge thanks to our contributors:

* 0xAX
* Arseniy Zaostrovnykh
* c-nixon
* CrLF0710
* Daroc Alden
* David DeSimone
* David Zmick
* dk87
* Felix S Klock II
* Femi Agbabiaka
* Jean Pierre Dudey
* Liang Ying-Ruei
* Martin Feckie
* Roger Marlow
* Sharif Nassar
* Victor Hugo Borja
* William Orr

If you're interested in contributing, we have a
[tutorial to get you started](https://github.com/Wilfred/remacs#porting-elisp-primitive-functions-walkthrough)
and a
[list of tasks you might like to start with](https://github.com/Wilfred/remacs#help-needed). All
PRs are reviewed, so we can help you if you get stuck!
