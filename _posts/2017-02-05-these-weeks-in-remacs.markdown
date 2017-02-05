--- 
layout: post
title: "These Weeks in Remacs"
tags:
 - emacs
---

It's been about two weeks since the last Remacs status update, but
development continues apace!

For readers who use Arch Linux, Remacs is now available as a
[remacs-git package on AUR](https://aur.archlinux.org/packages/remacs-git/).

We've seen a lot of work on builds recently. Thanks to a heroic effort
from some OS X developers, Remacs now
[builds on OS X](https://github.com/Wilfred/remacs/issues/38)! A
particular thanks to @pnkfelix who
[wrote a custom allocator](https://github.com/Wilfred/remacs/pull/112)
to ensure `unexec` works with Remacs using
[Rust RFC 1398](https://github.com/rust-lang/rfcs/blob/master/text/1398-kinds-of-allocators.md).

Our CI has improved too. We now run
[almost the entire Emacs test suite](https://github.com/Wilfred/remacs/pull/102)
on Travis, for both 64-bit Linux and OS X. The few failing tests are
being skipped, though in several cases
[they're legitimate upstream issues that we've reported](https://debbugs.gnu.org/cgi/bugreport.cgi?bug=25534).

Work continues on other platforms: we've found that
[Remacs won't compile on 32-bit systems](https://github.com/Wilfred/remacs/issues/95)
due to
[inappropriate use of tuple structs](https://github.com/rust-lang/rust/issues/39394). This
is currently being worked on, but we hope the
[#[repr(transparent)] Rust RFC](https://github.com/rust-lang/rfcs/pull/1758/files?short_path=01fbc76)
will eventually give us a cleaner solution.

Contributors have also been looking at using Rust crates to replace C
code included in Emacs. Work is underway for
[SHA checksums](https://github.com/Wilfred/remacs/issues/80),
[MD5 checksums](https://github.com/Wilfred/remacs/issues/117) and
[generating temporary files](https://github.com/Wilfred/remacs/issues/103).

Finally, Remacs now
[has a CONTRIBUTING.md](https://github.com/Wilfred/remacs/blob/master/CONTRIBUTING.md),
which GitHub shows when creating a new issue or PR. This should make
it easier for new contributors, as it teaches them how to contribute effectively.

If you're interested in contributing, our
[help needed](https://github.com/Wilfred/remacs#help-needed) list
is regularly updated with interesting ways to contribute. We also have
[issues tagged 'Help Wanted'](https://github.com/Wilfred/remacs/labels/help%20wanted)
on GitHub.

Onwards and upwards!
