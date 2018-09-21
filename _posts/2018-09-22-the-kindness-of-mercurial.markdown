--- 
layout: post
title: "The Kindness of Mercurial"
---

I've used git extensively, taught other developers to use it, and even
led company migrations to git. I'm very comfortable getting stuff
done, but it's still hard to defend git's defaults.

I've started dabbling with Mercurial, and I've been pleasantly
surprised. It manages to be powerful without the gotchas.

## No Staging Area

```
$ nano whatever.py
$ hg commit -m "fix bug in whatever"
```

Mercurial uses the Subversion model: by default, a commit includes all
the changes you've made. This is delightful for beginners. Creating a
commit is one of the first things you'll do when you start learning a
version control system.

When you're ready, Mercurial still enables you to commit individual
parts of individual files (like `git add -p`). There's even a cute
curses interface to make it easy:

<img src="/assets/hg_commit.png">

## No Branches

When I started using git, I didn't use branches. I was working alone
and already had plenty of new concepts to learn: commits, commit
messages, viewing history, and reverting.

Git pushes you to create branches. If you checkout an old commit, then
create a commit, you end up with dangling commits! Git shows a stern
warning, but you have to come up with a branch name in order to save
your work.

If you don't create a branch, you won't be able to see those commits
later in `git log`, even with `--all`. Git will even eventually
discard them! (Git guru test: when will those commits probably be
discarded?  What setting controls this?)

It's much rarer to create 'bookmarks' in Mercurial. You can create
commits at any point, and they still show up in `hg log`!

```
$ hg log --graph
@  changeset:   11:58c5cebe6230
|  tag:         tip
|  parent:      9:e7f0d0c82be6
|  user:        Wilfred Hughes <me@wilfred.me.uk>
|  date:        Sat Sep 22 00:41:13 2018 +0100
|  summary:     another commit!
|
| o  changeset:   10:086599110661
| |  parent:      8:aa5731f5c22b
| |  user:        Wilfred Hughes <me@wilfred.me.uk>
| |  date:        Sat Sep 22 00:36:42 2018 +0100
| |  summary:     side commit
| |
o |  changeset:   9:e7f0d0c82be6
|/   user:        Wilfred Hughes <me@wilfred.me.uk>
|    date:        Sat Sep 22 00:22:46 2018 +0100
|    summary:     more
|
o  changeset:   8:aa5731f5c22b
|  user:        Wilfred Hughes <me@wilfred.me.uk>
|  date:        Sat Sep 22 00:21:26 2018 +0100
|  summary:     init py
```

This output also shows that Mercurial offers incrementing commit
numbers for local commits. This is another nice affordance for
newcomers.

## Safe History Editing

Mercurial has some wonderful metadata on commits. It has a concept of
[phases](https://www.mercurial-scm.org/wiki/Phases), which
distinguishes 'public' commits from 'draft' commits.

Pushing a commit marks it as public. This prevents you accidentally
editing commits that others depend on.

```
$ hg commit --amend
abort: cannot amend public changesets
(see 'hg help phases' for details)
```

All the Mercurial commands understand phases, so you can safely edit
your unfinished commits without accidentally causing yourself (or
others) pain.

## Closing Thoughts

Git has a ton of tooling available. There's also a range of repository
hosting services, and extensive online resources.

Mercurial has fewer tools. There's a healthy plugin ecosystem which
acts as a testing area for new concepts and interfaces. This leads to
innovation and makes it a joy to use. Give it a try! 😊
