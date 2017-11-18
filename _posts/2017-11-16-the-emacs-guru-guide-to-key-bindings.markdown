--- 
layout: post
title: "The Emacs Guru Guide to Key Bindings"
---

The Emacs Guru Test is stated as:

> Imagine that you hold Control and type your name into Emacs. Can you
> describe what will happen?

Emacs shortcuts (known as 'key bindings') can seem ridiculous to
beginners. Some Emacsers even argue you should change them on day one.

There's a method to the madness. In this post, I'll describe the logic
behind the Emacs key bindings. I believe they're worth using.

Memes? Captions?

## Keybindings are Mnemonic

Emacs has a *ton* of keybindings.

    ELISP> (length global-map)
    143

My current Emacs instance has well over a hundred global key
bindings. Since keymaps are nested data structures, this doesn't even
count sequences like `C-h C-h`.

Let's look at some important basic commands:

| Command                  | Key Binding |
| --                       | --          |
| eXecute-extended-command | M-x         |
| Next-line                | C-n         |
| Previous-line            | C-p         |
| Forward-char             | C-f         |
| Backward-car             | C-b         |
| iSearch-forward          | C-s         |

Mnemonics are a really effective way of memorising things. There are
so many tools inside Emacs that there's no other approach that scales.

A great example of this is the Emacs clipboard. A traditional
application has three commands: cut, copy and paste. This assumes that
you only have one item in your clipboard.

Emacs doesn't have this limitation: its clipboard is a linked list (a
ring). You can:

* `kill`: remove text and insert it into the `kill-ring` (like cut)
* `kill-ring-save`: copy the selected text into the `kill-ring` (like
  copy)
* `yank`: insert text from the `kill-ring` (like paste)
* ``yank-pop`: replace the previously yanked text with the next item
  in the kill ring (no equivalent!)
  
This just doesn't fit neatly in the conventional cut/copy/paste key
bindings. The Emacs keybindings (which predate the convention of
`C-x` as cut) fit the Emacs terminology and ensure each command is
conveniently accessible.

(If you have your heart set on `C-x`, `C-c`, `C-v`, there's is CUA
mode. Emacs won't judge you.)

## Organised

Many Emacs movement commands are laid out in a consistent pattern. 

For example, movement by certain amount:

| Command      | Key Binding |
| --           | --          |
| forward-char | C-f         |
| forward-word | M-f         |
| forward-sexp | C-M-f       |

Moving to the end of something:

| Command          | Key Binding |
| --               | --          |
| move-end-of-line | C-e         |
| forward-sentence | M-e         |
| end-of-defun     | C-M-e       |

Deleting characters after the cursor (known as 'point' in Emacs
terminology):

| Command     | Key Binding |
| --          | --          |
| delete-char | C-d         |
| delete-word | M-d         |

Killing text:

| Command       | Key Binding |
| --            | --          |
| kill-line     | C-k         |
| kill-sentence | M-k         |
| kill-sexp     | C-M-k       |

The general pattern here is that `C-whatever` commands tend to be
small, dumb text operations. `M-whatever` commands are larger, and
usually operate on words.

`C-M-whatever` commands are slightly magical. These commands
understand the code they're looking at, and operate on whole
expressions. Emacs uses the term 'sexp' (s-expression), but these
commands usually work in any programming language!

TODO: prefix universally understood

## Room for you

With all these key bindings already defined, what bindings should
you use for your personal favourite commands?

Much like IP addresses `192.168.x.x` is reserved for private use,
Emacs has
[keys that are reserved](https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html) for
user configuration. All the sequences `C-c LETTER`, such as `C-c a`,
are reserved for your usage, as are `<F5>` through to `<F9>`.

## Discoverable

What happens when you press `C-a`? Emacs has a wonderful command
`describe-key` (bound to `C-h k`) that can tell you exactly what
happens when you press a key.

Type `C-h k C-a` and Emacs will show you:



C-h k, view lossage, helpful show all bindings, reminder of
keybindings, ivy shows keybinding

Everything is a keybinding, even normal letters.

## Flexible

Anything can be changed. Emacs tutorial will even update if you change
keybindings

Ultimately, learn help commands, then learn the patterns. Even as your
preferences evolve and you tailor Emacs to you, you will benefit
hugely from organising things into patterns.
