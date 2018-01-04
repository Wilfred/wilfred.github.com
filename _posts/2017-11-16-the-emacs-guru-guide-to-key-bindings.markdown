--- 
layout: post
title: "The Emacs Guru Guide to Key Bindings"
---

The Emacs Guru Test is:

> Imagine that you hold Control and type your name into Emacs. Can you
> describe what will happen?

Emacs shortcuts (known as 'keybindings') can seem ridiculous to
beginners. Some Emacsers even argue you should change them on day one.

They are wrong. In this post, I'll describe the logic behind the Emacs
keybindings. Not only will be you be closer to passing the guru test,
but you might even find you don't want to change them!

## There Are *How* Many?

Emacs has a *ton* of keybindings.

    ELISP> (length global-map)
    143
    
Emacs is a modal editor, so most keybindings are
mode-specific. However, but my current Emacs instance has well over a
hundred global shortcuts that work everywhere.

(Keymaps are nested data structures, so this actually undercounts. For
example, `C-h C-h` and `C-h f` are not counted separately.)

Even that is is drop in the bucket compared with how many commands we
could define keybindings for.

ELISP> (let ((total 0)) 
  (mapatoms 
   (lambda (sym)
     (when (commandp sym)
       (setq total (1+ total))))) 
  total)
8612

How can we possibly organise all these commands?
    
## Mnemonic Keybindings

Basic commands are often given keybindings based on their name. You'll
encounter all of these important commands during the tutorial.

| Command                      | Key Binding |
| --                           | --          |
| e**X**ecute-extended-command | M-x         |
| **N**ext-line                | C-n         |
| **P**revious-line            | C-p         |
| **F**orward-char             | C-f         |
| **B**ackward-car             | C-b         |
| i**S**earch-forward          | C-s         |

Mnemonics are a really effective way of memorising things.

A common objection at this point is that Emacs keybindings don't match
user's experience. That's unfortunate, but Emacs has too many powerful
tools to fit them all into the shortcuts you've seen before. Emacs
keybindings often predate shortcut conventions you might have learn
from other tools.

The clipboard is great example of this. You're probably used to `C-x`,
`C-c` and `C-v` for cut, copy and paste.

Emacs' clipboard is way more general. You can:

Emacs doesn't have this limitation: its clipboard is a linked list (a
ring). You can:

* `kill`: remove text and insert it into the `kill-ring`. This is like
  cut, but you can do it multiple times and join or multiple.
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

Transposing!

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
