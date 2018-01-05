--- 
layout: post
title: "The Emacs Guru Guide to Key Bindings"
---

> Imagine that you hold Control and type your name into Emacs. Can you
> describe what will happen?
>
> -- The 'Emacs Guru Test'

Emacs shortcuts (known as 'keybindings') can seem ridiculous to
beginners. Some Emacsers even argue you should change them on day one.

They are wrong. In this post, I'll describe the logic behind the Emacs
    keybindings. Not only will be you be closer to passing the guru test,
but you might even find you like some of the defaults!

## There Are *How* Many?

Emacs has a *ton* of keybindings.

    ELISP> (length global-map)
    143
    
Emacs is a modal editor, so most keybindings are
mode-specific. However, but my current Emacs instance has well over a
hundred global shortcuts that work everywhere.

(Keymaps are nested data structures, so this actually undercounts! For
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
encounter all of these important commands in the Emacs tutorial.

| Command                      | Key Binding |
| --                           | --          |
| e**X**ecute-extended-command | M-x         |
| **N**ext-line                | C-n         |
| **P**revious-line            | C-p         |
| **F**orward-char             | C-f         |
| **B**ackward-car             | C-b         |
| i**S**earch-forward          | C-s         |

Mnemonics are a really effective way of memorising things. If you can
remember the name of the command, you can probably remember the
keybinding too.

## Organised Keybindings

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

Transposing, which swaps text either side of the cursor:

| Command         | Key Binding |
| --              | --          |
| transpose-chars | C-t         |
| transpose-words | M-t         |
| transpose-sexps | C-M-t       |


Killing text:

| Command       | Key Binding |
| --            | --          |
| kill-line     | C-k         |
| kill-sentence | M-k         |
| kill-sexp     | C-M-k       |

Have you spotted the pattern?

The pattern here is that `C-whatever` commands are usually
small, dumb text operations. `M-whatever` commands are larger, and
usually operate on words.

`C-M-whatever` commands are slightly magical. These commands
understand the code they're looking at, and operate on whole
expressions. Emacs uses the term 'sexp' (s-expression), but these
commands usually work in any programming language!

## Discovering Keybindings

What happens when you press `C-a`? Emacs can tell you. `C-h k C-a`
will show you exactly what command is run.

You can also do this backwards. If Emacs has done something
unexpected, `C-h l` will reveal what the command was, and exactly
which keys triggered it.

Screenshots.

## Room for Emacs

Why are Emacs keybindings different from conventional shorcuts? Why
doesn't `C-c` copy text to the clipboard, like many other programs?

Emacs uses mnemonics for its clipboard commands: you 'kill' and 'yank'
text, so the keybindings are are `C-k` and `C-y`. If you really want,
you can use
[cua-mode](https://www.gnu.org/software/emacs/manual/html_node/emacs/CUA-Bindings.html) so
`C-x` acts as you expect.

The problem is that Emacs commands are too versatile, too general to
fit in the usual `C-x`, `C-c`, `C-v`. Emacs has *four* clipboard
commands:

1. `kill`: remove text and insert it into the `kill-ring`. This is like
  clipboard cut, but you can do it multiple times and Emacs will *remember
  every item in your clipboard*.
2. `kill-ring-save`: copy the selected text into the `kill-ring`. This
  is like clipboard copy, but you can also do this multiple times.
3. `yank`: insert text from the `kill-ring`. This is like clipboard paste.
4. `yank-pop`: replace the previously yanked text with the next item
  in the kill ring. There is no equivalent in a single-item clipboard!

The generality of Emacs means that it's hard to find a keybinding for
everything. Keybindings tend to be slightly longer as a result:
opening a file is `C-x C-f`, an additional keystroke over the `C-o` of
other programs.

## Room for you

With all these key bindings already defined, what bindings should
you use for your personal favourite commands?

Much like IP addresses `192.168.x.x` is reserved for private use,
Emacs has
[keys that are reserved](https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html) for
user configuration. All the sequences `C-c LETTER`, such as `C-c a`,
are reserved for your usage, as are `<F5>` through to `<F9>`.

## You Make The Rules

This doesn't mean that you should never modify keybindings. Emacsers
create weird and wonderful (evil, hydra, god-mode) ways of mapping
keys all the time.

Emacs will even try to accommodate this. If you open the tutorial
after changing a basic keybinding, it will try to suggest the correct
keys!

The secret to mastering Emacs is to remember everything is
self-documenting. Learn the help commands to find out which commands
are set up by default. Consider *following* the existing patterns when
you define new commands. `org-mode`, for example, redefines `C-M-t` to
transpose org elements.

Once you understand the patterns, you'll know when to follow and when
to break them, and you'll be much closer to passing that guru test.
