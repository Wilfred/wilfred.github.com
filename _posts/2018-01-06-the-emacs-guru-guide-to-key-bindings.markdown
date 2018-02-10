--- 
layout: post
title: "The Emacs Guru Guide to Key Bindings"
tags:
 - emacs
---

> Imagine that you hold Control and type your name into Emacs. Can you
> describe what will happen?
>
> -- The 'Emacs Guru Test'

Emacs shortcuts (known as 'key bindings') can seem ridiculous to
beginners. Some Emacs users even argue you should change them as soon
as you start using Emacs.

They are wrong. In this post, I'll describe the logic behind the Emacs
key bindings. Not only will you be closer to passing the guru test,
but you might even find you like some of the defaults!

## There Are *How* Many?

Emacs has a *ton* of key bindings.

{% highlight common-lisp %}
ELISP> (length global-map)
143
{% endhighlight %}
    
Emacs is a modal editor, so most key bindings are
mode-specific. However, my current Emacs instance has well over a
hundred global shortcuts that work everywhere.

(Keymaps are nested data structures, so this actually undercounts! For
example, `C-h C-h` and `C-h f` are not counted separately.)

Even that is a drop in the bucket compared with how many commands we
could define key bindings for.

{% highlight common-lisp %}
ELISP> (let ((total 0))
  (mapatoms
   (lambda (sym)
     (when (commandp sym)
       (setq total (1+ total)))))
  total)
8612
{% endhighlight %}
    
How can we possibly organise all these commands?
    
## Mnemonic Key Bindings

Basic commands are often given key bindings based on their name. You'll
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
key binding too.

## Organised Key Bindings

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

## Discovering Key Bindings

What happens when you press `C-a`? Emacs can tell you. `C-h k C-a`
will show you exactly what command is run.

<img src="/assets/describe_key.png">

If you use a command without its key binding, Emacs will helpfully
remind you there's a shortcut available.

<img src="/assets/emacs_hint.png">

You can even do this backwards! If Emacs has done something neat or
unexpected, you might wonder what command ran. `C-h l` will reveal
what the command was, and exactly which keys triggered it.

<img src="/assets/view_lossage.png">

## Room For Emacs

Why are Emacs key bindings different from conventional shortcuts? Why
doesn't `C-c` copy text to the clipboard, like many other programs?

Emacs uses mnemonics for its clipboard commands: you 'kill' and 'yank'
text, so the key bindings are are `C-k` and `C-y`. If you really want,
you can use
[cua-mode](https://www.gnu.org/software/emacs/manual/html_node/emacs/CUA-Bindings.html) so
`C-x` acts as you expect.

The problem is that Emacs commands are too versatile, too general to
fit in the usual `C-x`, `C-c`, `C-v`. Emacs has *four* clipboard
commands:

1. `kill`: remove text and insert it into the kill-ring. This is like
  clipboard cut, but you can do it multiple times and Emacs will
  *remember every item in your clipboard*.
2. `kill-ring-save`: copy the selected text into the kill-ring. This
  is like clipboard copy, but you can also do this multiple times.
3. `yank`: insert text from the kill-ring. This is like clipboard
   paste.
4. `yank-pop`: replace the previously yanked text with the next item
  in the kill ring. There is no equivalent in a single-item clipboard!

The generality of Emacs means that it's hard to find a key binding for
everything. Key bindings tend to be slightly longer as a result:
opening a file is `C-x C-f`, an additional keystroke over the `C-o` of
other programs.

## Room For You

With all these key bindings already defined, what bindings should
you use for your personal favourite commands?

Much like IP addresses `192.168.x.x` is reserved for private use,
Emacs has
[keys that are reserved](https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html) for
user configuration. All the sequences `C-c LETTER`, such as `C-c a`,
are reserved for your usage, as are `<F5>` through to `<F9>`.

For example, if you find yourself using `imenu` a lot, you might bind
`C-c i`:

{% highlight common-lisp %}
(global-set-key (kbd "C-c i") #'imenu)
{% endhighlight %}

## You Make The Rules

This doesn't mean that you should never modify key bindings. Emacsers
create
[weird](https://github.com/chrisdone/god-mode) [and](http://melpa.milkbox.net/#/key-chord) [wonderful](https://github.com/abo-abo/hydra) ways
of mapping keys all the time.

Emacs will even try to accommodate this. If you open the tutorial
after changing a basic key binding, it will update accordingly!

<img src="/assets/emacs_tutorial.png">

The secret to mastering Emacs is to remember everything is
self-documenting. Learn the help commands to find out which commands
have default key bindings. Consider *following* the existing patterns
when you define new key bindings or override existing
ones. `org-mode`, for example, redefines `C-M-t` to transpose org
elements.

Once you understand the patterns, you'll know when to follow and when
to break them. You'll also be much closer to passing that guru test!
