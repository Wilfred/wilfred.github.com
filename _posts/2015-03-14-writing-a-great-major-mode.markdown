--- 
layout: post
title: "Writing a Great Major Mode"
tags:
 - emacs
---

Writing a major mode is a rite of passage for elisp hackers. Sooner or
later, you will find a programming language or configuration format
that is too new or obscure to have Emacs support.

You decide to roll up your sleeves and plug this hole in the Emacs
ecosystem. At this point, you may start to wonder, **what makes a great
major mode**?

## It's a labour of love

It doesn't take much effort to write a basic major mode. However,
there is infinite scope for polish. Whilst Emacs has supported
programming in C since its inception, even
[in 2015 it has been improved](https://github.com/emacs-mirror/emacs/commits/master/lisp/progmodes/cc-mode.el)!

## 1: A basic syntax table

Suppose you wanted to write a major mode for JS. First, you'd

    (defconst my-js-mode-syntax-table
      (let ((table (make-syntax-table)))
        ;; Highlight strings
        (modify-syntax-entry ?' "\"" table)
        (modify-syntax-entry ?\" "\"" table)

        ;; // is a two character comment sequence, both the first and second character.
        (modify-syntax-entry ?/ ". 12" table)
        ;; and \n ends a comment
        (modify-syntax-entry ?\n ">" table)
        table))

    (defconst my-js-mode-keywords-regex
      (regexp-opt '("function" "for")))

    (define-derived-mode my-js-mode prog-mode "Simple JS Mode"
      :syntax-table my-js-mode-syntax-table
      (font-lock-fontify-buffer))

## 2: Keywords

## 3: Full syntax highlighting

## 4: Clever syntax highlighting

## 5: Navigation commands

## 6: Indentation

## 7: Interpreter integration

Basic to Cider/Slime.

## 8: Inline docs

eldoc, docstrings and parameters (cf pydoc)

## 8: Linter integration

## 9: Refactoring

## 0: Release on MELPA
