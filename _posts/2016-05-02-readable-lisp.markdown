--- 
layout: post
title: "Readable Lisp"
---

I love working with lisp. It's a delight to edit with structural
editors. Want to make if-statement unconditional? M-r.

Lisp also is incredibly flexible. I wouldn't trade infix syntax for
losing macros.

However, in the style of
[Euro-English](http://www.davidpbrown.co.uk/jokes/european-commission.html),
I want to explore moving from a traditional lisp syntax to something
that's less noisy.

(defun fizzbuzz (n)
  (let ((i 1))
    (while (<= i n)
      (cond
       ((= (mod i 15) 0) (message "FizzBuzz"))
       ((= (mod i 5) 0) (message "Buzz"))
       ((= (mod i 3) 0) (message "Fizz"))
       (t (message "%d" i)))
      (setq i (+ i 1)))))

Line based without brackets, fewer parens in let, using {} to denote
blocks, perhaps || to denote infix syntax?

Goals:

* fewer parens
* editor tooling friendly -- can programmatically indent, not just
  whitespace with multiple meanings like python
* stretch goal: infix
