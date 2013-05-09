--- 
layout: post
title: "Multi Paradigm Programming With Elisp"
---

Alternative title: Adventures in Multi Paradigm Programming

It's remarkable just how many types of programming are possible with
Emacs lisp. In fact, it's hard to find a style of programming that
isn't possible. I've translated a few examples from other languages
into elisp to showcase this diversity.

### Iterating over a sequence

Suppose we want to double every item in a list. In Java, we'd probably
use a for-each loop.

    import java.util.Arrays;
    import java.util.ArrayList;
    import java.util.List;
    
    // inside our function
    List<Integer> someList = Arrays.asList(1, 2, 3);
    List<Integer> resultList = new ArrayList();

    for (Integer x : someList) {
        resultList.add(x * 2);
    }
    
We can do this in elisp using the loop macro (originally from Common
Lisp):

    (eval-when-compile (require 'cl))

    (setq some-list '(1 2 3))
    (setq result-list)
    (loop for x in some-list do
          (add-to-list 'result-list (* 2 x) t))
          
In other languages though, we wouldn't mutate objects. In Ruby, we
might write it like this, using map:

    some_list = [1, 2, 3]
    some_list.map{|x| x*2 }
    
In Elisp, we can use mapcar:

    (setq some-list '(1 2 3))
    (mapcar (lambda (x) (* x 2) some-list)
    
In Python, we might use a list comprehension:

    some_list = [1, 2, 3]
    [x * 2 for x in some_list]
    
Elisp has the loop macro (originally from Common Lisp):

    (eval-when-compile (require 'cl))

    (setq some-list '(1 2 3))
    (loop for x in some-list collect (* 2 x))

In Scala, we can use special arguments to denote the function
arguments:

    val someList = List(1, 2, 3)
    someList.map(_ * 2)
    
Using anaphoric macros we can do this in elisp:

    (require 'dash) ;; available on MELPA and Marmalade

    (setq some-list '(1 2 3))
    (--map (* it 2) some-list)
    
### Function arguments

Suppose we want to find the mean of several numbers. In C, we might
write:

    float mean(float x, float y) {
        return (x + y) / 2;
    }

No sweat in elisp:

    (defun mean (x y)
        (/ (+ x y) 2))
        
C doesn't support variadic functions without also passing in the
number of arguments. In JavaScript, we can write a function that takes
any number of arguments:

    function mean() {
        var sum = 0;
        for (var i=0; i<arguments.length; i++) {
            sum += arguments[i];
        }
        return sum / arguments.length;
    }
    
We can use defun* (another feature originally from Common Lisp) to
write variadic functions in elisp. This gives us a straightforward
translation:

    (eval-when-compile (require 'cl))

    (defun* mean (&rest args)
      (let ((sum (apply '+ args)))
        (/ sum (length args))))
        
In Haskell, there's no straightforward way (but see [this](okmij.org/ftp/Haskell/polyvariadic.html) if you're
feeling adventurous) to write a variadic function. However, in
Haskell, we might write our mean function without any explicit
arguments at all! This is called 'point-free style':

    import Control.Arraow

    mean :: [Double] -> Double
    -- explicit arguments:
    mean xs = sum xs / fromIntegral (length xs)
    -- point-free alternative:
    mean = sum &&& fromIntegral . length >>> uncurry (/)
    
In elisp:

    (defun comp (f g)
      "Compose function F with function G."
      (lambda (x)
        (funcall f (funcall g x))))
    

Pattern matching:

(example and link)

Scheme's and-let:

(macro and example)

Generalising to monads (and-let is roughly Maybe monad):

(example)

Object-oriented:

(eieio example)

Classical Inheritance:

(eieio example)

Prototypical inheritance:

(eieio example)

Metaclasses:

(python example?)
(eieio example)

Lisp-1:

(macro example)

Scheme pure macros?

Scheme's infix notation proposal?

Type system: none yet, but Clojure and Scheme have it.
