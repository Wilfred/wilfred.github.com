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

Suppose we want to double every item in a list. In Ruby, we might
write it like this, using map:

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
    (loop for x in some-list collect (* x 2))

In Scala, we can use special arguments to denote the function
arguments:

    val someList = List(1, 2, 3)
    someList.map(_ * 2)
    
Using anaphoric macros we can do this in elisp:

    (require 'dash) ;; available on MELPA and Marmalade

    (setq some-list '(1 2 3))
    (--map (* it 2) some-list)
    
### Searching a sequence

Suppose we want to find the first integer in a list that's lower than
some value.  In Java, we'd probably use a for-each loop, terminating
as soon as we find the value we're looking for.

    import java.util.List;
    
    // inside some class
    public Integer findLessThan(Integer threshold, List<Integer> list) {
        for (Integer item: list) {
            if (item < threshold) {
                return item;
            }
        }
        return null;
    }

We can do this in elisp using dolist and return:

    (eval-when-compile (require 'cl))
    
    (defun find-less-than (threshold list)
      (dolist (item list)
        (when (< item threshold)
          (return item))))
          
In Haskell, we'd write this in a functional style, composing
functions:

    findLessThan :: Ord c => c -> [c] -> c
    findLessThan threshold = head . filter . (< threshold)
    
We can do this in elisp too:

    (eval-when-compile (require 'cl))
    (require 'dash)
    
    (defun find-less-than (threshold list)
      (->> list
        (--filter (< it threshold))
        first))

### Function arguments

Suppose we want to find the mean of several numbers. In C, we might
write:

    float mean(float x, float y) {
        return (x + y) / 2;
    }

No sweat in elisp:

    (defun mean (x y)
      (/ (+ x y) 2))
        
However, C doesn't support variadic functions without also passing in the
number of arguments. In many other languages, we can write a function that takes
any number of arguments. In JavaScript:

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
        
In Python, we might use keyword arguments when calling functions. This
can make function calls clearer (we can see which argument is which)
and facilitates optional arguments:

    def greet(greeting='Hello', name='anonymous'):
        return "%s %s!" % (greeting, name)
        
    greet() # "Hello anonymous!"
    greet(greeting='Hi') # "Hi anonymous!"
    greet(name='Wilfred', greeting='Hey') # "Hey Wilfred!"

defun* allows this in elisp too:

    (defun* greet (&key (greeting "Hello") (name "anonymous"))
      (format "%s %s!" greeting name))

    (greet)
    (greet :greeting "Hi")
    (greet :name "Wilfred" :greeting "Hey")

### Pattern matching and destructuring

    pcase
    destructuring-bind
    
Pattern matching:

(example and link)

### Monads

Scheme's and-let:

(macro and example)

Generalising to monads (and-let is roughly Maybe monad):

(example)

### Objects

EIEIO.

Classical Inheritance:

(eieio example)

Prototypical inheritance:

(eieio example)

Mixins:

    example

Metaclasses:

(python example?)
(eieio example)

### Namespaces

Lisp-1:

(macro example)

### What elisp doesn't have

Hygenic macros, reader macros, logic programming (miniKanren), infix notation (but
see SRFI 105), a type system (Clojure and Racket)

### Should I use these?

There's certainly more than one way to do it in elisp. Generally,
favouring idiomatic code is going to make it easier for others (and
the future you) to maintain code. I learnt this the hard way when
writing experimental packaging macros and making my code resistant to
debugging or jumping to the definition.
