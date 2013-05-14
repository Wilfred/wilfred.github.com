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
    
We can use defun* to write variadic functions in elisp. This gives us
a straightforward translation:

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

### Destructuring and Pattern Matching

In CoffeeScript, it's possible to destructure an array like this:

    sumPair = (pair) ->
        [first, second] = pair
        first + second
        
Elisp has you covered:

    (eval-when-compile (require 'cl))

    (defun sum-pair (pair)
      (destructuring-bind (first second) pair
        (+ first second)))

In functional languages like Ocaml, we can use a more general
technique of pattern matching:

    -- check me
    fun sumList []     = 0
    fun sumList (x:xs) = x + sumList xs
    
Elisp can do pattern matching too, with pcase:

    (defun sum-list (list)
      (pcase list
        (`nil 0)
        ;; note that elisp does not do TCO
        (`(,x . ,xs) (+ x (sum-list xs)))))

### Monads

Monads, as popularised by Haskell, don't really make sense without
type classes (we'll come to those later). However, the Maybe monad has
a natural elisp equivalent. A naive Haskell programmer might write:

    maybeAdd :: Maybe Int -> Maybe Int -> Maybe Int
    maybeAdd x y =
      case x of
        Just x' ->
          case y of
            Just y' -> Just $ x' + y'
            _ -> Nothing
        _ -> Nothing
        
There's a lot of wrapping and unwrapping here, which a monad can do
for us:

    maybeAdd :: Maybe Int -> Maybe Int -> Maybe Int
    maybeAdd x y = do
      x' <- x
      y' <- y
      return $ x' + y'
      
(An experienced Haskeller would just use `liftM2 (+)`, but that's not
relevant here.)

dash.el provides `-when-let*` (equivalent to Scheme's `and-let`) which
allows us to mimic this behaviour:

    (require 'dash)

    (defun maybe-add (x y)
      (-when-let* ((x* x)
                   (y* y))
        (+ x* y*)))
        
### Objects

Basic example:

    (require 'eieio)

    (defclass person ()
      ((name :initarg :name
             :initform "Anonymous")
       (color :initarg :color
              :type string)))

    (defmethod describe-them ((p person))
      (format
       "%s prefers the color %s."
       (oref p name)
       (oref p color)))

    (describe-them
     (person "example-person" :name "Bob" :color "psychadelic pink"))

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
