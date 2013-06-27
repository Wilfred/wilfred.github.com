--- 
layout: post
title: "Adventures in Multi Paradigm Programming"
---

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

    let rec sum_list list = match list with
      | [] -> 0
      | (x::xs) -> 1 + (sum_list xs);
    
Elisp can do pattern matching too, with pcase:

    (defun sum-list (list)
      (pcase list
        (`nil 0)
        ;; note that elisp does not do TCO
        ;; but see https://github.com/Wilfred/tco.el
        (`(,x . ,xs) (+ x (sum-list xs)))))

### Monads

Monads, as popularised by Haskell, don't really make sense without
type classes. However, the Maybe monad has a natural elisp
equivalent. A naive Haskell programmer might write:

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

A classic example of a class-based code structure might be a monster
in a game:

    class Monster(object):
        def __init__(self):
            self.health = 100
            self.alive = True

        def take_damage(damage):
            self.health -= damage

            if self.health =< 0:
                self.alive = False

Elisp has EIEIO (Enhanced Implementation of Emacs Interpreted
Objects), which is an implementation of CLOS (the Common Lisp Object
System). So, we can straightforwardly translate this:

    (require 'eieio)
    (eval-when-compile (require 'cl))

    (defclass monster ()
      ((health :initform 100)
       (alive :initform t)))

    (defmethod take-damage ((m monster) damage)
      (decf (oref m health) damage)
      (when (<= (oref m health) 0)
        (setf (oref m alive) nil)))

No discussion of object oriented code would be complete, of course,
without an example of class-based inheritance:

    class BossMonster(Monster):
        def __init__(self):
            super(BossMonster, self).__init__()
            self.health = 500

EIEIO version:

    (defclass boss-monster (monster)
      ((health :initform 500)))

Finally, EIEIO also has support for more exotic object-oriented
features, such as mixins:

    class TalksMixin(object):
        catchphrase = "Rawr!"

        def say(self, player):
            return self.catchphrase

    class NoisyMonster(Monster, TalksMixin):
        pass

EIEIO:

    (defclass talks-mixin ()
      ((catchphrase :initform "Rawr!")))

    (defmethod say ((thing talks-mixin))
      (oref thing catchphrase))

    (defclass noisy-monster (monster talks-mixin)
      ())

### Namespaces

Elisp has separate namespaces for functions and variables. So if we
store a function in a variable, we have to use `funcall` to use
it. Scheme, however, is a lisp-1 with a single namespace. In Scheme we
can write:

    ;; check me
    ;; assign a function to the symbol
    (define add-two
      (lambda (x) (+ x 2)))

    ;; assign a value to a symbol
    (define two 2)

    (add-two two) ;; 4

With a short macro, we can actually execute this code virtually
unchanged:

    (defmacro define (name object)
      `(setq ,name (fset ',name ,object)))

    ;; assign a function to the symbol add-two
    (define add-two
      (lambda (x) (+ x 2)))
    ;; assign a function to the symbol times-two
    (define times-two
      (lambda (x) (* x 2)))

    ;; normal function definition
    (define compose (f g)
      (lambda (x) (f (g x))))

    ((compose times-two add-two) 2) ;; 8

In Clojure, we can use the `ns` macro separate code into
namespaces. This prevents us having to worry about name clashes.

    ;; example
    
There's a [codex.el](https://github.com/sigma/codex) package that
allows us to do this in elisp:

    (require 'codex)

    (defcodex hello
      (:use emacs)
      (:export "greet"))

    (in-codex hello
      (defun say () "Hello world"))

    (defcodex goodbye
      (:use hello))

    (in-codex hello
      (defun say () "Goodbye world")
      (hello:say))

### What elisp doesn't have

A proper package system, hygenic macros, reader macros, logic programming (miniKanren), infix notation (but
see SRFI 105), a type system (Clojure and Racket)

### Should I use these?

Elisp can't do everything. There are some languages features that
simply can't be implemented by the users. For example, reader macros:

    ;; example
    
Then there are language features that haven't been implemented yet,
such as metaclasses
([implemented in CLOS](http://www.lispworks.com/documentation/HyperSpec/Body/07_.htm)),
hygenic macros
([implemented in Common Lisp](http://www.p-cos.net/documents/hygiene.pdf)),
logic programming
([implemented in Clojure](https://github.com/clojure/core.logic)) or a
type system
([implemented in Scheme](http://docs.racket-lang.org/ts-guide/)). For
the features I have demonstrated, some examples are very carefully
chosen to . For example, the `define` macro still won't allow you to
write `((foo) bar)`, it's a syntax error. Other examples are
impractical (codex.el make edebug unusable) or simply not idiomatic
and you'll find it very hard to get contributors.

All that aside, elisp is an immensely flexible, deeply hackable
language. Not only is it the fastest language to be productive in
(fixme: link), it also provides a whole zoo of language features, providing
an elegant way of expressing virtually any program.
