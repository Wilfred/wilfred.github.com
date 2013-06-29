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

{% highlight ruby %}
some_list = [1, 2, 3]
some_list.map{|x| x*2 }
{% endhighlight %}
    
In Elisp, we can use mapcar:

{% highlight common-lisp %}
(setq some-list '(1 2 3))
(mapcar (lambda (x) (* x 2) some-list)
{% endhighlight %}
    
In Python, we might use a list comprehension:

{% highlight python %}
some_list = [1, 2, 3]
[x * 2 for x in some_list]
{% endhighlight %}
    
Elisp has the `loop` macro (originally from Common Lisp):

{% highlight common-lisp %}
(eval-when-compile (require 'cl))

(setq some-list '(1 2 3))
(loop for x in some-list collect (* x 2))
{% endhighlight %}

In Scala, we can use special arguments to denote the function
arguments:

{% highlight scala %}
val someList = List(1, 2, 3)
someList.map(_ * 2)
{% endhighlight %}

Using anaphoric macros we can do this in elisp:

{% highlight common-lisp %}
(require 'dash) ;; available on MELPA and Marmalade

(setq some-list '(1 2 3))
(--map (* it 2) some-list)
{% endhighlight %}
    
### Searching a sequence

Suppose we want to find the first integer in a list that's lower than
some value.  In Java, we'd probably use a for-each loop, terminating
as soon as we find the value we're looking for.

{% highlight java %}
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
{% endhighlight %}

We can do this in elisp using `dolist` and `return`:

{% highlight common-lisp %}
(eval-when-compile (require 'cl))

(defun find-less-than (threshold list)
  (dolist (item list)
    (when (< item threshold)
      (return item))))
{% endhighlight %}
          
In Haskell, we'd write this in a functional style, composing
functions:

{% highlight haskell %}
findLessThan :: Ord c => c -> [c] -> c
findLessThan threshold = head . filter . (< threshold)
{% endhighlight %}
    
We can do this in elisp too:

{% highlight common-lisp %}
(eval-when-compile (require 'cl))
(require 'dash)

(defun find-less-than (threshold list)
  (->> list
    (--filter (< it threshold))
    first))
{% endhighlight %}

### Function arguments

Suppose we want to find the mean of several numbers. In C, we might
write:

{% highlight java %}
float mean(float x, float y) {
    return (x + y) / 2;
}
{% endhighlight %}

No sweat in elisp:

{% highlight common-lisp %}
(defun mean (x y)
  (/ (+ x y) 2))
{% endhighlight %}

However, C doesn't support variadic functions without also passing in the
number of arguments. In many other languages, we can write a function that takes
any number of arguments. In JavaScript:

{% highlight javascript %}
function mean() {
    var sum = 0;
    for (var i=0; i<arguments.length; i++) {
        sum += arguments[i];
    }
    return sum / arguments.length;
}
{% endhighlight %}
    
We can use `defun*` to write variadic functions in elisp. This gives us
a straightforward translation:

{% highlight common-lisp %}
(eval-when-compile (require 'cl))

(defun* mean (&rest args)
  (let ((sum (apply '+ args)))
    (/ sum (length args))))
{% endhighlight %}

In Python, we might use keyword arguments when calling functions. This
can make function calls clearer (we can see which argument is which)
and facilitates optional arguments:

{% highlight python %}
def greet(greeting='Hello', name='anonymous'):
    return "%s %s!" % (greeting, name)

greet() # "Hello anonymous!"
greet(greeting='Hi') # "Hi anonymous!"
greet(name='Wilfred', greeting='Hey') # "Hey Wilfred!"
{% endhighlight %}

`defun*` allows this in elisp too:

{% highlight common-lisp %}
(defun* greet (&key (greeting "Hello") (name "anonymous"))
  (format "%s %s!" greeting name))

(greet)
(greet :greeting "Hi")
(greet :name "Wilfred" :greeting "Hey")
{% endhighlight %}

### Destructuring and Pattern Matching

In CoffeeScript, it's possible to destructure an array like this:

{% highlight coffeescript %}
sumPair = (pair) ->
    [first, second] = pair
    first + second
{% endhighlight %}
        
Elisp has you covered:

{% highlight common-lisp %}
(eval-when-compile (require 'cl))

(defun sum-pair (pair)
  (destructuring-bind (first second) pair
    (+ first second)))
{% endhighlight %}

In functional languages like Ocaml, we can use a more general
technique of pattern matching:

{% highlight ocaml %}
let rec sum_list list = match list with
  | [] -> 0
  | (x::xs) -> 1 + (sum_list xs);
{% endhighlight %}
    
Elisp can do pattern matching too, with `pcase`:

{% highlight common-lisp %}
(defun sum-list (list)
  (pcase list
    (`nil 0)
    ;; note that elisp does not do TCO
    ;; but see https://github.com/Wilfred/tco.el
    (`(,x . ,xs) (+ x (sum-list xs)))))
{% endhighlight %}

### Monads

Monads, as popularised by Haskell, don't really make sense without
type classes. However, the Maybe monad has a natural elisp
equivalent. An inexperienced Haskell programmer might write:

{% highlight haskell %}
maybeAdd :: Maybe Int -> Maybe Int -> Maybe Int
maybeAdd x y =
  case x of
    Just x' ->
      case y of
        Just y' -> Just $ x' + y'
        _ -> Nothing
    _ -> Nothing
{% endhighlight %}
        
There's a lot of wrapping and unwrapping here, which a monad can do
for us:

{% highlight haskell %}
maybeAdd :: Maybe Int -> Maybe Int -> Maybe Int
maybeAdd x y = do
  x' <- x
  y' <- y
  return $ x' + y'
{% endhighlight %}
      
(An experienced Haskeller would just use `liftM2 (+)`, but that's not
relevant here.)

dash.el provides `-when-let*` (equivalent to Scheme's `and-let`) which
allows us to mimic this behaviour:

{% highlight common-lisp %}
(require 'dash)

(defun maybe-add (x y)
  (-when-let* ((x* x)
               (y* y))
    (+ x* y*)))
{% endhighlight %}

### Objects

A classic example of a class-based code structure might be a monster
in a game:

{% highlight python %}
class Monster(object):
    def __init__(self):
        self.health = 100
        self.alive = True

    def take_damage(damage):
        self.health -= damage
        if self.health =< 0:
            self.alive = False
{% endhighlight %}

Elisp has EIEIO (Enhanced Implementation of Emacs Interpreted
Objects), which is an implementation of CLOS (the Common Lisp Object
System). So, we can straightforwardly translate this:

{% highlight common-lisp %}
(require 'eieio)
(eval-when-compile (require 'cl))

(defclass monster ()
  ((health :initform 100)
   (alive :initform t)))

(defmethod take-damage ((m monster) damage)
  (decf (oref m health) damage)
  (when (<= (oref m health) 0)
    (setf (oref m alive) nil)))
{% endhighlight %}

No discussion of object-oriented code would be complete, of course,
without an example of class-based inheritance:

{% highlight python %}
class BossMonster(Monster):
    def __init__(self):
        super(BossMonster, self).__init__()
        self.health = 500
{% endhighlight %}

EIEIO version:

{% highlight common-lisp %}
(defclass boss-monster (monster)
  ((health :initform 500)))
{% endhighlight %}

Finally, EIEIO also has support for more exotic object-oriented
features, such as mixins:

{% highlight python %}
class TalksMixin(object):
    catchphrase = "Rawr!"

    def say(self, player):
        return self.catchphrase

class NoisyMonster(Monster, TalksMixin):
    pass
{% endhighlight %}

EIEIO:

{% highlight common-lisp %}
(defclass talks-mixin ()
  ((catchphrase :initform "Rawr!")))

(defmethod say ((thing talks-mixin))
  (oref thing catchphrase))

(defclass noisy-monster (monster talks-mixin)
  ())
{% endhighlight %}

### Namespaces

Elisp has separate namespaces for functions and variables. So if we
store a function in a variable, we have to use `funcall` to use
it. Scheme, however, is a lisp-1 with a single namespace. In Scheme we
can write:

{% highlight scheme %}
;; assign a function to the symbol
(define add-two
  (lambda (x) (+ x 2)))

;; assign a value to a symbol
(define two 2)

(add-two two) ;; 4
{% endhighlight %}

With a short macro, we can actually execute this code unchanged:

{% highlight common-lisp %}
(defmacro define (name object)
  `(setq ,name (fset ',name ,object)))

(define add-two
  (lambda (x) (+ x 2)))

(define two 2)

(add-two two) ;; 4
{% endhighlight %}

In Clojure, we can use explicit namespaces to separate code. This
prevents us having to worry about name clashes.

{% highlight clojure %}
;; idiomatic clojure would use the ns macro here instead
(in-ns 'hello)

(clojure.core/defn say []
  "Hello world")

(in-ns 'goodbye)

(clojure.core/defn say []
  "Goodbye world")

(hello/say)
{% endhighlight %}
    
There's a [codex.el](https://github.com/sigma/codex) package that
allows us to do this in elisp:

{% highlight common-lisp %}
(require 'codex)

(defcodex hello
  (:use emacs))

(in-codex hello
  (emacs:defun say () "Hello world"))

(defcodex goodbye
  (:use hello)
  (:use emacs))

(in-codex hello
  (emacs:defun say () "Goodbye world")
  (hello:say))
{% endhighlight %}

### What elisp doesn't have

Elisp can't do everything. There are some languages features that
simply can't be implemented by the users. There are no reader macros,
there's no FFI, and there's no multithreading (though threads are
being worked on).

There are also powerful language features that could be implemented,
but haven't yet been implemented in elisp. For example, metaclasses
([implemented in CLOS](http://www.lispworks.com/documentation/HyperSpec/Body/07_.htm)),
hygenic macros
([implemented in Common Lisp](http://www.p-cos.net/documents/hygiene.pdf)),
logic programming
([implemented in Clojure](https://github.com/clojure/core.logic)) or
even a type system
([implemented in Scheme](http://docs.racket-lang.org/ts-guide/)).

### Should I write code like this?

For the features I have demonstrated, some examples are carefully
chosen to showcase the capabilities of elisp. For example, the
`define` macro still won't allow you to write `((foo) bar)`, it's a
syntax error. Other examples are impractical (codex.el makes edebug
unusable), whilst still others are so rarely used that other elisp
developers will need time to understand the code.

All that aside, elisp is an immensely flexible, deeply hackable
language. Not only is it the fastest language to be productive in
("[learning any amount of elisp makes your life better immediately](http://joelmccracken.github.io/entries/why-you-should-learn-elisp/)"),
it also provides a whole zoo of language features, providing an
elegant way of expressing virtually any program.
