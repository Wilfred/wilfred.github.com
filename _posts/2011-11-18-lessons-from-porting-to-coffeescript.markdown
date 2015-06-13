--- 
layout: post
title: "Lessons From Porting To CoffeeScript"
---

I recently set out to do a project in
[CoffeeScript](http://coffeescript.org/)
(abbreviated CS). My aim was to build a Sudoku solver. One week and
one JS prototype later, I realised that Sudoku were more interesting
than I realised. I've recently ported
[the solver](http://www.wilfred.me.uk/sudoku/) to CoffeeScript, and
here's what I've learned:

### It's easy

Getting up and running is very straightforward. Once you have the
compiler, the
[entire language reference](http://coffeescript.org/#language)
is a single web page. Many of its language features have been taken
from Python or Ruby, so you can quickly grasp the key concepts. The
result: you can achieve basic proficiency very quickly indeed.

CS even supports embedding JS directly in your code. You don't need to
port entire program at once, you can work function-by-function and
test as you go.

If you're ever uncertain about syntax, CS even ships with its own
standalone REPL.

### It's readable

CoffeeScript is much more concise. The JS code was ~400 lines,
whilst the CS version was ~300 lines. Keywords are more readable too
-- for example, the JS operator `&&` becomes `and`.

CS allows you to escape boilerplate and ceremony. Rather than:

{% highlight javascript %}
for (var i=0; i<arr.length; i++) {
    doSomething(arr[i]);
}
{% endhighlight %}
    
you can simply use `in` in a way that JS won't support until
1.6. If we're lucky.

{% highlight coffeescript %}
for element in arr
  doSomething(element)
{% endhighlight %}
        
Virtually everything returns a value in CS. Want an array of 1s?

{% highlight coffeescript %}
arr = for i in [0...arraySize]
  1
{% endhighlight %}
        
This was a major benefit in my code.

### No foot shooting

JS has a wide variety of gotchas that web developers need to learn. CS
sidesteps many of these. `==` in CS compiles to `===`, so type
coercion can't catch you out. Variables do *not* leak to the global
object. CS even sidesteps the many gotchas of object literals:

{% highlight coffeescript %}
# no issue with 'class' as a keyword interfering with key names
myObject = {school: "St Thomas", class: "Maths"}

# no issue with object literals at the top level (try this in firebug)
{x: 1, y:2}
{% endhighlight %}

### It's young and imperfect

IDE support is limited (even Emacs support could use polish). Rails
offers CS support built-in these days, but many other toolchains don't
yet.

CS also has a few syntactic weirdnesses of its own. Operator
precedence can be surprising, which is unfortunate for a language that makes
brackets optional.

The following code:

{% highlight coffeescript %}
if not 1 in [2, 3]
{% endhighlight %}
    
is equivalent to:

{% highlight coffeescript %}
if (not 1) in [2, 3]
{% endhighlight %}
    
which can lead to some nasty bugs.

The CS range operators, `..` and `...`, are simply too similar and
make off-by-one errors easier. It also has the unfortunate result that
`[x.y]`, `[x..y]` and `[x...y]` are all syntactically valid, visually
similar but do different things.

It's also tempting to go further with things that CS *should* do. I
spend a lot of time chasing a bug of the form:

{% highlight coffeescript %}
if foo.length < bar.length:
  doSomething()
{% endhighlight %}
      
which should have been:

{% highlight coffeescript %}
if foo.length < bar.baz.length:
   doSomething()
{% endhighlight %}
       
CS largely does not change the semantics of the language. This makes
compiled code simpler, but holds back some of the [more interesting
opportunities](https://github.com/jashkenas/coffee-script/issues/350).

### It won't save you from learning JS

With a
few ([1](http://harvesthq.github.com/chosen), [2](http://batmanjs.org))
notable exceptions, the utilities you will want to use are written in
JS. CS developers still need to be comfortable in JS to dig into
documentation, learn about the DOM API, and (more rarely) debug. This
is rarely an issue in practice, since almost all CS developers have
some JS background.

### It gives you modern programming today

Many of the features CS offers today will seem familiar to anyone who
has read the ECMAScript Harmony proposals. It's all pretty
incestuous. The wonderful thing about CS is that you can have this
powerful, readable syntax today, on practically any browser.

That's worth having, in my book.
