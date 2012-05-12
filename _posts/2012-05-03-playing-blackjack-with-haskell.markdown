--- 
layout: post
title: "Playing Blackjack With Haskell"
---

Following a visit to Las Vegas a few months ago, I developed an
interest in blackjack. According to Wikipedia, the
[house edge for a casino is typically around 1%](https://secure.wikimedia.org/wikipedia/en/wiki/Blackjack#Rule_variations_and_their_consequences_for_the_house_edge).
Rather than performing a statistical analysis, I decided to write a
blackjack-playing bot and measure its losses.

This seemed like the perfect opportunity to get my feet wet with
Haskell. A card game required minimal third party libraries and seemed
well-suited to playing with Haskell's type system.

### The Haskell learning curve

Haskell is not an easy language to get started in. There are excellent
online books to get you started ([1](http://learnyouahaskell.com/),
[2](http://book.realworldhaskell.org/read/)), and #haskell on Freenode
were very helpful. However, the learning curve is difficult at the start.

Every Haskell program requires the programmer to have a moderate
understanding of at least the type system and the IO monad. There is a
substantial amount of syntax in just this. For example, you will want
to learn the `do` notation
([though some consider it harmful](http://www.haskell.org/haskellwiki/Do_notation_considered_harmful))
which includes the unfortunately named `return` keyword.

For example:

{% highlight haskell %}
greetingForUser = do
    name <- getLine
    let greeting = "Hello, " ++ name ++ "!"
    return greeting
{% endhighlight %}

This is a function definition that takes a line of input from stdin
and returns a string containing that name wrapped in a friendly
message.

So, what's going here? `getLine` returns a value of `IO String`, a
string which is marked as having come from the outside, impure
world. You can call it twice with the same input and get different
outputs. `do` notation allows us to use the string as an untainted
`String`, enabling us to keep `IO` actions out of the core
functionality. `return` then wraps the final value in `IO` again since
ultimately `greetingForUser` is an impure function.

Got that? Understanding `IO` is a conceptual hurdle you must overcome
before you can start writing useful code.

There are a number of syntax gotchas which must be learned to write
code effectively. For example, getting the type of array access can
produce parse errors (`:type !!` is incorrect, `:type (!!)` should be
used). To make matters worse, syntax between the REPL and source code
files is slightly different.  The excellent
[Hoogle search engine](http://www.haskell.org/hoogle/) goes a long way
to fix this. For example, you can search for functions like `$` which
are not Google-friendly.

Haskell offers very powerful, very high-level tools for coding. Once
you're comfortable with the normal syntax, you can even write
macros. With such high-level tools, it's often possible to greatly
abstract concepts.

For example, every value in Haskell has a type. Every type has a kind,
which is essentially a meta type. GHC (the most commonly used
interpreter/compiler) even supports a number of extensions to this
meta type system.

So the Haskell programmer has to really think about their code, up
front. This is hard for a beginner.

### Pedagogic effects

For the persistent learner, Haskell helps you think about the effects
and assumptions of code in a rigorous way.

For instance, using only pure functions for the core of a program does
produce more predictable, debug-friendly, testing friendly code. This
is a perspective that has improved my code in impure languages.

Here's an example from the bot. I originally dealt cards by randomly
chosing cards one-by-one from the deck:

{% highlight haskell %}
dealOneCard [] = error "No cards left in deck"
dealOneCard deck = do
  randomCardIndex <- randomRIO (0, length deck - 1)
  return (deck !! randomCardIndex)
{% endhighlight %}

This forced more of my functions to be impure. Any function whose
output is dependent on a random number generator is a pain to
test. With Haskell exposing this in the type system, this defect
stares you in the face. I later refactored this into a superior
function that reorders the whole deck in one go:

{% highlight haskell %}
shuffleCards shuffled [] = return shuffled
shuffleCards shuffled unshuffled = do
  randomCardIndex <- randomRIO (0, length unshuffled - 1)
  let randomCard = unshuffled !! randomCardIndex
      unshuffledBefore = take randomCardIndex unshuffled
      unshuffledAfter = drop (randomCardIndex + 1) unshuffled
  
  shuffleCards (randomCard:shuffled) (unshuffledBefore ++ unshuffledAfter)
{% endhighlight %}

The combination of Haskell's type system and syntax forces you to
think about every situation that your function needs to deal
with. Once the code is written, this is wonderful. Code resulting from
this process is generally robust and reliable. However, I found
writing the code to be less satisfying, as it's hard to write a
function piece-by-piece.

### Code reliability

In any language, it's well worth using lint tools to examine code to
find potential errors or issues. This has saved me countless hours of
debugging. In Python, I use the excellent
[Pyflakes](https://github.com/kevinw/pyflakes) to catch mistakes
before I've even run the code.

It's shocking how many of errors simply cannot occur in Haskell
code. Haskell's syntax and type system not only eliminated bugs that I
could catch with lint tools, they prevent bugs that I can only detect
at runtime in other languages. For example, the type system forces you
to label any value that may be null with `Maybe`, eliminating null
pointer exceptions entirely.

### Blackjack conclusions

Originally, I would run the simulation with 10,000 hands, since it
showed less variance in losses. I read later that a player typically
plays 80 hands in an hour, so I tried simulating an hour's play. 80
hands of blackjack produces a much larger range of outcomes, giving
the game a completely different feel.

I had also intended to implement a perfect strategy inside my
simulator. It became clear that a relatively simple strategy did not
produce losses significantly less than a perfect strategy. I found
that the opposite was generally true for house rules though. Small
changes in house rules can substantially different losses.

Would I choose to play Blackjack if I visit Las Vegas again? I might
have a clear idea of strategy now, but having lost over $100,000 in
virtual money so far, it's probably unwise.

### Getting the code

My blackjack code is
[available on GitHub](https://github.com/Wilfred/Blackjack). The
player's tactics are not optimal, but losses are around 2%.
