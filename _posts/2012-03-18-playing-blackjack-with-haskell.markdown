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

    greetingForUser = do
        name <- getLine
        let greeting = "Hello, " ++ name ++ "!"
        return greeting
        
This is a function definition that takes a line of input from stdin
and returns a string containing that name wrapped in a friendly
message.

`getLine` returns a value of `IO String`, a string which is marked as
having come from the outside, impure world. You can call it twice with
the same input and get different outputs. `do` notation allows us to
use the string as an untainted `String`, enabling us to keep `IO`
actions out of the core functionality. `return` then wraps the final
value in `IO` again since ultimately `greetingForUser` is an impure
function.

Got that? Understanding `IO` is a conceptual hurdle you must overcome
before you can start writing useful code.

There are a number of syntax gotchas which must be
learned to write code effectively. The excellent
[Hoogle search engine](http://www.haskell.org/hoogle/) allows a
Haskell programmer to search for keywords like `$` which are not
Google-friendly. For example, getting the type of
array access can produce parse errors (`:type !!` is incorrect, `:type (!!)`
should be used). To make matters worse, syntax between the REPL and
source code files is slightly different.

Haskell offers very powerful, very high-level tools for coding. Once
you're comfortable with the normal syntax, you can even write
macros. With such high-level tools, it's often possible to greatly
abstract concepts.

`fmap` is an excellent example of this. Whilst many languages offer a
`map` function that applies a function to every element in a list,
`fmap` works over anything that can be iterated over. A crude Python
implementation of `fmap` would look like:

    def fmap(function, iterable):
        iterable_type = type(iterable)
        new_values = (function(x) for x in iterable)
        return iterable_type(new_values)
        
The Python version is substantially less general: it doesn't work on
generators or trees. In practice, this power is a double-edged sword
due to the initial effort required to grok what's going on. Familiar
concepts like numbers sometimes require (seemingly exotic) type casts
in order to do simple arithmetic.

### Pedagogic effects

For the persistent learner, Haskell helps you think about the effects
and assumptions of code in a rigorous way.

For example, using only pure functions for the core of a program does
produce more predictable, debug-friendly, testing friendly code. This
is a perspective that has improved my code in impure languages.

For example, I originally dealt cards by randomly chosing cards
one-by-one from the deck:

    dealOneCard [] = error "No cards left in deck"
    dealOneCard deck = do
      randomCardIndex <- randomRIO (0, length deck - 1)
      return (deck !! randomCardIndex)

This forced more of my functions to be impure. Any function whose
output is dependent on a random number generator is a pain to
test. With Haskell exposing this in the type system, this defect
stares you in the face. I later refactored this into a superior
function that reorders the whole deck in one go:

    shuffleCards shuffled [] = return shuffled
    shuffleCards shuffled unshuffled = do
      randomCardIndex <- randomRIO (0, length unshuffled - 1)
      let randomCard = unshuffled !! randomCardIndex
          unshuffledBefore = take randomCardIndex unshuffled
          unshuffledAfter = drop (randomCardIndex + 1) unshuffled
      
      shuffleCards (randomCard:shuffled) (unshuffledBefore ++ unshuffledAfter)

The combination of Haskell's type system and syntax forces you to
think about every situation your function needs to deal with. Once the
code is written, this is wonderful. Code resulting from this process
is generally robust and reliable. However, I found writing the code to
be less satisfying, as it's hard to write a function piece-by-piece.

Pattern matching, no if without else

In any language, it's well worth using lint tools to examine code to
find potential errors or issues. This has saved me countless hours of
debugging.

Code is very stable. A remarkable number of classes of bug are
completely eliminated (buffer overflow, null pointer exceptions).

Code is very machine readable. Cf pyflakes.

### Blackjack lessons

Number of hands (fewer is noisy in terms of takings, more is
unrealistic and more computationall expensive)

Importance of fine strategy often lead to diminishing
returns. Tracking imperfect shuffling (multiple decks) or observing
cards in play only makes a very small amount of difference.

Subtle changes in house rules produce radically different takings.

### Getting the code

My blackjack code is
[available on GitHub](https://github.com/Wilfred/Blackjack). The
player's tactics are not optimal, but losses are around 2%.
