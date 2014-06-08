--- 
layout: post
title: "Trifle Lisp: Designing a Numeric System"
---

What numeric data types should a programming language offer? How do
they behave? How do they perform? Can users define their own?

As Trifle develops, these are questions we need good answers
for. Trifle seeks to be expressive and 'fast enough'. **The primary goal
of our numeric system is to be mathematically well-behaved**.

There are several numerical gotchas that programmers just get used
to. Many of these are simply implementation details that we shouldn't
have to care about when programming in a higher-level programming
language. Here's a Java example that breaks the mathematical principle
that 'if x and y are positive integers, then x times y is also positive'.

{% highlight java %}
class Multiply {
    public static void main(String[] args) {
        int x = 1;
        for (int i=0; i<40; i++) {
            x = x * 3;
            System.out.println(x);
        }
    }
}
{% endhighlight %}

Java's integers are limited to 32-bit, so this innocent-looking
program will print negative numbers. Trifle will protect users from
this by using arbitrary-sized integers.

When we stop using integers, it's even harder to write mathematically
sound programs. Here's a Python example:

{% highlight python %}
>>> 0.1 + 0.1 + 0.1 + 0.1 + 0.1 + 0.1 + 0.1 + 0.1 + 0.1 + 0.1
0.9999999999999999
{% endhighlight %}

This is because 0.1 cannot be represented exactly in the
base-2 floating-point representation that Python uses internally. Trifle
again tries to protect the user, this time by using base-10 floating
point numbers. These are much less surprising, since their string
representation shown to the user matches the underlying
representation. **A lisp should have homoiconic numbers as well as
programs!**

Base-10 floating point numbers have other nice properties, such as
displaying trailing zeroes according to high-school
arithmetic. Another Python example:

{% highlight python %}
>>> from decimal import *
>>> Decimal('1.10') * Decimal('1.10')
Decimal('1.2100')
{% endhighlight %}

However, we should be nervous about using any form of inexact
arithmetic. Rounding errors will occur, and they can be catastrophic.
[Some calculations can be dramatically incorrect when calculated with a computer](http://www.apmaths.uwo.ca/~rcorless/frames/AM261/paper4.pdf).
In Trifle, we **try to encourage users to use exact arithmetic** by also
offering exact, arbitrary-sized fractions. This enables to the user to
write many more programs, using all the operators from basic
arithmetic (`+` `-` `*` `/`), without risking bugs from rounding
errors.

One disadvantage of fractions is that they're harder to
read. `15298283/146395` is not as friendly as `104.5...`. To tackle
this, we're prototyping a shell that will also help users understand how big
their numbers are.

    >>> (/ 15298283 146395)
    15298283/146395
    [Fraction, roughly 104.5]

Finally, some users will want more numeric types. Common examples
include complex numbers, vectors and matrices. Users are free to do so
in Trifle. `+` is a normal function and may be redefined to add
support for new numeric types.

All these features come at a performance cost. Computers can calculate
much more quickly, and consuming less memory, by using fixed-size
integers and base-2 floating point arithmetic. For high performance
code or for foreign functions, we may add these data types in
future. In the meantime, we should have a language that's a little
less error-prone.

_Note: Exact arithmetic has yet to land in Trifle builds, but it is
being actively worked on._
