--- 
layout: post
title: "The Fastest BigInt In The West"
---

The
[Computer Language Benchmarks Game](http://benchmarksgame.alioth.debian.org/)
is an amazing project, featuring some meticulously optimised
programs.

However, it's not always easy to interpret the results, to see *why*
some programs or languages are faster than others. It's also not clear how
representative the test programs are of typical performance of that
language.

Instead, I thought it would be interesting to measure the performance
of a single aspect of programming languages, and explore why the
implementation led to that performance.

The Benchmarks Game defines a
[pidigits benchmark](http://benchmarksgame.alioth.debian.org/u64q/performance.php?test=pidigits#about)
which measures the performance of arbitrary size integers ('BigInt'),
and test programs are widely available.

However, many of the test programs are just using a foreign-function
interface to call out to a highly-optimised C numerics
library. Instead, I decided to constrain my test programs to only use
BigInts provided by the language or its standard library.

Limiting our benchmark to languages with built in BigInts
significantly limits the number of languages we can test. I decided to
include multiple implementations of the same language to see the
variation within the same language.

Unlike the Benchmarks Game, we aren't requiring all the test programs
for the same language to be
identical. [This is a controversial rule](https://alexgaynor.net/2011/apr/03/my-experience-computer-language-shootout/)
and I wasn't able to run the same program on all Scheme
implementations anyway.

I ran these programs an AMD Phenom X4 965 with 4 GiB RAM. The kernel
was Linux x86_64 3.16.4 and
[the exact code I used was in this git commit](https://github.com/Wilfred/the_end_times/tree/b3f0d1861c8eed35d2743e2497700e5a4400d750).

So, let's look at the results!

| Language | Implementation | Time in seconds |
|----------|----------------|-------------:|
| Common Lisp | SBCL 1.2.2 | 12.049 |
| Go | Go 1.3.3 | 2.899 |
| Haskell | GHC 7.8.3 | 1.583 |
| Java | OpenJDK 1.7.0_71 | 19.618 |
| Julia | Julia 0.3.1 |  7.395 |
| PHP | PHP 5.6.2 | 438.732 |
| Python3 | CPython 3.4.2 | 4.568 |
| Python3 | pypy 2.3.1 | 8.881 |
| Ruby | JRuby 1.7.16 | 29.197 |
| Ruby | MRI 2.1.3p242 | 27.721 |
| Ruby | Rubinius 2.2.10 | 19.010 |
| Rust | Rust 0.12.0-dev | 11.793 |
| Scheme | Chicken 4.9.0.1 | 8.350 |
| Scheme | Gambit 4.7.3 | 14.631 |
| Scheme | Guile 2.0.11 | 9.413 |
| Scheme | Racket 6.1 | 17.369 |

Wow, we can see a huge range of performance here. We'll take a look at
how the different implementations work, starting with the slowest.

The slowest BigInt here is **PHP**. PHP doesn't have a BigInt data type,
so its arbitrary size numerical functions take and return strings!
For example, check out [the docs for addition](http://php.net/manual/en/function.bcadd.php). This
adds huge overhead as the numbers get larger, because an increasing
amount of time is spent serialising and deserialising strings
representing numbers. If you run the benchmark in a shell, the
incremental slowdown is very clear.

The next slowest is **JRuby**. JRuby depends on the BigInteger.java
implementation provided by the JDK. This is very respectable
performance for a BigInt implemented in the host language, the
majority of the faster BigInt designs have been written in lower-level
languages.

**MRI** has its
[own BigInt implementation written in C](https://github.com/ruby/ruby/blob/v2_1_3/ext/bigdecimal/bigdecimal.c).

We're seeing a significant range of speeds for Scheme
implementations. It's interesting to note that **Guile** and **Racket** are
JIT interpreters, **Chicken** is compiled, and **Gambit* is interpreted (I
couldn't get the compiler to run on my machine). The idea that
compiled languages are always faster than interpreted is refuted here.

Since Scheme requires integers to be arbitrary sized, we're exercising
built-in functionality here rather than libraries. Gambit is a
self-hosting Scheme compiler so
[implements numerics in Scheme](https://github.com/feeley/gambit/blob/v4.7.0/lib/_num.scm). Chicken
is also largely written in Scheme. Racket
[uses GMP](https://github.com/plt/libs/blob/2f116c1b64af3f980a403cb4b57051457b2a9c39/math-x86_64-linux-natipkg/math/info.rkt),
[as does Guile](http://git.savannah.gnu.org/gitweb/?p=guile.git;a=blob;f=README;h=92d786c069837d81126d598e93416a20fc68a0c2;hb=HEAD#l65).

The implementations
are generally written in C.

Guile uses GMP.


**Rust** also implements BigInt as a library. In fact, whilst BigInt
is currently built-in, it will be
[an external library](https://github.com/rust-lang/num) in a future
release! Nonetheless, it's very impressive performance, especially for
a young library.

**SBCL** is a very well-optimised Common Lisp compiler, with
[some rather elegant implementation features](http://www.pvk.ca/Blog/2014/08/16/how-to-define-new-intrinsics-in-sbcl/).
This BigInt implementation is modestly described as
"[reasonably simple but decent](http://www.sbcl.org/gsoc2013/ideas/#sec-1.2)". SBCL
is exceptional at optimising fixed-size arithmetic, but the BigInt
performance here is still very good.

Python is often a slow language, but its performance here took me
completely by surprise. All integers in Python are arbitrary
sized. **CPython** has a number of interesting numerical
optimisations, including
[reusing integer objects between -5 and 256](http://www.laurentluce.com/posts/python-integer-objects-implementation/). **Pypy**
is a little slower here, as
[its JIT is unable to optimise this code](https://mail.python.org/pipermail/pypy-dev/2014-August/012713.html).

Ultimately, the fastest way to implement BigInt numerics is to use
[The GNU Multiple Precision Arithmetic Library](https://gmplib.org/)
(usually referred to as 'GMP'). **Julia** takes a
['batteries included' approach](https://github.com/JuliaLang/julia/#required-build-tools-and-external-libraries),
especially to its numeric support, so it uses GMP. As a result, it
performs extremely well in this benchmark as a result.

**Go** is the most heavily optimised language for this benchmark. Many
of Go's arithmetic functions have been written in hand-optimised
assembly. The extremely high performance produced reflects this.

Haskell's **GHC** also uses GMP. Very fast and better fine-tuning than
Julia. Considering moving away though, because of licensing constraints.

GMP is optimum in many cases!

https://mail.mozilla.org/pipermail/rust-dev/2014-June/010363.html
http://blog.regehr.org/archives/1154

Note: All the code for these benchmarks is
[available in this git repo](https://github.com/Wilfred/the_end_times)
and the language implementations I used were:



