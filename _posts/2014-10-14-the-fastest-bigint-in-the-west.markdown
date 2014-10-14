--- 
layout: post
title: "The Fastest BigInt In The West"
---

The
[Computer Language Benchmarks Game](http://benchmarksgame.alioth.debian.org/)
is an amazing project, featuring some exremely carefully optimised
functions.

However, it's not always easy to interpret the results, to see *why*
some languages are faster than others. It's also not clear how
representative the test programs are of typical language performance.

Instead, I thought it would be interesting to measure the performance
of the pidigits benchmark, limiting myself to the arbitrary size
integer provided by the standard library. Not all languages provide
this in their stdlib, so I measured multiple language implementations
where possible.

| Language | Implementation | Time/seconds |
|----------|----------------|-------------:|
| Common Lisp | SBCL | 11.635 |
| Go | Go | 2.956 |
| Julia | Julia |  5.790 |
| Haskell | GHC | 1.582 |
| Python3 | CPython | 4.391 |
| Python3 | pypy | 6.505 |
| Ruby | JRuby | 26.803 |
| Ruby | MRI | 26.135 |
| Ruby | Rubinius | 13.624 |
| Rust | Rust | 11.930 |
| Scheme | Chicken | 8.181 |
| Scheme | Gambit | 13.763 |
| Scheme | Guile | 8.644 |
| Scheme | Racket | 16.837 |

There's a huge range of performance here. Let's look at the different
implementation approaches.

The slowest BigInt here is **PHP**. PHP doesn't have a BigInt data type,
so its arbitrary size numerical functions take and return strings!
[The docs for addition are here](http://php.net/manual/en/function.bcadd.php). This
adds huge overhead as the numbers get larger, because an increasing
amount of time is spent serialising and deserialising strings
representing numbers. If you run the benchmark in a shell, the
incremental slowdown is very clear.

The next slowest is **JRuby**. JRuby depends on the BigInteger.java
implementation provided by the JDK. This is very respectable
performance for a BigInt implemented in the host language, the
majority of the faster BigInt designs have been written in C.

We're seeing a significant range of speeds for **Scheme**
implementations. It's interesting to note that Guile and Racket are
JIT interpreters, Chicken is compiled, and Gambit is interpreted (I
couldn't get the compiler to run on my machine). The idea that
compiled languages are always faster than interpreted is refuted here.

Since Scheme requires integers to be arbitrary sized, we're exercising
built-in functionality here rather than libraries. The implementations
are generally written in C.

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



