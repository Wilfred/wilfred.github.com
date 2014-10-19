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

<figure>
<div id="container" style="min-width: 310px; max-width: 800px; height:500px; margin: 0 auto"></div>
    <figcaption>pidigits performance (GMP implementations in green)</figcaption>
</figure>

Wow, we can see a huge range of performance here. Click and drag on
the chart to zoom in. We'll take a look at how the different
languages have been implemented, starting with the slowest.

The slowest BigInt here is **PHP**. PHP doesn't have a BigInt data type,
so its arbitrary size numerical functions take and return strings!
For example, check out [the docs for addition](http://php.net/manual/en/function.bcadd.php). This
adds huge overhead as the numbers get larger, because an increasing
amount of time is spent serialising and deserialising strings
representing numbers. If you run the benchmark in a shell, the
incremental slowdown is very clear.

The next slowest is **JRuby**. JRuby depends on the BigInteger.java
implementation provided by the JDK. As a result, it's unlikely to
outperform the Java test program. The performance difference between
**OpenJDK** and JRuby must be due to the additional interpreter
overhead in JRuby.

**MRI** has its
[own BigInt implementation written in C](https://github.com/ruby/ruby/blob/v2_1_3/bignum.c),
which uses GMP (the GNU MP Bignum library) when available. `ldd` reports that `/usr/bin/ruby` is
linked to libgmp on my system. In spite of this, performance is still
fairly slow. I presume the interpreter overhead is high in this benchmark.

**Rubinius** is the fastest Ruby implementation I benchmarked. The
design of Rubinius is somewhat based on the design of Smalltalk-80 as
described in the
'[blue book](http://stephane.ducasse.free.fr/FreeBooks/BlueBook/Bluebook.pdf)'. Much
of the interpreter is written in Ruby, but
[the BigInt implementation is C++](https://github.com/rubinius/rubinius/blob/v2.2.10/vm/builtin/bignum.cpp). Beating
a GMP based implementation is very impressive.

We're seeing a significant range of speeds for Scheme
implementations. It's interesting to note that Guile and Racket are
JIT interpreters, Chicken is compiled, and Gambit is interpreted (I
couldn't get the compiler to run on my machine). The idea that
compiled languages are always faster than interpreted is refuted here.

Gambit and Chicken are both self-hosting Scheme compilers. **Gambit**
[implements numerics in Scheme](https://github.com/feeley/gambit/blob/v4.7.0/lib/_num.scm). **Chicken**
is also largely written in Scheme, and my understanding of the
`compiler.scm` is that Chicken implements BigInts in Scheme too. This
is extremely impressive technically and makes these compilers more
hackable.

By contrast, **Racket**
[uses GMP](https://github.com/plt/libs/blob/2f116c1b64af3f980a403cb4b57051457b2a9c39/math-x86_64-linux-natipkg/math/info.rkt),
and **Guile**
[does too](http://git.savannah.gnu.org/gitweb/?p=guile.git;a=blob;f=README;h=92d786c069837d81126d598e93416a20fc68a0c2;hb=HEAD#l65).
GMP is capable of very high performance, and as a result these
implementations are substantially faster.

**SBCL** is a very well-optimised Common Lisp compiler, with
[some rather elegant compiler implementation features](http://www.pvk.ca/Blog/2014/08/16/how-to-define-new-intrinsics-in-sbcl/).
Its BigInt implementation is
[written in Common Lisp](https://github.com/sbcl/sbcl/blob/sbcl-1.2.2/src/code/bignum.lisp)
and is modestly described as
"[reasonably simple but decent](http://www.sbcl.org/gsoc2013/ideas/#sec-1.2)". SBCL
is exceptional at optimising fixed-size arithmetic, but the BigInt
performance here is still very good.

**Rust** demonstrates superb performance for a young language, with
[BigInt being implemented in the host language](https://github.com/rust-lang/rust/blob/0.12.0/src/libnum/bigint.rs). Rust
is seeking to be standalone systems language, and BigInts will
actually be [an external library](https://github.com/rust-lang/num) in a future
release.

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

## Conclusions

GMP is optimum in many cases! In some cases, languages are moving away
from it.

Languages aren't slow; implementations are.

Hackability is still important.

Hardware is ultimately the limiting factor.

Very hard to do a fair test.

https://mail.mozilla.org/pipermail/rust-dev/2014-June/010363.html
http://blog.regehr.org/archives/1154

<script src="/bower_components/jquery/dist/jquery.min.js"></script>
<script src="/bower_components/highcharts/highcharts.js"></script>
<script src="/bower_components/highcharts/modules/exporting.js"></script>

<script>
$('#container').highcharts({
    chart: {
        type: 'bar',
        zoomType: 'y',
    },
    title: {
        text: ''
    },
    xAxis: {
        categories: ["SBCL", "Go", "GHC", "OpenJDK", "Julia", "PHP", "CPython", "pypy", "JRuby", "MRI", "Rubinius", "Rust", "Chicken", "Gambit", "Guile", "Racket"],
        title: {
            text: null
        },
        labels: {
            style: {
                fontSize: "16px"
            }
        }
    },
    yAxis: {
        min: 0,
        title: {
            text: 'Time in seconds',
            align: 'high'
        },
        labels: {
            overflow: 'justify',
            style: {
                fontSize: "16px"
            }
        }
    },
    tooltip: {
        enabled: false
    },
    legend: {
        enabled: false,
    },
    exporting: {
        enabled: false,
    },
    plotOptions: {
        bar: {
            dataLabels: {
                enabled: true,
                style: {
                    fontSize: '16px',
                }
            }
        }
    },
    credits: {
        enabled: false
    },
    series: [{
        name: 'pidigits',
        data: [
            12.049, 2.899,
            {y: 1.583, color: "#55CC55"},
            19.618,
            {y: 7.395, color: "#55CC55"},
            438.72, 4.568, 8.881, 29.197,
            {y: 27.721, color: "#55CC55"},
            19.010, 11.793, 8.350, 14.631,
            {y: 9.431, color: "#55CC55"},
            {y: 17.369, color: "#55CC55"}
        ]
    }]
});
</script>

