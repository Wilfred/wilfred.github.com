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
BigInts provided by the language or its standard library. It also
ensures our test programs are fairly readable.

Limiting our benchmark to languages with built-in BigInts
significantly limits the number of languages we can test. I decided to
include multiple implementations of the same language to see the
variation within the same language.

We aren't requiring all the test programs for the same language to be
identical. I wasn't able to run the same program on all Scheme
implementations anyway.

By default, The Benchmarks Game shows the same
test program for different implementations. It also allows users to
submit programs that improve performance for one implementation at the
expense of another, and we have used those programs here.

I ran these programs an AMD Phenom X4 965 with 4 GiB RAM. The kernel
was Linux x86_64 3.16.4 and
[the exact code I used was in this git commit](https://github.com/Wilfred/the_end_times/commit/574ec7ee5a07ea34c0e158584bdaa8af774d368b).

So, let's look at the results!

<figure>
<div id="container" style="min-width: 310px; max-width: 800px; height:500px; margin: 0 auto"></div>
    <figcaption>pidigits performance (GMP implementations in green)</figcaption>
</figure>

Wow, we can see a huge range of performance here. **Click and drag on
the chart to zoom in**. We'll take a look at how the different
languages have been implemented, starting with the slowest.

The slowest BigInt here is **PHP**. PHP
[implements its arbitrary size arithmetic in C](https://github.com/php/php-src/blob/d0cb715373c3fbe9dc095378ec5ed8c71f799f67/ext/bcmath/bcmath.c).
However, it doesn't have a BigInt data type, so its arbitrary size
numerical functions take and return strings!  For example, check out
[the docs for addition](http://php.net/manual/en/function.bcadd.php). This
adds huge overhead as the numbers get larger, because an increasing
amount of time is spent serialising and deserialising strings
representing numbers. If you watch the test program running in a shell, the
linear slowdown is very clear.

The next slowest is **JRuby**. JRuby depends on the
[BigInteger.java](http://hg.openjdk.java.net/jdk7/jdk7/jdk/file/9b8c96f96a0f/src/share/classes/java/math/BigInteger.java)
implementation provided by the JDK. As a result, it's unlikely to
outperform the Java test program (which also users
BigInteger.java). The performance difference between **OpenJDK** and
JRuby is presumably due to the additional interpreter overhead in JRuby.

**MRI** has its
[own BigInt implementation written in C](https://github.com/ruby/ruby/blob/v2_1_3/bignum.c),
which uses GMP (the [GNU MP Bignum Library](https://gmplib.org/)) when available. `ldd` reports that `/usr/bin/ruby` is
linked to libgmp on my system. In spite of this, performance is still
fairly slow. I presume the interpreter overhead is high in this benchmark.

**Rubinius** was the fastest Ruby implementation in this benchmark. The
design of Rubinius is somewhat based on the design of Smalltalk-80 as
described in the
'[blue book](http://stephane.ducasse.free.fr/FreeBooks/BlueBook/Bluebook.pdf)'. Much
of the interpreter is written in Ruby, but
[the BigInt implementation is C++](https://github.com/rubinius/rubinius/blob/v2.2.10/vm/builtin/bignum.cpp). Beating
any GMP based implementation is very impressive.

We're seeing a significant range of speeds for Scheme
implementations. It's interesting to note that Guile and Racket are
JIT interpreters, and Gambit is interpreted in this benchmark (I
couldn't get the compiler to run on my machine).

**Gambit** is a self-hosting Scheme compiler (although it offers an
interpreter too). Gambit
[implements numerics in Scheme](https://github.com/feeley/gambit/blob/v4.7.0/lib/_num.scm). This
is extremely impressive technically and makes this compiler more
hackable.

By contrast, **Racket**
[uses GMP through its FFI](https://github.com/plt/libs/blob/2f116c1b64af3f980a403cb4b57051457b2a9c39/math-x86_64-linux-natipkg/math/info.rkt),
and **Guile**
[has C code that interfaces with GMP](http://git.savannah.gnu.org/gitweb/?p=guile.git;a=blob;f=libguile/numbers.c;hb=475772ea57c97d0fa0f9ed9303db137d9798ddd3).
GMP is capable of very high performance, and as a result these
implementations are substantially faster.

**SBCL** is a very well-optimised Common Lisp compiler, with
[some rather elegant compiler implementation features](http://www.pvk.ca/Blog/2014/08/16/how-to-define-new-intrinsics-in-sbcl/).
Its BigInt implementation is
[written in Common Lisp](https://github.com/sbcl/sbcl/blob/sbcl-1.2.2/src/code/bignum.lisp)
and runs very quickly. SBCL includes
[GMP bindings](https://github.com/sbcl/sbcl/blob/sbcl-1.2.2/contrib/sb-gmp/gmp.lisp)
but as GMP's memory representation of BigInt is different, it's not a
drop-in replacement ([though it may be in the future](http://www.sbcl.org/gsoc2013/ideas/#sec-1.2)).

**Rust** demonstrates superb performance for a young language, with
[BigInt being implemented in the host language](https://github.com/rust-lang/rust/blob/0.12.0/src/libnum/bigint.rs). Rust
is seeking to be standalone systems language, and BigInts will
actually be [an external library](https://github.com/rust-lang/num) in a future
release.

Python is often a slow language, but its performance here took me
completely by surprise. **CPython** has a
[BigInt implementation written in C](https://github.com/python/cpython/blob/65d4639677d60ec503bb2ccd2a196e5347065f27/Objects/longobject.c). CPython
includes a number of interesting numerical optimisations, including
[reusing integer objects between -5 and 256](http://www.laurentluce.com/posts/python-integer-objects-implementation/). **PyPy**
is a little slower here, as
[its JIT is unable to optimise this code](https://mail.python.org/pipermail/pypy-dev/2014-August/012713.html).

**Julia** takes a
['batteries included' approach](https://github.com/JuliaLang/julia/#required-build-tools-and-external-libraries),
especially to its numeric support, so
[it uses GMP through its FFI](https://github.com/JuliaLang/julia/blob/v0.3.1/base/gmp.jl). Julia
targets scientific computing and carefully monitors numeric
performance, so the strong performance here is unsurprising.

**Go** is the most heavily hand-optimised language for this
benchmark. Many of Go's arithmetic functions have been written in
[hand-optimised assembly](https://golang.org/src/pkg/math/big/arith_amd64.s)!
If you're not on an optimised architecture your code will use
[an equivalent Go implementation](https://golang.org/src/pkg/math/big/arith.go). The
performance numbers in this benchmark are deservedly impressive.

Finally, **GHC**
[uses GMP](https://github.com/ghc/packages-integer-gmp/blob/ghc-7.8.2-release/GHC/Integer/GMP/Prim.hs),
and its performance is nothing short of extraordinary. GHC is clearly
tuned to call into GMP with extremely low overheads.

## Closing Thoughts

[The GNU Multiple Precision Arithmetic Library](https://gmplib.org/)
is really, really fast on a wide range of hardware and over a wide
numeric range. This is a result of many years of careful
optimisation.

GMP isn't perfect. It is licensed under the ​GNU Lesser General Public
License (LGPL),
[which presents some problems with statically linked programs](https://ghc.haskell.org/trac/ghc/wiki/ReplacingGMPNotes#ReasonsforReplacingGMPastheBignumlibrary). When
implementing a language you don't want to constrain the licensing of
programs compiled with your compiler.

It's also important we don't lose sight of hackability. A language
implementation that has its own BigInt implementation is often easier
to work on. In some cases, that may be a higher priority than raw
performance. Any language worth its salt provides a FFI so users with
major performance needs can always call GMP themselves anyway.

There's also the interesting question of whether a language should
provide fixed-size or arbitrary-sized integers by default. The
[Rust community has wrestled with this](https://mail.mozilla.org/pipermail/rust-dev/2014-June/010363.html). Other
languages, such as Julia, settle for providing 64-bit integers by
default (on an x86-64 machine) in the hope that they're 'big enough'
for typical usage without requiring the memory allocation of arbitrary
sized
integers. Others have commented that [hardware could support BigInts much more efficiently than it does today](http://blog.regehr.org/archives/1154).

Finally, it's very hard to do a fair test, and I strongly suspect my
methodology isn't ideal. I have collected these test programs from a
variety of sources in order to have test programs that met my
criteria. I'm sure some these programs have significant scope for
optimisation still.

I hope you enjoyed this exploration of language
implementations. Languages are not fast or slow; implementations
are. All these implementations are amazing, production-ready feats of
engineering, and I've barely scratched the surface.

<script src="/bower_components/jquery/dist/jquery.min.js"></script>
<script src="/bower_components/highcharts/highcharts.js"></script>
<script src="/bower_components/highcharts/modules/exporting.js"></script>

<script>
$('#container').highcharts({
    chart: {
        type: 'bar',
        zoomType: 'y',
        style: {
            fontFamily: '"Merriweather", "PT Serif", Georgia, "Times New Roman", serif',
            fontSize: '15px'
        }
    },
    title: {
        text: null
    },
    xAxis: {
        categories: [
            "SBCL 1.2.2",
            "Go 1.3.3",
            "GHC 7.8.3",
            "OpenJDK 1.7.0_71",
            "Julia 0.3.1",
            "PHP 5.6.2",
            "CPython 3.4.2",
            "PyPy 2.4.0",
            "JRuby 1.7.16",
            "MRI 2.1.3p242",
            "Rubinius 2.2.10",
            "Rust 0.12.0-dev",
            "Gambit 4.7.3",
            "Guile 2.0.11",
            "Racket 6.1"
        ],
        
        title: {
            text: null
        },
        labels: {
            style: {
                fontSize: "15px"
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
                fontSize: "15px"
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
                    fontWeight: 'normal'
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
            438.72, 4.568, 6.463, 29.197,
            {y: 27.721, color: "#55CC55"},
            19.010, 11.793, 14.631,
            {y: 9.431, color: "#55CC55"},
            {y: 17.369, color: "#55CC55"}
        ]
    }]
});
</script>

