--- 
layout: post
title: "An Industrial-Grade BF Compiler"
---

What do you expect from your compiler? The best compilers offer:

* Highly optimised output
* Short compile times
* Readable syntax errors
* Helpful warnings
* Support for lots of architectures

Being a wildly overambitious developer, I set out to make
[bfc](https://github.com/Wilfred/bfc) provide this whole feature
set. Let's take a look at the result.

## Optimisations

Like any self-respecting LLVM frontend, bfc outputs LLVM IR
directly. To my surprise, BF compilers using a C backend (such as
[this one](https://github.com/matslina/bfoptimization/blob/76a5b4b7a9e67c44ccbeea9a53431f658bfedf04/optimizr.py))
were routinely beating bfc's performance.

LLVM provides an
[excellent set of tips for frontend compiler writers](http://llvm.org/docs/Frontend/PerformanceTips.html). Re-reading
this document, I noticed a new
[note had been added about pass ordering](https://github.com/llvm-mirror/llvm/commit/047904e858cad10493d8c7f7eaa8414996e8013c). It
turns out that LLVM's default optimisations are tuned for C, and bfc
*massively* benefits from running the optimisations again.

In other words:

    $ bfc-1.3 some_program.bf --dump-llvm > raw_ir.ll
    $ opt -S -O3 raw_ir.ll -o opt_once.ll
    $ opt -S -O3 opt_once.ll -o opt_twice.ll

`opt_twice.ll` was often much faster than `opt_once.ll`!

After fixing the LLVM passes, and reusing the BF benchmark from the
[previous blog post](/blog/2015/10/18/even-more-bf-optimisations/), we
see a significant speedup on all test programs:

<figure>
<div id="old-vs-new" style="min-width: 310px; max-width: 800px; height:500px; margin: 0 auto"></div>
</figure>

## Short Compile Times

LLVM provides command line tools for optimising (`opt`) and for
writing object files (`llc`). These are great for exploring LLVM.

However, bfc wrote its LLVM IR to temporary files and shelled out to
run these commands. This is slower and brittle: users with LLVM
libraries could compile bfc, but suffered unhelpful errors when bfc
could not find `opt` on `$PATH`.

bfc now uses the LLVM APIs directly for writing object files. This is
not as well documented (the
[Kaleidoscope tutorial](http://llvm.org/docs/tutorial/index.html) does
not cover it), but reading the source of `llc` and `rustc` shows
examples. You can see
[the API calls used by bfc here](https://github.com/Wilfred/bfc/blob/3a7ac4742b54ce6bb3e5fcab35fbf4e4e59736f1/src/llvm.rs#L821-L846).

For maximum performance, bfc users can specify `--opt` or `--llvm-opt`
to produce debug builds with fewer optimisations.

## Syntax Errors

'Syntax error' is not helpful, and programmers expect better. A
compiler should highlight where the error occurred, and provide a
clear description of what is wrong.

<img src="/assets/bfc_syntax_error.png">

bfc now provides friendly syntax errors, showing the filename, line
number, column number and highlighting the offending
syntax.

## Warnings

bfc performs static analysis and compile time execution. During this
process, it may find code that looks incorrect.

We want to warn the user when this occurs. This is difficult, because
bfc can dramatically change the AST as it optimises.

In v1.3.0 bfc gained position annotations to its AST. Optimisation
passes now preserve position information wherever possible.

However, some optimisations aggressively reorder BF instructions. If
those instructions are not consecutive, bfc cannot preserve position
information. Instead,
[I added simpler optimisations](https://github.com/Wilfred/bfc/commit/bfe59b069f0621ac2bc5f4092dd5dd95ba3eb1d8)
that run earlier and can keep position information.

Here's an example of a warning:

<img src="/assets/bfc_warning.png">

## Cross-Compilation

No self-respecting compiler can only target a single
architecture. With LLVM, it's easy to support
[a remarkable range of architectures](http://llvm.org/docs/doxygen/html/classllvm_1_1Triple.html#a547abd13f7a3c063aa72c8192a868154).

Previously, bfc always compiled to 32-bit x86. bfc now compiles to the
host architecture by default. Users with x86-64 machines will see a
modest performance improvement from this.

bfc now also supports cross-compilation by specifying an LLVM
target triple:

    $ bfc hello_world.bf --target=x86_64-pc-linux-gnu

## Scraping The Bottom Of The Barrel

Having implemented all these features, bfc now has the dubious title
of the most <strike>overengineered</strike> sophisticated BF compiler available.

There's not even much scope for further polish. There are
a few niche optimisations we lack, such as scan loops (`[>]`) and
integer division.

Some BF implementations also provide a `#` instruction, to
print the current cell values and aid debugging. bfc does not yet
provide this either.

I hope you've enjoyed this series on BF compilation. BF is a fantastic
playground, and LLVM is an incredible feat of engineering. If you
encounter a bug or limitation in bfc, don't hesitate to
[file a bug](https://github.com/Wilfred/bfc/issues/new).

<script src="/bower_components/jquery/dist/jquery.min.js"></script>
<script src="/bower_components/highcharts/highcharts.js"></script>
<script src="/bower_components/highcharts/modules/exporting.js"></script>

<script>
function plot(selector, categories, series, opts) {
    opts = opts || {};
    $(selector).highcharts({
        chart: {
            type: 'bar'
        },
        title: {
            text: null
        },
        xAxis: {
            categories: categories,
        },
        yAxis: {
            min: 0,
            max: null,
            title: {
                text: opts.title,
                align: 'high'
            },
        },
        tooltip: {
            valueSuffix: ' seconds',
            // The default pointFormat but with numbers rounded to 3dp.
            pointFormat: '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:.3f}</b><br/>'
        },
        plotOptions: {
            bar: {
            }
        },
        exporting: {
            enabled: false
        },
        credits: {
            enabled: false
        },
        series: series
    });
}

plot("#old-vs-new",
     ['mandelbrot', 'factor', 'long', 'hanoi', 'dbfi', 'awib'],
     [{
         name: 'v1.3.0',
         data: [3.71, 0.955, 1.421, 0.071, 10.396, 3.061]
     }, {
         name: 'v1.6.0',
         data: [1.431, 0.343, 0.829, 0.010, 3.469, 1.091]
     }],
     {
         title: 'Runtime in seconds (fastest of 3 runs)'
     }
    );
</script>
