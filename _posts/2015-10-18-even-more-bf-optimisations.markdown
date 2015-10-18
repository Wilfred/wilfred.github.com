--- 
layout: post
title: "Even More BF Optimisations"
---

I've received lots of positive feedback following the release of
[bfc v1.0.0](/blog/2015/08/29/an-optimising-bf-compiler/), my
optimising BF compiler.

However, the competition has been heating up! It's been interesting to
compare with performance other optimising BF implementations. In a few
cases, bfc's performance was matched by simpler implementations.

Challenge accepted. How can we leverage bfc's optimisations to
produce even faster BF programs?

## Reorder With Offset

One popular optimisation bfc lacked was instruction offsets.

If we have the code:

    >+>>++>

Our IR previously looked like this:

    PointerIncrement 1
    Increment 1
    PointerIncrement 2
    Increment 2
    PointerIncrement 1

However, accessing cells in our array at a specific offset is
cheap. If we're coding in C, accessing `some_array[index + 1]` is
often as fast as accessing `some_array[index]`.

We add an offset attribute to Increment and Set instructions. This has
two big advantages:

1. Array access with a constant offset is cheap in hardware.
2. We can combine many more pointer increment instructions.

After introducing offsets, our IR looks like this:

    Increment 1 (offset 1)
    Increment 2 (offset 3)
    PointerIncrement 4

There's another advantage of adding offsets to Increment and Set
instructions. Given a sequence of Increments and Sets with different
offsets, we can freely reorder them. Given the IR:

    Increment 1 (offset 1)
    Increment 2 (offset -1)
    Set 3 (offset 2)
    Set 4 (offset 0)

We sort by offset, producing the IR:

    Increment 2 (offset -1)
    Set 4 (offset 0)
    Increment 1 (offset 1)
    Set 3 (offset 2)

By accessing the cells array in sequential order, we get better cache
locality during program execution.

## Next Mutation Analysis

In bfc v1.0.0, we would remove redundant adjacent instructions. For
example, incrementing a cell is pointless just before writing to it:

    +,

and consecutive loops loops are dead:

    [some loop here][this loop is dead]

However, now that we're reordering instructions, our adjacent
instruction techniques may miss out.

bfc solves this by with 'next mutation analysis'. Given an
instruction, bfc is now able to find the next mutation instruction for
the current cell.

Of course, it's not always possible to find one. It
may not be possible to determine the next mutation at compile time
(e.g. complex loops), or there may not be another mutation for this
cell.

Here's an example. Given the IR:

    Loop (some loop body)
    Set 0 (offset 0) <- this is redundant!
    Set 0 (offset -1)

If a loop terminates, the current cell is 0, so we know this set is
redundant. However, after reordering, that set does not immediately
follow the loop:

    Loop (some loop body)
    Set 0 (offset -1)
    Set 0 (offset 0) <- this is still redundant!

bfc uses next mutation analysis to find this redundant set and remove
it, giving us:

    Loop (some loop body)
    Set 0 (offset -1)

This is a generalisation of the techniques used in v1.0.0, and it
applies in other situations too. For example, consider the program:

    [loop X]>.<[loop Y]

Loop Y is dead here, and bfc is able to remove it.

## Partial Loop Execution

bfc was already able to execute BF code at compile time, producing big
speedups in many popular BF programs.

However, v1.0.0 required loops to be completely executed at compile
time. This simplified the implementation, but many larger BF programs
have a big outer loop. These programs did not benefit from bfc's
compile time execution.

For example, given the IR:

    A
    B
    Loop:
      C
      D
      Loop:
        E
        F

If we couldn't execute the whole outer loop at compile time, we would
only be able to remove A and B from the final executable, giving us:

    Loop:
      C
      D
      Loop:
        E
        F

Now in v1.2.0, we can execute up to an arbitrary position in the code. As
in v1.0.0, bfc executes until it reaches a `,` instruction, it reaches
a time limit, or the program terminates.

If we only manage to partially execute a loop, we split the basic
block, and runtime execution begins where compile time execution
finished.

In our example above, suppose we reach the time limit just after
executing E. Our final executable looks like this:

    GOTO start
    Loop:
      C
      D
      Loop:
        E
      start:
        F


## Benchmarks

For v1.2.0 I've measured bfc using
[Mats Linander's benchmark suite](https://github.com/matslina/bfoptimization).

<figure>
<div id="old-vs-new" style="min-width: 310px; max-width: 800px; height:500px; margin: 0 auto"></div>
</figure>

v1.2.0 has improved performance in every case. If we look at
normalised performance, we can see which benchmarks have improved the
most relative to v1.0.0:

<figure>
<div id="old-vs-new-normalised" style="min-width: 310px; max-width: 800px; height:500px; margin: 0 auto"></div>
</figure>

## Lessons Learnt

There's a number of lessons I've learnt from these additional
optimisation techniques.

Firstly, optimisation is hard. Static analysis of programs is easy to
screw up even with a good test suite. It took me several hours to get
the last (known) bug out of the next mutation analysis.

Secondly, optimisation is never done. There are always further improvements
possible. The challenge is finding which optimisations are
sufficiently general that they apply to 'real
world' code. It's not even clear what the upper limit on performance
is. Mats Linander's project *still* beats bfc in many cases!

Thirdly, it's not enough to use an optimising compiler backend. LLVM
has many powerful optimisations, and
[tips for using them effectively](http://llvm.org/docs/Frontend/PerformanceTips.html). This
is no substitute for exploiting the semantics of the target language
in the frontend.

<!-- Geometric mean is 0.783, so 1 / 0.783 = 28% faster
We use the geometric mean because the numbers are normalised
(the pypy team recommends this).
-->

Finally, we have completed our challenge! bfc v1.2.0 produces
executables that are, on average, 28% faster than v1.0.0. This totally
ridiculous project is
[available in all its glory on GitHub](https://github.com/Wilfred/bfc).

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
            // The default pointFormat but with numbers rounded to 5dp.
            pointFormat: '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:.5f}</b><br/>'
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
         name: 'v1.0.0',
         data: [4.73, 1.64, 1.65, 0.16, 15.39, 4.73]
     }, {
         name: 'v1.2.0',
         data: [4.42, 1.38, 0.89, 0.09, 15.36, 4.59]
     }],
     {
         title: 'Runtime in seconds (fastest of 3 runs)'
     }
    );
plot("#old-vs-new-normalised",
     ['mandelbrot', 'factor', 'long', 'hanoi', 'dbfi', 'awib'],
     [{
         name: 'v1.0.0',
         data: [1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
     }, {
         name: 'v1.2.0',
         data: [4.42 / 4.73,
                1.38 / 1.64,
                0.89 / 1.65,
                0.09 / 0.16,
                15.36 / 15.39,
                4.59 / 4.73]
     }],
     {
         title: 'Runtime relative to v1.0.0'
     }
    );
</script>
