--- 
layout: post
title: "Even More BF Optimisations"
---

I've received lots of positive feedback following
[the first release of my optimising BF compiler](/blog/2015/08/29/an-optimising-bf-compiler/).

However, some uncharitable folks did not take my blog post at face
value and actually benchmarked bfc against some
other BF implementations. 

In some cases, bfc's performance was comparable or slower than some
other optimising implementations.

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

After reordering, we produce the IR:

    Increment 2 (offset -1)
    Set 4 (offset 0)
    Increment 1 (offset 1)
    Set 3 (offset 2)

This gives us better cache locality during program execution.

## Next Mutation Analysis

In bfc v1.0.0, we would remove redundant adjacent instructions. For
example, incrementing a cell before writing to it:

    +,

and dead loops:

    [some loop here][this loop is dead]

However, now that we're reordering instructions, our redundant
instruction optimisations may not help us.

bfc is now able to find the next mutation instruction for a given
cell. Of course, it's not always possible to find one. It may not be
possible to determine the next mutation at compile time (e.g. complex
loops), or there may not be another mutation for this cell.

For example, given the IR:

    Loop (some loop body)
    Set 0 (offset 0) <- this is redundant!
    Set 0 (offset -1)

If a loop terminates, the current cell is 0, so we know this set is
redundant. However, after reordering, that set does not immediately
follow the loop:

    Loop (some loop body)
    Set 0 (offset -1)
    Set 0 (offset 0) <- this is redundant!

bfc uses next mutation analysis to find this redundant set and remove
it.

This is a generalisation of the previous approach, and applies in more
cases. For example, consider the program:

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
in v1.0.0, bfc executes until it reaches a `,` instruction, a timer
expires, or the program terminates.

If we only manage to partially execute a loop, we split the basic
block and runtime execution continues where compile time execution
stopped.

In our example above, suppose timer fires just after executing E. Our
final executable looks like this:

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

<script src="/bower_components/jquery/dist/jquery.min.js"></script>
<script src="/bower_components/highcharts/highcharts.js"></script>
<script src="/bower_components/highcharts/modules/exporting.js"></script>

<script>
function plot(selector, categories, series) {
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
                text: 'Runtime in seconds (fastest of 3 runs)',
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
     }]
    );
</script>
