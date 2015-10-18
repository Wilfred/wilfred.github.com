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

In bfc v1.0.0, we would 

## Benchmarks:

(fastest of three runs)

mandelbrot.bf: v1.0: 4.73 seconds v1.2: 4.42 seconds

factor.bf with 133333333333337: v1.0: 1.64 seconds v1.2: 1.38 seconds

long.bf: v1.0: 1.65 seconds v1.2: 0.89 seconds

hanoi.bf: v1.0: 0.16 secs, v1.2: 0.09 seconds

dbfi.bf with its own input then hello: v1.0: 15.39 seconds, v1.2: 15.36 seconds

awib.bf: v1.0: 4.73 seconds v1.2: 4.59 seconds
