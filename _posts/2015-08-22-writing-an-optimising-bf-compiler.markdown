--- 
layout: post
title: "Writing an Optimising BF Compiler"
---

It seems like everyone's written a
[BF](https://en.wikipedia.org/wiki/Brainfuck) implementation. BF is
now so widespread that LLVM and libjit both provide tutorials for
writing compilers with their toolset!

However, what would an industrial strength compiler look like? I set
out to write
[a highly optimising compiler](https://github.com/Wilfred/bfc), called
`bfc`, and it's screaming fast. Let's take look.

## Baseline Performance

I've written a basic BF interpreter in C, so we'll use that as our
reference point for BF performance. Let's compare our compiler,
without optimisations, with this interpreter.

As you can see, even without optimisations, our compiler is
comfortably outperforming the interpreter.

## Collapsing Increments

Let's look at some peephole optimisations. Consider the BF program `++`. This increments current cell
twice. Our baseline compiler would convert this to:

    cell_val := load(cell_ptr)
    cell_val := cell_val + 1
    store(cell_ptr, cell_val)
    cell_val := load(cell_ptr)
    cell_val := cell_val + 1
    store(cell_ptr, cell_val)

However, the underlying computer is capable of adding numbers other
than one! We expand our intermediate representation (IR) to handle
increments, so we can compile this to:

    cell_val := load(cell_ptr)
    cell_val := cell_val + 2
    store(cell_ptr, cell_val)

We can do the same thing with pointers too. Rather than compiling `>>`
to this:

    cell_ptr := cell_ptr + 1
    cell_ptr := cell_ptr + 1

We compile it to this:

    cell_ptr := cell_ptr + 2

## Loop Simplification: Clear

A common BF idiom is `[-]`, which zeroes the current cell. `bfc`'s
optimiser knows about this, and it's smart enough to compile it to:

    store(cell_ptr, 0)

Setting a constant value is great because it opens up new opportunities
for collapsing instructions. We can go a step further and combine
increments, so `[-]+++` can be compiled to:

    store(cell_ptr, 3)

## Loop Simplification: Multiply

BF doesn't provide a multiply instruction, so programs have to use a
loop. `[->++<]` is equivalent to:

    cell#1 := cell#0 * 2 + cell#1
    cell#0 := 0

Our optimiser can also detect these and generate efficient code. This
also captures useful patterns like `[->+<]` (move cell 0 to cell 1)
and `[->+>+<<]` (move cell 0 to both cell 1 and cell 2).

As a result, we compile `[->++<]` to:

    cell0_val := load(cell_ptr)
    cell1_old_val := load(cell_ptr + 1)
    cell1_new_val := cell1_old_val + 2 * cell0_val
    store(cell_ptr + 1, cell1_new_val)
    store(cell_ptr, 0)

Loops are far more expensive than multiplication, so this improves
performance considerably.

## Dead Code Elimination

It's common to generate BF code with other tools, which can be
redundant. It's also common to add an introductory comment at the
beginning of a BF program:

    [Since we know that cells are initialised to zero,
     we can write whatever we want here because this
     loop is dead. We can use any of +-<>.,[] safely,
     as long as our brackets are matched.]
    actual code here!

`bfc` aggressively removes dead code. For example, cells in BF are
initialised to zero, so a loop at the beginning of a program is dead:

    [dead]not-dead

We also know that if a loop terminates, the current cell is zero. As a
result, consecutive loops are dead:

    foo[bar][dead][also dead]

We already know that `[-]+` is setting the current cell to a constant
value. We can extend this, so `[-]+[-]++` is equivalent to:

    [-]++

Finally, we can exploit I/O to remove dead code. `,` reads a byte from
stdin, and stores it in the current cell. As a result, the following
snippets can all be reduced to just `,`:

    +,
    -,
    [-]+,

The only side effects possible in a BF program are reading, writing
and infinite loops. We can exploit this to remove any commands after
the last side effect. For example, we convert `whatever.<>+-` to simply `whatever.`.

TODO: is I/O really dead, or is there a different term?

## Testing Optimisations

With all these different optimisations, how can we know that we have
combined them in the optimal order? There's a great
[Rust implementation of Quickcheck](#TODO) that we can use.

{% highlight rust %}
#[quickcheck]
fn should_be_idempotent(instrs: Vec<Instruction>) -> bool {
    // Once we've optimised once, running again shouldn't
    // reduce the instructions further.
    let minimal = optimise(instrs.clone());
    optimise(minimal.clone()) == minimal
}
{% endhighlight %}

This proved to be a fantastic way of finding corner cases in the
optimisation code. After various attempts at ordering optimisations,
Quickcheck found a BF program of the form `+>+-<-`. This showed `bfc`
had to run optimisations repeatedly to ensure that we exploited all
available opportunities.

## LLVM Optimisations

## Bounds Analysis

## Speculative Execution

## Closing Thoughts
