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
[bfc, a highly optimising compiler](https://github.com/Wilfred/bfc),
and it's remarkably fast. Let's take a closer look.

## Baseline Performance

I've written a basic BF interpreter in C, so we'll use that as our
reference point for BF performance. Let's compare bfc,
compiling to LLVM IR without optimisations, with this interpreter.

As you can see, even without optimisations, bfc is
comfortably outperforming this interpreter. Faster interpreters exist,
so let's look at optimisations we can add to our compiler.

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

A common BF idiom is `[-]`, which zeroes the current cell. bfc's
optimiser knows about this, and it's smart enough to compile it to:

    store(cell_ptr, 0)

Setting a constant value is great because it opens up new opportunities
for collapsing instructions. We can go a step further and combine
increments, so `[-]+++` can be compiled to:

    store(cell_ptr, 3)

## Loop Simplification: Multiply

BF doesn't provide a multiply instruction, so programs have to use a
loop. `[->++<]` is equivalent to:

    cell#1 := cell#1 + 2 * cell#0
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

bfc aggressively removes dead code. For example, cells in BF are
initialised to zero, so a loop at the beginning of a program is dead:

    [dead]not-dead

We also know that if a loop terminates, the current cell is zero. As a
result, consecutive loops are dead:

    foo[bar][dead][also dead]

We already know that `[-]+` is setting the current cell to a constant
value. We can extend this, so `[-]+[-]++` is equivalent to:

    [-]++

## Redundant Code Elimination

bfc can also remove code that it can prove has no effect.

We already know that `[-]+` is setting the current cell to a constant
value. Repeated instances of this will clobber the previous value. For
example, in the code:

    [-]+++[-]++[-]+

is setting the same cell three times. We can replace this with just
`[-]+`.

I/O can also clobber values, making previous instructions
redundant. `,` reads a byte from stdin, and stores it in the current
cell. As a result, the following snippets can all be reduced to just
`,`:

    +,
    -,
    [-]+,

Instructions at the end of a program also be redundant. The only
possible side effects in a BF program are reading, writing and
infinite loops. If we can prove the final instructions have no
observable side effects, we remove them. For example:

    foo.<[-]+>

This can be reduced to just `foo.`

bfc assumes that all cell access are in bounds (between 0 and
30,000). As a result, the program `<+` is optimised away by this, so
it will not throw an error.

## Testing Optimisations

With all these different optimisations, how can we know that we have
combined them in the optimal order? There's a great
[Rust implementation of Quickcheck](https://github.com/BurntSushi/quickcheck)
that we can use.

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
Quickcheck found a BF program of the form `+>+-<-`. This showed bfc
had to run optimisations repeatedly to ensure that we exploited all
available opportunities.

## LLVM Optimisations

LLVM offers a suite of optimisations that we can run. Surprisingly,
there's little overlap with our BF-specific optimisations. It does
give us an additional performance boost, so bfc uses LLVM
optimisations too.

(TODO: benchmark here)

## Bounds Analysis

One novel optimisation that bfc offers is bounds analysis. In BF,
programs have 30,000 cells available, initialised to 0. However, most
programs don't use the full 30,000.

bfc uses static analysis to work out the highest cell that a program
could possibly reach.

## Speculative Execution

Many BF programs don't take any inputs at all. For programs that do
take input, there's an initialisation phase that sets cells to certain
values first.

bfc exploits this by executing as much as it can at compile time. We
run for a set period of time (to avoid problems with infinite loops)
and terminate if encounter a `,` (a read instruction).

As a result, we compile the classic BF hello world to:

{% highlight llvm %}
@known_outputs = constant [13 x i8] c"Hello World!\0A"

declare i32 @write(i32, i8*, i32)

define i32 @main() {
entry:
  %0 = call i32 @write(i32 0, i8* getelementptr inbounds ([13 x i8]* @known_outputs, i32 0, i32 0), i32 13)
  ret i32 0
}
{% endhighlight %}

bfc runs speculative execution after peephole optimisations and dead code
removal, to maximise the amount of work we can do before timing out.

Even if we cannot speculatively execute the whole program, partial
execution is useful. We can discard any instructions executed at
compile time. We can also initialise cells to the values reached
during speculative execution.

For example, consider the program `+>+>+>++>,.`. bfc compiles this to:

{% highlight llvm %}
  ; Initialise cell #0, cell #1 and cell #2 to 1.
  ; Note that we combine adjacent cells with the
  ; same value into a single memset call.
  %offset_cell_ptr = getelementptr i8* %cells, i32 0
  call void @llvm.memset.p0i8.i32(i8* %offset_cell_ptr, i8 1, i32 3, i32 1, i1 true)

  ; Initialise cell #3 to 2.
  %offset_cell_ptr1 = getelementptr i8* %cells, i32 3
  call void @llvm.memset.p0i8.i32(i8* %offset_cell_ptr1, i8 2, i32 1, i32 1, i1 true)

  ; Intialise cell #4 to 0.
  %offset_cell_ptr2 = getelementptr i8* %cells, i32 4
  call void @llvm.memset.p0i8.i32(i8* %offset_cell_ptr2, i8 0, i32 1, i32 1, i1 true)
{% endhighlight %}

## Closing Thoughts

Related projects: interpreter for awib, compiler that does speculative execution.

Further work: more peephole optimisations: offset operations, scanning
left/right (TODO: link to blog post). Could also extract
multiplication by a cell (not just a constant), or extract division.

Bounds analysis could be smarter -- `[>]` cannot access beyond the
highest cell accessed so far.

Some of these optimisations could be pushed into LLVM, but very useful
to run before speculative execution.

Profiling speculative execution would allow us to run for more steps,
reducing runtime work.

Speculative execution must finish a top-level loop in current
implementation.

There's a tension between optimising compilers and new language
compilers.
